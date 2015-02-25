(in-package :swf-workers)

(declaim (optimize (speed 0) (debug 3) (safety 3) (space 0)))

;; TODO: Check for duplicate workflow and activties types in a worker

(defun find-workflow-execution (workflow-id)
  (let* ((time-filter `((:oldest-date . ,(local-time:timestamp- (local-time:now) 1 :year))))
         (execution-filter (serialize-slot :workflow-id workflow-id))
         (executions (or (aget (swf::list-open-workflow-executions :start-time-filter time-filter
                                                                   :execution-filter execution-filter)
                               :execution-infos)
                         (aget (swf::list-closed-workflow-executions :start-time-filter time-filter
                                                                     :execution-filter execution-filter)
                               :execution-infos))))
    (deserialize-slot :workflow-execution (aget (car executions) :execution))))


;;; Common worker ----------------------------------------------------------------------------------


(defvar *worker*)


(defclass worker ()
  ((service :initarg :service
            :initform swf::*default-swf-service*
            :reader worker-service)
   (task-list :initarg :task-list
              :initform "default"
              :reader worker-task-list)
   (identity :initarg :identity
             :initform (format nil "~A@~A" (sb-posix:getpid) (machine-instance))
             :reader worker-identity)
   (packages :initarg :packages
             :initform (list *package*)
             :reader worker-packages)))


(defun set-worker-thread-status (type format-control &rest args)
  (log-trace "~?" format-control args)
  (setf (sb-thread:thread-name sb-thread:*current-thread*)
        (format nil "~A(~A): ~?" type (worker-task-list *worker*) format-control args)))


(defun worker-start (*worker* type)
  (let ((swf::*service* (worker-service *worker*)))
    (with-log-context type
      (loop for error = (worker-handle-next-task type)
            do (when error
                 (set-worker-thread-status type "Pause due to error")
                 (sleep 15))))))


(defun worker-start-thread (worker type)
  (sb-thread:make-thread (lambda (worker type)
                           (worker-start worker type))
                         :name (format nil "~A-WORKER: Init" type)
                         :arguments (list worker type)))


(defun worker-handle-next-task (type)
  (handler-bind ((error
                  (lambda (error)
                    (unless (typep error 'activity-error)
                      (when *enable-debugging*
                        (with-simple-restart (continue "Log error and look for next task.")
                          (invoke-debugger error)))
                      (report-error error)
                      ;(return-from worker-handle-next-task error)
                      ))))
    (let ((task (worker-look-for-task type)))
      (when task
        (with-log-context (task-workflow-id task)
          (with-log-context (let ((ttype (or (aget (%task-payload task) :workflow-type)
                                             (aget (%task-payload task) :activity-type))))
                              (format nil "~A/~A" (aget ttype :name) (aget ttype :version)))
            (worker-handle-task type task)
            (values nil t)))))))


(defclass %task ()
  ((payload :initarg :payload
             :reader %task-payload)))
(defclass %decision-task (%task)  ())
(defclass %activity-task (%task) ())

(defun task-token (task)
  (aget (%task-payload task) :task-token))

(defun task-workflow-id (task)
  (aget (%task-payload task) :workflow-execution :workflow-id))

(defmethod print-object ((task %decision-task) stream)
  (print-unreadable-object (task stream :type t)
    (format stream "~S" (aget (%task-payload task) :workflow-type))))

(defmethod print-object ((task %activity-task) stream)
  (print-unreadable-object (task stream :type t)
    (format stream "~S" (aget (%task-payload task) :activity-type))))


(defun worker-look-for-task (type)
  (set-worker-thread-status type "Looking for task.")
  (let ((task (ecase type
                (:workflow
                 (let ((response (swf::poll-for-decision-task :all-pages t
                                                              :identity (worker-identity *worker*)
                                                              :task-list (worker-task-list *worker*))))
                   (when (aget response :events)
                     (make-instance '%decision-task :payload response))))
                (:activity
                 (let ((response (swf::poll-for-activity-task :identity (worker-identity *worker*)
                                                              :task-list (worker-task-list *worker*))))
                   (when (aget response :task-token)
                     (make-instance '%activity-task :payload response)))))))
    (if task
        (log-trace "Got task ~S" task)
        (log-trace "Got no task."))
    task))


(defun worker-handle-task (type task)
  (set-worker-thread-status type "Handling task: ~S" task)
  (restart-case
      (destructuring-bind (function &rest args)
          (ecase type
            (:workflow
             (worker-compute-workflow-response task))
            (:activity
             (worker-compute-activity-response task)))
        (set-worker-thread-status type "Sending response: ~S" function)
        (flet ((do-retry (error)
                 (when *enable-debugging*
                   (with-simple-restart (continue "Log error and retry.")
                     (invoke-debugger error)))
                 (log-info "~S: An error occured while sending task reply, will retry after 5 seconds pause."
                           type)
                 (report-error error)
                 (sleep 5)
                 (invoke-restart 'retry)))
          (loop repeat 24 do
                (with-simple-restart (retry "Retry send task reponse")
                  (handler-bind ((swf::http-error #'do-retry)
                                 (swf::internal-failure-error #'do-retry)
                                 (swf::service-unavailable-error #'do-retry)
                                 (swf::throttling-error #'do-retry))
                    (apply function args)
                    (return))))))
    (terminate-workflow ()
      :report "Terminate this workflow execution."
      (swf::terminate-workflow-execution :details "Terminated by restart."
                                         :run-id (aget (%task-payload task) :workflow-execution :run-id)
                                         :workflow-id (aget (%task-payload task) :workflow-execution :workflow-id)))))


(defun find-task-type-in-package (package type-spec)
  (let ((task-type (get (find-symbol (aget type-spec :name) package) 'task-type)))
    (when (and task-type
               (equal (aget type-spec :version) (getf (task-type-options task-type) :version)))
      task-type)))


(defun find-task-type (x)
  (etypecase x
    (symbol
     (get x 'task-type))
    (cons
     (some (lambda (package)
             (find-task-type-in-package package x))
           (worker-packages *worker*)))))


(defun worker-ensure-task-types (worker)
  (dolist (package (worker-packages worker))
    (do-symbols (symbol package)
      (let ((task-type (get symbol 'task-type)))
        (when task-type
          (ensure-task-type task-type))))))


(defun ensure-task-types (package)
  (do-symbols (symbol (find-package package))
    (let ((task-type (get symbol 'task-type)))
      (when task-type
        (ensure-task-type task-type)))))


(defun find-workflow-type (x)
  (let ((workflow-type (find-task-type x)))
    (when (typep workflow-type 'workflow-type)
      workflow-type)))


(defun find-activity-type (x)
  (let ((activity-type (find-task-type x)))
    (when (typep activity-type 'activity-type)
      activity-type)))


;;; Defining workflows ----------------------------------------------------------------------------


(defun parse-options (options &rest list-options)
  (loop for (key . value) in options
        collect key
        collect (if (member key list-options)
                    value
                    (car value))))


(defmacro %define-workflow (name
                            (&rest lambda-list)
                               (&body options)
                            &body body)
  (destructuring-bind (&key ((:name external-name) name)
                            version
                            (timeout 10)
                            default-child-policy
                            default-execution-start-to-close-timeout
                            (default-task-list "default")
                            default-task-start-to-close-timeout
                            description
                            context)
      (parse-options options :context)
    (assert version () "Version is missing")
    (let* ((string-name (string external-name))
           (string-version (string version))
           (context (loop for var-def in context
                          collect (if (consp var-def)
                                      (cons (car var-def) (cadr var-def))
                                      (cons var-def nil)))))
      (multiple-value-bind (normalized-lambda-list args-list-form)
          (parse-lambda-list lambda-list)
        `(progn
           (defun ,name (,@normalized-lambda-list)
             (%start-workflow  (get ',name 'task-type) ,args-list-form))
           (setf (get ',name 'task-type)
                 (make-instance 'workflow-type
                                :name ',name
                                :function (lambda (,@lambda-list)
                                            ,@(loop for (key . init-form) in context
                                                    collect `(unless (%context-boundp ,key)
                                                               (setf (%context ,key) ,init-form)))
                                            (macrolet ((context (key)
                                                         (unless (member key ',context :key #'car)
                                                           (warn "Unknown context variable ~S" key))
                                                         `(%context ,key)))
                                              (block ,name
                                                ,@body)))
                                :timeout ,timeout
                                :options (list :name ,string-name
                                               :version ,string-version
                                               :default-child-policy
                                               ,default-child-policy
                                               :default-execution-start-to-close-timeout
                                               ,default-execution-start-to-close-timeout
                                               :default-task-list
                                               ,default-task-list
                                               :default-task-start-to-close-timeout
                                               ,default-task-start-to-close-timeout
                                               :description ,description))))))))


;;; Defining activities ----------------------------------------------------------------------------


(defmacro define-activity (name
                           (&rest lambda-list)
                              (&body options)
                           &body body)
  (destructuring-bind (&key ((:name external-name) name)
                            version
                            (default-task-heartbeat-timeout :none)
                            (default-task-list "default")
                            (default-task-schedule-to-close-timeout :none)
                            (default-task-schedule-to-start-timeout :none)
                            (default-task-start-to-close-timeout :none)
                            description)
      (parse-options options)
    (assert version () "Version is missing")
    (let ((string-name (string external-name))
          (string-version (string version)))
      (multiple-value-bind (normalized-lambda-list args-list-form)
          (parse-lambda-list lambda-list)
        `(progn
           (defun ,name (,@normalized-lambda-list)
             (%schedule-activity (get ',name 'task-type) ,args-list-form))
           (setf (get ',name 'task-type)
                 (make-instance 'activity-type
                                :name ',name
                                :function (lambda (,@lambda-list)
                                            (block ,name
                                              ,@body))
                                :options (list :name ,string-name
                                               :version ,string-version
                                               :default-task-heartbeat-timeout
                                               ,default-task-heartbeat-timeout
                                               :default-task-list
                                               ',default-task-list
                                               :default-task-schedule-to-close-timeout
                                               ,default-task-schedule-to-close-timeout
                                               :default-task-schedule-to-start-timeout
                                               ,default-task-schedule-to-start-timeout
                                               :default-task-start-to-close-timeout
                                               ,default-task-start-to-close-timeout
                                               :description ,description))))))))


;;; Handling workflow tasks ------------------------------------------------------------------------


(defun worker-compute-workflow-response (task)
  (multiple-value-bind (context decisions)
      (run-decision-task task)
    (list #'swf::respond-decision-task-completed
          :task-token (task-token task)
          :execution-context context
          :decisions decisions)))


(defun run-decision-task (task)
  (let* ((workflow-type (aget (%task-payload task) :workflow-type))
         (workflow (or (find-workflow-type workflow-type)
                       (error "Could find workflow type ~S." workflow-type)))
         (decider-function (task-type-function workflow )))
    (set-worker-thread-status :workflow "Handling ~S" workflow)
    (let ((*wx* (make-workflow-execution
                 :events (aget (%task-payload task) :events)
                 :previous-started-event-id (aget (%task-payload task) :previous-started-event-id)
                 :started-event-id (aget (%task-payload task) :started-event-id)
                 :workflow-id (aget (deserialize-slot
                                     :workflow-execution
                                     (aget (%task-payload task) :workflow-execution))
                                    :workflow-id)
                 :run-id (aget (deserialize-slot
                                     :workflow-execution
                                     (aget (%task-payload task) :workflow-execution))
                               :run-id))))
      (log-trace "Start with context: ~S" (slot-value *wx* 'context))
      (let ((input (event-input (get-event *wx* 1)))) ;; TODO: is workflow execution started event always first?
        (catch 'exit-decider
          (apply decider-function input)))
      (log-trace "Done with context: ~S" (slot-value *wx* 'context))
      (log-trace "Made ~S decision~:P." (length (slot-value *wx* 'decisions)))
      (values (serialize-object (slot-value *wx* 'context))
              (mapcar #'transform-decision (nreverse (slot-value *wx* 'decisions)))))))


;;; Handling activity tasks ------------------------------------------------------------------------


(defvar *task-token*)

(defun heartbeat (&optional details)
  (when (boundp '*task-token*)
      (getf (swf::record-activity-task-heartbeat :task-token *task-token*
                                                 :details (serialize-object details))
            :cancel-requested)))


(define-condition activity-error (error)
  ((reason :initarg :reason
           :reader activity-error-reason)
   (details :initarg :details
            :initform nil
            :reader activity-error-details)))


(defvar *default-activity-error-reason*)
(defvar *default-activity-error-details*)


(defun error-detail (key)
  (getf *default-activity-error-details* key))


(defun (setf error-detail) (value key)
  (if (error-detail key)
    (setf (getf *default-activity-error-details* key) value)
    (progn (push value *default-activity-error-details*)
           (push key *default-activity-error-details*)))
  value)


(defun worker-compute-activity-response (task)
  (handler-case
      (list #'swf::respond-activity-task-completed
            :result (serialize-object (compute-activity-task-value task))
            :task-token (task-token task))
    (activity-error (error)
      (list #'swf::respond-activity-task-failed
            :task-token (task-token task)
            :reason (serialize-object (activity-error-reason error))
            :details (serialize-object (activity-error-details error))))))


(defun read-new-value ()
  (format t "Enter a new value: ")
  (eval (read)))


(defun compute-activity-task-value (task)
  (let* ((*task-token* (task-token task))
         (activity-type (aget (%task-payload task) :activity-type))
         (activity (or (find-activity-type activity-type)
                       (error "Could not find activity type ~S." activity-type)))
         (input (deserialize-object (aget (%task-payload task) :input))))
    (set-worker-thread-status :activity "Handling ~S" activity)
    (apply-activity-task-function activity input)))


(defun apply-activity-task-function (activity args)
  (let ((*default-activity-error-reason* :error)
        (*default-activity-error-details* nil))
    (restart-case
        (handler-bind ((error
                        (lambda (error)
                          (unless (typep error 'activity-error)
                            (when *enable-debugging*
                              (with-simple-restart (continue "Log and wrap error in activity-error.")
                                (invoke-debugger error)))
                            (report-error error)
                            (error 'activity-error
                                   :reason *default-activity-error-reason*
                                   :details (list* :condition (format nil "~A" error)
                                                   *default-activity-error-details*))))))
          (apply (task-type-function activity) args))
      (use-value (&rest new-value)
        :report "Return something else."
        :interactive read-new-value
        new-value))))
