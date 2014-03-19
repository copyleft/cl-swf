(in-package :swf-workers)

(declaim (optimize (speed 0) (debug 3) (safety 3) (space 0)))

;; TODO: Check for duplicate workflow and activties types in a worker

;;; Common worker ----------------------------------------------------------------------------------


(defvar *worker*)


(defclass worker ()
  ((service :initarg :service
            :initform (swf::service)
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
        (format nil "~A-WORKER: ~?" type format-control args)))


(defun worker-start (worker type)
  (with-log-context type
    (loop for error = (worker-handle-next-task worker type)
          do (when error
               (set-worker-thread-status type "Pause due to error")
               (sleep 15)))))


(defun worker-start-thread (worker type)
  (sb-thread:make-thread (lambda (worker type)
                           (worker-start worker type))
                         :name (format nil "~A-WORKER: Init" type)
                         :arguments (list worker type)))


(defun worker-handle-next-task (worker type)
  (let ((*worker* worker)
        (swf::*service* (worker-service worker)))
    (handler-bind ((error
                    (lambda (error)
                      (unless (typep error 'activity-error)
                        (when *enable-debugging*
                          (with-simple-restart (continue "Log error and look for next task.")
                            (invoke-debugger error)))
                        (report-error error)
                        (return-from worker-handle-next-task error)))))
      (let ((task (worker-look-for-task type)))
        (when task
          (with-log-context (task-workflow-id task)
            (with-log-context (let ((ttype (or (aget (%task-payload task) :workflow-type)
                                               (aget (%task-payload task) :activity-type))))
                                (format nil "~A/~A" (aget ttype :name) (aget ttype :version)))
              (worker-handle-task type task)
              (values nil t))))))))


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
                 (log-info "~S: An error occured while sending task reply, will retry after 15 seconds pause."
                           type)
                 (report-error error)
                 (invoke-restart 'retry)))
          (loop repeat 40 do
                (with-simple-restart (retry "Retry send task reponse")
                  (handler-bind ((swf::http-error #'do-retry)
                                 (swf::internal-failure-error #'do-retry)
                                 (swf::service-unavailable-error #'do-retry)
                                 (swf::throttling-error #'do-retry))
                    (apply function args)
                    (return)))
                (sleep 15))))
    (terminate-workflow ()
      :report "Terminate this workflow exectuion and all child workflows."
      (swf::terminate-workflow-execution :child-policy :terminate
                                         :details "Terminated by restart."
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


(defun find-workflow-type (x)
  (let ((workflow-type (find-task-type x)))
    (when (typep workflow-type 'workflow-type)
      workflow-type)))


(defun find-activity-type (x)
  (let ((activity-type (find-task-type x)))
    (when (typep activity-type 'activity-type)
      activity-type)))


;;; Defining workflows ----------------------------------------------------------------------------


(defmacro define-workflow (name
                           (&rest workflow-args)
                              (&key (version :1)
                                    (timeout 10)
                                    default-child-policy
                                    default-execution-start-to-close-timeout
                                    (default-task-list "default")
                                    default-task-start-to-close-timeout
                                    description)
                           &body body)
  (let ((string-name (string name))
        (string-version (string version)))
    `(progn
       (defun ,name (&key ,@workflow-args
                       control
                       child-policy
                       execution-start-to-close-timeout
                       tag-list
                       task-list
                       task-start-to-close-timeout
                       workflow-id)
         (if (boundp '*wx*)
             (start-child-workflow-execution-decision
              :control control
              :child-policy child-policy
              :execution-start-to-close-timeout execution-start-to-close-timeout
              :input (list ,@(loop for arg in workflow-args
                                   collect (intern (symbol-name arg) :keyword)
                                   collect arg))
              :tag-list tag-list
              :task-list task-list
              :task-start-to-close-timeout task-start-to-close-timeout
              :workflow-id workflow-id
              :workflow-type (get ',name 'task-type))
             (swf::start-workflow-execution
              :child-policy child-policy
              :execution-start-to-close-timeout execution-start-to-close-timeout
              :input (serialize-object
                      (list ,@(loop for arg in workflow-args
                                    collect (intern (symbol-name arg) :keyword)
                                    collect arg)))
              :tag-list tag-list
              :task-list task-list
              :task-start-to-close-timeout task-start-to-close-timeout
              :workflow-id (serialize-slot :workflow-id workflow-id)
              :workflow-type (serialize-slot :workflow-type (get ',name 'task-type)))))
       (setf (get ',name 'task-type)
             (make-instance 'workflow-type
                            :name ',name
                            :function (lambda (&key ,@workflow-args)
                                        ,@body)
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
                                           :description ,description))))))


;;; Defining activities ----------------------------------------------------------------------------


(defmacro define-activity (name
                           (&rest activity-args)
                              (&key (version :1)
                                    (default-task-heartbeat-timeout :none)
                                    (default-task-list "default")
                                    (default-task-schedule-to-close-timeout :none)
                                    (default-task-schedule-to-start-timeout :none)
                                    (default-task-start-to-close-timeout :none)
                                    description)
                           &body body)
  (let ((string-name (string name))
        (string-version (string version)))
    `(progn
       (defun ,name (&key ,@activity-args
                       activity-id
                       control
                       heartbeat-timeout
                       schedule-to-close-timeout
                       schedule-to-start-timeout
                       start-to-close-timeout
                       task-list)
         (schedule-activity-task-decision
          :activity-id activity-id
          :activity-type (get ',name 'task-type)
          :control control
          :heartbeat-timeout heartbeat-timeout
          :input (list ,@(loop for arg in activity-args
                               collect (intern (symbol-name arg) :keyword)
                               collect arg))
          :schedule-to-close-timeout schedule-to-close-timeout
          :schedule-to-start-timeout schedule-to-start-timeout
          :start-to-close-timeout start-to-close-timeout
          :task-list task-list))
       (setf (get ',name 'task-type)
             (make-instance 'activity-type
                            :name ',name
                            :function (lambda (&key ,@activity-args)
                                        ,@body)
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
                                           :description ,description))))))


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
                               :workflow-id))))
      (log-trace "Start with context: ~S" (slot-value *wx* 'context))
      (let ((input (event-input (slot-value (workflow-task) 'started-event))))
        (dolist (*event* (new-events))
          (cond ((slot-value *wx* 'ignore-events)
                 (log-trace "Ignoring new event: ~S" *event*))
                (t
                 (log-trace "Processing new event: ~S" *event*)
                 (log-trace "Task: ~S ~S" (event-task-event-slot *event*) (event-task *event*))
                 (apply decider-function input)
                 (log-trace "Context: ~S" (slot-value *wx* 'context))))))
      (log-trace "Made ~S decision~:P." (length (slot-value *wx* 'decisions)))
      (values (unless (equal (slot-value *wx* 'old-context) (slot-value *wx* 'context))
                (serialize-object (slot-value *wx* 'context)))
              (mapcar #'transform-decision (nreverse (slot-value *wx* 'decisions)))))))


;;; Handling activity tasks ------------------------------------------------------------------------


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
  (let ((*default-activity-error-reason* :error)
        (*default-activity-error-details* nil))
    (restart-case
        (handler-bind ((error
                        (lambda (error)
                          (unless (typep error 'activity-error)
                            (with-simple-restart (continue "Log and wrap error in activity-error.")
                              (invoke-debugger error))
                            (report-error error)
                            (error 'activity-error
                                   :reason *default-activity-error-reason*
                                   :details (list* :condition (format nil "~A" error)
                                                   *default-activity-error-details*))))))
          (let* ((activity-type (aget (%task-payload task) :activity-type))
                 (activity (or (find-activity-type activity-type)
                               (error "Could not find activity type ~S." activity-type)))
                 (input (deserialize-object (aget (%task-payload task) :input))))
            (set-worker-thread-status :activity "Handling ~S" activity)
            (apply (task-type-function activity) input)))
      (use-value (&rest new-value)
        :report "Return something else."
        :interactive read-new-value
        new-value))))
