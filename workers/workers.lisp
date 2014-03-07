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
   (packages :initarg :packages
             :initform (list *package*)
             :reader worker-packages)))

(defun set-worker-thread-status (format-control &rest args)
  (log-trace "SWF-WORKER: ~?" format-control args)
  (setf (sb-thread:thread-name sb-thread:*current-thread*)
        (format nil "SWF-WORKER: ~?" format-control args)))


(defun worker-start (worker type)
  (with-error-handling
    (let ((inter-task-pause 0))
      (loop
       (handler-case
           (progn
             (worker-handle-next-task worker type)
             (setf inter-task-pause (max 0 (1- inter-task-pause))))
         (swf::throttling-error (e)
           (setf inter-task-pause (min (* 60 60 2) (+ 10 (random 20) inter-task-pause)))
           (log-warn "~S: Throttling: ~S" type (swf::swf-error-action e))))
       (set-worker-thread-status "~S: Inter task pause: ~D seconds" type inter-task-pause)
       (sleep inter-task-pause)))))


(defun worker-start-thread (worker type)
  (sb-thread:make-thread (lambda (worker type)
                           (worker-start worker type))
                         :name (format nil "~S worker loop" type)
                         :arguments (list worker type)))


(defun worker-look-for-task (worker type)
  (set-worker-thread-status "~S: Looking for task." type)
  (let ((swf::*service* (worker-service worker)))
    (let ((identity (princ-to-string sb-thread:*current-thread*)))
      (let ((task (ecase type
                    (:workflow
                     (let ((response (swf::poll-for-decision-task :all-pages t
                                                                  :identity identity
                                                                  :task-list (worker-task-list worker))))
                       (when (aget response :events)
                         response)))
                    (:activity
                     (let ((response (swf::poll-for-activity-task :identity identity
                                                                  :task-list (worker-task-list worker))))
                       (when (aget response :task-token)
                         response))))))
        (if task
            (log-trace "~S: Got task for ~S" type (aget task :workflow-execution))
            (log-trace "~S: Got no task." type))
        task))))


(defun worker-handle-next-task (worker type)
  (with-simple-restart (carry-on "Stop handle-next-task.")
    (let ((task (worker-look-for-task worker type)))
      (when task
        (with-simple-restart (carry-on "Stop handling this ~A task." type)
          (worker-handle-task worker type task))))))


(defun worker-handle-task (worker type task)
  (set-worker-thread-status "~S: Handling task." type)
  (let ((*worker* worker)
        (swf::*service* (worker-service worker)))
    (restart-case
        (destructuring-bind (function &rest args)
            (ecase type
              (:workflow
               (worker-compute-workflow-response task))
              (:activity
               (worker-compute-activity-response task)))
          (set-worker-thread-status "~S: Sending response: ~S" type function)
          (apply function args))
      (retry ()
        :report "Retry handle task"
        (worker-handle-task worker type task))
      (terminate-workflow ()
        :report "Terminate this workflow exectuion and all child workflows."
        (swf::terminate-workflow-execution :child-policy :terminate
                                           :details "Terminated by restart."
                                           :run-id (aget task :workflow-execution :run-id)
                                           :workflow-id (aget task :workflow-execution :workflow-id))))))


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
                       child-policy
                       execution-start-to-close-timeout
                       tag-list
                       task-list
                       task-start-to-close-timeout
                       workflow-id)
         (%start-workflow :child-policy child-policy
                          :execution-start-to-close-timeout execution-start-to-close-timeout
                          :input (list ,@(loop for arg in workflow-args
                                               collect (intern (symbol-name arg) :keyword)
                                               collect arg))
                          :tag-list tag-list
                          :task-list task-list
                          :task-start-to-close-timeout task-start-to-close-timeout
                          :workflow-id workflow-id
                          :workflow-type (get ',name 'task-type)))
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


(defun %start-workflow (&key child-policy
                          execution-start-to-close-timeout
                          input
                          tag-list
                          task-list
                          task-start-to-close-timeout
                          workflow-id
                          workflow-type)
  (loop with input-string = (serialize-object input)
        for id-bits from 16 by 8
        for id = (or workflow-id
                     (format nil "~(~36R~)" (random (expt 2 id-bits))))
        do
        (handler-case
            (return
              (alist :workflow-id id
                     :run-id (swf::start-workflow-execution
                              :child-policy child-policy
                              :execution-start-to-close-timeout execution-start-to-close-timeout
                              :input input-string
                              :tag-list tag-list
                              :task-list task-list
                              :task-start-to-close-timeout task-start-to-close-timeout
                              :workflow-id id
                              :workflow-type (serialize-task-type workflow-type))))
          (swf::workflow-execution-already-started-error (err)
            (when workflow-id
              (error err))))))


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
          :task-token (aget task :task-token)
          :execution-context context
          :decisions decisions)))


(defun run-decision-task (task)
  (let* ((workflow-type (aget task :workflow-type))
         (workflow (or (find-workflow-type workflow-type)
                       (error "Could find workflow type ~S." workflow-type)))
         (decider-function (task-type-function workflow)))
    (set-worker-thread-status ":WORKFLOW: Handling ~S" workflow)
    (let ((*wx* (make-workflow-execution-info
                 :events (aget task :events)
                 :previous-started-event-id (aget task :previous-started-event-id)
                 :started-event-id (aget task :started-event-id))))
      (log-trace ":WORKFLOW: ~S start with context: ~S" workflow (slot-value *wx* 'context))
      (apply decider-function (event-input (task-started-event *wx*)))
      (log-trace ":WORKFLOW: ~S done with context: ~S" workflow (slot-value *wx* 'context))
      (log-trace ":WORKFLOW: ~S made ~S decision~:P." workflow (length (slot-value *wx* 'decisions)))
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
      (let ((*default-activity-error-reason* :error)
            (*default-activity-error-details* nil))
        (let (error)
          (restart-case
              (handler-bind ((error (lambda (e) (setf error e))))
                (let ((value (compute-activity-task-value task)))
                  (list #'swf::respond-activity-task-completed
                        :result (serialize-object value)
                        :task-token (aget task :task-token))))
            (carry-on ()
              :report "Fail activity"
              (error 'activity-error
                     :reason *default-activity-error-reason*
                     :details (list* :condition (format nil "~A" error)
                                     *default-activity-error-details*))))))
    (activity-error (error)
      (list #'swf::respond-activity-task-failed
            :task-token (aget task :task-token)
            :reason (serialize-object (activity-error-reason error))
            :details (serialize-object (activity-error-details error))))))


(defun read-new-value ()
  (format t "Enter a new value: ")
  (eval (read)))


(defun compute-activity-task-value (task)
  (restart-case
      (let* ((activity-type (aget task :activity-type))
             (activity (or (find-activity-type activity-type)
                           (error "Could not find activity type ~S." activity-type)))
             (input (deserialize-object (aget task :input))))
        (set-worker-thread-status ":ACTIVITY: Handling ~S" activity)
        (apply (task-type-function activity) input))
    (use-value (&rest new-value)
      :report "Return something else."
      :interactive read-new-value
      new-value)))
