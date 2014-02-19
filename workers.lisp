(in-package :swf-workers)

(declaim (optimize (speed 0) (debug 3) (safety 3) (space 0)))


(defun make-uuid ()
  (with-open-file (in #p"/proc/sys/kernel/random/uuid")
    (read-line in)))


;;; Helpers for dealing with assoc objects

(defun aget (alist key &rest keys)
  (let ((value (cdr (assoc key alist))))
    (if keys
        (apply #'aget value (car keys) (cdr keys))
        value)))


(defun agetter (&rest keys)
  (lambda (alist)
    (apply #'aget alist keys)))


(defun alist (&rest data)
  (loop for (key value) on data by #'cddr
        collect (cons key value)))


;;; Serialization ---------------------------------------------------------------------------------


(defpackage #:%amazon-flow-serialization)


(defun serialize-object (object)
  (when object
    (with-standard-io-syntax
      (let ((*package* (find-package :%amazon-flow-serialization))
            (*print-circle* t))
        (prin1-to-string object)))))


(defun deserialize-object (string)
  (when string
    (with-standard-io-syntax
      (let ((*package* (find-package :%amazon-flow-serialization)))
        (read-from-string string)))))


;;; Defining workflows ----------------------------------------------------------------------------


(defmacro define-workflow (name
                           (&rest workflow-args)
                              (&key (version :1)
                                    default-child-policy
                                    default-execution-start-to-close-timeout
                                    (default-task-list (alist :name "default"))
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
                          :workflow-type (alist :name ,string-name :version ,string-version)))
       (setf (get ',name 'workflow)
             (alist :name ,string-name
                    :version ,string-version
                    :decider (lambda (task)
                               ,@body)
                    :options (list :name ,string-name
                                   :version ,string-version
                                   :default-child-policy ,default-child-policy
                                   :default-execution-start-to-close-timeout ,default-execution-start-to-close-timeout
                                   :default-task-list ',default-task-list
                                   :default-task-start-to-close-timeout ,default-task-start-to-close-timeout
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
                     :run-id (aget
                              (swf::start-workflow-execution
                               :child-policy child-policy
                               :execution-start-to-close-timeout execution-start-to-close-timeout
                               :input input-string
                               :tag-list tag-list
                               :task-list task-list
                               :task-start-to-close-timeout task-start-to-close-timeout
                               :workflow-id id
                               :workflow-type workflow-type)
                              :run-id)))
          (swf::workflow-execution-already-started-error (err)
            (when workflow-id
              (error err))))))


(defun find-workflow-in-package (package name version)
  (let ((symbol (find-symbol name package)))
    (when symbol
      (let ((workflow (get symbol 'workflow)))
        (when workflow
          (when (and (equal name (aget workflow :name))
                     (equal version (aget workflow :version)))
            workflow))))))


(defun find-workflow (packages name version)
  (let ((workflow (some (lambda (package)
                          (find-workflow-in-package package name version))
                        packages)))
    (or workflow
        (error "Could not find workflow ~A/~A in ~S" name version packages))))


(defun ensure-workflow-type (workflow)
  (handler-case
      (apply #'swf::register-workflow-type (aget workflow :options))
    (swf::type-already-exists-error ()
      ;; TODO check if options are equal
      )))


(defun ensure-workflow-types (&rest packages)
  (dolist (package packages)
    (do-symbols (symbol package)
      (let ((workflow (get symbol 'workflow)))
        (when workflow
          (ensure-workflow-type workflow))))))



;;; Defining activitys ----------------------------------------------------------------------------


(defmacro define-activity (name
                           (&rest activity-args)
                              (&key (version :1)
                                    (default-task-heartbeat-timeout :none)
                                    (default-task-list (alist :name "default"))
                                    (default-task-schedule-to-close-timeout :none)
                                    (default-task-schedule-to-start-timeout :none)
                                    (default-task-start-to-close-timeout :none)
                                    description)
                           &body body)
  (let ((string-name (string name))
        (string-version (string version)))
    `(progn
       (defun ,name (&key ,@activity-args
                       (activity-id (make-uuid))
                       control
                       heartbeat-timeout
                       schedule-to-close-timeout
                       schedule-to-start-timeout
                       start-to-close-timeout
                       task-list)
         (alist :decision-type 'swf::schedule-activity-task
                :schedule-activity-task-decision-attributes
                (alist :activity-id activity-id
                       :activity-type (alist :name ,string-name :version ,string-version)
                       :control control
                       :heartbeat-timeout heartbeat-timeout
                       :input (serialize-object
                               (list ,@(loop for arg in activity-args
                                             collect (intern (symbol-name arg) :keyword)
                                             collect arg)))
                       :schedule-to-close-timeout schedule-to-close-timeout
                       :schedule-to-start-timeout schedule-to-start-timeout
                       :start-to-close-timeout start-to-close-timeout
                       :task-list task-list)))
       (setf (get ',name 'activity)
             (alist :name ,string-name
                    :version ,string-version
                    :function (lambda (&key ,@activity-args)
                                ,@body)
                    :options (list :name ,string-name
                                   :version ,string-version
                                   :default-task-heartbeat-timeout ,default-task-heartbeat-timeout
                                   :default-task-list ',default-task-list
                                   :default-task-schedule-to-close-timeout ,default-task-schedule-to-close-timeout
                                   :default-task-schedule-to-start-timeout ,default-task-schedule-to-start-timeout
                                   :default-task-start-to-close-timeout ,default-task-start-to-close-timeout
                                   :description ,description))))))


(defun find-activity-in-package (package name version)
  (let ((symbol (find-symbol name package)))
    (when symbol
      (let ((activity (get symbol 'activity)))
        (when activity
          (when (and (equal name (aget activity :name))
                     (equal version (aget activity :version)))
            activity))))))


(defun find-activity (packages name version)
  (let ((activity (some (lambda (package)
                          (find-activity-in-package package name version))
                        packages)))
    (or activity
        (error "Could not find activity ~A/~A in ~S" name version packages))))


(defun ensure-activity-type (activity)
  (handler-case
      (apply #'swf::register-activity-type (aget activity :options))
    (swf::type-already-exists-error ()
      ;; TODO check if options are equal
      )))


(defun ensure-activity-types (&rest packages)
  (dolist (package packages)
    (do-symbols (symbol package)
      (let ((activity (get symbol 'activity)))
        (when activity
          (ensure-activity-type activity))))))



;;; Common worker ----------------------------------------------------------------------------------


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


(defgeneric worker-start (worker)
  (:method ((worker worker))
    (loop (worker-handle-next-task worker))))


(defgeneric worker-start-thread (worker)
  (:method ((worker worker))
    (sb-thread:make-thread (lambda (worker)
                             (worker-start worker))
                           :name (format nil "Worker for ~S" worker)
                           :arguments (list worker))))


(defgeneric worker-look-for-task (worker))
(defmethod worker-look-for-task :around (worker)
  (let ((swf::*service* (worker-service worker)))
    (call-next-method)))


(defgeneric worker-handle-next-task (worker)
  (:method ((worker worker))
    (with-error-handling
      (with-simple-restart (carry-on "Stop handle-next-task.")
        (let ((task (worker-look-for-task worker)))
          (when task
            ;;(break "Handling task ~S" worker)
            (with-simple-restart (carry-on "Stop handling this task.")
              (worker-handle-task worker task))))))))


(defgeneric worker-handle-task (worker task)
  (:method ((worker worker) task)
    (let ((swf::*service* (worker-service worker)))
      (restart-case
          (destructuring-bind (function &rest args)
              (worker-compute-task-response worker task)
            (apply function args))
        (retry ()
          :report "Retry handle task"
          (worker-handle-task worker task))
        (terminate-workflow ()
          :report "Terminate this workflow exectuion and all child workflows."
          (swf::terminate-workflow-execution :child-policy :terminate
                                             :details "Terminated by restart."
                                             :run-id (aget task :workflow-execution :run-id)
                                             :workflow-id (aget task :workflow-execution :workflow-id)))))))


(defgeneric worker-compute-task-response (worker task))



;;; Workflow worker --------------------------------------------------------------------------------


(defclass workflow-worker (worker)
  ())

;; TODO: Check for duplicate workflows in initialize-instance :after


(defmethod worker-look-for-task ((wfw workflow-worker))
  (let ((res (swf::poll-for-decision-task :all-pages t
                                          :identity (princ-to-string sb-thread:*current-thread*)
                                          :task-list (alist :name (worker-task-list wfw)))))
    (when (aget res :events)
      res)))


(defmethod worker-compute-task-response ((wfw workflow-worker) task)
  (list #'swf::respond-decision-task-completed
        :task-token (aget task :task-token)
        :decisions (run-decision-task wfw task)))


(defun run-decision-task (wfw task)
  (let* ((workflow-type (aget task :workflow-type))
         (workflow (find-workflow (worker-packages wfw)
                                  (aget workflow-type :name)
                                  (aget workflow-type :version)))
         (decider-function (aget workflow :decider)))
    (funcall decider-function task)))


;;; Activity worker --------------------------------------------------------------------------------


(defclass activity-worker (worker)
  ())

;; TODO: Check for duplicate activities in initialize-instance :after


(defmethod worker-look-for-task ((aw activity-worker))
  (let ((res (swf::poll-for-activity-task :identity (princ-to-string sb-thread:*current-thread*)
                                          :task-list (alist :name (worker-task-list aw)))))
    (when (aget res :task-token)
      res)))


(defmethod worker-compute-task-response ((aw activity-worker) task)
  (restart-case
      (let ((values (compute-activity-task-values aw task)))
        (list #'swf::respond-activity-task-completed
              :result (serialize-object values)
              :task-token (aget task :task-token)))
    (carry-on ()
      :report "Fail activity task"
      (list #'swf::respond-activity-task-failed
            :task-token (aget task :task-token)
            :reason "wrong" ;; TODO return condition object somehow
            :details "something wrong"))))


(defun read-new-values ()
  (format t "Enter a new value: ")
  (multiple-value-list (eval (read))))


(defun compute-activity-task-values (aw task)
  (restart-case
      (let* ((activity-type (aget task :activity-type))
             (activity (find-activity (worker-packages aw)
                                     (aget activity-type :name)
                                     (aget activity-type :version)))
            (input (deserialize-object (aget task :input))))
        (multiple-value-list (apply (aget activity :function) input)))
    (use-value (&rest new-values)
      :report "Return something else."
      :interactive read-new-values
      new-values)))
