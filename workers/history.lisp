(in-package #:swf-workers)


(declaim (optimize (speed 0) (space 0) (debug 3)))


(defclass history-event ()
  ((id :initarg :id
       :reader event-id)
   (timestamp :initarg :timestamp
              :reader event-timestamp)
   (task :initform nil
         :reader event-task)))


(defclass task ()
  ((id :initarg :id
       :initform nil
       :reader task-id)
   (states :initform (make-hash-table)
           :reader task-states)
   (events :initform nil
           :reader task-events)
   (old-events :initform nil
               :reader task-old-events)
   (new-events :initform nil
               :reader task-new-events)))


(defun task-state-event (task state)
  (first (gethash state  (task-states task))))


(defun task-state (task)
  (values (or (caar (task-events task)) :new)
          (or (caar (task-old-events task)) :new)))


(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t)
    (multiple-value-bind (new-state old-state) (task-state task)
      (format stream "~@[~S ~]~@[~A->~]~A ~A+~A=~A"
              (task-id task)
              (unless (eql old-state new-state) old-state)
              new-state
              (length (task-old-events task))
              (length (task-new-events task))
              (length (task-events task))))))


(defclass decision-task (task) ())
(defclass activity-task (task) ())
(defclass timer-task (task) ())
(defclass marker-task (task) ())
(defclass signal-task (task) ())
(defclass workflow-task (task) ())
(defclass child-workflow-task (workflow-task) ())


(defclass workflow-execution ()
  ((run-id :initarg :run-id)
   (workflow-id :initarg :workflow-id)
   (context :initform nil)
   (old-context :initform nil)
   (decisions :initform nil)
   (events :initarg :events)
   (new-events :initarg :new-events)
   (previous-started-event-id :initarg :previous-started-event-id)
   (started-event-id :initarg :started-event-id)
   (workflow-task :initform (make-instance 'workflow-task))
   (tasks :initform (make-hash-table :test #'equal))))


(defun make-workflow-execution (&key events previous-started-event-id
                                  started-event-id
                                  run-id
                                  workflow-id)
  (let* ((events (map 'vector #'make-history-event events))
         (wx (make-instance 'workflow-execution
                            :events events
                            :new-events (coerce (subseq events previous-started-event-id) 'list)
                            :previous-started-event-id previous-started-event-id
                            :started-event-id started-event-id
                            :workflow-id workflow-id
                            :run-id run-id)))
    (loop for event across events do
          (setf (slot-value event 'task) (index-event wx event)))
    wx))


(defun get-event (wx id)
  (aref (slot-value wx 'events) (1- id)))


;; Functions operating on current workflow execution (wx) ------------------------------------------


(defvar *wx*)


(defun workflow-task ()
  (slot-value *wx* 'workflow-task))


(defun started-timestamp ()
  "Timestamp when this worklfow execution started."
  (event-timestamp (task-state-event (workflow-task) :started)))


(defun current-timestamp ()
  "The current timestamp, ie. the timestamp when this decision task started."
  (event-timestamp (get-event *wx* (slot-value *wx* 'started-event-id))))


(defun current-runtime ()
  "The runtime of this workflow execution so far."
  (local-time:timestamp-difference (current-timestamp) (started-timestamp)))


(defun new-events ()
  (slot-value *wx* 'new-events))


(defun updated-tasks ()
  (remove nil (remove-duplicates (mapcar #'event-task (new-events)))))


(defun context (key &optional default)
  (getf (slot-value *wx* 'context) key default))


(defun (setf context) (new-value key &optional default)
  (setf (getf (slot-value *wx* 'context) key default) new-value))


;; Tasks -------------------------------------------------------------------------------------------


(defun add-task (wx task-type id)
  (setf (gethash (cons task-type id) (slot-value wx 'tasks))
        (make-instance task-type :id id)))


(defun find-task (wx task-type id)
  (if (eq 'workflow-task task-type)
      (slot-value wx 'workflow-task)
      (gethash (cons task-type id) (slot-value wx 'tasks))))


(defun update-task (wx event task-type id state)
  (let ((task (or (find-task wx task-type id)
                   (add-task wx task-type id)))
        (state+event (cons state event)))
    (push event (gethash state (task-states task)))
    (push state+event (slot-value task 'events))
    (if (< (slot-value wx 'previous-started-event-id) (event-id event))
        (push state+event (slot-value task 'new-events))
        (push state+event (slot-value task 'old-events)))
    task))


(defun new-task (wx event task-type id state)
  (add-task wx task-type id)
  (update-task wx event task-type id state))


(defgeneric index-event (wx event))
(defgeneric get-event-type (type))


(defun make-history-event (alist)
  (multiple-value-bind (class attrs-slot)
      (get-event-type (aget alist :event-type))
    (apply #'make-instance class
           :id (aget alist :event-id)
           :timestamp (aget alist :event-timestamp)
           (loop for (key . value ) in (aget alist attrs-slot)
                 collect key
                 collect (deserialize-slot key value)))))


(defmacro define-history-event (name slots &body body)
  `(progn
     (defclass ,name (history-event)
       ,(loop for slot-name in slots
              collect `(,slot-name
                        :initarg ,(intern (symbol-name slot-name) :keyword)
                        :initform nil
                        :reader ,(intern (format nil "EVENT-~A" slot-name)))))
     (defmethod get-event-type ((type (eql ,(intern (subseq (symbol-name name) 0
                                                            (- (length (symbol-name name)) 6))
                                                    :keyword))))
       (values ',name
               ,(intern (format nil "~A-ATTRIBUTES" name) :keyword)))
     (defmethod index-event (wx (event ,name))
       (with-slots (id ,@slots)
           event
         (flet ((%new-task (task-type id state)
                  (new-task wx event task-type id state))
                (%update-task (task-type id state)
                  (update-task wx event task-type id state))
                (%get-event (event-id)
                  (get-event wx event-id)))
           (declare (ignorable (function %new-task)
                               (function %update-task)
                               (function %get-event)))
           ,@body)))))


;; Marker events -----------------------------------------------------------------------------------


(define-history-event marker-recorded-event
    (decision-task-completed-event-id
     details
     marker-name)
  (%new-task 'marker-task marker-name :recorded))


(define-history-event record-marker-failed-event
    (cause
     decision-task-completed-event-id
     marker-name)
  (%new-task 'marker-task marker-name :record-failed))


;; Signal events -----------------------------------------------------------------------------------


(define-history-event workflow-execution-signaled-event
    (external-initiated-event-id
     external-workflow-execution
     input
     signal-name)
  (%new-task 'signal-task signal-name :signaled))


;; Timer events ------------------------------------------------------------------------------------


(define-history-event timer-started-event
    (control
     decision-task-completed-event-id
     start-to-fire-timeout
     timer-id)
  (%new-task 'timer-task timer-id :started))


(define-history-event start-timer-failed-event
    (cause
     decision-task-completed-event-id
     timer-id)
  (%new-task 'timer-task timer-id :start-failed))


(define-history-event timer-fired-event
    (started-event-id
     timer-id)
  (%update-task 'timer-task timer-id :fired))


(define-history-event timer-canceled-event
    (decision-task-completed-event-id
     started-event-id
     timer-id)
  (%update-task 'timer-task timer-id :canceled))


(define-history-event cancel-timer-failed-event
    (cause
     timer-id)
  (%update-task 'timer-task timer-id :cancel-failed))


;; Workflow events -------------------------------------------------------------------------


(define-history-event workflow-execution-started-event
    (child-policy
     continued-execution-run-id
     execution-start-to-close-timeout
     input
     parent-initiated-event-id
     parent-workflow-execution
     tag-list
     task-list
     task-start-to-close-timeout
     workflow-type)
  (%update-task 'workflow-task nil :started))


(define-history-event workflow-execution-completed-event
    (decision-task-completed-event-id
     result)
  (%update-task 'workflow-task nil :completed))


(define-history-event workflow-execution-failed-event
    (decision-task-completed-event-id
     details
     reason)
  (%update-task 'workflow-task nil :failed))


(define-history-event workflow-execution-timed-out-event
    (child-policy
     timeout-type)
  (%update-task 'workflow-task nil :timed-out))


(define-history-event workflow-execution-canceled-event
    (decision-task-completed-event-id
     details)
  (%update-task 'workflow-task nil :canceled))


(define-history-event workflow-execution-terminated-event
    (cause
     child-policy
     details
     reason)
  (%update-task 'workflow-task nil :terminated))


(define-history-event workflow-execution-continued-as-new-event
    (child-policy
     decision-task-completed-event-id
     execution-start-to-close-timeout
     input
     new-execution-run-id
     tag-list
     task-list
     task-start-to-close-timeout
     workflow-type)
  (%update-task 'workflow-task nil :continued-as-new))


(define-history-event continue-as-new-workflow-execution-failed-event
    (cause
     decision-task-completed-event-id)
  (%update-task 'workflow-task nil :continue-as-new-failed))


(define-history-event workflow-execution-cancel-requested-event
    (cause
     external-initiated-event-id
     external-workflow-execution)
  (%update-task 'workflow-task nil :cancel-requested))


;; Decision events -------------------------------------------------------------------------


(define-history-event decision-task-scheduled-event
    (start-to-close-timeout
     task-list)
  (%new-task 'decision-task id :scheduled))


(define-history-event decision-task-started-event
    (identity
     scheduled-event-id)
  (%update-task 'decision-task scheduled-event-id :started))


(define-history-event decision-task-completed-event
    (execution-context
     scheduled-event-id
     started-event-id)
  (when execution-context
    (setf (slot-value wx 'old-context) (copy-tree execution-context))
    (setf (slot-value wx 'context) execution-context))
  (%update-task 'decision-task scheduled-event-id :completed))


(define-history-event decision-task-timed-out-event
    (scheduled-event-id
     started-event-id
     timeout-type)
  (%update-task 'decision-task scheduled-event-id :timed-out))


;; Activity events -------------------------------------------------------------------------


(define-history-event activity-task-scheduled-event
    (activity-id
     activity-type
     control
     decision-task-completed-event-id
     heartbeat-timeout
     input
     schedule-to-close-timeout
     schedule-to-start-timeout
     start-to-close-timeout
     task-list)
  (%new-task 'activity-task activity-id :scheduled))


(define-history-event schedule-activity-task-failed-event
    (activity-id
     activity-type
     cause
     decision-task-completed-event-id)
  (%new-task 'activity-task activity-id :schedule-failed))


(define-history-event activity-task-started-event
    (identity
     scheduled-event-id)
  (%update-task 'activity-task (event-activity-id (%get-event scheduled-event-id)) :started))


(define-history-event activity-task-completed-event
    (result
     scheduled-event-id
     started-event-id)
  (%update-task 'activity-task (event-activity-id (%get-event scheduled-event-id)) :completed))


(define-history-event activity-task-failed-event
    (details
     reason
     scheduled-event-id
     started-event-id)
  (%update-task 'activity-task (event-activity-id (%get-event scheduled-event-id)) :failed))


(define-history-event activity-task-timed-out-event
    (details
     scheduled-event-id
     started-event-id
     timeout-type)
  (%update-task 'activity-task (event-activity-id (%get-event scheduled-event-id)) :timed-out))


(define-history-event activity-task-canceled-event
    (details
     latest-cancel-requested-event-id
     scheduled-event-id
     started-event-id)
  (%update-task 'activity-task (event-activity-id (%get-event scheduled-event-id)) :canceled))


(define-history-event activity-task-cancel-requested-event
    (activity-id
     decision-task-completed-event-id)
  (%update-task 'activity-task activity-id :cancel-requested))


(define-history-event request-cancel-activity-task-failed-event
    (activity-id
     cause
     decision-task-completed-event-id)
  (%new-task 'activity-task activity-id :request-cancel-failed))


;; Child workflow -------------------------------------------------------------------------


(define-history-event start-child-workflow-execution-initiated-event
    (child-policy
     control
     decision-task-completed-event-id
     execution-start-to-close-timeout
     input
     tag-list
     task-list
     task-start-to-close-timeout
     workflow-id
     workflow-type)
  (%new-task 'child-workflow-task workflow-id :initiated))


(define-history-event start-child-workflow-execution-failed-event
    (cause
     control
     decision-task-completed-event-id
     initiated-event-id
     workflow-id
     workflow-type)
  (%new-task 'child-workflow-task workflow-id :start-failed))


(define-history-event child-workflow-execution-started-event
    (initiated-event-id
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow (aget workflow-execution :workflow-id) :started))


(define-history-event child-workflow-execution-completed-event
    (initiated-event-id
     result
     started-event-id
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow (aget workflow-execution :workflow-id) :completed))


(define-history-event child-workflow-execution-failed-event
    (details
     initiated-event-id
     reason
     started-event-id
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow (aget workflow-execution :workflow-id) :failed))


(define-history-event child-workflow-execution-timed-out-event
    (initiated-event-id
     started-event-id
     timeout-type
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow (aget workflow-execution :workflow-id) :timed-out))


(define-history-event child-workflow-execution-canceled-event
    (details
     initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow (aget workflow-execution :workflow-id) :canceled))


(define-history-event child-workflow-execution-terminated-event
    (initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow (aget workflow-execution :workflow-id) :terminated))


;; External workflow ----------------------------------------------------------------------


(define-history-event signal-external-workflow-execution-initiated-event ; TODO
    (control
     decision-task-completed-event-id
     input
     run-id
     signal-name
     workflow-id))


(define-history-event external-workflow-execution-signaled-event ; TODO
    (initiated-event-id
     workflow-execution))


(define-history-event signal-external-workflow-execution-failed-event ; TODO
    (cause
     control
     decision-task-completed-event-id
     initiated-event-id
     run-id
     workflow-id))


(define-history-event request-cancel-external-workflow-execution-initiated-event ; TODO
    (control
     decision-task-completed-event-id
     run-id
     workflow-id))


(define-history-event external-workflow-execution-cancel-requested-event ; TODO
    (initiated-event-id
     workflow-execution))


(define-history-event request-cancel-external-workflow-execution-failed-event ; TODO
    (cause
     control
     decision-task-completed-event-id
     initiated-event-id
     run-id
     workflow-id))
