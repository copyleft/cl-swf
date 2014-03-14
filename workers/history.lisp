(in-package #:swf-workers)


(declaim (optimize (speed 0) (space 0) (debug 3)))


(defclass history-event ()
  ((id :initarg :id
       :reader event-id)
   (timestamp :initarg :timestamp
              :reader event-timestamp)))

(defgeneric get-event-type (type))
(defgeneric update-history-with-event (event))

(defun make-history-event (alist)
  (multiple-value-bind (class attrs-slot)
      (get-event-type (aget alist :event-type))
    (apply #'make-instance class
           :id (aget alist :event-id)
           :timestamp (aget alist :event-timestamp)
           (loop for (key . value ) in (aget alist attrs-slot)
                 collect key
                 collect (deserialize-slot key value)))))


(defvar *wx*)

(defvar *current-event*)

(defclass task ()
  ((state :initarg :state
          :initform nil
          :reader task-state)
   (previous-state :initarg :previous-state
                   :initform nil
                   :reader task-previous-state)
   (scheduled-event :initarg :scheduled-event
                       :initform nil
                       :reader task-scheduled-event)
   (started-event :initform nil
                     :reader task-started-event)
   (closed-event :initform nil
                    :reader task-closed-event)
   (request-cancel-events :initform nil
                             :reader task-request-cancel-events)))


(defclass decision-task (task) ())
(defclass activity-task (task) ())
(defclass workflow-task (task) ())
(defclass child-workflow-task (workflow-task) ())
(defclass timer-task (task) ())


(defclass workflow-execution-info (workflow-task)
  ((run-id :initarg :run-id)
   (workflow-id :initarg :workflow-id)
   (context :initform nil)
   (old-context :initform nil)
   (decisions :initform nil)
   (events :initarg :events)
   (previous-started-event-id :initarg :previous-started-event-id)
   (started-event-id :initarg :started-event-id)
   (activity-tasks :initform (make-hash-table :test #'equal))
   (decision-tasks :initform (make-hash-table))
   (timer-tasks :initform (make-hash-table :test #'equal))
   (child-workflow-tasks :initform (make-hash-table :test #'equal))
   (markers :initform (make-hash-table))))


(defun started-timestamp ()
  "Timestamp when this worklfow execution started."
  (event-timestamp (task-started-event *wx*)))


(defun current-timestamp ()
  "The current timestamp, ie. the timestamp when this decision task started."
  (event-timestamp (get-event (slot-value *wx* 'started-event-id))))


(defun current-runtime ()
  "The runtime of this workflow execution so far."
  (local-time:timestamp-difference (current-timestamp) (started-timestamp)))


(defun new-events ()
  (with-slots (events previous-started-event-id) *wx*
    (coerce (subseq events previous-started-event-id) 'list)))


(defun context (key &optional default)
  (getf (slot-value *wx* 'context) key default))


(defun (setf context) (new-value key &optional default)
  (setf (getf (slot-value *wx* 'context) key default) new-value))


(defun %record-marker (marker-name)
  (push *current-event* (gethash marker-name (slot-value *wx* 'markers))))


(defun get-marker-events (marker-name)
  (gethash marker-name (slot-value *wx* 'markers)))


(defun marker-details (marker-name &optional default)
  (let ((event (first (get-marker-events marker-name))))
    (if event
        (values (event-details event) t)
        (values default nil))))


(defun make-workflow-execution-info (&key events previous-started-event-id
                                       started-event-id
                                       run-id
                                       workflow-id)
  (let ((*wx* (make-instance 'workflow-execution-info
                             :events (map 'vector #'make-history-event events)
                             :previous-started-event-id previous-started-event-id
                             :started-event-id started-event-id
                             :workflow-id workflow-id
                             :run-id run-id)))
    (map nil #'update-history-with-event (slot-value *wx* 'events))
    *wx*))


(defun get-event (id)
  (with-slots (events) *wx*
    (aref events (1- id))))


(defvar *task*)


(defun get-tasks-table (type)
  (slot-value *wx* (ecase type
                     (activity-task 'activity-tasks)
                     (decision-task 'decision-tasks)
                     (timer-task 'timer-tasks)
                     (child-workflow-task 'child-workflow-tasks))))


(defun get-task (type &optional id error-p)
  (case type
    (workflow-task *wx*)
    (otherwise
     (or (gethash id (get-tasks-table type))
         (when error-p
           (error "Task not found: ~S ~S" type id))))))


(defun get-timer (id)
  (get-task 'timer-task id))


(defun get-activity (id)
  (get-task 'activity-task id))


(defun %add-task (type &optional id)
  (let ((tasks (get-tasks-table type)))
    (let ((task (make-instance type)))
      (setf (gethash id tasks) task)
      task)))

(defmacro with-new-task ((type &optional id) &body body)
  `(let ((*task* (%add-task ',type ,id)))
     ,@body))

(defmacro with-task ((type &optional id) &body body)
  `(let ((*task* (get-task ',type ,id)))
     ,@body))

(defun %state (new-state)
  (setf (slot-value *task* 'state) new-state)
  (when (< (event-id *current-event*) (slot-value *wx* 'previous-started-event-id))
    (setf (slot-value *task* 'previous-state) new-state)))

(defun %schedule ()
  (slot-value *task* 'scheduled-event) *current-event*)

(defun %start ()
  (setf (slot-value *task* 'started-event) *current-event*))

(defun %close ()
  (setf (slot-value *task* 'closed-event) *current-event*))

(defun %request-cancel ()
  (push *current-event* (slot-value *task* 'request-cancel-events)))


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
     (defmethod update-history-with-event ((*current-event* ,name))
       (with-slots (id ,@slots)
           *current-event*
         ,@body))))


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
  (with-task (workflow-task)
    (%start)
    (%state :started)))


(define-history-event workflow-execution-completed-event
    (decision-task-completed-event-id
     result)
  (with-task (workflow-task)
    (%close)
    (%state :closed)))


(define-history-event workflow-execution-failed-event
    (decision-task-completed-event-id
     details
     reason)
  (with-new-task (workflow-task)
    (%close)
    (%state :failed)))


(define-history-event workflow-execution-timed-out-event
    (child-policy
     timeout-type)
  (with-task (workflow-task)
    (%close)
    (%state :timed-out)))


(define-history-event workflow-execution-canceled-event
    (decision-task-completed-event-id
     details)
  (with-task (workflow-task)
    (%close)
    (%state :canceled)))


(define-history-event workflow-execution-terminated-event
    (cause
     child-policy
     details
     reason)
  (with-task (workflow-task)
    (%close)
    (%state :terminated)))


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
  (with-task (workflow-task)
    (%close)
    (%state :continued-as-new)))


(define-history-event continue-as-new-workflow-execution-failed-event
    (cause
     decision-task-completed-event-id))


(define-history-event workflow-execution-cancel-requested-event
    (cause
     external-initiated-event-id
     external-workflow-execution)
  (with-task (workflow-task)
    (%request-cancel)))


;; Decision events -------------------------------------------------------------------------


(define-history-event decision-task-scheduled-event
    (start-to-close-timeout
     task-list)
  (with-new-task (decision-task id)
    (%schedule)
    (%state :scheduled)))


(define-history-event decision-task-started-event
    (identity
     scheduled-event-id)
  (with-task (decision-task scheduled-event-id)
    (%start)
    (%state :started)))


(define-history-event decision-task-completed-event
    (execution-context
     scheduled-event-id
     started-event-id)
  (with-task (decision-task scheduled-event-id)
    (%close)
    (%state :completed)
    (when execution-context
      (setf (slot-value *wx* 'old-context) (copy-tree execution-context))
      (setf (slot-value *wx* 'context) execution-context))))


(define-history-event decision-task-timed-out-event
    (scheduled-event-id
     started-event-id
     timeout-type)
  (with-task (decision-task scheduled-event-id)
    (%close)
    (%state :timed-out)))


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
  (with-new-task (activity-task activity-id)
    (%schedule)
    (%state :scheduled)))


(define-history-event schedule-activity-task-failed-event ; TODO
    (activity-id
     activity-type
     cause
     decision-task-completed-event-id))


(define-history-event activity-task-started-event
    (identity
     scheduled-event-id)
  (with-task (activity-task (event-activity-id (get-event scheduled-event-id)))
    (%start)
    (%state :started)))


(define-history-event activity-task-completed-event
    (result
     scheduled-event-id
     started-event-id)
  (with-task (activity-task (event-activity-id (get-event scheduled-event-id)))
    (%close)
    (%state :completed)))


(define-history-event activity-task-failed-event
    (details
     reason
     scheduled-event-id
     started-event-id)
  (with-task (activity-task (event-activity-id (get-event scheduled-event-id)))
    (%close)
    (%state :failed)))


(define-history-event activity-task-timed-out-event
    (details
     scheduled-event-id
     started-event-id
     timeout-type)
  (with-task (activity-task (event-activity-id (get-event scheduled-event-id)))
    (%close)
    (%state :timed-out)))


(define-history-event activity-task-canceled-event
    (details
     latest-cancel-requested-event-id
     scheduled-event-id
     started-event-id)
  (with-task (activity-task (event-activity-id (get-event scheduled-event-id)))
    (%close)
    (%state :canceled)))


(define-history-event activity-task-cancel-requested-event
    (activity-id
     decision-task-completed-event-id)
  (with-task (activity-task activity-id)
    (%request-cancel)))


(define-history-event request-cancel-activity-task-failed-event ; TODO
    (activity-id
     cause
     decision-task-completed-event-id))


;; Misc events -------------------------------------------------------------------------


(define-history-event workflow-execution-signaled-event ; TODO
    (external-initiated-event-id
     external-workflow-execution
     input
     signal-name))


(define-history-event marker-recorded-event ; TODO
    (decision-task-completed-event-id
     details
     marker-name))


;; Timer events -------------------------------------------------------------------------


(define-history-event timer-started-event
    (control
     decision-task-completed-event-id
     start-to-fire-timeout
     timer-id)
  (with-new-task (timer-task timer-id)
    (%start)
    (%state :started)))


(define-history-event start-timer-failed-event ; TODO
    (cause
     decision-task-completed-event-id
     timer-id))


(define-history-event timer-fired-event
    (started-event-id
     timer-id)
  (with-task (timer-task timer-id)
    (%close)
    (%state :fired)))


(define-history-event timer-canceled-event
    (decision-task-completed-event-id
     started-event-id
     timer-id)
  (with-task (timer-task timer-id)
    (%close)
    (%state :canceled)))


(define-history-event cancel-timer-failed-event ; TODO
    (cause
     timer-id))


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
  (with-new-task (child-workflow-task workflow-id)
    (%schedule)
    (%state :scheduled)))


(define-history-event start-child-workflow-execution-failed-event ; TODO
    (cause
     control
     decision-task-completed-event-id
     initiated-event-id
     workflow-id
     workflow-type))


(define-history-event child-workflow-execution-started-event
    (initiated-event-id
     workflow-execution
     workflow-type)
  (with-task (child-workflow-task (aget workflow-execution :workflow-id))
    (%start)
    (%state :started)))


(define-history-event child-workflow-execution-completed-event
    (initiated-event-id
     result
     started-event-id
     workflow-execution
     workflow-type)
  (with-task (child-workflow-task (aget workflow-execution :workflow-id))
    (%close)
    (%state :completed)))


(define-history-event child-workflow-execution-failed-event
    (details
     initiated-event-id
     reason
     started-event-id
     workflow-execution
     workflow-type)
  (with-task (child-workflow-task (aget workflow-execution :workflow-id))
    (%close)
    (%state :failed)))


(define-history-event child-workflow-execution-timed-out-event
    (initiated-event-id
     started-event-id
     timeout-type
     workflow-execution
     workflow-type)
  (with-task (child-workflow-task (aget workflow-execution :workflow-id))
    (%close)
    (%state :timed-out)))


(define-history-event child-workflow-execution-canceled-event
    (details
     initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (with-task (child-workflow-task (aget workflow-execution :workflow-id))
    (%close)
    (%state :canceled)))


(define-history-event child-workflow-execution-terminated-event
    (initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (with-task (child-workflow-task (aget workflow-execution :workflow-id))
    (%close)
    (%state :terminated)))


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
