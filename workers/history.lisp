(in-package #:swf-workers)


(declaim (optimize (speed 0) (space 0) (debug 3)))


(defclass history-event ()
  ((id :initarg :id
       :reader event-id)
   (timestamp :initarg :timestamp
              :reader event-timestamp)))

(defgeneric get-event-type (type))
(defgeneric update-history-with-event (event))
(defgeneric transform-event-slot (event-type slot value))

(defun make-history-event (alist)
  (multiple-value-bind (class attrs-slot)
      (get-event-type (aget alist :event-type))
    (apply #'make-instance class
           :id (aget alist :event-id)
           :timestamp (aget alist :event-timestamp)
           (loop for (key . value ) in (aget alist attrs-slot)
                 collect key
                 collect (transform-event-slot class key value)))))


(defvar *wx*)

(defvar *current-event-id*)


(defclass task ()
  ((state :initarg :state
          :initform nil
          :reader task-state)
   (previous-state :initarg :previous-state
                   :initform nil
                   :reader task-previous-state)
   (scheduled-event-id :initarg :scheduled-event-id
                       :initform nil
                       :reader task-scheduled-event-id)
   (started-event-id :initform nil
                     :reader task-started-event-id)
   (closed-event-id :initform nil
                    :reader task-closed-event-id)
   (request-cancel-event-ids :initform nil
                             :reader task-request-cancel-event-ids)))


(defclass decision-task (task) ())
(defclass activity-task (task) ())
(defclass workflow-task (task) ())
(defclass child-workflow-task (workflow-task) ())
(defclass timer-task (task) ())


(defmethod task-scheduled-event (task)
  (when (task-scheduled-event-id task)
    (get-event (task-scheduled-event-id task))))

(defmethod task-started-event (task)
  (when (task-started-event-id task)
    (get-event (task-started-event-id task))))

(defmethod task-closed-event (task)
  (when (task-closed-event-id task)
    (get-event (task-closed-event-id task))))


(defclass workflow-execution-info (workflow-task)
  ((decisions :initform nil)
   (events :initarg :events)
   (previous-started-event-id :initarg :previous-started-event-id)
   (started-event-id :initarg :started-event-id)
   (activity-tasks :initform (make-hash-table :test #'equal))
   (decision-tasks :initform (make-hash-table))
   (timer-tasks :initform (make-hash-table :test #'equal))
   (child-workflow-tasks :initform (make-hash-table :test #'equal))))


(defun make-workflow-execution-info (&key events previous-started-event-id
                                       started-event-id)
  (let ((*wx* (make-instance 'workflow-execution-info
                             :events (map 'vector #'make-history-event events)
                             :previous-started-event-id previous-started-event-id
                             :started-event-id started-event-id)))
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


(defun %add-task (type &optional id)
  (let ((tasks (get-tasks-table type)))
    (assert (null (gethash id tasks)) () "Program error: duplicate task id ~S ~S" type id)
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
  (when (< *current-event-id* (slot-value *wx* 'previous-started-event-id))
    (setf (slot-value *task* 'previous-state) new-state)))

(defun %schedule ()
  (slot-value *task* 'scheduled-event-id) *current-event-id*)

(defun %start ()
  (setf (slot-value *task* 'started-event-id) *current-event-id*))

(defun %close ()
  (setf (slot-value *task* 'closed-event-id) *current-event-id*))

(defun %request-cancel ()
  (push *current-event-id* (slot-value *task* 'request-cancel-event-ids)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun normalize-slot (slot)
    (if (consp slot)
        slot
        (list slot 'identity))))

(defmacro define-history-event (name slots &body body)
  (let ((slots (mapcar #'normalize-slot slots)))
    `(progn
       (defclass ,name (history-event)
         ,(loop for (slot-name) in slots
                collect `(,slot-name
                          :initarg ,(intern (symbol-name slot-name) :keyword)
                          :initform nil
                          :reader ,(intern (format nil "EVENT-~A" slot-name)))))
       (defmethod get-event-type ((type (eql ,(intern (subseq (symbol-name name) 0
                                                              (- (length (symbol-name name)) 6))
                                                      :keyword))))
         (values ',name
                 ,(intern (format nil "~A-ATTRIBUTES" name) :keyword)))
       ,@(loop for (slot-name transformer) in slots collect
               `(defmethod transform-event-slot ((event-type (eql ',name))
                                                 (slot (eql ,(intern (symbol-name slot-name) :keyword)))
                                                 value)
                  (,transformer value)))
       (defmethod update-history-with-event ((event ,name))
         (with-slots (id ,@(mapcar #'car slots))
             event
           (let ((*current-event-id* id))
             ,@body))))))


;; Workflow events -------------------------------------------------------------------------


(define-history-event workflow-execution-started-event
    (child-policy
     continued-execution-run-id
     execution-start-to-close-timeout
     (input deserialize-object)
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
     (result deserialize-object))
  (with-task (workflow-task)
    (%close)
    (%state :closed)))


(define-history-event workflow-execution-failed-event
    (decision-task-completed-event-id
     (details deserialize-object)
     (reason deserialize-keyword))
  (with-task (workflow-task)
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
     (details deserialize-object))
  (with-task (workflow-task)
    (%close)
    (%state :canceled)))


(define-history-event workflow-execution-terminated-event
    (cause
     child-policy
     (details deserialize-object)
     (reason deserialize-keyword))
  (with-task (workflow-task)
    (%close)
    (%state :terminated)))


(define-history-event workflow-execution-continued-as-new-event
    (child-policy
     decision-task-completed-event-id
     execution-start-to-close-timeout
     (input deserialize-object)
     new-execution-run-id
     tag-list
     task-list
     task-start-to-close-timeout
     workflow-type)
  (with-task (workflow-task)
    (%close)
    (%state :continued-as-new)))


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
    ((identity deserialize-keyword)
     scheduled-event-id)
  (with-task (decision-task scheduled-event-id)
    (%start)
    (%state :started)))


(define-history-event decision-task-completed-event
    (scheduled-event-id
     started-event-id)
  (with-task (decision-task scheduled-event-id)
    (%close)
    (%state :completed)))


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
     (activity-type find-activity-type)
     (control deserialize-object)
     decision-task-completed-event-id
     heartbeat-timeout
     (input deserialize-object)
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
    ((result deserialize-object)
     scheduled-event-id
     started-event-id)
  (with-task (activity-task (event-activity-id (get-event scheduled-event-id)))
    (%close)
    (%state :completed)))


(define-history-event activity-task-failed-event
    ((details deserialize-object)
     (reason deserialize-keyword)
     scheduled-event-id
     started-event-id)
  (with-task (activity-task (event-activity-id (get-event scheduled-event-id)))
    (%close)
    (%state :failed)))


(define-history-event activity-task-timed-out-event
    ((details deserialize-object)
     scheduled-event-id
     started-event-id
     timeout-type)
  (with-task (activity-task (event-activity-id (get-event scheduled-event-id)))
    (%close)
    (%state :timed-out)))


(define-history-event activity-task-canceled-event
    ((details deserialize-object)
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
     (input deserialize-object)
     (signal-name deserialize-keyword)))


(define-history-event marker-recorded-event ; TODO
    (decision-task-completed-event-id
     (details deserialize-object)
     (marker-name deserialize-keyword)))


;; Timer events -------------------------------------------------------------------------


(define-history-event timer-started-event
    ((control deserialize-object)
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
     (control deserialize-object)
     decision-task-completed-event-id
     execution-start-to-close-timeout
     (input deserialize-object)
     tag-list
     task-list
     task-start-to-close-timeout
     workflow-id
     workflow-type
     workflow-execution)
  (with-new-task (child-workflow workflow-execution)
    (%schedule)
    (%state :scheduled)))


(define-history-event start-child-workflow-execution-failed-event ; TODO
    (cause
     (control deserialize-object)
     decision-task-completed-event-id
     initiated-event-id
     workflow-id
     workflow-type
     workflow-execution))


(define-history-event child-workflow-execution-started-event
    (initiated-event-id
     workflow-execution
     workflow-type)
  (with-task (child-workflow workflow-execution)
    (%start)
    (%state :started)))


(define-history-event child-workflow-execution-completed-event
    (initiated-event-id
     (result deserialize-object)
     started-event-id
     workflow-execution
     workflow-type)
  (with-task (child-workflow workflow-execution)
    (%close)
    (%state :completed)))


(define-history-event child-workflow-execution-failed-event
    ((details deserialize-object)
     initiated-event-id
     (reason deserialize-keyword)
     started-event-id
     workflow-execution
     workflow-type)
  (with-task (child-workflow workflow-execution)
    (%close)
    (%state :failed)))


(define-history-event child-workflow-execution-timed-out-event
    (initiated-event-id
     started-event-id
     timeout-type
     workflow-execution
     workflow-type)
  (with-task (child-workflow workflow-execution)
    (%close)
    (%state :timed-out)))


(define-history-event child-workflow-execution-canceled-event
    ((details deserialize-object)
     initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (with-task (child-workflow workflow-execution)
    (%close)
    (%state :canceled)))


(define-history-event child-workflow-execution-terminated-event
    (initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (with-task (child-workflow workflow-execution)
    (%close)
    (%state :terminated)))


;; External workflow ----------------------------------------------------------------------


(define-history-event signal-external-workflow-execution-initiated-event ; TODO
    ((control deserialize-object)
     decision-task-completed-event-id
     (input deserialize-object)
     run-id
     (signal-name deserialize-keyword)
     workflow-id))


(define-history-event external-workflow-execution-signaled-event ; TODO
    (initiated-event-id
     workflow-execution))


(define-history-event signal-external-workflow-execution-failed-event ; TODO
    (cause
     (control deserialize-object)
     decision-task-completed-event-id
     initiated-event-id
     run-id
     workflow-id))


(define-history-event request-cancel-external-workflow-execution-initiated-event ; TODO
    ((control deserialize-object)
     decision-task-completed-event-id
     run-id
     workflow-id))


(define-history-event external-workflow-execution-cancel-requested-event ; TODO
    (initiated-event-id
     workflow-execution))


(define-history-event request-cancel-external-workflow-execution-failed-event ; TODO
    (cause
     (control deserialize-object)
     decision-task-completed-event-id
     initiated-event-id
     run-id
     workflow-id))
