(in-package #:swf-workers)


(declaim (optimize (speed 0) (space 0) (debug 3)))


(defclass event ()
  ((id :initarg :id
       :reader event-id)
   (timestamp :initarg :timestamp
              :reader event-timestamp)
   (is-new :initarg :is-new
           :reader event-is-new)
   (task :initform nil
         :reader event-task)
   (task-event-slot :initform nil
                    :reader event-task-event-slot)))


(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t)
    (format stream "~A~:[~;*~] ~A"
            (event-id event)
            (event-is-new event)
            (event-timestamp event))))


(defclass task ()
  ((id :initarg :id
       :initform nil
       :reader task-id)
   (new-state :initform nil
              :reader task-new-state)
   (old-state :initform nil
              :reader task-old-state)
   (events :initform nil
           :reader task-events)))


(defun task-state (task)
  (values (or (task-new-state task)
              (task-old-state task)
              :new)
          (or (task-old-state task)
              :new)))


(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t)
    (multiple-value-bind (state old-state) (task-state task)
      (format stream "~@[~S ~]~@[~A->~]~A ~A+~A=~A"
              (task-id task)
              (unless (eql old-state state) old-state)
              state
              (count-if-not #'event-is-new (task-events task))
              (count-if #'event-is-new (task-events task))
              (length (task-events task))))))


(defclass workflow-execution ()
  ((run-id :initarg :run-id)
   (workflow-id :initarg :workflow-id)
   (context :initform nil)
   (old-context :initform nil)
   (decisions :initform nil)
   (ignore-events :initform nil)
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
  (let* ((events (map 'vector (lambda (event)
                                (make-event previous-started-event-id event))
                      events))
         (wx (make-instance 'workflow-execution
                            :events events
                            :new-events (coerce (subseq events previous-started-event-id) 'list)
                            :previous-started-event-id previous-started-event-id
                            :started-event-id started-event-id
                            :workflow-id workflow-id
                            :run-id run-id)))
    (loop for event across events do
          (index-event wx event))
    wx))


(defun get-event (wx id)
  (aref (slot-value wx 'events) (1- id)))


;; Functions operating on current workflow execution (wx) and event  -------------------------------


(defvar *wx*)
(defvar *event*)

(defun new-events ()
  (slot-value *wx* 'new-events))


(defun ignore-remaining-events ()
  (setf (slot-value *wx* 'ignore-events) t))


(defun updated-tasks ()
  (remove nil (remove-duplicates (mapcar #'event-task (new-events)))))


(defun context (key &optional default)
  (getf (slot-value *wx* 'context) key default))


(defun (setf context) (new-value key &optional default)
  (setf (getf (slot-value *wx* 'context) key default) new-value))


(defmacro with-context ((&rest vars) &body body)
  `(symbol-macrolet
       (,@(loop for var-def in vars
                for name = (if (consp var-def) (first var-def) var-def)
                for keyword = (intern (string name) :keyword)
                for default = (when (consp var-def) (second var-def))
                collect `(,name (context ,keyword ,default))))
     ,@body))


(defun started-timestamp ()
  "Timestamp when this worklfow execution started."
  (event-timestamp (slot-value (workflow-task) 'started-event)))


(defun current-timestamp ()
  "The current timestamp, ie. the timestamp when this decision task started."
  (event-timestamp (get-event *wx* (slot-value *wx* 'started-event-id))))


(defun current-runtime ()
  "The runtime of this workflow execution so far."
  (local-time:timestamp-difference (current-timestamp) (started-timestamp)))


(defun retract-decisions ()
  (log-trace "Retracting ~D decisions" (length (slot-value *wx* 'decisions)))
  (setf (slot-value *wx* 'decisions) nil))

(defun workflow-task ()
  (slot-value *wx* 'workflow-task))

(defun is-cancel-requested? ()
  (slot-boundp (workflow-task) 'cancel-requested-event))

(defun activity-result ()
  ;; TODO: works only during completed event
  (event-result *event*))

;; Current event / task functions

(defun task ()
  (event-task *event*))

(defun type? (task-type)
    (typep (task) task-type))

(defun id? (id)
  (equal id (task-id (task))))

(defun event? (event)
  (eq event (event-task-event-slot *event*)))


;; Tests for event types


(defun recorded? () (event? 'recorded-event))
(defun failed? () (event? 'failed-event))
(defun signaled? () (event? 'signaled-event))
(defun started? () (event? 'started-event))
(defun fired? () (event? 'fired-event))
(defun canceled? () (event? 'canceled-event))
(defun cancel-failed? () (event? 'cancel-failed-event))
(defun completed? () (event? 'completed-event))
(defun timed-out? () (event? 'timed-out-event))
(defun terminated-event? () (event? 'terminated-event))
(defun continued-as-new? () (event? 'continued-as-new-event))
(defun continue-as-new-failed? () (event? 'continue-as-new-failed-event))
(defun cancel-requested? () (event? 'cancel-requested-event))
(defun scheduled? () (event? 'scheduled-event))
(defun schedule-failed? () (event? 'schedule-failed-event))
(defun request-cancel-failed? () (event? 'request-cancel-failed-event))
(defun initiated? () (event? 'initiated-event))
(defun start-failed? () (event? 'start-failed-event))


;; Tests for task types


(defun marker? (&key id)
  (and (type? 'marker-task)
       (or (null id)
           (id? id))))

(defun signal? (&key id)
  (and (type? 'signal-task)
       (or (null id)
           (id? id))))

(defun timer? (&key id)
  (and (type? 'timer-task)
       (or (null id)
           (id? id))))

(defun workflow? ()
  (type? 'workflow-task))

(defun decision? ()
  (type? 'decision-task))

(defun activity? (&key id)
  (and (type? 'activity-task)
       (or (null id)
           (id? id))))

(defun child-workflow? (&key id)
  (and (type? 'child-workflow-task)
       (or (null id)
           (id? id))))

;; Tasks -------------------------------------------------------------------------------------------


(defun add-task (wx task-type id)
  (setf (gethash (cons task-type id) (slot-value wx 'tasks))
        (make-instance task-type :id id)))


(defun find-task (wx task-type id)
  (if (eq 'workflow-task task-type)
      (slot-value wx 'workflow-task)
      (gethash (cons task-type id) (slot-value wx 'tasks))))


(defun update-task (wx event task-type id event-slot)
  (let ((task (or (find-task wx task-type id)
                  (add-task wx task-type id))))
    (push event (slot-value task 'events))
    (setf (slot-value task event-slot) event)
    (setf (slot-value event 'task) task)
    (setf (slot-value event 'task-event-slot) event-slot)
    task))


(defun new-task (wx event task-type id state)
  (add-task wx task-type id)
  (update-task wx event task-type id state))


(defgeneric index-event (wx event))
(defgeneric get-event-type (type))


(defun make-event (previous-started-event-id alist)
  (multiple-value-bind (class attrs-slot)
      (get-event-type (aget alist :event-type))
    (let ((id (aget alist :event-id)))
      (apply #'make-instance class
             :id id
             :timestamp (aget alist :event-timestamp)
             :is-new (< previous-started-event-id id)
             (loop for (key . value ) in (aget alist attrs-slot)
                   collect key
                   collect (deserialize-slot key value))))))


(defmacro define-event (name short-name slots &body body)
  `(progn
     (defclass ,name (event)
       ((short-name :initform ',short-name
                    :reader event-short-name
                    :allocation :class)
        ,@(loop for slot-name in slots
                collect `(,slot-name
                          :initarg ,(intern (symbol-name slot-name) :keyword)
                          :initform nil
                          :reader ,(intern (format nil "EVENT-~A" slot-name))))))
     (defmethod get-event-type ((type (eql ,(intern (subseq (symbol-name name) 0
                                                            (- (length (symbol-name name)) 6))
                                                    :keyword))))
       (values ',name
               ,(intern (format nil "~A-ATTRIBUTES" name) :keyword)))
     (defmethod index-event (wx (event ,name))
       (with-slots (id ,@slots)
           event
         (flet ((%new-task (task-type id)
                  (new-task wx event task-type id ',short-name))
                (%update-task (task-type id)
                  (update-task wx event task-type id ',short-name))
                (%get-event (event-id)
                  (get-event wx event-id)))
           (declare (ignorable (function %new-task)
                               (function %update-task)
                               (function %get-event)))
           ,@body)))))


;; Marker events -----------------------------------------------------------------------------------


(defclass marker-task (task)
  ((recorded-event)
   (failed-event)))


(define-event marker-recorded-event recorded-event
    (decision-task-completed-event-id
     details
     marker-name)
  (%new-task 'marker-task marker-name))


(define-event record-marker-failed-event failed-event
    (cause
     decision-task-completed-event-id
     marker-name)
  (%new-task 'marker-task marker-name))


;; Signal events -----------------------------------------------------------------------------------


(defclass signal-task (task)
  ((signaled-event)))


(define-event workflow-execution-signaled-event signaled-event
    (external-initiated-event-id
     external-workflow-execution
     input
     signal-name)
  (%new-task 'signal-task signal-name))


;; Timer events ------------------------------------------------------------------------------------


(defclass timer-task (task)
  ((started-event)
   (failed-event)
   (fired-event)
   (canceled-event)
   (cancel-failed-event)))


(define-event timer-started-event started-event
    (control
     decision-task-completed-event-id
     start-to-fire-timeout
     timer-id)
  (%new-task 'timer-task timer-id))


(define-event start-timer-failed-event failed-event
    (cause
     decision-task-completed-event-id
     timer-id)
  (%new-task 'timer-task timer-id))


(define-event timer-fired-event fired-event
    (started-event-id
     timer-id)
  (%update-task 'timer-task timer-id))


(define-event timer-canceled-event canceled-event
    (decision-task-completed-event-id
     started-event-id
     timer-id)
  (%update-task 'timer-task timer-id))


(define-event cancel-timer-failed-event cancel-failed-event
    (cause
     timer-id)
  (%update-task 'timer-task timer-id))


;; Workflow events -------------------------------------------------------------------------


(defclass workflow-task (task)
  ((started-event)
   (completed-event)
   (failed-event)
   (timed-out-event)
   (canceled-event)
   (terminated-event)
   (continued-as-new-event)
   (continue-as-new-failed-event)
   (cancel-requested-event)))


(define-event workflow-execution-started-event started-event
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
  (%update-task 'workflow-task nil))


(define-event workflow-execution-completed-event completed-event
    (decision-task-completed-event-id
     result)
  (%update-task 'workflow-task nil))


(define-event workflow-execution-failed-event failed-event
    (decision-task-completed-event-id
     details
     reason)
  (%update-task 'workflow-task nil))


(define-event workflow-execution-timed-out-event timed-out-event
    (child-policy
     timeout-type)
  (%update-task 'workflow-task nil))


(define-event workflow-execution-canceled-event canceled-event
    (decision-task-completed-event-id
     details)
  (%update-task 'workflow-task nil))


(define-event workflow-execution-terminated-event terminated-event
    (cause
     child-policy
     details
     reason)
  (%update-task 'workflow-task nil))


(define-event workflow-execution-continued-as-new-event continued-as-new-event
    (child-policy
     decision-task-completed-event-id
     execution-start-to-close-timeout
     input
     new-execution-run-id
     tag-list
     task-list
     task-start-to-close-timeout
     workflow-type)
  (%update-task 'workflow-task nil))


(define-event continue-as-new-workflow-execution-failed-event continue-as-new-failed-event
    (cause
     decision-task-completed-event-id)
  (%update-task 'workflow-task nil))


(define-event workflow-execution-cancel-requested-event cancel-requested-event
    (cause
     external-initiated-event-id
     external-workflow-execution)
  (%update-task 'workflow-task nil))


;; Decision events -------------------------------------------------------------------------


(defclass decision-task (task)
  ((scheduled-event)
   (started-event)
   (completed-event)
   (timed-out-event)))


(define-event decision-task-scheduled-event scheduled-event
    (start-to-close-timeout
     task-list)
  (%new-task 'decision-task id))


(define-event decision-task-started-event started-event
    (identity
     scheduled-event-id)
  (%update-task 'decision-task scheduled-event-id))


(define-event decision-task-completed-event completed-event
    (execution-context
     scheduled-event-id
     started-event-id)
  (when execution-context
    (setf (slot-value wx 'old-context) (copy-tree execution-context))
    (setf (slot-value wx 'context) execution-context))
  (%update-task 'decision-task scheduled-event-id))


(define-event decision-task-timed-out-event timed-out-event
    (scheduled-event-id
     started-event-id
     timeout-type)
  (%update-task 'decision-task scheduled-event-id))


;; Activity events -------------------------------------------------------------------------


(defclass activity-task (task)
  ((scheduled-event)
   (schedule-failed-event)
   (started-event)
   (completed-event)
   (failed-event)
   (timed-out-event)
   (canceled-event)
   (cancel-requested-event)
   (request-cancel-failed-event)))


(define-event activity-task-scheduled-event scheduled-event
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
  (%new-task 'activity-task activity-id))


(define-event schedule-activity-task-failed-event schedule-failed-event
    (activity-id
     activity-type
     cause
     decision-task-completed-event-id)
  (%new-task 'activity-task activity-id))


(define-event activity-task-started-event started-event
    (identity
     scheduled-event-id)
  (%update-task 'activity-task (event-activity-id (%get-event scheduled-event-id))))


(define-event activity-task-completed-event completed-event
    (result
     scheduled-event-id
     started-event-id)
  (%update-task 'activity-task (event-activity-id (%get-event scheduled-event-id))))


(define-event activity-task-failed-event failed-event
    (details
     reason
     scheduled-event-id
     started-event-id)
  (%update-task 'activity-task (event-activity-id (%get-event scheduled-event-id))))


(define-event activity-task-timed-out-event timed-out-event
    (details
     scheduled-event-id
     started-event-id
     timeout-type)
  (%update-task 'activity-task (event-activity-id (%get-event scheduled-event-id))))


(define-event activity-task-canceled-event canceled-event
    (details
     latest-cancel-requested-event-id
     scheduled-event-id
     started-event-id)
  (%update-task 'activity-task (event-activity-id (%get-event scheduled-event-id))))


(define-event activity-task-cancel-requested-event cancel-requested-event
    (activity-id
     decision-task-completed-event-id)
  (%update-task 'activity-task activity-id))


(define-event request-cancel-activity-task-failed-event request-cancel-failed-event
    (activity-id
     cause
     decision-task-completed-event-id)
  (%new-task 'activity-task activity-id))


;; Child workflow -------------------------------------------------------------------------


(defclass child-workflow-task (task)
  ((initiated-event)
   (start-failed-event)
   (started-event)
   (completed-event)
   (failed-event)
   (timed-out-event)
   (canceled-event)
   (terminated-event)))


(define-event start-child-workflow-execution-initiated-event initiated-event
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
  (%new-task 'child-workflow-task workflow-id))


(define-event start-child-workflow-execution-failed-event start-failed-event
    (cause
     control
     decision-task-completed-event-id
     initiated-event-id
     workflow-id
     workflow-type)
  (%new-task 'child-workflow-task workflow-id))


(define-event child-workflow-execution-started-event started-event
    (initiated-event-id
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow-task (aget workflow-execution :workflow-id)))


(define-event child-workflow-execution-completed-event completed-event
    (initiated-event-id
     result
     started-event-id
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow-task (aget workflow-execution :workflow-id)))


(define-event child-workflow-execution-failed-event failed-event
    (details
     initiated-event-id
     reason
     started-event-id
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow-task (aget workflow-execution :workflow-id)))


(define-event child-workflow-execution-timed-out-event timed-out-event
    (initiated-event-id
     started-event-id
     timeout-type
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow-task (aget workflow-execution :workflow-id)))


(define-event child-workflow-execution-canceled-event canceled-event
    (details
     initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow-task (aget workflow-execution :workflow-id)))


(define-event child-workflow-execution-terminated-event terminated-event
    (initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (%update-task 'child-workflow-task (aget workflow-execution :workflow-id)))


;; Signal external workflow ----------------------------------------------------------------------


(defclass signal-external-task (task)
  ((initiated-event)
   (signaled-event)
   (failed-event)))


(define-event signal-external-workflow-execution-initiated-event initiated-event
    (control
     decision-task-completed-event-id
     input
     run-id
     signal-name
     workflow-id)
  (%new-task 'signal-external-task id))


(define-event external-workflow-execution-signaled-event signaled-event
    (initiated-event-id
     workflow-execution)
  (%update-task 'signal-external-task initiated-event-id))


(define-event signal-external-workflow-execution-failed-event failed-event
    (cause
     control
     decision-task-completed-event-id
     initiated-event-id
     run-id
     workflow-id)
  (%update-task 'signal-external-task initiated-event-id))


;; Cancel external workflow ----------------------------------------------------------------------


(defclass cancel-external-task (task)
  ((initiated-event)
   (cancel-requested-event)
   (failed-event)))


(define-event request-cancel-external-workflow-execution-initiated-event initiated-event
    (control
     decision-task-completed-event-id
     run-id
     workflow-id)
  (%new-task 'cancel-external-task id))


(define-event external-workflow-execution-cancel-requested-event cancel-requested-event
    (initiated-event-id
     workflow-execution)
  (%update-task 'cancel-external-task initiated-event-id))


(define-event request-cancel-external-workflow-execution-failed-event failed-event
    (cause
     control
     decision-task-completed-event-id
     initiated-event-id
     run-id
     workflow-id)
  (%update-task 'cancel-external-task 'initiated-event-id))
