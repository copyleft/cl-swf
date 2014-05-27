(in-package #:swf-workers)


(declaim (optimize (speed 0) (space 0) (debug 3)))


(defclass event ()
  ((id :initarg :id
       :reader event-id)
   (timestamp :initarg :timestamp
              :reader event-timestamp)
   (is-new :initarg :is-new
           :reader event-is-new)))


(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t)
    (format stream "~A~:[~;*~] ~A"
            (event-id event)
            (event-is-new event)
            (event-timestamp event))))


(defclass workflow-execution ()
  ((run-id :initarg :run-id)
   (workflow-id :initarg :workflow-id)
   (context :initform nil)
   (decisions :initform nil)
   (events :initarg :events)
   (new-events :initarg :new-events)
   (previous-started-event-id :initarg :previous-started-event-id)
   (started-event-id :initarg :started-event-id)
   (task-events :initform nil)))


(defun make-workflow-execution (&key events
                                  previous-started-event-id
                                  started-event-id
                                  run-id
                                  workflow-id)
  (let* ((events (map 'vector (lambda (event)
                                (make-event previous-started-event-id event))
                      events))
         (*wx* (make-instance 'workflow-execution
                              :events events
                              :new-events (when previous-started-event-id
                                            (coerce (subseq events previous-started-event-id) 'list))
                              :previous-started-event-id previous-started-event-id
                              :started-event-id started-event-id
                              :workflow-id workflow-id
                              :run-id run-id)))
    (loop for event across events do
          (index-event *wx* event))
    *wx*))


(defun get-event (wx id)
  (aref (slot-value wx 'events) (1- id)))


(defun get-workflow-execution (execution)
  (make-workflow-execution
   :workflow-id (aget execution :workflow-id)
   :run-id (aget execution :run-id)
   :events (aget (swf::get-workflow-execution-history
                  :all-pages t
                  :execution (serialize-slot :workflow-execution execution))
                 :events)))


(defmacro with-workflow-execution ((worker execution) &body body)
  `(let* ((*worker* ,worker)
          (swf::*service* (worker-service *worker*))
          (*wx* (get-workflow-execution ,execution)))
     ,@body))


;; Functions operating on current workflow execution (wx) and event  -------------------------------


(defvar *wx*)
(defvar *event*)


(defun new-events ()
  (slot-value *wx* 'new-events))


(defun %context (key)
  (getf (slot-value *wx* 'context) key))


(defun %context-boundp (key)
  (not (eq '%%unbound (getf (slot-value *wx* 'context) key '%%unbound))))


(defun (setf %context) (new-value key)
  (setf (getf (slot-value *wx* 'context) key) new-value))


(defmacro with-context ((&rest vars) &body body)
  `(symbol-macrolet
       (,@(loop for var in vars
                for keyword = (intern (string var) :keyword)
                collect `(,var (context ,keyword))))
     ,@body))


(defun started-timestamp ()
  "Timestamp when this worklfow execution started."
  ;; TODO
  ;;(event-timestamp (slot-value (workflow-task) 'started-event))
  )


(defun current-timestamp ()
  "The current timestamp, ie. the timestamp when this decision task started."
  (event-timestamp (get-event *wx* (slot-value *wx* 'started-event-id))))


(defun current-runtime ()
  "The runtime of this workflow execution so far."
  (local-time:timestamp-difference (current-timestamp) (started-timestamp)))


(defun retract-decisions ()
  (log-trace "Retracting ~D decisions" (length (slot-value *wx* 'decisions)))
  (setf (slot-value *wx* 'decisions) nil))


(defun activity-result ()
  (event-result *event*))


;; defining workflow

(defvar *task-name*)
(defvar *task-event*)


(defun parse-handler (form)
  (destructuring-bind (on events &body body)
      form
    (assert (eq 'on on))
    (unless (listp events)
      (setf events (list events)))
    (list events
          `(,events ,@body))))


(defun verify-events (name type events)
  (let* ((defined-events (ecase type
                           (:workflow '((:started) (:signaled :start-timer-failed)))
                           (:timer '((:fired)))
                           (:activity '(()
                                        (:canceled
                                         :completed
                                         :failed
                                         :timed-out)))
                           (:child-workflow '(()
                                              (:canceled
                                               :completed
                                               :failed
                                               :started
                                               :terminated
                                               :timed-out
                                               :start-failed)))))
         (missing (set-difference (first defined-events) events))
         (undefined (set-difference (set-difference events (first defined-events))
                                    (second defined-events))))
    (when missing
      (warn "Unhandled events in task ~S: ~S" name missing))
    (when undefined
      (warn "Undefined events in task ~S: ~S" name undefined))))


(defmacro task (name (&rest args) init-form &body body)
  (declare (ignore name args init-form body)))


(defun task-type-for-init-form (init-form)
  (case (car init-form)
    ((nil) :workflow)
    (start-timer :timer)
    (otherwise
     (etypecase (find-task-type (car init-form))
       (workflow-type :child-workflow)
       (activity-type :activity)
       (null (error "Invalid init-form for task: ~S" init-form))))))


(defun parse-task (form)
  (destructuring-bind (task name (&rest args) init-form &body body)
      form
    (assert (eq 'task task))
    (let ((kwname (if name
                      (intern (symbol-name name) :keyword)
                      :workflow))
          (type (task-type-for-init-form init-form))
          (handlers (mapcar #'parse-handler body)))
      (verify-events name type (apply #'append (mapcar #'first handlers)))
      (list (when name
              `(,name (&rest args)
                      (handler ,kwname 'init args)))
            `(,kwname
              (destructuring-bind (,@args) input
                (case task-event
                  ,@(when name
                            `((init
                               (start-task ,kwname
                                           input
                                           (lambda () ,init-form)))))
                  ,@(mapcar #'second handlers)
                  (otherwise
                   (default-handler ,type task-event)))))))))


(defmacro define-workflow/2 (name (&rest lambda-list) (&body options) &body tasks)
  (let* ((workflow-task `(task nil () nil
                           ,@(loop while (eq 'on (caar tasks))
                                   collect (pop tasks))))
         (tasks (mapcar #'parse-task (cons workflow-task tasks))))
    `(define-workflow ,name ,lambda-list ,options
       (labels (,@(remove nil (mapcar #'first tasks))
                (handler (task-name task-event input)
                  (ecase task-name
                    ,@(mapcar #'second tasks))))
         (run-task-events #'handler)))))


;; Task starting functions

(defvar *control* nil)
(defvar *task-id*)

(defun start-task (name args init-fn)
  (let ((*control* (list :name name :args args))
        (*task-id* (random 100000) ;; TODO fixme
          ))
    (funcall init-fn)))

(defgeneric default-handler (task-type event-name)
  (:method (task-type event-name)
    (log-error "Unhandled event: ~S ~S ~S" task-type event-name *event*)))


(defun start-timer (start-to-fire-timeout)
  (start-timer-decision :start-to-fire-timeout start-to-fire-timeout
                        :control *control*
                        :timer-id *task-id*))


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
             :is-new (and previous-started-event-id (< previous-started-event-id id))
             (loop for (key . value ) in (aget alist attrs-slot)
                   collect key
                   collect (deserialize-slot key value))))))


(defmacro define-event (name slots &body body)
  `(progn
     (defclass ,name (event)
       (,@(loop for slot-name in slots
                collect `(,slot-name
                          :initarg ,(intern (symbol-name slot-name) :keyword)
                          :initform nil
                          :reader ,(intern (format nil "EVENT-~A" slot-name))))))
     (defmethod get-event-type ((type (eql ,(intern (subseq (symbol-name name) 0
                                                            (- (length (symbol-name name)) 6))
                                                    :keyword))))
       (values ',name
               ,(intern (format nil "~A-ATTRIBUTES" name) :keyword)))
     (defmethod index-event (wx (*event* ,name))
       (with-slots (id ,@slots)
           *event*
         ,@body))))


(defun trigger (order event-name &optional control)
  (when (event-is-new *event*)
    (let ((control (if control
                       (event-control (get-event *wx* control))
                       '(:name :workflow))))
      (log-trace "Trigger ~S ~S ~S ~S" order event-name control *event*)
      (push (list order event-name control *event*) (slot-value *wx* 'task-events)))))


(defun run-task-events (handler)
  (dolist (event (sort (slot-value *wx* 'task-events) #'< :key #'car))
    (destructuring-bind (event-name control event)
        (cdr event)
      (destructuring-bind (&key name args &allow-other-keys)
          control
        (let ((*event* event))
          (log-trace "Task-event ~S ~S ~S ~S" name event-name args event)
          (funcall handler name event-name args))))))


;; Marker events -----------------------------------------------------------------------------------


(define-event marker-recorded-event
    (decision-task-completed-event-id
     details
     marker-name)
  ;; TODO record marker in some list / index
  )


(define-event record-marker-failed-event
    (cause
     decision-task-completed-event-id
     marker-name)
  (trigger 0 :record-marker-failed))


;; Signal events -----------------------------------------------------------------------------------


(define-event workflow-execution-signaled-event
    (external-initiated-event-id
     external-workflow-execution
     input
     signal-name)
  (trigger 0 :signaled))


;; Timer events ------------------------------------------------------------------------------------


(define-event timer-started-event
    (control
     decision-task-completed-event-id
     start-to-fire-timeout
     timer-id))


(define-event start-timer-failed-event
    (cause
     decision-task-completed-event-id
     timer-id)
  (trigger 0 :start-timer-failed))


(define-event timer-fired-event
    (started-event-id
     timer-id)
  (trigger 0 :fired started-event-id))


(define-event timer-canceled-event
    (decision-task-completed-event-id
     started-event-id
     timer-id))


(define-event cancel-timer-failed-event
    (cause
     decision-task-completed-event-id
     timer-id)
  (trigger 0 :cancel-timer-failed))


;; Workflow events -------------------------------------------------------------------------


(define-event workflow-execution-started-event
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
  (trigger 0 :started))


(define-event complete-workflow-execution-failed-event
    (cause
     decision-task-completed-event-id)
  ;; TODO: trigger event for this?
  )


(define-event workflow-execution-completed-event
    (decision-task-completed-event-id
     result))


(define-event workflow-execution-failed-event
    (decision-task-completed-event-id
     details
     reason))


(define-event workflow-execution-timed-out-event
    (child-policy
     timeout-type))


(define-event workflow-execution-canceled-event
    (decision-task-completed-event-id
     details))


(define-event workflow-execution-terminated-event
    (cause
     child-policy
     details
     reason))


(define-event workflow-execution-continued-as-new-event
    (child-policy
     decision-task-completed-event-id
     execution-start-to-close-timeout
     input
     new-execution-run-id
     tag-list
     task-list
     task-start-to-close-timeout
     workflow-type))


(define-event continue-as-new-workflow-execution-failed-event
    (cause
     decision-task-completed-event-id)
  (trigger 0 :continue-as-new-failed))


(define-event workflow-execution-cancel-requested-event
    (cause
     external-initiated-event-id
     external-workflow-execution)
  (trigger 0 :cancel-requested))


;; Decision events -------------------------------------------------------------------------


(defclass decision-task (task)
  ((scheduled-event)
   (started-event)
   (completed-event)
   (timed-out-event)))


(define-event decision-task-scheduled-event
    (start-to-close-timeout
     task-list))


(define-event decision-task-started-event
    (identity
     scheduled-event-id))


(define-event decision-task-completed-event
    (execution-context
     scheduled-event-id
     started-event-id)
  (when execution-context
    (setf (slot-value wx 'context) execution-context)))


(define-event decision-task-timed-out-event
    (scheduled-event-id
     started-event-id
     timeout-type)
  (trigger 0 :decision-task-timed-out))


;; Activity events -------------------------------------------------------------------------


(define-event activity-task-scheduled-event
    (activity-id
     activity-type
     control
     decision-task-completed-event-id
     heartbeat-timeout
     input
     schedule-to-close-timeout
     schedule-to-start-timeout
     start-to-close-timeout
     task-list))


(define-event schedule-activity-task-failed-event
    (activity-id
     activity-type
     cause
     decision-task-completed-event-id)
  (trigger 0 :schedule-activity-task-failed))


(define-event activity-task-started-event
    (identity
     scheduled-event-id))


(define-event activity-task-completed-event
    (result
     scheduled-event-id
     started-event-id)
  (trigger 0 :completed scheduled-event-id))


(define-event activity-task-failed-event
    (details
     reason
     scheduled-event-id
     started-event-id)
  (trigger 0 :failed scheduled-event-id))


(define-event activity-task-timed-out-event
    (details
     scheduled-event-id
     started-event-id
     timeout-type)
  (trigger 0 :timed-out scheduled-event-id))


(define-event activity-task-canceled-event
    (details
     latest-cancel-requested-event-id
     scheduled-event-id
     started-event-id)
  (trigger 0 :canceled scheduled-event-id))


(define-event activity-task-cancel-requested-event
    (activity-id
     decision-task-completed-event-id))


(define-event request-cancel-activity-task-failed-event
    (activity-id
     cause
     decision-task-completed-event-id)
  (trigger 0 :request-cancel-activity-task-failed))


;; Child workflow -------------------------------------------------------------------------


(define-event start-child-workflow-execution-initiated-event
    (child-policy
     control
     decision-task-completed-event-id
     execution-start-to-close-timeout
     input
     tag-list
     task-list
     task-start-to-close-timeout
     workflow-id
     workflow-type))


(define-event start-child-workflow-execution-failed-event
    (cause
     control
     decision-task-completed-event-id
     initiated-event-id
     workflow-id
     workflow-type)
  (trigger 0 :start-failed initiated-event-id))


(define-event child-workflow-execution-started-event
    (initiated-event-id
     workflow-execution
     workflow-type)
  (trigger 0 :started initiated-event-id))


(define-event child-workflow-execution-completed-event
    (initiated-event-id
     result
     started-event-id
     workflow-execution
     workflow-type)
  (trigger 0 :completed initiated-event-id))


(define-event child-workflow-execution-failed-event
    (details
     initiated-event-id
     reason
     started-event-id
     workflow-execution
     workflow-type)
  (trigger 0 :failed initiated-event-id))


(define-event child-workflow-execution-timed-out-event
    (initiated-event-id
     started-event-id
     timeout-type
     workflow-execution
     workflow-type)
  (trigger 0 :timed-out initiated-event-id))


(define-event child-workflow-execution-canceled-event
    (details
     initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (trigger 0 :canceled initiated-event-id))


(define-event child-workflow-execution-terminated-event
    (initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (trigger 0 :terminated initiated-event-id))


;; Signal external workflow ----------------------------------------------------------------------


(define-event signal-external-workflow-execution-initiated-event
    (control
     decision-task-completed-event-id
     input
     run-id
     signal-name
     workflow-id))


(define-event external-workflow-execution-signaled-event
    (initiated-event-id
     workflow-execution)
  (trigger 0 :signaled initiated-event-id))


(define-event signal-external-workflow-execution-failed-event
    (cause
     control
     decision-task-completed-event-id
     initiated-event-id
     run-id
     workflow-id)
  (trigger 0 :failed initiated-event-id))


;; Cancel external workflow ----------------------------------------------------------------------


(define-event request-cancel-external-workflow-execution-initiated-event
    (control
     decision-task-completed-event-id
     run-id
     workflow-id))


(define-event external-workflow-execution-cancel-requested-event
    (initiated-event-id
     workflow-execution)
  (trigger 0 :cancel-requested initiated-event-id))


(define-event request-cancel-external-workflow-execution-failed-event
    (cause
     control
     decision-task-completed-event-id
     initiated-event-id
     run-id
     workflow-id)
  (trigger 0 :failed initiated-event-id))
