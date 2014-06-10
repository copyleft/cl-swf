(in-package #:swf-workers)


(declaim (optimize (speed 0) (space 0) (debug 3)))


(define-condition simple-style-warning (simple-condition style-warning) ())
(defun style-warn (datum &rest args)
  (warn 'simple-style-warning :format-control datum :format-arguments args))


(defvar *wx*)
(defvar *event* nil)


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
   (canceled-requested-event-ids :initform nil)
   (markers :initform nil)
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
    (trigger -1000 :decision-task-started)
    (trigger 1000 :decision-task-completed)
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

(defun child-workflow-result ()
  (event-result *event*))

(defun workflow-execution-started-event ()
  ;; TODO is this event always first?
  (get-event *wx* 1))


(defun complete-workflow (&optional result)
  (complete-workflow-execution-decision :result result))


(defun continue-as-new ()
  (with-slots (child-policy
               execution-start-to-close-timeout
               input
               tag-list
               task-list
               task-start-to-close-timeout
               workflow-type)
      (workflow-execution-started-event)
    (continue-as-new-workflow-execution-decision
     :child-policy child-policy
     :execution-start-to-close-timeout execution-start-to-close-timeout
     :input input
     :tag-list tag-list
     :task-list task-list
     :task-start-to-close-timeout task-start-to-close-timeout
     :workflow-type-version (getf (task-type-options workflow-type) :version))))


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
                           (:workflow '((:started)
                                        (:cancel-timer-failed
                                         :continue-as-new-failed
                                         :decision-task-timed-out
                                         :record-marker-failed
                                         :request-cancel-activity-task-failed
                                         :schedule-activity-task-failed
                                         :start-timer-failed
                                         :cancel-requested
                                         :signaled
                                         :decision-task-completed)))
                           (:timer '((:fired)))
                           (:activity '((:completed)
                                        (:canceled
                                         :completed
                                         :failed
                                         :timed-out)))
                           (:child-workflow '((:canceled
                                               :completed
                                               :failed
                                               :terminated
                                               :timed-out)
                                              (:started
                                               :start-failed)))
                           (:signal-ext '((:signaled
                                           :failed)
                                          ()))
                           (:cancel-ext '((:cancel-requested
                                           :failed)
                                          ()))))
         (missing (set-difference (first defined-events) events))
         (undefined (set-difference (set-difference events (first defined-events))
                                    (second defined-events))))
    (when missing
      (style-warn "Unhandled events in task ~S: ~S" name missing))
    (when undefined
      (style-warn "Undefined events in task ~S: ~S" name undefined))))


(defmacro task (name (&rest args) init-form &body body)
  (declare (ignore name args init-form body)))


(defun task-type-for-init-form (init-form)
  (case (car init-form)
    ((nil) :workflow)
    (start-timer :timer)
    (start-child-workflow :child-workflow)
    (schedule-activity :activity)
    (signal-external-workflow-execution :signal-ext)
    (request-cancel-external-workflow-execution :cancel-ext)
    (otherwise
     (error "Invalid init-form for task: ~S" init-form))))


(defun normalize-key-parameter (param)
  (let ((var param)
        keyword-name
        init-form
        supplied-p-parameter)
    (when (consp var)
      (setf init-form (second var))
      (setf supplied-p-parameter (third var))
      (setf var (car var)))
    (when (consp var)
      (setf keyword-name (cadr var))
      (setf var (car var)))
    (unless keyword-name
      (setf keyword-name (intern (string var) :keyword)))
    (unless supplied-p-parameter
      (setf supplied-p-parameter (gensym (format nil "~A-P-" var))))
    `((,keyword-name ,var) ,init-form ,supplied-p-parameter)))


(defun normalize-optional-parameter (param)
  (let ((var param)
        init-form
        supplied-p-parameter)
    (when (consp var)
      (setf init-form (second var))
      (setf supplied-p-parameter (third var))
      (setf var (car var)))
    (unless supplied-p-parameter
      (setf supplied-p-parameter (gensym (format nil "~A-P-" var))))
    `(,var ,init-form ,supplied-p-parameter)))


(defun normalize-aux-parameter (param)
  (let ((var param)
        init-form)
    (when (consp var)
      (setf init-form (second var))
      (setf var (car var)))
    `(,var ,init-form)))

;(normalize-key-parameter 'key)
;(normalize-key-parameter '(key 34))
;(normalize-key-parameter '((key :knob) 323))
;(normalize-optional-parameter 'b)
;(normalize-optional-parameter '(b 3))
;(normalize-aux-parameter 'a)
;(normalize-aux-parameter '(a 99))

(defun parse-lambda-list (lambda-list)
  (let* ((optional-start (member '&optional lambda-list))
         (rest-start (member '&rest lambda-list))
         (key-start (member '&key lambda-list))
         (allow-other-keys (member '&allow-other-keys lambda-list))
         (aux-start (member '&aux lambda-list))
         (vars (ldiff lambda-list (or optional-start rest-start key-start allow-other-keys aux-start)))
         (optionals (mapcar #'normalize-optional-parameter
                            (ldiff (cdr optional-start) (or rest-start key-start allow-other-keys aux-start))))
         (rest (ldiff rest-start (or key-start allow-other-keys aux-start)))
         (rest-var (second rest))
         (keys (mapcar #'normalize-key-parameter
                       (ldiff (cdr key-start) (or allow-other-keys aux-start))))
         (aux (mapcar #'normalize-aux-parameter (cdr aux-start)))
         (all-vars (append vars
                           (mapcar #'car optionals)
                           (cdr rest)
                           (mapcar #'cadar keys)
                           (mapcar #'car aux)))
         (normalized-lambda-list `(,@vars
                                   ,@(when optionals `(&optional ,@optionals))
                                   ,@rest
                                   ,@(when keys `(&key ,@keys))
                                   ,@allow-other-keys
                                   ,@(when aux `(&aux ,@aux))))
         (args-list-form `(loop
                           ,@(loop :for var :in vars append `(:collect ,var))
                           ,@(loop :for (var nil supplied-p) :in optionals
                                   :append `(when ,supplied-p :collect ,var))
                           ,@(when rest `(:append ,rest-var))
                           ,@(unless rest
                                     (loop :for ((key var) nil supplied-p) :in keys
                                           :append `(when ,supplied-p :collect ,key :and :collect ,var)))
                           :while nil)))
    (values normalized-lambda-list
            args-list-form
            all-vars
            vars
            optionals
            rest
            keys
            allow-other-keys
            aux)))


;(parse-lambda-list '(a q &optional (b 3) &rest x &key c (d a) &aux tr))
;(parse-lambda-list '(a q &optional (b 3) &key c (d a) &aux tr))
(defun parse-task (form)
  (destructuring-bind (task name (&rest args) init-form &body body)
      form
    (assert (eq 'task task))
    (multiple-value-bind (normalized-lambda-list args-list-form all-vars)
        (parse-lambda-list args)
      (let ((kwname (if name
                        (intern (symbol-name name) :keyword)
                        :workflow))
            (type (task-type-for-init-form init-form))
            (handlers (mapcar #'parse-handler body)))
        (verify-events name type (apply #'append (mapcar #'first handlers)))
        (list (when name
                `(,name (,@normalized-lambda-list)
                        (declare (ignorable ,@all-vars))
                        (start-task ,kwname
                                    ,args-list-form
                                    (lambda () ,init-form))))
              `(,kwname
                (destructuring-bind (,@args) input
                  (declare (ignorable ,@all-vars))
                  (case task-event
                    ,@(mapcar #'second handlers)
                    (otherwise
                     (default-handler ,type task-event))))))))))


(defmacro define-workflow (name (&rest lambda-list) (&body options) &body tasks)
  (let* ((workflow-task `(task nil () nil
                           ,@(loop while (eq 'on (caar tasks))
                                   collect (pop tasks))))
         (tasks (mapcar #'parse-task (cons workflow-task tasks))))
    `(%define-workflow ,name ,lambda-list ,options
       (let (handler)
         (flet (,@(remove nil (mapcar #'first tasks)))
           (labels ((handler (task-name task-event input)
                      (ecase task-name
                        ,@(mapcar #'second tasks))))
             (setf handler #'handler)
             (run-task-events #'handler)))))))


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


(defvar *workflow-starter* nil)


(defmacro start-child-workflow (form &body (&key child-policy
                                                 execution-start-to-close-timeout
                                                 tag-list
                                                 task-list
                                                 task-start-to-close-timeout))
  (let ((workflow-type (gensym "WORKFLOW-TYPE"))
        (args (gensym "ARGS")))
    `(let ((*workflow-starter*
            (lambda (,workflow-type ,args)
              (start-child-workflow-execution-decision
               :control *control*
               :child-policy ,child-policy
               :execution-start-to-close-timeout ,execution-start-to-close-timeout
               :input ,args
               :tag-list ,tag-list
               :task-list ,task-list
               :task-start-to-close-timeout ,task-start-to-close-timeout
               :workflow-id *task-id*
               :workflow-type ,workflow-type))))
       ,form)))


(defmacro start-workflow (form &body (&key child-policy
                                           execution-start-to-close-timeout
                                           tag-list
                                           task-list
                                           task-start-to-close-timeout
                                           workflow-id))
  (let ((workflow-type (gensym "WORKFLOW-TYPE"))
        (args (gensym "ARGS")))
    `(let ((*workflow-starter*
            (lambda (,workflow-type ,args)
              (swf::start-workflow-execution
               :child-policy ,child-policy
               :execution-start-to-close-timeout ,execution-start-to-close-timeout
               :input (serialize-object ,args)
               :tag-list ,tag-list
               :task-list ,task-list
               :task-start-to-close-timeout ,task-start-to-close-timeout
               :workflow-id (serialize-slot :workflow-id (or ,workflow-id (task-type-name ,workflow-type)))
               :workflow-type (serialize-slot :workflow-type ,workflow-type)))))
       ,form)))


(defun %start-workflow (workflow-type args)
  (if *workflow-starter*
      (funcall *workflow-starter* workflow-type args)
      (error "Must wrap in START-WORKFLOW or START-CHILD-WORKFLOW")))


(defvar *activity-scheduler* nil)

(defmacro schedule-activity (form &body (&key heartbeat-timeout
                                              schedule-to-close-timeout
                                              schedule-to-start-timeout
                                              start-to-close-timeout
                                              task-list))
  (let ((activity-type (gensym "ACTIVITY-TYPE"))
        (args (gensym "ARGS")))
    `(let ((*activity-scheduler*
            (lambda (,activity-type ,args)
              (schedule-activity-task-decision
               :activity-id *task-id*
               :activity-type ,activity-type
               :control *control*
               :heartbeat-timeout ,heartbeat-timeout
               :input ,args
               :schedule-to-close-timeout ,schedule-to-close-timeout
               :schedule-to-start-timeout ,schedule-to-start-timeout
               :start-to-close-timeout ,start-to-close-timeout
               :task-list ,task-list))))
       ,form)))


(defun %schedule-activity (activity-type args)
  (if *activity-scheduler*
      (funcall *activity-scheduler* activity-type args)
      (apply-activity-task-function activity-type args)))



(defun request-cancel-external-workflow-execution (&key run-id workflow-id)
  (request-cancel-external-workflow-execution-decision :control *control*
                                                       :run-id run-id
                                                       :workflow-id workflow-id))


(defun signal-external-workflow-execution (signal-name &key input workflow-id run-id)
  (signal-external-workflow-execution-decision :control *control*
                                               :run-id run-id
                                               :workflow-id workflow-id
                                               :signal-name signal-name
                                               :input input))


(defun record-marker (name &rest details)
  (push (list name details) (slot-value *wx* 'markers))
  (record-marker-decision :marker-name name :details details))


(defun get-marker (name)
  (find name (slot-value *wx* 'markers) :key #'car))


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
  (when (or (null *event*)
            (event-is-new *event*))
    (let ((control (if control
                       (event-control (get-event *wx* control))
                       '(:name :workflow))))
      (log-trace "Trigger ~S ~S ~S ~S" order event-name control *event*)
      (push (list order event-name control *event*) (slot-value *wx* 'task-events)))))


(defun run-task-events (handler)
  (dolist (event (stable-sort (reverse (slot-value *wx* 'task-events)) #'< :key #'car))
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
  (push (list marker-name details id) (slot-value *wx* 'markers)))


(define-event record-marker-failed-event
    (cause
     decision-task-completed-event-id
     marker-name)
  (trigger -1 :record-marker-failed))


;; Signal events -----------------------------------------------------------------------------------


(define-event workflow-execution-signaled-event
    (external-initiated-event-id
     external-workflow-execution
     input
     signal-name)
  (trigger -10 :signaled))


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
  (trigger -10 :start-timer-failed))


(define-event timer-fired-event
    (started-event-id
     timer-id)
  (trigger -1 :fired started-event-id))


(define-event timer-canceled-event
    (decision-task-completed-event-id
     started-event-id
     timer-id))


(define-event cancel-timer-failed-event
    (cause
     decision-task-completed-event-id
     timer-id)
  (trigger -10 :cancel-timer-failed))


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
  (trigger -10 :started))


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
  (trigger -10 :continue-as-new-failed))


(define-event workflow-execution-cancel-requested-event
    (cause
     external-initiated-event-id
     external-workflow-execution)
  (push id (slot-value *wx* 'canceled-requested-event-ids))
  (trigger -2 :cancel-requested))


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
  (trigger -10 :decision-task-timed-out))


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
  (trigger -10 :schedule-activity-task-failed))


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
  (trigger -10 :request-cancel-activity-task-failed))


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
