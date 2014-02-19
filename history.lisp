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
                 collect value))))


(defvar *wx*)

(defclass workflow-execution-info ()
  (;(run-id :initarg :run-id)
   ;(workflow-id :initarg :workflow-id)
   ;(workflow-type :initarg :workflow-type)
   (parent-run-id :initarg :parent-run-id)
   (close-status :initarg :close-status)
   (close-timestamp :initarg :close-timestamp)
   (tag-list :initarg :tag-list)
   (task-list :initarg :task-list)
   (child-policy :initarg :child-policy)
   (execution-start-to-close-timeout :initarg :execution-start-to-close-timeout)
   (task-start-to-close-timeout :initarg :task-start-to-close-timeout)
   (input :initarg :input)
   (open-activity-tasks :initarg :open-activity-tasks)
   (open-child-workflow-executions :initarg :open-child-workflow-executions)
   (open-decision-tasks :initarg :open-decision-tasks)
   (open-timers :initarg :open-timers)
   (cancel-requested :initarg :cancel-requested)
   (history :initarg :history)
   (last-decision-task-completed-event-id :initform nil)
   (workflow-task :initform (make-hash-table))
   (activity-tasks :initform (make-hash-table :test #'equal))
   (decision-tasks :initform (make-hash-table))
   (timer-tasks :initform (make-hash-table :test #'equal))
   (child-workflow-tasks :initform (make-hash-table :test #'equal))
   (previous-started-event-id :initform nil)))


(defun make-workflow-execution-info (history)
  (let ((*wx* (make-instance 'workflow-execution-info
                             :history (map 'vector #'make-history-event history))))
    (map nil #'update-history-with-event (slot-value *wx* 'history))
    *wx*))


(defun last-event-id ()
  (with-slots (history) *wx*
    (1- (length history))))


(defun get-event (id)
  (with-slots (history) *wx*
    (aref history id)))


(defclass task ()
  ((scheduled-event-id :initarg :scheduled-event-id
                       :reader task-scheduled-event-id)
   (started-event-id :initform nil
                     :reader task-started-event-id)
   (closed-event-id :initform nil
                    :reader task-closed-event-id)
   (request-cancel-event-ids :initform nil
                             :reader task-request-cancel-event-ids)))

(defun get-tasks (type)
  (slot-value *wx* (ecase type
                     (:workflow 'workflow-task)
                     (:activity 'activity-tasks)
                     (:decision 'decision-tasks)
                     (:timer 'timer-tasks)
                     (:child-workflow 'child-workflow-tasks))))

(defun get-task (type &optional id)
  (gethash id (get-tasks type)))

(defun count-open-tasks (type)
  (loop for task being the hash-values of (get-tasks type)
        count (not (task-closed-event-id task))))

(defun %schedule-task (type &optional id)
  (let ((tasks (get-tasks type)))
    (assert (null (gethash id tasks)) () "Program error: duplicate task id ~S ~S" type id)
    (let ((task (make-instance 'task :scheduled-event-id (last-event-id))))
      (setf (gethash id tasks) task)
      task)))

(defun %start-task (type &optional id)
  (when (eql type :decision)
    (setf (slot-value *wx* 'previous-started-event-id) (last-event-id)))
  (let ((task (gethash id (get-tasks type))))
    (assert task () "Program error: unknown task id ~S ~S" type id)
    (setf (slot-value task 'started-event-id) (last-event-id))
    task))

(defun %close-task (type &optional id)
  (let ((task (gethash id (get-tasks type))))
    (assert task () "Program error: unknown task id ~S ~S" type id)
    (setf (slot-value task 'closed-event-id) (last-event-id))
    task))

(defun %request-cancel-task (type &optional id)
  (when (eq type :workflow)
    (setf (slot-value *wx* 'cancel-requested) t))
  (let ((task (gethash id (get-tasks type))))
    (assert task () "Program error: unknown task id ~S ~S" type id)
    (push (last-event-id) (slot-value task 'request-cancel-event-ids))
    task))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun normalize-slot (slot)
    (if (consp slot)
        slot
        (list slot :type slot :required t))))


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
       (defmethod update-history-with-event ((event ,name))
         (with-slots (id ,@(mapcar #'car slots))
             event
           ,@body)))))


(deftype event-id () '(integer 0 99999999))
(deftype decision-task-completed-event-id () 'event-id)
(deftype scheduled-event-id () 'event-id)
(deftype started-event-id () 'event-id)
(deftype initiated-event-id () 'event-id)

(defun string-64-p (string)
  (and (stringp string) (<= (length string) 64)))
(deftype string-64 () '(satisfies string-64-p))
(defun string-256-p (string)
  (and (stringp string) (<= (length string) 256)))
(deftype string-256 () '(satisfies string-256-p))
(defun string-2k-p (string)
  (and (stringp string) (<= (length string) 2048)))
(deftype string-2k () '(satisfies string-2k-p))
(defun string-32k-p (string)
  (and (stringp string) (<= (length string) 32768)))
(deftype string-32k () '(satisfies string-32k-p))

(deftype activity-type () '(cons string-256 string-64))
(deftype workflow-type () '(cons string-256 string-64))

(deftype run-id () 'integer)
(deftype activity-id () 'string-256)
(deftype workflow-id () 'string-256)
(deftype workflow-execution () '(cons workflow-id run-id))
(deftype timer-id () 'string-256)
(deftype task-list () 'string-256)


(deftype timeout () '(integer 0 99999999))
(deftype timeout-or-none () '(or timeout (member :none)))

(defun tag-list-p (tag-list)
  (and (listp tag-list)
       (<= (length tag-list) 5)
       (every #'string-256-p tag-list)))
(deftype tag-list () '(satisfies tag-list-p))

(deftype schedule-activity-task-failed-cause ()
  '(member
    :activity-type-deprecated
    :activity-type-does-not-exist
    :activity-id-already-in-use
    :open-activities-limit-exceeded
    :activity-creation-rate-exceeded
    :default-schedule-to-close-timeout-undefined
    :default-task-list-undefined
    :default-schedule-to-start-timeout-undefined
    :default-start-to-close-timeout-undefined
    :default-heartbeat-timeout-undefined
    :operation-not-permitted))

(deftype activity-task-timeout-type ()
  '(member :start-to-close :schedule-to-start :schedule-to-close :heartbeat))

(deftype child-policy ()
  '(member :terminate :request-cancel :abandon))


;; Workflow events -------------------------------------------------------------------------


(define-history-event workflow-execution-started-event
    (child-policy
     (continued-execution-run-id :type run-id)
     (execution-start-to-close-timeout :type timeout)
     (input :type string-32k)
     (parent-initiated-event-id :type event-id)
     (parent-workflow-execution :type workflow-execution)
     tag-list
     task-list
     (task-start-to-close-timeout :type timeout)
     workflow-type)
  (%schedule-task :workflow)
  (%start-task :workflow))


(define-history-event workflow-execution-completed-event
    (decision-task-completed-event-id
     (result :type string-32k))
  (%close-task :workflow))


(define-history-event workflow-execution-failed-event
    (decision-task-completed-event-id
     (details :type string-32k)
     (reason :type string-256))
  (%close-task :workflow))


(define-history-event workflow-execution-timed-out-event
    (child-policy
     (timeout-type :type (member :start-to-close) :required t))
  (%close-task :workflow))


(define-history-event workflow-execution-canceled-event
    (decision-task-completed-event-id
     (details :type string-32k))
  (%close-task :workflow))


(define-history-event workflow-execution-terminated-event
    ((cause :type (member :child-policy-applied :event-limit-exceeded :operator-initiated))
     child-policy
     (details :type string-32k)
     (reason :type string-256))
  (%close-task :workflow))


(define-history-event workflow-execution-continued-as-new-event
    (child-policy
     decision-task-completed-event-id
     (execution-start-to-close-timeout :type timeout-or-none)
     (input :type string-32k)
     (new-execution-run-id :type run-id :required t)
     tag-list
     task-list
     (task-start-to-close-timeout :type timeout-or-none)
     workflow-type)
  (%close-task :workflow))


(define-history-event workflow-execution-cancel-requested-event
    ((cause :type (member :child-policy-applied))
     (external-initiated-event-id :type event-id)
     (external-workflow-execution :type workflow-execution))
  (%request-cancel-task :workflow))


;; Decision events -------------------------------------------------------------------------


(define-history-event decision-task-scheduled-event
    ((start-to-close-timeout :type timeout)
     task-list)
  (%schedule-task :decision id))


(define-history-event decision-task-started-event
    ((identity :type string-256)
     scheduled-event-id)
  (%start-task :decision scheduled-event-id))


(define-history-event decision-task-completed-event
    (scheduled-event-id
     started-event-id)
  (%close-task :decision scheduled-event-id))


(define-history-event decision-task-timed-out-event
    (scheduled-event-id
     started-event-id
     (timeout-type :type (member :start-to-close) :required t))
  (%close-task :decision scheduled-event-id))


;; Activity events -------------------------------------------------------------------------

(define-history-event activity-task-scheduled-event
    (activity-id
     activity-type
     (control :type string-32k)
     decision-task-completed-event-id
     (heartbeat-timeout :type timeout)
     (input :type string-32k)
     (schedule-to-close-timeout :type timeout)
     (schedule-to-start-timeout :type timeout)
     (start-to-close-timeout :type timeout)
     task-list)
  (%schedule-task :activity activity-id))


(define-history-event schedule-activity-task-failed-event
    (activity-id
     activity-type
     (cause :type schedule-activity-task-failed-cause :required t)
     decision-task-completed-event-id)
  (%schedule-task :activity activity-id)
  (%start-task :activity activity-id)
  (%close-task :activity activity-id))


(define-history-event activity-task-started-event
    ((identity :type string-256)
     scheduled-event-id)
  (%start-task :activity (event-activity-id (get-event scheduled-event-id))))


(define-history-event activity-task-completed-event
    ((result :type string-32k)
     scheduled-event-id
     started-event-id)
  (%close-task :activity (event-activity-id (get-event scheduled-event-id))))


(define-history-event activity-task-failed-event
    ((details :type string-32k)
     (reason :type string-256)
     scheduled-event-id
     started-event-id)
  (%close-task :activity (event-activity-id (get-event scheduled-event-id))))


(define-history-event activity-task-timed-out-event
    ((details :type string-2k)
     scheduled-event-id
     started-event-id
     (timeout-type :type activity-task-timeout-type :required t))
  (%close-task :activity (event-activity-id (get-event scheduled-event-id))))


(define-history-event activity-task-canceled-event
    ((details :type string-32k)
     (latest-cancel-requested-event-id :type event-id)
     scheduled-event-id
     started-event-id)
  (%close-task :activity (event-activity-id (get-event scheduled-event-id))))


(define-history-event activity-task-cancel-requested-event
    (activity-id
     decision-task-completed-event-id)
  (%request-cancel-task :activity activity-id))


(define-history-event request-cancel-activity-task-failed-event
    (activity-id
     (cause :type (member :activity-id-unknown) :required t)
     decision-task-completed-event-id))


;; Misc events -------------------------------------------------------------------------


(define-history-event workflow-execution-signaled-event
    ((external-initiated-event-id :type event-id)
     (external-workflow-execution :type workflow-execution)
     (input :type string-32k)
     (signal-name :type string-256 :required t)))


(define-history-event marker-recorded-event
    (decision-task-completed-event-id
     (details :type string-32k)
     (marker-name :type string-256 :required t)))


;; Timer events -------------------------------------------------------------------------


(define-history-event timer-started-event
    ((control :type string-32k)
     decision-task-completed-event-id
     (start-to-fire-timeout :type timeout :required t)
     timer-id)
  (%schedule-task :timer timer-id)
  (%start-task :timer timer-id))


(define-history-event start-timer-failed-event
    ((cause :type (member :timer-id-already-in-use) :required t)
     decision-task-completed-event-id
     timer-id)
  (%schedule-task :timer timer-id)
  (%close-task :timer timer-id)
  (%close-task :timer timer-id))


(define-history-event timer-fired-event
    (started-event-id
     timer-id)
  (%close-task :timer timer-id))


(define-history-event timer-canceled-event
    (decision-task-completed-event-id
     started-event-id
     timer-id)
  (%close-task :timer timer-id))


(define-history-event cancel-timer-failed-event
    ((cause :type (member :timer-id-unknown) :required t)
     timer-id))


;; Child workflow -------------------------------------------------------------------------


(define-history-event start-child-workflow-execution-initiated-event
    ((child-policy :type child-policy)
     (control :type string-32k)
     decision-task-completed-event-id
     (execution-start-to-close-timeout :type timeout-or-none)
     (input :type string-32k)
     tag-list
     (task-list :type task-list)
     (task-start-to-close-timeout :type timeout-or-none)
     workflow-id
     workflow-type
     workflow-execution)
  (%schedule-task :child-workflow workflow-execution))


(deftype start-child-workflow-execution-failed-cause ()
  '(member
    :workflow-type-does-not-exist
    :workflow-type-deprecated
    :open-children-limit-exceeded
    :open-workflows-limit-exceeded
    :child-creation-rate-exceeded
    :workflow-already-running
    :default-execution-start-to-close-timeout-undefined
    :default-task-list-undefined
    :default-task-start-to-close-timeout-undefined
    :default-child-policy-undefined
    :operation-not-permitted))


(define-history-event start-child-workflow-execution-failed-event
    ((cause :type start-child-workflow-execution-failed-cause :required t)
     (control :type string-32k)
     decision-task-completed-event-id
     initiated-event-id
     workflow-id
     workflow-type
     workflow-execution)
  (%close-task :child-workflow workflow-execution))


(define-history-event child-workflow-execution-started-event
    (initiated-event-id
     workflow-execution
     workflow-type)
  (%start-task :child-workflow workflow-execution))


(define-history-event child-workflow-execution-completed-event
    (initiated-event-id
     (result :type string-32k)
     started-event-id
     workflow-execution
     workflow-type)
  (%close-task :child-workflow workflow-execution))


(define-history-event child-workflow-execution-failed-event
    ((details :type string-32k)
     initiated-event-id
     (reason :type string-256)
     started-event-id
     workflow-execution
     workflow-type)
  (%close-task :child-workflow workflow-execution))


(define-history-event child-workflow-execution-timed-out-event
    (initiated-event-id
     started-event-id
     (timeout-type :type (member :start-to-close) :required t)
     workflow-execution
     workflow-type)
  (%close-task :child-workflow workflow-execution))


(define-history-event child-workflow-execution-canceled-event
    ((details :type string-32k)
     initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (%close-task :child-workflow workflow-execution))


(define-history-event child-workflow-execution-terminated-event
    (initiated-event-id
     started-event-id
     workflow-execution
     workflow-type)
  (%close-task :child-workflow workflow-execution))


;; External workflow ----------------------------------------------------------------------


(define-history-event signal-external-workflow-execution-initiated-event
    ((control :type string-32k)
     decision-task-completed-event-id
     (input :type string-32k)
     (run-id :type run-id)
     (signal-name :type string-256 :required t)
     workflow-id))


(define-history-event external-workflow-execution-signaled-event
    (initiated-event-id
     workflow-execution))


(define-history-event signal-external-workflow-execution-failed-event
    ((cause :type (member :unknown-external-workflow-execution) :required t)
     (control :type string-32k)
     decision-task-completed-event-id
     initiated-event-id
     (run-id :type run-id)
     workflow-id))


(define-history-event request-cancel-external-workflow-execution-initiated-event
    ((control :type string-32k)
     decision-task-completed-event-id
     (run-id :type run-id)
     workflow-id))


(define-history-event external-workflow-execution-cancel-requested-event
    (initiated-event-id
     workflow-execution))


(define-history-event request-cancel-external-workflow-execution-failed-event
    ((cause :type (member :unknown-external-workflow-execution) :required t)
     (control :type string-32k)
     decision-task-completed-event-id
     initiated-event-id
     (run-id :type run-id)
     workflow-id))
