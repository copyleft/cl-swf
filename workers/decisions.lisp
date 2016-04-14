(in-package #:swf-workers)


(defun add-decision (decision)
  (log-trace "Add decision ~S" decision)
  (push decision (slot-value *wx* 'decisions))
  decision)


(defclass decision () ())
(defgeneric transform-decision (decision))

(defmacro define-decision (name slots)
  (let ((class-name (intern (format nil "~A-DECISION" name))))
    `(progn
       (defclass ,class-name (decision)
         ,(loop for slot-name in slots
                collect `(,slot-name
                          :initarg ,(intern (symbol-name slot-name) :keyword)
                          :initform nil
                          :reader ,(intern (format nil "DECISION-~A" slot-name)))))
       (defun ,class-name (&key ,@slots)
         (add-decision
          (make-instance ',class-name
                         ,@(loop for slot-name in slots
                                 collect (intern (symbol-name slot-name) :keyword)
                                 collect slot-name))))
       (defmethod transform-decision ((decision ,class-name))
         (alist :decision-type ',name
                ,(intern (format nil "~A-DECISION-ATTRIBUTES" name) :keyword)
                (list ,@(loop for slot-name in slots collect
                              `(cons ,(intern (symbol-name slot-name) :keyword)
                                     (serialize-slot ,(intern (symbol-name slot-name) :keyword)
                                                     (slot-value decision ',slot-name))))))))))


(define-decision cancel-timer
    (timer-id))


(define-decision cancel-workflow-execution
    (details))


(define-decision complete-workflow-execution
    (result))


(define-decision continue-as-new-workflow-execution
    (child-policy
     execution-start-to-close-timeout
     input
     tag-list
     task-list
     task-priority
     task-start-to-close-timeout
     workflow-type-version))


(define-decision fail-workflow-execution
    (details
     reason))


(define-decision record-marker
    (details
     marker-name))


(define-decision request-cancel-activity-task
    (activity-id))


(define-decision request-cancel-external-workflow-execution
    (control
     run-id
     workflow-id))


(define-decision schedule-activity-task
    (activity-id
     activity-type
     control
     heartbeat-timeout
     input
     schedule-to-close-timeout
     schedule-to-start-timeout
     start-to-close-timeout
     task-list
     task-priority))


(define-decision signal-external-workflow-execution
    (control
     input
     run-id
     signal-name
     workflow-id))


(define-decision start-child-workflow-execution
    (child-policy
     control
     execution-start-to-close-timeout
     input
     tag-list
     task-list
     task-priority
     task-start-to-close-timeout
     workflow-id
     workflow-type))


(define-decision start-timer
    (control
     start-to-fire-timeout
     timer-id))
