(in-package #:swf-workers)


(defun add-decision (decision)
  (log-trace "Add decision ~S" decision)
  (push decision (slot-value *wx* 'decisions))
  decision)


(defclass decision () ())
(defgeneric transform-decision (decision))

(defmacro define-decision (name slots)
  (let ((slots (mapcar #'normalize-slot slots))
        (class-name (intern (format nil "~A-DECISION" name))))
    `(progn
       (defclass ,class-name (decision)
         ,(loop for (slot-name) in slots
                collect `(,slot-name
                          :initarg ,(intern (symbol-name slot-name) :keyword)
                          :initform nil
                          :reader ,(intern (format nil "DECISION-~A" slot-name)))))
       (defun ,class-name (&key ,@(mapcar #'car slots))
         (add-decision
          (make-instance ',class-name
                         ,@(loop for (slot-name) in slots
                                 collect (intern (symbol-name slot-name) :keyword)
                                 collect slot-name))))
       (defmethod transform-decision ((decision ,class-name))
         (alist :decision-type ',name
                ,(intern (format nil "~A-DECISION-ATTRIBUTES" name) :keyword)
                (list ,@(loop for (slot-name transformer) in slots collect
                              `(cons ,(intern (symbol-name slot-name) :keyword)
                                     (,transformer (slot-value decision ',slot-name))))))))))


(define-decision cancel-timer
    ((timer-id serialize-id)))


(define-decision cancel-workflow-execution
    ((details serialize-object)))


(define-decision complete-workflow-execution
    ((result serialize-object)))


(define-decision continue-as-new-workflow-execution
    (child-policy
     execution-start-to-close-timeout
     (input serialize-object)
     tag-list
     task-list
     task-start-to-close-timeout
     workflow-type-version))


(define-decision fail-workflow-execution
    ((details serialize-object)
     (reason serialize-object)))


(define-decision record-marker
    ((details serialize-object)
     (marker-name serialize-id)))


(define-decision request-cancel-activity-task
    ((activity-id serialize-id)))


(define-decision request-cancel-external-workflow-execution
    ((control serialize-object)
     run-id
     (workflow-id serialize-id)))


(define-decision schedule-activity-task
    ((activity-id serialize-id)
     (activity-type serialize-task-type)
     (control serialize-object)
     heartbeat-timeout
     (input serialize-object)
     schedule-to-close-timeout
     schedule-to-start-timeout
     start-to-close-timeout
     task-list))


(define-decision signal-external-workflow-execution
    ((control serialize-object)
     (input serialize-object)
     run-id
     (signal-name serialize-id)
     (workflow-id serialize-object)))


(define-decision start-child-workflow-execution
    (child-policy
     (control serialize-object)
     execution-start-to-close-timeout
     (input serialize-object)
     tag-list
     task-list
     task-start-to-close-timeout
     (workflow-id serialize-id)
     (workflow-type serialize-task-type)))


(define-decision start-timer
    ((control serialize-object)
     start-to-fire-timeout
     (timer-id serialize-id)))
