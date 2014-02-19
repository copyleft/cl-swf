(in-package #:swf-workers)


(defvar *decisions*)


(defmacro %decision (type &rest attrs)
  `(push (alist :decision-type ,type
                ,(intern (format nil "~A-DECISION-ATTRIBUTES" type) :keyword)
                (alist ,@attrs))
         *decisions*))


(defun cancel-workflow-execution (&key details)
  (%decision :cancel-workflow-execution
             :details details))


(defun record-marker (&key details marker-name)
  (%decision :record-marker
             :details details
             :marker-name marker-name))


(defun start-timer (&key control start-to-fire-timeout timer-id)
  (%decision :start-timer
             :control control
             :start-to-fire-timeout start-to-fire-timeout
             :timer-id (or timer-id (make-uuid))))


(defun schedule-activity-task (&key activity-id
                                 activity-type
                                 control
                                 heartbeat-timeout
                                 input
                                 schedule-to-close-timeout
                                 schedule-to-start-timeout
                                 start-to-close-timeout
                                 task-list)
  (%decision :schedule-activity-task
             :activity-id (or activity-id (make-uuid))
             :activity-type activity-type
             :control control
             :heartbeat-timeout heartbeat-timeout
             :input (serialize-object input)
             :schedule-to-close-timeout schedule-to-close-timeout
             :schedule-to-start-timeout schedule-to-start-timeout
             :start-to-close-timeout start-to-close-timeout
             :task-list task-list))
