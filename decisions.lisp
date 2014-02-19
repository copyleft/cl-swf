(in-package #:swf-workers)


(defvar *decisions*)


(defmacro %decision (type &rest attrs)
  `(push (alist :decision-type ,type
                ,(intern (format nil "~A-DECISION-ATTRIBUTES" type) :keyword)
                (alist ,@attrs))
         *decisions*))


(defun cancel-timer (timer-id)
  (%decision :cancel-timer :timer-id timer-id))


(defun cancel-workflow-execution (details)
  (%decision :cancel-workflow-execution
             :details details))


(defun complete-workflow-execution (result)
  (%decision :complete-workflow-execution :result (serialize-object result)))


(defun continue-as-new-workflow-execution (&key child-policy
                                             execution-start-to-close-timeout
                                             input
                                             tag-list
                                             task-list
                                             task-start-to-close-timeout
                                             workflow-type-version)
  (%decision :continue-as-new-workflow-execution
             :child-policy child-policy
             :execution-start-to-close-timeout execution-start-to-close-timeout
             :input (serialize-object input)
             :tag-list tag-list
             :task-list task-list
             :task-start-to-close-timeout task-start-to-close-timeout
             :workflow-type-version workflow-type-version))


(defun fail-workflow-execution (&key details reason)
  (%decision :fail-workflow-execution
             :details details
             :reason reason))


(defun record-marker (&key details marker-name)
  (%decision :record-marker
             :details details
             :marker-name marker-name))


(defun request-cancel-activity-task (activity-id)
  (%decision :request-cancel-activity-task :activity-id activity-id))


(defun request-cancel-external-workflow-execution (&key control run-id workflow-id)
  (%decision :request-cancel-external-workflow-execution
             :control control
             :run-id run-id
             :workflow-id workflow-id))


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


(defun signal-external-workflow-execution (&key control
                                             input
                                             run-id
                                             signal-name
                                             workflow-id)
  (%decision :signal-external-workflow-execution
             :control control
             :input (serialize-object input)
             :run-id run-id
             :signal-name signal-name
             :workflow-id workflow-id))


(defun start-child-workflow-execution (&key child-policy
                                         control
                                         execution-start-to-close-timeout
                                         input
                                         tag-list
                                         task-list
                                         task-start-to-close-timeout
                                         workflow-id
                                         workflow-type)
  (%decision :start-child-workflow-execution
             :child-policy child-policy
             :control control
             :execution-start-to-close-timeout execution-start-to-close-timeout
             :input (serialize-object input)
             :tag-list tag-list
             :task-list task-list
             :task-start-to-close-timeout task-start-to-close-timeout
             :workflow-id (or workflow-id (make-uuid))
             :workflow-type workflow-type))


(defun start-timer (&key control start-to-fire-timeout timer-id)
  (%decision :start-timer
             :control control
             :start-to-fire-timeout start-to-fire-timeout
             :timer-id (or timer-id (make-uuid))))



