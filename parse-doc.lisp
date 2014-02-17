(in-package :swf)

(ql:quickload "cl-html5-parser")
(ql:quickload "cl-ppcre")

(use-package :html5-parser)


(defparameter *doc-base* "http://docs.aws.amazon.com/amazonswf/latest/apireference")

(defun node-contents* (node)
  (cons (or (node-value node) "")
        (let (texts)
          (element-map-children (lambda (x)
                                  (push (node-contents x) texts))
                                node)
          (nreverse texts))))

(defun node-contents (node)
  (apply #'concatenate 'string (node-contents* node)))

(defun node-contents-trimmed (node)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (node-contents node)))


(defun find-node-if (node predicate)
  (if (funcall predicate node)
      node
      (element-map-children (lambda (n)
                              (let ((found (find-node-if n predicate)))
                                (when found
                                    (return-from find-node-if found))))
                            node)))

(defun find-next-node-if (start-node predicate)
  (loop for prev-node = start-node then node
        for node = (or (node-first-child prev-node)
                       (node-next-sibling prev-node)
                       (loop for parent = (node-parent prev-node) then (node-parent parent)
                             while parent
                             ;; do (format t "P:~S~%" parent)
                             when (node-next-sibling parent)
                             do (return (node-next-sibling parent))))
        while node
        ;;  do (format t "~S~%" node)
        if (funcall predicate node) do (return node)))


(defun parse-type-info (type)
  (cl-ppcre:register-groups-bind (array type) ("Type:\\s*(array of)?\\s*([^\\s]*)" type)
    (if array
        (list :array-of type)
        type)))


(defun parse-required-info (info)
  (if (cl-ppcre:scan "Required:\\s+Yes" info)
      t
      nil))

(defun parse-valid-values (string)
  (when string
    (loop for value in (cl-ppcre:split #\| string)
          collect (string-trim " " value))))

(defun parse-type-def (elt)
  (let ((slot-name)
        (slots))
    (element-map-children
     (lambda (elt)
       (cond ((equal "dt" (node-name elt))
              (setf slot-name (node-contents-trimmed elt)))
             ((equal "dd" (node-name elt))
              (let* ((type-elt (find-node-if elt
                                            (lambda (n)
                                              (and (stringp (node-value n))
                                                   (search "Type:" (node-value n))))))
                     (sub-type (if (node-next-sibling type-elt)
                                   (node-value (node-first-child (node-next-sibling type-elt)))
                                   ""))
                     (required-elt (find-node-if elt
                                            (lambda (n)
                                              (and (stringp (node-value n))
                                                   (search "Required:" (node-value n))))))
                     (valid-values-elt (find-node-if elt
                                                 (lambda (n)
                                                   (and (stringp (node-value n))
                                                        (search "Valid Values:" (node-value n))))))
                     (type (parse-type-info (format nil "~A~A"
                                                     (node-value type-elt)
                                                     sub-type)))
                     (required (parse-required-info (node-value required-elt)))
                     (valid-values (when valid-values-elt
                                     (parse-valid-values (node-value (node-first-child
                                                                      (node-next-sibling valid-values-elt)))))))
                (push `(,slot-name
                        :type ,type
                        :required ,required
                        ,@(if valid-values
                              `(:valid-values ,valid-values)))
                      slots)))))
     elt)
    (nreverse slots)))

(defun parse-type-section (doc header)
  (let ((contents
         (find-next-node-if
          (find-next-node-if doc (lambda (n) (equal header (node-value n))))
          (lambda (n) (equal "dl" (node-name n))))))
    (parse-type-def contents)))

(defun parse-type (name)
  (let* ((doc-text (drakma:http-request (format nil "~A/API_~A.html" *doc-base* name)))
         (type-info (parse-type-section (parse-html5 doc-text) "Contents")))
    `(define-swf-type ,name
       ,@type-info)))

(defun define-types (&rest types)
  (with-open-file (out (asdf:system-relative-pathname :amazon-swf "types.lisp")
                       :direction :output
                       :if-exists :supersede)
    (print '(in-package :swf) out)
    (terpri out)
    (dolist (type types)
      (format t "Type: ~A~%" type)
      (print (parse-type type) out)
      (terpri out))))

(defun parse-action (name emptyp)
   (let* ((doc-text (drakma:http-request (format nil "~A/API_~A.html" *doc-base* name)))
          (doc (parse-html5 doc-text))
          (request-type (parse-type-section doc "Request Parameters"))
          (response-type (unless emptyp (parse-type-section doc "Response Elements"))))
     `(define-swf-action ,name
          (,@request-type)
        ,@response-type)))

(defun define-actions (&rest actions)
  (with-open-file (out (asdf:system-relative-pathname :amazon-swf "actions.lisp")
                       :direction :output
                       :if-exists :supersede)
    (print '(in-package :swf) out)
    (terpri out)
    (loop for (action emptyp) on actions by #'cddr do
          (format t "Action: ~A~%" action)
          (print (parse-action action emptyp) out)
          (terpri out))))

(defun define-all-actions ()
  (define-actions
      "CountClosedWorkflowExecutions" nil
      "CountOpenWorkflowExecutions" nil
    "CountPendingActivityTasks" nil
    "CountPendingDecisionTasks" nil
    "DeprecateActivityType" t
    "DeprecateDomain" t
    "DeprecateWorkflowType" t
    "DescribeActivityType" nil
    "DescribeDomain" nil
    "DescribeWorkflowExecution" nil
    "DescribeWorkflowType" nil
    "GetWorkflowExecutionHistory" nil
    "ListActivityTypes" nil
    "ListClosedWorkflowExecutions" nil
    "ListDomains" nil
    "ListOpenWorkflowExecutions" nil
    "ListWorkflowTypes" nil
    "PollForActivityTask" nil
    "PollForDecisionTask" nil
    "RecordActivityTaskHeartbeat" nil
    "RegisterActivityType" t
    "RegisterDomain" t
    "RegisterWorkflowType" t
    "RequestCancelWorkflowExecution" t
    "RespondActivityTaskCanceled" t
    "RespondActivityTaskCompleted" t
    "RespondActivityTaskFailed" t
    "RespondDecisionTaskCompleted" t
    "SignalWorkflowExecution" t
    "StartWorkflowExecution" nil
    "TerminateWorkflowExecution" t))

(defun define-all-types ()
  (define-types
      "ActivityTask"
      "ActivityTaskCancelRequestedEventAttributes"
    "ActivityTaskCanceledEventAttributes"
    "ActivityTaskCompletedEventAttributes"
    "ActivityTaskFailedEventAttributes"
    "ActivityTaskScheduledEventAttributes"
    "ActivityTaskStartedEventAttributes"
    "ActivityTaskStatus"
    "ActivityTaskTimedOutEventAttributes"
    "ActivityType"
    "ActivityTypeConfiguration"
    "ActivityTypeDetail"
    "ActivityTypeInfo"
    "ActivityTypeInfos"
    "CancelTimerDecisionAttributes"
    "CancelTimerFailedEventAttributes"
    "CancelWorkflowExecutionDecisionAttributes"
    "CancelWorkflowExecutionFailedEventAttributes"
    "ChildWorkflowExecutionCanceledEventAttributes"
    "ChildWorkflowExecutionCompletedEventAttributes"
    "ChildWorkflowExecutionFailedEventAttributes"
    "ChildWorkflowExecutionStartedEventAttributes"
    "ChildWorkflowExecutionTerminatedEventAttributes"
    "ChildWorkflowExecutionTimedOutEventAttributes"
    "CloseStatusFilter"
    "CompleteWorkflowExecutionDecisionAttributes"
    "CompleteWorkflowExecutionFailedEventAttributes"
    "ContinueAsNewWorkflowExecutionDecisionAttributes"
    "ContinueAsNewWorkflowExecutionFailedEventAttributes"
    "Decision"
    "DecisionTask"
    "DecisionTaskCompletedEventAttributes"
    "DecisionTaskScheduledEventAttributes"
    "DecisionTaskStartedEventAttributes"
    "DecisionTaskTimedOutEventAttributes"
    "DomainConfiguration"
    "DomainDetail"
    "DomainInfo"
    "DomainInfos"
    "ExecutionTimeFilter"
    "ExternalWorkflowExecutionCancelRequestedEventAttributes"
    "ExternalWorkflowExecutionSignaledEventAttributes"
    "FailWorkflowExecutionDecisionAttributes"
    "FailWorkflowExecutionFailedEventAttributes"
    "History"
    "HistoryEvent"
    "MarkerRecordedEventAttributes"
    "PendingTaskCount"
    "RecordMarkerDecisionAttributes"
    "RecordMarkerFailedEventAttributes"
    "RequestCancelActivityTaskDecisionAttributes"
    "RequestCancelActivityTaskFailedEventAttributes"
    "RequestCancelExternalWorkflowExecutionDecisionAttributes"
    "RequestCancelExternalWorkflowExecutionFailedEventAttributes"
    "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes"
    "Run"
    "ScheduleActivityTaskDecisionAttributes"
    "ScheduleActivityTaskFailedEventAttributes"
    "SignalExternalWorkflowExecutionDecisionAttributes"
    "SignalExternalWorkflowExecutionFailedEventAttributes"
    "SignalExternalWorkflowExecutionInitiatedEventAttributes"
    "StartChildWorkflowExecutionDecisionAttributes"
    "StartChildWorkflowExecutionFailedEventAttributes"
    "StartChildWorkflowExecutionInitiatedEventAttributes"
    "StartTimerDecisionAttributes"
    "StartTimerFailedEventAttributes"
    "TagFilter"
    "TaskList"
    "TimerCanceledEventAttributes"
    "TimerFiredEventAttributes"
    "TimerStartedEventAttributes"
    "WorkflowExecution"
    "WorkflowExecutionCancelRequestedEventAttributes"
    "WorkflowExecutionCanceledEventAttributes"
    "WorkflowExecutionCompletedEventAttributes"
    "WorkflowExecutionConfiguration"
    "WorkflowExecutionContinuedAsNewEventAttributes"
    "WorkflowExecutionCount"
    "WorkflowExecutionDetail"
    "WorkflowExecutionFailedEventAttributes"
    "WorkflowExecutionFilter"
    "WorkflowExecutionInfo"
    "WorkflowExecutionInfos"
    "WorkflowExecutionOpenCounts"
    "WorkflowExecutionSignaledEventAttributes"
    "WorkflowExecutionStartedEventAttributes"
    "WorkflowExecutionTerminatedEventAttributes"
    "WorkflowExecutionTimedOutEventAttributes"
    "WorkflowType"
    "WorkflowTypeConfiguration"
    "WorkflowTypeDetail"
    "WorkflowTypeFilter"
    "WorkflowTypeInfo"
    "WorkflowTypeInfos"))




