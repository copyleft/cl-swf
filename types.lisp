
(IN-PACKAGE :SWF) 

(DEFINE-SWF-TYPE "ActivityTask"
  ("activityId" :TYPE "String" :REQUIRED T)
  ("activityType" :TYPE "ActivityType" :REQUIRED T)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("taskToken" :TYPE "String" :REQUIRED T)
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ActivityTaskCancelRequestedEventAttributes"
  ("activityId" :TYPE "String" :REQUIRED T)
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ActivityTaskCanceledEventAttributes"
  ("details" :TYPE "String" :REQUIRED NIL)
  ("latestCancelRequestedEventId" :TYPE "Long" :REQUIRED NIL)
  ("scheduledEventId" :TYPE "Long" :REQUIRED T)
  ("startedEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ActivityTaskCompletedEventAttributes"
  ("result" :TYPE "String" :REQUIRED NIL)
  ("scheduledEventId" :TYPE "Long" :REQUIRED T)
  ("startedEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ActivityTaskFailedEventAttributes"
  ("details" :TYPE "String" :REQUIRED NIL)
  ("reason" :TYPE "String" :REQUIRED NIL)
  ("scheduledEventId" :TYPE "Long" :REQUIRED T)
  ("startedEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ActivityTaskScheduledEventAttributes"
  ("activityId" :TYPE "String" :REQUIRED T)
  ("activityType" :TYPE "ActivityType" :REQUIRED T)
  ("control" :TYPE "String" :REQUIRED NIL)
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("heartbeatTimeout" :TYPE "String" :REQUIRED NIL)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("scheduleToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("scheduleToStartTimeout" :TYPE "String" :REQUIRED NIL)
  ("startToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("taskList" :TYPE "TaskList" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ActivityTaskStartedEventAttributes"
  ("identity" :TYPE "String" :REQUIRED NIL)
  ("scheduledEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ActivityTaskStatus"
  ("cancelRequested" :TYPE "Boolean" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ActivityTaskTimedOutEventAttributes"
  ("details" :TYPE "String" :REQUIRED NIL)
  ("scheduledEventId" :TYPE "Long" :REQUIRED T)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("timeoutType" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("START_TO_CLOSE" "SCHEDULE_TO_START" "SCHEDULE_TO_CLOSE" "HEARTBEAT"))) 

(DEFINE-SWF-TYPE "ActivityType"
  ("name" :TYPE "String" :REQUIRED T)
  ("version" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ActivityTypeConfiguration"
  ("defaultTaskHeartbeatTimeout" :TYPE "String" :REQUIRED NIL)
  ("defaultTaskList" :TYPE "TaskList" :REQUIRED NIL)
  ("defaultTaskScheduleToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("defaultTaskScheduleToStartTimeout" :TYPE "String" :REQUIRED NIL)
  ("defaultTaskStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "ActivityTypeDetail"
  ("configuration" :TYPE "ActivityTypeConfiguration" :REQUIRED T)
  ("typeInfo" :TYPE "ActivityTypeInfo" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ActivityTypeInfo"
  ("activityType" :TYPE "ActivityType" :REQUIRED T)
  ("creationDate" :TYPE "DateTime" :REQUIRED T)
  ("deprecationDate" :TYPE "DateTime" :REQUIRED NIL)
  ("description" :TYPE "String" :REQUIRED NIL)
  ("status" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("REGISTERED" "DEPRECATED"))) 

(DEFINE-SWF-TYPE "ActivityTypeInfos"
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)
  ("typeInfos" :TYPE (:ARRAY-OF "ActivityTypeInfo") :REQUIRED T)) 

(DEFINE-SWF-TYPE "CancelTimerDecisionAttributes"
  ("timerId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "CancelTimerFailedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("TIMER_ID_UNKNOWN" "OPERATION_NOT_PERMITTED"))
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("timerId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "CancelWorkflowExecutionDecisionAttributes"
  ("details" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "CancelWorkflowExecutionFailedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("UNHANDLED_DECISION" "OPERATION_NOT_PERMITTED"))
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ChildWorkflowExecutionCanceledEventAttributes"
  ("details" :TYPE "String" :REQUIRED NIL)
  ("initiatedEventId" :TYPE "Long" :REQUIRED T)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ChildWorkflowExecutionCompletedEventAttributes"
  ("initiatedEventId" :TYPE "Long" :REQUIRED T)
  ("result" :TYPE "String" :REQUIRED NIL)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ChildWorkflowExecutionFailedEventAttributes"
  ("details" :TYPE "String" :REQUIRED NIL)
  ("initiatedEventId" :TYPE "Long" :REQUIRED T)
  ("reason" :TYPE "String" :REQUIRED NIL)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ChildWorkflowExecutionStartedEventAttributes"
  ("initiatedEventId" :TYPE "Long" :REQUIRED T)
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ChildWorkflowExecutionTerminatedEventAttributes"
  ("initiatedEventId" :TYPE "Long" :REQUIRED T)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ChildWorkflowExecutionTimedOutEventAttributes"
  ("initiatedEventId" :TYPE "Long" :REQUIRED T)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("timeoutType" :TYPE "String" :REQUIRED T :VALID-VALUES ("START_TO_CLOSE"))
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "CloseStatusFilter"
  ("status" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("COMPLETED" "FAILED" "CANCELED" "TERMINATED" "CONTINUED_AS_NEW"
    "TIMED_OUT"))) 

(DEFINE-SWF-TYPE "CompleteWorkflowExecutionDecisionAttributes"
  ("result" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "CompleteWorkflowExecutionFailedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("UNHANDLED_DECISION" "OPERATION_NOT_PERMITTED"))
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ContinueAsNewWorkflowExecutionDecisionAttributes"
  ("childPolicy" :TYPE "String" :REQUIRED NIL :VALID-VALUES
   ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
  ("executionStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("tagList" :TYPE (:ARRAY-OF "Strings") :REQUIRED NIL)
  ("taskList" :TYPE "TaskList" :REQUIRED NIL)
  ("taskStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("workflowTypeVersion" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "ContinueAsNewWorkflowExecutionFailedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("UNHANDLED_DECISION" "WORKFLOW_TYPE_DEPRECATED"
    "WORKFLOW_TYPE_DOES_NOT_EXIST"
    "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    "DEFAULT_TASK_LIST_UNDEFINED" "DEFAULT_CHILD_POLICY_UNDEFINED"
    "OPERATION_NOT_PERMITTED"))
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "Decision"
  ("cancelTimerDecisionAttributes" :TYPE "CancelTimerDecisionAttributes"
   :REQUIRED NIL)
  ("cancelWorkflowExecutionDecisionAttributes" :TYPE
   "CancelWorkflowExecutionDecisionAttributes" :REQUIRED NIL)
  ("completeWorkflowExecutionDecisionAttributes" :TYPE
   "CompleteWorkflowExecutionDecisionAttributes" :REQUIRED NIL)
  ("continueAsNewWorkflowExecutionDecisionAttributes" :TYPE
   "ContinueAsNewWorkflowExecutionDecisionAttributes" :REQUIRED NIL)
  ("decisionType" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("ScheduleActivityTask" "RequestCancelActivityTask"
    "CompleteWorkflowExecution" "FailWorkflowExecution"
    "CancelWorkflowExecution" "ContinueAsNewWorkflowExecution" "RecordMarker"
    "StartTimer" "CancelTimer" "SignalExternalWorkflowExecution"
    "RequestCancelExternalWorkflowExecution" "StartChildWorkflowExecution"))
  ("failWorkflowExecutionDecisionAttributes" :TYPE
   "FailWorkflowExecutionDecisionAttributes" :REQUIRED NIL)
  ("recordMarkerDecisionAttributes" :TYPE "RecordMarkerDecisionAttributes"
   :REQUIRED NIL)
  ("requestCancelActivityTaskDecisionAttributes" :TYPE
   "RequestCancelActivityTaskDecisionAttributes" :REQUIRED NIL)
  ("requestCancelExternalWorkflowExecutionDecisionAttributes" :TYPE
   "RequestCancelExternalWorkflowExecutionDecisionAttributes" :REQUIRED NIL)
  ("scheduleActivityTaskDecisionAttributes" :TYPE
   "ScheduleActivityTaskDecisionAttributes" :REQUIRED NIL)
  ("signalExternalWorkflowExecutionDecisionAttributes" :TYPE
   "SignalExternalWorkflowExecutionDecisionAttributes" :REQUIRED NIL)
  ("startChildWorkflowExecutionDecisionAttributes" :TYPE
   "StartChildWorkflowExecutionDecisionAttributes" :REQUIRED NIL)
  ("startTimerDecisionAttributes" :TYPE "StartTimerDecisionAttributes"
   :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "DecisionTask"
  ("events" :TYPE (:ARRAY-OF "HistoryEvent") :REQUIRED T)
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)
  ("previousStartedEventId" :TYPE "Long" :REQUIRED NIL)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("taskToken" :TYPE "String" :REQUIRED T)
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "DecisionTaskCompletedEventAttributes"
  ("executionContext" :TYPE "String" :REQUIRED NIL)
  ("scheduledEventId" :TYPE "Long" :REQUIRED T)
  ("startedEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "DecisionTaskScheduledEventAttributes"
  ("startToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("taskList" :TYPE "TaskList" :REQUIRED T)) 

(DEFINE-SWF-TYPE "DecisionTaskStartedEventAttributes"
  ("identity" :TYPE "String" :REQUIRED NIL)
  ("scheduledEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "DecisionTaskTimedOutEventAttributes"
  ("scheduledEventId" :TYPE "Long" :REQUIRED T)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("timeoutType" :TYPE "String" :REQUIRED T :VALID-VALUES ("START_TO_CLOSE"))) 

(DEFINE-SWF-TYPE "DomainConfiguration"
  ("workflowExecutionRetentionPeriodInDays" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "DomainDetail"
  ("configuration" :TYPE "DomainConfiguration" :REQUIRED T)
  ("domainInfo" :TYPE "DomainInfo" :REQUIRED T)) 

(DEFINE-SWF-TYPE "DomainInfo"
  ("description" :TYPE "String" :REQUIRED NIL)
  ("name" :TYPE "String" :REQUIRED T)
  ("status" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("REGISTERED" "DEPRECATED"))) 

(DEFINE-SWF-TYPE "DomainInfos"
  ("domainInfos" :TYPE (:ARRAY-OF "DomainInfo") :REQUIRED T)
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "ExecutionTimeFilter"
  ("latestDate" :TYPE "DateTime" :REQUIRED NIL)
  ("oldestDate" :TYPE "DateTime" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ExternalWorkflowExecutionCancelRequestedEventAttributes"
  ("initiatedEventId" :TYPE "Long" :REQUIRED T)
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)) 

(DEFINE-SWF-TYPE "ExternalWorkflowExecutionSignaledEventAttributes"
  ("initiatedEventId" :TYPE "Long" :REQUIRED T)
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)) 

(DEFINE-SWF-TYPE "FailWorkflowExecutionDecisionAttributes"
  ("details" :TYPE "String" :REQUIRED NIL)
  ("reason" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "FailWorkflowExecutionFailedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("UNHANDLED_DECISION" "OPERATION_NOT_PERMITTED"))
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "History"
  ("events" :TYPE (:ARRAY-OF "HistoryEvent") :REQUIRED T)
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "HistoryEvent"
  ("activityTaskCancelRequestedEventAttributes" :TYPE
   "ActivityTaskCancelRequestedEventAttributes" :REQUIRED NIL)
  ("activityTaskCanceledEventAttributes" :TYPE
   "ActivityTaskCanceledEventAttributes" :REQUIRED NIL)
  ("activityTaskCompletedEventAttributes" :TYPE
   "ActivityTaskCompletedEventAttributes" :REQUIRED NIL)
  ("activityTaskFailedEventAttributes" :TYPE
   "ActivityTaskFailedEventAttributes" :REQUIRED NIL)
  ("activityTaskScheduledEventAttributes" :TYPE
   "ActivityTaskScheduledEventAttributes" :REQUIRED NIL)
  ("activityTaskStartedEventAttributes" :TYPE
   "ActivityTaskStartedEventAttributes" :REQUIRED NIL)
  ("activityTaskTimedOutEventAttributes" :TYPE
   "ActivityTaskTimedOutEventAttributes" :REQUIRED NIL)
  ("cancelTimerFailedEventAttributes" :TYPE "CancelTimerFailedEventAttributes"
   :REQUIRED NIL)
  ("cancelWorkflowExecutionFailedEventAttributes" :TYPE
   "CancelWorkflowExecutionFailedEventAttributes" :REQUIRED NIL)
  ("childWorkflowExecutionCanceledEventAttributes" :TYPE
   "ChildWorkflowExecutionCanceledEventAttributes" :REQUIRED NIL)
  ("childWorkflowExecutionCompletedEventAttributes" :TYPE
   "ChildWorkflowExecutionCompletedEventAttributes" :REQUIRED NIL)
  ("childWorkflowExecutionFailedEventAttributes" :TYPE
   "ChildWorkflowExecutionFailedEventAttributes" :REQUIRED NIL)
  ("childWorkflowExecutionStartedEventAttributes" :TYPE
   "ChildWorkflowExecutionStartedEventAttributes" :REQUIRED NIL)
  ("childWorkflowExecutionTerminatedEventAttributes" :TYPE
   "ChildWorkflowExecutionTerminatedEventAttributes" :REQUIRED NIL)
  ("childWorkflowExecutionTimedOutEventAttributes" :TYPE
   "ChildWorkflowExecutionTimedOutEventAttributes" :REQUIRED NIL)
  ("completeWorkflowExecutionFailedEventAttributes" :TYPE
   "CompleteWorkflowExecutionFailedEventAttributes" :REQUIRED NIL)
  ("continueAsNewWorkflowExecutionFailedEventAttributes" :TYPE
   "ContinueAsNewWorkflowExecutionFailedEventAttributes" :REQUIRED NIL)
  ("decisionTaskCompletedEventAttributes" :TYPE
   "DecisionTaskCompletedEventAttributes" :REQUIRED NIL)
  ("decisionTaskScheduledEventAttributes" :TYPE
   "DecisionTaskScheduledEventAttributes" :REQUIRED NIL)
  ("decisionTaskStartedEventAttributes" :TYPE
   "DecisionTaskStartedEventAttributes" :REQUIRED NIL)
  ("decisionTaskTimedOutEventAttributes" :TYPE
   "DecisionTaskTimedOutEventAttributes" :REQUIRED NIL)
  ("eventId" :TYPE "Long" :REQUIRED T)
  ("eventTimestamp" :TYPE "DateTime" :REQUIRED T)
  ("eventType" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("WorkflowExecutionStarted" "WorkflowExecutionCancelRequested"
    "WorkflowExecutionCompleted" "CompleteWorkflowExecutionFailed"
    "WorkflowExecutionFailed" "FailWorkflowExecutionFailed"
    "WorkflowExecutionTimedOut" "WorkflowExecutionCanceled"
    "CancelWorkflowExecutionFailed" "WorkflowExecutionContinuedAsNew"
    "ContinueAsNewWorkflowExecutionFailed" "WorkflowExecutionTerminated"
    "DecisionTaskScheduled" "DecisionTaskStarted" "DecisionTaskCompleted"
    "DecisionTaskTimedOut" "ActivityTaskScheduled" "ScheduleActivityTaskFailed"
    "ActivityTaskStarted" "ActivityTaskCompleted" "ActivityTaskFailed"
    "ActivityTaskTimedOut" "ActivityTaskCanceled" "ActivityTaskCancelRequested"
    "RequestCancelActivityTaskFailed" "WorkflowExecutionSignaled"
    "MarkerRecorded" "RecordMarkerFailed" "TimerStarted" "StartTimerFailed"
    "TimerFired" "TimerCanceled" "CancelTimerFailed"
    "StartChildWorkflowExecutionInitiated" "StartChildWorkflowExecutionFailed"
    "ChildWorkflowExecutionStarted" "ChildWorkflowExecutionCompleted"
    "ChildWorkflowExecutionFailed" "ChildWorkflowExecutionTimedOut"
    "ChildWorkflowExecutionCanceled" "ChildWorkflowExecutionTerminated"
    "SignalExternalWorkflowExecutionInitiated"
    "SignalExternalWorkflowExecutionFailed" "ExternalWorkflowExecutionSignaled"
    "RequestCancelExternalWorkflowExecutionInitiated"
    "RequestCancelExternalWorkflowExecutionFailed"
    "ExternalWorkflowExecutionCancelRequested"))
  ("externalWorkflowExecutionCancelRequestedEventAttributes" :TYPE
   "ExternalWorkflowExecutionCancelRequestedEventAttributes" :REQUIRED NIL)
  ("externalWorkflowExecutionSignaledEventAttributes" :TYPE
   "ExternalWorkflowExecutionSignaledEventAttributes" :REQUIRED NIL)
  ("failWorkflowExecutionFailedEventAttributes" :TYPE
   "FailWorkflowExecutionFailedEventAttributes" :REQUIRED NIL)
  ("markerRecordedEventAttributes" :TYPE "MarkerRecordedEventAttributes"
   :REQUIRED NIL)
  ("recordMarkerFailedEventAttributes" :TYPE
   "RecordMarkerFailedEventAttributes" :REQUIRED NIL)
  ("requestCancelActivityTaskFailedEventAttributes" :TYPE
   "RequestCancelActivityTaskFailedEventAttributes" :REQUIRED NIL)
  ("requestCancelExternalWorkflowExecutionFailedEventAttributes" :TYPE
   "RequestCancelExternalWorkflowExecutionFailedEventAttributes" :REQUIRED NIL)
  ("requestCancelExternalWorkflowExecutionInitiatedEventAttributes" :TYPE
   "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes" :REQUIRED
   NIL)
  ("scheduleActivityTaskFailedEventAttributes" :TYPE
   "ScheduleActivityTaskFailedEventAttributes" :REQUIRED NIL)
  ("signalExternalWorkflowExecutionFailedEventAttributes" :TYPE
   "SignalExternalWorkflowExecutionFailedEventAttributes" :REQUIRED NIL)
  ("signalExternalWorkflowExecutionInitiatedEventAttributes" :TYPE
   "SignalExternalWorkflowExecutionInitiatedEventAttributes" :REQUIRED NIL)
  ("startChildWorkflowExecutionFailedEventAttributes" :TYPE
   "StartChildWorkflowExecutionFailedEventAttributes" :REQUIRED NIL)
  ("startChildWorkflowExecutionInitiatedEventAttributes" :TYPE
   "StartChildWorkflowExecutionInitiatedEventAttributes" :REQUIRED NIL)
  ("startTimerFailedEventAttributes" :TYPE "StartTimerFailedEventAttributes"
   :REQUIRED NIL)
  ("timerCanceledEventAttributes" :TYPE "TimerCanceledEventAttributes"
   :REQUIRED NIL)
  ("timerFiredEventAttributes" :TYPE "TimerFiredEventAttributes" :REQUIRED NIL)
  ("timerStartedEventAttributes" :TYPE "TimerStartedEventAttributes" :REQUIRED
   NIL)
  ("workflowExecutionCancelRequestedEventAttributes" :TYPE
   "WorkflowExecutionCancelRequestedEventAttributes" :REQUIRED NIL)
  ("workflowExecutionCanceledEventAttributes" :TYPE
   "WorkflowExecutionCanceledEventAttributes" :REQUIRED NIL)
  ("workflowExecutionCompletedEventAttributes" :TYPE
   "WorkflowExecutionCompletedEventAttributes" :REQUIRED NIL)
  ("workflowExecutionContinuedAsNewEventAttributes" :TYPE
   "WorkflowExecutionContinuedAsNewEventAttributes" :REQUIRED NIL)
  ("workflowExecutionFailedEventAttributes" :TYPE
   "WorkflowExecutionFailedEventAttributes" :REQUIRED NIL)
  ("workflowExecutionSignaledEventAttributes" :TYPE
   "WorkflowExecutionSignaledEventAttributes" :REQUIRED NIL)
  ("workflowExecutionStartedEventAttributes" :TYPE
   "WorkflowExecutionStartedEventAttributes" :REQUIRED NIL)
  ("workflowExecutionTerminatedEventAttributes" :TYPE
   "WorkflowExecutionTerminatedEventAttributes" :REQUIRED NIL)
  ("workflowExecutionTimedOutEventAttributes" :TYPE
   "WorkflowExecutionTimedOutEventAttributes" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "MarkerRecordedEventAttributes"
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("details" :TYPE "String" :REQUIRED NIL)
  ("markerName" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "PendingTaskCount"
  ("count" :TYPE "Number" :REQUIRED T)
  ("truncated" :TYPE "Boolean" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "RecordMarkerDecisionAttributes"
  ("details" :TYPE "String" :REQUIRED NIL)
  ("markerName" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "RecordMarkerFailedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES ("OPERATION_NOT_PERMITTED"))
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("markerName" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "RequestCancelActivityTaskDecisionAttributes"
  ("activityId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "RequestCancelActivityTaskFailedEventAttributes"
  ("activityId" :TYPE "String" :REQUIRED T)
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("ACTIVITY_ID_UNKNOWN" "OPERATION_NOT_PERMITTED"))
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "RequestCancelExternalWorkflowExecutionDecisionAttributes"
  ("control" :TYPE "String" :REQUIRED NIL)
  ("runId" :TYPE "String" :REQUIRED NIL)
  ("workflowId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "RequestCancelExternalWorkflowExecutionFailedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"
    "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    "OPERATION_NOT_PERMITTED"))
  ("control" :TYPE "String" :REQUIRED NIL)
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("initiatedEventId" :TYPE "Long" :REQUIRED T)
  ("runId" :TYPE "String" :REQUIRED NIL)
  ("workflowId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes"
  ("control" :TYPE "String" :REQUIRED NIL)
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("runId" :TYPE "String" :REQUIRED NIL)
  ("workflowId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "Run"
  ("runId" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "ScheduleActivityTaskDecisionAttributes"
  ("activityId" :TYPE "String" :REQUIRED T)
  ("activityType" :TYPE "ActivityType" :REQUIRED T)
  ("control" :TYPE "String" :REQUIRED NIL)
  ("heartbeatTimeout" :TYPE "String" :REQUIRED NIL)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("scheduleToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("scheduleToStartTimeout" :TYPE "String" :REQUIRED NIL)
  ("startToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("taskList" :TYPE "TaskList" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "ScheduleActivityTaskFailedEventAttributes"
  ("activityId" :TYPE "String" :REQUIRED T)
  ("activityType" :TYPE "ActivityType" :REQUIRED T)
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("ACTIVITY_TYPE_DEPRECATED" "ACTIVITY_TYPE_DOES_NOT_EXIST"
    "ACTIVITY_ID_ALREADY_IN_USE" "OPEN_ACTIVITIES_LIMIT_EXCEEDED"
    "ACTIVITY_CREATION_RATE_EXCEEDED"
    "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED" "DEFAULT_TASK_LIST_UNDEFINED"
    "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"
    "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED" "OPERATION_NOT_PERMITTED"))
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)) 

(DEFINE-SWF-TYPE "SignalExternalWorkflowExecutionDecisionAttributes"
  ("control" :TYPE "String" :REQUIRED NIL)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("runId" :TYPE "String" :REQUIRED NIL)
  ("signalName" :TYPE "String" :REQUIRED T)
  ("workflowId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "SignalExternalWorkflowExecutionFailedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"
    "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    "OPERATION_NOT_PERMITTED"))
  ("control" :TYPE "String" :REQUIRED NIL)
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("initiatedEventId" :TYPE "Long" :REQUIRED T)
  ("runId" :TYPE "String" :REQUIRED NIL)
  ("workflowId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "SignalExternalWorkflowExecutionInitiatedEventAttributes"
  ("control" :TYPE "String" :REQUIRED NIL)
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("runId" :TYPE "String" :REQUIRED NIL)
  ("signalName" :TYPE "String" :REQUIRED T)
  ("workflowId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "StartChildWorkflowExecutionDecisionAttributes"
  ("childPolicy" :TYPE "String" :REQUIRED NIL :VALID-VALUES
   ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
  ("control" :TYPE "String" :REQUIRED NIL)
  ("executionStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("tagList" :TYPE (:ARRAY-OF "Strings") :REQUIRED NIL)
  ("taskList" :TYPE "TaskList" :REQUIRED NIL)
  ("taskStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("workflowId" :TYPE "String" :REQUIRED T)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "StartChildWorkflowExecutionFailedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("WORKFLOW_TYPE_DOES_NOT_EXIST" "WORKFLOW_TYPE_DEPRECATED"
    "OPEN_CHILDREN_LIMIT_EXCEEDED" "OPEN_WORKFLOWS_LIMIT_EXCEEDED"
    "CHILD_CREATION_RATE_EXCEEDED" "WORKFLOW_ALREADY_RUNNING"
    "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    "DEFAULT_TASK_LIST_UNDEFINED"
    "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    "DEFAULT_CHILD_POLICY_UNDEFINED" "OPERATION_NOT_PERMITTED"))
  ("control" :TYPE "String" :REQUIRED NIL)
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("initiatedEventId" :TYPE "Long" :REQUIRED T)
  ("workflowId" :TYPE "String" :REQUIRED T)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "StartChildWorkflowExecutionInitiatedEventAttributes"
  ("childPolicy" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
  ("control" :TYPE "String" :REQUIRED NIL)
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("executionStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("tagList" :TYPE (:ARRAY-OF "Strings") :REQUIRED NIL)
  ("taskList" :TYPE "TaskList" :REQUIRED T)
  ("taskStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("workflowId" :TYPE "String" :REQUIRED T)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "StartTimerDecisionAttributes"
  ("control" :TYPE "String" :REQUIRED NIL)
  ("startToFireTimeout" :TYPE "String" :REQUIRED T)
  ("timerId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "StartTimerFailedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("TIMER_ID_ALREADY_IN_USE" "OPEN_TIMERS_LIMIT_EXCEEDED"
    "TIMER_CREATION_RATE_EXCEEDED" "OPERATION_NOT_PERMITTED"))
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("timerId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "TagFilter"
  ("tag" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "TaskList"
  ("name" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "TimerCanceledEventAttributes"
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("timerId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "TimerFiredEventAttributes"
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("timerId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "TimerStartedEventAttributes"
  ("control" :TYPE "String" :REQUIRED NIL)
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("startToFireTimeout" :TYPE "String" :REQUIRED T)
  ("timerId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowExecution"
  ("runId" :TYPE "String" :REQUIRED T)
  ("workflowId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowExecutionCancelRequestedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED NIL :VALID-VALUES ("CHILD_POLICY_APPLIED"))
  ("externalInitiatedEventId" :TYPE "Long" :REQUIRED NIL)
  ("externalWorkflowExecution" :TYPE "WorkflowExecution" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "WorkflowExecutionCanceledEventAttributes"
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("details" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "WorkflowExecutionCompletedEventAttributes"
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("result" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "WorkflowExecutionConfiguration"
  ("childPolicy" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
  ("executionStartToCloseTimeout" :TYPE "String" :REQUIRED T)
  ("taskList" :TYPE "TaskList" :REQUIRED T)
  ("taskStartToCloseTimeout" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowExecutionContinuedAsNewEventAttributes"
  ("childPolicy" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("executionStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("newExecutionRunId" :TYPE "String" :REQUIRED T)
  ("tagList" :TYPE (:ARRAY-OF "Strings") :REQUIRED NIL)
  ("taskList" :TYPE "TaskList" :REQUIRED T)
  ("taskStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowExecutionCount"
  ("count" :TYPE "Number" :REQUIRED T)
  ("truncated" :TYPE "Boolean" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "WorkflowExecutionDetail"
  ("executionConfiguration" :TYPE "WorkflowExecutionConfiguration" :REQUIRED T)
  ("executionInfo" :TYPE "WorkflowExecutionInfo" :REQUIRED T)
  ("latestActivityTaskTimestamp" :TYPE "DateTime" :REQUIRED NIL)
  ("latestExecutionContext" :TYPE "String" :REQUIRED NIL)
  ("openCounts" :TYPE "WorkflowExecutionOpenCounts" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowExecutionFailedEventAttributes"
  ("decisionTaskCompletedEventId" :TYPE "Long" :REQUIRED T)
  ("details" :TYPE "String" :REQUIRED NIL)
  ("reason" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "WorkflowExecutionFilter"
  ("workflowId" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowExecutionInfo"
  ("cancelRequested" :TYPE "Boolean" :REQUIRED NIL)
  ("closeStatus" :TYPE "String" :REQUIRED NIL :VALID-VALUES
   ("COMPLETED" "FAILED" "CANCELED" "TERMINATED" "CONTINUED_AS_NEW"
    "TIMED_OUT"))
  ("closeTimestamp" :TYPE "DateTime" :REQUIRED NIL)
  ("execution" :TYPE "WorkflowExecution" :REQUIRED T)
  ("executionStatus" :TYPE "String" :REQUIRED T :VALID-VALUES ("OPEN" "CLOSED"))
  ("parent" :TYPE "WorkflowExecution" :REQUIRED NIL)
  ("startTimestamp" :TYPE "DateTime" :REQUIRED T)
  ("tagList" :TYPE (:ARRAY-OF "Strings") :REQUIRED NIL)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowExecutionInfos"
  ("executionInfos" :TYPE (:ARRAY-OF "WorkflowExecutionInfo") :REQUIRED T)
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "WorkflowExecutionOpenCounts"
  ("openActivityTasks" :TYPE "Number" :REQUIRED T)
  ("openChildWorkflowExecutions" :TYPE "Number" :REQUIRED T)
  ("openDecisionTasks" :TYPE "Number" :REQUIRED T)
  ("openTimers" :TYPE "Number" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowExecutionSignaledEventAttributes"
  ("externalInitiatedEventId" :TYPE "Long" :REQUIRED NIL)
  ("externalWorkflowExecution" :TYPE "WorkflowExecution" :REQUIRED NIL)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("signalName" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowExecutionStartedEventAttributes"
  ("childPolicy" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
  ("continuedExecutionRunId" :TYPE "String" :REQUIRED NIL)
  ("executionStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("parentInitiatedEventId" :TYPE "Long" :REQUIRED NIL)
  ("parentWorkflowExecution" :TYPE "WorkflowExecution" :REQUIRED NIL)
  ("tagList" :TYPE (:ARRAY-OF "Strings") :REQUIRED NIL)
  ("taskList" :TYPE "TaskList" :REQUIRED T)
  ("taskStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowExecutionTerminatedEventAttributes"
  ("cause" :TYPE "String" :REQUIRED NIL :VALID-VALUES
   ("CHILD_POLICY_APPLIED" "EVENT_LIMIT_EXCEEDED" "OPERATOR_INITIATED"))
  ("childPolicy" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
  ("details" :TYPE "String" :REQUIRED NIL)
  ("reason" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "WorkflowExecutionTimedOutEventAttributes"
  ("childPolicy" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
  ("timeoutType" :TYPE "String" :REQUIRED T :VALID-VALUES ("START_TO_CLOSE"))) 

(DEFINE-SWF-TYPE "WorkflowType"
  ("name" :TYPE "String" :REQUIRED T)
  ("version" :TYPE "String" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowTypeConfiguration"
  ("defaultChildPolicy" :TYPE "String" :REQUIRED NIL :VALID-VALUES
   ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
  ("defaultExecutionStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
  ("defaultTaskList" :TYPE "TaskList" :REQUIRED NIL)
  ("defaultTaskStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "WorkflowTypeDetail"
  ("configuration" :TYPE "WorkflowTypeConfiguration" :REQUIRED T)
  ("typeInfo" :TYPE "WorkflowTypeInfo" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowTypeFilter"
  ("name" :TYPE "String" :REQUIRED T)
  ("version" :TYPE "String" :REQUIRED NIL)) 

(DEFINE-SWF-TYPE "WorkflowTypeInfo"
  ("creationDate" :TYPE "DateTime" :REQUIRED T)
  ("deprecationDate" :TYPE "DateTime" :REQUIRED NIL)
  ("description" :TYPE "String" :REQUIRED NIL)
  ("status" :TYPE "String" :REQUIRED T :VALID-VALUES
   ("REGISTERED" "DEPRECATED"))
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(DEFINE-SWF-TYPE "WorkflowTypeInfos"
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)
  ("typeInfos" :TYPE (:ARRAY-OF "WorkflowTypeInfo") :REQUIRED T)) 
