
(IN-PACKAGE :SWF) 

(SWF::DEFINE-SWF-ACTION "CountClosedWorkflowExecutions"
    (("closeStatusFilter" :TYPE "CloseStatusFilter" :REQUIRED NIL)
     ("closeTimeFilter" :TYPE "ExecutionTimeFilter" :REQUIRED NIL)
     ("domain" :TYPE "String" :REQUIRED T)
     ("executionFilter" :TYPE "WorkflowExecutionFilter" :REQUIRED NIL)
     ("startTimeFilter" :TYPE "ExecutionTimeFilter" :REQUIRED NIL)
     ("tagFilter" :TYPE "TagFilter" :REQUIRED NIL)
     ("typeFilter" :TYPE "WorkflowTypeFilter" :REQUIRED NIL))
  ("count" :TYPE "Number" :REQUIRED T)
  ("truncated" :TYPE "Boolean" :REQUIRED NIL)) 

(SWF::DEFINE-SWF-ACTION "CountOpenWorkflowExecutions"
    (("domain" :TYPE "String" :REQUIRED T)
     ("executionFilter" :TYPE "WorkflowExecutionFilter" :REQUIRED NIL)
     ("startTimeFilter" :TYPE "ExecutionTimeFilter" :REQUIRED T)
     ("tagFilter" :TYPE "TagFilter" :REQUIRED NIL)
     ("typeFilter" :TYPE "WorkflowTypeFilter" :REQUIRED NIL))
  ("count" :TYPE "Number" :REQUIRED T)
  ("truncated" :TYPE "Boolean" :REQUIRED NIL)) 

(SWF::DEFINE-SWF-ACTION "CountPendingActivityTasks"
    (("domain" :TYPE "String" :REQUIRED T)
     ("taskList" :TYPE "TaskList" :REQUIRED T))
  ("count" :TYPE "Number" :REQUIRED T)
  ("truncated" :TYPE "Boolean" :REQUIRED NIL)) 

(SWF::DEFINE-SWF-ACTION "CountPendingDecisionTasks"
    (("domain" :TYPE "String" :REQUIRED T)
     ("taskList" :TYPE "TaskList" :REQUIRED T))
  ("count" :TYPE "Number" :REQUIRED T)
  ("truncated" :TYPE "Boolean" :REQUIRED NIL)) 

(SWF::DEFINE-SWF-ACTION "DeprecateActivityType"
    (("activityType" :TYPE "ActivityType" :REQUIRED T)
     ("domain" :TYPE "String" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "DeprecateDomain"
    (("name" :TYPE "String" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "DeprecateWorkflowType"
    (("domain" :TYPE "String" :REQUIRED T)
     ("workflowType" :TYPE "WorkflowType" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "DescribeActivityType"
    (("activityType" :TYPE "ActivityType" :REQUIRED T)
     ("domain" :TYPE "String" :REQUIRED T))
  ("configuration" :TYPE "ActivityTypeConfiguration" :REQUIRED T)
  ("typeInfo" :TYPE "ActivityTypeInfo" :REQUIRED T)) 

(SWF::DEFINE-SWF-ACTION "DescribeDomain"
    (("name" :TYPE "String" :REQUIRED T))
  ("configuration" :TYPE "DomainConfiguration" :REQUIRED T)
  ("domainInfo" :TYPE "DomainInfo" :REQUIRED T)) 

(SWF::DEFINE-SWF-ACTION "DescribeWorkflowExecution"
    (("domain" :TYPE "String" :REQUIRED T)
     ("execution" :TYPE "WorkflowExecution" :REQUIRED T))
  ("executionConfiguration" :TYPE "WorkflowExecutionConfiguration" :REQUIRED T)
  ("executionInfo" :TYPE "WorkflowExecutionInfo" :REQUIRED T)
  ("latestActivityTaskTimestamp" :TYPE "DateTime" :REQUIRED NIL)
  ("latestExecutionContext" :TYPE "String" :REQUIRED NIL)
  ("openCounts" :TYPE "WorkflowExecutionOpenCounts" :REQUIRED T)) 

(SWF::DEFINE-SWF-ACTION "DescribeWorkflowType"
    (("domain" :TYPE "String" :REQUIRED T)
     ("workflowType" :TYPE "WorkflowType" :REQUIRED T))
  ("configuration" :TYPE "WorkflowTypeConfiguration" :REQUIRED T)
  ("typeInfo" :TYPE "WorkflowTypeInfo" :REQUIRED T)) 

(SWF::DEFINE-SWF-ACTION "GetWorkflowExecutionHistory"
    (("domain" :TYPE "String" :REQUIRED T)
     ("execution" :TYPE "WorkflowExecution" :REQUIRED T)
     ("maximumPageSize" :TYPE "Number" :REQUIRED NIL)
     ("nextPageToken" :TYPE "String" :REQUIRED NIL)
     ("reverseOrder" :TYPE "Boolean" :REQUIRED NIL))
  ("events" :TYPE (:ARRAY-OF "HistoryEvent") :REQUIRED T)
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)) 

(SWF::DEFINE-SWF-ACTION "ListActivityTypes"
    (("domain" :TYPE "String" :REQUIRED T)
     ("maximumPageSize" :TYPE "Number" :REQUIRED NIL)
     ("name" :TYPE "String" :REQUIRED NIL)
     ("nextPageToken" :TYPE "String" :REQUIRED NIL)
     ("registrationStatus" :TYPE "String" :REQUIRED T :VALID-VALUES
      ("REGISTERED" "DEPRECATED"))
     ("reverseOrder" :TYPE "Boolean" :REQUIRED NIL))
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)
  ("typeInfos" :TYPE (:ARRAY-OF "ActivityTypeInfo") :REQUIRED T)) 

(SWF::DEFINE-SWF-ACTION "ListClosedWorkflowExecutions"
    (("closeStatusFilter" :TYPE "CloseStatusFilter" :REQUIRED NIL)
     ("closeTimeFilter" :TYPE "ExecutionTimeFilter" :REQUIRED NIL)
     ("domain" :TYPE "String" :REQUIRED T)
     ("executionFilter" :TYPE "WorkflowExecutionFilter" :REQUIRED NIL)
     ("maximumPageSize" :TYPE "Number" :REQUIRED NIL)
     ("nextPageToken" :TYPE "String" :REQUIRED NIL)
     ("reverseOrder" :TYPE "Boolean" :REQUIRED NIL)
     ("startTimeFilter" :TYPE "ExecutionTimeFilter" :REQUIRED NIL)
     ("tagFilter" :TYPE "TagFilter" :REQUIRED NIL)
     ("typeFilter" :TYPE "WorkflowTypeFilter" :REQUIRED NIL))
  ("executionInfos" :TYPE (:ARRAY-OF "WorkflowExecutionInfo") :REQUIRED T)
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)) 

(SWF::DEFINE-SWF-ACTION "ListDomains"
    (("maximumPageSize" :TYPE "Number" :REQUIRED NIL)
     ("nextPageToken" :TYPE "String" :REQUIRED NIL)
     ("registrationStatus" :TYPE "String" :REQUIRED T :VALID-VALUES
      ("REGISTERED" "DEPRECATED"))
     ("reverseOrder" :TYPE "Boolean" :REQUIRED NIL))
  ("domainInfos" :TYPE (:ARRAY-OF "DomainInfo") :REQUIRED T)
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)) 

(SWF::DEFINE-SWF-ACTION "ListOpenWorkflowExecutions"
    (("domain" :TYPE "String" :REQUIRED T)
     ("executionFilter" :TYPE "WorkflowExecutionFilter" :REQUIRED NIL)
     ("maximumPageSize" :TYPE "Number" :REQUIRED NIL)
     ("nextPageToken" :TYPE "String" :REQUIRED NIL)
     ("reverseOrder" :TYPE "Boolean" :REQUIRED NIL)
     ("startTimeFilter" :TYPE "ExecutionTimeFilter" :REQUIRED T)
     ("tagFilter" :TYPE "TagFilter" :REQUIRED NIL)
     ("typeFilter" :TYPE "WorkflowTypeFilter" :REQUIRED NIL))
  ("executionInfos" :TYPE (:ARRAY-OF "WorkflowExecutionInfo") :REQUIRED T)
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)) 

(SWF::DEFINE-SWF-ACTION "ListWorkflowTypes"
    (("domain" :TYPE "String" :REQUIRED T)
     ("maximumPageSize" :TYPE "Number" :REQUIRED NIL)
     ("name" :TYPE "String" :REQUIRED NIL)
     ("nextPageToken" :TYPE "String" :REQUIRED NIL)
     ("registrationStatus" :TYPE "String" :REQUIRED T :VALID-VALUES
      ("REGISTERED" "DEPRECATED"))
     ("reverseOrder" :TYPE "Boolean" :REQUIRED NIL))
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)
  ("typeInfos" :TYPE (:ARRAY-OF "WorkflowTypeInfo") :REQUIRED T)) 

(SWF::DEFINE-SWF-ACTION "PollForActivityTask"
    (("domain" :TYPE "String" :REQUIRED T)
     ("identity" :TYPE "String" :REQUIRED NIL)
     ("taskList" :TYPE "TaskList" :REQUIRED T))
  ("activityId" :TYPE "String" :REQUIRED T)
  ("activityType" :TYPE "ActivityType" :REQUIRED T)
  ("input" :TYPE "String" :REQUIRED NIL)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("taskToken" :TYPE "String" :REQUIRED T)
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)) 

(SWF::DEFINE-SWF-ACTION "PollForDecisionTask"
    (("domain" :TYPE "String" :REQUIRED T)
     ("identity" :TYPE "String" :REQUIRED NIL)
     ("maximumPageSize" :TYPE "Number" :REQUIRED NIL)
     ("nextPageToken" :TYPE "String" :REQUIRED NIL)
     ("reverseOrder" :TYPE "Boolean" :REQUIRED NIL)
     ("taskList" :TYPE "TaskList" :REQUIRED T))
  ("events" :TYPE (:ARRAY-OF "HistoryEvent") :REQUIRED T)
  ("nextPageToken" :TYPE "String" :REQUIRED NIL)
  ("previousStartedEventId" :TYPE "Long" :REQUIRED NIL)
  ("startedEventId" :TYPE "Long" :REQUIRED T)
  ("taskToken" :TYPE "String" :REQUIRED T)
  ("workflowExecution" :TYPE "WorkflowExecution" :REQUIRED T)
  ("workflowType" :TYPE "WorkflowType" :REQUIRED T)) 

(SWF::DEFINE-SWF-ACTION "RecordActivityTaskHeartbeat"
    (("details" :TYPE "String" :REQUIRED NIL)
     ("taskToken" :TYPE "String" :REQUIRED T))
  ("cancelRequested" :TYPE "Boolean" :REQUIRED T)) 

(SWF::DEFINE-SWF-ACTION "RegisterActivityType"
    (("defaultTaskHeartbeatTimeout" :TYPE "String" :REQUIRED NIL)
     ("defaultTaskList" :TYPE "TaskList" :REQUIRED NIL)
     ("defaultTaskScheduleToCloseTimeout" :TYPE "String" :REQUIRED NIL)
     ("defaultTaskScheduleToStartTimeout" :TYPE "String" :REQUIRED NIL)
     ("defaultTaskStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
     ("description" :TYPE "String" :REQUIRED NIL)
     ("domain" :TYPE "String" :REQUIRED T) ("name" :TYPE "String" :REQUIRED T)
     ("version" :TYPE "String" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "RegisterDomain"
    (("description" :TYPE "String" :REQUIRED NIL)
     ("name" :TYPE "String" :REQUIRED T)
     ("workflowExecutionRetentionPeriodInDays" :TYPE "String" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "RegisterWorkflowType"
    (("defaultChildPolicy" :TYPE "String" :REQUIRED NIL :VALID-VALUES
      ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
     ("defaultExecutionStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
     ("defaultTaskList" :TYPE "TaskList" :REQUIRED NIL)
     ("defaultTaskStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
     ("description" :TYPE "String" :REQUIRED NIL)
     ("domain" :TYPE "String" :REQUIRED T) ("name" :TYPE "String" :REQUIRED T)
     ("version" :TYPE "String" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "RequestCancelWorkflowExecution"
    (("domain" :TYPE "String" :REQUIRED T)
     ("runId" :TYPE "String" :REQUIRED NIL)
     ("workflowId" :TYPE "String" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "RespondActivityTaskCanceled"
    (("details" :TYPE "String" :REQUIRED NIL)
     ("taskToken" :TYPE "String" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "RespondActivityTaskCompleted"
    (("result" :TYPE "String" :REQUIRED NIL)
     ("taskToken" :TYPE "String" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "RespondActivityTaskFailed"
    (("details" :TYPE "String" :REQUIRED NIL)
     ("reason" :TYPE "String" :REQUIRED NIL)
     ("taskToken" :TYPE "String" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "RespondDecisionTaskCompleted"
    (("decisions" :TYPE (:ARRAY-OF "Decision") :REQUIRED NIL)
     ("executionContext" :TYPE "String" :REQUIRED NIL)
     ("taskToken" :TYPE "String" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "SignalWorkflowExecution"
    (("domain" :TYPE "String" :REQUIRED T)
     ("input" :TYPE "String" :REQUIRED NIL)
     ("runId" :TYPE "String" :REQUIRED NIL)
     ("signalName" :TYPE "String" :REQUIRED T)
     ("workflowId" :TYPE "String" :REQUIRED T))) 

(SWF::DEFINE-SWF-ACTION "StartWorkflowExecution"
    (("childPolicy" :TYPE "String" :REQUIRED NIL :VALID-VALUES
      ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
     ("domain" :TYPE "String" :REQUIRED T)
     ("executionStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
     ("input" :TYPE "String" :REQUIRED NIL)
     ("tagList" :TYPE (:ARRAY-OF "Strings") :REQUIRED NIL)
     ("taskList" :TYPE "TaskList" :REQUIRED NIL)
     ("taskStartToCloseTimeout" :TYPE "String" :REQUIRED NIL)
     ("workflowId" :TYPE "String" :REQUIRED T)
     ("workflowType" :TYPE "WorkflowType" :REQUIRED T))
  ("runId" :TYPE "String" :REQUIRED NIL)) 

(SWF::DEFINE-SWF-ACTION "TerminateWorkflowExecution"
    (("childPolicy" :TYPE "String" :REQUIRED NIL :VALID-VALUES
      ("TERMINATE" "REQUEST_CANCEL" "ABANDON"))
     ("details" :TYPE "String" :REQUIRED NIL)
     ("domain" :TYPE "String" :REQUIRED T)
     ("reason" :TYPE "String" :REQUIRED NIL)
     ("runId" :TYPE "String" :REQUIRED NIL)
     ("workflowId" :TYPE "String" :REQUIRED T))) 
