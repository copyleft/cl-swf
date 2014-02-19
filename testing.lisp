(in-package #:swf-workers)


(define-activity say-hello (what)
    ()
  (print what))


(define-workflow hello (greeting)
    (:version :4 :default-child-policy :terminate)
  (if (< 10 (count-open-tasks :timer))
      (list (alist :decision-type :cancel-workflow-execution))
      (list (alist :decision-type :record-marker
                   :record-marker-decision-attributes (alist :marker-name "test1"))
            (alist :decision-type :start-timer
                   :start-timer-decision-attributes (alist :start-to-fire-timeout 1000
                                                           :timer-id (make-uuid)))
            (say-hello :what "hei"))))


(swf::with-service ()
  (hello :greeting "hei"
         :execution-start-to-close-timeout 600
         :task-start-to-close-timeout 30))


(swf::with-service ()
  (ensure-workflow-types *package*)
  (ensure-activity-types *package*))


(defparameter *wfw* (make-instance 'workflow-worker))
(defparameter *aw* (make-instance 'activity-worker))

(worker-start-thread *wfw*)
(worker-start-thread *aw*)


;; -----------------------------------------------------------------------------

(setf *auto-carry-on* nil)


(worker-look-for-task *wfw*)
(worker-compute-task-response *wfw* *wtask*)
(worker-handle-task *wfw* *wtask*)
(worker-handle-next-task *wfw*)

(worker-look-for-task *aw*)
(worker-compute-task-response *aw* *atask*)
(worker-handle-task *aw* *atask*)
(worker-handle-next-task *aw*)


(swf::with-service ()
  (swf::signal-workflow-execution :workflow-id "2k0"
                                  :signal-name "13"))

(swf::with-service ()
  (swf::request-cancel-workflow-execution :workflow-id "110c"))


(swf::with-service ()
  (swf::list-open-workflow-executions :all-pages t
                                      :execution-filter (alist :workflow-id "1d7o")
                                      :start-time-filter (alist :oldest-date (local-time:parse-timestring "2000-01-01"))))


(swf::with-service ()
  (swf::list-domains :registration-status :registered))

(swf::with-service ()
  (swf::list-workflow-types :registration-status :registered))

(swf::with-service ()
  (swf::list-workflow-types :registration-status :registered))

(swf::swf-to-json 'swf::decision (say-hello))

(swf::swf-to-json 'swf::poll-for-decision-task-response *wtask*)

(swf::json-to-swf 'swf::register-workflow-type-request
                  (swf::swf-to-json 'swf::register-workflow-type-request
                                    '((:domain . "hei")
                                      (:name . "name")
                                      (:version . "version")
                                      (:default-task-list . "foo")
                                      (:default-task-list . ((:name . "foo"))))))

(make-workflow-execution-info (aget *wtask* :events))


(local-time:enable-read-macros)
(defparameter *wtask*
  '((:EVENTS
     ((:EVENT-ID . 1) (:EVENT-TIMESTAMP . @2014-02-18T19:20:16.859000+01:00)
      (:EVENT-TYPE . :WORKFLOW-EXECUTION-STARTED)
      (:WORKFLOW-EXECUTION-STARTED-EVENT-ATTRIBUTES
       (:CHILD-POLICY . :TERMINATE)
       (:EXECUTION-START-TO-CLOSE-TIMEOUT :DAYS 1 :HOURS 0 :MINUTES 0 :SECONDS
                                          0)
       (:INPUT . "(:GREETING \"hei\")") (:PARENT-INITIATED-EVENT-ID . 0)
       (:TASK-LIST (:NAME . "default"))
       (:TASK-START-TO-CLOSE-TIMEOUT :DAYS 1 :HOURS 0 :MINUTES 0 :SECONDS 0)
       (:WORKFLOW-TYPE (:NAME . "HELLO") (:VERSION . "x1"))))
     ((:DECISION-TASK-SCHEDULED-EVENT-ATTRIBUTES
       (:START-TO-CLOSE-TIMEOUT :DAYS 1 :HOURS 0 :MINUTES 0 :SECONDS 0)
       (:TASK-LIST (:NAME . "default")))
      (:EVENT-ID . 2) (:EVENT-TIMESTAMP . @2014-02-18T19:20:16.859000+01:00)
      (:EVENT-TYPE . :DECISION-TASK-SCHEDULED))
     ((:DECISION-TASK-STARTED-EVENT-ATTRIBUTES
       (:IDENTITY . "#<THREAD \"worker\" RUNNING {100AE44283}>")
       (:SCHEDULED-EVENT-ID . 2))
      (:EVENT-ID . 3) (:EVENT-TIMESTAMP . @2014-02-19T13:05:46.001000+01:00)
      (:EVENT-TYPE . :DECISION-TASK-STARTED)))
    (:PREVIOUS-STARTED-EVENT-ID . 0) (:STARTED-EVENT-ID . 3)
    (:TASK-TOKEN
     . "AAAAKgAAAAEAAAAAAAAAA8NDmaPXj043oiseFwslkV4XiAJgbu86JQo6vFglSVvhZmwy1svYilizF3iyIX7/3RulpJZrK5A9HNncMQMYMeBHGHWI/DdS5nJz8peUoFDHPsMbP/24FNMb1kBgEGgTARVGHGKXyiLHKKYAzjHQDPX1SZt8AXHdaLPDYP5AH5tl5N2hqwizuOH2vXKkSQKJYk3dOt5P7oNCyzTUNWh3DZhyTK3saYoi5E4auZn7hF9ccSGMDjHViCUgrdElmjNChBjDt1BF3ZfU0bAmM5qMkko=")
    (:WORKFLOW-EXECUTION
     (:RUN-ID . "12ChrohesRJisovBMwZZFCMtVCk5toQkH9hoZGai+zil8=")
     (:WORKFLOW-ID . "1d7o"))
    (:WORKFLOW-TYPE (:NAME . "HELLO") (:VERSION . "1"))))

(defparameter *wtask*
  '((:EVENTS
     ((:EVENT-ID . 1) (:EVENT-TIMESTAMP . @2014-02-18T19:20:16.859000+01:00)
      (:EVENT-TYPE . :WORKFLOW-EXECUTION-STARTED)
      (:WORKFLOW-EXECUTION-STARTED-EVENT-ATTRIBUTES
       (:CHILD-POLICY . :TERMINATE)
       (:EXECUTION-START-TO-CLOSE-TIMEOUT :DAYS 1 :HOURS 0 :MINUTES 0 :SECONDS
                                          0)
       (:INPUT . "(:GREETING \"hei\")") (:PARENT-INITIATED-EVENT-ID . 0)
       (:TASK-LIST (:NAME . "default"))
       (:TASK-START-TO-CLOSE-TIMEOUT :DAYS 1 :HOURS 0 :MINUTES 0 :SECONDS 0)
       (:WORKFLOW-TYPE (:NAME . "HELLO") (:VERSION . "1"))))
     ((:DECISION-TASK-SCHEDULED-EVENT-ATTRIBUTES
       (:START-TO-CLOSE-TIMEOUT :DAYS 1 :HOURS 0 :MINUTES 0 :SECONDS 0)
       (:TASK-LIST (:NAME . "default")))
      (:EVENT-ID . 2) (:EVENT-TIMESTAMP . @2014-02-18T19:20:16.859000+01:00)
      (:EVENT-TYPE . :DECISION-TASK-SCHEDULED))
     ((:DECISION-TASK-STARTED-EVENT-ATTRIBUTES
       (:IDENTITY . "#<THREAD \"worker\" RUNNING {100AE44283}>")
       (:SCHEDULED-EVENT-ID . 2))
      (:EVENT-ID . 3) (:EVENT-TIMESTAMP . @2014-02-19T13:05:46.001000+01:00)
      (:EVENT-TYPE . :DECISION-TASK-STARTED))
     ((:DECISION-TASK-COMPLETED-EVENT-ATTRIBUTES (:SCHEDULED-EVENT-ID . 2)
                                                 (:STARTED-EVENT-ID . 3))
      (:EVENT-ID . 4) (:EVENT-TIMESTAMP . @2014-02-19T13:25:30.103000+01:00)
      (:EVENT-TYPE . :DECISION-TASK-COMPLETED))
     ((:EVENT-ID . 5) (:EVENT-TIMESTAMP . @2014-02-19T13:25:30.103000+01:00)
      (:EVENT-TYPE . :MARKER-RECORDED)
      (:MARKER-RECORDED-EVENT-ATTRIBUTES
       (:DECISION-TASK-COMPLETED-EVENT-ID . 4) (:MARKER-NAME . "test1")))
     ((:EVENT-ID . 6) (:EVENT-TIMESTAMP . @2014-02-19T13:27:58.204000+01:00)
      (:EVENT-TYPE . :WORKFLOW-EXECUTION-SIGNALED)
      (:WORKFLOW-EXECUTION-SIGNALED-EVENT-ATTRIBUTES
       (:EXTERNAL-INITIATED-EVENT-ID . 0) (:SIGNAL-NAME . "13")))
     ((:DECISION-TASK-SCHEDULED-EVENT-ATTRIBUTES
       (:START-TO-CLOSE-TIMEOUT :DAYS 1 :HOURS 0 :MINUTES 0 :SECONDS 0)
       (:TASK-LIST (:NAME . "default")))
      (:EVENT-ID . 7) (:EVENT-TIMESTAMP . @2014-02-19T13:27:58.204000+01:00)
      (:EVENT-TYPE . :DECISION-TASK-SCHEDULED))
     ((:DECISION-TASK-STARTED-EVENT-ATTRIBUTES
       (:IDENTITY . "#<THREAD \"worker\" RUNNING {1006450C93}>")
       (:SCHEDULED-EVENT-ID . 7))
      (:EVENT-ID . 8) (:EVENT-TIMESTAMP . @2014-02-19T13:29:30.878000+01:00)
      (:EVENT-TYPE . :DECISION-TASK-STARTED))
     ((:DECISION-TASK-COMPLETED-EVENT-ATTRIBUTES (:SCHEDULED-EVENT-ID . 7)
                                                 (:STARTED-EVENT-ID . 8))
      (:EVENT-ID . 9) (:EVENT-TIMESTAMP . @2014-02-19T13:29:31.130000+01:00)
      (:EVENT-TYPE . :DECISION-TASK-COMPLETED))
     ((:EVENT-ID . 10) (:EVENT-TIMESTAMP . @2014-02-19T13:29:31.130000+01:00)
      (:EVENT-TYPE . :MARKER-RECORDED)
      (:MARKER-RECORDED-EVENT-ATTRIBUTES
       (:DECISION-TASK-COMPLETED-EVENT-ID . 9) (:MARKER-NAME . "test1")))
     ((:EVENT-ID . 11) (:EVENT-TIMESTAMP . @2014-02-19T13:30:09.144000+01:00)
      (:EVENT-TYPE . :WORKFLOW-EXECUTION-SIGNALED)
      (:WORKFLOW-EXECUTION-SIGNALED-EVENT-ATTRIBUTES
       (:EXTERNAL-INITIATED-EVENT-ID . 0) (:SIGNAL-NAME . "13")))
     ((:DECISION-TASK-SCHEDULED-EVENT-ATTRIBUTES
       (:START-TO-CLOSE-TIMEOUT :DAYS 1 :HOURS 0 :MINUTES 0 :SECONDS 0)
       (:TASK-LIST (:NAME . "default")))
      (:EVENT-ID . 12) (:EVENT-TIMESTAMP . @2014-02-19T13:30:09.144000+01:00)
      (:EVENT-TYPE . :DECISION-TASK-SCHEDULED))
     ((:EVENT-ID . 13) (:EVENT-TIMESTAMP . @2014-02-19T13:30:21.864000+01:00)
      (:EVENT-TYPE . :WORKFLOW-EXECUTION-SIGNALED)
      (:WORKFLOW-EXECUTION-SIGNALED-EVENT-ATTRIBUTES
       (:EXTERNAL-INITIATED-EVENT-ID . 0) (:SIGNAL-NAME . "13")))
     ((:EVENT-ID . 14) (:EVENT-TIMESTAMP . @2014-02-19T13:31:05.389000+01:00)
      (:EVENT-TYPE . :WORKFLOW-EXECUTION-CANCEL-REQUESTED)
      (:WORKFLOW-EXECUTION-CANCEL-REQUESTED-EVENT-ATTRIBUTES
       (:EXTERNAL-INITIATED-EVENT-ID . 0)))
     ((:DECISION-TASK-STARTED-EVENT-ATTRIBUTES
       (:IDENTITY . "#<THREAD \"worker\" RUNNING {1006D8E5B3}>")
       (:SCHEDULED-EVENT-ID . 12))
      (:EVENT-ID . 15) (:EVENT-TIMESTAMP . @2014-02-19T13:31:23.116000+01:00)
      (:EVENT-TYPE . :DECISION-TASK-STARTED)))
    (:PREVIOUS-STARTED-EVENT-ID . 8) (:STARTED-EVENT-ID . 15)
    (:TASK-TOKEN
     . "AAAAKgAAAAEAAAAAAAAAA7tjCIoRiviRcFFKa2CJc1bNJn2gl72nknoPpJGV5ep3hIXbe9NoLVTwVF1HYoBEKym+ySa5880R91WFwtYM0sCRPBKOubXD8HSl2yPnfyPCGY9Qi4yd2NWLGHt1SCy3DR8eTEdPFvnQMjTMVk1NIBmjV+qqIqdxkiWh3XrcT3H11ouTrNnu+SNaprkWUJFogfC4ix8R8rDnqq+19dvZnh1mA7XW2iJouSY4bEsx6qTWHAxlts93H7JonG8/HmmKqjjKYPPit5nqEc+rPSrDnA4=")
    (:WORKFLOW-EXECUTION
     (:RUN-ID . "12ChrohesRJisovBMwZZFCMtVCk5toQkH9hoZGai+zil8=")
     (:WORKFLOW-ID . "1d7o"))
    (:WORKFLOW-TYPE (:NAME . "HELLO") (:VERSION . "1"))))


(defparameter *atask*
  '((:ACTIVITY-ID . "9571")
    (:ACTIVITY-TYPE (:NAME . "SAY-HELLO") (:VERSION . "1"))
    (:INPUT . "(:WHAT \"hei\")") (:STARTED-EVENT-ID . 7)
    (:TASK-TOKEN
     . "AAAAKgAAAAEAAAAAAAAAA1BeAmLVKD7Ad9iRXTg6eKnE+INLFq24VUBlbnj64eVH40FJ9+bCiN67b6zlefvkRLAy8SCwei9+3d5o3/DUfyBacFe/6n9XGsDyQFl4RoiTFNLExSAZVbAsc5OVeltf1RQC4RJbM6ZNM2gYjPNlj94OM/3sHw5MtqVpDEevsikQWsc1MoMKqMthxFRmbZsRmhtCCSIQNdBC8Y2aoOpuvLO/feyPjDP9QW/wEcrUmT7/9QPtwse3HKXNZfY90s2uborFXFWMcyE+Q1m66ahAWAQ=")
    (:WORKFLOW-EXECUTION
     (:RUN-ID . "12pPfc/fIIJswf45L1H7nzN+8fLMBcLZ3Ef67zjD40kJ0=")
     (:WORKFLOW-ID . "tg1"))))
