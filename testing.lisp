(in-package #:swf-workers)

(define-workflow hello (greeting)
    ()
  (if (find-if (lambda (event)
                 (eq 'swf::workflow-execution-cancel-requested (aget event :event-type)))
               (aget task :events))
      (list (alist :decision-type 'swf::cancel-workflow-execution))
      (list (alist :decision-type 'swf::record-marker
                   :record-marker-decision-attributes (alist :marker-name "test1")))))


(swf::with-service ()
  (hello :greeting "hei"
         :task-start-to-close-timeout '(:minutes 1)))



(swf::with-service ()
  (defparameter *wfw* (make-instance 'workflow-worker)))

(worker-look-for-task *wfw*)
(worker-compute-task-response *wfw* *wtask*)
(worker-handle-task *wfw* *wtask*)

(worker-handle-next-task *wfw*)


(swf::with-service ()
  (swf::signal-workflow-execution :workflow-id "2k0"
                                  :signal-name "13"))

(swf::with-service ()
  (swf::request-cancel-workflow-execution :workflow-id "2k0"))


(swf::with-service ()
  (swf::list-open-workflow-executions :all-pages t
                                      :execution-filter (alist :workflow-id "1d7o")
                                      :start-time-filter (alist :oldest-date (local-time:parse-timestring "2000-01-01"))))

(local-time:enable-read-macros)

(defparameter *wtask*
  '((:EVENTS
     ((:EVENT-ID . 1) (:EVENT-TIMESTAMP . @2014-02-18T19:20:16.859000+01:00)
      (:EVENT-TYPE . SWF::WORKFLOW-EXECUTION-STARTED)
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
      (:EVENT-TYPE . SWF::DECISION-TASK-SCHEDULED))
     ((:DECISION-TASK-STARTED-EVENT-ATTRIBUTES
       (:IDENTITY . "#<THREAD \"worker\" RUNNING {100AE44283}>")
       (:SCHEDULED-EVENT-ID . 2))
      (:EVENT-ID . 3) (:EVENT-TIMESTAMP . @2014-02-19T13:05:46.001000+01:00)
      (:EVENT-TYPE . SWF::DECISION-TASK-STARTED)))
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
      (:EVENT-TYPE . SWF::WORKFLOW-EXECUTION-STARTED)
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
      (:EVENT-TYPE . SWF::DECISION-TASK-SCHEDULED))
     ((:DECISION-TASK-STARTED-EVENT-ATTRIBUTES
       (:IDENTITY . "#<THREAD \"worker\" RUNNING {100AE44283}>")
       (:SCHEDULED-EVENT-ID . 2))
      (:EVENT-ID . 3) (:EVENT-TIMESTAMP . @2014-02-19T13:05:46.001000+01:00)
      (:EVENT-TYPE . SWF::DECISION-TASK-STARTED))
     ((:DECISION-TASK-COMPLETED-EVENT-ATTRIBUTES (:SCHEDULED-EVENT-ID . 2)
                                                 (:STARTED-EVENT-ID . 3))
      (:EVENT-ID . 4) (:EVENT-TIMESTAMP . @2014-02-19T13:25:30.103000+01:00)
      (:EVENT-TYPE . SWF::DECISION-TASK-COMPLETED))
     ((:EVENT-ID . 5) (:EVENT-TIMESTAMP . @2014-02-19T13:25:30.103000+01:00)
      (:EVENT-TYPE . SWF::MARKER-RECORDED)
      (:MARKER-RECORDED-EVENT-ATTRIBUTES
       (:DECISION-TASK-COMPLETED-EVENT-ID . 4) (:MARKER-NAME . "test1")))
     ((:EVENT-ID . 6) (:EVENT-TIMESTAMP . @2014-02-19T13:27:58.204000+01:00)
      (:EVENT-TYPE . SWF::WORKFLOW-EXECUTION-SIGNALED)
      (:WORKFLOW-EXECUTION-SIGNALED-EVENT-ATTRIBUTES
       (:EXTERNAL-INITIATED-EVENT-ID . 0) (:SIGNAL-NAME . "13")))
     ((:DECISION-TASK-SCHEDULED-EVENT-ATTRIBUTES
       (:START-TO-CLOSE-TIMEOUT :DAYS 1 :HOURS 0 :MINUTES 0 :SECONDS 0)
       (:TASK-LIST (:NAME . "default")))
      (:EVENT-ID . 7) (:EVENT-TIMESTAMP . @2014-02-19T13:27:58.204000+01:00)
      (:EVENT-TYPE . SWF::DECISION-TASK-SCHEDULED))
     ((:DECISION-TASK-STARTED-EVENT-ATTRIBUTES
       (:IDENTITY . "#<THREAD \"worker\" RUNNING {1006450C93}>")
       (:SCHEDULED-EVENT-ID . 7))
      (:EVENT-ID . 8) (:EVENT-TIMESTAMP . @2014-02-19T13:29:30.878000+01:00)
      (:EVENT-TYPE . SWF::DECISION-TASK-STARTED))
     ((:DECISION-TASK-COMPLETED-EVENT-ATTRIBUTES (:SCHEDULED-EVENT-ID . 7)
                                                 (:STARTED-EVENT-ID . 8))
      (:EVENT-ID . 9) (:EVENT-TIMESTAMP . @2014-02-19T13:29:31.130000+01:00)
      (:EVENT-TYPE . SWF::DECISION-TASK-COMPLETED))
     ((:EVENT-ID . 10) (:EVENT-TIMESTAMP . @2014-02-19T13:29:31.130000+01:00)
      (:EVENT-TYPE . SWF::MARKER-RECORDED)
      (:MARKER-RECORDED-EVENT-ATTRIBUTES
       (:DECISION-TASK-COMPLETED-EVENT-ID . 9) (:MARKER-NAME . "test1")))
     ((:EVENT-ID . 11) (:EVENT-TIMESTAMP . @2014-02-19T13:30:09.144000+01:00)
      (:EVENT-TYPE . SWF::WORKFLOW-EXECUTION-SIGNALED)
      (:WORKFLOW-EXECUTION-SIGNALED-EVENT-ATTRIBUTES
       (:EXTERNAL-INITIATED-EVENT-ID . 0) (:SIGNAL-NAME . "13")))
     ((:DECISION-TASK-SCHEDULED-EVENT-ATTRIBUTES
       (:START-TO-CLOSE-TIMEOUT :DAYS 1 :HOURS 0 :MINUTES 0 :SECONDS 0)
       (:TASK-LIST (:NAME . "default")))
      (:EVENT-ID . 12) (:EVENT-TIMESTAMP . @2014-02-19T13:30:09.144000+01:00)
      (:EVENT-TYPE . SWF::DECISION-TASK-SCHEDULED))
     ((:EVENT-ID . 13) (:EVENT-TIMESTAMP . @2014-02-19T13:30:21.864000+01:00)
      (:EVENT-TYPE . SWF::WORKFLOW-EXECUTION-SIGNALED)
      (:WORKFLOW-EXECUTION-SIGNALED-EVENT-ATTRIBUTES
       (:EXTERNAL-INITIATED-EVENT-ID . 0) (:SIGNAL-NAME . "13")))
     ((:EVENT-ID . 14) (:EVENT-TIMESTAMP . @2014-02-19T13:31:05.389000+01:00)
      (:EVENT-TYPE . SWF::WORKFLOW-EXECUTION-CANCEL-REQUESTED)
      (:WORKFLOW-EXECUTION-CANCEL-REQUESTED-EVENT-ATTRIBUTES
       (:EXTERNAL-INITIATED-EVENT-ID . 0)))
     ((:DECISION-TASK-STARTED-EVENT-ATTRIBUTES
       (:IDENTITY . "#<THREAD \"worker\" RUNNING {1006D8E5B3}>")
       (:SCHEDULED-EVENT-ID . 12))
      (:EVENT-ID . 15) (:EVENT-TIMESTAMP . @2014-02-19T13:31:23.116000+01:00)
      (:EVENT-TYPE . SWF::DECISION-TASK-STARTED)))
    (:PREVIOUS-STARTED-EVENT-ID . 8) (:STARTED-EVENT-ID . 15)
    (:TASK-TOKEN
     . "AAAAKgAAAAEAAAAAAAAAA7tjCIoRiviRcFFKa2CJc1bNJn2gl72nknoPpJGV5ep3hIXbe9NoLVTwVF1HYoBEKym+ySa5880R91WFwtYM0sCRPBKOubXD8HSl2yPnfyPCGY9Qi4yd2NWLGHt1SCy3DR8eTEdPFvnQMjTMVk1NIBmjV+qqIqdxkiWh3XrcT3H11ouTrNnu+SNaprkWUJFogfC4ix8R8rDnqq+19dvZnh1mA7XW2iJouSY4bEsx6qTWHAxlts93H7JonG8/HmmKqjjKYPPit5nqEc+rPSrDnA4=")
    (:WORKFLOW-EXECUTION
     (:RUN-ID . "12ChrohesRJisovBMwZZFCMtVCk5toQkH9hoZGai+zil8=")
     (:WORKFLOW-ID . "1d7o"))
    (:WORKFLOW-TYPE (:NAME . "HELLO") (:VERSION . "1"))))
