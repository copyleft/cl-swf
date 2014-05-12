(defpackage #:swf-workers
  (:nicknames #:swfw)
  (:use #:common-lisp
        #:swf-logging)
  (:export
   #:define-activity
   #:define-workflow
   #:context
   #:start-timer
   #:complete-workflow
   #:cancel-workflow
   #:fail-workflow
   ;; Test for task types
   #:marker?
   #:signal?
   #:timer?
   #:workflow?
   #:decision?
   #:activity?
   #:child-workflow?
   ;; Test for event types
   #:recorded?
   #:failed?
   #:signaled?
   #:started?
   #:fired?
   #:canceled?
   #:cancel-failed?
   #:completed?
   #:timed-out?
   #:terminated?
   #:continued-as-new?
   #:continue-as-new-failed?
   #:cancel-requested?
   #:scheduled?
   #:schedule-failed?
   #:request-cancel-failed?
   #:initiated?
   #:start-failed?))


(in-package #:swf-workers)
(define-log-macros swf-workers)
