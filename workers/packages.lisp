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
   #:on
   #:task
   #:start-child-workflow
   #:schedule-activity
   #:reschedule-activity
   #:activity-result
   #:continue-as-new
   ))


(in-package #:swf-workers)
(define-log-macros swf-workers)
