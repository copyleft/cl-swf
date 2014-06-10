(defpackage #:workers-test
  (:use #:common-lisp
        #:swf-workers))

(in-package #:workers-test)


(declaim (optimize (speed 0) (space 0) (debug 3)))


;;(setf swf-workers::*enable-debugging* t)
;;(setf swf-workers::*enable-debugging* nil)


(log5:defoutput iso-timestamp (local-time:format-rfc3339-timestring nil (local-time:now)))

(defun start-logging ()
  (defvar *log-sender*
    (let ((out (open "/tmp/swf.log" :direction :output :if-does-not-exist :create :if-exists :append)))
      (log5:start-sender 'trace (log5:stream-sender :location out)
                         :category-spec '(log5:dribble+)
                         :output-spec '(iso-timestamp
                                        log5:category
                                        log5:message)))))
;(start-logging)



(defun make-worker (&optional (task-list "default"))
  (make-instance 'swf-workers::worker
                 :packages (list (find-package :workers-test))
                 :task-list task-list))


(defun start-worker (type &optional (task-list "default"))
  (swfw::worker-start-thread (make-worker task-list) type))


(defun start-workers ()
  (start-worker :workflow)
  (start-worker :activity))
;; (start-workers)


(defun ensure-test-task-types ()
  (swf::with-service ()
    (swfw::ensure-task-types :workers-test)))
;; (ensure-test-task-types)


;;;

(define-activity add (a b)
    ((:version :1))
  (+ a b))

(define-workflow test (&key hei)
    ((:version :2)
     (:default-execution-start-to-close-timeout (* 60 5))
     (:default-task-start-to-close-timeout 60)
     (:default-child-policy :terminate))
  (on :started
      (adding-child 7 4))
  (on :start-timer-failed
      (swfw::fail-workflow-execution-decision))
  (task adding-child (a b)
      (start-child-workflow (adding-child a b))
    (on :completed
        (wait 5 :b (swfw::activity-result)))
    (on (:timed-out :canceled :terminated :failed)
        (swfw::fail-workflow-execution-decision :reason :spite)))
  (task wait (a &key b)
      (start-timer a)
    (on :fired
        (swfw::complete-workflow-execution-decision :result (list a b hei)))))


(define-workflow adding-child (a b)
    ((:version :2)
     (:default-execution-start-to-close-timeout (* 60 5))
     (:default-task-start-to-close-timeout 60)
     (:default-child-policy :terminate))
  (on :started
      (add-it a b))
  (task add-it (a b)
      (schedule-activity (add a b))
    (on :completed
        (swfw::complete-workflow-execution-decision :result (swfw::activity-result)))
    (on (:timed-out :canceled :failed)
        (swfw::fail-workflow-execution-decision :reason :spite))))


;;(swf::with-service () (swfw::start-workflow (test :hei "du")))
