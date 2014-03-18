(defpackage #:swf
  (:use #:common-lisp
        #:swf-logging
        #:aws-sign4
        #:json-streams
        #:drakma)
  (:export))

(in-package #:swf)
(define-log-macros swf)
