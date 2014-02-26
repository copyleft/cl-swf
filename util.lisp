(in-package #:swf-workers)


(defun make-uuid ()
  (with-open-file (in #p"/proc/sys/kernel/random/uuid")
    (read-line in)))


;;; Serialization ---------------------------------------------------------------------------------
;;;
;;; Serialization is limited to numbers, strings, T, NIL, keyword symbols, conses / lists
;;; and local-time:timestamp.
;;;

(defpackage #:%swf-serialization
  (:import-from :common-lisp #:t #:nil))


(defvar *swf-serialization-readtable*
  (let ((*readtable* (copy-readtable nil)))
    (local-time:enable-read-macros)
    *readtable*))


(defun serializable-p (object &optional (cons-cache (make-hash-table)))
  (typecase object
    (cons
     (prog1 (and (not (gethash object cons-cache))
                 (serializable-p (car object) cons-cache)
                 (serializable-p (cdr object) cons-cache))
       (setf (gethash object cons-cache) t)))
    (real t)
    (keyword t)
    (string t)
    ((member t nil) t)
    (local-time:timestamp t)))


(defun serialize-object (object)
  (when object
    (assert (serializable-p object) () "~S is not serializable" object)
    (with-standard-io-syntax
      (let ((*package* (find-package :%swf-serialization))
            (*readtable* *swf-serialization-readtable*))
        (prin1-to-string object)))))


(defun deserialize-object (string)
  (when string
    (with-standard-io-syntax
      (let ((*package* (find-package :%swf-serialization))
            (*readtable* *swf-serialization-readtable*))
        (read-from-string string)))))


;;; Helpers for dealing with assoc objects

(defun aget (alist key &rest keys)
  (let ((value (cdr (assoc key alist))))
    (if keys
        (apply #'aget value (car keys) (cdr keys))
        value)))


(defun agetter (&rest keys)
  (lambda (alist)
    (apply #'aget alist keys)))


(defun alist (&rest data)
  (loop for (key value) on data by #'cddr
        collect (cons key value)))
