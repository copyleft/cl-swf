(in-package #:swf-workers)


(defun make-uuid ()
  (with-open-file (in #p"/proc/sys/kernel/random/uuid")
    (read-line in)))


;;; Serialization ---------------------------------------------------------------------------------


(defpackage #:%amazon-flow-serialization)


(defun serialize-object (object)
  (when object
    (with-standard-io-syntax
      (let ((*package* (find-package :%amazon-flow-serialization))
            (*print-circle* t))
        (prin1-to-string object)))))


(defun deserialize-object (string)
  (when string
    (with-standard-io-syntax
      (let ((*package* (find-package :%amazon-flow-serialization)))
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
