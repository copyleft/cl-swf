(in-package #:swf-workers)


(defun make-uuid ()
  (with-open-file (in #p"/proc/sys/kernel/random/uuid")
    (read-line in)))


(defclass task-type ()
  ((name :initarg :name :reader task-type-name)
   (options :initarg :options :reader task-type-options)
   (function :initarg :function :reader task-type-function)))


(defun serialize-task-type (task-type)
  (with-slots (options) task-type
    (alist :name (getf options :name)
           :version (getf options :version))))


(defclass workflow-type (task-type) ())
(defclass activity-type (task-type) ())

(defgeneric ensure-task-type (task-type))

(defmethod ensure-task-type ((workflow-type workflow-type))
  (handler-case
      (apply #'swf::register-workflow-type (task-type-options workflow-type))
    (swf::type-already-exists-error ()
      ;; TODO check if options are equal
      )))

(defmethod ensure-task-type ((activity-type activity-type))
  (handler-case
      (apply #'swf::register-activity-type (task-type-options activity-type))
    (swf::type-already-exists-error ()
      ;; TODO check if options are equal
      )))


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


(defun serialize-keyword (keyword)
  (check-type keyword keyword)
  (symbol-name keyword))


(defun deserialize-keyword (string)
  (intern string :keyword))


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
