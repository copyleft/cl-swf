(in-package #:swf-workers)


(defun make-uuid ()
  (with-open-file (in #p"/proc/sys/kernel/random/uuid")
    (read-line in)))


(defclass task-type ()
  ((name :initarg :name :reader task-type-name)
   (options :initarg :options :reader task-type-options)
   (function :initarg :function :reader task-type-function)
   (timeout :initarg :timeout :reader task-type-timeout)))


(defmethod print-object ((task-type task-type) stream)
  (print-unreadable-object (task-type stream :type t)
    (prin1 (task-type-name task-type) stream)))


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
;;; Serialization is limited to numbers, strings, T, NIL, symbols, conses / lists
;;; and local-time:timestamp
;;;

(defpackage #:%swf-serialization
  (:import-from :common-lisp #:t #:nil))


(defvar *swf-serialization-readtable*
  (let ((*readtable* (copy-readtable nil)))
    (local-time:enable-read-macros)
    *readtable*))


(defun serialize-to-stream (object stream)
  (etypecase object
    ((or number string keyword local-time:timestamp
         (member t nil))
     (prin1 object stream))
    (symbol
     (princ (package-name (symbol-package object)) stream)
     (princ "::" stream)
     (princ (symbol-name object) stream))
    (cons
     (princ "(" stream)
     (loop for c on object do
           (serialize-to-stream (car c) stream)
           (when (cdr c)
             (princ #\Space stream))
           (when (and (cdr c)
                      (not (consp (cdr c))))
             (princ ". " stream)
             (serialize-to-stream (cdr c) stream)))
     (princ ")" stream))))


(defun serialize-object (object)
  (when object
    (with-standard-io-syntax
      (let ((*package* (find-package :%swf-serialization))
            (*readtable* *swf-serialization-readtable*))
        (with-output-to-string (out)
          (serialize-to-stream object out))))))


(defun deserialize-object (string)
  (when string
    (with-standard-io-syntax
      (let ((*package* (find-package :%swf-serialization))
            (*readtable* *swf-serialization-readtable*))
        (read-from-string string)))))


(defun serialize-id (id)
  "An id can eiter be a keyword, string or an integer. The name of the
keyword or the string must not contain a : (colon), / (slash),
| (vertical bar), or any control characters (\u0000-\u001f | \u007f -
\u009f). Also, it must not contain the literal string 'arn'."
  (if (stringp id)
      (serialize-object id)
      (princ-to-string id)))


(defun deserialize-id (string)
  (when (plusp (length string))
    (if (char= #\" (char string 0))
        (deserialize-object string)
        (or (ignore-errors (parse-integer string))
            (intern string :keyword)))))


(defun serialize-slot (slot value)
  (case slot
    ((:timer-id
      :marker-name
      :activity-id
      :workflow-id)
     (serialize-id value))
    ((:details
      :result
      :input
      :reason
      :control
      :execution-context)
     (serialize-object value))
    ((:activity-type
      :workflow-type)
     (serialize-task-type value))
    (otherwise value)))


(defun deserialize-slot (slot value)
  (case slot
    ((:timer-id
      :marker-name
      :activity-id
      :workflow-id)
     (deserialize-id value))
    ((:details
      :result
      :input
      :reason
      :control
      :execution-context)
     (deserialize-object value))
    ((:workflow-execution)
     (alist :run-id (aget value :run-id)
            :workflow-id (deserialize-id (aget value :workflow-id))))
    (:activity-type
     (find-activity-type value))
    (:workflow-type
     (find-workflow-type value))
    (otherwise value)))


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
