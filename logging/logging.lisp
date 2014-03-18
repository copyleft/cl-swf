(in-package #:swf-logging)


(defvar *context* nil)


(defmacro with-log-context (context &body body)
  `(let ((*context* (cons ,context *context*)))
     ,@body))


(defun log-context ()
  (reverse *context*))


(defmacro define-log-macros (category)
  `(progn
     (log5:defcategory ,category)
     ,@(loop for level in '(log5:error log5:warn log5:info log5:trace)
             collect
             `(defmacro ,(intern (format nil "LOG-~A" level)) (control &rest args)
                `(log5:log-for (and ,',category ,',level) "~S ~@?" (log-context) ,control ,@args)))))
