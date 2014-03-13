(in-package :swf-workers)


(defvar *enable-debugging* nil)


(defun report-error (error)
  (let ((backtrace
	 (with-output-to-string (s) (ignore-errors
				      (let ((sb-ext:*debug-print-variable-alist*
					     (list (cons *print-length* 20) (cons *print-level* 6))))
					(sb-debug:backtrace 50 s))))))

    (log-error "~A"
               (with-output-to-string (message)
                 (flet ((heading (text)
                          (format message "~%~%~A~%~A" text #.(make-string 100 :initial-element #\=))))
                   (format message "~A~%	Error of type ~S." error (type-of error))
                   (heading "Backtrace")
                   (terpri message)
                   (princ backtrace message))))))
