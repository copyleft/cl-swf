(in-package :swf-workers)


(defvar *enable-debugging* nil)


(defun report-error (error)
  (ignore-errors
    (let ((backtrace
           (with-output-to-string (s) (let ((sb-ext:*debug-print-variable-alist*
                                             (list (cons '*print-length* 20) (cons '*print-level* 6))))
                                        (sb-debug:backtrace 50 s)))))

      (log-error "~A"
                 (with-output-to-string (message)
                   (format message "~A~%	Error of type ~S." error (type-of error))
                   (terpri message)
                   (terpri message)
                   (princ backtrace message))))))
