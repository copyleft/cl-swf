(in-package :swf-workers)


(defvar *auto-carry-on* t)


(defmacro with-error-handling (&body body)
  `(handler-bind ((error #'report-and-handle-error))
     ,@body))


(defun report-and-handle-error (error)
  (let ((backtrace
	 (with-output-to-string (s) (ignore-errors
				      (let ((sb-ext:*debug-print-variable-alist*
					     (list (cons *print-length* 20) (cons *print-level* 6))))
					(sb-debug:backtrace 100 s))))))

    (let ((message *debug-io*))

      (flet ((heading (text)
               (format message "~%~%~A~%~A" text #.(make-string 100 :initial-element #\=))))

        (format message "~A~%	Error of type ~S." error (type-of error))

        (heading "Restarts")
        (loop with carry-on-idx
              with carry-on-restart = nil
              for i from 0
              for restart in (compute-restarts)
              when (and (null carry-on-restart)
                        (eq 'carry-on (restart-name restart)))
              do (setf carry-on-restart restart
                       carry-on-idx i)
              do (format message "~&~:[  ~;=>~]~2D: [~A] ~A~%"
                         (eql i carry-on-idx)
                         i (restart-name restart) restart)
              finally (when carry-on-restart
                        (format message "~%Will invoke restart ~D: [~S] ~A."
                                carry-on-idx (restart-name carry-on-restart)
                                carry-on-restart)))

                                        ;(heading "Backtrace")
                                        ;(terpri message)
                                        ;(princ backtrace message)
        ))

    ;; On productions server, simply carry-on
    (when *auto-carry-on*
      (invoke-restart 'carry-on))))
