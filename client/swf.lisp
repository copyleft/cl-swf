(in-package :swf)


(defun http-post* (uri payload headers)
  "Makes a http post request. Should wait at least 70 seconds for a
response. Payload is a string, headers is assoc list where the keys
are string designators and the values are string. Returns three
values, the result as a string, the http status code as an integer,
and the reponse headers as an assoc list."
  (multiple-value-bind (result-octets status-code headers)
      (drakma:http-request uri
                           :method :post
                           :additional-headers (remove :content-type headers :key #'car :test #'equalp)
                           :content-type (cdr (assoc :content-type headers :test #'equalp))
                           :content (flex:string-to-octets payload :external-format :utf-8)
                           :force-binary t)
    (values (if result-octets
                (flex:octets-to-string result-octets :external-format :utf-8)
                "")
            status-code
            headers)))


(defun http-post (uri payload headers &key (timeout 300))
  (let* (result
         status
         headers-in
         (thread (sb-thread:make-thread (lambda ()
                                          (multiple-value-setq (result status headers-in)
                                            (http-post* uri payload headers)))
                                        :name (cdr (assoc :x-amz-target headers)))))
    (unwind-protect
         (loop with wait-until = (+ (get-universal-time) timeout)
               until (or (not (sb-thread:thread-alive-p thread))
                         (< wait-until (get-universal-time)))
               do (sleep 1))
      (when (sb-thread:thread-alive-p thread)
        (log-trace "http timeout ~S" (cdr (assoc :x-amz-target headers)))
        (sb-thread:terminate-thread thread)))
    (values result status headers-in)))


(defun swf-request* (region action payload)
  "Executes an swf action. Region is a string designator for an aws
region. Action is a string. Payload is a JSON object. Returns four
values, the result JSON object or NIL, the http status code, the http
headers, the unparsed result as a string."
  (let* ((payload-string (json-stringify payload))
         (host (format nil "swf.~(~A~).amazonaws.com" region))
         (url (format nil "https://~A/" host))
         (target (format nil "SimpleWorkflowService.~A" action))
         (content-type "application/x-amz-json-1.0"))
    (log-trace "swf-request: ~A ~A" region action)
    (multiple-value-bind (authz date)
        (aws-sign4 :region region
                   :service :swf
                   :method :post
                   :host host
                   :path "/"
                   :headers `((:x-amz-target . ,target)
                              (:content-type . ,content-type))
                   :payload payload-string)
      (multiple-value-bind (result-string status headers)
          (http-post url payload-string
                     `((:x-amz-target . ,target)
                       (:content-type . ,content-type)
                       (:x-amz-date . ,date)
                       (:authorization . ,authz)))
        (log-trace "  result status: ~A size: ~A" status (length result-string))
        (values (when (plusp (length result-string))
                  (json-parse result-string :use-ratios t))
                status
                headers
                result-string)))))


(define-condition swf-error (error)
  ((region      :initarg :region
                :reader swf-error-region)
   (action      :initarg :action
                :reader swf-error-action)
   (payload     :initarg :payload
                :reader swf-error-payload)
   (status-code :initarg :status-code
                :reader swf-error-status-code)
   (type        :initarg :type
                :reader swf-error-type)
   (message     :initarg :message
                :reader swf-error-message)
   (request-id  :initarg :request-id
                :reader swf-error-request-id))
  (:report (lambda (c stream)
             (format stream "~A: ~A~%Action: ~A~%Region: ~A~@[~%Domain: ~A~]~%Request id: ~A"
                     (swf-error-type c)
                     (swf-error-message c)
                     (swf-error-action c)
                     (swf-error-region c)
                     (cdr (assoc "domain" (cdr (swf-error-payload c)) :test #'equal))
                     (swf-error-request-id c)))))


(defmacro define-swf-conditions (&body names)
  (flet ((fault-name (string)
           (remove #\- (format nil "~:(~A~)-Fault"
                               (subseq string 0 (- (length string) 6))))))
    `(progn
       (defparameter *swf-conditions*
         '(,@(loop for name in names
                   collect `(,(fault-name (symbol-name name)) . ,name))))
       ,@(loop for name in names
               collect `(define-condition ,name (swf-error) ())))))


(define-swf-conditions
  domain-already-exists-error
  domain-deprecated-error
  limit-exceeded-error
  operation-not-permitted-error
  type-already-exists-error
  type-deprecated-error
  unknown-resource-error
  workflow-execution-already-started-error)


(defun swf-request (service action payload)
  "Executes an swf action. Region is a string designator for an aws
region. Action is a string. Payload is a JSON object. Returns four the
result JSON object or NIL. Might signal an error of subtype swf-error."
  (assert service () "No swf service given")
  (multiple-value-bind (result status headers)
      (let ((*aws-credentials* (getf service :credentials)))
        (swf-request* (getf service :region) action payload))
    (cond ((eql status 200)
           result)
          ((numberp status)
           (let* ((type-string (cdr (assoc "__type" (cdr result) :test #'string=)))
                  (hash-pos (position #\# type-string :from-end t))
                  (type (subseq type-string (1+ hash-pos)))
                  (condition (or (cdr (assoc type *swf-conditions* :test #'string=))
                                 'swf-error)))
             (error condition
                    :region (getf service :region)
                    :action action
                    :payload payload
                    :status-code status
                    :type type
                    :message (cdr (assoc "message" (cdr result) :test #'string-equal))
                    :request-id (cdr (assoc :x-amzn-requestid headers :test #'equalp))))))))


(defun credentials-from-file ()
  (let (access-key secret)
    (with-open-file (in (merge-pathnames ".aws" (user-homedir-pathname)))
      (setf access-key (read-line in))
      (setf secret (read-line in)))
    (lambda ()
      (values access-key secret))))



(defvar *service*)


(defun service (&key region credentials domain)
  (list :region (or region :eu-west-1)
        :credentials (or credentials (credentials-from-file))
        :domain (or domain "default")))


(defmacro with-service ((&key region credentials domain) &body body)
  `(let ((*service* (service :region ,region
                             :credentials ,credentials
                             :domain ,domain)))
     ,@body))
