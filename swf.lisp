(in-package :swf)


(defun http-post (uri payload headers)
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
        (values (when (plusp (length result-string))
                  (json-parse result-string :use-ratios t))
                status
                headers
                result-string)))))


(define-condition swf-error (error)
  ((status-code :initarg :status-code
                :reader swf-error-status-code)
   (type        :initarg :type
                :reader swf-error-type)
   (message     :initarg :message
                :reader swf-error-message)
   (request-id  :initarg :request-id
                :reader swf-error-request-id))
  (:report (lambda (c stream)
             (format stream "~A: ~A (Request id ~A)"
                     (swf-error-type c)
                     (swf-error-message c)
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


(defun swf-request (credentials region action payload)
  "Executes an swf action. Region is a string designator for an aws
region. Action is a string. Payload is a JSON object. Returns four the
result JSON object or NIL. Might signal an error of subtype swf-error."
  (multiple-value-bind (result status headers)
      (let ((*aws-credentials* credentials))
        (swf-request* region action payload))
    (cond ((eql status 200)
           result)
          (t
           (let* ((type-string (cdr (assoc "__type" (cdr result) :test #'string=)))
                  (hash-pos (position #\# type-string :from-end t))
                  (type (subseq type-string (1+ hash-pos)))
                  (condition (or (cdr (assoc type *swf-conditions* :test #'string=))
                                 'swf-error)))
             (error condition
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


(defun swf-service (credentials region)
  (lambda (action payload)
    (swf-request credentials region action payload)))


(defvar *swf-service*)


(defmacro with-service (&body body)
  `(let ((*swf-service* (swf-service (credentials-from-file) :eu-west-1)))
     ,@body))
