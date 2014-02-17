(in-package #:swf)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun dashed-to-camelcase (string &optional init-cap-p)
    (coerce
     (loop for prev-char = (if init-cap-p #\- #\A) then char
           for char across string
           unless (char= #\- char)
           collect (if (char= #\- prev-char)
                       (char-upcase char)
                       (char-downcase char)))
     'string))


  (defun camelcase-to-dashed (string &optional init-cap-p)
    (coerce
     (loop for first = t then nil
           for char across string
           when (and (upper-case-p char)
                     (or (not first) (not init-cap-p)))
           collect #\-
           collect (char-upcase char))
     'string))


  (defun camelcase-to-keyword (string &optional init-cap-p)
    (intern (camelcase-to-dashed string init-cap-p) :keyword))


  (defun keyword-to-camelcase (symbol &optional init-cap-p)
    (dashed-to-camelcase (symbol-name symbol) init-cap-p))


  (defun json-to-keyword (string)
    (if (some #'lower-case-p string)
        (intern (camelcase-to-dashed string t) :swf)
        (intern (substitute #\- #\_ string) :keyword)))


  (defun keyword-to-json (symbol)
    (etypecase symbol
      (keyword (substitute #\_ #\- (symbol-name symbol)))
      (symbol (dashed-to-camelcase (symbol-name symbol) t)))))


(defun swf-type-p (object type)
  ;; todo: implentent this
  (declare (ignore object type))
  t)

(defgeneric transform-for-type (simple-type type type-def data direction))

(defmacro define-type-transform (simple-type &body case-body)
  `(defmethod transform-for-type ((simple-type (eql ',simple-type))
                                  type type-def data direction)
     (ecase direction
       ,@case-body)))

(define-type-transform string
  (:from-json data)
  (:to-json
    (check-type data string)
    data))

(define-type-transform integer
  (:from-json data)
  (:to-json
    (check-type data integer)
    data))

(define-type-transform boolean
  (:from-json data)
  (:to-json (if data t nil)))

(define-type-transform integer-as-string
  (:from-json (parse-integer data))
  (:to-json
    (check-type data integer)
    (princ-to-string data)))

(defun normalize-timeout (timeout)
  (decode-timeout (encode-timeout timeout)))

(defun decode-timeout (seconds)
  (multiple-value-bind (minutes seconds)
      (truncate seconds 60)
    (multiple-value-bind (hours minutes)
        (truncate minutes 60)
      (multiple-value-bind (days hours)
          (truncate hours 24)
        (list :days days :hours hours :minutes minutes :seconds seconds)))))

(defun encode-timeout (timeout)
  (destructuring-bind (&key (days 0) (hours 0) (minutes 0) (seconds 0))
      timeout
    (+ seconds (* 60
                  (+ minutes (* 60
                                (+ hours (* 24
                                            days))))))))

(define-type-transform timeout
  (:from-json
   (if (string= "NONE" data)
       :none
       (decode-timeout (parse-integer data))))
  (:to-json
   (if (eq :none data)
       "NONE"
       (princ-to-string (encode-timeout data)))))

(define-type-transform date-time
  (:from-json
   (multiple-value-bind (unix frac)
       (truncate data)
     (local-time:unix-to-timestamp unix :nsec (truncate (* frac (expt 10 9))))))
  (:to-json
   (coerce (+ (local-time:timestamp-to-unix data)
              (/ (local-time:nsec-of data) (expt 10 9)))
           'double-float)))

(define-type-transform member
  (:from-json (json-to-keyword data))
  (:to-json (keyword-to-json data)))

(define-type-transform array-of
  (:from-json
   (loop for value in (cdr data)
         collect (json-to-swf (second type) value)))
  (:to-json
   `(:array ,@(loop for value in data
                    collect (swf-to-json (second type) value)))))

(define-type-transform object
  (:from-json
   (loop for (key . value) in (cdr data)
         for slot-name = (camelcase-to-keyword key)
         for slot-type = (second (assoc slot-name type-def))
         do (assert slot-type () "SWF type ~S do not have slot ~S." type slot-name)
         collect (cons slot-name (json-to-swf slot-type value))))
  (:to-json
   (loop for (slot-name nil required) in type-def
         when required
         do (assert (assoc slot-name data) () ;; FIXME: doesn't work when slot is NIL
                    "Required slot ~S missing in object ~S" slot-name data))
   `(:object ,@(loop for (slot-name . value) in data
                     for slot-type = (or (second (assoc slot-name type-def))
                                         (error "SWF type ~S do not have slot ~S." type slot-name))
                     when (or value (eq 'boolean slot-type))
                     collect (cons (keyword-to-camelcase slot-name)
                                   (swf-to-json slot-type value))))))

(defun transform% (type data direction)
  (let ((type-def (when (symbolp type) (get type 'swf-type))))
    (if type-def
        (transform-for-type 'object type type-def data direction)
        (transform-for-type (if (listp type)
                                (car type)
                                type)
                            type nil data direction))))

(defun swf-to-json (type data)
  (transform% type data :to-json))

(defun json-to-swf (type data)
  (transform% type data :from-json))


(defmacro %define-swf-type (name &body def)
  (let ((predicate (intern (format nil "~A-P" name) *package*)))
    `(progn
       (defun ,predicate (object)
           (swf-type-p object ',name))
       (deftype ,name () '(satisfies ,predicate))
       (setf (get ',name 'swf-type) ',def))))

(defun ends-with (suffix string)
  (eql (search suffix string)
       (- (length string) (length suffix))))

(defun type-for-slot (slot-name type-def valid-values)
  (cond (valid-values
         `(member ,@(mapcar #'json-to-keyword valid-values)))
        ((ends-with "Timeout" slot-name)
         'timeout)
        ((ends-with "PeriodInDays" slot-name)
         'integer-as-string)
        (t
         (let* ((arrayp (listp type-def))
                (type-string (if arrayp (second type-def) type-def))
                (type-symbol (intern (camelcase-to-dashed type-string t)))
                (type (case type-symbol
                        (strings 'string)
                        (long 'integer)
                        (number 'integer)
                        (otherwise type-symbol))))
           (if arrayp
               (list 'array-of type)
               type)))))

(defmacro define-swf-type (name &body slot-defs)
  `(%define-swf-type ,(intern (camelcase-to-dashed name t))
     ,@(loop for (slot-name . slot-def) in slot-defs
             collect `(,(camelcase-to-keyword slot-name)
                        ,(type-for-slot slot-name (getf slot-def :type) (getf slot-def :valid-values))
                        ,(getf slot-def :required)))))


(defun typed-swf-action (action request-type response-type data)
  (let* ((payload (swf-to-json request-type data))
         (response (funcall *swf-service* action payload)))
    (when response-type
      (json-to-swf response-type response))))


(defun typed-swf-action-paged (action request-type response-type paged-slot payload)
  (let* ((all-pages (cdr (assoc :all-pages payload)))
         (payload* (remove :all-pages payload :key #'car))
         (result (typed-swf-action action request-type response-type payload*)))
    (when result
      (cond (all-pages
             (let ((pages (loop for page = result then (typed-swf-action
                                                        action request-type response-type
                                                        (acons :next-page-token next-page-token
                                                               payload*))
                                for next-page-token = (cdr (assoc :next-page-token page))
                                collect (cdr (assoc paged-slot page))
                                while next-page-token)))
               (setf (cdr (assoc paged-slot result)) (apply #'append pages))
               result))
            (t result)))))


(defmacro define-swf-action (string-name (&body request) &body response)
  (let ((name (intern (camelcase-to-dashed string-name t)))
        (request-type (format nil "~ARequest" string-name))
        (response-type (format nil "~AResponse" string-name))
        (slots (loop for (slot-name) in request
                     collect (intern (camelcase-to-dashed slot-name))))
        (paged-slot (when (assoc "nextPageToken" response :test #'string=)
                      (intern (camelcase-to-dashed (car (find-if (lambda (x)
                                                                   (and (consp (getf x :type))
                                                                        (eq :array-of (car (getf x :type)))))
                                                                 response :key #'cdr)))
                              :keyword))))
    `(progn
       (define-swf-type ,request-type
         ,@request)
       ,@(when response
               `((define-swf-type ,response-type
                    ,@response)))
       (defun ,name (&rest args &key ,@(when paged-slot '(all-pages)) ,@slots)
         (declare (ignore ,@(when paged-slot '(all-pages)) ,@slots))
         (,(if paged-slot 'typed-swf-action-paged 'typed-swf-action)
           ,(keyword-to-camelcase name t)
           ',(intern (camelcase-to-dashed request-type t))
           ',(when response (intern (camelcase-to-dashed response-type t)))
           ,@(when paged-slot (list paged-slot))
           (loop for (key value) on args by #'cddr collect (cons key value)))))))
