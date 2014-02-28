(in-package :swf-workers)

(declaim (optimize (speed 0) (debug 3) (safety 3) (space 0)))


(defclass task ()
  ((state :initarg :state
          :initform nil)
   (scheduled-event-id :initarg :scheduled-event-id
                       :initform nil
                       :reader task-scheduled-event-id)
   (started-event-id :initform nil
                     :reader task-started-event-id)
   (closed-event-id :initform nil
                    :reader task-closed-event-id)
   (request-cancel-event-ids :initform nil
                             :reader task-request-cancel-event-ids)))


(defclass activity (task)
  ((name :reader activity-name)
   (version :reader activity-version)
   (options :reader activity-options)
   (function :reader activity-function)))


(defclass workflow (worker-task)
  ((name :reader workflow-name)
   (version :reader workflow-version)
   (options :reader workflow-options)
   (function :reader workflow-function)))
