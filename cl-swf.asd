(defsystem #:cl-swf
  :name "cl-swf"
  :depends-on (:drakma
               :json-streams
               :aws-sign4)
  :serial t
  :components ((:file "packages")
               (:file "swf")
               (:file "transform")
               (:file "types")
               (:file "actions")))
