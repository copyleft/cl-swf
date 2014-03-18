(defsystem #:cl-swf
  :name "cl-swf"
  :depends-on (:log5
               :drakma
               :json-streams
               :aws-sign4)
  :serial t
  :components ((:module "logging"
                        :serial t
                        :components
                        ((:file "packages")
                         (:file "logging")))
               (:module "client"
                        :serial t
                        :components
                        ((:file "packages")
                         (:file "swf")
                         (:file "transform")
                         (:file "types")
                         (:file "actions")))
               (:module "workers"
                        :serial t
                        :components
                        ((:file "packages")
                         (:file "error-handling")
                         (:file "util")
                         (:file "history")
                         (:file "decisions")
                         (:file "workers")))))
