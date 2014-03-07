(in-package #:swf)


(log5:defcategory swf)


(defmacro log-error (control &rest args)
  `(log5:log-for (and log5:error swf) ,control ,@args))


(defmacro log-warn (control &rest args)
  `(log5:log-for (and log5:warn swf) ,control ,@args))


(defmacro log-info (control &rest args)
  `(log5:log-for (and log5:info swf) ,control ,@args))


(defmacro log-trace (control &rest args)
  `(log5:log-for (and log5:trace swf) ,control ,@args))

(defmacro log-dribble (control &rest args)
  `(log5:log-for (and log5:dribble swf) ,control ,@args))
