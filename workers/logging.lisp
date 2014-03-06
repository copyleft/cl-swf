(in-package #:swf-workers)


(log5:defcategory swf-workers)


(defmacro log-error (control &rest args)
  `(log5:log-for (and log5:error swf-workers) ,control ,@args))


(defmacro log-warn (control &rest args)
  `(log5:log-for (and log5:warn swf-workers) ,control ,@args))


(defmacro log-info (control &rest args)
  `(log5:log-for (and log5:info swf-workers) ,control ,@args))


(defmacro log-debug (control &rest args)
  `(log5:log-for (and log5:debugging swf-workers) ,control ,@args))


(defmacro log-trace (control &rest args)
  `(log5:log-for (and log5:trace swf-workers) ,control ,@args))
