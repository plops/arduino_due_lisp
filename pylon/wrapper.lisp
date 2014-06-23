(in-package :pylon)

(cffi:defcfun ("pylon_wrapper_initialize" initialize) :void)
(cffi:defcfun ("pylon_wrapper_terminate" terminate) :void)
(cffi:defcfun ("pylon_wrapper_create" create) (:pointer :void)
  (max-cameras :unsigned-int))

(cffi:defcfun ("pylon_wrapper_start_grabbing" start-grabbing) :void
  (cams :pointer))

(cffi:defcfun ("pylon_wrapper_grab" grab) :void
  (cams :pointer)
  (ww :int)
  (hh :int)
  (buf (:pointer :unsigned-char))
  (success-p (:pointer :int))
  (w (:pointer :int))
  (h (:pointer :int)))
