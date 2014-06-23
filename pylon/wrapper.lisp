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
  (camera (:pointer :int))
  (success-p (:pointer :int))
  (w (:pointer :int))
  (h (:pointer :int)))

(cffi:defcfun ("pylon_wrapper_get_max_i" get-max-i) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_get_min_i" get-min-i) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_get_inc_i" get-inc-i) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_get_value_i" %get-value-i) :int
  (cams :pointer)
  (cam :int)
  (node :string)
  (verify :int)
  (ignore-cache :int))

(defun get-value-i (cams cam node &optional (verify nil) (ignore-cache nil))
  (%get-value-i cams cam node (if verify 1 0) (if ignore-cache 1 0)))

(cffi:defcfun ("pylon_wrapper_set_value_i" set-value-i) :void
  (cams :pointer)
  (cam :int)
  (node :string)
  (value :int))

(cffi:defcfun ("pylon_wrapper_get_symbolics_e" get-symbolics-e) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_set_value_e" set-value-e) :void
  (cams :pointer)
  (cam :int)
  (node :string)
  (value :int))

(cffi:defcfun ("pylon_wrapper_get_value_e" get-value-e) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_to_string_e" to-string-e) :void
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_from_string_e" from-string-e) :void
  (cams :pointer)
  (cam :int)
  (node :string)
  (value :string))



