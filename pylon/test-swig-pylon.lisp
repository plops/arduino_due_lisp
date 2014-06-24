(cffi:load-foreign-library "libpylon_wrapper.so")
(pylon_wrapper_initialize)
(defparameter *fac* (pylon_wrapper_factory))

(defparameter *cams* (pylon_wrapper_create *fac* 2))
