(in-package :pylon)

(cffi:defcfun ("pylon_wrapper_initialize" pylon-initialize) :void)
(cffi:defcfun ("pylon_wrapper_terminate" pylon-terminate) :void)
