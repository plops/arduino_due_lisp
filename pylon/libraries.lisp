(in-package :pylon)

(defun load-pylon-libraries ()
  (cffi:load-foreign-library "libpylon_wrapper.so"))
