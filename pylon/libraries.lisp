(in-package :pylon)

(defun load-pylon-libraries ()
  (cffi:load-foreign-library "/home/martin/arduino_due_lisp/pylon/libpylon_wrapper.so"))
