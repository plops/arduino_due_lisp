(in-package :pylon)

(let ((lib nil))
 (defun load-pylon-libraries ()
   (setf lib (cffi:load-foreign-library "/home/martin/arduino_due_lisp/pylon/libpylon_wrapper.so")))
 (defun unload-pylon-library ()
   (cffi:close-foreign-library lib)))

#+nil
(cffi:list-foreign-libraries)
#+nil
(cffi:close-foreign-library (first (cffi:list-foreign-libraries)))
#+nil
(load-pylon-libraries)
#+nil
(unload-pylon-library)
