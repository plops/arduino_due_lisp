(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry*
	'(*default-pathname-defaults*
	  #p"/home/martin/arduino_due_lisp/image-processing/"))
  (asdf:load-system "image-processing")
)

(defpackage :image-processing-test
  (:use :cl :image-processing))
