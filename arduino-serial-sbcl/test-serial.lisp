(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry*
	'(*default-pathname-defaults*
	  #p"/home/martin/arduino_due_lisp/arduino-serial-sbcl/"))
  (asdf:load-system "arduino-serial-sbcl"))


(defpackage :test-serial
  (:use :cl :arduino-serial-sbcl))
(in-package :test-serial)
	    
(defparameter *ard* 
  (multiple-value-list
   (open-serial (first (directory "/dev/ttyACM0")))))

(destructuring-bind (str fd) *ard*
  (talk-arduino fd str "(+ 1 2)"))

(close-serial (second *ard*))

(defparameter *ard8* 
  (multiple-value-list
   (open-serial (first (directory "/dev/ttyACM0")) 
		:element-type '(unsigned-byte 8))))
