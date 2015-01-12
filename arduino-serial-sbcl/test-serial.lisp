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

(destructuring-bind (str fd) *ard*
  (talk-arduino fd str "(dac 728 1200)"))

(close-serial (second *ard*))

(defparameter *ard8* 
  (multiple-value-list
   (open-serial (first (directory "/dev/ttyACM0")) 
		:element-type '(unsigned-byte 8))))

(defun talk-arduino-now (cmd &key (time .009d0))
 (destructuring-bind (str fd) *ard8*
   (let ((s
	  (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				 :external-format :latin-1 
				 :buffering :full)))
     (ensure-response-buffer-clear fd s)
     ;(sleep .3)
     (talk-arduino fd s cmd))))
