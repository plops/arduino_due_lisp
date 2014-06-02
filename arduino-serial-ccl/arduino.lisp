;; code to talk to arduino from clozure common lisp

(defpackage :arduino
  (:use :cl)
  (:export #:no-arduino
	   #:talk-arduino))

(in-package :arduino)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (load "/home/martin/src/ccl/library/serial-streams.lisp"))

(defvar *serial* nil)


(define-condition no-arduino () ())
;; in gentoo user should be in group uucp for access to ACM device
(defparameter *serial*
  (let ((fn (first (directory "/dev/ttyACM*"))))
    (if fn
	(ccl::make-serial-stream (concatenate 'string "/dev/" (pathname-name fn))
					;:format 'character
				 :baud-rate 115200
				 :parity nil
				 :char-bits 8
				 :stop-bits 1 
				 :flow-control nil)
	(progn
	  (break "no arduino to connect to.")
	 nil))))
#+nil
(progn
  (format *serial* "(dac ~d ~d)~%" (+ -900 2048) (+ 130 2048)) 
  (force-output *serial*)
  (sleep .1)
  (list 
   (read-line *serial*)))

#+nil
(read-line *serial*)

(defun talk-arduino (cmd)
  (format *serial* cmd) 
  (force-output *serial*)
  (sleep .1)
  (list 
   (read-line *serial*)))
