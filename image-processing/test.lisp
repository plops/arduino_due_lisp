(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry*
	'(*default-pathname-defaults*
	  #p"/home/martin/arduino_due_lisp/image-processing/"))
  (asdf:load-system "image-processing"))

(defpackage :image-processing-test
  (:use :cl :image-processing))

(in-package :image-processing-test)

(require :sb-sprof)

#+nil
(sb-sprof:with-profiling (:max-samples 1000
				       :report :flat
				       :loop nil)
  (cpu-test 7))

(time (cpu-test 7))

(defun cpu-test (n)
 (let ((a (make-array (list 512 512) :element-type '(complex double-float))))
   (dotimes (i (expt 2 n))
     (.abs a))
   nil))
