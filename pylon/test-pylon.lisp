(load "~/quicklisp/setup.lisp")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry* '(*default-pathname-defaults*
                                  #p"/home/martin/arduino_due_lisp/pylon/"))
  (asdf:load-system "pylon"))

(defpackage :pylon-test
  (:use :cl :cffi))

(in-package :pylon-test)

(pylon:initialize)
(defparameter *cams* (pylon:create 1))
(pylon:get-max-i *cams* 0 "OffsetX")
(pylon:get-max-i *cams* 0 "Width")
(pylon:terminate)

(loop for e in '("Width" "Height" "OffsetX" "OffsetY") collect
 (pylon:get-value-i *cams* 0 e t nil))

(pylon:get-min-i *cams* 1 "Width")
;  // Width Height OffsetX OffsetY
;  // PixelFormat
(pylon:get-value-e *cams* 1 "PixelFormat")
(pylon:get-symbolics-e *cams* 0 "PixelFormat")

(pylon:get-value-e *cams* 0 "PixelFormat")

(progn
  (pylon:initialize)
  (defparameter *cams* (pylon:create 2))
  (pylon:start-grabbing *cams*)

  (defparameter *buf*
    (foreign-alloc :unsigned-char :count (* 1040 1040)))
  (with-foreign-objects ((cam :int)
			 (success-p :int)
			 (w :int)
			 (h :int))
    (pylon:grab *cams* 1040 1040 *buf*
		cam success-p w h)
    (format nil "~a~%" (list (mem-ref cam :int)
			     (mem-ref success-p :int)
			     (mem-ref w :int)
			     (mem-ref h :int)))))

#+nil
(with-foreign-objects ((cam :int)
			 (success-p :int)
			 (w :int)
			 (h :int))
    (pylon:grab *cams* 1040 1040 *buf*
		cam success-p w h)
    (format nil "~a~%" (list (mem-ref cam :int)
			     (mem-ref success-p :int)
			     (mem-ref w :int)
			     (mem-ref h :int))))

