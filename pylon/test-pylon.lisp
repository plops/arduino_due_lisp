;; you have to make sure that the following environment variables are
;; set when starting the common lisp interpreter:
;; export PYLON_ROOT=${HOME}/pylon-3.2.1-x86_64/pylon3
;; export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${PYLON_ROOT}/genicam/bin/Linux64_x64
;; export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${PYLON_ROOT}/lib64
;; export GENICAM_ROOT_V2_3=${PYLON_ROOT}/genicam
;; export PYLON_CAMEMU=2


#-sbcl (load "~/quicklisp/setup.lisp")

(declaim (optimize (debug 3)))
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
(loop for e in '("Width" "Height" "OffsetX" "OffsetY") collect
 (pylon:get-value-i *cams* 0 e t nil))
(pylon:get-min-i *cams* 1 "Width")
(pylon:get-value-e *cams* 1 "PixelFormat")
(pylon:get-symbolics-e *cams* 0 "PixelFormat")
(pylon:to-string-e *cams* 0 "PixelFormat")
(pylon:from-string-e *cams* 0 "PixelFormat" "Mono16")

(pylon:get-symbolics-e *cams* 0 "TriggerMode")

(pylon:get-value-e *cams* 0 "PixelFormat")

#+nil (pylon:terminate)


;  // Width Height OffsetX OffsetY
;  // PixelFormat

#+nil (progn
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

