(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry*
	'(*default-pathname-defaults*
	  #p"/home/martin/arduino_due_lisp/arduino-serial-sbcl/"
	  #p"/home/martin/arduino_due_lisp/pylon/"
	  #p"/home/martin/arduino_due_lisp/image-processing/"
	  #p"/home/martin/stage/cl-pure-x11/"))
  (asdf:load-system "pylon")
  (asdf:load-system "arduino-serial-sbcl")
  (asdf:load-system "pure-x11"))


(defpackage :pylon-test-x11
  (:use :cl :cffi :pure-x11))

(in-package :pylon-test-x11)

#+nil
(pylon:initialize)

#+nil
(defparameter *cams* (pylon:create (pylon::factory) 3) "Handle to multiple Pylon cameras.")
#+nil
(pylon:cams-open *cams*)

#+nil
(loop for j below 3 collect
      (append 
       (list (parse-integer (pylon:cam-get-serial-number *cams* j)))
       (loop for e in '("BinningHorizontal" "BinningVertical" 
            "Width" "Height"
            "OffsetX" "OffsetY" 
            "ExposureTimeRaw" "GainRaw" 
            "GevTimestampTickFrequency" "GevSCPSPacketSize") collect
            (pylon:get-value-i *cams* j e t nil)
            )
       (list :trigger-mode (pylon:get-value-e *cams* j "TriggerMode")
         :last-error (pylon:get-value-e *cams* j "LastError")
         :rate-p (pylon:get-value-b *cams* j "AcquisitionFrameRateEnable")
         :reverse-x (pylon:get-value-b *cams* j "ReverseX")
         :rate (pylon:get-value-f *cams* j "ResultingFrameRateAbs")
         ;; :temp (pylon:get-value-f *cams* j "TemperatureAbs")
	 )))

;; => ((21433565 1 1 280 280 777 337   70 0 125000000 1500 :TRIGGER-MODE 0 :LAST-ERROR 0 :RATE-P 0 :REVERSE-X 0 :RATE 97.18173)
;;     (21433566 1 1 512 512 789 112 2975 0 125000000 1500 :TRIGGER-MODE 0 :LAST-ERROR 0 :RATE-P 0 :REVERSE-X 0 :RATE 54.318306)
;;     (21433540 1 1 280 280 985 427   35 0 125000000 1500 :TRIGGER-MODE 0 :LAST-ERROR 0 :RATE-P 0 :REVERSE-X 1 :RATE 97.18173))

#+nil
(progn ;; open a window and draw a line
  (connect)
  (make-window)
  (draw-window 0 0 100 100))


#+nil
(let*((w 512)
      (h 512)
      (c 4)
      (a (make-array (list h w c)
             :element-type '(unsigned-byte 8))))
  (dotimes (j h)
    (dotimes (i w)
      (setf (aref a j i 0) (mod i 255)  ;; b
        (aref a j i 1) (mod j 255)  ;; g
        (aref a j i 2) 255          ;; r
        (aref a j i 3) 255)))       ;; a
  (put-image-big-req a))

(defun put-sf-image (a w h)
  (declare (type (simple-array single-float 2) a))
  (let* ((a1 (sb-ext:array-storage-vector a))
	 (c 4)
	 (b (make-array (list h w c)
			:element-type '(unsigned-byte 8)
			:initial-element 255))
	 (b1 (sb-ext:array-storage-vector b)))
    (dotimes (i (* w h))
      (setf (aref b1 (+ 0 (* 4 i))) (floor (* 255s0 (aref a1 i)) 4095)
	    (aref b1 (+ 1 (* 4 i))) (floor (* 255s0 (aref a1 i)) 4095)
	    (aref b1 (+ 2 (* 4 i))) (floor (* 255s0 (aref a1 i)) 4095)))
    (put-image-big-req b)))

(defparameter *buf-s* (make-array (list 512 512) :element-type 'single-float))

#+nil
(unwind-protect 
     (progn
       (pylon:start-grabbing *cams*)
       (let ((th (sb-thread:make-thread 
		  #'(lambda ()
		      (loop for i below 200 do
			   (multiple-value-bind (cam success-p w h framenr timestamp) 
			       (pylon::grab-sf *cams* *buf-s*)
			     (put-sf-image *buf-s* w h)
			     )))
		  :name "camera-acquisition")))
	 (sleep .001)
	 (sb-thread:join-thread th)))
  (pylon:stop-grabbing *cams*))
