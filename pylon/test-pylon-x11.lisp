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
         :temp (pylon:get-value-f *cams* j "TemperatureAbs"))))

#+nil
(unwind-protect 
     (progn
       (pylon:start-grabbing *cams*)
       (let ((th (sb-thread:make-thread 
		  #'(lambda ()
		      (loop for i below 100 do
			   (multiple-value-bind (cam success-p w h framenr timestamp) 
			       (pylon::grab-store *cams* fds))))
		  :name "camera-acquisition")))
	 (sleep .001)
	 (sb-thread:join-thread th)))
  (pylon:stop-grabbing *cams*))
