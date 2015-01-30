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
