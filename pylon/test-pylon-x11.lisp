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
  (make-window :width (+ 512 280 280) :height 512)
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

(defun put-sf-image (a w h &key (dst-x 0) (dst-y 0))
  (declare (type (simple-array single-float 2) a)
	   (type (unsigned-byte 16) w h dst-x dst-y)
	   (optimize (speed 3)))
  (let* ((a1 (sb-ext:array-storage-vector a))
	 (c 4)
	 (b (make-array (list h w c)
			:element-type '(unsigned-byte 8)
			:initial-element 255))
	 (b1 (sb-ext:array-storage-vector b))
	 (n (* w h))
	 (scale (/ 255s0 4095)))
    (declare (type (simple-array single-float 1) a1)
	     (type (simple-array (unsigned-byte 8) 1) b1))
    (dotimes (i n)
      (let ((v (floor (* scale (aref a1 i)))))
	(declare (type (unsigned-byte 8) v))
       (setf (aref b1 (+ 0 (* 4 i))) v
	     (aref b1 (+ 1 (* 4 i))) v
	     (aref b1 (+ 2 (* 4 i))) v)))
    (put-image-big-req b :dst-x dst-x :dst-y dst-y) ))

(defparameter *buf-s* (make-array (list 512 512) :element-type 'single-float))



(defun get-us-time ()
 (multiple-value-bind (s us) (sb-unix::get-time-of-day)
   (+ (* 1000000 s) us)))

#+nil
(let ((old-time 0)
      (old-stamp 0)
      (old-frame 0))
  (with-open-file (s "times.dat" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for (time cam success w h frame timestamp) in (reverse *log*) and i from 0
       do (when (< cam 1) 
	    (unless (= i 0)
	      (format s "~a  ~a ~a ~a~%"
		      time
		      frame
		      (- time old-time)
		      (- timestamp old-stamp)))
	    (setf old-time time
		  old-stamp timestamp
		  old-frame frame)))))

#+nil
(unwind-protect 
     (let ((start (get-us-time)))
       (progn
	 (defparameter *log* nil)
	(dotimes (i 3) ;; reset frame timers on the cameras ;
	  (pylon::command-execute *cams* i "GevTimestampControlReset"))
	(pylon:start-grabbing *cams*)
	(let ((th (sb-thread:make-thread 
		   #'(lambda ()
		       (loop for i below 2000 do
			    (multiple-value-bind (cam success-p w h framenr timestamp) 
				(pylon::grab-sf *cams* *buf-s*)
			      (push (list  (- (get-us-time) start) cam success-p w h framenr timestamp) *log*)
			      (put-sf-image *buf-s* w h :dst-x (ecase cam
								 (0 0)
								 (1 280)
								 (2 (+ 512 280)))))))
		   :name "camera-acquisition")))
	  (sleep .001)
	  (sb-thread:join-thread th))))
  (pylon:stop-grabbing *cams*))
