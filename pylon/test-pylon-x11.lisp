(declaim (optimize (debug 3) (safety 3)))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry*
	'(*default-pathname-defaults*
	  #p"/home/martin/arduino_due_lisp/arduino-serial-sbcl/"
	  #p"/home/martin/arduino_due_lisp/pylon/"
	  #p"/home/martin/arduino_due_lisp/image-processing/"
	  #p"/home/martin/stage/cl-pure-x11/"
	  #p"/home/martin/stage/cl-ics/"
	  #p"/home/martin/stage/cl-cffi-fftw3/"
	  #p"/home/martin/arduino_due_lisp/image-processing/"))
  (asdf:load-system "pylon")
  (asdf:load-system "arduino-serial-sbcl")
  (asdf:load-system "pure-x11")
  (asdf:load-system "fftw")
  (asdf:load-system "ics")
  (asdf:load-system "image-processing")
  (require :sb-sprof))


(defpackage :pylon-test-x11
  (:use :cl :cffi :pure-x11 :image-processing))

(in-package :pylon-test-x11)

(progn
  (pylon:initialize)
  (defparameter *cams* (pylon:create (pylon::factory) 3) "Handle to multiple Pylon cameras.")
  (pylon:cams-open *cams*))

#+nil
(pylon:terminate *cams*)
#+nil
(pylon:cams-close *cams*)
(defparameter *ard* 
  (multiple-value-list
   (arduino-serial-sbcl:open-serial 
    (first (directory "/dev/ttyACM0")))))

#+nil
(progn (arduino-serial-sbcl:close-serial (second *ard*))
       (setf *trigger-outputs-initialized* nil))
(defvar *trigger-outputs-initialized* nil)
(defun initialize-trigger-outputs ()
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   "(progn
 (pin-mode 6 1)
 (pin-mode 10 1)
 (pin-mode 11 1)
 (pin-mode 12 1)
 (digital-write 11 0)
 (digital-write 12 0)  
 (digital-write 10 0))")
  (setf *trigger-outputs-initialized* t))

(initialize-trigger-outputs)
(defun trigger-all-cameras-seq (n &key (delay-ms 24))
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   (format nil 
	   "(progn
  (set 'i 0)
  (while (< i ~a)
    (delay ~a)
    (digital-write 11 1)
    (digital-write 12 1) 
    (digital-write 10 1) 
    (delay 1)
    (digital-write 11 0)
    (digital-write 12 0) 
    (digital-write 10 0)
    (set 'i (+ i 1))))
"	   n delay-ms)
   :time .1d0))

(format nil "nalfdn s ~d" 3)

(defun trigger-all-cameras-once ()
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   (format nil 
	   "(progn
    (delay 1)
    (digital-write 11 1)
    (digital-write 12 1) 
    (digital-write 10 1) 
    (delay 1)
    (digital-write 11 0)
    (digital-write 12 0) 
    (digital-write 10 0))
")
   :time .1d0))

(defun trigger-flipmount-once ()
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   (format nil 
	   "(progn (digital-write 6 1) (delay 1) (digital-write 6 0))")
   :time .1d0))

#+nil
(arduino-serial-sbcl:talk-arduino
   ( second *ard*) 
   (first *ard*)
   "(progn (+ 1 2))")
#+nil
(arduino-serial-sbcl:talk-arduino
   ( second *ard*) 
   (first *ard*)
   "(room)")
#+nil
(arduino-serial-sbcl:talk-arduino
   ( second *ard*) 
   (first *ard*)
   "(dac 1500 2120)")
#+nil
(trigger-all-cameras-once)
(defun arduino-dac (x y)
  (declare (type (integer 0 4095) x y))
 (arduino-serial-sbcl:talk-arduino
  ( second *ard*) 
  (first *ard*)
  (format nil "(dac ~a ~a)" x y)))

(defun arduino-trigger (&optional (active nil))
  (dotimes (i 3)
    (pylon:set-value-e *cams* i "TriggerMode" (if active 1 0))))

(fftw:prepare-threads)

(fftw::%fftwf_plan_with_nthreads 6)


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
#+nil
(pylon:set-value-i *cams* 1 "OffsetX" 789)
#+nil
(pylon:set-value-i *cams* 1 "OffsetY" 112)

;; ((21433565 1 1 256 256 783 342   70 0 125000000 9000 :TRIGGER-MODE 0 :LAST-ERROR 1 :RATE-P 0 :REVERSE-X 0 :RATE 105.82011)
;;  (21433566 1 1 512 512 789 112 2975 0 125000000 9000 :TRIGGER-MODE 0 :LAST-ERROR 1 :RATE-P 0 :REVERSE-X 0 :RATE 54.318306)
;;  (21433540 1 1 256 256 996 439  105 0 125000000 9000 :TRIGGER-MODE 0 :LAST-ERROR 1 :RATE-P 0 :REVERSE-X 1 :RATE 105.82011))

;; cam1 66 roi settings to be outside of fiber core 332 383

(progn ;; open a window and draw a line
  (connect)
  (make-window :width (+ 512 256 256) :height (+ 512 512 64))
  (draw-window 0 0 100 100))

(defun put-sf-image (a w h &key (dst-x 0) (dst-y 0) (scale-max 4095s0))
  (declare (type (simple-array single-float 2) a)
	   (type (unsigned-byte 16) w h dst-x dst-y)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((a1 (sb-ext:array-storage-vector a))
	 (c 4)
	 (b (make-array (list h w c)
			:element-type '(unsigned-byte 8)
			:initial-element 255))
	 (b1 (sb-ext:array-storage-vector b))
	 (n (* w h))
	 (scale (/ 255s0 scale-max)))
    (declare (type (simple-array single-float 1) a1)
	     (type (simple-array (unsigned-byte 8) 1) b1))
    (dotimes (i n)
      (let ((v (round (* scale (aref a1 i)))))
	(declare (type (unsigned-byte 8) v))
       (setf (aref b1 (+ 0 (* 4 i))) v
	     (aref b1 (+ 1 (* 4 i))) v
	     (aref b1 (+ 2 (* 4 i))) v)))
    (put-image-big-req b :dst-x dst-x :dst-y dst-y) ))

(defun put-csf-image (a &key (w (array-dimension a 1)) (h (array-dimension a 0))
			  (x0 0 x0-p) (y0 0 y0-p) (x1 0 x1-p) (y1 0 y1-p)
			  (dst-x 0) (dst-y 0) (scale (/ 20s0 4095)) (offset 0s0)
			  (fun #'abs))
  (declare (type (simple-array (complex single-float) 2) a)
	   (type (unsigned-byte 16) w h dst-x dst-y)
	   (type fixnum x1 y1 x0 y0)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((a1 (sb-ext:array-storage-vector a))
	 (c 4)
	 (b (make-array (list h w c)
			:element-type '(unsigned-byte 8)
			:initial-element 255))
	 (b1 (sb-ext:array-storage-vector b))
	 (n (* w h)))
    (declare (type (simple-array (complex single-float) 1) a1)
	     (type (simple-array (unsigned-byte 8) 1) b1))
    
    (progn ;dotimes (j h)
     (dotimes (i n)
       (let ((v (min 255 (max 0 (round (* scale (+ (funcall fun (aref a1 i)) offset)))))))
	 (declare (type (unsigned-byte 8) v))
	 (setf (aref b1 (+ 0 (* 4 i))) v
	       (aref b1 (+ 1 (* 4 i))) v
	       (aref b1 (+ 2 (* 4 i))) v)
	 #+nil (setf (aref b j i 0) v
	       (aref b j i 1) v
	       (aref b j i 2) v))))
    (when (and x0-p x1-p y0-p y1-p)
     (let ((v 255)
	   (xx0 (max 0 (min w (min x0 x1))))
	   (xx1 (max 0 (min w (max x0 x1))))
	   (yy0 (max 0 (min h (min y0 y1))))
	   (yy1 (max 0 (min h (max y0 y1)))))
       (dotimes (c 3)
	 (loop for i from xx0 upto xx1 do
	      (setf (aref b yy0 i c) v
		    (aref b yy1 i c) v))
	 (loop for j from yy0 upto yy1 do
	      (setf (aref b j xx0 c) v
		    (aref b j xx1 c) v)))))
    (put-image-big-req b :dst-x dst-x :dst-y dst-y) ))

(defparameter *buf-s* (loop for i below 3 collect (make-array (list 512 512) :element-type 'single-float)))
(defparameter *buf-s-capture* (make-array (list 512 512) :element-type 'single-float))
(defparameter *buf-cs* (make-array (list 512 512) :element-type '(complex single-float)))
(defparameter *buf-cs64in* (make-array (list 64 64) :element-type '(complex single-float)))
(defparameter *buf-cs64out* (make-array (list 64 64) :element-type '(complex single-float)))

(fftw::%fftwf_import_wisdom_from_filename "fiberholo.fftwf.wisdom")
(time
 (progn 
   (progn (fftw::rftf *buf-s-capture* :out-arg *buf-cs* :w 256 :h 256 :flag fftw::+patient+) nil)
   (progn (fftw::rftf *buf-s-capture* :out-arg *buf-cs* :w 512 :h 512 :flag fftw::+patient+) nil)))
(fftw::%fftwf_export_wisdom_to_filename "fiberholo.fftwf.wisdom")


(defun get-us-time ()
 (multiple-value-bind (s us) (sb-unix::get-time-of-day)
   (+ (* 1000000 s) us)))

(defparameter *log* nil)
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

(defun reset-camera-timers (cams n)
  (dotimes (i n) ;; reset frame timers on the cameras ;
    (pylon::command-execute cams i "GevTimestampControlReset")))


(defparameter *plan64* (fftw::planf *buf-cs64in* :out *buf-cs64out* :w 64 :h 64 :flag fftw::+measure+ :sign fftw::+backward+))
#+nil
(fftw::%fftwf_destroy_plan *plan64*)

(defparameter *plan256-0* (fftw::rplanf (elt *buf-s* 0) :out *buf-cs* :w 256 :h 256 :flag fftw::+measure+))
(defparameter *plan256-2* (fftw::rplanf (elt *buf-s* 2) :out *buf-cs* :w 256 :h 256 :flag fftw::+measure+))
(defparameter *plan512-1* (fftw::rplanf (elt *buf-s* 1) :out *buf-cs* :w 512 :h 512 :flag fftw::+measure+))

(defun cam-dst-x (cam)
  (ecase cam
    (0 0)
    (1 256)
    (2 (+ 512 256))))

(defparameter *store* (loop for i from 0 below 10 collect (make-array (list 64 64) :element-type '(complex single-float))))
(defparameter *store-index* 0)

#+nil
(/ (* (expt 70 2) 64 64 3 2 2 8) (* 1024 1024s0)) 
(let* ((n (* 16 16))
       (store (loop for i from 0 below n collect
		   (loop for cam below 3 collect
			(loop for pol below 2 collect
			     (loop for ft below 1 collect 
				  (make-array (list 64 64) :element-type '(complex single-float)))))))
       (current-index 0))
  (defun get-stored-array (ft pol cam &optional (index current-index index-p))
    (declare (values (simple-array (complex single-float) 2) &optional))
    ;;(format t "storing in ~a~%" (list 'ft ft 'pol pol 'cam cam 'index index))
    (prog1
	(elt (elt (elt (elt store (mod index (length store))) cam) pol) ft)
      (unless index-p
	(setf current-index (mod (1+ current-index) (length store))))))
  (defun get-current-index () current-index)
  (defun reset-current-index () (setf current-index 0))
  (defun get-stored-array-length () (length store)))
#+nil
(get-current-index)
#+nil
(get-stored-array 0)

#+nil
(dotimes (i (length *store*))
 (put-csf-image (elt *store* i) :w 64 :h 64 :dst-x (* 64 i) :scale 1s0 :offset 0s0))
(defparameter *avg* nil)


(defun display-mosaic (&key (start 0) (end (+ start 15)) (pol 0) (avg-start 0) (avg-end (get-stored-array-length)) 
			 (subtract-avg nil))
  (when subtract-avg (calc-avg :start avg-start :end avg-end))
  (let ((scale 1s0)
       (offset 70s0))
   (progn 
     ;(draw-window 0 0 100 100)
     (dotimes (ft 2)
      (dotimes (cam 3)
	(loop for i from start below (min end (get-stored-array-length)) do
	     (let ((z (if subtract-avg
			  (image-processing::.-csf (get-stored-array ft pol cam i) (elt *avg* cam))
			  (get-stored-array ft pol cam i)))
		   (si (- i start))) 
	       (put-csf-image z
			      :w 64 :h 64 :dst-x (+ (* 65 ft) (* 2 65 si))
			      :dst-y (* 65 cam)
			      :scale (/ 255 (* 2 3.1415)) :offset 3.1416s0 :fun #'phase)
	       (put-csf-image z
			      :w 64 :h 64 :dst-x (+ (* 65 ft) (* 2 65 si))
			      :dst-y (+ (* 3 65) (* 65 cam))
			      :scale (ecase ft (0 100s0)
					    (1 scale)) :offset 0s0 :fun #'abs)
	       (let ((scal (ecase ft (0 100s0)
				  (1 scale)))
		     (off (ecase ft (0 1s0)
				  (1 70s0))))
		 (put-csf-image z
			       :w 64 :h 64 :dst-x (+ (* 65 ft) (* 2 65 si))
			       :dst-y (+ (* 6 65) (* 65 cam))
			       :scale scal :offset off :fun #'realpart)
		 (put-csf-image z
				:w 64 :h 64 :dst-x (+ (* 65 ft) (* 2 65 si))
				:dst-y (+ (* 9 65) (* 65 cam))
				:scale scal :offset off :fun #'imagpart)))))))))

(defun abs2 (z)
  (declare (type (complex single-float) z)
	   (inline )
	   (optimize (speed 3) (safety 3) (debug 1))
	   (values single-float &optional))
  (realpart (* z (conjugate z))))

(defun get-global-maximum-position (&key (x 0) (y 0) (ft 0) (pol 0) (cam 1)
				      (w (floor (sqrt (get-stored-array-length)))) (h w)
				      (x-offset 0) (y-offset 0))
  (let ((z (get-stored-array ft pol cam (min (* w h) (+ (min w (+ x x-offset)) 
							(* w (min h (+ y y-offset))))))))
    (destructuring-bind (hh ww) (array-dimensions z)
      (let ((ma (abs2 (aref z 0 0)))
	    (pos (list 0s0 0 0)))
       (loop for i below ww do
	    (loop for j below hh do
		 (let ((v (abs2 (aref z j i))))
		   (when (< ma v)
		     (setf ma v
			   pos (list v j i))))))
       pos))))

(defun get-local-maximum-positions
    (&key (x 0) (y 0) (ft 0) (pol 0) (cam 1)
       (w (floor (sqrt (get-stored-array-length)))) (h w)
       (x-offset 0) (y-offset 0))
  ;; neighbors
  ;; a b c
  ;; d e f
  ;; g h i
  (macrolet ((z (yo xo)
	       `(get-stored-array ft pol cam (min (* w h) (+ (min w (+ x ,xo x-offset)) 
							  (* w (min h (+ y ,yo y-offset))))))))
   (let ((ze (z 0 0))
	 (za (z -1 -1))
	 (zb (z -1 0))
	 (zc (z -1 1))
	 (zd (z 0 -1))
	 (zf (z 0 1))
	 (zg (z 1 -1))
	 (zh (z 1 0))
	 (zi (z 1 1)))
     (destructuring-bind (hh ww) (array-dimensions ze)
       (let ((pos nil))
	 (loop for i from 1 below (1- ww) do
	      (loop for j from 1 below (1- hh) do
		   (let ((v (abs2 (aref ze j i))))
		     (macrolet ((local-max-p (z)
				  `(and (< (abs2 (aref ,z (1+ j) (1+ i))) v) ;; i
					(< (abs2 (aref ,z (1+ j) 0)) v) ;; h
					(< (abs2 (aref ,z (1+ j) (1- i))) v) ;; g
					(< (abs2 (aref ,z (1- j) (1+ i))) v) ;; c 
					(< (abs2 (aref ,z (1- j) i)) v) ;; b 
					(< (abs2 (aref ,z (1- j) (1- i))) v) ;; a
					(< (abs2 (aref ,z j (1+ i))) v) ;; f
					(< (abs2 (aref ,z j (1- i))) v) ;; d
					)))
		       (when (and (local-max-p ze)
				  (local-max-p zb)
				  (local-max-p zh)
				  (local-max-p zf)
				  (local-max-p zd))
			(push (list v j i) pos))))))
	 (sort pos #'> :key #'first))))))


#+nil
(first (get-local-maximum-positions :x 5 :y 5))
#+nil
(defun get-reflex-position (&key (x 0) (y 0) (ft 0)  (cam 1)
			      (w (floor (sqrt (get-stored-array-length)))) (h w)
			      (x-offset 0) (y-offset 0))
  (let ((z0 (get-stored-array ft 0 cam (min (* w h) (+ (min w (+ x x-offset)) 
						       (* w (min h (+ y y-offset)))))))
	(z1 (get-stored-array ft 1 cam (min (* w h) (+ (min w (+ x x-offset)) 
							(* w (min h (+ y y-offset))))))))
    (destructuring-bind (hh ww) (array-dimensions z)
      (let ((ma (abs2 (aref z 0 0)))
	    (pos nil))
       (loop for i below ww do
	    (loop for j below hh do
		 (let ((v (abs2 (aref z j i))))
		   (when (< ma v)
		     (setf ma v
			   pos (list j i))))))
       pos))))

#+nil
(loop for i below 32 collect
     (loop for j below 32 collect
	  (get-global-maximum-position :x i :y j)))


(defun display-mosaic-onecam (&key (ft 0) (w (floor (sqrt (get-stored-array-length)))) (h w) (x-offset 0) (y-offset 0) (cam 1) (pol 0) (scale 100s0) (offset 0s0) (mark-global-maxima-p t) (global-threshold 0s0))
  (loop for i from 0 below w do
       (loop for j from 0 below h do
	    (let ((z (get-stored-array ft pol cam (min (* w h) (+ (min w (+ i x-offset)) 
								 (* w (min h (+ j y-offset)))))))
		  ) 
	  (put-csf-image z
			 :w 64 :h 64 
			 :dst-x (* 65 i)
			 :dst-y (* 65 j)
			 :scale scale :offset offset :fun #'abs))))
  (when mark-global-maxima-p
   (loop for i below w do
	(loop for j below h do
	     (progn ;let ((ma (get-local-maximum-positions :pol pol :ft ft :x i :y j :cam cam)))
	      (progn ;dolist (pos (subseq ma  0 (min 2 (length ma))))
		(destructuring-bind (v y x) (get-global-maximum-position :x i :y j :cam cam :ft ft :pol pol :x-offset x-offset
								       :y-offset y-offset :w w :h h)
		  (incf x (* 65 i))
		  (incf y (* 65 j))
		  (when (< global-threshold v) (draw-window (max 0 x) (max 0 (- y 10)) x (max 0 (- y 3)))
		   (draw-window (max 0 x) (+ y 10) x (+ y 3))
		   (draw-window (max 0 (- x 10)) y (max 0 (- x 3)) y)
		   (draw-window (max 0 (+ x 10)) y (+ x 3) y)))))))))
#+nil
(display-mosaic-onecam-swap :pol 0 :cam 1 :x-offset 0 :y-offset 0 :w 14 :h 14)
(defun display-mosaic-onecam-swap (&key (w 16) (h 16) (x-offset 0) (y-offset 0) (cam 1) (pol 0))
  (let ((a (make-array (list 64 64) 
		       :element-type '(simple-array (complex single-float) 2)
		       :initial-contents
		       (loop for i below 64 collect
			    (loop for j below 64 collect
				 (make-array (list h w) :element-type '(complex single-float)))))))
    (declare (type (simple-array (simple-array (complex single-float) 2) 2) a))
    (loop for i from 0 below w do
	(loop for j from 0 below h do
	     (let ((z (get-stored-array 0 pol cam (+ i (* w j))))) 
	       (dotimes (u 64)
		 (dotimes (v 64)
		   (setf (aref (aref a v u) j i) (aref z v u)))))))
   #+nil (dotimes (u 64)
      (dotimes (v 64)
	(setf (aref (aref a v u) 0 0) (* .1s0 (complex (* 1s0 v) u)))))
    (loop for u from x-offset below 64 do ;dotimes (u 64)
      (loop for v from y-offset below 64 do ; dotimes (v 64)
       (put-csf-image (aref a v u)
		      :w w :h h 
		      :dst-x (* (1+ w) (- u x-offset))
		      :dst-y (* (1+ h) (- v y-offset))
		      :scale 100s0 :offset 0s0 :fun #'abs)))))

(defun display-mosaic-pronounce-reflex (&key (w 16) (h 16) (x-offset 0) (y-offset 0) (cam 1))
  (loop for i from 0 below w do
       (loop for j from 0 below h do
	    (let ((z0 (get-stored-array 0 0 cam (min (* w h) (+ (min w (+ i x-offset)) 
								 (* w (min h (+ j y-offset)))))))
		  (z1 (get-stored-array 0 1 cam (min (* w h) (+ (min w (+ i x-offset)) 
								 (* w (min h (+ j y-offset)))))))
		  ) 
	  (put-csf-image (image-processing::.-csf z0 z1)
			 :w 64 :h 64 
			 :dst-x (* 65 i)
			 :dst-y (* 65 j)
			 :scale 100s0 :offset 0s0 :fun #'abs)))))

(defun display-sum-reflex (&key (w 16) (h 16) (x-offset 0) (y-offset 0) (cam 1))
  (let ((a (make-array (list 64 64) :element-type '(complex single-float))))
   (loop for i from 0 below w do
	(loop for j from 0 below h do
	     (let ((z0 (get-stored-array 0 0 cam (min (* w h) (+ (min w (+ i x-offset)) 
								 (* w (min h (+ j y-offset)))))))
		   (z1 (get-stored-array 0 1 cam (min (* w h) (+ (min w (+ i x-offset)) 
								 (* w (min h (+ j y-offset)))))))
		   )
	       (image-processing::^.+csf a (image-processing::.abs-csf (image-processing::.-csf z0 z1)))
	       )))
   (put-csf-image a  :w 64 :h 64 
		  :dst-x 0
		  :dst-y 0
		  :scale .06s0 :offset -600s0 :fun #'abs)))
#+nil
(display-sum-reflex :w 20 :h 20 :cam 1)

#+nil
(progn
  (arduino-dac 1600 2030)
  (acquire)
  (display-mosaic :start 30 :subtract-avg t))

#+nil
(calc-avg :start 7)
#+nil
(display-mosaic :start 70 :subtract-avg t :avg-start 30)
#+nil
(display-mosaic :pol 0 :start (+ 20 20 (* 40 20)) :subtract-avg nil)
#+nil
(display-mosaic-onecam :pol 0 :cam 1 :x-offset 0 :y-offset 0 :w 14 :h 14)
#+nil
(display-mosaic-pronounce-reflex :x-offset 7 :y-offset 10 :w 40 :h 40)

(defun calc-avg (&key (start 0) (end (get-stored-array-length)))
 (let ((avg (loop for i below 3 collect
		 (make-array (list 64 64) :element-type '(complex single-float)
			     :initial-element (complex 0s0))))
       (len (min end (get-stored-array-length))))
   (dotimes (cam 3)
     (let ((a (elt avg cam)))
       (loop for k from start below len do
	    (let ((b (get-stored-array 0 0 cam k)))
	      (dotimes (i 64)
		(dotimes (j 64)
		  (incf (aref a j i) (/ (aref b j i) (- len start)))))))
       (put-csf-image a :w 64 :h 64 :dst-x 0
		      :dst-y (* 65 cam)
		      :scale 2s0 :offset 0s0 :fun #'abs
		      )
       (put-csf-image a :w 64 :h 64 :dst-x 65
		      :dst-y (* 65 cam)
		      :scale (/ 255 (* 2 3.1415)) :offset 3.1416s0 :fun #'phase
		      )))
   (defparameter *avg* avg)))

#+nil
(draw-window 0 0 100 200)


(defun tukey-window (nn &key (alpha .9s0))
  "The Tukey window,[8][39] also known as the tapered cosine window,
can be regarded as a cosine lobe of width alpha N/2 that is convolved
with a rectangular window of width (1 - alpha/2)N. For alpha=0
rectangular, for alpha=1 Hann window."
  (declare (type (unsigned-byte 32) nn)
	   (type single-float alpha)
	   (values (simple-array single-float 1) &optional))
  (let ((w (make-array nn :element-type 'single-float :initial-element 0s0))
	(n-1 (- nn 1)))
    (dotimes (n nn)
      (setf (aref w n)
	    (cond ((<= 0s0 n (* alpha .5s0 n-1))
		   (* .5s0 (+ 1s0 (cos (* (coerce pi 'single-float) (- (/ (* 2s0 n)
						     (* alpha n-1)) 1s0))))))
		  ((<= (* alpha .5s0 n-1) n (* n-1 (- 1s0 (* .5s0 alpha))))
		   1.0s0)
		  ((<= (* n-1 (- 1 (* .5s0 alpha))) n n-1)
		   (* .5s0 (+ 1s0 (cos (* (coerce pi 'single-float) (+ (/ (* 2s0 n)
						     (* alpha n-1))
						  (/ -2.0s0 alpha)
						  1s0))))))
		  (t 0s0))))
    w))
#+nil
(tukey-window 64)




(defun tukey-window2 (&key (w 100) (h w) (alpha-x .2s0) (alpha-y alpha-x))
  (declare (type (unsigned-byte 32) w h)
	   (values (simple-array single-float 2) &optional))
  (let ((b (make-array (list h w) :element-type 'single-float))
	(wh (tukey-window h :alpha alpha-y))
	(ww (tukey-window w :alpha alpha-x)))
    (dotimes (j h)
      (dotimes (i w)
	(setf (aref b j i) (* (aref wh j)
			      (aref ww i)))))
    b))


(defparameter *win* nil)
(defun create-windows (&key (w 512) (h w) (alpha-x .2) (alpha-y alpha-x))
  (setf *win*
	(tukey-window2 :w w :h h :alpha-x alpha-x :alpha-y alpha-y))
  nil)


(create-windows)

(defun draw-frame (buf w h pol cam framenr x y &key (repetitions 1)
						 (repetition 0)
						 (extract-w 64) (extract-h extract-w) (scale #.(/ 20s0 4095)) (offset (- 12000s0)) (update-display-p nil))
  (declare (optimize (speed 3))
	   (type fixnum w h pol x y)
	   (type (integer 1 100000) extract-w extract-h)
	   (type (integer 0 1000000) framenr)
	   (type (integer 0 3) cam)
	   (type (integer 0 10000) repetition)
	   (type (integer 1 10000) repetitions)
	   (type (simple-array single-float 2) buf)
	   (type single-float scale offset))
  (let ((accum (elt *buf-s* cam)))
    (declare (type (simple-array single-float 2) accum))
    (cond
      ((or (= cam 2) (= cam 0))
       (let ((accum1 (sb-ext:array-storage-vector accum))
	     (buf1 (sb-ext:array-storage-vector buf)))
	(dotimes (i (* 256 256))
	  (incf (aref accum1 i) (aref buf1 i)))))
      ((= cam 1)
       (dotimes (i 512)
	 (dotimes (j 512)
	   (incf (aref accum j i) (aref buf j i)))))))
  (when (< repetition (- repetitions 1))
    (return-from draw-frame))


  #+nil (when update-display-p (put-sf-image (elt *buf-s* cam) w h :dst-x (cam-dst-x cam) ))
  (when (= cam 1) ;; premultiply only the camera with the reflected light with a tukey window
    (let ((current-buf-s (elt *buf-s* cam)))
      (declare (type (simple-array single-float 2) *win* current-buf-s))
      (dotimes (j 512)
	(dotimes (i 512)
	  (setf (aref current-buf-s j i) (* (aref *win* j i) (aref current-buf-s j i)))))))

  
  (put-sf-image (elt *buf-s* cam) w h :dst-x (cam-dst-x cam) :scale-max (* .3 repetitions 4095s0) )

  (let* ((a (make-array (list h w) :element-type 'single-float))
	 (b (elt *buf-s* cam))
	 (b1 (sb-ext:array-storage-vector b))
	 (a1 (sb-ext:array-storage-vector a))
	 (wa (floor extract-w 2))
	 (ha (floor extract-h 2)))
    (dotimes (i (* h w))
      (setf (aref a1 i) (aref b1 i)))
    (sb-sys:with-pinned-objects (a *buf-cs*)
      (let ((plan (fftw::rplanf a :out *buf-cs* :w w :h h :flag fftw::+measure+)))
	(fftw::%fftwf_execute plan)
	(put-csf-image *buf-cs*
		     :w (1+ (floor w 2)) :h h 
		     :x0 (- x wa 1) 
		     :y0 (- y ha 1)
		     :x1 (+ x wa) 
		     :y1 (+ y ha)
		     :dst-x (cam-dst-x cam) :dst-y 512 
		     :scale (/ scale 30s0) :offset (* offset)
		     )
	(fftw::%fftwf_destroy_plan plan))))
  
  (cond ;; the following transform from (elt *buf-s* cam) to buf-cs
    ((= 0 cam) (fftw::%fftwf_execute *plan256-0*))
    ((= 2 cam) (fftw::%fftwf_execute *plan256-2*))
    ((= 1 cam) (fftw::%fftwf_execute *plan512-1*)))
  
  (let ((accum (elt *buf-s* cam))) ;; reset the accumulation buffer
    (declare (type (simple-array single-float 2) accum))
    (dotimes (i 512)
      (dotimes (j 512)
	(setf (aref accum j i) 0s0))))
  #+nil
  (let ((wa (floor extract-w 2))
	(ha (floor extract-h 2)))
    (when t ;;update-display-p
      (put-csf-image *buf-cs*
		     :w (1+ (floor w 2)) :h h 
		     :x0 (- x wa 1) 
		     :y0 (- y ha 1)
		     :x1 (+ x wa) 
		     :y1 (+ y ha)
		     :dst-x (cam-dst-x cam) :dst-y 512 
		     :scale (/ scale 1s0) :offset 0s0 ;(* offset)
		     ))
    (extract-csf* *buf-cs* *buf-cs64in* 
		  :w (1+ (floor w 2)) :h h
		  :x  (- x wa 1) :y (- y ha 1)
		  :w-extract extract-w :h-extract extract-h))

  (let* ((a (get-stored-array 0 pol cam (floor framenr repetitions)))
	 (pixels1 (expt (cond ((or (= 0 cam) (= 2 cam)) 256)
			      ((= 1 cam) 512)
			      (t (error "unexpected value for camera index: ~a." cam)))
			1))
	 (pixels2 (* 64))
	 (s (/ 1s0 (* pixels1 pixels2))))
    (declare (type (simple-array (complex single-float) 2) a *buf-cs64out*  *buf-cs64in*)
	     (type fixnum pixels1 pixels2)
	     (type single-float s))
    (dotimes (i 64)
      (dotimes (j 64)
	(setf (aref a j i) (* s (expt -1 (+ i j)) (aref *buf-cs64in* j i))))))
  #+nil
  (progn
    (fftw::%fftwf_execute *plan64*)
    (let* ((a (get-stored-array 1 pol cam framenr)
	     #+nil (elt *store* *store-index*))
	   (pixels1 (expt (cond ((or (= 0 cam) (= 2 cam)) 256)
				((= 1 cam) 512)
				(t (error "unexpected value for camera index: ~a." cam)))
			  1))
	   (pixels2 (* 64))
	   (s (/ 1s0 (* pixels1 pixels2))))
      (declare (type (simple-array (complex single-float) 2) a *buf-cs64out*  *buf-cs64in*)
	       (type fixnum pixels1 pixels2)
	       (type single-float s))
      (dotimes (i 64)
	(dotimes (j 64)
	  (setf (aref a j i) (* s (expt -1 (+ i j)) (aref *buf-cs64out* j i)))))
      (when update-display-p 
	(put-csf-image a :w 64 :h 64 :dst-x (cam-dst-x cam) :dst-y (- 512 64) :scale 1s0 :offset 0s0)))))

#+nil
(get-stored-array 0 0 0 0)

(defun draw-rect (x1 y1 x2 y2)
  (draw-window x1 y1 x2 y1)
  (draw-window x1 y2 x2 y2)
  (draw-window x1 y1 x1 y2)
  (draw-window x2 y1 x2 y2))

;; measure center of the circle in the 64x64 area
;; cam 0 5 2 54 54
;; cam 1 3 5 58 58
;; cam 2 5 5 54 54

(+ 2 (/ 54 2))

2 0 1
(list 
 (/ (- 79801595406 8421881) 200e6)

 (/ (- 79802002987 8510280) 200e6)

 (/ (- 79801869366 8467775) 200e6))

(/ 3900 398.967)

(let ((last-presentation-time 0)
      (start 0))
  (defun start-acquisition-thread (&key (pol 0) (n 2000) (us-between-x11-updates 200000) (x11-display-p nil)
				     (repetitions 1))
    (setf last-presentation-time (get-us-time)
	  start last-presentation-time)
    (sb-thread:make-thread 
     #'(lambda ()
	 (reset-current-index)
	 (progn ;; sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop nil)
	   (dotimes (i n)
	     (dotimes (rep repetitions)
	      (let* ((current (get-us-time))
		     (do-update-p (< us-between-x11-updates (abs (- current last-presentation-time)) )))
		(dotimes (j 3)
		  (multiple-value-bind (cam success-p w h imagenr blockid timestamp value-min value-max) 
		      (pylon::grab-sf *cams* *buf-s-capture*)
		    (push (list  (- (get-us-time) start) cam success-p w h imagenr blockid timestamp value-min value-max
				 ) *log*)
		    (when success-p ;; do-update-p
		      (let ((k '((84 208) (230 172) (62 68))))
			(destructuring-bind (x y) (elt k cam)
			  (draw-frame *buf-s-capture* w h pol cam (1- imagenr) x y :extract-w 64
				      :repetitions repetitions
				      :repetition rep
				      :scale (/ 10s0 4095) :update-display-p (and do-update-p x11-display-p)
				      :offset (let ((o -12000s0)) (ecase cam 
								    (0 o)
								    (1 (* 2 o))
								    (2 o)))))))))
		(when do-update-p
		  (setf last-presentation-time current)))))))
     :name "camera-acquisition")))

#+nil
(trigger-flipmount-once)

#+nil
(/ 200 (/ (- 497273145 1333122) 125e6))
 
#+nil
(pylon:stop-grabbing *cams*)
#+nil
(pylon:start-grabbing *cams*)
#+nil
(progn 
  (setf
   *trigger-outputs-initialized* nil)
  (initialize-trigger-outputs))

#+nil
(require :sb-sprof)
#+nil
(sb-sprof:with-profiling (:max-samples 1000
                               :report :flat
                               :loop nil)
       (acquire))
#+nil
(time (acquire))
#+nil
(arduino-trigger t)
#+nil
(dotimes (i 100)
 (trigger-all-cameras-once ))
#+nil
(dotimes (i 200)
  (trigger-all-cameras-once))
(/ 400 (/ (- 1257702841 10743457) 125e6)) ; => with 24ms delay on arduino 40fps
(/ (- 957629656 9973955) 200e6)
(/ 400 7.8) ;; 51fps
(/ 400 (/ (- 957629656 9973955) 125e6)) ;; with 18ms delay 52.7fps
 #+nil
(list (- 3100 (* 15 85)) (- 3100 (* 15  70)))
(* 1000 (/ 54.3))

(/ (- 6486417746 20014) 125e6) ;; 51.8s seconds
;; overall time 108s
;; 400 frames in 51.8s
(/ 400 51.8s0) ;; 7.7fps

#+nil
(arduino-dac 2000 2000)

(defun acquire ()
  (let* ((n (get-stored-array-length))
	 (nx (sqrt n))
	 (ny (sqrt n))
	 (center-x 1825)
	 (center-y 2050)
	 (radius 1800)
	 (s0 (elt *buf-s* 0))
	 (s1 (elt *buf-s* 1))
	 (s2 (elt *buf-s* 2)))
    (sb-sys:with-pinned-objects (s0 s1 s2 *buf-s* *buf-cs* *buf-cs64in* *buf-cs64out*)
      (let ((*plan64* (fftw::planf *buf-cs64in* :out *buf-cs64out* 
				   :w 64 :h 64 :flag fftw::+measure+ 
				   :sign fftw::+backward+))
	    (*plan256-0* (fftw::rplanf (elt *buf-s* 0) :out *buf-cs* :w 256 :h 256 
				       :flag fftw::+measure+))
	    (*plan256-2* (fftw::rplanf (elt *buf-s* 1) :out *buf-cs* :w 256 :h 256 
				       :flag fftw::+measure+))
	    (*plan512-1* (fftw::rplanf (elt *buf-s* 2) :out *buf-cs* :w 512 :h 512 
				     :flag fftw::+measure+)))
	(unwind-protect 
	     (progn
	       (defparameter *log* nil)
	       (arduino-trigger t)
	       (reset-camera-timers *cams* 3)
	       (pylon:start-grabbing *cams*)
	       (let ((th (start-acquisition-thread :pol 0 :n n :x11-display-p nil)))
		 (sleep .02)
		 (trigger-all-cameras-seq n :delay-ms 18)
		 #+nil
		 (dotimes (i n)
		   (let ((ix (mod i nx))
			 (iy (floor i nx)))
		     (arduino-dac (floor (- center-x (* 2 radius (- (/ ix nx) 1/2))))
				  (floor (- center-y (* 2 radius (- (/ iy ny) 1/2))))))
		   (sleep .02)
		   (trigger-all-cameras-seq n))
		 #+nil (dotimes (i 30)
			 (trigger-all-cameras-once)
			 (sleep .01))
		 (sb-thread:join-thread th)))
	  (pylon:stop-grabbing *cams*))
	;; stop and restart grabbing so that image numbers start from 1 again
	(trigger-flipmount-once)
	#+nil
	(unwind-protect 
	     (progn
	       ;;(defparameter *log* nil)
	       (arduino-trigger t)
	       (reset-camera-timers *cams* 3)
	       (pylon:start-grabbing *cams*)
	       ;;(reset-camera-timers *cams* 3)
	       (let ((th (start-acquisition-thread :pol 1 :n n)))
		 (sleep .02)
		 (trigger-all-cameras-seq n)
		 #+nil (dotimes (i n)
			 (let ((ix (mod i nx))
			       (iy (floor i nx)))
			   (arduino-dac (floor (- center-x (* 2 radius (- (/ ix nx) 1/2))))
					(floor (- center-y (* 2 radius (- (/ iy ny) 1/2))))))
			 (sleep .02)
			 (trigger-all-cameras-once))
		 (sb-thread:join-thread th)))
	  (pylon:stop-grabbing *cams*))
	(progn 
	  (fftw::%fftwf_destroy_plan *plan64*)
	  (fftw::%fftwf_destroy_plan *plan256-0*)
	  (fftw::%fftwf_destroy_plan *plan256-2*)
	  (fftw::%fftwf_destroy_plan *plan512-1*))))))

(defun trigger-all-cameras-seq-2d-scan ( &key 
					   (starti (- 2000 500)) (startj (- 2000 500))
					   (maxi (+ 2000 500)) (maxj (+ 2000 500))
					   (stepi/4 10)
					   (stepj 10)
					   (delay/4-ms 5)
					   (line-delay-ms 100))
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
  (format t "scan ~a" (list (list starti maxi stepi/4)
			  (list startj maxj stepj)
			  'time (/ (+ (* (/ (- maxj startj) stepj) 
					 (/ (- maxi starti) (* 4 stepi/4))
					 (* delay/4-ms))
				      (* (/ (- maxj startj) stepj) line-delay-ms))
				   1000s0))
	  )
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   (format nil 
	   "(progn
  (set 'i ~a)
  (set 'j ~a)
  (while (< j ~a)
    (while (< i ~a)
      (dac i j)
      (set 'i (+ i ~a)) (delay ~a) (dac i j)
      (set 'i (+ i ~a)) (delay ~a) (dac i j)
      (set 'i (+ i ~a)) (delay ~a) (dac i j)
      (delay ~a)
      (digital-write 11 1)
      (digital-write 12 1) 
      (digital-write 10 1) 
      (delay 1)
      (digital-write 11 0)
      (digital-write 12 0) 
      (digital-write 10 0)
      (set 'i (+ i ~a)))
    (set 'i ~a)
    (set 'j (+ j ~a))
    (dac i j)
    (delay ~a)))"
	   starti startj
	   maxj maxi
	   stepi/4 delay/4-ms
           stepi/4 delay/4-ms
	   stepi/4 delay/4-ms
	   delay/4-ms
	   stepi/4
	   starti
	   stepj
	   line-delay-ms)
   :time (+ .4s0 (/ (+ (* (/ (- maxj startj) stepj) 
					 (/ (- maxi starti) (* 4 stepi/4))
					 (* delay/4-ms))
				      (* (/ (- maxj startj) stepj) line-delay-ms))
		    1000s0))))

#+nil
(let ((count 10))
  (loop while (< 0 count) collect (progn (decf count) count)))

(defun trigger-all-cameras-seq-2d-scan-with-repetition ( &key 
					   (starti (- 2000 500)) (startj (- 2000 500))
					   (maxi (+ 2000 500)) (maxj (+ 2000 500))
					   (stepi/4 10)
					   (stepj 10)
					   (repetitions 1)
					   (delay/4-ms 5)
					   (line-delay-ms 100))
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
  (format t "scan ~a" (list (list starti maxi stepi/4)
			  (list startj maxj stepj)
			  'time (/ (+ (* (/ (- maxj startj) stepj) 
					 (/ (- maxi starti) (* 4 stepi/4))
					 (* delay/4-ms))
				      (* (/ (- maxj startj) stepj) line-delay-ms))
				   1000s0))
	  )
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   (format nil 
	   "(progn
  (set 'i ~a)
  (set 'j ~a)
  (while (< j ~a)
    (while (< i ~a)
      (dac i j)
      (set 'i (+ i ~a)) (delay ~a) (dac i j)
      (set 'i (+ i ~a)) (delay ~a) (dac i j)
      (set 'i (+ i ~a)) (delay ~a) (dac i j)
      (delay ~a)
      (set 'count ~a)
      (while (< 0 count)
        (set 'count (- count 1))
        (digital-write 11 1)
        (digital-write 12 1) 
        (digital-write 10 1)
        (delay 1)
        (digital-write 11 0)
        (digital-write 12 0) 
        (digital-write 10 0)
        (delay ~a))
      (set 'i (+ i ~a)))
    (set 'i ~a)
    (set 'j (+ j ~a))
    (dac i j)
    (delay ~a)))"
	   starti startj
	   maxj maxi
	   stepi/4 delay/4-ms
           stepi/4 delay/4-ms
	   stepi/4 delay/4-ms
	   delay/4-ms
	   repetitions
	   (* 4 delay/4-ms)
	   stepi/4
	   starti
	   stepj
	   line-delay-ms)
   :time (+ .4s0 (/ (+ (* (/ (- maxj startj) stepj) 
			  (/ (- maxi starti) (* 4 stepi/4))
			  (* delay/4-ms repetitions))
		       (* (/ (- maxj startj) stepj) line-delay-ms))
		    1000s0))))

(defun acquire-2d (&key (x11-display-p nil) (repetitions 1))
  (let* ((n (get-stored-array-length))
	 (nx (floor (sqrt n)))
	 (ny (floor (sqrt n)))
	 (reps repetitions)
	 (center-x 1825)
	 (center-y 2050)
	 (radius 1800)
	 (s0 (elt *buf-s* 0))
	 (s1 (elt *buf-s* 1))
	 (s2 (elt *buf-s* 2)))
    (progn 
      (setf
       *trigger-outputs-initialized* nil)
      (initialize-trigger-outputs))
    (sb-sys:with-pinned-objects (s0 s1 s2 *buf-cs* *buf-cs64in* *buf-cs64out*)
      (let ((*plan64* (fftw::planf *buf-cs64in* :out *buf-cs64out* 
				   :w 64 :h 64 :flag fftw::+measure+ 
				   :sign fftw::+backward+))
	    (*plan256-0* (fftw::rplanf s0 :out *buf-cs* :w 256 :h 256 :flag fftw::+measure+))
	    (*plan256-2* (fftw::rplanf s2 :out *buf-cs* :w 256 :h 256 :flag fftw::+measure+))
	    (*plan512-1* (fftw::rplanf s1 :out *buf-cs* :w 512 :h 512 :flag fftw::+measure+)))
	(macrolet ((do-trigger ()
		     `(let* ((ci 1700)
			     (cj 2200)
			     (stepi/4 15)
			     (stepj (* 4 stepi/4)))
			(trigger-all-cameras-seq-2d-scan-with-repetition
			 :starti (- ci (* (floor nx 2) (* 4 stepi/4)))
			 :startj (- cj (* (floor ny 2) stepj))
			 :maxi (+ ci (* (floor nx 2) (* 4 stepi/4)))
			 :maxj (+ cj (* (floor ny 2) stepj))
			 :stepi/4 stepi/4
			 :stepj stepj
			 :repetitions reps
			 :line-delay-ms 30
			 :delay/4-ms 5))))
	 (unwind-protect 
	      (progn
		(defparameter *log* nil)
		(arduino-trigger t)
	       
		(reset-camera-timers *cams* 3)
		(pylon:start-grabbing *cams*)
		(let ((th (start-acquisition-thread :pol 0 :n n :x11-display-p x11-display-p  :repetitions reps)))
		  (sleep .02)
		  (do-trigger)
		  (sb-thread:join-thread th)))
	   (pylon:stop-grabbing *cams*))
	 (trigger-flipmount-once)
	 (unwind-protect 
	      (progn
		(reset-camera-timers *cams* 3)
		(pylon:start-grabbing *cams*)
		(let ((th (start-acquisition-thread :pol 1 :n n :x11-display-p x11-display-p :repetitions reps)))
		  (sleep .02)
		  (do-trigger)
		  (sb-thread:join-thread th)))
	   (pylon:stop-grabbing *cams*)))
	(progn 
	  (fftw::%fftwf_destroy_plan *plan64*)
	  (fftw::%fftwf_destroy_plan *plan256-0*)
	  (fftw::%fftwf_destroy_plan *plan256-2*)
	  (fftw::%fftwf_destroy_plan *plan512-1*))))))


#+nil
(let* ((n (* 32 32)  #+nil (get-stored-array-length))
       (nx (floor (sqrt n) 4))
       (ny (floor (sqrt n) 4))
       (center-x 1825)
       (center-y 2050)
       (radius 1800)
       (ci 1700)
       (cj 2200)
       (stepi/4 10)
       (stepj (* 4 stepi/4)))
  (trigger-all-cameras-seq-2d-scan-with-repetition
   :repetitions 40
   :starti (- ci (* (floor nx 2) stepi/4 4))
   :startj (- cj (* (floor ny 2) stepj))
   :maxi (+ ci (* (floor nx 2) 4 stepi/4))
   :maxj (+ cj (* (floor ny 2) stepj))
   :stepi/4 stepi/4
   :stepj stepj
   :line-delay-ms 10
   :delay/4-ms 5))

#+nil
(progn
  (pure-x11::clear-area)
  (dotimes (pol 2)
   (loop for i from  0 below 32 by 1 do 
	(loop for j from 0 below 32 by 1 do
	     (destructuring-bind (v y x) (get-global-maximum-position :x i :y j :cam 1 :ft 0 :pol pol :x-offset 0
								      :y-offset 0)
	       (setf x (+ 20 (* 10 x)))
	       (setf y (+ 20 (* 10 y)))
	       (when (< 140 v)
		 (let ((q 2)
		       (p 0))
		   (draw-window (max 0 x) (max 0 (- y q)) x (max 0 (- y p)))
		   (draw-window (max 0 x) (+ y q) x (+ y p))
		   (draw-window (max 0 (- x q)) y (max 0 (- x p)) y)
		   (draw-window (max 0 (+ x q)) y (+ x p) y))))))))



#+nil
(with-open-file (s "/dev/shm/o.dat" :direction :output :if-does-not-exist :create
		   :if-exists :supersede)
  (let ((res nil))
    (loop for i below 32 collect
	 (loop for j below 32 collect
	      (push (first (get-global-maximum-position :x i :y j :cam 1 :ft 0 :pol 0 :x-offset 0
							:y-offset 0))
		    res)))
    ;; values from 5000 to 7
    ;; threshold seems to be at element 600 at around 20 or 30
    (loop for e in (sort res #'>) and i from 0 do
	 (format s "~a ~a~%" i e)))) 

#+nil
(progn
  (pure-x11::clear-area)
 (display-mosaic-onecam :ft 0 :pol 0 :cam 1
			:x-offset 0 :y-offset 34 :w 16 :h 16
			:scale .1s0 :offset (* 0 -6.0s0)
			:mark-global-maxima-p NIL :global-threshold 100s0))
#+nil
(display-mosaic-onecam :ft 1 :pol 0 :cam 1 :x-offset 0 :y-offset 0 :w 32 :h 32 :scale 100s0)
#+nil
(display-mosaic-onecam-swap :pol 0 :cam 1 :x-offset 0 :y-offset 0 :w 16 :h 16)
#+nil
(acquire-2d :x11-display-p nil :repetitions 30)
#+nil
(acquire-2d :x11-display-p t :repetitions 10)
#+nil
(let ((a (make-array (list 64 64 2 3 64 64) :element-type '(complex single-float)
		     )))
  (ics:write-ics2 (format nil "/dev/shm/o.ics") a))

#+nil
(block-laser)
#+nil
(unblock-laser)

(defun block-laser ()
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) (first *ard*)
   "(progn (pin-mode 8 1) (digital-write 8 0))"))

(defun unblock-laser ()
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) (first *ard*)
   "(digital-write 8 1)"))

#+nil
(let* ((n (get-stored-array-length))
      (mw (floor (sqrt n)))
      (mh (floor (sqrt n))))
 (dotimes (pol 2)
   (let ((a (make-array (list mh mw 3 64 64) :element-type '(complex single-float))))
     (dotimes (j mh)
       (dotimes (i mw)
	 (dotimes (cam 3)
	   (let ((b (get-stored-array 0 pol cam (+ i (* mw j)))))
	     (dotimes (y 64)
	       (dotimes (x 64)
		 (setf (aref a j i cam y x) (aref b y x))))))))
     (ics:write-ics2 (format nil "/dev/shm/o-pol~1d.ics" pol) a))))



#+nil
(/ (* 64 64 64 64 2 3 2 4) (* 1024 1024s0))
