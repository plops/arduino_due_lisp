(declaim (optimize (debug 3) (safety 3)))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry*
	'(*default-pathname-defaults*
	  #p"/home/martin/arduino_due_lisp/arduino-serial-sbcl/"
	  #p"/home/martin/arduino_due_lisp/pylon/"
	  #p"/home/martin/arduino_due_lisp/image-processing/"
	  #p"/home/martin/stage/cl-pure-x11/"
	  #p"/home/martin/stage/cl-cffi-fftw3/"
	  #p"/home/martin/arduino_due_lisp/image-processing/"))
  (asdf:load-system "pylon")
  (asdf:load-system "arduino-serial-sbcl")
  (asdf:load-system "pure-x11")
  (asdf:load-system "fftw")
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
   "(dac 1600 2120)")
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

;; => ((21433565 1 1 256 256 783 342   70 0 125000000 1500 :TRIGGER-MODE 0 :LAST-ERROR 0 :RATE-P 0 :REVERSE-X 0 :RATE 105.82011)
;;     (21433566 1 1 512 512 789 112 2975 0 125000000 1500 :TRIGGER-MODE 0 :LAST-ERROR 0 :RATE-P 0 :REVERSE-X 0 :RATE 54.318306)
;;     (21433540 1 1 256 256 996 439   35 0 125000000 1500 :TRIGGER-MODE 0 :LAST-ERROR 0 :RATE-P 0 :REVERSE-X 1 :RATE 105.82011))


(progn ;; open a window and draw a line
  (connect)
  (make-window :width (+ 512 256 256) :height (+ 512 512 64))
  (draw-window 0 0 100 100))

(defun put-sf-image (a w h &key (dst-x 0) (dst-y 0))
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
	 (scale (/ 255s0 4095)))
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

(defparameter *buf-s* (make-array (list 512 512) :element-type 'single-float))
(defparameter *buf-cs* (make-array (list 512 512) :element-type '(complex single-float)))
(defparameter *buf-cs64in* (make-array (list 64 64) :element-type '(complex single-float)))
(defparameter *buf-cs64out* (make-array (list 64 64) :element-type '(complex single-float)))

(fftw::%fftwf_import_wisdom_from_filename "fiberholo.fftwf.wisdom")
(time
 (progn 
   (progn (fftw::rftf *buf-s* :out-arg *buf-cs* :w 256 :h 256 :flag fftw::+patient+) nil)
   (progn (fftw::rftf *buf-s* :out-arg *buf-cs* :w 512 :h 512 :flag fftw::+patient+) nil)))
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

(defparameter *plan256* (fftw::rplanf *buf-s* :out *buf-cs* :w 256 :h 256 :flag fftw::+measure+))
(defparameter *plan512* (fftw::rplanf *buf-s* :out *buf-cs* :w 512 :h 512 :flag fftw::+measure+))

(defun cam-dst-x (cam)
  (ecase cam
    (0 0)
    (1 256)
    (2 (+ 512 256))))

(defparameter *store* (loop for i from 0 below 10 collect (make-array (list 64 64) :element-type '(complex single-float))))
(defparameter *store-index* 0)

#+nil
(/ (* (expt 70 2) 64 64 3 2 2 8) (* 1024 1024s0)) 

(let* ((n (* 30 30))
       (store (loop for i from 0 below n collect
		   (loop for cam below 3 collect
			(loop for pol below 2 collect
			     (loop for ft below 2 collect 
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


(defun display-mosaic-onecam (&key (w 16) (h 16) (x-offset 0) (y-offset 0) (cam 1) (pol 0))
  (loop for i from 0 below w do
       (loop for j from 0 below h do
	    (let ((z (get-stored-array 0 pol cam (min (* w h) (+ (min w (+ i x-offset)) 
								 (* w (min h (+ j y-offset)))))))
		  ) 
	  (put-csf-image z
			 :w 64 :h 64 
			 :dst-x (* 65 i)
			 :dst-y (* 65 j)
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
(defun draw-frame (buf w h pol cam framenr x y &key (extract-w 64) (extract-h extract-w) (scale #.(/ 20s0 4095)) (offset (- 12000s0)))
  (declare (optimize (speed 3))
	   (type fixnum w h pol cam framenr x y extract-w extract-h)
	   (type single-float scale offset))
  #+nil (put-sf-image buf w h :dst-x (cam-dst-x cam) )
  (cond ((or (= 0 cam) (= 2 cam)) (fftw::%fftwf_execute *plan256*))
	((= 1 cam) (fftw::%fftwf_execute *plan512*)))
  (let ((wa (floor extract-w 2))
	(ha (floor extract-h 2)))
    #+nil (put-csf-image *buf-cs* :w (1+ (floor w 2)) :h h 
		  :x0 (- x wa 1) 
		  :y0 (- y ha 1)
		  :x1 (+ x wa) 
		  :y1 (+ y ha)
		  :dst-x (cam-dst-x cam) :dst-y 512 
		  :scale scale :offset offset)
   (extract-csf* *buf-cs* *buf-cs64in* 
		 :w (1+ (floor w 2)) :h h
		 :x  (- x wa 1) :y (- y ha 1)
		 :w-extract extract-w :h-extract extract-h))

  (let* ((a (get-stored-array 0 pol cam framenr))
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
    #+nil (setf *store-index* (mod (1+ *store-index*)
			     (length *store*)))
    (dotimes (i 64)
      (dotimes (j 64)
	(setf (aref a j i) (* s (expt -1 (+ i j)) (aref *buf-cs64out* j i)))))
    #+nil (put-csf-image a :w 64 :h 64 :dst-x (cam-dst-x cam) :dst-y (- 512 64) :scale 1s0 :offset 0s0)))



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
  (defun start-acquisition-thread (&key (pol 0) (n 2000) (us-between-x11-updates 200000))
    (setf last-presentation-time (get-us-time)
	  start last-presentation-time)
    (sb-thread:make-thread 
     #'(lambda ()
	 (reset-current-index)
	 (progn ;; sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop nil)
	   (dotimes (i n)
	     (let* ((current (get-us-time))
		    (do-update-p (< (- current last-presentation-time) us-between-x11-updates)))
	       (dotimes (j 3)
		 (multiple-value-bind (cam success-p w h imagenr blockid timestamp) 
		     (pylon::grab-sf *cams* *buf-s*)
		   (push (list  (- (get-us-time) start) cam success-p w h imagenr blockid timestamp) *log*)
		   (when success-p ;; do-update-p
		     (let ((k '((84 208) (230 172) (62 68))))
		       (destructuring-bind (x y) (elt k cam)
			 (draw-frame *buf-s* w h pol cam (1- imagenr) x y :extract-w 64 
				     :scale (/ 40s0 4095) :offset (let ((o -12000s0)) (ecase cam 
											(0 o)
											(1 (* 2 o))
											(2 o)))))))))
	       (when do-update-p
		 (setf last-presentation-time current))))))
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
	 (radius 1800))
    (sb-sys:with-pinned-objects (*buf-s* *buf-cs* *buf-cs64in* *buf-cs64out*)
      (let ((*plan64* (fftw::planf *buf-cs64in* :out *buf-cs64out* 
				   :w 64 :h 64 :flag fftw::+measure+ 
				   :sign fftw::+backward+))
	    (*plan256* (fftw::rplanf *buf-s* :out *buf-cs* :w 256 :h 256 
				     :flag fftw::+measure+))
	    (*plan512* (fftw::rplanf *buf-s* :out *buf-cs* :w 512 :h 512 
				     :flag fftw::+measure+)))
	(unwind-protect 
	     (progn
	       (defparameter *log* nil)
	       (arduino-trigger t)
	       (reset-camera-timers *cams* 3)
	       (pylon:start-grabbing *cams*)
	       (let ((th (start-acquisition-thread :pol 0 :n n)))
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
	  (fftw::%fftwf_destroy_plan *plan256*)
	  (fftw::%fftwf_destroy_plan *plan512*))))))

(defun trigger-all-cameras-seq-2d-scan ( &key 
					   (starti (- 2000 500)) (startj (- 2000 500))
					   (maxi (+ 2000 500)) (maxj (+ 2000 500))
					   (stepi 50)
					   (stepj 50)
					   (delay-ms 18)
					   (line-delay-ms 100))
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
  (format t "scan ~a" (list (list starti maxi stepi)
			  (list startj maxj stepj)
			  'time (/ (+ (* (/ (- maxj startj) stepj) 
					 (/ (- maxi starti) stepi)
					 delay-ms)
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
	   delay-ms
	   stepi starti stepj
	   line-delay-ms)
   :time (+ .4s0 (/ (+ (* (/ (- maxj startj) stepj) 
		   (/ (- maxi starti) stepi)
		   delay-ms)
		(* (/ (- maxj startj) stepj) line-delay-ms))
	     1000s0)))))


(defun acquire-2d ()
  (let* ((n (get-stored-array-length))
	 (nx (sqrt n))
	 (ny (sqrt n))
	 (center-x 1825)
	 (center-y 2050)
	 (radius 1800))
    (progn 
      (setf
       *trigger-outputs-initialized* nil)
      (initialize-trigger-outputs))
    (sb-sys:with-pinned-objects (*buf-s* *buf-cs* *buf-cs64in* *buf-cs64out*)
      (let ((*plan64* (fftw::planf *buf-cs64in* :out *buf-cs64out* 
				   :w 64 :h 64 :flag fftw::+measure+ 
				   :sign fftw::+backward+))
	    (*plan256* (fftw::rplanf *buf-s* :out *buf-cs* :w 256 :h 256 
				     :flag fftw::+measure+))
	    (*plan512* (fftw::rplanf *buf-s* :out *buf-cs* :w 512 :h 512 
				     :flag fftw::+measure+)))
	(unwind-protect 
	     (progn
	       (defparameter *log* nil)
	       (arduino-trigger t)
	       
	       (reset-camera-timers *cams* 3)
	       (pylon:start-grabbing *cams*)
	       (let ((th (start-acquisition-thread :pol 0 :n n)))
		 (sleep .02)
		 (let* ((ci 1700)
			(cj 2200)
			(stepi 110)
			(stepj stepi))
		   (trigger-all-cameras-seq-2d-scan :starti (- ci (* (floor nx 2) stepi))
						    :startj (- cj (* (floor ny 2) stepj))
						    :maxi (+ ci (* (floor nx 2) stepi))
						    :maxj (+ cj (* (floor ny 2) stepj))
						    :stepi stepi
						    :stepj stepj
						    :line-delay-ms 100
						    :delay-ms 18))
		 :name "arduino-trigger"
		 
		 (sb-thread:join-thread th)))
	  (pylon:stop-grabbing *cams*))

	(progn 
	  (fftw::%fftwf_destroy_plan *plan64*)
	  (fftw::%fftwf_destroy_plan *plan256*)
	  (fftw::%fftwf_destroy_plan *plan512*))))))

#+nil
(display-mosaic-onecam :pol 0 :cam 0 :x-offset 0 :y-offset 0 :w 30 :h 30)
#+nil
(acquire-2d)
#+nil
(trigger-all-cameras-seq-2d-scan)
#+nil
(trigger-all-cameras-seq 200)
#+nil
(let* ((n (get-stored-array-length))
       (nx (sqrt n))
       (ny (sqrt n))
       (ci 2000)
       (cj 2000)
       (stepi 10)
       (stepj stepi))
  (trigger-all-cameras-seq-2d-scan :starti (- ci (* (floor nx 2) stepi))
				   :startj (- cj (* (floor ny 2) stepj))
				   :maxi (+ ci (* (floor nx 2) stepi))
				   :maxj (+ cj (* (floor ny 2) stepj))
						    :stepi stepi
						    :stepj stepj
						    :delay-ms 18))
