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
  (asdf:load-system "image-processing"))


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
(arduino-serial-sbcl:close-serial (second *ard*))

(defvar *trigger-outputs-initialized* nil)

(defun initialize-trigger-outputs ()
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   "(progn
 (pin-mode 6 1)
 (pin-mode 10 1)
 (pin-mode 11 1)
 (pin-mode 12 1))")
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

#+nil
(arduino-serial-sbcl:talk-arduino
   ( second *ard*) 
   (first *ard*)
   "(progn (+ 1 2))")
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

(let* ((n 200)
       (store (loop for i from 0 below n collect
		   (loop for cam below 3 collect
			(make-array (list 64 64) :element-type '(complex single-float)))))
       (current-index 0))
  (defun get-stored-array (cam &optional (index current-index index-p))
    (declare (values (simple-array (complex single-float) 2) &optional))
    (format t "storing in index ~a~%" index)
    (prog1
	(elt (elt store (mod index (length store))) cam)
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


(defun display-mosaic (&key (start 0) (end (+ start 15)) (subtract-avg nil))
  (when subtract-avg (calc-avg))
  (let ((scale 1s0)
       (offset 70s0))
   (progn 
     ;(draw-window 0 0 100 100)
     (dotimes (cam 3)
       (loop for i from start below (min end (get-stored-array-length)) do
	 (let ((z (if subtract-avg
		      (image-processing::.-csf (get-stored-array cam i) (elt *avg* cam))
		      (get-stored-array cam i)))
	       (si (- i start))) 
	   (put-csf-image z
			  :w 64 :h 64 :dst-x (* 65 si)
			  :dst-y (* 65 cam)
			  :scale (/ 255 (* 2 3.1415)) :offset 3.1416s0 :fun #'phase)
	   (put-csf-image z
			  :w 64 :h 64 :dst-x (* 65 si)
			  :dst-y (+ (* 3 65) (* 65 cam))
			  :scale scale :offset 0s0 :fun #'abs)
	   (put-csf-image z
			  :w 64 :h 64 :dst-x (* 65 si)
			  :dst-y (+ (* 6 65) (* 65 cam))
			  :scale scale :offset offset :fun #'realpart)
	   (put-csf-image z
			  :w 64 :h 64 :dst-x (* 65 si)
			  :dst-y (+ (* 9 65) (* 65 cam))
			  :scale scale :offset offset :fun #'imagpart)))))))
#+nil
(progn
  (arduino-dac 1600 2030)
  (acquire)
  (display-mosaic :start 30 :subtract-avg t))

#+nil
(display-mosaic :start 40 :subtract-avg nil)

(defun calc-avg ()
 (let ((avg (loop for i below 3 collect
		 (make-array (list 64 64) :element-type '(complex single-float)
			     :initial-element (complex 0s0))))
       (len (get-stored-array-length)))
   (dotimes (cam 3)
     (let ((a (elt avg cam)))
       (loop for k from 0 below len do
	    (let ((b (get-stored-array cam k)))
	      (dotimes (i 64)
		(dotimes (j 64)
		  (incf (aref a j i) (/ (aref b j i) len))))))
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
(defun draw-frame (buf w h cam framenr x y &key (extract-w 64) (extract-h extract-w) (scale #.(/ 20s0 4095)) (offset (- 12000s0)))
  (put-sf-image buf w h :dst-x (cam-dst-x cam) )
  (cond ((or (= 0 cam) (= 2 cam)) (fftw::%fftwf_execute *plan256*))
	((= 1 cam) (fftw::%fftwf_execute *plan512*)))
  (let ((wa (floor extract-w 2))
	(ha (floor extract-h 2)))
   (put-csf-image *buf-cs* :w (1+ (floor w 2)) :h h 
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
  
  (fftw::%fftwf_execute *plan64*)
  (let* ((a (get-stored-array cam framenr)
	   #+nil (elt *store* *store-index*))
	 (pixels1 (expt (cond ((or (= 0 cam) (= 2 cam)) 256)
			      ((= 1 cam) 512)
			      (t (error "unexpected value for camera index: ~a." cam)))
			1))
	 (pixels2 (* 64))
	 (s (/ (* pixels1 pixels2))))
    (declare (type (simple-array (complex single-float) 2) a *buf-cs64out*  *buf-cs64in*))
    #+nil (setf *store-index* (mod (1+ *store-index*)
			     (length *store*)))
    (dotimes (i 64)
      (dotimes (j 64)
	(setf (aref a j i) (* s (expt -1 (+ i j)) (aref *buf-cs64out* j i)))))
    (put-csf-image a :w 64 :h 64 :dst-x (cam-dst-x cam) :dst-y (- 512 64) :scale 1s0 :offset 0s0)))

(defun draw-rect (x1 y1 x2 y2)
  (draw-window x1 y1 x2 y1)
  (draw-window x1 y2 x2 y2)
  (draw-window x1 y1 x1 y2)
  (draw-window x2 y1 x2 y2))

(let ((last-presentation-time 0)
      (start 0))
  (defun start-acquisition-thread (&key (n 2000) (us-between-x11-updates 200000))
    (setf last-presentation-time (get-us-time)
	  start last-presentation-time)
    (sb-thread:make-thread 
     #'(lambda ()
	 (reset-current-index)
	 (dotimes (i n)
	   (let* ((current (get-us-time))
		  (do-update-p (< (- current last-presentation-time) us-between-x11-updates)))
	     (dotimes (j 3)
	       (multiple-value-bind (cam success-p w h framenr timestamp) 
		   (pylon::grab-sf *cams* *buf-s*)
		 (push (list  (- (get-us-time) start) cam success-p w h framenr timestamp) *log*)
		 (when do-update-p
		   (let ((k '((84 208) (230 172) (62 68))))
		    (destructuring-bind (x y) (elt k cam)
		      (draw-frame *buf-s* w h cam (1- framenr) x y :extract-w 64 
				  :scale (/ 40s0 4095) :offset (let ((o -12000)) (ecase cam 
										  (0 o)
										  (1 (* 2 o))
										  (2 o))))
		      #+nil (draw-rect ))))))
	     (when do-update-p
	       (setf last-presentation-time current)))))
     :name "camera-acquisition")))


#+nil
(acquire)
(defun acquire ()
 (let ((n (get-stored-array-length)))
   (unwind-protect 
	(progn
	  (defparameter *log* nil)
	  (reset-camera-timers *cams* 3)
	  (arduino-trigger t)
	  (pylon:start-grabbing *cams*)
	  (let ((th (start-acquisition-thread :n n)))
	    (sleep .001)
	    (dotimes (i n)
	      (arduino-dac 1600 (- 3100 (* 15 i)))
	      (trigger-all-cameras-once))
	    (sb-thread:join-thread th)))
     (pylon:stop-grabbing *cams*))))
