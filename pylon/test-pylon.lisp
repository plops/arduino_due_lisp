;; you have to make sure that the following environment variables are
;; set when starting the common lisp interpreter:
;; export PYLON_ROOT=${HOME}/pylon-3.2.1-x86_64/pylon3
;; export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${PYLON_ROOT}/genicam/bin/Linux64_x64
;; export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${PYLON_ROOT}/lib64
;; export GENICAM_ROOT_V2_3=${PYLON_ROOT}/genicam
;; export PYLON_CAMEMU=2

#-sbcl
(load "~/quicklisp/setup.lisp")

;(ql:quickload "cffi")
;(ql:quickload "trivial-garbage")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry*
	'(*default-pathname-defaults*
	  #p"/home/martin/arduino_due_lisp/arduino-serial-sbcl/"
	  #p"/home/martin/stage/cl-cffi-fftw3/"
	  #p"/home/martin/stage/cl-ics/"
	  #p"/home/martin/arduino_due_lisp/pylon/"
	  #p"/home/martin/arduino_due_lisp/image-processing/"))
  (asdf:load-system "fftw")
  (asdf:load-system "pylon")
  (asdf:load-system "ics")
  (asdf:load-system "arduino-serial-sbcl")
  (asdf:load-system "image-processing"))


(defpackage :pylon-test
  (:use :cl :cffi :image-processing))

(in-package :pylon-test)

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

#+nil
(initialize-trigger-outputs)

#+nil
(arduino-serial-sbcl:talk-arduino
   ( second *ard*) 
   (first *ard*)
   "(progn (+ 1 2))")
#+nil
(arduino-serial-sbcl:talk-arduino
   ( second *ard*) 
   (first *ard*)
   "(dac 1600 2500)")

#+nil
(loop for r from 100 below 400 by 20 do ;dotimes (seq 10)
  (let ((n 420)
	;(r 500d0)
	) 
  (dotimes (i n)
    (let ((z (+ (complex 1550d0 2500) (* r (exp (complex 0d0 (* 2d0 pi i (/ 1d0 n)))))) ))
      ;(sleep .01)
      (arduino-serial-sbcl:talk-arduino
       (second *ard*) 
       (first *ard*)
       (format nil "(dac ~d ~d)" 
	       (floor (realpart z))
	       (floor (imagpart z))))))))




#+nil
(loop for i from 1000 below 4000 by 10
     do
     (sleep .05)
     (format t "~a~%" i)
     (arduino-serial-sbcl:talk-arduino
      ( second *ard*) 
      (first *ard*)
      
      (format nil "(dac 1550 ~a)"
	      i)))

#+nil
(arduino-serial-sbcl:talk-arduino
   ( second *ard*) 
   (first *ard*)
   "(set 'setq (macro (name val)
                  (list set (list quote name) val)))")

#+nil
(arduino-serial-sbcl:talk-arduino
   ( second *ard*) 
   (first *ard*)
   "(room)")

#+nil
(arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   "(progn 
  (set 'i 0)
  (while (< i 1000) 
    (print i)
    (set 'i (+ i 1))))"
   :time .1d0)

#+nil
(arduino-serial-sbcl::upload-lisp-system (second *ard*) (first *ard*))

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


(defun trigger-all-cameras-seq-2d-scan ( &key 
					   (starti 400) (startj 1800)
					   (maxi 2900) (maxj 3700)
					   (stepi 100)
					   (stepj 100)
					   (delay-ms 24)
					   (line-delay-ms 100))
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
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
    (delay ~a)
    (dac i j)))"
	   starti startj
	   maxj maxi
	   delay-ms
	   stepi starti stepj
	   line-delay-ms)
   :time 8d0))



(defun trigger-all-cameras-seq-1d-scan (j &key 
					   (starti 400)
					   (maxi 2900) 
					   (stepi 100)
					   (delay-ms 24))
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   (format nil 
	   "(progn
  (set 'i ~a)
  (while (< i ~a)
    (dac i ~a)
    (delay ~a)
    (digital-write 11 1)
    (digital-write 12 1) 
    (digital-write 10 1) 
    (delay 1)
    (digital-write 11 0)
    (digital-write 12 0) 
    (digital-write 10 0)
    (set 'i (+ i ~a))))"
	   starti
	   maxi
	   j
	   delay-ms
	   stepi)
   :time 8d0))




#+nil
(trigger-all-cameras-seq-2d-scan)

(defun trigger-all-cameras-seq-2d-scan** ( &key 
					   (starti 400) (startj 1800)
					   (maxi 2900) (maxj 3700)
					   (stepi 100)
					   (stepj 100)
					   (delay-ms 24))
  
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
    (set 'j (+ j ~a))))"
	  starti startj
	  maxj maxi
	  delay-ms
	  stepi stepj)
  )



#+nil
(trigger-all-cameras-seq-2d-scan)
#+nil
(trigger-all-cameras)


(defun trigger-all-cameras ()
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   "(progn
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 1) 
 (digital-write 11 0)
 (digital-write 12 0) 
 (digital-write 10 0))"
   :time .001d0))



#+nil
(loop for i below 1000 do
     (format t "~a~%" i)
     ;(sleep (/ 60d0))
     (trigger-all-cameras))
#+nil
(tilt-mirror 1550 2500)

(defun trigger-all-cameras-several-times (&key (n 475))
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   (format nil "(dotimes (i ~d)
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 1) 
 (digital-write 11 0)
 (digital-write 12 0) 
 (digital-write 10 0)
 (delay 25)
 (print i))" n)
   :time .001d0))


#+nil
(trigger-all-cameras-several-times)

(defun block-laser ()
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) (first *ard*)
   "(progn (pin-mode 8 1) (digital-write 8 0))"))

(defun unblock-laser ()
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) (first *ard*)
   "(digital-write 8 1)"))

#+nil
(block-laser)
#+nil
(unblock-laser)


;; fiber center first coordinate:   800 .. 1550 .. 2750
;; fiber center second coordinate: 1800 .. 2500 .. 3580

(defun tilt-mirror-and-trigger-all-cameras (x y)
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
;; delay waits ms
   (format nil "(progn
 (dac ~a ~a)
 (delay 5)
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 1) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))" x y)
   :time .001d0)) 

(defun tilt-mirror (x y)  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
;; delay waits ms
   (format nil "(dac ~a ~a)" x y)))


#+nil
(tilt-mirror 0 0)

(fftw:prepare-threads)

(fftw::%fftwf_plan_with_nthreads 6)

(pylon:initialize)
(defparameter *fact* (pylon::factory))
(defparameter *cams* (pylon:create *fact* 3))

;; http://www.imagemagick.org/Usage/fourier/
;; for i in e{66,65,40}h.tiff; do
;;   name=k`basename $i .tiff`.miff
;;   nameabs=k`basename $i .tiff`-0.miff
;;   namelog=k`basename $i .tiff`-0log.miff
;;   convert $i -fft    +depth +adjoin $name
;;   scale=`convert $nameabs -auto-level -format "%[fx:exp(log(mean)/log(0.5))]" info:`
;;   convert $nameabs -auto-level -evaluate log $scale  $namelog
;; done

;; in display Image Edit -> region of interest to find the position of first order
;; 40 66x66+127+365
;; 65 66x66+63+260
;; 66 66x66-25+25

(list (list 40 (+ 33 (- 322 256)) (+ 33 256 83))
      (list 65 (+ 33 (- 386 256)) (+ 33 256 189))
      (list 66 (+ 33 (- 474 256)) (+ 33 (- 426 256))))

(list (list 40 (+ 45 130) (+ 45 122))
      (list 65 (+ 45 301) (+ 45 773))
      (list 66 (+ 45 184) (+ 45 126)))

(defparameter *cam-parameters*
  `((21433565    1    1  280   280 nil  777  337 230  82  90  0   70 "transmission with polrot (top)")
    (21433566    1    1  512   512 nil  789  112 482 429  90  0  875 "backreflection with polrot")  
    (21433540    1    1  280   280 t    985  427 206 213  90  0   70 "transmission same pol"))
  "    id      binx  biny  w    h  rev   x    y  kx  ky   d   g   e   name")
;; i reverseX the 40 to compensate for the pbs
;; 33040

(eval-when (:compile-toplevel :execute :load-toplevel)
 (let ((a nil))
   (defun init-cam-parameter-hash ()
     (setf a (make-hash-table))
     (loop for (id      binx  biny  w   h rev    x    y kx  ky   d  g   e   name)
	in *cam-parameters* do
	  (setf (gethash id a) (list id      binx  biny  w   h rev    x    y kx  ky   d  g   e   name))))
   (defun get-cam-parameters (cam)
     (gethash (parse-integer (pylon:cam-get-serial-number *cams* cam)) a))))


(init-cam-parameter-hash)
#+nil
(get-cam-parameters 1)

#+nil
(parse-integer (pylon:cam-get-serial-number *cams* 2))

(pylon:cams-open *cams*)
#+nil
(pylon:cams-close *cams*)
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
(loop for i below 3 do 
     (pylon::command-execute *cams* i "ClearLastError"))
#+nil
(pylon::command-isdone *cams* 1 "ClearLastError")

#+nil
(loop for i below 3 collect
     (destructuring-bind (    id      binx  biny  w   h rev    x    y  kx  ky   d   g   e   name)
	 (get-cam-parameters i)
       (let ((inc (pylon:get-inc-i *cams* i "ExposureTimeRaw")))
	 (pylon:set-value-i *cams* i "ExposureTimeRaw" (* inc (floor e inc))))
       (list e (pylon:get-value-i *cams* i "ExposureTimeRaw"))))

#+nil
(dotimes (i 3)
 (pylon:set-value-e *cams* i "TriggerMode" 1))

#+nil
(dotimes (i 3)
 (pylon:to-string-e *cams* i "TriggerMode"))

#+nil
(dotimes (i 3)
  (pylon:set-value-e *cams* i "TriggerMode" 0))


(defparameter *buf-c1* (make-array (* 1 1) :element-type '(complex double-float)))
(defparameter *out-cs1* (make-array (* 1 1) :element-type '(complex single-float)))
(defparameter *out-c1* (make-array (* 1 1) :element-type '(complex double-float)))
(defparameter *buf-c* (make-array (list 1 1) :element-type '(complex double-float)
				  :displaced-to *buf-c1*))
(defparameter *out-c* (make-array (list 1 1) :element-type '(complex double-float)
				  :displaced-to *out-c1*))
(defparameter *out-cs* (make-array (list 1 1) :element-type '(complex single-float)
			    :displaced-to *out-cs1*))
(defparameter *buf-s1* (make-array (* 1 1 ) :element-type 'single-float))
(defparameter *buf-s* (make-array (list 1 1) :element-type 'single-float))
 

(let ((w 1920)
      (h 1080))
  (defparameter *buf-c1* (make-array (* w h) :element-type '(complex double-float)))
  (defparameter *out-cs1* (make-array (* w h) :element-type '(complex single-float)))
  (defparameter *out-c1* (make-array (* w h) :element-type '(complex double-float)))
  (defparameter *buf-c* (make-array (list h w) :element-type '(complex double-float)
				    :displaced-to *buf-c1*))
  (defparameter *out-c* (make-array (list h w) :element-type '(complex double-float)
				    :displaced-to *out-c1*))
  (defparameter *out-cs* (make-array (list h w) :element-type '(complex single-float)
			    :displaced-to *out-cs1*))
  (defparameter *buf-s1* (make-array (* w h) :element-type 'single-float))
  (defparameter *buf-s* (make-array (list h w) :element-type 'single-float
				    :displaced-to *buf-s1*))
  nil)

(declaim (type (simple-array (complex single-float) 1) *out-cs1*)
	 (type (simple-array single-float 1) *out-s1*)
	 (type (array (complex single-float) 2) *out-cs*)
	 (type (array single-float 2) *out-s*))


#+nil
(pylon:start-grabbing *cams*)


#+nil
(tilt-mirror-and-trigger-all-cameras 1550 2500)

#+nil
(progn
 (pylon:start-grabbing *cams*)
 (LOOP FOR I BELOW 1 DO
      (let ((th (sb-thread:make-thread 
		 #'(lambda ()
		     (progn
		       (format t "waiting for cameras ... ")
		       ;(tilt-mirror 1550 2500)
		       (loop for i below 3 collect
			    (destructuring-bind (cam success-p w h framenr) 
				(multiple-value-list (pylon:grab-cdf *cams* *buf-c*))
			      (format t "~a~%" (list cam success-p w h))
			  ;    (sleep .1)
			      ))))
		 :name "camera-acquisition")))
	(sleep .1)
	(trigger-all-cameras)
	(sb-thread:join-thread th)
	(sleep .1)
	(format t "3 cameras responded ~%")))
 (pylon:stop-grabbing *cams*))

#+nil
(dotimes (i 1)
  (trigger-all-cameras))



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
(step (tukey-window 64))




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
 
#+nil
(write-pgm8 "/dev/shm/tukey.pgm" (.uint8 (tukey-window2 :w 512)))

(declaim (optimize (speed 3)))

(defun subtract-bg-and-multiply-window (a bg win)
  "calculate win*(a-bg) and return result in a"
  (declare (type (array single-float 2) a bg win)
	   (values (array single-float 2) &optional)
	   (optimize (speed 3)))
  (let* ((a1 (.linear a))
	 (b1 (.linear bg))
	 (w1 (.linear win))
	 (n (min (array-total-size a)
		 (array-total-size bg)
		 (array-total-size win))))
    (declare (type (simple-array single-float 1) a1 b1 w1))
    (dotimes (i n)
      (setf (aref a1 i) (* (aref w1 i)
			   (- (aref a1 i) (aref b1 i)))))
    a))

(defun subtract-bg-and-multiply-window1 (a bg win)
  "calculate win*(a-bg) and return result in a"
  (declare (type (simple-array single-float 1) a bg win)
	   (values (simple-array single-float 1) &optional)
	   (optimize (speed 3)))
  (let ((n (min (length a) (length bg) (length win))))
   (dotimes (i n)
     (setf (aref a i) (* (aref win i)
			 (- (aref a i) (aref bg i))))))
  a)

(defparameter *win* nil)

(defun create-windows (darks &key (alpha-x .2) (alpha-y alpha-x))
  (setf *win*
	(loop for e in darks collect
	     (destructuring-bind (h w) (array-dimensions e)
	       (tukey-window2 :w w :h h :alpha-x alpha-x :alpha-y alpha-y))))
  nil)

(declaim (optimize (speed 0) (debug 3) (safety 3)))


#+nil
(create-windows (first *dark*))

#+nil
(dotimes (i 3)
  (ics:write-ics2 (format nil "/dev/shm/o~d.ics" i) (.abs (elt *blob* i))))

#+nil
(dotimes (i 3)
  (destructuring-bind (ii j x y v outc) (first (elt *bla* i))
    (format t "~a~%" (list i (get-cam-parameters i)))
    (write-pgm8 (format nil "/dev/shm/o~d.pgm" i) (.uint8 (.log (.abs outc))))))

(defvar *bla* nil)
(defparameter *blob* nil)
#+nil 
(run)
(defvar *dark* nil)

(+ 500 (* (+ 14 35) 50))

(defparameter *diff* nil)
(defun run-several-s ()
  (declare (optimize (debug 3) (speed 3)))
  (defparameter *diff* nil)
  ;; make sure the fft can has an optimized plan
  (fftw::%fftwf_import_wisdom_from_filename "fiberholo.fftwf.wisdom")
  (time
   (progn 
     (progn (fftw::rftf *buf-s* :out-arg *out-cs* :w 1024 :h 1024 :flag fftw::+patient+) nil)
     (progn (fftw::rftf *buf-s* :out-arg *out-cs* :w 512 :h 512 :flag fftw::+patient+) nil)))
  
  (dotimes (i 3)
    (pylon:set-value-e *cams* i "TriggerMode" 1))
  (let* ((step 50)
	 (starti 450)
	 (maxi 2800)
	 (stepi step)
	 (startj 1100)
	 (maxj 2950)
	 (stepj step)
	 (count-first (let ((count 0))
			(loop for j from starti below maxi by stepi do
			     (incf count)) 
			count))
	 (count-second (let ((count 0))
			 (loop for yj from startj below maxj by stepj do
			      (incf count)) 
			 count)))
    (let* ((old 0)
	  (buf-s (make-array (list 1024 1024) :element-type 'single-float))
		    (buf-cs (make-array (list 1024 (+ 1 (floor 1024 2))) :element-type '(complex single-float)))
		    (accum-buf-s (loop for i below 3 collect
				      (destructuring-bind (id binx biny ww hh rev ox oy x y d g e name) 
					  (get-cam-parameters i)
				       (declare (ignorable id binx biny ox oy d g e name x y))
				       (make-array (list hh (+ 1 (floor ww 2))) :element-type 'single-float))))
		   (dc-s (make-array (list count-second count-first 3)
				     :element-type 'single-float))
		   (ext-cs (make-array (list count-second count-first 3)
				       :initial-contents 
				       (loop for j below count-second collect
					    (loop for i below count-first collect
						 (loop for i below 3 collect
						      (make-array (list 90 90) 
								  :element-type '(complex single-float)))))))
		   (plan (loop for i below 3 collect 
			      (destructuring-bind (id binx biny ww hh rev ox oy x y d g e name) 
				  (get-cam-parameters i)
				(declare (ignorable id binx biny ox oy d g e name x y))
				(fftw::rplanf buf-s :out buf-cs :w ww :h hh :flag fftw::+measure+)))))
      (unwind-protect 
		 (progn
		   (dotimes (i 3)
		     (pylon::command-execute *cams* i "GevTimestampControlReset"))
		   (pylon:start-grabbing *cams*)
		   (let ((th (sb-thread:make-thread 
			      #'(lambda ()
				  (loop for yj from startj below maxj by stepj and yji from 0 collect ;; second
				       (loop for j from starti below maxi by stepi and ji from 0 collect ;; first
					    (loop for i below 3 do
						 (multiple-value-bind (cam success-p w h framenr timestamp) 
						     (pylon::grab-sf *cams* buf-s)
						   
						   (declare (ignorable framenr)
							    (type (unsigned-byte 32) w h))
						   (if success-p
						       (destructuring-bind (id binx biny ww hh rev ox oy x y d g e name) 
							   (get-cam-parameters cam)
							 (declare (ignorable id binx biny ox oy d g e name x y))
							 (assert (= ww w))
							 (assert (= hh h))
							 (when (= 0 cam)
							   (push (list yji ji (/ (- timestamp old) 125e6)) *diff*)
							   (setf old timestamp))
							 (format t "~a~%" cam)
							 (when (and *dark* *win*)
							   (let ((win (.linear (elt *win* cam)))
								 (d (.linear (elt (first *dark*) cam)))
								 (s (.linear buf-s)))
							     (declare (type (simple-array single-float 1) s win d))
							     (sb-sys:with-pinned-objects (win d s)
							       (pylon::helper-subtract-bg-multiply-window 
								(sb-sys:vector-sap s)
								(sb-sys:vector-sap d)
								(sb-sys:vector-sap win) (* w h)))))

							 (progn
							   (fftw::%fftwf_execute (elt plan cam))
							   (setf (aref dc-s yji ji cam) (realpart (aref buf-cs 0 0)))
							   #+nil (.accum (elt accum-buf-s cam) (.abs2 buf-cs))
							   #+nil 
							   (extract-csf* (make-array (list hh (+ 1 (floor ww 2)))
										     :element-type '(complex single-float)
										     :displaced-to buf-cs)
									 (aref ext-cs yji ji cam) :x x :y y :w d :h d)
							   
							   (pylon::%helper-extract-csf ;; note: there is still a bug in this code in case of horizontal aliasing of rft arrays
							    (sb-sys:vector-sap (sb-ext:array-storage-vector buf-cs))
							    (sb-sys:vector-sap
							     (sb-ext:array-storage-vector (aref ext-cs yji ji cam)))
							    x y (1+ (floor w 2)) h
							    d d)))
						       (format t "acquisition error.~%")))))))
			      :name "camera-acquisition")))
		     (sleep .001)
		     (trigger-all-cameras-seq-2d-scan :starti starti :startj startj
						      :maxi maxi :maxj maxj
						      :stepj step :stepi step :delay-ms 40 :line-delay-ms 100)
		     (defparameter *steering-params* (list 'i starti maxi stepi
						    'j startj maxj stepj))
		     (sb-thread:join-thread th)))
	      (pylon:stop-grabbing *cams*)
	      (defparameter *result* ext-cs)
	      (defparameter *result2* dc-s)
	      (defparameter *result3* accum-buf-s)
	      
	      (loop for p in plan do (fftw::%fftwf_destroy_plan p))
	      (sb-ext:gc :full t)
	      (tilt-mirror 0 0)))))
#+nil
(time (progn (run-several-s) nil))

#+nil
(loop for i below 3 do
     (write-pgm8 (format nil "/dev/shm/o~d.pgm" i) (.uint8 (.log (elt *result3* i)))))

#+nil
(get-cam-parameters 2)

#+nil
(time
 (destructuring-bind (hh ww kk) (array-dimensions *result*)
   (let ((a (make-array (list hh ww kk 90 90) :element-type '(complex single-float))))
     (dotimes (jj hh)
       (dotimes (ii ww)
	 (dotimes (k kk)
	   (let ((b (aref *result* jj ii k)))
	     (dotimes (j 90)
	       (dotimes (i 90)
		 (setf (aref a jj ii k j i) (aref b j i))))))))
     (ics:write-ics2 (format nil "/media/sdc1/dat/0805/o9.ics") a))
   (with-open-file (s (format nil "/media/sdc1/dat/0805/o9.dat") :direction :output
		      :if-exists :supersede :if-does-not-exist :create)
     (format s "~a~%" *steering-params*)
     (format s "~a ~a~%" 'cam '(id      binx  biny  w   h  rev   x    y  kx  ky   d   g   e   name))
     (dotimes (cam 3)
       (format s "~d ~s~%" cam (get-cam-parameters cam))))))



#+nil
(time
 (loop for k below 3 do
      (let ((a (make-array (list 66 66) :element-type 'single-float)))
	(loop for i below 1000 do
	     (.accum a (.abs2 (aref *result* i k))))
	(write-pgm8 (format nil "/dev/shm/o~3,'0d.pgm" k) (.uint8 (.abs (fftw::ftf a)))))))


(defun run-several-s-without-ft ()
  (declare (optimize (debug 3) (speed 3)))
  (defparameter *diff* nil)
  (dotimes (i 3)
    (pylon:set-value-e *cams* i "TriggerMode" 1))
  (let* ((step 20)
	 (starti 450)
	 (maxi 2800)
	 (stepi step)
	 (startj 1100)
	 (maxj 2950)
	 (stepj step)
	 (count-first (let ((count 0))
			(loop for j from starti below maxi by stepi do
			     (incf count)) 
			count))
	 (count-second (let ((count 0))
			 (loop for yj from startj below maxj by stepj do
			      (incf count)) 
			 count))
	 (fda (loop for i below 3 collect (open (format nil "/media/sdd3/b/cam~d" i)
						:direction :output
						:if-exists :supersede)))
	 (fds (mapcar #'sb-sys:fd-stream-fd fda)))
    (unwind-protect 
	 (progn
	   (dotimes (i 3)
	     (pylon::command-execute *cams* i "GevTimestampControlReset"))
	   (pylon:start-grabbing *cams*)
	   (let ((th (sb-thread:make-thread 
		      #'(lambda ()
			  (loop for yj from startj below maxj by stepj and yji from 0 collect ;; second
			       (loop for j from starti below maxi by stepi and ji from 0 collect ;; first
				    (loop for i below 3 do
					 (multiple-value-bind (cam success-p w h framenr timestamp) 
					     (pylon::grab-store *cams* fds))))))
		      :name "camera-acquisition")))
	     (sleep .001)
	     (trigger-all-cameras-seq-2d-scan :starti starti :startj startj
					      :maxi maxi :maxj maxj
					      :stepj step :stepi step :delay-ms 80 :line-delay-ms 100)
	     (defparameter *steering-params* (list 'i starti maxi stepi
						   'j startj maxj stepj))
	     (sb-thread:join-thread th)))
      (pylon:stop-grabbing *cams*)
      
      (sb-ext:gc :full t)
      (tilt-mirror 0 0))
    (mapcar #'close fda)))


(let* ((step 20)
       (starti 450)
       (maxi 2800)
       (stepi step)
       (startj 1100)
       (maxj 2950)
	 (stepj step)
       (count-first (let ((count 0))
		      (loop for j from starti below maxi by stepi do
			   (incf count)) 
		      count))
       (count-second (let ((count 0))
		       (loop for yj from startj below maxj by stepj do
			    (incf count)) 
		       count)))
  (* count-first count-second))

(/ (* 3 10974 1920 1080 12 (/ 8)) (* 1024 1024d0 1024))
;; 95Gb


#+nil
(run-several-s-without-ft)

#+nil
(time
 (destructuring-bind (h w cams) (array-dimensions *result*)
   (loop for k below cams do
	(let ((a (make-array (list (* h 66)
				   (* w 66))
			     :element-type '(complex single-float))))
	  (loop for j from 20 below h do
	       (loop for i from 20 below w do
		    (let ((b (aref *result* j i k)))
		      (dotimes (jj 66)
			(dotimes (ii 66)
			  (setf (aref a (+ (* 66 (- j 20)) jj) (+ (* 66 (- i 20)) ii))
				(aref b jj ii)))))))
	  (write-pgm8 (format nil "/dev/shm/o~1,'0d.pgm" k)
		      (.uint8 (.abs a)))))))


#+nil
(loop for i below 3 collect (get-cam-parameters i))

#+nil
(time
 (let ((hh 66)
       (ww 66))
  (destructuring-bind (h w cams) (array-dimensions *result*)
    (loop for k below cams do
	 (let ((a (make-array (list (* h hh)
				    (* w ww))
			      :element-type '(complex single-float))))
	   (loop for j from 0 below h do
		(loop for i from 0 below w do 
		     (let ((b #+nil (extract (aref *result* j i k) :w ww :h hh)
			      (fftw::ftf (aref *result* j i k) :sign fftw::+backward+ :flag fftw::+patient+)
			     ))
		       (dotimes (jj hh)
			 (dotimes (ii ww)
			   (setf (aref a (+ (* hh (- j 0)) jj) (+ (* ww (- i 0)) (if (= k 0) (- (1- ww) ii)
										       ii)))
				 (aref b jj ii)))))))
	   (write-pgm8 (format nil "/dev/shm/o~1,'0d.pgm" k)
		       (.uint8 (.abs a))))))))

#+nil
(time
 (fftw::ftf (aref *result* 12 12 0) :sign fftw::+backward+ :flag fftw::+patient+))

#+nil
(write-pgm8 "/dev/shm/o.pgm" (.uint8 (.abs *result-mosaic*)))

#+nil
(loop for j from 400 below 2900 by 100 and i from 0 collect (list i j)) ;; 11 is center
#+nil
(loop for j from 1800 below 3700 by 100 and i from 0 collect (list i j)) ;; 9 is center

#+nil
(time
 (loop for j below 19 do
      (loop for i below 25 do
	   (write-pgm8 (format nil "/dev/shm/ko-~3,'0d-~3,'0d.pgm" j i) (.uint8 (.log (.abs (aref *result* j i 1))))))))

#+nil
(let ((j 10) (i 10) (cam 1) (w 66) (h 66))
  (let ((a (make-array (list h w) :element-type '(complex single-float))))
    (destructuring-bind (id binx biny ww hh rev ox oy x y d g e name) (get-cam-parameters cam)
      #+nil (extract-csf* (aref *result* j i cam) a :x x :y y :w w :h h)
     (pylon::%helper-extract-csf 
       (sb-sys:vector-sap (sb-ext:array-storage-vector (aref *result* j i cam)))
       (sb-sys:vector-sap (sb-ext:array-storage-vector a))
       x y (+ 1 (floor 580 2)) 580 w h)
      (write-pgm8 (format nil "/dev/shm/eo-~3,'0d-~3,'0d-~1,'0d.pgm" j i cam) (.uint8 (.log (.abs a #+nil (aref *result* j i cam)))))
      (list x y))))


#+nil
(get-cam-parameters 1)


#+nil
(time
 (loop for j below 19 do
      (loop for i below 25 do
	   (write-pgm8 (format nil "/dev/shm/o-~3,'0d-~3,'0d.pgm" j i) (.uint8 (aref *result2* j i 1))))))

#+nil
(let ((j 15)
      (i 8))
 (write-pgm8 (format nil "/dev/shm/o-~3,'0d-~3,'0d.pgm" j i) (.uint8 (.log (.abs (aref *result* j i 2))))))

#+nil
(room) 

#+nil
(let* ((v (loop for e across (let ((s (sort *bsdl* #'<)))
			       (subseq s 0 (1- (length s)))
			      )
	       collect e))
       (mean (/ (reduce #'+ v)
		(length v)))
       (var (/ 
	     (loop for e in v sum (- (expt e 2) (expt mean 2)))
	     (length v)))) 
  (list (* 1000 mean) (* 1000 (sqrt var)) (* 1000 (- (reduce #'max v) (reduce #'min v))))) ;; => 25.00 0.43

#+nil
(defparameter *bsdl*  (make-array (length *diff*) :element-type 'single-float :initial-contents  (mapcar #'third *diff*)))

#+nil
(let ((s (sort *bsdl* #'<)))
 (subseq s 0 (1- (length s)a)))

#+nil 
(sectf *features* (union *features* (list :gige)))

#+nil
(time
 (progn (fftw::rftf *buf-s* :out-arg *out-cs* :w 1024 :h 1024 :flag fftw::+patient+) nil))

#+nil
(fftw::%fftwf_export_wisdom_to_filename "fiberholo.fftwf.wisdom")

#+nil
(fftw::%fftwf_import_wisdom_from_filename "fiberholo.fftwf.wisdom")

#+nil
(progn (let ((count 0)
	     (step 12))
	 (loop for yj from 1800 below 3700 by step do
	      (loop for j from 400 below 2900 by step do
		   (incf count)))
	 (list (/ (* 66 66 8 3 count) (* 1024d0 1024))
	       (/ count 305.0)))) ; => 39.9 fps

(/
 (/ (- 1288674299061 1288671173986)
    125e6))

(/ (/ (- 1325445558296 1325442558192) 125e6))
 
#+nil
(DOTIMES (i 3)
 (write-pgm8 (format nil "/dev/shm/o~d.pgm" i) (.uint8 (.log (.abs (sixth (first (elt *bla* i))))))))

 
#+nil
(run)

#+nil
(length (elt *bla* 0))


#+nil
(require :sb-sprof)

#+nil
(/ (* 475 3 512 512) 17.29)

#+nil
(time
 (progn 
   (fftw::%fftwf_import_wisdom_from_filename "fiberholo.fftwf.wisdom")
   (format t "~a~%" (multiple-value-list (get-decoded-time)b))
   (sb-sprof:with-profiling (:max-samples 1000
					  :report :flat
					  :loop nil)
     (run-several-s))
   nil
   (format t "~a~%" (multiple-value-list (get-decoded-time)))))

(defun make-camera-buffer (cam) 
  (destructuring-bind (id binx biny ww hh rev ox oy x y d g e name) (get-cam-parameters cam)
    (declare (ignorable id binx biny rev ox oy x y d g e name))
    (make-array (list hh ww) :element-type 'single-float :initial-element 0s0)))

#+nil
(get-cam-parameters 0)

#+nil
(make-camera-buffer 0)

(defun capture-dark-images (&optional (n 10))
  (block-laser)
  (sleep .1)
  (dotimes (i 3)
    (pylon:set-value-e *cams* i "TriggerMode" 0))
  (unwind-protect 
   (progn
     (pylon:start-grabbing *cams*)
     (time (prog1
	  (let ((cambuf (loop for cam below 3 collect (make-camera-buffer cam)))
		(count (loop for cam below 3 collect 0)))
	    (loop for j below n do
		 (loop for i below 3 do
		      (destructuring-bind (cam success-p w h framenr timestamp) 
			  (multiple-value-list (pylon::grab-sf *cams* *buf-s*))
			(declare (ignorable framenr timestamp))
			(if success-p
			    (destructuring-bind (id binx biny ww hh rev ox oy x y d g e name) 
				(get-cam-parameters cam)
			      (declare (ignorable id binx biny rev ox oy x y d g e name))
			      (assert (= ww w))
			      (assert (= hh h))
			      (let* ((q (make-array (list h w)
						    :element-type 'single-float
						    :displaced-to *buf-s1*))
					;(v (.mean q))
				     )
				(.accum (elt cambuf cam) q)
				(incf (elt count cam))))
			    (format t "acquisition error.~%")))))
	    (loop for cam below 3 do
		 (format t "~a~%" (list (first (get-cam-parameters cam))
					'collected-dark-frames (elt count cam)))
		 (unless (= 0 (elt count cam))
		   (setf (elt cambuf cam) (.* (elt cambuf cam) (/ (elt count cam))))))
	    
	    (values cambuf count)))))
    (progn (pylon:stop-grabbing *cams*)
	    (unblock-laser))))


#+nil
(progn
 (capture-dark-images 300)
 nil)

#+nil
(sb-sprof:with-profiling (:max-samples 1000
                                       :report :flat
                                       :loop nil)
  (defparameter *dark* (multiple-value-list (capture-dark-images 200))))

#+nil
(time
 (progn
   (dotimes (i 3)
     (pylon::command-execute *cams* i "GevTimestampControlReset"))
   (defparameter *dark* (multiple-value-list (capture-dark-images 1000)))
   (create-windows (first *dark*))))

#+nil
(reduce #'+ (sb-ext:array-storage-vector (elt (first *dark*) 0)
	     ))

#+nil
(dotimes (i 3)
 (ics:write-ics2 (format nil "/dev/shm/dark_~d.ics" i) (elt (first *bla*) i)))

#+nil
(setf *bla* nil)
#+nil
(sb-ext:gc :full t)
;; 902s to run
;; 980s when extracted complex arrays are safed

;; Evaluation took:
;;   2731.082 seconds of real time
;;   5921.979000 seconds of total run time (5796.728000 user, 125.251000 system)
;;   [ Run times consist of 8.982 seconds GC time, and 5912.997 seconds non-GC time. ]
;;   216.84% CPU
;;   7,681,963,930,572 processor cycles
;;   34,209,748,240 bytes consed


;; (room)
;; Dynamic space usage is:   568,429,344 bytes.
;; Read-only space usage is:      5,680 bytes.
;; Static space usage is:         3,120 bytes.
;; Control stack usage is:        8,616 bytes.
;; Binding stack usage is:        1,072 bytes.
;; Control and binding stack usage is for the current thread only.
;; Garbage collection is currently enabled.

;; Breakdown for dynamic space:
;;   432,097,984 bytes for     5,720 simple-array-complex-double-float objects.
;;   28,747,552 bytes for   408,538 instance objects.
;;   107,648,368 bytes for 2,673,532 other objects.
;;   568,493,904 bytes for 3,087,790 dynamic objects (space total.)

#+nil
(pylon:terminate *cams*)
#+nil
(defun run2 ()
 (let ((aj (make-hash-table))
       (ayj (make-hash-table)))
   (loop for e in (aref *bla* 0) do
	(when (and e (listp e))
	  (destructuring-bind (j yj ji yji v im) e
	    (format t "~a~%" (list ji yji))))
	#+nil
	(setf (gethash j aj) 1
	      (gethash yj ayj) 1))
   (defparameter *bla1* (list aj ayj))))

#+nil
(run2)



#+nil
(time
 (let* ((date "0715")
	(ver 1)
	(h 
	 (1+ (loop for (j yj ji yji v im) in (aref *bla* 0) maximize yji)))
	(w
	 (1+ (loop for (j yj ji yji v im) in (aref *bla* 0) maximize ji)))
	(a (make-array (list 3 h w 66 66) :element-type '(complex single-float))))
   (dotimes (cam 3)
     (loop for (j yj ji yji v im) in (aref *bla* cam) do
	  (dotimes (jj 66)
	    (dotimes (ii 66)
	      (setf (aref a cam yji ji jj ii) (coerce (aref im jj ii) '(complex single-float)))))))
   (ics:write-ics2 (format nil "/home/martin/scan~a_~d.ics" date ver) a)
   (with-open-file (s (format nil "/home/martin/scan~a_~d.dat" date ver) :direction :output
		      :if-exists :supersede :if-does-not-exist :create)
     ;(format t "~a~%" (capture-dark-images))
     (dotimes (cam 3)
       (format s "~a" (list (get-cam-parameters cam)
			    (loop for (a b c d e im) in (aref *bla* cam) collect
				 (list a b c d e))))))))
