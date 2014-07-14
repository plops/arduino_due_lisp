;; you have to make sure that the following environment variables are
;; set when starting the common lisp interpreter:
;; export PYLON_ROOT=${HOME}/pylon-3.2.1-x86_64/pylon3
;; export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${PYLON_ROOT}/genicam/bin/Linux64_x64
;; export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${PYLON_ROOT}/lib64
;; export GENICAM_ROOT_V2_3=${PYLON_ROOT}/genicam
;; export PYLON_CAMEMU=2

#-sbcl
(load "~/quicklisp/setup.lisp")


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
 (pin-mode 10 1)
 (pin-mode 11 1)
 (pin-mode 12 1))")
  (setf *trigger-outputs-initialized* t))

#+nil
(initialize-trigger-outputs)

#+nil
(arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   "(progn (+ 1 2))")

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
   :time .00001d0))

#+nil
(trigger-all-cameras)

(defun block-laser ()
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) (first *ard*)
   "(progn (pin-mode 8 1) (digital-write 8 0))"))

(defun unblock-laser ()
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) (first *ard*)
   "(digital-write 8 1)"))


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
 (digital-write 10 0))" x y))) 

(defun tilt-mirror (x y)  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
;; delay waits ms
   (format nil "(dac ~a ~a)" x y)))

(fftw:prepare-threads)

(pylon:initialize)
(defparameter *fact* (pylon::factory))
(defparameter *cams* (pylon:create *fact* 3))

(defparameter *cam-parameters*
  `((21433565    2    2   512 512   249  17 167 478  66   0 12635 "transmission with polrot (top)")
    (21433566    1    1   580 580   520 215 220  11  66  28 33040 "backreflection with polrot") ;; this one has order on zero line
    (21433540    2    2   512 512   365   0 101 138  66   0 16135 "transmission same pol"))
  "    id      binx  biny  w   h     x    y  kx  ky   d   g   e   name")



(eval-when (:compile-toplevel :execute :load-toplevel)
 (let ((a nil))
   (defun init-cam-parameter-hash ()
     (setf a (make-hash-table))
     (loop for (id      binx  biny  w   h     x    y kx  ky   d  g   e   name)
	in *cam-parameters* do
	  (setf (gethash id a) (list id      binx  biny  w   h     x    y kx  ky   d  g   e   name))))
   (defun get-cam-parameters (cam)
     (gethash (parse-integer (pylon:cam-get-serial-number *cams* cam)) a))))


(init-cam-parameter-hash)


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
		       "ExposureTimeRaw" "GainRaw") collect
	   (pylon:get-value-i *cams* j e t nil))
      (list (pylon:get-value-e *cams* j "TriggerMode")
	    (pylon:get-value-b *cams* j "AcquisitionFrameRateEnable")
	    (pylon:get-value-f *cams* j "ResultingFrameRateAbs"))))

#+nil
(dotimes (i 3)
 (pylon:set-value-e *cams* i "TriggerMode" 1))

#+nil
(dotimes (i 3)
 (pylon:to-string-e *cams* i "TriggerMode"))

#+nil
(dotimes (i 3)
  (pylon:set-value-e *cams* i "TriggerMode" 0))

(defparameter *buf-c1* nil)
(defparameter *buf-c* nil)
(defparameter *out-c1* nil)
(defparameter *out-c* nil)

(let ((w 580)
      (h 580))
  (setf *buf-c1* (make-array (* w h) :element-type '(complex double-float)))
  (setf *out-c1* (make-array (* w h) :element-type '(complex double-float)))
  (setf *buf-c* (make-array (list h w) :element-type '(complex double-float)
				    :displaced-to *buf-c1*))
  (setf *out-c* (make-array (list h w) :element-type '(complex double-float)
				    :displaced-to *out-c1*))
  )

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



(defun tukey-window (nn &key (alpha .9d0))
  "The Tukey window,[8][39] also known as the tapered cosine window,
can be regarded as a cosine lobe of width alpha N/2 that is convolved
with a rectangular window of width (1 - alpha/2)N. For alpha=0
rectangular, for alpha=1 Hann window."
  (declare (type (unsigned-byte 32) nn)
	   (type double-float alpha)
	   (values (simple-array double-float 1) &optional))
  (let ((w (make-array nn :element-type 'double-float))
	(n-1 (- nn 1)))
    (dotimes (n nn)
      (setf (aref w n)
	    (cond ((<= 0 n (* alpha .5 n-1))
		   (* .5 (+ 1 (cos (* pi (- (/ (* 2 n)
					       (* alpha n-1)) 1))))))
		  ((<= (* alpha .5 n-1) n (* n-1 (- 1 (* .5 alpha))))
		   1d0)
		  ((<= (* n-1 (- 1 (* .5 alpha))) n n-1)
		   (* .5 (+ 1 (cos (* pi (+ (/ (* 2 n)
					       (* alpha n-1))
					    (/ -2d0 alpha)
					    1)))))))))
    w))



(defun tukey-window2 (&key (w 100) (h w) (alpha-x .2d0) (alpha-y alpha-x))
  (declare (type (unsigned-byte 32) w h)
	   (values (simple-array double-float 2) &optional))
  (let ((b (make-array (list h w) :element-type 'double-float))
	(wh (tukey-window h :alpha alpha-y))
	(ww (tukey-window w :alpha alpha-x)))
    (dotimes (j h)
      (dotimes (i w)
	(setf (aref b j i) (* (aref wh j)
			      (aref ww i)))))
    b))

#+nil
(write-pgm8 "/dev/shm/tukey.pgm" (.uint8 (tukey-window2 :w 512)))

(defun subtract-bg-and-multiply-window (a bg win)
  "calculate win*(a-bg) and return result in a"
  (declare (type (array (complex double-float) 2) a)
	   (type (array double-float 2) bg win)
	   (values (array (complex double-float) 2) &optional))
  (let* ((a1 (.linear a))
	 (b1 (.linear bg))
	 (w1 (.linear win))
	 (n (min (array-total-size a)
		 (array-total-size bg)
		 (array-total-size win))))
    (declare (type (simple-array (complex double-float) 1) a1)
	     (type (simple-array double-float 1) b1 w1))
    (dotimes (i n)
      (setf (aref a1 i) (* (aref w1 i)
			   (- (aref a1 i) (aref b1 i)))))
    a))

(defparameter *win* nil)

(defun create-windows (darks &key (alpha-x .2d0) (alpha-y alpha-x))
  (setf *win*
	(loop for e in darks collect
	     (destructuring-bind (h w) (array-dimensions e)
	       (tukey-window2 :w w :h h :alpha-x alpha-x :alpha-y alpha-y))))
  nil)

#+nil
(create-windows (first *dark*))

#+nil
(dotimes (i 3)
  (ics:write-ics2 (format nil "/dev/shm/o~d.ics" i) (.abs (elt *blob* i))))
#+nil
(dotimes (i 3)
  (destructuring-bind (id cam x y im) (elt *blob* i)
   (write-pgm8 (format nil "/dev/shm/o~d.pgm" id) (.uint8 (.abs im)))))
(defvar *bla* nil)
(defparameter *blob* nil)
#+nil 
(run)
(defvar *dark* nil)


(defun run-raw ()
  (setf *bla* (make-array 3 :initial-element nil))  (unless *trigger-outputs-initialized*)
  (dotimes (i 3)
    (pylon:set-value-e *cams* i "TriggerMode" 1))
  (let ((fds nil))
   (unwind-protect 
	(progn 
	  (setf fds
		(loop for i below 3 collect
		     (sb-unix::unix-open (format nil "/dev/shm/r~a.raw" i) (logior sb-unix:o_creat 
										     sb-unix:o_trunc
										     sb-unix:o_wronly) 
					 #o666)))
	  (defparameter *blaf* fds)
	  (pylon:start-grabbing *cams*)
	  (				;let ((yj 2550) (yji 0)) ;
	   loop for yj from 1800 below 3700 by 100  and yji from 0 collect
	   (				;let ((j 1550) (ji 0)) ;
	    loop for j from 400 below 2900 by 100 and ji from 0 collect
	    (let ((th (sb-thread:make-thread 
		       #'(lambda ()
			   (progn
			     ;(tilt-mirror j yj)
			     (loop for i below 3 do
				  (destructuring-bind (cam success-p w h framenr) 
				      (multiple-value-list (pylon::grab-store *cams* fds))
				    (unless (= 1 success-p)
				      (format t "acquisition error. ~a~%" success-p))))))
		       :name "camera-acquisition")))
	      (sleep .001)
	      (trigger-all-cameras)
	      (sleep .001)
	      (sb-thread:join-thread th)))))
    (progn (pylon:stop-grabbing *cams*)
	   (loop for e in fds do
		(sb-unix::unix-close e))))))



#+nil
(require :sb-sprof)

#+nil
(time
 (progn (format t "~a~%" (multiple-value-list (get-decoded-time)))
	(sb-sprof:with-profiling (:max-samples 1000
                                       :report :flat
                                       :loop nil)
	  (run-raw))
	(format t "~a~%" (multiple-value-list (get-decoded-time)))))

(defun run ()
  (setf *bla* (make-array 3 :initial-element nil))  (unless *trigger-outputs-initialized*)
  (dotimes (i 3)
    (pylon:set-value-e *cams* i "TriggerMode" 1))
  (unwind-protect 
       (progn
	 (pylon:start-grabbing *cams*)
	 (				;let ((yj 2550) (yji 0)) ;
	  loop for yj from 1800 below 3700 by 100  and yji from 0 collect
	       (				;let ((j 1550) (ji 0)) ;
		loop for j from 400 below 2900 by 100 and ji from 0 collect
		     (let ((th (sb-thread:make-thread 
				#'(lambda ()
				    (progn
				      (tilt-mirror j yj)
				      (loop for i below 3 do
					   (destructuring-bind (cam success-p w h framenr) 
					       (multiple-value-list (pylon:grab-cdf *cams* *buf-c*))
					     (if success-p
						 (destructuring-bind (id binx biny ww hh ox oy x y d g e name) 
						     (get-cam-parameters cam)
						   (declare (ignorable id binx biny ox oy d g e name))
						   (assert (= ww w))
						   (assert (= hh h))
						   (when (and *dark* *win*)
						     (subtract-bg-and-multiply-window
						      *buf-c* (elt (first *dark*) cam)
						      (elt *win* cam)))
						   (fftw:ft *buf-c* :out-arg *out-c* :w w :h h :flag fftw::+measure+)
						   (let* ((q (make-array (list h w)
									 :element-type '(complex double-float)
									 :displaced-to *out-c*))
							  #+nil
							  (v (.mean (.abs2 q)))
							  (v 1d0))
						     (format t "~a~%" (list j yj))
						     (push (list j yj ji yji v  *out-c*  (extract q :x x :y y :w d :h d)) 
							   (aref *bla* cam))))
						 (format t "acquisition error.~%"))))))
				:name "camera-acquisition")))
		       (sleep .01)
		       (trigger-all-cameras)
		       (sleep .01)
		       (sb-thread:join-thread th)))))
    (pylon:stop-grabbing *cams*)))

#+nil
(/ 
 (let ((count 0)
       (step 100))
   (loop for yj from 1800 below 3700 by step do
	(loop for j from 400 below 2900 by step do
	     (incf count)))
   count)
 33.36) ;; => 14.2 fps
 
#+nil
(run)

#+nil
(length (elt *bla* 0))


#+nil
(require :sb-sprof)

#+nil
(time
 (progn (format t "~a~%" (multiple-value-list (get-decoded-time)))
	(sb-sprof:with-profiling (:max-samples 1000
                                       :report :flat
                                       :loop nil)
	  (run))
	(format t "~a~%" (multiple-value-list (get-decoded-time)))))

(defun make-camera-buffer (cam) 
  (destructuring-bind (id binx biny ww hh ox oy x y d g e name) (get-cam-parameters cam)
    (make-array (list hh ww) :element-type 'double-float :initial-element 0d0)))

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
		      (destructuring-bind (cam success-p w h framenr) 
			  (multiple-value-list (pylon:grab-cdf *cams* *buf-c*))
			(if success-p
			    (destructuring-bind (id binx biny ww hh ox oy x y d g e name) 
				(get-cam-parameters cam)
			      (assert (= ww w))
			      (assert (= hh h))
			      (let* ((q (.realpart
					 (make-array (list h w)
						     :element-type '(complex double-float)
						     :displaced-to (.linear *buf-c*))))
					;(v (.mean q))
				     )
				(.accum (elt cambuf cam) q)
				(incf (elt count cam))))
			    (format t "acquisition error.~%")))))
	    (loop for cam below 3 do
		 (unless (= 0 (elt count cam))
		   (setf (elt cambuf cam) (.* (elt cambuf cam) (/ (elt count cam))))))
	    (values cambuf count)))))
    (progn (pylon:stop-grabbing *cams*)
	    (unblock-laser))))


#+nil
(sb-sprof:with-profiling (:max-samples 1000
                                       :report :flat
                                       :loop nil)
  (defparameter *dark* (multiple-value-list (capture-dark-images 200))))

#+nil
(time
 (defparameter *dark* (multiple-value-list (capture-dark-images 1000))))
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
 (let* ((date "0713")
	(ver 10)
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

