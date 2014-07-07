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
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))"))

#+nil
(trigger-all-cameras)

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
 (delay 100)
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))" x y))) 

(defun tilt-mirror (x y)
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
;; delay waits ms
   (format nil "(dac ~a ~a)" x y)))

(fftw:prepare-threads)

(pylon:initialize)
(defparameter *fact* (pylon::factory))
(defparameter *cams* (pylon:create *fact* 3))

;; camera 0: FullName Basler acA1920-25gm#00305315DFC4#192.168.6.101:3956 serial 21433540 *cam1*
;; camera 1: FullName Basler acA1920-25gm#00305315DFDD#192.168.4.100:3956 serial 21433565 *cam2*
;; camera 2: FullName Basler acA1920-25gm#00305315DFDE#192.168.5.102:3956 serial 21433566 *cam3*

;; aravis serial   area of interest  1st order  exposure  gain
;; *cam1*   40     1024x1024+720+0   +124+274   3430      37
;; *cam2*   65     1024x1024+452+21  +168+161   7980      0
;; *cam3*   66     600x600+520+213   +225+18    16975     37

(defparameter *first-orders* `((124 274)
			       (168 161)
			       (225 18)))
(defparameter *cam-sizes* `((1024 1024)
			    (1024 1024)
			    (600 600)))

(pylon:cams-open *cams*)
#+nil
(pylon:cams-close *cams*)
#+nil
(loop for j below 3 collect
     (append 
      (loop for e in '("Width" "Height" "OffsetX" "OffsetY" "ExposureTimeRaw" "GainRaw") collect
	   (pylon:get-value-i *cams* j e t nil))
      (list (pylon:get-value-e *cams* j "TriggerMode"))))

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

(let ((w 1024)
      (h 1024))
  (setf *buf-c1* (make-array (* 1024 1024) :element-type '(complex double-float)))
  (setf *out-c1* (make-array (* 1024 1024) :element-type '(complex double-float)))
  (setf *buf-c* (make-array (list 1024 1024) :element-type '(complex double-float)
				    :displaced-to *buf-c1*))
  (setf *out-c* (make-array (list 1024 1024) :element-type '(complex double-float)
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
			    (destructuring-bind (cam success-p w h) 
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

(defvar *bla* nil)
(defun run () ; defparameter *bla*
  (setf *bla* (make-array 3 :initial-element nil))
  (unless *trigger-outputs-initialized*
    (initialize-trigger-outputs))
  (pylon:start-grabbing *cams*)
  (loop for yj from 1800 below 3700 by 50 and yji from 0 collect
       (loop for j from 400 below 2900 by 50 and ji from 0 collect
	    (let ((th (sb-thread:make-thread 
		   #'(lambda ()
		       (progn
			 (tilt-mirror j yj)
			 (loop for i below 3 collect
			      (destructuring-bind (cam success-p w h) 
				  (multiple-value-list (pylon:grab-cdf *cams* *buf-c*))
				(if success-p
				    (destructuring-bind (x y) (elt *first-orders* cam)
				      (destructuring-bind (hh ww) (elt *cam-sizes* cam)
					(assert (= ww w))
					(assert (= hh h))
					(fftw:ft *buf-c* :out-arg *out-c* :w w :h h)
					(let* ((q (.abs
						   (extract 
						    (make-array (list h w)
								:element-type '(complex double-float)
								:displaced-to *out-c*)
						    :x x :y y :w 66 :h 66)))
					       (v (.mean q)))
					  #+nil
					  (write-pgm8 (format nil "/dev/shm/o~d.pgm" cam)
						      (.uint8 
						       (.abs
							(extract
							 (make-array (list h w)
								     :element-type '(complex double-float)
								     :displaced-to *out-c*)
							 :x x :y y :w 66 :h 66))))
					  (format t "~a~%" (list cam j yj v))
					  (push (list j yj ji yji v (extract
							      (make-array (list h w)
									  :element-type '(complex double-float)
									  :displaced-to *out-c*)
							      :x x :y y :w 66 :h 66)) 
						(aref *bla* cam)))))
				    (format t "acquisition error.~%"))))))
		   :name "camera-acquisition")))
	  (sleep .01)
	  (trigger-all-cameras)
	  (sleep .01)
	  #+nil	(arduino-serial-sbcl:talk-arduino
		 (second *ard*) 
		 (first *ard*)
		 "(progn
 (digital-write 11 1)
 (delay 10)
 (digital-write 11 0)
 (delay 100)
 (digital-write 12 1)
 (delay 10)
 (digital-write 12 0)
 (delay 100)
 (digital-write 10 1)
 (delay 10)
 (digital-write 10 0)
")
	  (sb-thread:join-thread th))))
  (pylon:stop-grabbing *cams*))

#+nil
(time  (run))

;; 902s to run
;; 980s when extracted complex arrays are safed

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
 (let* ((h 
	 (1+ (loop for (j yj ji yji v im) in (butlast (aref *bla* 0)) maximize yji)))
	(w
	 (1+ (loop for (j yj ji yji v im) in (butlast (aref *bla* 0)) maximize ji)))
	(a (make-array (list h w 66 66) :element-type '(complex single-float))))
   (loop for (j yj ji yji v im) in (butlast (aref *bla* 0)) do
	(dotimes (jj 66)
	  (dotimes (ii 66)
	    (setf (aref a yji ji jj ii) (coerce (aref im jj ii) '(complex single-float))))))
   (ics:write-ics2 "/home/martin/scan.ics" a)))
