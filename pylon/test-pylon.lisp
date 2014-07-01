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
	  #p"/home/martin/arduino_due_lisp/pylon/"
	  #p"/home/martin/arduino_due_lisp/image-processing/"))
  (asdf:load-system "fftw")
  (asdf:load-system "pylon")
  (asdf:load-system "arduino-serial-sbcl")
  (asdf:load-system "image-processing"))

(defpackage :pylon-test
  (:use :cl :cffi :image-processing))

(in-package :pylon-test)



#+nil
(defparameter *ard* 
  (multiple-value-list
   (arduino-serial-sbcl:open-serial 
    (first (directory "/dev/ttyACM0")))))

(defun initialize-trigger-outputs ()
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   "(progn
 (pin-mode 10 1)
 (pin-mode 11 1)
 (pin-mode 12 1)"))

#+nil
(initialize-trigger-outputs)

#+nil
(defun trigger-all-cameras ()
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
  (arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
;; delay waits ms
   (format nil "(progn
 (dac ~a ~a)
 (delay 1000)
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))" x y)))

(fftw:prepare-threads)

(pylon:initialize)
(defparameter *fact* (pylon::factory))
(defparameter *cams* (pylon:create *fact* 3))

;; camera 0 Using device Basler acA1920-25gm#00305315DFDD#192.168.4.100:3956
;; camera 1 Using device Basler acA1920-25gm#00305315DFDE#192.168.5.102:3956
;; camera 2 Using device Basler acA1920-25gm#00305315DFC4#192.168.6.101:3956

;; correspondence between aravis and pylon cameras:
;; (set-region *cam2* :keep-old nil :h 1024 :w 1024 :x 452 :y 21) ;; pylon cam 0
;; (set-region *cam1* :keep-old nil :h 1024 :w 1024 :x 135 :y 0)  ;; pylon cam 2
;; (set-region *cam3* :keep-old nil :h 600 :w 600 :x 520 :y 213)  ;; pylon cam 1

;; 65 1024x1024+452+21
;; 40 1024x1024+135+0
;; 66 600x600+520+213


#+nil
((q1 (extract out1 :x (+ 33 867) :y (+ 33 243) :w 66 :h 66)) ; pylon2
 (q2 (extract out2 :x (+ 33 138) :y (+ 33 128) :w 66 :h 66)) ; pylon0
 (q3 (extract out3 :x (+ 33 193) :y (+ 33 -10) :w 66 :h 66))) ; pylon1

(defparameter *first-orders* `((,(+ 33 138) ,(+ 33 128))
			       (,(+ 33 193) ,(+ 33 -10))
			       (,(+ 33 867) ,(+ 33 243))))
(defparameter *cam-sizes* `((1024 1024)
			    (600 600)
			    (1024 1024)))

(pylon:cams-open *cams*)

(pylon:get-max-i *cams* 0 "OffsetX")
(pylon:get-max-i *cams* 0 "Width")
(loop for e in '("Width" "Height" "OffsetX" "OffsetY") collect
  (pylon:get-value-i *cams* 1 e t nil))
;; (pylon:get-min-i *cams* 1 "Width")
;; (pylon:get-value-e *cams* 1 "PixelFormat")
(pylon:get-symbolics-e *cams* 0 "PixelFormat")
(pylon:to-string-e *cams* 0 "PixelFormat")
(pylon:from-string-e *cams* 0 "PixelFormat" "Mono16")

(pylon:get-symbolics-e *cams* 0 "TriggerMode")

(pylon:get-value-e *cams* 0 "PixelFormat")
(pylon:get-value-e *cams* 2 "TriggerMode")
(pylon:set-value-e *cams* 2 "TriggerMode" 1)
(dotimes (i 3)
 (pylon:set-value-e *cams* i "TriggerMode" 1))

(dotimes (i 3)
 (pylon:set-value-e *cams* i "TriggerMode" 0))

(pylon:start-grabbing *cams*)



(let ((w 1024)
      (h 1024))
  (defparameter *buf-c1* (make-array (* 1024 1024) :element-type '(complex double-float)))
  (defparameter *out-c1* (make-array (* 1024 1024) :element-type '(complex double-float)))
  (defparameter *buf-c* (make-array (list 1024 1024) :element-type '(complex double-float)
				    :displaced-to *buf-c1*))
  (defparameter *out-c* (make-array (list 1024 1024) :element-type '(complex double-float)
				    :displaced-to *out-c1*))
  )

#+nil
(type-of *buf-c*)
#+nil
(sb-impl::array-header-p *buf-c*)
#+nil
(progn
  (handler-case
      (sb-ext:array-storage-vector *buf-c*)
    (simple-error ()
	(array-displacement *buf-c*)))
  nil)
#+nil
(defparameter *plan*
  (fftw::plan *buf-c* *out-c*))

#+nil
(fftw::%fftw_execute *plan*)

#+nil
(defparameter *bla*
 (fftw:ft (make-array (list 600 600)
		      :element-type '(complex double-float)
		      :displaced-to *buf-c*) :out-arg *out-c*))

#+nil
(image-processing:write-pgm8 "/dev/shm/o.pgm"
			     (image-processing:.uint8 
			      (image-processing:.log (image-processing:.abs
						      (make-array (list 600 600)
								  :element-type '(complex double-float)
								  :displaced-to *out-c*)))))

(mem-aref *buf* :unsigned-char 1)

#+nil
(trigger-all-cameras)

(/ 100 18.5)

(tilt-mirror-and-trigger-all-cameras 1550 2500)

;; 10 images in .9s    1.9s
;; 100 images in 10.5s 18.5s
(defparameter *bla*
 (loop for j from 800 below 2750 by 50 collect
      (progn
	(tilt-mirror-and-trigger-all-cameras j 2500)
	(loop for i below 3 collect
	     (destructuring-bind (cam success-p w h) 
		 (multiple-value-list (pylon:grab-cdf *cams* *buf-c*))
	       (when success-p
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
		       (write-pgm8 (format nil "/dev/shm/o~d.pgm" cam)
				   (.uint8 (.abs (extract (make-array (list h w)
								      :element-type '(complex double-float)
								      :displaced-to *out-c*)
							  :x x :y y :w 66 :h 66))))
		       (format t "~a~%" (list cam j v))
		      (list cam h w j v))))
		 (format t "acquisition not successful ~%")))))))

*bla*

(pylon:terminate *cams*)


;  // Width Height OffsetX OffsetY
;  // PixelFormat

#+nil (progn
  (pylon:initialize)
  (defparameter *cams* (pylon:create 2))
  (pylon:start-grabbing *cams*)

  (defparameter *buf*
    (foreign-alloc :unsigned-char :count (* 1040 1040)))
  (with-foreign-objects ((cam :int)
			 (success-p :int)
			 (w :int)
			 (h :int))
    (pylon:grab *cams* 1040 1040 *buf*
		cam success-p w h)
    (format nil "~a~%" (list (mem-ref cam :int)
			     (mem-ref success-p :int)
			     (mem-ref w :int)
			     (mem-ref h :int)))))

#+nil
(with-foreign-objects ((cam :int)
			 (success-p :int)
			 (w :int)
			 (h :int))
    (pylon:grab *cams* 1040 1040 *buf*
		cam success-p w h)
    (format nil "~a~%" (list (mem-ref cam :int)
			     (mem-ref success-p :int)
			     (mem-ref w :int)
			     (mem-ref h :int))))

