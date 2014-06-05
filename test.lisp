(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry* '(*default-pathname-defaults*
				  #p"/home/martin/stage/cl-cffi-fftw3/"
				  #p"/home/martin/arduino_due_lisp/"
				  #p"/home/martin/arduino_due_lisp/arduino-serial-ccl/"))
  (asdf:load-system "fftw")
  (asdf:load-system "arv")
  (asdf:load-system "arduino")
  (ql:quickload "bordeaux-threads"))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (fftw:prepare-threads))

(defpackage :test-arv
  (:use :cl :arv :arduino))
(in-package :test-arv)

(defparameter *cam1* nil)
(defparameter *cam2* nil)
(defparameter *cam3* nil)

(defun open-cameras ()
  (sleep .1)
  (defparameter *cam1* (make-instance 'camera :name "Basler-21433540"))
  (sleep .1)
  (defparameter *cam2* (make-instance 'camera :name "Basler-21433565"))
  (sleep .1)
  (defparameter *cam3* (make-instance 'camera :name "Basler-21433566")))

;; trigger
#+nil
(progn
  (talk-arduino "(pin-mode 11 1)") ;; 40 cam1
  (talk-arduino "(pin-mode 12 1)") ;; 65 cam2
  (talk-arduino "(pin-mode 10 1)")) ;; 66 cam3


(defun init-cameras ()
 (loop for c in (list *cam1* *cam2* *cam3*) do
      (sleep .1)
      (set-acquisition-mode c 'continuous #+nil 'single-frame
			    )
      (gc-enumeration-set-int-value c "TriggerMode" 1)
      (set-pixel-format c "Mono12Packed"))
 (set-region *cam2* :keep-old nil :h 1024 :w 1024 :x 452 :y 21)
 (set-region *cam1* :keep-old nil :h 1024 :w 1024 :x 135 :y 0)
 (set-region *cam3* :keep-old nil :h 600 :w 600 :x 520 :y 213))

#+nil
(progn
  (let ((i 3))
   (bordeaux-threads:make-thread (lambda () (sleep 1)
					 (format t "bla ~d~%" i))
				 :name "mythread"))
  (bordeaux-threads:make-thread (lambda () (format t "bla2~%"))
			       :name "mythread"))


#+nil
(defparameter *BLA3* (loop for i below 1 collect
			  (progn
			      (format t "waiting for image~%")
			      (acquire-single-image *cam3* :use-dark nil)
			    )))


(defun get-universal-time-usec () ;; why does this not work?
  "Return a single integer for the current time of
   day in universal time format in microseconds."
  (ccl:rlet ((tv :timeval))
    (ccl::gettimeofday tv)
    (+ (ccl:pref tv :timeval.tv_usec)
       (* 1000000 (+ (ccl:pref tv :timeval.tv_sec) ccl::unix-to-universal-time)))))
#+nil
(get-universal-time-usec)

(defun trigger-all-cameras ()
 (talk-arduino
  "(progn
 (pin-mode 10 1)
 (pin-mode 11 1)
 (pin-mode 12 1)
 (delay 10)
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))"))

#+nil
(trigger-all-cameras)

#+nil
(defparameter *BLA3* 
  (progn
    (start-acquisition *cam3*)
    (prog1
     (loop for i below 3 collect
	  (progn
	    (format t "waiting for image ~a" (get-universal-time-usec))
	    (prog1 (pop-block-copy-push-buffer *cam3* :use-dark nil)
	    (format t ".~%"))
	    ))
      (stop-acquisition *cam3*))))

#+nil
(progn ;; shutter laser off
  (talk-arduino "(pin-mode 8 1)")
  (talk-arduino "(digital-write 8 0)"))
#+nil
(progn ;; shutter laser on
  (talk-arduino "(pin-mode 8 1)")
  (talk-arduino "(digital-write 8 1)"))



; cam1 first order is at 66x66+867+243 (measured in fiji)
 ; (extract *blau* :x (+ 33 867) :y (+ 33 243) :w 66 :h 66)
; cam2 first order is at 66x66+138+128 (measured in fiji)
 ; (extract *blau* :x (+ 33 138) :y (+ 33 128) :w 66 :h 66)
; cam3 first order is at 66x66+193-10 (measured in fiji)
 ; (extract *blau* :x (+ 33 193) :y (+ 33 -10) :w 66 :h 66)


;; fiber center first coordinate:   800 .. 1550 .. 2750
;; fiber center second coordinate: 1800 .. 2500 .. 3580

#+nil
(list (- 3580 1800)
      (- 2750 800)
      (* .5 (+ 3580 1800)))
#+nil
(progn
  (trigger-all-cameras)
  (sleep .2)
  (time (open-cameras))
  (sleep .2)
  (time (init-cameras))
  (sleep .2)
  (let ((s1 (list (arv::aoi-height *cam1*) (arv::aoi-width *cam1*)))
	(s2 (list (arv::aoi-height *cam2*) (arv::aoi-width *cam2*)))
	(s3 (list (arv::aoi-height *cam3*) (arv::aoi-width *cam3*))))
    (let ((in1 (fftw:make-foreign-complex-array-as-double s1))
	  (out1 (fftw:make-foreign-complex-array-as-double s1))
	  (in2 (fftw:make-foreign-complex-array-as-double s2))
	  (out2 (fftw:make-foreign-complex-array-as-double s2))
	  (in3 (fftw:make-foreign-complex-array-as-double s3))
	  (out3 (fftw:make-foreign-complex-array-as-double s3))
	  (start (get-universal-time-usec)))
      (start-acquisition *cam1*)
      (start-acquisition *cam2*)
      (start-acquisition *cam3*)
      (format t "acquisitions started~%")
      (defparameter *bla*
	(loop for i from 0 upto 4000 by 100 collect
	    (progn

	      ;; wait for picture on all cameras
	      (let ((t1 (bordeaux-threads:make-thread 
			  (lambda () (pop-block-copy-push-buffer-mono12p-cdf *cam1* in1))))
		     (t2 (bordeaux-threads:make-thread 
			  (lambda () (pop-block-copy-push-buffer-mono12p-cdf *cam2* in2))))
		     (t3 (bordeaux-threads:make-thread 
			  (lambda () (pop-block-copy-push-buffer-mono12p-cdf *cam3* in3)))))
		;; send the trigger signal
		(talk-arduino
		 (format nil "(progn
 (dac 1480 ~d)
 (delay 50)
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))" i))
		;; wait until all cameras have sent their data
		(bordeaux-threads:join-thread t1)
		(bordeaux-threads:join-thread t2)
		(bordeaux-threads:join-thread t3)
	       (let ((atime (- (get-universal-time-usec) start)))
		 (fftw:ft in1 out1)
		 (fftw:ft in2 out2)
		 (fftw:ft in3 out3)
		 (let ((v1 (.mean (.abs (extract-cdf* out1 :x (+ 33 867) :y (+ 33 243) :w 66 :h 66))))
		       (v2 (.mean (.abs (extract-cdf* out2 :x (+ 33 138) :y (+ 33 128) :w 66 :h 66))))
		       (v3 (.mean (.abs (extract-cdf* out3 :x (+ 33 193) :y (+ 33 -10) :w 66 :h 66)))))
		   (format t "~a~%" (list i v1 v2 v3 atime))
		   (list i v1 v2 v3 atime)))))))))
  (stop-acquisition *cam1*)
  (stop-acquisition *cam2*)
  (stop-acquisition *cam3*)
  (format t "finished2~%"))

#+nil
(with-open-file (f "/dev/shm/o.dat" :direction :output
		   :if-exists :supersede :if-does-not-exist :create)
  (loop for (i e1 e2 e3 tim) in *bla*  do (format f "~d ~g ~g ~g~%" i e1 e2 e3)))

#+nil
(loop for i in (list *cam1* *cam2* *cam3*) do
     (destroy-stream i))

#+nil
(loop for i in (list *cam1* *cam2* *cam3*) do
     (gc-enumeration-set-int-value i "TriggerMode" 1))

