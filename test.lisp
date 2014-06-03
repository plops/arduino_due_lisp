(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry* '(*default-pathname-defaults*
				  #p"/home/martin/stage/cl-cffi-fftw3/"
				  #p"/home/martin/arduino_due_lisp/"
				  #p"/home/martin/arduino_due_lisp/arduino-serial-ccl/"))
  (asdf:load-system "fftw")
  (asdf:load-system "arv")
  (asdf:load-system "arduino")
  (ql:quickload "bordeaux-threads"))

(fftw:prepare-threads)


(defpackage :test-arv
  (:use :cl :arv :arduino))
(in-package :test-arv)

(defparameter *cam1* (make-instance 'camera :name "Basler-21433540"))
(defparameter *cam2* (make-instance 'camera :name "Basler-21433565"))
(defparameter *cam3* (make-instance 'camera :name "Basler-21433566"))

;; trigger
#+nil
(talk-arduino "(pin-mode 11 1)") ;; 40 cam1
#+nil
(talk-arduino "(pin-mode 12 1)") ;; 65 cam2
#+nil
(talk-arduino "(pin-mode 10 1)") ;; 66 cam3



(get-statistics *cam3*)

(progn
 (loop for c in (list *cam1* *cam2* *cam3*) do
      (set-acquisition-mode c 'continuous #+nil 'single-frame
			    )
      (gc-enumeration-set-int-value c "TriggerMode" 1)
      (set-pixel-format c "Mono12Packed"))
 (set-region *cam2* :keep-old nil :h 1024 :w 1024 :x 452 :y 21)
 (set-region *cam1* :keep-old nil :h 1024 :w 1024 :x 135 :y 0)
 (set-region *cam3* :keep-old nil :h 600 :w 600 :x 520 :y 213))

(get-n-buffers *cam3*)
(ensure-at-least-one-buffer-in-stream *cam3*)

(push-buffer *cam3*)

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

#+nil
(ccl:use-interface-dir :libc)

(defun get-universal-time-usec () ;; why does this not work?
  "Return a single integer for the current time of
   day in universal time format in microseconds."
  (rlet ((tv :timeval))
    (ccl::gettimeofday tv)
    (+ (pref tv :timeval.tv_usec)
       (* 1000000 (pref tv :timeval.tv_sec) ccl::unix-to-universal-time))))
#+nil
(get-universal-time-usec)


#+nil
(defparameter *BLA3* 
  (progn
    (start-acquisition *cam3*)
    (prog1
     (loop for i below 20 collect
	  (progn
	    (format t "waiting for image ~a" (get-universal-time))
	    (pop-block-copy-push-buffer *cam3* :use-dark nil)
	    (format t ".~%")
	    ))
      (stop-acquisition *cam3*))))

(bordeaux-threads:make-thread 
 (lambda ()
   (format t "sending trigger~%")
   (talk-arduino
    (format nil "(progn
 (delay 10)
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))"))))


(talk-arduino
	       (format nil "(progn
 (delay 10)
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))"))

; cam1 first order is at 66x66+867+243 (measured in fiji)
 ; (extract *blau* :x (+ 33 867) :y (+ 33 243) :w 66 :h 66)
; cam2 first order is at 66x66+138+128 (measured in fiji)
 ; (extract *blau* :x (+ 33 138) :y (+ 33 128) :w 66 :h 66)
; cam3 first order is at 66x66+193-10 (measured in fiji)
 ; (extract *blau* :x (+ 33 193) :y (+ 33 -10) :w 66 :h 66)


;; fiber center first coordinate: 800 .. 1550 .. 2750
;; fiber center second coordinate: 1800 .. 2500  .. 3580

(list (- 3580 1800)
      (- 2750 800)
      (* .5 (+ 3580 1800)))

(progn
  (let ((w (arv::aoi-width *cam3*))
	(h (arv::aoi-height *cam3*)))
    (let ((in (fftw:make-foreign-complex-array-as-double (list h w)))
	  (out (fftw:make-foreign-complex-array-as-double (list h w))))
      (start-acquisition *cam3*)
      (defparameter *bla*
	(loop for i from 1500 upto 4000 by 20 collect
	    (progn
	      (bordeaux-threads:make-thread
	       (lambda () 
		 (sleep .02)
		 (format t "sending trigger~%")
		 (talk-arduino
		  (format nil "(progn
 (dac 1550 ~d)
 (delay 10)
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))" i)))
	       :name "mythread")
	      (format t "waiting for image .. ")
	      (let ((im (pop-block-copy-push-buffer *cam3* :use-dark nil)))
		(format t "image arrived~%")
		(dotimes (i w)
		  (dotimes (j h)
		    (setf (aref in j i 0) (* 1d0 (aref im j i)))))
		(fftw:ft in out)
		(let ((v (.mean (extract (.abs* out) :x (+ 33 193) :y (+ 33 -10) :w 66 :h 66))))
		  (format t "~a~%" (list i v))
		  (list i v))))))))
  (stop-acquisition *cam3*)
  (format t "finished2"))

(with-open-file (f "/dev/shm/o.dat" :direction :output
		   :if-exists :supersede :if-does-not-exist :create)
  (loop for (i e) in *bla*  do (format f "~d ~d~%" i (floor e))))


(loop for i in (list *cam1* *cam2* *cam3*) do
     (destroy-stream i))

(destroy-stream *cam3*)



(loop for i in (list *cam1* *cam2* *cam3*) do
     (gc-enumeration-set-int-value i "TriggerMode" 1))

(gc-enumeration-get-int-value *cam3* "AcquisitionMode")
