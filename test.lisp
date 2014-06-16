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

(defvar *cam1* nil)
(defvar *cam2* nil)
(defvar *cam3* nil)

(defun open-cameras ()
  (unless *cam1*
   (sleep .1)
   (setf *cam1* (make-instance 'camera :name "Basler-21433540"))
   (sleep .1)
   (setf *cam2* (make-instance 'camera :name "Basler-21433565"))
   (sleep .1)
   (setf *cam3* (make-instance 'camera :name "Basler-21433566"))))

;; trigger
#+nil
(progn
  (talk-arduino "(pin-mode 11 1)") ;; 40 cam1
  (talk-arduino "(pin-mode 12 1)") ;; 65 cam2
  (talk-arduino "(pin-mode 10 1)")) ;; 66 cam3




(defvar *cameras-initialized*  nil)
(defun init-cameras ()
  (unless *cam1*
    (open-cameras))
  (unless *cameras-initialized*
    (setf *cameras-initialized* t)
    (loop for c in (list *cam1* *cam2* *cam3*) do
	 (sleep .1)
	 (set-acquisition-mode c 'continuous #+nil 'single-frame
			       )
	 (gc-enumeration-set-int-value c "TriggerMode" 1)
	 (set-pixel-format c "Mono12Packed"))
    (set-region *cam2* :keep-old nil :h 1024 :w 1024 :x 452 :y 21)
    (set-region *cam1* :keep-old nil :h 1024 :w 1024 :x 135 :y 0)
    (set-region *cam3* :keep-old nil :h 600 :w 600 :x 520 :y 213)))

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
(loop for i below 100 do
     (sleep .2)
 (trigger-all-cameras))


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
(talk-arduino
 "(dac 1550 2500)")


#+nil
(list (- 3580 1800)
      (- 2750 800)
      (* .5 (+ 3580 1800)))

(defparameter *use-aravis* t)
#+nil
(progn
  (trigger-all-cameras)
  (when *use-aravis* (init-cameras))      
  (sleep .2)
  (format t "cameras initialized~%")
  (let ((s1 (if *use-aravis*
		(list (arv::aoi-height *cam1*) (arv::aoi-width *cam1*))
		(list 10 10)))
	(s2 (if *use-aravis*
		(list (arv::aoi-height *cam2*) (arv::aoi-width *cam2*))
		(list 10 10)))
	(s3 (if *use-aravis*
		(list (arv::aoi-height *cam3*) (arv::aoi-width *cam3*))
		(list 10 10))))
    (let ((in1 (fftw:make-foreign-complex-array-as-double s1))
	  (out1 (fftw:make-foreign-complex-array-as-double s1))
	  (in2 (fftw:make-foreign-complex-array-as-double s2))
	  (out2 (fftw:make-foreign-complex-array-as-double s2))
	  (in3 (fftw:make-foreign-complex-array-as-double s3))
	  (out3 (fftw:make-foreign-complex-array-as-double s3))
	  (start (get-universal-time-usec)))
      (prepare-mosaic 1 (+ 1 (floor (- 4000 0) 100)) 66 66)
      (when *use-aravis*
	(sleep 1)
	(dotimes (i 10)
	  (push-buffer *cam1*)
	  (push-buffer *cam2*)
	  (push-buffer *cam3*))
	(format t "10 buffers sent to each camera~%")
	(sleep .1)
	(start-acquisition *cam1*)
	(start-acquisition *cam2*)
	(start-acquisition *cam3*))
      (format t "acquisitions started~%")
      (defparameter *bla*
	(loop for i from 1800 upto 3580 by 20 collect
	     (progn
	       ;; move mirror
	       (talk-arduino (format nil "(dac 1480 ~d)" i))
	       ;; tell cameras to wait for trigger signal
	       (let ((t1 (bordeaux-threads:make-thread 
			  (lambda ()
			    (when *use-aravis*
			      (pop-block-copy-push-buffer-mono12p-cdf *cam1* in1)))
			  :name "blocking_cam1"))
		     (t2 (bordeaux-threads:make-thread 
			  (lambda () 
			    (when *use-aravis*
			      (pop-block-copy-push-buffer-mono12p-cdf *cam2* in2)))
			  :name "blocking_cam2"))
		     (t3 (bordeaux-threads:make-thread 
			  (lambda ()
			    (when *use-aravis* 
			      (pop-block-copy-push-buffer-mono12p-cdf *cam3* in3)))
			  :name "blocking_cam3")))
		 ;; send the trigger signal
		 (talk-arduino "(progn
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))")
		 ;; wait until all cameras have sent their data
		 (bordeaux-threads:join-thread t1)
		 (bordeaux-threads:join-thread t2)
		 (bordeaux-threads:join-thread t3)
		 (sleep .1)
		 ;; (write-pgm (format nil "/dev/shm/o1_~4,'0d.pgm" i) (.uint16 (.abs* in1)))
		 ;; (write-pgm (format nil "/dev/shm/o2_~4,'0d.pgm" i) (.uint16 (.abs* in2)))
		 ;; (write-pgm (format nil "/dev/shm/o3_~4,'0d.pgm" i) (.uint16 (.abs* in3)))
		 (let ((atime (- (get-universal-time-usec) start)))
		   (format t "~a~%" (list (multiple-value-list (get-n-buffers *cam1*))
					  (multiple-value-list (get-n-buffers *cam2*))
					  (multiple-value-list (get-n-buffers *cam3*))))
		   (fftw:ft in1 out1)
		   (fftw:ft in2 out2)
		   (fftw:ft in3 out3)
		   (let* ((q1 (.abs (extract-cdf* out1 :x (+ 33 867) :y (+ 33 243) :w 66 :h 66)))
			  (v1 (.mean q1))
			  (q2 (.abs (extract-cdf* out2 :x (+ 33 138) :y (+ 33 128) :w 66 :h 66)))
			  (v2 (.mean q2))
			  (q3 (.abs (extract-cdf* out3 :x (+ 33 193) :y (+ 33 -10) :w 66 :h 66)))
			  (v3 (.mean q3)))
		     
		     ;; (write-pgm (format nil "/dev/shm/ko1_~4,'0d.pgm" i) (.uint16 (.log (.abs* out1))))
		     ;; (write-pgm (format nil "/dev/shm/ko2_~4,'0d.pgm" i) (.uint16 (.log (.abs* out2))))
		     ;; (write-pgm (format nil "/dev/shm/ko3_~4,'0d.pgm" i) (.uint16 (.log (.abs* out3))))


		     ;; (write-pgm (format nil "/dev/shm/sko1_~4,'0d.pgm" i) (.uint16 q1))
		     ;; (write-pgm (format nil "/dev/shm/sko2_~4,'0d.pgm" i) (.uint16 q2))
		     ;; (write-pgm (format nil "/dev/shm/sko3_~4,'0d.pgm" i) (.uint16 q3))
		     (fill-mosaic 0 (floor i 100) q3)
		     (format t "~a~%" (list i v1 v2 v3 atime))
		     (list i v1 v2 v3 atime)))))))
      (defparameter *bla-mosaic* (get-mosaic))))
  (when *use-aravis* (stop-acquisition *cam1*)
	(stop-acquisition *cam2*)
	(stop-acquisition *cam3*))
  (talk-arduino "(progn
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))")
  (format t "finished2~%"))

#+nil
(write-pgm "/dev/shm/o.pgm" (.uint16 *bla-mosaic*))

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

#+nil 
(set-exposure *cam1* 5000d0)
#+nil
(list (get-exposure *cam1*)
      (get-exposure *cam2*)
      (get-exposure *cam3*))
#+nil
(get-region *cam1*)
#+nil
(stop-acquisition *cam1*)
#+nil
(set-region *cam1* :keep-old nil :h 1024 :w 1024 :x 135 :y 0)
#+nil
(open-cameras)
#+nil
(init-cameras)

(let ((mosaic nil))
  (defun prepare-mosaic (h w hh ww)
    (setf mosaic (make-array (list (* h hh) (* w ww))
			     :element-type 'double-float)))
  (defun fill-mosaic (jj ii a)
    (destructuring-bind (h w) (array-dimensions mosaic)
      (destructuring-bind (hh ww) (array-dimensions a)
	(assert (<= 0 jj))
	(assert (<= 0 ii))
	(assert (<= ii (floor w ww)))
	(assert (<= jj (floor h hh)))
	(dotimes (j hh)
	  (dotimes (i ww)
	    (setf (aref mosaic (+ (* jj hh) j) (+ (* ii ww) i))
		  (aref a j i)))))))
  (defun get-mosaic ()
    mosaic))

