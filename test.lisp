(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry* '(*default-pathname-defaults*
				  #p"/home/martin/stage/cl-cffi-fftw3/"
				  #p"/home/martin/arduino_due_lisp/"
				  #p"/home/martin/arduino_due_lisp/arduino-serial-ccl/"))
  (asdf:load-system "fftw")
  (asdf:load-system "arv")
  (asdf:load-system "arduino"))

(fftw:prepare-threads)

(defpackage :test-arv
  (:use :cl :arv :arduino))
(in-package :test-arv)

(defparameter *cam1* (make-instance 'camera :name "Basler-21433540"))
(defparameter *cam2* (make-instance 'camera :name "Basler-21433565"))
(defparameter *cam3* (make-instance 'camera :name "Basler-21433566"))

(get-statistics *cam1*)

(progn
 (loop for c in (list *cam1* *cam2* *cam3*) do
      (set-acquisition-mode c 'single-frame)
      (set-pixel-format c "Mono12Packed")
      )
 (set-region *cam2* :keep-old nil :h 1024 :w 1024 :x 452 :y 21)
 (set-region *cam1* :keep-old nil :h 1024 :w 1024 :x 135 :y 0)
 (set-region *cam3* :keep-old nil :h 600 :w 600 :x 520 :y 213))

(get-n-buffers *cam3*)
(ensure-at-least-one-buffer-in-stream *cam3*)

(push-buffer *cam3*)

#+nil
(defparameter *BLA3* (loop for i below 100 collect
			  (prog1
			   (acquire-single-image *cam3* :use-dark nil)
			  (talk-arduino
	       (format nil "(progn
 (delay 10)
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))")))))


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

(let ((first (acquire-single-image *cam3* :use-dark nil)))
  (destructuring-bind (h w) (array-dimensions first)
    (let ((in (fftw:make-foreign-complex-array-as-double (list h w)))
	  (out (fftw:make-foreign-complex-array-as-double (list h w))))
      (defparameter *bla*
       (loop for i from 1500 upto 4000 by 20 collect
	    (progn
	      (sleep .02)
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
 (digital-write 10 0))" i))
	      (let ((im (acquire-single-image *cam3* :use-dark nil)))
		(dotimes (i w)
		  (dotimes (j h)
		    (setf (aref in j i 0) (* 1d0 (aref im j i)))))
		(fftw:ft in out)
		(let ((v (.mean (extract (.abs* out) :x (+ 33 193) :y (+ 33 -10) :w 66 :h 66))))
		  (format t "~a~%" (list i v))
		  (list i v))))))))
  (format t "finished2"))

(with-open-file (f "/dev/shm/o.dat" :direction :output
		   :if-exists :supersede :if-does-not-exist :create)
  (loop for (i e) in *bla*  do (format f "~d ~d~%" i (floor e))))
