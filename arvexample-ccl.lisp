;; Run C-C C-k to load this file into slime
#.(load "/home/martin/quicklisp/setup.lisp")
(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cffi))

(declaim (optimize (speed 0) (safety 3) (debug 3)))

#+nil 
(progn ;; this only needs to be called once, to run h-to-ffi.sh
       ;; (ffigen) and generate the cdb foreign function database
       ;; files
  (require :parse-ffi)
  (ccl::parse-standard-ffi-files :arv)
  (ccl::parse-standard-ffi-files :v4l2))

#.(ccl:use-interface-dir :arv)
(cffi:load-foreign-library "libaravis-0.4.so")
;ccl::*shared-libraries*
;ccl::*eeps* ;; hash table with external functions

;; before any other function we have to call g_type_init from
;; /usr/include/glib-2.0/gobject/gtype.h
(#_g_type_init)

#+nil
(#_arv_update_device_list)
#+nil
(#_arv_get_n_devices)
#+nil
(#_arv_get_n_interfaces)
#+nil
(char*-to-lisp
 (#_arv_get_interface_id 1))
#+nil
(char*-to-lisp
 (#_arv_get_device_id 0))







;; if no camera is connected, make sure to call arv-fake-gv-camera-0.4
#+nil
(defparameter *cam1* (make-instance 'camera :name "Basler-21433540"))
#+nil
(set-pixel-format *cam1* "Mono12Packed")
#+nil
(set-acquisition-mode *cam1* 'single-frame)
#+nil
(defparameter *bla* (acquire-single-image *cam1* :use-dark nil))
;; 65 1024x1024+452+21
;; 40 1024x1024+135+0
;; 66 600x600+520+213
#+nil
(progn
 (loop for c in (list *cam1* *cam2* *cam3*) do
      (set-acquisition-mode c 'single-frame)
      (set-pixel-format c "Mono12Packed")
      )
 (set-region *cam2* :keep-old nil :h 1024 :w 1024 :x 452 :y 21)
 (set-region *cam1* :keep-old nil :h 1024 :w 1024 :x 135 :y 0)
 (set-region *cam3* :keep-old nil :h 600 :w 600 :x 520 :y 213))
#+nil
(defparameter *bla*
 (loop for c in (list *cam1* *cam2* *cam3*) collect
      (acquire-image-using-full-range c :use-dark nil)))

#+nil
(loop for e in *bla* and i from 0 do
     (write-pgm (format nil "/dev/shm/~d.pgm" i)
		e))

#+nil
(get-region *cam1*)
#+nil
(set-region *cam1* :keep-old nil :h 1024 :w 1024 :y 23 :x 0)

#+nil
(defparameter *cam1* (make-instance 'camera :name "Basler-21433540"))
#+nil
(defparameter *cam2* (make-instance 'camera :name "Basler-21433565"))
#+nil
(defparameter *cam3* (make-instance 'camera :name "Basler-21433566"))
#+nil
(set-region *cam1* :x 103 :y 379 :w 800 :h 64)
#+nil
(set-pixel-format *cam1* "Mono12Packed")
#+nil
(set-acquisition-mode *cam1* 'continuous)
#+nil
(set-exposure *cam1* 2105d0)
#+nil
(set-packet-size *cam1* 1400)
#+nil
(get-packet-size *cam1*)
#+nil
(defparameter *bla* ;; 300 images in 1.9s
  (progn
    (start-acquisition *cam1*)
    (prog1
	(let ((buf (pop-block-copy-push-buffer *cam1* :use-dark nil)))
	  (time
	   (loop for i below 3000 do
		(pop-block-copy-push-buffer *cam1* :out buf :use-dark nil))))
      (stop-acquisition *cam1*))))

#+nil
(#_arv_shutdown)

#+nil
(destroy-stream *cam1*)

#+nil
(progn
  (destroy-stream *cam1*)
  (destroy-stream *cam2*)
  (destroy-stream *cam3*))

#+nil
(set-pixel-format *cam1* "Mono8")
#+nil
(progn  (set-region *cam1* :x 712 :y 712 :w 600 :h 600)
	(loop for c in (list *cam1*) and i from 1 do 
	     (set-exposure c 4000d0)
	     (set-acquisition-mode c 'single-frame)
	     (set-pixel-format c "Mono12Packed")
	     (push-buffer c)
	     (write-pgm (format nil "/dev/shm/~d.pgm" i)
			(acquire-single-image c :use-dark nil))))

#+nil
(time
 (let ((count 0))
   (acquire-continuous-images *cam1* :use-dark nil :fun #'(lambda (im)
							    (write-pgm (format nil "/dev/shm/~3,'0d.pgm" count) im)
							    (when (< (incf count) 100)
							      t)))))
;; 23.9s

#+nil
(dotimes (i 100)
 (write-pgm (format nil "/dev/shm/~d.pgm" 1)
	    (acquire-single-image *cam1* :use-dark nil)))

#+nil
(defparameter *cam2*
	   (make-instance 'camera :name "Basler-21211553"))

;; network card has to be configured like this:
;; ifconfig enp0s7 169.254.140.1
#+nil
(progn
  (progn (defparameter *cam2*
	   (make-instance 'camera :name "Basler-21211553"))
	 
	 (defparameter *cam1*
	   (make-instance 'camera)))
  (set-region-centered *cam1* :cx 800 :cy 1024 :w 512 :h 512)
  (gc-enumeration-set-int-value *cam1* "Correction_Mode" 0)
  (set-region *cam2* :x (- 659 512) :w 512  :h 494)
  (loop for c in (list *cam1* *cam2*) and i from 1 do 
       (set-exposure c 1000d0)
       (set-acquisition-mode c 'single-frame)
       (set-pixel-format c "Mono12Packed")
       (ensure-at-least-one-buffer-in-stream c)
       (write-pgm (format nil "/dev/shm/~d.pgm" i)
		  (acquire-single-image c :use-dark nil))))

#+nil
(set-region-centered *cam1* :cx 790 :cy 1000 :w 256 :h 256)
#+nil
(set-region-centered *cam2* :cx 400 :cy 270 :w 390  :h 390)


#+nil
(loop for c in (list *cam1* *cam2*) and i from 1 do 
       (set-exposure c 4000d0)
       (set-acquisition-mode c 'single-frame)
       (set-pixel-format c "Mono12Packed")
       (push-buffer c)
       (write-pgm (format nil "/dev/shm/~d.pgm" i)
		  (acquire-single-image c :use-dark t)))




#+nil
(list (set-exposure *cam2* 1d0) (get-exposure *cam2*))
#+nil
(get-exposure *cam1*)
#+nil
(set-exposure *cam1* 2000d0)

#+nil
(progn
  (setf (slot-value *cam1* 'dark-image) (acquire-single-image *cam1* :use-dark nil))
 nil)

#+nil
(set-region *cam1*)
#+nil
(set-pixel-format *cam1* "Mono12Packed")
#+nil
(defparameter *bla* (acquire-single-image *cam1*))
#+nil
(set-acquisition-mode *cam1* 'single-frame)
#+nil
(set-acquisition-mode *cam2* 'single-frame)
#+nil
(let ((a (make-array (reduce #'* (array-dimensions *bla*))
		     :element-type (array-element-type *bla*)
		     :displaced-to *bla*)))
  (list
   (reduce #'max a)
   (reduce #'min a)))
#+nil
(write-pgm "/dev/shm/2.pgm" (acquire-single-image *cam2*))

#+nil
(temperatures *cam1*)
#+nil
(progn
  ;(delete-file "/dev/shm/1.pgm")
  ;(delete-file "/dev/shm/2.pgm")
  (loop for c in (list *cam1* *cam2*) and i from 1 do 
       (set-exposure c 3000d0)
       (set-acquisition-mode c 'single-frame)
       (set-pixel-format c "Mono12Packed")
       (talk-arduino "(dac 1000 2000)")
       (write-pgm (format nil "/dev/shm/~d.pgm" i)
		  (acquire-single-image c))))

#+nil
(set-exposure *cam2* 300d0)
#+nil
(set-exposure *cam1* 900d0)
#+nil 
(+ 19 25 15)
#+nil
(+ 30 3.5 2.5 13.5 9) ;; path camera 2 (good interference)

#+nil
(talk-arduino "(dac 1041 2047)")
#+nil
(talk-arduino "(+ 2000 2047)")
#+nil
(talk-arduino "(+ 2 2)")
#+nil
(progn
  (loop for i from 600 upto 2000 by 10 do
       (sleep .02)
      (talk-arduino
       (format nil "(progn
 (dac ~d 2047)
 (delay 10)
 (digital-write 11 1)
 (digital-write 12 1) 
 (digital-write 10 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0)
 (digital-write 10 0))" i))
      )
  (format t "finished2"))

#+nil
(talk-arduino "(dac 2048 2148)")

#+nil
(dotimes (i 10)
  (sleep .2)
  (talk-arduino "(progn
 (digital-write 11 1)
 (digital-write 12 1) 
 (delay 10) 
 (digital-write 11 0)
 (digital-write 12 0))"))

;; fsm moves +/- 1.5 degree, (26.2 mrad) for voltages in +/- 10v
;; (* 150 (tan 26.2e-3)) corresponds to 3.9 mm after 150mm



#+nil
(progn
  (talk-arduino "(dac 2047 2047)")
 (dotimes (j 128)
   (loop for c in (list *cam1* *cam2*) and i from 1 do 
	(format t ".")
	(write-pgm (format nil "/dev/shm/~3,'0d_~d.pgm" j i)
		   (if nil
		       (average-images c :number 100 :use-dark t)
		       (acquire-single-image c :use-dark t))))))

(+ 21 24 11)

#+nil
(gc-enumeration-get-int-value *cam1* "Correction_Mode")
#+nil
(gc-enumeration-set-int-value *cam1* "Correction_Mode" 0)


#+nil
(read-line *serial*)

#+nil
(talk-arduino "(+ 1 2)")

;; pin 8 is connected to thorlab shutter controller (shows x-gate and enabled)

#+nil
(talk-arduino "(pin-mode 8 1)") ;; set pin 8 to output

#+nil
(talk-arduino "(digital-write 8 0)") ;; set pin 8 to high

#+nil
(talk-arduino "(dac 2047 2047)")

;; length of fiber 85cm
;; 18.5 36
(+ 18.5 3.6 (* 1.5 85) 32) ;; red camera through fiber
(+ 18.5 3.6 (* 1.5 85) 11 22)

;; with reflector prism at 8.3cm
(+ 28 2.5 17.5 60 11.5 27.2 11) ;; red camera reference 
(+ 28 2.5 17.5 60 13.5 27.7 9.3) ;; small black camera reference

(+ 8.3 (* .5 (- 181.2 157.7))) 

#+nil
(talk-arduino (format nil "(progn (dac ~d ~d) (delay 10) (print (adc 0)))" (+ 2048) (+ 2048)))
#+nil
(talk-arduino (format nil "(adc 0)"))


#+nil
(dotimes (i 10)
 (let ((c (complex (+ 1200d0) 2047d0))
       (r 1150d0)
       (n (* 12)))
   (prog1
       (loop for i below n collect 
	    (let ((z (+ c (* r (exp (complex 0d0 (* 2 pi i (/ 1d0 n))))))))
					;(sleep .05)
	      (let ((cmd (format nil "(dac ~d ~d)" 
				 (floor (min 4095 (max 0 (realpart z))))
				 (floor (min 4095 (max 0 (imagpart z)))))))
		(talk-arduino cmd)
		cmd)))
     #+nil
     (talk-arduino  (format nil "(dac ~d ~d)" (floor (realpart c)) (floor (imagpart c)))))))

#+nil
(let ((res nil)
      (ic 1200)
      (ir 1100)
      (jc 2047)
      (jr 1100))
 (loop for j from (- jc jr) upto (+ jc jr) by 100 do
      (loop for i from (- ic ir) upto (+ ic ir) by 100 do
	   (talk-arduino (format nil "(dac ~d ~d)" i j))
	   (let* ((a (acquire-single-image *cam1*))
		  (a1 (make-array (reduce #'* (array-dimensions a))
				  :element-type (array-element-type a)
				  :displaced-to a))
		  (val (reduce #'+ a1)))
	     (push (list i j val) res)
	     (format t "~a~%" (list i j val)))))
 (defparameter *scan* (reverse res)))

#+nil
(sort (copy-list *scan*) #'< :key #'third)

#+nil
(defparameter *scan1* *scan*)



#+nil
(defparameter *bla*
 (average-images *cam1*))

#+nil
(talk-arduino (format nil "(dac ~d 2047)" (+ (* 40 15) 2047)) )


;; in a run that worked cam1 206.22 .. 9930.9 and cam2 22.0 .. 2271


#+nil
(set-exposure *cam2* 400d0)

#+nil
(get-n-buffers *cam2*)
#+nil
(start-acquisition *cam2*)
#+nil
(ensure-no-threads-waiting-for-buffer *cam2*)
#+nil
(push-buffer *cam2*)

#+nil
(dotimes (i 1000)
  ;(sleep .2)
 (progn
   (format t "~a~%" (list (get-statistics *cam1*) (multiple-value-list (get-n-buffers *cam1*))
			  (get-statistics *cam2*) (multiple-value-list (get-n-buffers *cam2*))))
   (write-pgm "/dev/shm/1.pgm" (acquire-single-image *cam1* :use-dark t) #+nil (acquire-image-using-full-range *cam1*))
   (write-pgm "/dev/shm/2.pgm" (acquire-single-image *cam2*) #+nil (acquire-image-using-full-range *cam2*))))


#+nil
(progn
  (talk-arduino "(pin-mode 8 1)")
  (talk-arduino "(digital-write 8 0)")
  (setf (dark-image *cam1*) (average-images *cam1* :number 100 :use-dark nil))
  (setf (dark-image *cam2*) (average-images *cam2* :number 100 :use-dark nil))
  (setf (dark-image *cam3*) (average-images *cam3* :number 100 :use-dark nil))
  (sleep .2)
  (talk-arduino "(digital-write 8 1)"))

#+nil
(loop for i from 0 below 300 by 3 do
     (format t "~a~%" i)
     (talk-arduino (format nil "(dac ~d 2048)" (floor (+ 2000 (* 1000 (sin (* 2 pi (/ i 100)))))))))


#+nil
(talk-arduino (format nil "(dac 657 2048)" ))
#+nil
(talk-arduino (format nil "(+ 1 2)" ))



#+nil
(loop for dir-num from 3 below 15 do
 (let ((ic 2047)
       (ir 1500)
       (jc 2047)
       (jr 1600))
   (ensure-directories-exist (format nil "/media/sdc1/dat/~d/" dir-num))
   (loop for j from (- jc jr) upto (+ jc jr) by 40 do
	(loop for i from (- ic ir) upto (+ ic ir) by 40 do
	     (format t "~a~%" (list 'i i 'j j))
	     (talk-arduino (format nil "(dac ~d ~d)~%" i j))
					;(sleep 2)
	     (loop for c in (list *cam1* *cam2* *cam3*) and k from 1 do 
		  (format t "acquire ~d ~a~%" k (list (get-statistics c) (multiple-value-list (get-n-buffers c))))
		  
		  (let ((im (if nil
				(average-images c :number 10 :use-dark t)
					;(acquire-single-image c :use-dark t)
				(acquire-image-using-full-range c)
				)))
		    (write-pgm (format nil "/dev/shm/~d.pgm" k) im)
		    (write-pgm (format nil "/media/sdc1/dat/~d/i~4,'0d_j~4,'0d_~d_~2,6$.pgm" dir-num i j k (get-exposure c)) im)))))))

;; network usage is 2.2Mbytes/s
;; (/ (* 11 1024) 2.2) => 5100 seconds for one scan

 



#+nil
(defparameter *bla* (acquire-image-using-full-range *cam3* :use-dark nil))
#+nil
(destructuring-bind (h w) (array-dimensions *bla*)
 (let* (;; allocate a 1d array
	(a1 (make-array (* w h) :element-type '(complex double-float)))
	;; create a 2d array for access
	(a (fftw:make-foreign-complex-array-as-double (list h w))
	  )
	(b (fftw:make-foreign-complex-array-as-double (list h w)))
	#+nil(b1 (make-array (array-total-size *bla*) :element-type (array-element-type *bla*)
			:displaced-to *bla*)))
  

   (dotimes (i w)
     (dotimes (j h)
       (setf (aref a j i 0) (* 1d0 (aref *bla* j i)))))

   (format t "running FFT!~%")
   ;; call fftw
   (time (defparameter *bla2* (fftw:ft a b)))
   ;; takes 1.5s 75Mb allocated
   ;; takes 0.05s 528bytes allocated

   ;; print out each element of the array. scale data to lie within 0..9
#+nil   (progn
     (terpri)
     (destructuring-bind (h w) (array-dimensions *bla*)
       (dotimes (j h)
	 (dotimes (i w)
	   (format t "~1,'0d" (floor (abs (aref *bla* j i)) (/ (* h w) 9))))
	 (terpri))))))


#+nil
(defparameter *bla2-abs* (.log (.abs* *bla2*)))

#+nil
(.mean (extract (.abs* *bla2*) :x (+ 33 193) :y (+ 33 -10) :w 66 :h 66))


#+nil
(defparameter *blau* (.uint16 *bla2-abs*))
#+nil
(write-pgm "/dev/shm/o.pgm" *blau*)
#+nil
(write-pgm "/dev/shm/o.pgm" (extract *blau* :x (+ 33 193) :y (+ 33 -10) :w 66 :h 66))

#+nil
(write-pgm "/dev/shm/o2.pgm" *bla*)


#+nil
(write-pgm "/dev/shm/r.pgm" (.uint16 (.rr '(128 128))))
#+nil
(write-pgm "/dev/shm/p.pgm" (.uint16 (.phiphi '(128 128))))

;; use rr to create an array where the value in radial circles
;; increases by at least 2pi in each step. add phiphi to this. then
;; each pixel gets a unique id and they are sorted on concentric
;; circles starting from the center

#+nil
(let* ((w 128)
       (s (list w w))
       (r (.* (.rr s) (* 4 pi w)))
       (p (.phiphi s)))
 (write-pgm "/dev/shm/i.pgm" (.uint16 (.+ r p))))







#+nil
(defparameter *bla3* (acquire-single-image *cam3* :use-dark nil))




