#+nil (/ 
 (with-open-file (s "/dev/shm/frame024.raw")
   (file-length s))
 (* 1024))
(/ (* 1280 960) 1024) 

;; yuyv


(defun read-raw (&optional  (fn "/dev/shm/frame000.raw"))
 (let* ((w 1600)
       (h 1200)
       (a (make-array (* w h 2) :element-type '(unsigned-byte 8))))
   (with-open-file (s fn :element-type '(unsigned-byte 8))
     (assert (= (length a) (file-length s)))
     (read-sequence a s))
   (let* ((y (make-array (* w h) :element-type '(unsigned-byte 8)))
	  (y2 (make-array (list h w) :element-type '(unsigned-byte 8)
			  :displaced-to y)))
     (dotimes (i (length y))
       (setf (aref y i) (aref a (* 2 i))))
     y2)))
#+nil
(progn
  (defparameter *dat* (extract (read-raw)))

 (write-pgm "/dev/shm/o3.pgm" *dat*)

 (defparameter *hist*
   (hist-ub8 *dat*))

 (defparameter *kdat*
   (ft (convert-ub8-to-cdf *dat*)))



 (hist-ub8 (scale *kdat*))
 (write-pgm "/dev/shm/k.pgm" (scale :a *kdat* :scale 1e-1)))


					; "~/dat/i1220_j2027_2.pgm"


(declaim (optimize (debug 3) (speed 0) (safety 3)))

;; in order to locate the areas with orders in the fourier transform
;; of the hologram, i use the following approach: the phase of the
;; first order changes slightly from image to image. therefore, the
;; derivative of the phase with respect to time is small and nearly
;; constant in these areas, while it jumps all over the place in areas
;; where there is no hologram energy and the phase jumps all over the
;; place. i calculate the variance of the phase derivative. using its
;; histogram i find the threshold between areas that contain data and
;; areas with no useful information.


#+nil
(let* ((dir "/media/sda2/stabil-p/1*.pgm")
       (z (ft (convert-any-to-cdf (read-pgm (first (directory dir)))))))
  (defparameter *bla* nil)
  (loop for e in (rest (directory dir))
     do
       (let* ((w (ft (convert-any-to-cdf (read-pgm e))))
	      (p (phase-diff :a z :c w)))
	 (setf z w)
	 (push p *bla*)
	 )))
#+nil
(defparameter *blaa*
 (destructuring-bind (h w) (array-dimensions (first *bla*))
   (let ((a (make-array (list (length *bla*) h w) :element-type 'double-float)))
     (loop for e in *bla* and k from 0 do
	  (dotimes (i w)
	    (dotimes (j h)
	      (setf (aref a k j i) (aref e j i)))))
     a)))

#+nil
(progn
  (defparameter *bla* nil)
  (gc :full t))

#+nil
(progn
 (defparameter *mean*
   (destructuring-bind (z h w) (array-dimensions *blaa*)
     (let ((a (make-array (list h w) :element-type 'double-float)))
       (dotimes (k z)
	 (dotimes (j h)
	   (dotimes (i w)
	     (incf (aref a j i) (/ (aref  *blaa* k j i) z)))))
       a)))
 (write-pgm "/dev/shm/mean.pgm"
	    (scale-df :scale 100000 :a *mean*)))
#+nil
(defparameter *var*
  (destructuring-bind (z h w) (array-dimensions *blaa*)
    (let ((a (make-array (list h w) :element-type 'double-float)))
      (dotimes (k z)
	(dotimes (j h)
	  (dotimes (i w)
	    (incf (aref a j i) (- (expt (/ (aref  *blaa* k j i) 1d0) 2)
				  (expt (aref *mean* j i) 2))))))
      a)))
#+nil
(write-pgm "/dev/shm/var.pgm"
	   (let ((l (.linear *var*)))
	     (scale-df :scale (* 255 (/ (* pi pi))) :a *var*
		       :mask  #'(lambda (x) (< (aref l x) (* pi pi)) ;; test threshold
					))))

#+nil
(progn ;; find threshold in histogram of image with variance of phase time derivatives
  (defparameter *var-hist*
    (multiple-value-list 
     (hist-df :a *var* :n 200 :mi 0 :ma 400)))
  
  (with-open-file (s "/dev/shm/o.dat" :direction :output
		     :if-exists :supersede :if-does-not-exist :create)
    (destructuring-bind (a mi ma) *var-hist* 		;loop for a in *var-hist* do
      (loop for e across a and i from 0 do
	   (format s "~a ~a~%" (+ (- mi) (* (- ma mi) (/ i (* 1d0 (length a))))) e))
      (terpri s))))



#+nil
(progn ;; show the phase difference of each image with respect to the first image
 (let* ((dir "/media/sda2/stabil-p/1*.pgm")
	(z (ft (convert-any-to-cdf (read-pgm (first (directory dir)))))))
   (loop for e in (subseq (directory dir) 1)
      do
	(let ((w (ft (convert-any-to-cdf (read-pgm e))))
	      (v1 (.linear *var*)))
	  (write-pgm (concatenate 'string "/dev/shm/k" (pathname-name e) ".pgm")
		     (scale :scale 1e-4 :a w
			       :mask #'(lambda (x) (< (aref v1 x) 4))
			       ))
	  (write-pgm (concatenate 'string "/dev/shm/p" (pathname-name e) ".pgm")
		     (scale-df :scale 100 :offset .2d0 :a (phase-diff :a z :c w)
			       :mask #'(lambda (x) (< (aref v1 x) 4)))
		     )))))


(defun read-pgm (filename)
  (declare ((or pathname string) filename)
           (values (or (simple-array (unsigned-byte 8) 2)
                       (simple-array (unsigned-byte 16) 2)) &optional))
  (with-open-file (s filename)
    (unless (equal (symbol-name (read s)) "P5")
      (error "no PGM file"))
    ;(read-line s)
    (let* ((w (read s))
           (h (read s))
           (grays (read s))
           (pos (+ 1 (file-position s))))
      (declare ((integer 0 65535) grays w h))
      (let* ((type (if (<= grays 255)
                       '(unsigned-byte 8)
                       '(unsigned-byte 16)))
             (data (make-array (list h w)
                               :element-type type))
             (data-1d (make-array (* h w)
                                  :element-type type
                                  :displaced-to data)))
        (with-open-file (s2 filename :element-type type)
          (file-position s2 pos)
          (read-sequence data-1d s2))
        data))))


(defun write-pgm (filename img)
  (declare (simple-string filename)
           ((array (unsigned-byte 8) 2) img)
           (values null &optional))
  (destructuring-bind (h w)
      (array-dimensions img)
    (declare ((integer 0 65535) w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%255~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :append)
      (let ((data-1d (make-array 
                      (* h w)
                      :element-type '(unsigned-byte 8)
                      :displaced-to img)))
        (write-sequence data-1d s)))
    nil))


(defun hist-ub8 (a)
  (declare (type (array (unsigned-byte 8) *) a))
  (let* ((n (array-total-size a))
	 (a1 (make-array n :element-type (array-element-type a)
			 :displaced-to a))
	 (hist (make-array 256 :element-type 'fixnum :initial-element 0)))
    (dotimes (i n)
      (incf (aref hist (aref a1 i))))
    hist))

(defun .linear (a)
  (let* ((n (array-total-size a))
	 (a1 (make-array n :element-type (array-element-type a)
			 :displaced-to a)))
    a1))

(defun .log (a)
  (let* ((a1 (.linear a))
	 (b (make-array (array-dimensions a)
			:element-type 'double-float))
	 (b1 (.linear b)))
    (dotimes (i (length b1))
      (setf (aref b1 i) (log (aref a1 i))))
    b))

(defun .abs (a &key (offset 1e-4))
  (let* ((a1 (.linear a))
	 (b (make-array (array-dimensions a)
			:element-type 'double-float))
	 (b1 (.linear b)))
    (dotimes (i (length b1))
      (setf (aref b1 i) (+ offset (abs (aref a1 i)))))
    b))

(defun hist-df (&key a (n 100) mi ma)
  (declare (type (array double-float *) a))
  (let* ((a1 (.linear a))
	 (ma (or ma (reduce #'max a1)))
	 (mi (or mi (reduce #'min a1)))
	 (hist (make-array n :element-type 'fixnum :initial-element 0)))
    (unless (= ma mi)
     (dotimes (i (length a1))
       (incf (aref hist (max 0 (min (1- n) (floor (* (- n 1) (/ (- (aref a1 i) mi)
								(- ma mi))))))))))
    (values hist mi ma)))



(defconstant +forward+ -1)
(defconstant +backward+ 1)
(defconstant +measure+ 0)
(defconstant +estimate+ (ash 1 6)) ;; array isn't overwritten during planning

(load-shared-object "libfftw3.so.3")
(define-alien-type plan (* int))

(define-alien-routine fftw_execute
    void
  (plan plan))

(define-alien-routine fftw_destroy_plan
    void
  (plan plan))

(define-alien-routine fftw_plan_dft
    plan
  (rank int)
  (n (* int))
  (in (* double-float))	 ;; actually complex
  (out (* double-float))	  ;; actually complex
  (sign int)
  (flags unsigned-int))


(load-shared-object "libfftw3_threads.so.3")
  
(define-alien-routine ("fftw_init_threads" init-threads)
    int)

(define-alien-routine ("fftw_plan_with_nthreads" plan-with-nthreads)
    void
  (nthreads int))


(progn
 (init-threads)
 (plan-with-nthreads 4))


(defun plan (in &optional out)
  (declare (type (array (complex double-float) *) in))
  (let* ((in-d (array-displacement in))
	 (out-d (array-displacement (if out out in))))
    (if (not (and in-d out-d))
	(error "initially you should allocate data as a 1d array in lisp and then use displacement.")
     (let* ((rank (array-rank in))
	    (dims-in (make-array rank :element-type '(signed-byte 32)
				 :initial-contents (array-dimensions in)))
	    (in-sap (sb-sys:vector-sap
		     (sb-ext:array-storage-vector in-d)))
	    (out-sap (sb-sys:vector-sap
		      (sb-ext:array-storage-vector out-d))))
       (format t "array alignment ~a" (list in-sap out-sap))
       (sb-sys:with-pinned-objects (dims-in in out)
	 (fftw_plan_dft rank (sb-sys:vector-sap dims-in)
			in-sap out-sap +forward+ +estimate+))))))

(defun ft (in)
  (declare (type (array (complex double-float) *) in))
  (let* ((out1 (make-array (reduce #'* (array-dimensions in))
			   :element-type '(complex double-float)))
	 (out (make-array (array-dimensions in)
			  :element-type '(complex double-float)
			  :displaced-to out1)))
    (if (and (array-displacement in)
	     (equal '(complex double-float) (array-element-type in)))
	(sb-sys:with-pinned-objects (in out)
	  (let ((plan (plan in out)))
	    (fftw_execute plan)))
	(let* ((a1 (make-array (reduce #'* (array-dimensions in))
			       :element-type '(complex double-float)))
	       (a (make-array (array-dimensions in)
			      :element-type '(complex double-float)
			      :displaced-to a1))
	       (in1 (sb-ext:array-storage-vector in))
	       (in-type (array-element-type in1)))
	  (format t "input array is not displaced to 1d array, I will make a copy.")
	  (cond
	    ((eq 'double-float in-type)
	     (format t "input array is not of complex double-float type. I will convert it.")
	     (dotimes (i (length a1))
	       (setf (aref a1 i) (complex (aref in i)))))
	    ((equal '(complex double-float) in-type)
	     (dotimes (i (length a1))
	       (setf (aref a1 i) (aref in i))))
	    
	    (t (format t "input array has an unsupported element type.")))
	  (sb-sys:with-pinned-objects (a out)
	   (let ((plan (plan a out)))
	     (fftw_execute plan)))))
    out))

(defun convert-ub8-to-cdf (a)
  (let* ((b (make-array (array-total-size a) :element-type '(complex double-float)))
	 (b2 (make-array (array-dimensions a) :element-type '(complex double-float)
			 :displaced-to b))
	 (a1 (make-array (array-total-size a) :element-type (array-element-type a)
			 :displaced-to a)))
    (dotimes (i (length a1))
      (setf (aref b i) (complex (* 1d0 (aref a1 i)))))
    b2))

(defun convert-any-to-cdf (a)
  (let* ((b (make-array (array-total-size a) :element-type '(complex double-float)))
	 (b2 (make-array (array-dimensions a) :element-type '(complex double-float)
			 :displaced-to b))
	 (a1 (make-array (array-total-size a) :element-type (array-element-type a)
			 :displaced-to a)))
    (dotimes (i (length a1))
      (setf (aref b i) (complex (* 1d0 (aref a1 i)))))
    b2))

(defun scale (&key (a) (scale 4e-3) (mask #'(lambda (x) (declare (ignorable x)) t)))
  (declare (type (array (complex double-float) *) a))
  (let* ((b1 (make-array (array-total-size a) :element-type '(unsigned-byte 8)))
	(b2 (make-array (array-dimensions a) :element-type '(unsigned-byte 8)
			:displaced-to b1))
	(a1 (make-array (array-total-size a) :element-type (array-element-type a)
			:displaced-to a)))
    (dotimes (i (length b1))
      (when (funcall mask i)
       (setf (aref b1 i) (min 255 (max 0 (floor (* scale (abs (aref a1 i)))))))))
    b2))

(defun scale-df (&key a (scale 4e-3) (offset 0d0) (mask #'(lambda (x) (declare (ignorable x)) t)))
  (declare (type (array double-float *) a))
  (let* ((b1 (make-array (array-total-size a) :element-type '(unsigned-byte 8)))
	 (b2 (make-array (array-dimensions a) :element-type '(unsigned-byte 8)
			 :displaced-to b1))
	 (a1 (.linear a)))
    (dotimes (i (length b1))
      (when (funcall mask i)
       (setf (aref b1 i) (min 255 (max 0 (floor
					  (* scale (+ offset (aref a1 i)))))))))
    b2))

(defun phase-diff (&key (a) (c) )
  (declare (type (array (complex double-float) *) a c))
  (let* ((b1 (make-array (array-total-size a) :element-type 'double-float))
	 (b2 (make-array (array-dimensions a) :element-type 'double-float
			 :displaced-to b1))
	 (a1 (make-array (array-total-size a) :element-type (array-element-type a)
			 :displaced-to a))
	 (c1 (make-array (array-total-size c) :element-type (array-element-type c)
			 :displaced-to c)))
    (dotimes (i (length b1))
      (setf (aref b1 i) (- (imagpart (/ (- (aref a1 i)
					   (aref c1 i))
					(aref a1 i))))))
    b2))

(defun phase-diff2 (&key (a) (c) (d))
  (declare (type (array (complex double-float) *) a c))
  (let* ((b1 (make-array (array-total-size a) :element-type 'double-float))
	 (b2 (make-array (array-dimensions a) :element-type 'double-float
			 :displaced-to b1))
	 (a1 (.linear a))
	 (c1 (.linear c))
	 (d1 (.linear d)))
    (dotimes (i (length b1))
      (setf (aref b1 i) (realpart (/ (+ (- (aref a1 i))
					(* 4 (aref c1 i))
					(- (aref d1 i)))
				     (aref c1 i)))))
    b2))


(defun next-power-of-two (n)
  (expt 2 (ceiling (log n 2))))



(defun extract (a &key
		(x (floor (array-dimension a 1) 2))
		(y (floor (array-dimension a 0) 2)) 
		(w (next-power-of-two (min x y 
					   (- (array-dimension a 1) x)
					   (- (array-dimension a 0) y))))
		(h w))
  (let* ((b1 (make-array (* h w) :element-type (array-element-type a)
			 :initial-element 0))
	 (b (make-array (list h w)
			:element-type (array-element-type a)
			:displaced-to b1))
	 (ox (- x (floor w 2)))
	 (oy (- y (floor h 2))))
    (assert (<= 0 ox))
    (assert (<= 0 oy))
    (assert (< (+ w ox) (array-dimension a 1)))
    (assert (< (+ h oy) (array-dimension a 0)))
    (dotimes (j h)
      (dotimes (i w)
	(setf (aref b j i)
	      (aref a (+ j oy) (+ i ox)))))
    b))
