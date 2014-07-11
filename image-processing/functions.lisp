
(declaim (optimize (speed 3) (safety 1) (debug 0)))
(in-package :image-processing)

(defun .linear (a)
;  #+sbcl  (declare (values (simple-array * *) &optional))
  #+sbcl
  (let ((d (array-displacement a)))
   (if d
       d
       (sb-ext:array-storage-vector a)))
  #-sbcl
  (make-array (array-total-size a)
	      :element-type (array-element-type a)
	      :displaced-to a))

(defun .max (a)
  (reduce #'max (.linear a)))
(defun .min (a)
  (reduce #'min (.linear a)))

(defun .abs (a)
  (declare (type (array (complex double-float) 2) a))
  (let* ((b (make-array (array-dimensions a) :element-type 'double-float))
	 (b1 (.linear b))
	 (a1 (.linear a))
	 (n (array-total-size a)))
    (declare (type (array (complex double-float) 2) a)
	     (type (simple-array double-float 2) b)
	     (type (array (complex double-float) 1) a1)
	     (type (simple-array double-float 1) b1))
    (dotimes (i n)
      (setf (aref b1 i) (abs (aref a1 i))))
    b))

(defun .abs2 (a)
  (declare (type (array (complex double-float) 2) a))
  (let* ((b (make-array (array-dimensions a) :element-type 'double-float))
	 (b1 (.linear b))
	 (a1 (.linear a))
	 (n (array-total-size a)))
    (declare (type (array (complex double-float) 2) a)
	     (type (simple-array double-float 2) b)
	     (type (array (complex double-float) 1) a1)
	     (type (simple-array double-float 1) b1))
    (dotimes (i n)
      (setf (aref b1 i) (expt (abs (aref a1 i)) 2)))
    b))

(defun .realpart (a)
  (declare (type (array (complex double-float) 2) a)
	   (values (simple-array double-float 2) &optional))
  (let* ((b (make-array (array-dimensions a) :element-type 'double-float))
	 (b1 (.linear b))
	 (a1 (.linear a))
	 (n (array-total-size a)))
    (declare (type (simple-array double-float 2) b)
	     (type (simple-array (complex double-float) 1) a1)
	     (type (simple-array double-float 1) b1))
    (dotimes (i n)
      (setf (aref b1 i) (realpart (aref a1 i))))
    b))

(defun .accum (dst b)
  (declare (optimize (speed 3)))
  (declare (type (array double-float 2) b)
	   (type (simple-array double-float 2) dst)
	   (values (simple-array double-float 2) &optional))
  (let* ((b1 (.linear b))
	 (dst1 (.linear dst))
	 (n (array-total-size dst)))
    (declare (type (simple-array double-float 1) b1 dst1))
    (dotimes (i n)
      (setf (aref dst1 i) (+ (aref dst1 i) (aref b1 i))))
    dst))

(defun .abs* (a) ;; this is an ugly hack because clozure common lisp
		 ;; doesn't support foreign complex double-float
		 ;; arrays
  (declare (type (array double-float 3) a))
  (let* ((b (make-array (butlast (array-dimensions a)) :element-type 'double-float))
	 (b1 (.linear b))
	 (a1 (.linear a)))
    (declare (type (array double-float 2) b)
	     (type (array double-float 1) b1 a1))
    (dotimes (i (length b1))
      (setf (aref b1 i) (abs (complex (aref a1 (* 2 i))
				      (aref a1 (+ 1 (* 2 i)))))))
    b))

(defun .log (a)
  (let* ((b (make-array (array-dimensions a) :element-type 'double-float))
	 (b1 (.linear b))
	 (a1 (.linear a)))
    (dotimes (i (length a1))
      (let ((v (aref a1 i)))
	(setf (aref b1 i) (if (= v 0d0)
			      0d0
			      (log v)))))
    b))

(defun .uint8 (a)
  (let* ((b (make-array (array-dimensions a) :element-type '(unsigned-byte 8)))
	 (b1 (.linear b))
	 (a1 (.linear a))
	 (ma (.max a))
	 (mi (.min a))
	 (s (if (< (- ma mi) 1e-3)
		1
		(/ 255 (- ma mi)))))
    (dotimes (i (length a1))
      (setf (aref b1 i) (min 255 (max 0 (floor (* s (- (aref a1 i) mi)))))))
    b))


(defun .uint16 (a)
  (let* ((b (make-array (array-dimensions a) :element-type '(unsigned-byte 16)))
	 (b1 (.linear b))
	 (a1 (.linear a))
	 (ma (.max a))
	 (mi (.min a))
	 (s (if (< (- ma mi) 1e-3)
		1
		(/ 65535 (- ma mi)))))
    (dotimes (i (length a1))
      (setf (aref b1 i) (min 65535 (max 0 (floor (* s (- (aref a1 i) mi)))))))
    b))

(defun next-power-of-two (n)
  (expt 2 (ceiling (log n 2))))


(defun extract (a &key
                (x (floor (array-dimension a 1) 2))
                (y (floor (array-dimension a 0) 2))
                (w (next-power-of-two (min x y
                                           (- (array-dimension a 1) x)
                                           (- (array-dimension a 0) y))))
                (h w))
  (declare ;(type (array (complex double-float) 2) a)
	   (values (array (complex double-float) 2) &optional))
  (let* ((b1 (make-array (* h w) :element-type '(complex double-float)
                         :initial-element (complex 0d0)))
         (b (make-array (list h w)
                        :element-type (array-element-type a)
                        :displaced-to b1))
         (ox (- x (floor w 2)))
         (oy (- y (floor h 2))))
    (declare (type (simple-array (complex double-float) 1) b1)
	     (type (array (complex double-float) 2) b))
    ;(assert (<= 0 ox))
    ;(assert (<= 0 oy))
    ;(assert (< (+ w ox) (array-dimension a 1)))
    ;(assert (< (+ h oy) (array-dimension a 0)))
    (destructuring-bind (hh ww) (array-dimensions a)
     (dotimes (j h)
       (dotimes (i w)
	 (setf (aref b j i)
	       (aref a (mod (+ j oy) hh) (mod (+ i ox) ww))))))
    b))

(defun extract-cdf* (a &key
		     (x (floor (array-dimension a 1) 2))
		     (y (floor (array-dimension a 0) 2))
		     (w (next-power-of-two (min x y
						(- (array-dimension a 1) x)
						(- (array-dimension a 0) y))))
		     (h w))
  (declare (type (array double-float 3) a))
  (destructuring-bind (hh ww two) (array-dimensions a)
    (declare (ignore two))
    (let* ((b (make-array (list h w) :element-type '(complex double-float)))
	   (ox (- x (floor w 2)))
	   (oy (- y (floor h 2))))
      (declare (type (array (complex double-float) 2) b))
      (dotimes (j h)
       (dotimes (i w)
	 (setf (aref b j i) (complex (aref a (mod (+ j oy) hh) (mod (+ i ox) ww) 0)
				     (aref a (mod (+ j oy) hh) (mod (+ i ox) ww) 1)))))
     b)))

(defun .rr (a)
  (unless (or (arrayp a) (listp a))
    (error ".rr argument must be dimension list or array."))
  (let* ((dims (typecase a
		 (array (array-dimensions a))
		 (list a)))
	 (rank (length dims))
	 (b (make-array dims :element-type 'double-float)))
    (unless (= rank 2)
      (error "only rank 2 is supported for now"))
    (destructuring-bind (h w) dims
      (dotimes (j h)
	(dotimes (i w)
	  (setf (aref b j i) (sqrt (+ 0d0
				      (expt (/ (- i (floor w 2)) w) 2)
				      (expt (/ (- j (floor h 2)) h) 2)))))))
    b))

(defun .* (a b)
  (let* ((c (make-array (array-dimensions a)
			:element-type (array-element-type a)))
	 (c1 (.linear c))
	 (a1 (.linear a)))
    (typecase b
      (array (let ((b1 (.linear b)))
	       (dotimes (i (length c1))
		 (setf (aref c1 i) (* (aref a1 i) (aref b1 i))))))
      (number (dotimes (i (length c1))
		 (setf (aref c1 i) (* (aref a1 i) b)))))
    c))


(defun .+ (a b)
  (let* ((c (make-array (array-dimensions a)
			:element-type (array-element-type a)))
	 (c1 (.linear c))
	 (a1 (.linear a)))
    (typecase b
      (array (let ((b1 (.linear b)))
	       (dotimes (i (length c1))
		 (setf (aref c1 i) (+ (aref a1 i) (aref b1 i))))))
      (number (dotimes (i (length c1))
		 (setf (aref c1 i) (+ (aref a1 i) b)))))
    c))

(defun ^.- (a b)
  "calculate a-b and return result in a"
  (declare (type (array (complex double-float) 2) a)
	   (type (array double-float 2) b)
	   (values (array (complex double-float) 2) &optional))
  (let* ((a1 (.linear a)))
    (typecase b
      (array (let ((b1 (.linear b)))
	       (dotimes (i (array-total-size b))
		 (setf (aref a1 i) (- (aref a1 i) (aref b1 i))))))
      (number (dotimes (i (array-total-size a))
		 (setf (aref a1 i) (- (aref a1 i) b)))))
    a))


(defun .phiphi (a)
  (unless (or (arrayp a) (listp a))
    (error ".phiphi argument must be dimension list or array."))
  (let* ((dims (typecase a
		 (array (array-dimensions a))
		 (list a)))
	 (rank (length dims))
	 (b (make-array dims :element-type 'double-float)))
    (unless (= rank 2)
      (error "only rank 2 is supported for now"))
    (destructuring-bind (h w) dims
      (dotimes (j h)
	(dotimes (i w)
	  (setf (aref b j i) (atan (* 1d0 (- j (floor h 2)))
				   (- i (floor w 2)))))))
    b))

(defun .mean (a)
  (declare (type (array double-float 2) a)
	   (values double-float &optional))
  (let ((sum 0d0)
	(n (array-total-size a))
	(a1 (.linear a)))
    (declare (type (simple-array double-float 1) a1)
	     (type double-float sum))
    (dotimes (i n)
      (incf sum (aref a1 i)))
    (/ sum n)))

(defun write-pgm8 (filename img)
  (declare (type simple-string filename)
           ((array (unsigned-byte 8) 2) img)
           #+sbcl (values null &optional))
  (destructuring-bind (h w) (array-dimensions img)
    (declare (type fixnum w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%255~%~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :append) ;; FIXME: i think this append doesn't work as expected and sometimes eats one of the new lines
      (let ((data-1d (make-array 
                      (* h w)
                      :element-type '(unsigned-byte 8)
                      :displaced-to img)))
        (write-sequence data-1d s)))
    nil))

(defun write-pgm16 (filename img)
  (declare (type simple-string filename)
           ((array (unsigned-byte 16) 2) img)
           #+sbcl (values null &optional))
  (destructuring-bind (h w) (array-dimensions img)
    (declare (type fixnum w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%65535~%~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 16)
                       :direction :output
                       :if-exists :append) ;; FIXME: i think this append doesn't work as expected and sometimes eats one of the new lines
      (let ((data-1d (make-array 
                      (* h w)
                      :element-type '(unsigned-byte 16)
                      :displaced-to img)))
        (write-sequence data-1d s)))
    nil))
