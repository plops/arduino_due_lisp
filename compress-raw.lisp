
(defun read-pgm (filename)
  (declare ((or pathname string) filename)
           #+sbcl (values (or (simple-array (unsigned-byte 8) 2)
                              (simple-array (unsigned-byte 16) 2)) &optional))
  (with-open-file (s filename)
    (unless (equal (symbol-name (read s)) "P5")
      (error "no PGM file"))
    (let* ((w (read s))
	   (h (read s))
	   (grays 65535 ;(read s)
	     )
	   (pos (+ 5 (file-position s))))
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
      (format s "P5~%~D ~D~%65535~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 16)
                       :direction :output
                       :if-exists :append)
      (let ((data-1d (make-array 
                      (* h w)
                      :element-type '(unsigned-byte 16)
                      :displaced-to img)))
        (write-sequence data-1d s)))
    nil))

#+nil
(defparameter *dat* (loop for f in (directory "/dev/shm/dat/*_2.pgm")
		       collect (read-pgm f)))

(defun .linear (a)
  (make-array (reduce #'* (array-dimensions a))
	      :element-type (array-element-type a)
	      :displaced-to a))

(defun .byte-swap (a)
  (let* ((b (make-array (array-dimensions a)
			:element-type (array-element-type a)))
	 (b1 (.linear b))
	 (a1 (.linear a)))
    (dotimes (i (length a1))
      (setf (ldb (byte 8 0) (aref b1 i)) (ldb (byte 8 8) (aref a1 i))
	    (ldb (byte 8 8) (aref b1 i)) (ldb (byte 8 0) (aref a1 i))))
    b))

#+nil
(loop for e in *dat* and i from 0 do
     (write-pgm (format nil "/dev/shm/o~4,'0d.pgm" i) (.byte-swap e)))

(declaim (optimize (speed 3) (safety 3) (debug 3)))

(defun .3d (a-list)
  (let ((b (make-array (append (array-dimensions (first a-list))
			       (list (length a-list)))
		       :element-type (array-element-type (first a-list)))))
    (declare (type (simple-array (unsigned-byte 16) 3) b))
    (destructuring-bind (h w z) (array-dimensions b)
      (declare (fixnum h w z))
      (dotimes (k z)
	(format t "slice: ~a~%" (list k z))
	(let ((slice (elt a-list k)))
	  ;(declare (type (simple-array (unsigned-byte 16) 2) slice))
	  (dotimes (j h)
	    (dotimes (i w)
	      (setf (aref b j i z) (aref (elt a-list k) j i)))))))
    b))

#+nil
(defparameter *dat3* (.3d *dat*))
