#.(require :cffi)
(cffi:load-foreign-library "libid_lib.so")

;; int int int complex-double-float size=(+ (* 21 m) 80 (* n (+ 17 (* 2 krank))))
(cffi:defcfun "idzr_aidi_" :void (m :pointer) (n :pointer) (krank :pointer) (w :pointer))

(defun idzr-aidi (m n krank &key (size (+ (* 21 m) 80 (* n (+ 17 (* 2 krank))))))
  (let* ((w (make-array size :element-type '(complex double-float))))
    (cffi:with-pointer-to-vector-data (wp w)
     (cffi:with-foreign-objects ((mp :int)
				(np :int)
				(krankp :int))
       (setf (cffi:mem-ref mp :int) m
	     (cffi:mem-ref np :int) n
	     (cffi:mem-ref krankp :int) krank)
       (idzr-aidi- mp np krankp wp)))
    w)) 

#+nil
(defparameter *bla* (idzr-aidi 10 10 4))

;; input:
;; int m n krank
;; complex double a(m,n)
;; complex double w size:
;; (+ (* (+ (* 2 krank) 22) m)
;;    (* (+ (* 6 krank) 21) n)
;;    (* 8 krank krank)
;;    (* 10 krank)
;;    90)



;; output:
;; double s(krank)
;; complex double  u(m,krank) v(n,krank)
;; int ier
(cffi:defcfun "idzr_asvd_" :void (m :pointer) (n :pointer) (a :pointer) (krank :pointer) (w :pointer) (u :pointer) (v :pointer) (s :pointer) (ier :pointer))

(defun idzr-asvd (m n a krank
		  &key (w (idzr-aidi m n krank
				     :size (+ (* (+ (* 2 krank) 22) m)
					      (* (+ (* 6 krank) 21) n)
					      (* 8 krank krank)
					      (* 10 krank) 90))))
  "Return u diag(s) v^*, the singular value decomposition of rank
krank approximating array a."
  (let* ((v (make-array (* n krank) :element-type '(complex double-float)))
	 (u (make-array (* m krank) :element-type '(complex double-float)))
	 (s (make-array krank :element-type 'double-float)))
    (cffi:with-pointer-to-vector-data (up u)
     (cffi:with-pointer-to-vector-data (vp v)
       (cffi:with-pointer-to-vector-data (sp s)
	 (cffi:with-pointer-to-vector-data (wp w)
	   (cffi:with-pointer-to-vector-data (ap a)
	     (cffi:with-foreign-objects ((mp :int)
					 (np :int)
					 (krankp :int)
					 (ierp :int))
	       (setf (cffi:mem-ref mp :int) m
		     (cffi:mem-ref np :int) n
		     (cffi:mem-ref krankp :int) krank)
	       (idzr-asvd- mp np ap krankp wp up vp sp ierp)
	       (unless (= (cffi:mem-ref ierp :int) 0)
		 (error "idzr-asvd didn't succeed."))))))))
    (values u s v)))


 

#+nil
(let* ((m 300) ;; 300x300 takes 1.5s
       (n 300)
       (k 300)
       (a (make-array (* m n) :element-type '(complex double-float))))
  (loop for i below (length a) do
       (setf (aref a i) (complex (random 1d0) (random 1d0))))
  (time
   (defparameter *bla*
     (multiple-value-list
      (idzr-asvd m n a k)))))

;; 3.9Gb

#+nil
(time
 (gc :full t))




(cffi:load-foreign-library "liblapack.so")

(cffi:defcfun "zgesvd_" :void
  (JOBU :pointer)(JOBVT :pointer)(M :pointer)(N :pointer)(A :pointer)
  (LDA :pointer)(S :pointer)(U :pointer)(LDU :pointer)(VT :pointer)
  (LDVT :pointer)(WORK :pointer)(LWORK :pointer)(RWORK :pointer)(INFO :pointer))

(defun zgesvd (a m n &key (lda m) (ldu m) (ldvt (min m n)) (verbose nil))
  "Return u diag(s) v^*, the singular value decomposition of rank
krank approximating matrix A. The contents of A are destroyed."
  (let* ((krank (min m n))
	 (vt (make-array (* krank ldvt) :element-type '(complex double-float)))
	 (u (make-array (* krank ldu) :element-type '(complex double-float)))
	 (s (make-array krank :element-type 'double-float))
	 (rwork (make-array (* 5 krank) :element-type 'double-float))
	 (work (make-array 1 :element-type '(complex double-float))))
    (cffi:with-pointer-to-vector-data (up u)
     (cffi:with-pointer-to-vector-data (vtp vt)
       (cffi:with-pointer-to-vector-data (sp s)
	 (cffi:with-pointer-to-vector-data (rworkp rwork)
	   (cffi:with-pointer-to-vector-data (workp work)
	    (cffi:with-pointer-to-vector-data (ap a)
	     (cffi:with-foreign-objects ((jobup :char)
					 (jobvtp :char)
					 (mp :int)
					 (ldap :int)
					 (np :int)
					 (ldup :int)
					 (ldvtp :int)
					 (lworkp :int)
					 (infop :int)
					 )
	       (setf (cffi:mem-ref jobup :char) (char-code #\S) ;; (min m n) columns in U
		     (cffi:mem-ref jobvtp :char) (char-code #\S) ;; (min m n) rows in V
		     (cffi:mem-ref mp :int) m ;; rows of input matrix A,
		     (cffi:mem-ref ldap :int) lda ;; leading dimension of A, fortran arrays are transposed!
		     (cffi:mem-ref np :int) n ;; columns of A
		     (cffi:mem-ref ldup :int) ldu
		     (cffi:mem-ref ldvtp :int) ldvt
		     (cffi:mem-ref lworkp :int) -1 ;; first call asks for optimum size of work array
		     )
	       (zgesvd- jobup jobvtp mp np ap ldap sp up ldup vtp ldvtp workp lworkp rworkp infop)
	       (if (= (cffi:mem-ref infop :int) 0)
		   (progn
		     (when verbose
		      (format t "optimum size of work is ~a" (aref work 0)))
		     (setf (cffi:mem-ref lworkp :int) (floor (realpart (aref work 0))))
		     (let ((work (make-array (cffi:mem-ref lworkp :int) :element-type '(complex double-float))))
		       (cffi:with-pointer-to-vector-data (workp work)
			 (zgesvd- jobup jobvtp mp np ap ldap sp up ldup vtp ldvtp workp lworkp rworkp infop)
			 (if (= (cffi:mem-ref infop :int) 0)
			     (return-from zgesvd (values u s vt))
			     (error "calculating svd failed. info is ~d~%" (cffi:mem-ref infop :int))))))
		   (error "asking for optimal size of work array failed. info is ~d~%" (cffi:mem-ref infop :int)))
	       )))))))))

#+nil
(let* ((m 300) 
       (n 300)
       (k 300)
       (a (make-array (* n m) :element-type '(complex double-float)))
       (a2 (make-array (* n m) :element-type '(complex double-float))))
  (loop for i below (length a) do
       (let ((v (complex (random 1d0) (random 1d0))))
	 (setf (aref a i) v
	       (aref a2 i) v)))
  (time
   (defparameter *bla* (multiple-value-list (zgesvd a m n))))
  (time
   (defparameter *bla2*
     (multiple-value-list
      (idzr-asvd m n a2 k)))))

#+nil
(let ((s (second *bla*))
      (s2 (second *bla2*)))
  (loop for i below (length s) maximize
       (progn
	 (format t "~d ~g~%" i (- (aref s i) (aref s2 i)))
	 (abs (- (aref s i) (aref s2 i))))))

(defun read-ics-volume (name &key (x 80) (y 84) (z 151))
  (let ((pos
         (with-open-file (s name :direction :input)
           (do ((l (read s) (read s)))
               ((string= "END" (unless (numberp l) (string l)))))
           (file-position s))))
    (with-open-file (s name :direction :input
                       :element-type '(unsigned-byte 8))
      ;; FIXME: why do I need -170 offset?
      (file-position s (+ 1 pos))
      (let* ((a1 (make-array (* z y x 16)
                             :element-type '(unsigned-byte 8)
					; :displaced-to a
			     )))
        (read-sequence a1 s)
        (let ((a (make-array (* z y x)
			     :element-type '(complex double-float))))
	  (cffi:with-pointer-to-vector-data (a1p a1)
	   (cffi:with-pointer-to-vector-data (ap a)
	     (loop for i  below (* z y x 2) do
		  (setf (cffi:mem-aref ap :int i)
			(cffi:mem-aref a1p :int i)))))
	  a)))))


(defun convert-ub2cdf (a)
  (declare (optimize (safety 0)))
  (the (simple-array (complex double-float) (#.(/ #xf7bc00 16)))
       a)
  #+nil
  (let ((len (length a)))
    (make-array (floor len 16)
		:element-type '(complex double-float)
		:displaced-to a)))

#+nil
(defparameter *blub*
  (multiple-value-list
   (zgesvd *bla* 80 (* 151 84))))
#+nil
(defparameter *bla* (read-ics-volume (elt (directory "/media/sda4/b/20140309/0_/*.ics") 120)))
#+nil
(loop for i from 0 below 12 collect (code-char (aref *bla* i)))
#+nil    
(defparameter *bla2* (convert-ub2cdf *bla*))

(defun load-all-ics ()
 (let* ((fns (directory "/media/sda4/b/20140309/0_/*.ics"))
	(a (read-ics-volume (elt (directory "/media/sda4/b/20140309/0_/*.ics") 0)))
	(la (length a))
	(b (make-array (* la (length fns))
		       :element-type '(complex double-float))))
   (loop for fn in fns and j from 0 do
	(format t "~a~%" fn)
	(let ((a (read-ics-volume fn)))
	  (dotimes (i (length a))
	    (setf (aref b (+ (* j la) i)) (aref a i)))))
   b))
#+nil
(time (defparameter *bla* (load-all-ics))) ;; takes 18s


(defun .linear (a)
  (make-array (array-total-size a)
	      :element-type (array-element-type a)
	      :displaced-to a))

(defun .reshape (a dims)
  (unless (= (array-total-size a)
	     (reduce #'* dims))
    (error "reshape: number of elements must match, they don't: ~a."
	   (list (array-dimensions a)
		 dims)))
  (make-array dims :element-type (array-element-type a)
	      :displaced-to a))

#+nil
(defparameter *bla4* (.reshape *bla* '(80 84 151 161)))

(defmacro ind (&optional (start 0) (end 'end))
  `(cons ,start ',end))


(defun replace-end (ind size)
  (cond ((and (listp ind) (eq 'end (cdr ind))) (cons (car ind) size))
	((and (listp ind)) (cons (car ind) (cdr ind)))
	(t ind)))

#+nil
(replace-end (ind) 23)

(defun ind-range (ind)
  (and (listp ind) (- (cdr ind) (car ind))))
#+nil
(ind-range '(0 . 4))

(defmacro do-region ((indices end &optional (start '(0 0 0))) &body body)
  "Write intertwined loops to traverse a vector, an image or a volume."
  (unless (and (= (length indices)
		  (length end)))
    (error "Number of indices and interval-ends are not equal."))
  (labels ((rec (ind end start acc) ;; several loops
             (if (null end)
                 acc
                 (rec (cdr ind) (cdr end) (cdr start)
                      `((loop for ,(car ind) from ,(car start)
                           below ,(car end) do ,@acc))))))
    (first (rec (reverse indices) ;; first index is outermost loop
		(reverse end)
		(reverse start) body))))
#+nil
(let ((sum 0))
  (do-region ((k j i) (4 4 5))
    (incf sum (+ k j i)))
  sum)

;; example index: x y z  stride w h d
;; x+w*y+z*w*h = x+w*(y+z*h) = (z*h+y)*w+x
;; p=x+w*y => y = p/w  x=p mod w
;; p=(z*h+y)*w+x => z = p/(h*w)  y=(p-z*w*h)/w  x=p-z*w*h-y*w
;;                                              x=p-w*(y+z*h)

;; x y z t : w h d T
;; p=x+w y+wh z+whd t
;; t = p/(whd)
;; z = (p-t whd) / (wh)
;; y = (p - t whd - z wh)/ w
;; x = (p - t whd - z wh - y*w)

;; c0 c1 c2 c3 : w0 w1 w2 w3
;; p = c0 + w0 c1 + w0 w1 c2 + w0 w1 w2 c3 = sum(prod(w_j,j,i-1,i)*c_i,i,0,n-1)
;; n=3 c3 = p/prod(w_j,j,0,2)
;; c2 = (p-c3*prod(w_j,j,0,2))/prod(w_j,j,0,1)
;; c1 = (p-c3*prod(w_j,j,0,2)-c2*prod(w_j,j,0,1))/prod(w_j,j,0,0)
;; c0 = (p-c3*prod(w_j,j,0,2)-c2*prod(w_j,j,0,1)-c1*prod(w_j,j,0,0))
;; c4 = 0
;; c-1 = 0
;; n=1..2 c_n = (p-sum(c_i prod(w_j,j,0,i-1),i,n+1,nmax))/prod(w_j,j,0,n-1)
;; n=0 c_n = (p-sum(c_i prod(w_j,j,0,i-1),i,n+1,nmax))
(defun 1d-index (strides indices)
  ;; stridex: d h w
  ;; indices: z y x
  (let ((start (elt indices 0)))
    (loop for i from 1 below (length strides) do
	 (setf start (+ (* start (elt strides i))
			(elt indices i))))
    start))

#+nil
(1d-index '(4 4 5) '(1 2 3))

;; http://julia.readthedocs.org/en/latest/manual/arrays/

(defun multi-index (strides p)
  ;; stridex: d h w
  (let* ((dims (length strides))
	 (c (make-array dims :element-type 'fixnum)))
    (format t "dims=~a, strides=~a~%" dims strides)
    (setf (aref c (1- dims)) (floor p (reduce #'* (subseq strides 1 (length strides)))))
    (format t "p=~a c_(dims-1)=c(~a)=~a~%" p (1- dims) (aref c (1- dims)))
    (macrolet ((prod ((var start end) &body body)
		 `(let ((prod 1))
		    (loop for ,var from ,start below ,end do
			 (setf prod (* prod ,@body)))
		    prod)))
      (loop for n from (- dims 2) downto 1 do
	  (setf (aref c n)
		(floor (- p (loop for i from (+ n 1) below dims sum
				 (prog1
				     (* (aref c i) (prod (j 0 i) (elt strides j)))
				   (format t "~a~%" (list  (aref c i) 'i i (prod (j 0 i) (elt strides j)))))))
		       (prod (j 0 (- n 1)) (elt strides j))))
	   (format t "filling ~a~%" (list n (aref c n))) )
      (let ((n 0))
	(setf (aref c 0)
	      (- p (loop for i from (+ n 1) below (1- dims) sum
			(* (aref c i) (prod (j 0 i) (elt strides j))))))))
    c))
#+nil
(let ((strides '(4 4
		 5 3)))
 (floor 733 (reduce #'* (subseq strides 0 (1- (length strides))))))

#+nil
(multi-index '(4 2) 8)
#+nil
(1d-index '(2 4) '(2 1))
(defun subarray (a &rest rest)
  (let* ((inds
	  (loop for size in (array-dimensions a) and r in rest collect
	       (replace-end r size)))
	 (number-of-ranges (count-if #'listp inds))
	 (new-dims (remove-if #'null (loop for ind in inds collect
					  (ind-range ind))))
	 (b (make-array new-dims :element-type (array-element-type a)))
	 (a1 (.linear a))
	 (b1 (.linear b)))
    
    (dotimes (i (reduce #'* new-dims))
      (setf (aref b1 (1d-index new-dims i))))
    b))



#+nil
(subarray *bla4* (ind 3 12) (ind) 3 4)

#+nil
(write-pgm "/dev/shm/o.pgm" )

(defun write-pgm (filename img &key (scale 1e-3))
  (declare (type simple-string filename)
	   #+sbcl (values null &optional))
  (destructuring-bind (h w) (array-dimensions img)
    (declare (type fixnum w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%255~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :append) ;; FIXME: i think this append doesn't work as expected and sometimes eats one of the new lines
      (let* ((data-1d (make-array (* h w)
				  :element-type (array-element-type img)
				  :displaced-to img))
	     (ub8 (make-array (* h w)
			      :element-type '(unsigned-byte 8))))
	(dotimes (i (* h w))
	  (setf (aref ub8 i) (min 255 (max 0 (* scale (abs (aref data-1d i)))))))
        (write-sequence data-1d s)))
    nil))






#+nil
(room)

;; Dynamic space usage is:   3,109,631,104 bytes.
;; Read-only space usage is:      5,824 bytes.
;; Static space usage is:         4,032 bytes.
;; Control stack usage is:        8,200 bytes.
;; Binding stack usage is:        1,040 bytes.
;; Control and binding stack usage is for the current thread only.
;; Garbage collection is currently enabled.

;; Breakdown for dynamic space:
;;   2,727,567,504 bytes for         9 simple-array-complex-double-float objects.
;;   194,924,960 bytes for 6,091,405 complex-double-float objects.
;;   187,138,640 bytes for 1,312,047 other objects.
;;   3,109,631,104 bytes for 7,403,461 dynamic objects (space total.)
