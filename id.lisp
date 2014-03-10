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
#+nil
(defun run ()
 (defparameter *bla*
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
     b)))
#+nil
(defparameter *bla* (run))

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
