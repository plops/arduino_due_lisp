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
(let* ((m 10000)
       (n 10000)
       (k 1000)
       (a (make-array (* m n) :element-type '(complex double-float))))
  (time
   (loop for i below (length a) do
	(setf (aref a i) (complex (random 1d0) (random 1d0)))))
  (time
   (gc :full t))
  (time
   (defparameter *bla*
     (multiple-value-list
      (idzr-asvd m n a k)))))

;; 3.9Gb

#+nil
(time
 (gc :full t))

#+nil
(let ((s (second *bla*)))
  (loop for i below (length s) do
       (format t "~d ~f~%" i (aref s i))))
