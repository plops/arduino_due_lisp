(defun .floor (ls value)
  (mapcar #'(lambda (x) (floor x value)) ls))

(defun .round (ls)
  (mapcar #'(lambda (x) (round x)) ls))

(defun .- (a b)
  (if (listp b)
      (mapcar #'- a b)
      (mapcar #'(lambda (x) (- x b)) a)))

(defun .+ (a b)
  (mapcar #'+ a b))

(defun flatten (x)
  ;; this is from graham's onlisp
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro do-region ((dim names start end) &body body)
  (let* ((n (length names))
	 (indices (loop for d below dim collect
		       (loop for i below n collect (gensym)))))
    (labels ((rec (dim acc)
	       (if (= dim -1)
		   acc
		   (rec (1- dim)
			`((loop for
			       ,@(reduce #'append
					 (loop for i below n collect
					      (if (= i (1- n))
						  `(,(elt (elt indices dim) i) 
						     from (aref ,start ,i ,dim) below (aref ,end ,i ,dim))
						  `(,(elt (elt indices dim) i) 
						     from (aref ,start ,i ,dim) below (aref ,end ,i ,dim) and)))) 
			     do
			       ,@acc))))))
      (first (rec (1- dim) `((macrolet (,@(loop for name in names and i from 0 below n collect
					       (let ((offsets (loop for d below dim collect (gensym))))
						 `(,name (&optional ,@(loop for o in offsets collect (list o 0)))
							 (list 'aref ',name ,@(loop for o in offsets and d from 0 collect
									    `(list '+ ,o ',(elt (elt indices d) i))
										   ))))))
			       ,@body)))))))

#+nil
(let* ((dim 3)
       (n 2)
       (start-l '((0 0 0) (1 1 1)))
       (end-l '((2 2 2) (3 3 3)))
       (astart (make-array (list n dim) :element-type 'fixnum :initial-contents start-l))
       (aend (make-array (list n dim) :element-type 'fixnum :initial-contents end-l))
       (dst (make-array (list 3 3 3) :element-type 'single-float))
       (src (make-array (list 3 3 3) :element-type 'single-float)))
  (do-region (3 (src dst) astart aend)
    (setf (dst) (src))))


(defun extract (a size &optional
			 (center (.floor (array-dimensions a) 2))
			 (border 0))
  (when (< (length size) (array-rank a))
    (setf size (append size (subseq (array-dimensions a) (length size)))))
  (when (< (length center) (array-rank a))
    (setf center (append center (.floor 
				 (subseq 
				  (array-dimensions a) (length size))
				 2))))
  (let* ((srccenter (.round center))
	 (srcstart (.- srccenter (.floor size 2)))
	 (srcend (.+ srcstart (.- size 1)))
	 (dststart (loop for e in srcstart collect
			(if (< e 0)
			    (- e)
			    0)))
	 (dstend (loop for s in size
		    and se in srcend
		    and is in (array-dimensions a) collect
		      (+ s -1
			 (if (<= is se)
			     (+ (- se ) is -1)
			     0))))
	 (srcend2 (loop for se in srcend
		     and is in (array-dimensions a) collect
		       (if (<= is se)
			   (- is 1)
			   se)))
	 (srcstart2 (loop for ss in srcstart collect
			 (if (< ss 0) 0 ss))))
    (let ((dst (make-array dstend :element-type (array-element-type a)
			 :initial-element border)))
      (let* ((dim 2)
	     (start-l (list srcstart2 dststart))
	     (end-l (list srcend2 dstend))
	     (n (length end-l))
	     (start (make-array (list n dim) :element-type 'fixnum :initial-contents start-l))
	     (end (make-array (list n dim) :element-type 'fixnum :initial-contents end-l)))
	(typecase a
	  ((array * 2) (do-region (2 (a dst) start end)
			 (setf (dst) (a))))
	  ((array * 1) (do-region (1 (a dst) start end)
			 (setf (dst) (a))))
	  ((array * 3) (do-region (3 (a dst) start end)
			 (setf (dst) (a))))
	  ((array * 4) (do-region (4 (a dst) start end)
			 (setf (dst) (a))))((array * 2) (do-region (2 (a dst) start end)
			 (setf (dst) (a))))
	  ((array * 5) (do-region (5 (a dst) start end)
			 (setf (dst) (a))))
	  (t (error "array dimension not supported.")))
	dst))))

#+nil
(let ((a (make-array (list 10)))
      (a2 (make-array (list 10 10) :element-type '(complex double-float))))
  (typecase a
    ((array t 2) 2)
    ((array t 1) 1)
    ((array * 2) 's2)
    ((array * 1) 's1)
    (t 'nix)))

#+il
(let ((a (make-array (list 10 10))))
  (extract a '(3 3)))

#+il
(let ((a (make-array (list 10))))
  (eval (extract a '(3))))

#+nil
(let ((a 3)
      (q (make-array 4)))
  (symbol-macrolet ((b (aref q aa)))
    (let ((aa 2))
     (setf b a))
    q))
