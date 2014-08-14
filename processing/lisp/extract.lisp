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

(defmacro do-region ((dim &rest rest) &body body)
  (let ((spec (loop for (name ends starts) in rest collect 
		   (let ((indices (loop for e in ends collect (gensym))))
		     (list name (reverse ends) (reverse starts) (reverse indices))))))
    (labels ((rec (dim acc)
	       (if (= dim 1)
		   acc
		   (rec (1- dim)
			(loop for (name ends starts indices) in spec collect
			     (list name (cdr ends) (cdr starts) (cdr indices)))
			`((loop for
			       ,@(flatten 
				  (loop for i from (1- (length spec)) downto 0 and
				       (name ends starts indices) in spec collect 
				       (if (= i 0)
					   `(,(car indices) from ,(car starts) below ,(car ends))
					   `(,(car indices) from ,(car starts) below ,(car ends) and)))) 
			     do
			       ,@acc))))))
      `(symbol-macrolet (,@(loop for (name ends starts indices) in spec collect
				`(,name (aref ,name ,@(loop for ind in indices collect ind)))))
	 (let (())
	  ,(first (rec dim spec)))))))

#+nil
(do-region (3 (src (5 4 2) (0 0 0)) (dst (10 8 4) (0 0 0)))
  (setf dst src))

(defun fun-do-region (rest body)
  (let ((spec (loop for (name ends starts) in rest collect 
		   (let ((indices (loop for e in ends collect (gensym))))
		     (list name (reverse ends) (reverse starts) (reverse indices))))))
    (labels ((rec (spec acc)
	       (if (null (second (first spec)))
		   acc
		   (rec (loop for (name ends starts indices) in spec collect
			     (list name (cdr ends) (cdr starts) (cdr indices)))
			`((loop for
			       ,@(flatten 
				  (loop for i from (1- (length spec)) downto 0 and
					       (name ends starts indices) in spec collect 
				       (if (= i 0)
					   `(,(car indices) from ,(car starts) below ,(car ends))
					   `(,(car indices) from ,(car starts) below ,(car ends) and)))) 
			     do
			       ,@acc))))))
      `(symbol-macrolet () #+nil (,@(loop for (name ends starts indices) in spec collect
				   `(,name (aref ,name ,@(loop for ind in indices collect ind)))
				   )) 
	 ,(first (rec spec body)))
      )))
#+nil
(fun-do-region '((src (5 4 2) (0 0 0))
		 (dst (10 8 4) (0 0 0)))
	       '((setf dst src)))

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
    (let ((b (make-array dstend :element-type (array-element-type a)
			 :initial-element border)))
      (fun-do-region `((a ,srcend2 ,srcstart2)
		       (b ,dstend ,dststart))
		     '((setf b a))))))

(let ((a (make-array (list 10)))
      (a2 (make-array (list 10 10) :element-type '(complex double-float))))
  (typecase a
    ((array t 2) 2)
    ((array t 1) 1)
    ((array * 2) 's2)
    ((array * 1) 's1)
    (t 'nix)))

#+il
(let ((a (make-array (list 10))))
  (extract a '(3)))

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
