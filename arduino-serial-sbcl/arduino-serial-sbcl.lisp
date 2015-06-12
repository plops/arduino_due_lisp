(defpackage :arduino-serial-sbcl
  (:shadowing-import-from :cl close open ftruncate truncate time abort
			  read write)
  (:use :cl :sb-posix)
  (:export #:open-serial
	   #:close-serial
	   #:fd-type
	   #:serial-recv-length
	   #:read-response
	   #:talk-arduino))

(in-package :arduino-serial-sbcl)

(defconstant FIONREAD #x541B)
(defconstant IXANY #o4000)
(defconstant CRTSCTS #o20000000000)

(deftype fd-type ()
  `(unsigned-byte 31))

(defun open-serial (tty &key (element-type 'character) (rate B115200))
  (declare (type (or pathname string) tty)
	   (values stream fd-type &optional))
  (let* ((fd (sb-posix:open
	      tty (logior O-RDWR
			  O-NOCTTY #+nil (this terminal can't control this program)
			  O-NDELAY #+nil (we don't wait until dcd is space)
			  )))
	 (term (tcgetattr fd))
	 (baud-rate rate))

    (fcntl fd F-SETFL (logior O-RDWR O-NOCTTY)) #+nil (reset file status flags, clearing e.g. O-NDELAY)

    (cfsetispeed baud-rate term)
    (cfsetospeed baud-rate term)

    (macrolet ((set-flag (flag &key (on ()) (off ()))
		 `(setf ,flag (logior ,@on (logand ,flag ,@off)))))

    (setf
     (aref (termios-cc term) VMIN) 1 #+nil (wake up after 32 chars are read)
     (aref (termios-cc term) VTIME) 5 #+nil (wake up when no char arrived for .1 s))

     ;; check and strip parity, handshake off
     (set-flag (termios-iflag term)
	       :on ()
	       :off (IXON IXOFF IXANY
		     IGNBRK BRKINT PARMRK ISTRIP
		     INLCR IGNCR ICRNL))

     ;; process output
     (set-flag (termios-oflag term)
	       :off (OPOST))

     ;; canonical input but no echo
     (set-flag (termios-lflag term)
	       :on ()
	       :off (ICANON ECHO ECHONL IEXTEN ISIG))

     ;; enable receiver, local mode, 8N1 (no parity)
     (set-flag (termios-cflag term)
	       :on (CLOCAL CREAD
			   CS8 CRTSCTS)
	       :off (CSTOPB CSIZE PARENB)))

    (tcflush fd TCIFLUSH) #+nil (throw away any input data)

    (tcsetattr fd TCSANOW term) #+nil (set terminal port attributes)
    (values
     (sb-sys:make-fd-stream fd :input t :output t :element-type element-type
			    :buffering :full)
     fd)))

(defun close-serial (fd)
  (declare (fd-type fd)
	   (values null &optional))
  (fcntl fd F-SETFL 0) #+nil (reset file status flags, clearing e.g. O-NONBLOCK)
  (sb-posix:close fd) #+nil (this will set DTR low)
  nil)

(defun serial-recv-length (fd)
  (declare (fd-type fd)
	   (values (signed-byte 32) &optional))
  (sb-alien:with-alien ((bytes sb-alien:int))
    (ioctl fd FIONREAD (sb-alien:addr bytes))
    bytes))

(defun read-response (tty-fd tty-stream)
  (declare (fd-type tty-fd)
	   (stream tty-stream)
	   (values string &optional))
  (declare (fd-type tty-fd)
	   (stream tty-stream)
	   (values string &optional))
  (let ((n (serial-recv-length tty-fd)))
    (if (eq 0 n)
	""
	(let ((ret (make-string n)))
	  (dotimes (i n)
	    (setf (char ret i) (read-char tty-stream)))
	  ret))))

;; the arduino can be restarted by resetting dtr
;; c code is in the end of tty_ioctl(4)
;; more information on resetting arduino:
;; http://playground.arduino.cc/Main/DisablingAutoResetOnSerialConnection

(defun set-dtr (fd val)
  (declare (fd-type fd)
           (boolean val)
           (values (signed-byte 32) &optional))
  (let ((TIOCM-DTR 2)
        (TIOCMSET #x5418))
    (sb-alien:with-alien ((serial sb-alien:int))
      (setf (ldb (byte 1 TIOCM-DTR) serial) (if val 1 0))
      (arduino-serial-sbcl::ioctl fd TIOCMSET (sb-alien:addr serial))
      serial)))


(defun write-arduino (tty-stream command)
  (declare (stream tty-stream)
	   (string command))
  (format tty-stream "~a~a" command #\Return)
  (finish-output tty-stream))

(defun write-arduino-image (tty-stream data)
  (declare (type stream tty-stream)
	   (type (array (unsigned-byte 8) *) data))
  (write-sequence data tty-stream)
  (finish-output tty-stream))


#+nil
(defparameter *ard8-2* (multiple-value-list
		      (open-serial (car (last (directory "/dev/ttyACM*"))) :element-type '(unsigned-byte 8))))



(defun ensure-response-buffer-clear (fd str)
 (unless (= 0 (serial-recv-length fd))
   (read-response fd str)))

(defun talk-arduino (tty-fd tty-stream command &key (time .009d0))
  (declare (fd-type tty-fd)
	   (stream tty-stream)
	   (string command)
	   (values string &optional))
  (ensure-response-buffer-clear tty-fd tty-stream)
  (write-arduino tty-stream command)
  ;(sleep .1)
  (let ((n (do ((i 0 (1+ i))
		(n 0 (serial-recv-length tty-fd)))
	       ((or (< 0 n) (<= 30 i)) n)
	     (sleep time))))
    (if (eq 0 n)
	""
	(read-response tty-fd tty-stream))))


(defparameter *femto-lisp-system* "(set 'list (lambda args args))

 (set 'setq (macro (name val)
                  (list set (list quote name) val)))

  (setq f-body (lambda (e)
               (cond ((atom e)        e)
                     ((eq (cdr e) ()) (car e))
                     (t               (cons progn e)))))

 (setq defmacro
      (macro (name args . body)
             (list 'setq name (list 'macro args (f-body body)))))

 (defmacro defun (name args . body)
  (list 'setq name (list 'lambda args (f-body body))))

 (defmacro define (name . body)
  (if (symbolp name)
      (list 'setq name (car body))
      (cons 'defun (cons (car name) (cons (cdr name) body)))))

 (defun identity (x) x)
 (setq null not)
 (defun consp (x) (not (atom x)))

 (defun map (f lst)
  (if (atom lst) lst
      (cons (f (car lst)) (map f (cdr lst)))))

 (defmacro let (binds . body)
  (cons (list 'lambda (map car binds) (f-body body))
        (map cadr binds)))

 (defun nconc lsts
  (cond ((null lsts) ())
        ((null (cdr lsts)) (car lsts))
        (t ((lambda (l d) (if (null l) d
			      (prog1 l
				(while (consp (cdr l)) (set 'l (cdr l)))
				(rplacd l d))))
            (car lsts) (apply nconc (cdr lsts))))))

 (defun append lsts
  (cond ((null lsts) ())
        ((null (cdr lsts)) (car lsts))
        (t ((label append2 (lambda (l d)
                             (if (null l) d
				 (cons (car l)
				       (append2 (cdr l) d)))))
            (car lsts) (apply append (cdr lsts))))))

 (defun member (item lst)
  (cond ((atom lst) ())
        ((eq (car lst) item) lst)
        (t (member item (cdr lst)))))

 (defun macrop (e) (and (consp e) (eq (car e) 'macro) e))
 (defun macrocallp (e) (and (symbolp (car e))
                           (boundp (car e))
                           (macrop (eval (car e)))))
 (defun macroapply (m args) (apply (cons 'lambda (cdr m)) args))

 (defun macroexpand-1 (e)
  (if (atom e) e
      (let ((f (macrocallp e)))
	(if f (macroapply f (cdr e))
	    e))))

 (defun append.2 (l tail)
  (cond ((null l)  tail)
        ((atom l)  (cons l tail))
        (t         (cons (car l) (append.2 (cdr l) tail)))))

 (defun macroexpand (e)
  ((label mexpand
          (lambda (e env f)
            (progn
              (while (and (consp e)
                          (not (member (car e) env))
                          (set 'f (macrocallp e)))
                (set 'e (macroapply f (cdr e))))
              (if (and (consp e)
                       (not (or (eq (car e) 'quote)
                                (eq (car e)  quote))))
                  (let ((newenv
                         (if (and (or (eq (car e) 'lambda) (eq (car e) 'macro))
                                  (consp (cdr e)))
                             (append.2 (cadr e) env)
			     env)))
                    (map (lambda (x) (mexpand x newenv nil)) e))
		  e))))
   e nil nil))

 (defmacro defmacro (name args . body)
  (list 'setq name (list 'macro args (macroexpand (f-body body)))))

 (setq =   eq)
 (setq eql eq)
 (define (>  a b) (< b a))
 (define (<= a b) (not (< b a)))
 (define (>= a b) (not (< a b)))
 (define (mod x y) (- x (* (/ x y) y)))
 (define (abs x)   (if (< x 0) (- x) x))
 (define (truncate x) x)
 (setq K prog1)
 (define (funcall f . args) (apply f args))
 (define (symbol-function sym) (eval sym))
 (define (symbol-value    sym) (eval sym))

 (define (caar x) (car (car x)))
 (define (cadr x) (car (cdr x)))
 (define (cdar x) (cdr (car x)))
 (define (cddr x) (cdr (cdr x)))
 (define (caaar x) (car (car (car x))))
 (define (caadr x) (car (car (cdr x))))
 (define (cadar x) (car (cdr (car x))))
 (define (caddr x) (car (cdr (cdr x))))
 (define (cdaar x) (cdr (car (car x))))
 (define (cdadr x) (cdr (car (cdr x))))
 (define (cddar x) (cdr (cdr (car x))))
 (define (cdddr x) (cdr (cdr (cdr x))))

 (define (equal a b)
    (if (and (consp a) (consp b))
	(and (equal (car a) (car b))
	     (equal (cdr a) (cdr b)))
	(eq a b)))
"
 ;; (defun compare (a b)
 ;;  (cond ((eq a b) 0)
 ;;        ((or (atom a) (atom b)) (if (< a b) -1 1))
 ;;        (t (let ((c (compare (car a) (car b))))
 ;;             (if (not (eq c 0))
 ;;                 c
 ;; 		 (compare (cdr a) (cdr b)))))))

 ;; (defun every (pred lst)
 ;;  (or (atom lst)
 ;;      (and (pred (car lst))
 ;;           (every pred (cdr lst)))))

 ;; (defun any (pred lst)
 ;;  (and (consp lst)
 ;;       (or (pred (car lst))
 ;;           (any pred (cdr lst)))))

 ;; (defun listp (a) (or (eq a ()) (consp a)))

 ;; (defun length (l)
 ;;  (if (null l) 0
 ;;      (+ 1 (length (cdr l)))))

 ;;  (defun nthcdr (n lst)
 ;;  (if (<= n 0) lst
 ;;      (nthcdr (- n 1) (cdr lst))))

 ;; (defun list-ref (lst n)
 ;;  (car (nthcdr n lst)))

 ;; (defun list* l
 ;;  (if (atom (cdr l))
 ;;      (car l)
 ;;      (cons (car l) (apply list* (cdr l)))))

 ;; (defun nlist* l
 ;;  (if (atom (cdr l))
 ;;      (car l)
 ;;      (rplacd l (apply nlist* (cdr l)))))

 ;; (defun lastcdr (l)
 ;;  (if (atom l) l
 ;;      (lastcdr (cdr l))))

 ;; (defun last (l)
 ;;  (cond ((atom l)        l)
 ;;        ((atom (cdr l))  l)
 ;;        (t               (last (cdr l)))))

 ;; (defun map! (f lst)
 ;;  (prog1 lst
 ;;    (while (consp lst)
 ;;      (rplaca lst (f (car lst)))
 ;;      (set 'lst (cdr lst)))))

 ;; (defun mapcar (f . lsts)
 ;;  ((label mapcar-
 ;;          (lambda (lsts)
 ;;            (cond ((null lsts) (f))
 ;;                  ((atom (car lsts)) (car lsts))
 ;;                  (t (cons (apply f (map car lsts))
 ;;                           (mapcar- (map cdr lsts)))))))
 ;;   lsts))

 ;; (defun transpose (M) (apply mapcar (cons list M)))

 ;; (defun filter (pred lst)
 ;;  (cond ((null lst) ())
 ;;        ((not (pred (car lst))) (filter pred (cdr lst)))
 ;;        (t (cons (car lst) (filter pred (cdr lst))))))

 ;; (define (foldr f zero lst)
 ;;    (if (null lst) zero
 ;; 	(f (car lst) (foldr f zero (cdr lst)))))

 ;; (define (foldl f zero lst)
 ;;    (if (null lst) zero
 ;; 	(foldl f (f (car lst) zero) (cdr lst))))

 ;; (define (reverse lst) (foldl cons nil lst))

 ;; (define (reduce0 f zero lst)
 ;;    (if (null lst) zero
 ;; 	(reduce0 f (f zero (car lst)) (cdr lst))))

 ;; (defun reduce (f lst)
 ;;  (reduce0 f (car lst) (cdr lst)))

 ;; (define (copy-list l) (map identity l))
 ;; (define (copy-tree l)
 ;;    (if (atom l) l
 ;; 	(cons (copy-tree (car l))
 ;; 	      (copy-tree (cdr l)))))

 ;; (define (assoc item lst)
 ;;    (cond ((atom lst) ())
 ;; 	  ((eq (caar lst) item) (car lst))
 ;; 	  (t (assoc item (cdr lst)))))

 ;; (define (nreverse l)
 ;;    (let ((prev nil))
 ;;      (while (consp l)
 ;; 	(set 'l (prog1 (cdr l)
 ;; 		  (rplacd l (prog1 prev
 ;; 			      (set 'prev l))))))
 ;;      prev))

 ;; (defmacro let* (binds . body)
 ;;  (cons (list 'lambda (map car binds)
 ;;              (cons progn
 ;;                    (nconc (map (lambda (b) (cons 'setq b)) binds)
 ;;                           body)))
 ;;        (map (lambda (x) nil) binds)))

 ;; (defmacro labels (binds . body)
 ;;  (cons (list 'lambda (map car binds)
 ;;              (cons progn
 ;;                    (nconc (map (lambda (b)
 ;;                                  (list 'setq (car b) (cons 'lambda (cdr b))))
 ;;                                binds)
 ;;                           body)))
 ;;        (map (lambda (x) nil) binds)))

 ;;  (defmacro when   (c . body) (list if c (f-body body) nil))
 ;;  (defmacro unless (c . body) (list if c nil (f-body body)))

 ;; (defmacro dotimes (var . body)
 ;;  (let ((v (car var))
 ;;        (cnt (cadr var)))
 ;;    (list 'let (list (list v 0))
 ;;          (list while (list < v cnt)
 ;;                (list prog1 (f-body body) (list 'setq v (list + v 1)))))))

 ;; (defun map-int (f n)
 ;;  (let ((acc nil))
 ;;    (dotimes (i n)
 ;;      (setq acc (cons (f i) acc)))
 ;;    (nreverse acc)))

 ;; (setq *plists* nil)

 ;; (defun symbol-plist (sym)
 ;;  (cdr (or (assoc sym *plists*) '(()))))

 ;; (defun set-symbol-plist (sym lst)
 ;;  (let ((p (assoc sym *plists*)))
 ;;    (if (null p)
 ;;        (setq *plists* (cons (cons sym lst) *plists*))
 ;; 	(rplacd p lst))))

 ;; (defun get (sym prop)
 ;;  (let ((pl (symbol-plist sym)))
 ;;    (if pl
 ;;        (let ((pr (member prop pl)))
 ;;          (if pr (cadr pr) nil))
 ;; 	nil)))

 ;; (defun put (sym prop val)
 ;;  (let ((p (assoc sym *plists*)))
 ;;    (if (null p)
 ;;        (setq *plists* (cons (list sym prop val) *plists*))
 ;; 	(let ((pr (member prop p)))
 ;; 	  (if (null pr)
 ;; 	      (rplacd p (cons prop (cons val (cdr p))))
 ;; 	      (rplaca (cdr pr) val)))))
 ;;  val)
 ;; (setq *setf-place-list*
 ;;      '((car     rplaca   identity)
 ;;        (cdr     rplacd   identity)
 ;;        (caar    rplaca   car)
 ;;        (cadr    rplaca   cdr)
 ;;        (cdar    rplacd   car)
 ;;        (cddr    rplacd   cdr)
 ;;        (caaar   rplaca   caar)
 ;;        (caadr   rplaca   cadr)
 ;;        (cadar   rplaca   cdar)
 ;;        (caddr   rplaca   cddr)
 ;;        (cdaar   rplacd   caar)
 ;;        (cdadr   rplacd   cadr)
 ;;        (cddar   rplacd   cdar)
 ;;        (cdddr   rplacd   cddr)
 ;;        (get     put      identity)
 ;;        (aref    aset     identity)
 ;;        (symbol-function   set                identity)
 ;;        (symbol-value      set                identity)
 ;;        (symbol-plist      set-symbol-plist   identity)))

 ;; (defun setf-place-mutator (place val)
 ;;  (if (symbolp place)
 ;;      (list 'setq place val)
 ;;      (let ((mutator (assoc (car place) *setf-place-list*)))
 ;; 	(if (null mutator)
 ;; 	    (error '|setf: error: unknown place | (car place))
 ;; 	    (if (eq (caddr mutator) 'identity)
 ;; 		(cons (cadr mutator) (append (cdr place) (list val)))
 ;; 		(list (cadr mutator)
 ;; 		      (cons (caddr mutator) (cdr place))
 ;; 		      val))))))

 ;; (defmacro setf args
 ;;  (f-body
 ;;   ((label setf-
 ;;           (lambda (args)
 ;;             (if (null args)
 ;;                 nil
 ;; 		 (cons (setf-place-mutator (car args) (cadr args))
 ;; 		       (setf- (cddr args))))))
 ;;    args)))

 ;; (defun revappend (l1 l2) (nconc (reverse l1) l2))
 ;; (defun nreconc   (l1 l2) (nconc (nreverse l1) l2))

 ;; (defun builtinp (x)
 ;;  (and (atom x)
 ;;       (not (symbolp x))
 ;;       (not (numberp x))))

 ;; (defun self-evaluating-p (x)
 ;;  (or (eq x nil)
 ;;      (eq x t)
 ;;      (and (atom x)
 ;;           (not (symbolp x)))))

 ;; (defmacro backquote (x) (bq-process x))

 ;; (defun splice-form-p (x)
 ;;  (or (and (consp x) (or (eq (car x) '*comma-at*)
 ;;                         (eq (car x) '*comma-dot*)))
 ;;      (eq x '*comma*)))

 ;; (defun bq-process (x)
 ;;  (cond ((self-evaluating-p x)        x)
 ;;        ((atom x)                     (list quote x))
 ;;        ((eq (car x) 'backquote)      (bq-process (bq-process (cadr x))))
 ;;        ((eq (car x) '*comma*)        (cadr x))
 ;;        ((not (any splice-form-p x))
 ;;         (let ((lc    (lastcdr x))
 ;;               (forms (map bq-bracket1 x)))
 ;;           (if (null lc)
 ;;               (cons 'list forms)
 ;; 	       (nconc (cons 'nlist* forms) (list (bq-process lc))))))
 ;;        (t (let ((p x) (q '()))
 ;;             (while (and (consp p)
 ;;                         (not (eq (car p) '*comma*)))
 ;;               (setq q (cons (bq-bracket (car p)) q))
 ;;               (setq p (cdr p)))
 ;;             (cons 'nconc
 ;;                   (cond ((consp p) (nreconc q (list (cadr p))))
 ;;                         ((null p)  (nreverse q))
 ;;                         (t         (nreconc q (list (bq-process p))))))))))

 ;; (defun bq-bracket (x)
 ;;  (cond ((atom x)                   (list cons (bq-process x) nil))
 ;;        ((eq (car x) '*comma*)      (list cons (cadr x)       nil))
 ;;        ((eq (car x) '*comma-at*)   (list 'copy-list (cadr x)))
 ;;        ((eq (car x) '*comma-dot*)  (cadr x))
 ;;        (t                          (list cons (bq-process x) nil))))

 ;; (defun bq-bracket1 (x)
 ;;  (if (and (consp x) (eq (car x) '*comma*))
 ;;      (cadr x)
 ;;     (bq-process x)
 ;;)) "
 )





(defun upload-lisp-system (tty-fd tty-stream)
  (let ((sys (let* ((*readtable* (copy-readtable nil))
		    (start 0)
		    (line t))
	       (setf (readtable-case *readtable*) :preserve)
	       (loop while line collect
		    (multiple-value-setq (line start)
		      (read-from-string *femto-lisp-system* nil nil :start start))))))
    (dolist (s sys)
      (format t "~a~%"
	      (list
	       s
	       (talk-arduino tty-fd tty-stream
			     (format nil "~a" s)
			     :time 0.01d0)))
      (sleep .03))))

#+nil
(let ((sys (let* ((*readtable* (copy-readtable nil))
		    (start 0)
		    (line t))
	       (setf (readtable-case *readtable*) :preserve)
	       (loop while line collect
		    (multiple-value-setq (line start)
		      (read-from-string *femto-lisp-system* nil nil :start start))))))
    (dolist (s sys)
      (terpri)
      (format t "~a" s)
     ))
