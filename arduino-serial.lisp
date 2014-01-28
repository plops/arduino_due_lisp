(defpackage :serial
  (:shadowing-import-from :cl close open ftruncate truncate time abort
			  read write)
  (:use :cl :sb-posix)
  (:export #:open-serial
	   #:close-serial
	   #:fd-type
	   #:serial-recv-length
	   #:read-response))

(in-package :serial)
 
(defconstant FIONREAD #x541B)
(defconstant IXANY #o4000)
(defconstant CRTSCTS #o20000000000)
 
(deftype fd-type ()
  `(unsigned-byte 31))

(defun open-serial (tty &key (element-type 'character))
  (declare (type (or pathname string) tty)
	   (values stream fd-type &optional))
  (let* ((fd (sb-posix:open
	      tty (logior O-RDWR
			  O-NOCTTY #+nil (this terminal can't control this program)
			  O-NDELAY #+nil (we don't wait until dcd is space)
			  )))
	 (term (tcgetattr fd))
	 (baud-rate B115200))
 
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
		     INLCR IGNCR ICRNL
			  ))
 
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
(defparameter *ard* (multiple-value-list
		     (open-serial "/dev/ttyACM0")))

(defparameter *ard8* (multiple-value-list
		      (open-serial (first (directory "/dev/ttyACM*")) :element-type '(unsigned-byte 8))))



(defun ensure-response-buffer-clear (fd str)
 (unless (= 0 (serial-recv-length fd))
   (read-response fd str)))

(defun talk-arduino (tty-fd tty-stream command)
  (declare (fd-type tty-fd)
	   (stream tty-stream)
	   (string command)
	   (values string &optional))
  (ensure-response-buffer-clear tty-fd tty-stream)
  (write-arduino tty-stream command)
  (sleep .1)
  (let ((n (do ((i 0 (1+ i))
		(n 0 (serial-recv-length tty-fd)))
	       ((or (< 0 n) (<= 30 i)) n)
	     (sleep .08d0))))
    (if (eq 0 n)
	""
	(read-response tty-fd tty-stream))))



#+nil
(destructuring-bind (str fd) *ard8*
  (close-serial fd))

#+nil
(loop for i from 0 upto 10 do
     (format t "~a~%"
	     (destructuring-bind (str fd) *ard*
	       (talk-arduino fd str (format nil "(* 2 (+ 1 ~d))" i)))))

#+nil
(destructuring-bind (str fd) *ard*
  (ensure-response-buffer-clear fd str))

#+nil
(destructuring-bind (str fd) *ard*
  (talk-arduino fd str "()"))

#+nil
(destructuring-bind (str fd) *ard8*
  (let* ((n (serial-recv-length fd))
	 (a (make-array n :element-type '(unsigned-byte 8))))
    (read-sequence a str)
    (when (< 0 n)
     (map 'string #'(lambda (x) (code-char x)) a))))

#+nil
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (ensure-response-buffer-clear fd s)
    (talk-arduino fd s "(+ 1 2)")))

#+nil
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (ensure-response-buffer-clear fd s)
    (talk-arduino fd s "(room)")))


#+nil
(defparameter *response*
 (destructuring-bind (str fd) *ard8*
   (let ((s
	  (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				 :external-format :latin-1 
				 :buffering :full)))
     (ensure-response-buffer-clear fd s)
     (loop for i below 100 collect
	  (progn
	    (format t "~a~%" i)
	    (talk-arduino fd s (format nil "(setq var~a ~a)" i i)))))))

#+nil
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (read-response fd s)))



#+nil
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (ensure-response-buffer-clear fd s)
    (talk-arduino fd s "(bla)")))

#+nil
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (ensure-response-buffer-clear fd s)
    (talk-arduino fd s "(set 'list (lambda args args))
     (set 'setq (macro (name val) (list set (list quote name) val)))
    (setq f-body (lambda (e)
	       (cond ((atom e)        e)
		     ((eq (cdr e) ()) (car e))
		     (t               (cons progn e)))))")))
#+nil
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (ensure-response-buffer-clear fd s)
    (talk-arduino fd s "(set 'setq (macro (name val) (list set (list quote name) val)))")))

(defparameter *system*
 (with-open-file (s "system.lsp")
   (loop for l = (read s nil nil) while l collect l)))

(defparameter *system-lines*
 (with-open-file (s "system.lsp")
   (loop for l = (read-line s nil nil) while l collect l)))

#+nil
(with-open-file (s "lisp_system.h" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (format s "const char *lisp_system=~%")
  (loop for e in *system-lines* do
       (format s "\"~a\"~%" e))
  (format s ";~%"))


#+nil
(+ 1 2)
#+nil
(set 'list (lambda b b))
#+nil
(list 1 12)
#+nil
(defparameter *response*
  (loop for e in *system* collect
       (let ((def (string-downcase
		   (format nil "~a" e))))
	 (format t "input: ~a~%" def)
	 (format t "output: \"~a\"~%"
		 (destructuring-bind (str fd) *ard8*
		   (let ((s
			  (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
						 :external-format :latin-1 
						 :buffering :full)))
		     
		     (let ((resp (talk-arduino fd s def)))
		       (when (string= "" resp)
			 (break "error ~a.~%" def))
		       resp)
		     ))))))

#+nil
(progn
  (defparameter *bla* nil)
  (loop for j below 100 do
       (let ((i (if (= 0 (mod j 2)) 0 0)))
	 ;(sleep .1)
     (setf *bla*
	   (destructuring-bind (str fd) *ard8*
	     (let ((s
		    (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
					   :external-format :latin-1 
					   :buffering :full)))
	       (list
		(talk-arduino fd s (string-downcase
				    (format nil "~a" `(dac 2048 ,(+ 2048 i)))))
		#+nil (progn (sleep 1)
		       (ensure-response-buffer-clear fd s)))))))))

#+nil
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (list
     (talk-arduino fd s (string-downcase
			 (format nil "~a" `(dac 2048 2048))))
     #+nil (progn (sleep 1)
		  (ensure-response-buffer-clear fd s)))))

#+nil
(progn
						     (set 'i 0)
						     (while (< i 1000)
						      (dac (+ 2048 i) 2048)
						      (delay-microseconds 100)
						      (set 'i (+ i 1))))

#+nil
(progn
						      (set 'list (lambda args args))
						      (set 'x (lambda () (progn (delay-microseconds 100) (list (micros) (adc 0)))))
						      (dac 0 1000)
						      (delay 100)
						      (set 'start (list (micros) (adc 0)))
						      (dac 0 ,(+ 1110 (* 10 i)))
						      (list start ,@(loop for i below 230 collect '(x))))

#+nil
(read-from-string (first *bla*))

#+nil
(read-from-string (concatenate 'string (first *bla*) (second *bla*)))

#+nil
(with-open-file (s "step.dat" :direction :output :if-exists :supersede
		   :if-does-not-exist :create)
  (loop for b in *bla* do
       (let* ((dat (read-from-string (concatenate 'string (first b) (second b))))
	      (start (first (first dat))))
	 (loop for i from 0 and (micros adc) in dat do
	      (format s "~d ~d~%" (- micros start) adc)))
       (terpri s)))

#+nil
(let ((i 0))
  (while (< i 4095)
    (progn
      (dac i 0)
      (delay 1))
    (set 'i (+ i 1)))
  (while (< 0 i)
    (progn
      (dac i 0)
      (delay 1))
    (set 'i (- i 1))))
#+nil
(list 1 23 4)
#+nil
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (talk-arduino fd s (string-downcase
			(format nil "~a" '(defmacro dotimes2 (var . body)
					   (let ((v (car var))
						 (cnt (cadr var)))
					     (list 'let (list (list v 0))
						   (list 'while (list '< v cnt)
							 (list 'prog1 (f-body body)
							       (list 'setq v (list '+ v 1))))))))))))

#+nil
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (defparameter *a*
     (loop for a in `(,@(loop for i from 0 upto 4000 by 100 collect i) 4095) collect
	  (progn
	    (talk-arduino fd s (string-downcase
				(format nil "~a" `(dac ,a 0))))
	    (sleep .1)
	    (let ((resp
		   (talk-arduino fd s (string-downcase
				       (format nil "~a" `(progn
							   (set 'list (lambda args args))
							   (dac ,a 0)
							   (list ,@(loop for i below 100 collect '(adc 0)))))))))
	      (let ((vals (read-from-string resp)))
		(when (consp vals)
		  (let ((n (length vals)))
		    (let* ((avg (* (/ 1d0 n) (loop for e in vals sum e)))
			   (var (* (/ 1d0 n) (loop for e in vals sum (- (expt e 2) (expt avg 2))))))
		      (list a avg (sqrt var))))))))))

    (defparameter *b*
     (loop for b in `(,@(loop for i from 0 upto 4000 by 100 collect i) 4095) collect
	  (progn
	    (talk-arduino fd s (string-downcase
				(format nil "~a" `(dac 0 ,b))))
	    (sleep .1)
	    (let ((resp
		   (talk-arduino fd s (string-downcase
				       (format nil "~a" `(progn
							   (set 'list (lambda args args))
							   (dac 0 ,b)
							   (list ,@(loop for i below 100 collect '(adc 1)))))))))
	      (let ((vals (read-from-string resp)))
		(when (consp vals)
		  (let ((n (length vals)))
		    (let* ((avg (* (/ 1d0 n) (loop for e in vals sum e)))
			   (var (* (/ 1d0 n) (loop for e in vals sum (- (expt e 2) (expt avg 2))))))
		      (list b avg (sqrt var))))))))))))

;; results of fitting
;; channel A differential amplifier
;; a               = 0.00152444       +/- 5.183e-06    (0.34%)   V/DACADU
;; b               = 0.0155642        +/- 0.01174      (75.43%)  V
;; ;; channel B differential amplifier
;; a2              = 0.00150965       +/- 1.276e-05    (0.8454%) V/DACADU
;; b2              = 0.0598476        +/- 0.02891      (48.3%)   V
;; ;; channel A digitizer (ADC0)
;; s1              = 0.447223         +/- 0.0004653    (0.104%)  ADCADU/DACADU
;; o1              = -17.1033         +/- 1.119        (6.542%)  ADCADU
;; ;; channel B digitizer (ADC1)
;; s2              = 0.439523         +/- 0.0004876    (0.1109%) ADCADU/DACADU
;; o2              = -17.9308         +/- 1.172        (6.539%)  ADCADU

 


;; channel a+ measured amplified voltage (differential channel a+)
#+nil
(defparameter *av* '((0 0) (100 .165) (1000 1.56) (2000 3.06) (3000 4.61) (4095 6.24)))

#+nil
(loop for (adu volt) in (remove-if #'null *bv*)
   do
     (format t "~a ~f~%" adu volt))

#+nil
(loop for (adu adc err) in (remove-if #'null *b*)
   do
     (format t "~a ~f~%" adu adc))

;; channel b+
#+nil
(defparameter *bv* '((0 0) (100 .23) (1000 1.61) (2000 3.12) (3000 4.55) (4095 6.24)))

#+nil
(defparameter *a*
  '((0 0.0d0 0.0d0) (100 25.46d0 1.1083320801997525d0) NIL
    (300 115.3d0 1.4247806848774496d0)
    (400 159.89000000000001d0 1.1566762727719166d0)
    (500 204.56d0 1.79064234284669d0) NIL (700 294.36d0 1.315446692189472d0)
    (800 338.89d0 1.340857934312619d0) (900 384.02d0 1.3414917070276935d0)
    (1000 428.85d0 1.098863048787558d0) NIL
    (1200 518.19d0 1.128671785749849d0) (1300 563.02d0 1.183046913702319d0)
    (1400 607.74d0 1.213424904954972d0) NIL
    (1600 697.69d0 1.3091600360256948d0) (1700 741.96d0 1.4759403781691298d0)
    (1800 787.0600000000001d0 1.2947586647267082d0)
    (1900 831.74d0 0.9446692542611704d0) NIL
    (2100 921.48d0 1.5778466338548343d0) (2200 966.08d0 1.383329317217331d0)
    (2300 1011.1d0 1.4035668847336196d0) NIL
    (2500 1100.71d0 1.3137351331146943d0)
    (2600 1145.58d0 1.2343419299909317d0) (2700 1190.17d0 1.04933312149811d0)
    (2800 1235.41d0 1.087152243104639d0) NIL
    (3000 1324.82d0 1.38115893368247d0) (3100 1369.4d0 1.2884098725424002d0)
    (3200 1414.26d0 1.2619033243560545d0) NIL
    (3400 1504.22d0 1.1711532777651765d0)
    (3500 1548.6200000000001d0 1.770762547506388d0)
    (3600 1593.41d0 1.3645145656475783d0)
    (3700 1638.22d0 1.3607350954750046d0) NIL
    (3900 1728.18d0 1.2359611642566808d0)
    (4000 1772.78d0 1.054324428204316d0)
    (4095 1815.49d0 1.1789402019441408d0)))

#+nil
(defparameter *b*
  '((0 0.0d0 0.0d0) (100 23.57d0 1.234949391675633d0) NIL
    (300 111.87d0 1.101408189545751d0) (400 155.75d0 1.1779218989389746d0)
    (500 199.79d0 1.0420652570740656d0) NIL
    (700 287.95d0 1.0136567466359518d0) (800 331.92d0 1.0067770358835157d0)
    (900 376.13d0 0.9343982020546097d0) (1000 420.25d0 1.0037429949942367d0)
    NIL (1200 508.17d0 1.149391143168434d0)
    (1300 552.25d0 1.2031209415515964d0) (1400 596.13d0 1.7415797426449486d0)
    NIL (1600 684.42d0 1.3651373557499218d0)
    (1700 728.51d0 1.3152566289467107d0) (1800 772.27d0 1.018381068207499d0)
    (1900 816.1800000000001d0 1.4098226838325283d0) NIL
    (2100 904.3100000000001d0 1.146254770954569d0)
    (2200 948.44d0 1.3661625085963243d0) (2300 992.59d0 1.0497142467752956d0)
    NIL (2500 1080.84d0 1.120000000047564d0)
    (2600 1124.58d0 1.0693923509340169d0)
    (2700 1168.8600000000001d0 1.0678951257496703d0)
    (2800 1212.8700000000001d0 1.1802965727840689d0) NIL
    (3000 1300.94d0 1.1473447607152647d0)
    (3100 1344.91d0 1.0685972112461393d0)
    (3200 1388.8600000000001d0 1.2249081596973042d0) NIL
    (3400 1476.84d0 1.111035553126647d0)
    (3500 1521.23d0 0.9365361711334499d0)
    (3600 1565.08d0 1.0457533171139406d0)
    (3700 1608.94d0 1.093800712959488d0) NIL
    (3900 1697.25d0 1.2278029157808674d0)
    (4000 1740.98d0 1.3113351973765348d0)
    (4095 1782.81d0 1.5079456224810837d0)))


#+nil
(let ((i 0))
  (while (< i 4)
    (prog1
	(progn (print i))
      (setq i (+ 1 i)))))

#+nil
(defun f-body (e)
  (cond ((atom e)        e)
	((eq (cdr e) ()) (car e))
	(t               (cons progn e))))

#+nil
(defmacro while (check prog)
  `(loop while ,check do ,prog))

#+nil
(let ((i 0))
  (while (< i 10)
    (prog1
	(print i)
      (incf i))))

#+nil
(defmacro dotimes2 (var . body)
  (let ((v (car var))
        (cnt (cadr var)))
    (list 'let (list (list v 0))
          (list 'while (list '< v cnt)
                (list 'prog1 (f-body body)
		      (list 'setq v (list '+ v 1)))))))

#+nil
(dotimes2 (i 10)
	  )

#+nil
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (ensure-response-buffer-clear fd s)))


#+nil
(destructuring-bind (str fd) *ard*
  (talk-arduino fd str "(list 1 2 3)"))


