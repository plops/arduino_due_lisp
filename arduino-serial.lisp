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
  (declare (type string tty)
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
		      (open-serial "/dev/ttyACM0" :element-type '(unsigned-byte 8))))

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
	     (sleep .03d0))))
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
    (talk-arduino fd s "(+ 1 2 3)")))

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
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (talk-arduino fd s (string-downcase
			(format nil "~a" '(set 'list (lambda args args)))))))
#+nil
(list 1 23 4)
#+nil
(destructuring-bind (str fd) *ard8*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (talk-arduino fd s (string-downcase
			(format nil "~a" '(let ((i 0))
					   (while (< i 4)
					     (setq i (+ 1 i))
					     (print i))))))))

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


