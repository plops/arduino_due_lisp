;; I used the following code to program the an ArduCAM board, that was
;; connected to an Arduino Due running femtolisp. Unfortunately the
;; ArduCAM couldn't acquire images fast enough to be interesting for
;; my application.


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
    (sb-thread:make-thread 
     #'(lambda ()
	 (format t "acquisition program starts .. ")
	 (sb-ext:run-program "/home/martin/aravis/tests/arv-example" nil)
	 (format t "acquisition program ended~%")))
    (sleep .3)
    (talk-arduino fd s "(dotimes (i 2000)(delay-microseconds 5000)(dac 1900 (+ 1000 i)))")))



#+nil
(defun read-arduino-usb (&key (time .009))
 (destructuring-bind (str fd) *ard8-2*
   (let* ((n (do ((i 0 (1+ i))
		  (n 0 (serial-recv-length fd)))
		 ((or (< 0 n) (<= 30 i)) n)
	       (sleep time)))
	  (a (make-array n :element-type '(unsigned-byte 8))))
     (read-sequence a str)
     a)))

(defun write-reg (addr mode)
  (format nil "(cam-write-reg ~a ~a)" addr mode))
(defun read-reg (addr)
  (format nil "(cam-read-reg ~a)" addr))

#+nil
(talk-arduino-now (write-reg +arduchip-mode+ +mode-cam2lcd+)
		  :time (* 1 .009d0))

(defconstant +arduchip-mode+ #x2 "address for switching communication direction") 
(defconstant +mode-mcu2lcd+ #x0)
(defconstant +mode-cam2lcd+ #x1)
(defconstant +mode-lcd2mcu+ #x2)

(defconstant +arduchip-trig+ #x41 "address for trigger source")
(defconstant +mask-vsync+ #x01)
(defconstant +mask-shutter+ #x02)
(defconstant +mask-cap-done+ #x08)

(defconstant +arduchip-tim+ #x03 "address for timing control")
(defconstant +mask-href-level+ #x1 "0 = High active , 		1 = Low active")
(defconstant +mask-vsync-level+ #x2 "0 = High active , 		1 = Low active")
(defconstant +mask-lcd-bken+ #x4 "0 = Enable, 			1 = Disable")
(defconstant +mask-delay+ #x8 "0 = no delay, 			1 = delay one clock")
(defconstant +mask-mode+ #x10 "0 = LCD mode, 			1 = FIFO mode")
(defconstant +mask-fifo-pwrdn+ #x20 "0 = Normal operation, 	1 = FIFO power down")


#+nil
(talk-arduino-now (read-reg +arduchip-trig+))
#+nil
(talk-arduino-now (write-reg +arduchip-tim+ +mask-mode+))
#+nil
(talk-arduino-now "(cam-flush-fifo)")

#+nil
(talk-arduino-now "(cam-start-capture)")

#+nil
(/= 0 (logand +mask-cap-done+ (read-from-string (talk-arduino-now (read-reg +arduchip-trig+)))))

#+nil
(time
 (let* ((a (make-array (list 240 320 2) :element-type '(unsigned-byte 8)))
	(a1 (make-array (array-total-size a) :element-type '(unsigned-byte 8)
			:displaced-to a)))
   (loop for j below 240 do
	(loop for i below 320 do
	     (defparameter *bla* a)
	     (setf (aref a j i 0) (read-from-string (talk-arduino-now "(cam-read-fifo)"))
		   (aref a j i 1) (read-from-string (talk-arduino-now "(cam-read-fifo)")))
	     (format t "~a~%" (list j i (aref a j i 0) (aref a j i 1)))))))
#+nil
(talk-arduino-now "(+ 1 2)")
#+nil
(talk-arduino-now "(spi-clock 78 6)") ;; default spi pin is 78u on arduino due
#+nil
(list
  (talk-arduino-now (write-reg +arduchip-mode+ +mode-cam2lcd+)
		    :time (* 1 .009d0))
  (talk-arduino-now (read-reg +arduchip-trig+))
  (talk-arduino-now (write-reg +arduchip-tim+ +mask-mode+))
  
  (talk-arduino-now "(cam-flush-fifo)")
  (talk-arduino-now "(cam-start-capture)")
  (sleep .1)
  (dotimes (i 240)
    (format t "~a~%" i)
    (dotimes (j 10)
     (talk-arduino-now (format nil "(fifo-to-usb ~d)" 64)))))

;; plug the native port into usb, check which /dev/ttyACM? pops up in dmesg
;; run 'cat /dev/ttyACM1 > raw.dat' or hexdump /dev/ttyACM1 to receive the image data
#+nil
(with-open-file (s "raw.dat" :element-type '(unsigned-byte 8))
  (let* ((a (make-array '(240 640) :element-type '(unsigned-byte 8)))
	 (a1 (make-array (* 240 640) :element-type '(unsigned-byte 8)
			 :displaced-to a)))
    (read-sequence a1 s)

    (write-pgm "raw.pgm" a)))

#+nil
(read-arduino-usb)

#+nil
(defun write-pgm (filename img)
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
                       :element-type (array-element-type img)
                       :direction :output
                       :if-exists :append)
      (let ((data-1d (make-array 
                      (* h w)
                      :element-type (array-element-type img)
                      :displaced-to img)))
        (write-sequence data-1d s)))
    nil))

#+nil
(talk-arduino-now "(cam-read-fifo)")
#+nil
(talk-arduino-now "(+ 1 2)")
