#+nil
(ql:quickload :cl-ppcre)


(defun comma-list (list)
  "print elements of a list with commas in between"
  (with-output-to-string (s)
    (loop for e in (butlast list) do
	 (format s "~a," e))
    (format s "~a" (car (last list)))))


(defclass template ()
  ((header :accessor header :initarg :header)
   (globals :accessor globals :initarg :globals)
   (enums :accessor enums :initarg :enums)
   (lisp-name :accessor lisp-name :initarg :lisp-name)
   (setup :accessor setup :initarg :setup :documentation "these expressions will be called by setup() when the arduino program starts")
   (init :accessor init :initarg :init :documentation "definition of initialization functions that will get called from setup")
   (fun :accessor fun :initarg :fun :documentation "definition of functions that can be called from lisp")
   (stack :accessor stack :initarg :stack :documentation "code that will obtain parameters from the lisp stack and pass them to the new functions")))

(defmethod combine ((a template) (b template))
  (make-instance 'template
		 :header (concatenate 'string (slot-value a 'header) (slot-value b 'header))
		 :globals (concatenate 'string (slot-value a 'globals) (slot-value b 'globals))
		 :enums (comma-list (list (slot-value a 'enums) (slot-value b 'enums)))
		 :lisp-name (comma-list (list (slot-value a 'lisp-name) (slot-value b 'lisp-name)))
		 :setup (concatenate 'string (slot-value a 'setup) (slot-value b 'setup))
		 :init (concatenate 'string (slot-value a 'init) (slot-value b 'init))
		 :fun (concatenate 'string (slot-value a 'fun) (slot-value b 'fun))
		 :stack (concatenate 'string (slot-value a 'stack) (slot-value b 'stack))))

(defun emit-c-fun (name args fun)
    (if args
      (format nil "value_t ~a (~a) ~%{~a~%}~%"
	      name (comma-list args) fun)
      (format nil "value_t ~a () ~%{~a~%}~%"
	      name                fun)))

(defun emit-global (g)
  (format nil "~a~%" g))

(defun emit-header (g)
  (format nil "~a~%" g))

(defun emit-enum (name)
  (format nil "~a" name))

(defun emit-lisp-name (name)
  (format nil "~s" name))

(defun emit-to-setup (name)
  (format nil "~a~%" name))

(defun emit-case (lisp-name name enum-name args)
  (if args
      (format nil "case ~a: {~%  argcount(~s,nargs,~a); 
  v= ~a_fun(~a);
}  break;~%"
	      enum-name
	      lisp-name (length args)
	      name (comma-list (loop for i from (- (length args)) upto -1 collect
				    (format nil "tonumber(Stack[SP~a],~s)"
					    i lisp-name))))
      (format nil "case ~a: {~%  argcount(~s,nargs,~a); 
  v= ~a_fun();
}  break;~%"
	      enum-name
	      lisp-name (length args)
	      name )))

(defun parse-name-or-list (name-or-list)
  (cond ((consp name-or-list) (values (first name-or-list)
				      (second name-or-list)))
	(t (values name-or-list
		   name-or-list))))



(defmacro gen-c-chunks (name-or-list arglist &key init global fun to-setup header)
  (multiple-value-bind (lisp-name name) (parse-name-or-list name-or-list)
    (let ((enum-name (string-upcase (concatenate 'string "F_" name))))
      `(make-instance
	'template 
	:header (when ,header (emit-header ,header))
	:globals (when ,global (emit-global ,global))
	:enums  (when ,enum-name (emit-enum ,enum-name))
	:lisp-name  (emit-lisp-name ,lisp-name)
	:setup (when ,to-setup (emit-to-setup ,to-setup))
	:init (when ,init
		(emit-c-fun ,(concatenate 'string name "_init")
			    '() ,init))
	:fun (when ,fun
	       (emit-c-fun ,(concatenate 'string name "_fun")
			   ',arglist ,fun))
	:stack (emit-case ,lisp-name ,name ,enum-name ',arglist)))))

(defparameter *dac*
  (gen-c-chunks "dac" ("unsigned short b" "unsigned short a")
	       :header "#include <SPI/SPI.h>"
	       :global "const int dac_chip_select_pin = 16;"
	       :init "
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE2);
  pinMode(dac_chip_select_pin, OUTPUT);
"
	       :to-setup "
  dac_init();
  dac_fun(2048,2048);
  analogReadResolution(12);
"
	       :fun "
  digitalWrite(dac_chip_select_pin, LOW);
  byte x,y,z;
  x = (0xff0 & a) >> 4;
  y = ((0xf & a) << 4) + ((0xf00 & b) >> 8);
  z = 0xff & b;
  SPI.transfer(x); 
  SPI.transfer(y); 
  SPI.transfer(z); 
  digitalWrite(dac_chip_select_pin, HIGH);
  return T;"))


(defparameter *base*
  (reduce #'combine
	  (list (gen-c-chunks ("pin-mode" "pinMode") ("uint8_t pin" "uint8_t mode")
			      :fun "
  pinMode(pin,mode);
  return T;"
			      :to-setup "
  pinMode_fun(8,1);
  digitalWrite_fun(8,1);")
		
		(gen-c-chunks ("digital-write" "digitalWrite") ("uint32_t ulPin" "uint32_t ulVal")
			      :fun "
  digitalWrite(ulPin,ulVal);
  return T;")
		(gen-c-chunks ("analog-write" "analogWrite") ("uint32_t ulPin" "uint32_t ulVal")
			      :fun "
  analogWrite(ulPin,ulVal);
  return T;")

		(gen-c-chunks ("adc" "analogRead") ("uint32_t ulPin")
			      :to-setup "analogReadResolution(12);
"
			      :fun "
  return number(analogRead(ulPin));")

		(gen-c-chunks ("delay-microseconds" "delayMicroseconds") ("unsigned int us")
			      :fun "
  delayMicroseconds(us);
  return T;")

		(gen-c-chunks "delay" ("unsigned long ms")
			      :fun "
  delay(ms);
  return T;")

		(gen-c-chunks "micros" ()
			      :fun "
  return number(micros());")

		(gen-c-chunks "room" ()
			      :fun "
 	  {
 	    char s[80];
 	    snprintf(s,sizeof(s), \"heap: %d/%d, stack: %d/%d\",
 		     (curheap-fromspace)/8, heapsize/8, SP, N_STACK);
 	    Serial.println(s);
 	  }
 	  return number((curheap-fromspace)/8);"))))
#+nil
(defparameter *arducam*
  (reduce #'combine
	  (list (gen-c-chunks ("cam-write-reg" "myCAM_write_reg") ("uint8_t addr" "uint8_t data")
			      :header "#include <Wire/Wire.h>
#include <ArduCAM/ArduCAM.h>
#include <SPI/SPI.h>"
	       :global "const int slave_select_pin = 10;
ArduCAM myCAM(OV2640,slave_select_pin);"
	       :to-setup "
  Wire1.begin();
  pinMode(slave_select_pin,OUTPUT);
  SPI.begin();
  myCAM.write_reg(ARDUCHIP_MODE, 0x00);
  myCAM.set_format(BMP);
  myCAM.InitCAM();
"
	       
	       :fun "
  myCAM.write_reg(addr,data);
  return T;")
		(gen-c-chunks ("spi-clock" "spi_clock") ("uint8_t pin" "uint8_t divider") :fun  "
SPI.setClockDivider(pin,divider);
return T;")
		(gen-c-chunks ("cam-read-reg" "myCAM_read_reg") ("uint8_t addr")
			      :fun "
  return number(myCAM.read_reg(addr));
")
		(gen-c-chunks ("cam-read-fifo" "myCAM_read_fifo") ()
			      :fun "
  return number(myCAM.read_fifo());
")
		(gen-c-chunks ("cam-flush-fifo" "myCAM_flush_fifo") ()
			      :fun "
  myCAM.flush_fifo();
  return T;
")
		(gen-c-chunks ("cam-start-capture" "myCAM_start_capture") ()
			      :fun "
  myCAM.start_capture();
  return T;
")
		(gen-c-chunks ("cam-clear-fifo-flag" "myCAM_clear_fifo_flag") ()
			      :fun "
  myCAM.clear_fifo_flag();
  return T;
")
		(gen-c-chunks ("fifo-to-usb" "fifo_to_usb") ("uint16_t n")
			      :fun "
char*buf=(char*)malloc(n);
int i;
for(i=0;i<n;i++)
  buf[i]=myCAM.read_fifo();
int ret=USBD_Send(0x3, (void*)buf, n);
free(buf);
return number(ret);"))))

		   


;; +HEADERS+
;; #include <SPI/SPI.h>

;; +FUNCTIONS+

;; const int chipSelectPin = 16;
;; void setup_max532()
;; {
;;   // start the SPI library:
;;   SPI.begin();
;; }

;; +FUNCTIONS_ENUM+
;; F_DAC, F_DIGITALWRITE, F_PINMODE, F_ADC, F_DELAY, F_DELAYMICROSECONDS, F_MICROS, F_ROOM

;; +FUNCTIONS_NAMES+
;; "dac", "digital-write", "pin-mode",
;; "adc", "delay", "delay-microseconds", "micros", "room"

;; 	+FUNCTIONS_STACK_PROCESSING+
;; 	case F_DAC:
;; 	  argcount("dac", nargs, 2);
;; 	  writeDAC(tonumber(Stack[SP-2],"dac"),tonumber(Stack[SP-1],"dac"));
;; 	    v=T;
;; 	  break;

#+nil
(progn
  (defparameter *template-file*
    (with-open-file (s "/home/martin/arduino_due_lisp/arduino-femtolisp/arduino_due_lisp.template")
      (let ((a (make-string (file-length s))))
	(read-sequence a s)
	a)))
  (loop for (e slot) in `(("\\+HEADERS\\+" header)
			  ("\\+GLOBALS\\+" globals)
			  ("\\+ENUMS\\+" enums)
			  ("\\+LISP_NAME\\+" lisp-name)
			  ("\\+SETUP\\+" setup)
			  ("\\+INIT\\+" init)
			  ("\\+FUN\\+" fun)
			  ("\\+STACK\\+" stack)) do
       (let ((defs #+nil *base*
	       (combine *base* *dac*) 
	       #+nil   (combine *base* *arducam*)))
	(setf *template-file*
	      (cl-ppcre:regex-replace e *template-file*
				      (slot-value defs slot)))))
  (with-open-file (s "/home/martin/arduino_due_lisp/arduino-femtolisp/arduino_due_lisp.ino"
		     :if-exists :supersede
		     :direction :output
		     :if-does-not-exist :create)
    (format s "~a~%" *template-file*)))
