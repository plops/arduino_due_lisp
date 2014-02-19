(require :cl-ppcre)

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


(defun comma-list (list)
  "print elements of a list with commas in between"
  (with-output-to-string (s)
    (loop for e in (butlast list) do
	 (format s "~a," e))
    (format s "~a" (car (last list)))))

(defun emit-c-fun (name args fun)
  (format t "value_t ~a (~a) ~%{~a~%}~%"
	  name (comma-list args) fun))

(defun emit-global (g)
  (format t "~a~%" g))
(defun emit-header (g)
  (format t "~a~%" g))


;; case F_DAC:
;; 	  argcount("dac", nargs, 2);
;; 	  writeDAC(tonumber(Stack[SP-2],"dac"),tonumber(Stack[SP-1],"dac"));
;; 	    v=T;
;; 	  break;

(defun emit-enum (name)
  (format t "~a~%" name))

(defun emit-to-setup (name)
  (format t "~a~%" name))



#+nil
(comma-list '(1 2 3))

(defun emit-case (lisp-name name enum-name args)
  (format t "case ~a: {~%  argcount(~s,nargs,~a); 
  v= ~a_fun(~a);
}  break;~%"
	  enum-name
	  lisp-name (length args)
	  name (comma-list (loop for i from (- (length args)) upto -1 collect
		     (format nil "tonumber(Stack[SP~a],~s)"
			     i lisp-name)))))




(defun parse-name-or-list (name-or-list)
  (cond ((consp name-or-list) (values (first name-or-list)
				      (second name-or-list)))
	(t (values name-or-list
		   name-or-list))))

(defmacro gen-c-chunks (name-or-list arglist &key init global fun to-setup header)
  (multiple-value-bind (lisp-name name) (parse-name-or-list name-or-list)
    (let ((enum-name (string-upcase (concatenate 'string "F_" name))))
      `(progn
	 (when ,header (emit-header ,header))
	 (when ,global (emit-global ,global))
	 (when ,enum-name (emit-enum ,enum-name))
	 (when ,to-setup (emit-to-setup ,to-setup))
	 (when ,init
	   (emit-c-fun ,(concatenate 'string name "_init")
		       '() ,init))
	 (when ,fun
	   (emit-c-fun ,(concatenate 'string name "_fun")
		       ',arglist ,fun))
	 (emit-case ,lisp-name ,name ,enum-name ',arglist)))))


(gen-c-chunks "dac" ("unsigned short b" "unsigned short a")
	      :header "#include <SPI/SPI.h>"
	      :global "const int dac_chip_select_pin = 16;"
	      :init "
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE2);
  pinMode(chipSelectPin, OUTPUT);
  analogReadResolution(12);"
	      :to-setup "
  dac_init();
  dac_fun(2048,2048);"
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
  return T;")

(gen-c-chunks ("pin-mode" "pinMode") ("unint8_t pin" "uint8_t mode")
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

(gen-c-chunks ("adc" "analogRead") ("uint32_t ulPin")
	      :fun "
  return number(analogRead(ulPin));")

(gen-c-chunks ("delay-microseconds" "delayMicroseconds") ("unsigned int us")
	      :fun "
  delayMicroseconds(us));
  return T;")

(gen-c-chunks "delay" ("unsigned long ms")
	      :fun "
  delay(ms));
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
 	  return number((curheap-fromspace)/8);")

(defparameter *template*
 (with-open-file (s "arduino_due_lisp.template")
   (let ((a (make-string (file-length s))))
     (read-sequence a s)
     a)))
(format t "~a~%"
 (cl-ppcre:regex-replace "\\+FUNCTIONS_ENUM\\+" *template* "blabla"))



