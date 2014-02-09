+HEADERS+
#include <SPI/SPI.h>

+FUNCTIONS+

const int chipSelectPin = 16;
void setup_max532()
{
  // start the SPI library:
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE2);
 //SPI.setClockDivider(); //default clock of 4MHz should work
  
  // initalize the  chip select pins:
  pinMode(chipSelectPin, OUTPUT);
  analogReadResolution(12);
}

void writeDAC(unsigned short b, unsigned short a)
{
  digitalWrite(chipSelectPin, LOW);
  byte x,y,z;
  x = (0xff0 & a) >> 4;
  y = ((0xf & a) << 4) + ((0xf00 & b) >> 8);
  z = 0xff & b;
  SPI.transfer(x); 
  SPI.transfer(y); 
  SPI.transfer(z); 
  digitalWrite(chipSelectPin, HIGH);
}

case F_DAC:
	  argcount("dac", nargs, 2);
	  writeDAC(tonumber(Stack[SP-2],"dac"),tonumber(Stack[SP-1],"dac"));
	    v=T;
	  break;

(defun emit-c-fun (name args fun return)
  (format t "~a ~a (~{~a,~}) ~%{~a~%}~%"
	  (or return "void") name args fun))

(defmacro gen-c-chunks (name arglist &key return init global fun)
  `(progn
     (emit-c-fun ,(concatenate 'string name "_init")
		 '() ,init nil)
     (emit-c-fun ,(concatenate 'string name "_fun")
		 ',arglist ,fun ,return)))

(gen-c-chunks "dac" ("unsigned short b" "unsigned short a")
	      :return nil
	      :global "const int dac_chip_select_pin = 16;"
	      :init "
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE2);
  pinMode(chipSelectPin, OUTPUT);
  analogReadResolution(12);"
	      :fun "
  digitalWrite(dac_chip_select_pin, LOW);
  byte x,y,z;
  x = (0xff0 & a) >> 4;
  y = ((0xf & a) << 4) + ((0xf00 & b) >> 8);
  z = 0xff & b;
  SPI.transfer(x); 
  SPI.transfer(y); 
  SPI.transfer(z); 
  digitalWrite(dac_chip_select_pin, HIGH);")

+FUNCTIONS_ENUM+
F_DAC, F_DIGITALWRITE, F_PINMODE, F_ADC, F_DELAY, F_DELAYMICROSECONDS, F_MICROS, F_ROOM

+FUNCTIONS_NAMES+
"dac", "digital-write", "pin-mode",
"adc", "delay", "delay-microseconds", "micros", "room"

	+FUNCTIONS_STACK_PROCESSING+
	case F_DAC:
	  argcount("dac", nargs, 2);
	  writeDAC(tonumber(Stack[SP-2],"dac"),tonumber(Stack[SP-1],"dac"));
	    v=T;
	  break;
	case F_DIGITALWRITE:
	  argcount("digital-write", nargs, 2);
	  digitalWrite(tonumber(Stack[SP-2],"digital-write"),tonumber(Stack[SP-1],"digital-write"));
	    v=T;
	  break;
	case F_PINMODE:
	  argcount("pin-mode", nargs, 2);
	  pinMode(tonumber(Stack[SP-2],"pin-mode"),tonumber(Stack[SP-1],"pin-mode"));
	  v=T;
	  break;
	case F_ADC:
	  argcount("adc", nargs, 1);
	  v = number(analogRead(tonumber(Stack[SP-1],"adc")));
	  break;
	case F_DELAY:
	  argcount("delay", nargs, 1);
	  delay(tonumber(Stack[SP-1],"delay"));
	  v = T;
	  break;
	case F_DELAYMICROSECONDS:
	  argcount("delay-microseconds", nargs, 1);
	  delayMicroseconds(tonumber(Stack[SP-1],"delay-microseconds"));
	  v = T;
	  break;
	case F_MICROS:
	  argcount("micros", nargs, 0);
	  v = number(micros());
	  break;
	case F_ROOM:
	  argcount("room", nargs, 0);
	  {
	    char s[80];
	    snprintf(s,sizeof(s), "heap: %d/%d, stack: %d/%d",
		     (curheap-fromspace)/8, heapsize/8, SP, N_STACK);
	    Serial.println(s);
	  }
	  v = number((curheap-fromspace)/8);
	  break;

