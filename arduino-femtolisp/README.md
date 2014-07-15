# femtolisp for arduino due

## introduction

This is a port of femto lisp, that can run on the Arduino Due.  I
added some code to access digital and analogue outputs and timing
functions of the Arduino from within Lisp.

This is my an Arduino Due board, that runs femtolisp. Some digital
ports are connected to synchronization inputs of three cameras and a
laser shutter. The board on the right provides an additional two
channel DAC that I use to drive an XY mirror.

![Photography of my current Arduino Due system.](/arduino-femtolisp/docs/arduino-due.jpg?raw=true "Arduino Due with DAC board")

I've decided to port femtolisp/tiny/lisp.c, because only a few changes
were required to obtain a very flexible system that allows instant
reprogramming (instead of waiting to reflash the device with every
incremental change of the software). For the porting it was sufficient
to modify functions for input and output using the serial port and to
adjust the sizes of the heap and the stack.

Femtolisp -- like many others -- is based on only a few built-in
primitives such as cons, macro and set and that are implemented in
C. The rest of the Lisp language is defined at runtime with the help
of this primitive.

To be able to access various functions specific to the Arduino
(including input and output as well as timing) have added additional
primitives.


## usage

### general functions

```quote```

```cond```

```if```

```and```

```or```

```while```

```lambda```

```macro```

```label```

```progn```

```eq```

```atom```

```cons```

```car```

```cdr```

```read```

```eval```

```print```

```set```

```not```

```load```

```symbolp```

```numberp```

```+```

```-```

```*```

```/```

```<```


```prog1```

```apply```

```rplaca```

```rplacd```

```boundp```



### arduino due specific:

```
(room)
```

Displays sizes of heap and stack.

```
(delay n)
```

Wait n milliseconds.


```(delay-microseconds n)```

Wait n microseconds.

```(dac a b)```

Write analog voltage using a MAX432 DAC. The parameters a, b are
numbers in the range [0.2047]. Note: This needs extra hardware to be
attached to the Arduino Due.

```(adc chan)```

Read an internal ADC channel of the arduino, returns 32-bit number of
which only 12 bits are used.  The parameter chan is in the range [0.12].

```(pin-mode port direction)```

example:
```lisp
(pin-mode 8 1) ;; set pin 8 to output
```

```(digital-write port value)```

example generating pulse train on pin 8:
```lisp
(let ((i 0)) 
 (while (< i 10)
  (digitial-write 8 1)
  (delay 10)
  (digital-write 8 0)
  (delay 1)
  (set i (+ i 1))))
```

## serial communication

An important part of the development environment is the editor.  I
communicate with the Arduino Due femtolisp in Emacs using a Common
Lisp session in Slime using the package:
https://github.com/plops/arduino_due_lisp/tree/master/arduino-serial-sbcl

I tested one implementation in SBCL and another one in CCL.

example:
```common-lisp
;; this would only work in sbcl
(setf asdf:*central-registry*
	'(*default-pathname-defaults* ;; make the directory with the package known:
	  #p"/home/martin/arduino_due_lisp/arduino-serial-sbcl/"))
(asdf:load-system "arduino-serial-sbcl")

(defparameter *ard* 
  (multiple-value-list
   (arduino-serial-sbcl:open-serial 
    (first (directory "/dev/ttyACM0")))))

(arduino-serial-sbcl:talk-arduino
   (second *ard*) 
   (first *ard*)
   "(progn (+ 1 2))")

;; this returns:
;; => "3
;; > "
```