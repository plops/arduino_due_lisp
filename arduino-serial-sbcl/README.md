# arduino-serial-sbcl

This package allows to communicate with an Arduino (tested on Uno and
Due) using its USB connected serial port (usually /dev/ttyACM0) on
Linux.

## usage:

``` open-serial tty [element-type]  => (values stream fd) ```

Open the serial device tty (e.g. "/dev/ttyACM0") and return a stream
of element-type (by default character a character stream) and the file
descriptor as multiple values. The file descriptor is convenient if
the device shall be closed again, later.

``` close-serial fd ```

Close the file descriptor fd.

``` serial-recv-length fd => bytes ```

Returns the number of bytes that are currently in the receive buffer.

``` read-response tty-fd tty-stream => string ```

Returns the data from the receive buffer as a string.

``` talk-arduino tty-fs tty-stream command &key (time 0.009d0) => string```

Sends command to the serial device, waits for the 'time' seconds and
returns the devices response as a string.


## example:

Please see test-serial.lisp for a usage example (how to load the asd
system with ASDF).


In the easiest case the device can be opened like this:

```common-lisp
(defparameter *ard* 
  (multiple-value-list
   (open-serial (first (directory "/dev/ttyACM0")))))
```

Now it is possible to communicate with the Arduino using character strings:

```common-lisp
(destructuring-bind (str fd) *ard*
  (talk-arduino fd str "(+ 1 2)"))
```

Here is the response to the above command of an Arduino Due that is
running femtolisp:

```
"3
> "
```

After use, the file descriptor can be closed:
```common-lisp
(close-serial (second *ard*))
```

It is also possible to open the serial port in binary:.

```common-lisp
(defparameter *ard8* 
  (multiple-value-list
   (open-serial (first (directory "/dev/ttyACM0")) 
		:element-type '(unsigned-byte 8))))
```

In that case I use sb-sys:make-fd-stream to convert the file
descriptor into a character stream:

```common-lisp
(defun talk-arduino-now (cmd &key (time .009d0))
 (destructuring-bind (str fd) *ard8*
   (let ((s
	  (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				 :external-format :latin-1 
				 :buffering :full)))
     (ensure-response-buffer-clear fd s)
     ;(sleep .3)
     (talk-arduino fd s cmd))))
```

This is the reason why I decided to have open-serial return the stream
as well as the filedescriptor.

I used the binary streams to implement a protocol for communication
with the ArduCAM camera adaptor board (see test-arducam.lisp).

The file test-dac-outputs.lisp contains calibration results when I
first build the amplified dual-channel DAC that now controls the
deflection angle of the fast steering mirror.