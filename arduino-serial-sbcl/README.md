* arduino-serial-sbcl

This package allows to communicate with an Arduino (tested on Uno and
Due) using its USB connected serial port (usually /dev/ttyACM0) on
Linux.

Please see test-serial.lisp for a usage example (how to load the asd
system with ASDF).

In the easiest case the device can be opened like this:

```
(defparameter *ard* 
  (multiple-value-list
   (open-serial (first (directory "/dev/ttyACM0")))))
```

Now it is possible to communicate with the Arduino using character strings:

```
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
```
(close-serial (second *ard*))
```

