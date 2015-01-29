#+generate-pylon-doc(mgl-pax:define-package #:pylon
  (:documentation "CFFI binding to the Pylon library for Basler Gig-E
  cameras (it uses a C wrapper to this C++ library). See
  PYLON:@PYLON-MANUAL.")
  (:use #:common-lisp #:mgl-pax))

#-generate-pylon-doc
(defpackage #:pylon
    (:use #:cl)
    (:export
     #:initialize
     #:terminate
     #:create
     #:factory
     #:start-grabbing
     #:grab
     #:grab-cdf
     #:get-max-i
     #:get-min-i
     #:get-inc-i
     #:get-value-f
     #:get-value-b
     #:get-value-i
     #:set-value-i
     #:get-symbolics-e
     #:set-value-e
     #:get-value-e
     #:to-string-e
     #:from-string-e
     #:stop-grabbing
     #:cams-open
     #:cams-close
     #:cam-open
     #:cam-close
     #:cam-get-serial-number
     #:cam-get-full-name))

(in-package #:pylon)

#+generate-pylon-doc
(defsection @pylon-manual (:title "Pylon Basler Gig-E Camera library manual")
  "This package provides a CFFI interface to the Basler Pylon library for communicating with their Gig-E vision cameras.

I ran into problems when I tried to read out three Gig-E cameras
simultaneously using the free software aravis.

The cameras (Basler) come with a proprietary development kit Pylon
that contains a rather primitive Viewer application. This application
seems to work well with all three cameras but doesn't allow to store
the images.

I decided to call the Pylon SDK from Common Lisp. Unfortunately, Pylon
only comes with a C++ interface and I had to create a few C wrapper
functions first. 

Before finding this solution I have already started to call the C++
library directly from cling (a LLVM based C++ REPL). Using this
command line interface it is quite easy to test the
library. Unfortunately, Cling is not very well integrated with emacs
and I didn't learn enough about it to understand how to do
introspection. Therefore, I still prefer developing in Common Lisp.
"
  (initialize function)
  (terminate function)
  (create function)
  (factory function)
  (start-grabbing function)
  (grab-sf function)
  (grab-cdf function)
  (grab function)
  
  (get-max-i function)
  (get-min-i function)
  (get-inc-i function)
  (get-value-f function)
  (get-value-b function)
  (get-value-i function)
  (set-value-i function)
  (get-symbolics-e function)
  (set-value-e function)
  (get-value-e function)
  (to-string-e function)
  (from-string-e function)
  (stop-grabbing function)
  (cams-open function)
  (cams-close function)
  (cam-open function)
  (cam-close function)
  (cam-get-serial-number function)
  (cam-get-full-name function)

  (@pylon-return section)
  (@pylon-example section))

#+generate-pylon-doc
(defsection @pylon-return (:title "Return values and errors")
  "Pylon uses C++ exceptions to notify the user of its error
conditions. Each C wrapper function catches the exceptions and prints
the error on stdout. In Emacs these notifications are shown in the
`*inferior-lisp*` buffer. In case of an error functions that should
return a pointer will return `NULL`. Functions that should return
`int` will return -1.

Here is an example of the *inferior-lisp* buffer in SBCL:

```
(progn (load \"/home/martin/quicklisp/dists/quicklisp/software/slime-2.7/swank-loader.lisp\" :verbose t) 
       (funcall (read-from-string \"swank-loader:init\")) (funcall (read-from-string \"swank:start-server\") \"/tmp/slime.28966\"))

This is SBCL 1.2.0.129-c06b8c1, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
*
; loading #P\"/home/martin/quicklisp/dists/quicklisp/software/slime-2.7/swank-loader.lisp\"
STYLE-WARNING: redefining EMACS-INSPECT (#<BUILT-IN-CLASS T>) in DEFMETHOD
STYLE-WARNING:
   Implicitly creating new generic function
   SWANK-BACKEND::STREAM-READ-CHAR-WILL-HANG-P.
WARNING: These Swank interfaces are unimplemented:
 (DISASSEMBLE-FRAME SLDB-BREAK-AT-START SLDB-BREAK-ON-RETURN)
;; Swank started at port: 33794.
33794
* STYLE-WARNING:
   redefining THREAD-FOR-EVALUATION (#<STRUCTURE-CLASS
                                       MULTITHREADED-CONNECTION>
                                     #<SB-MOP:EQL-SPECIALIZER
                                       {100584C683}>) in DEFMETHOD
creating factory
finding devices
preparing camera array
attaching cameras
camera 0
Using device Basler acA1920-25gm#00305315DFDD#192.168.4.100:3956
camera 1
Using device Basler acA1920-25gm#00305315DFDE#192.168.5.102:3956
camera 2
Using device Basler acA1920-25gm#00305315DFC4#192.168.6.101:3956
max(OffsetX)=896
max(Width)=1468
0 Mono8
1 Mono12
2 Mono12Packed
3 YUV422Packed
4 YUV422_YUYV_Packed

-=--:**--F1  *inferior-lisp*   All L42    (Comint:run) ----------------------------
```")


#+generate-pylon-doc
(defsection @pylon-example (:title "Example usage")
  "The following code will open three cameras, reset their frame
  timers, set them to external trigger and acquire 100 images (overall
  300 calls to grab-sf). The triggers are generated by the function
  TRIGGER-ALL-CAMERAS and isn't documented here. In my code I instruct
  the femtolisp running on an Arduino Due to create the trigger
  pulses with the correct timing.

  The (FACTORY FUNCTION) returns the instance of the transport level
  factory. It is quite an internal construct to the Pylon C++ library
  and not useful in this Common Lisp wrapper except that it is
  necessary for the call to CREATE.

  (CREATE FUNCTION) tries to open at most MAX-CAMERAS and returns an
  opaque pointer handle to an array of these cameras. Some other
  functions of this wrapper can be given this
  handle (e.g. COMMAND-EXECUTE, CAM-OPEN, CAM-CLOSE, START-GRABBING,
  TERMINATE, GRAB-SF, GET-VALUE-I, CAM-GET-SERIAL-NUMBER,
  CAM-GET-FULL-NAME) in this case the function parameter is called
  CAMS. Most of these functions also have a following integer
  parameter CAM which indicates which camera of the array is to be
  accessed.


```common-lisp
(defparameter *fact* (pylon::factory) \"Handle to Factory, which is needed for call to PYLON:CREATE.\")
(defparameter *cams* (pylon:create *fact* 3) \"Handle to multiple Pylon cameras.\")
(dotimes (i 3)
  (pylon:set-value-e *cams* i \"TriggerMode\" 1))
(let* ((old 0)
       (buf-s (make-array (list 1024 1024) :element-type 'single-float)))
  (unwind-protect 
       (progn
	 (dotimes (i 3) ;; reset frame timers on the cameras ;
	   (pylon::command-execute *cams* i \"GevTimestampControlReset\"))
	 (pylon:start-grabbing *cams*)
	 (let ((th (sb-thread:make-thread 
		    #'(lambda ()
			(loop for i below 100 do
			     (dotimes (some-cam 3)
			       (multiple-value-bind (cam success-p w h framenr timestamp) 
				   (pylon::grab-sf *cams* buf-s)
				 (declare (ignorable framenr)
					  (type (unsigned-byte 32) w h))
				 (if success-p
				     (progn
				     ;; acquired data is in buf-s ;
				     ;; do something with it ;
				       ) 
				     (format t \"acquisition error.~%\"))))))
		    :name \"camera-acquisition\")))
	   (sleep .001)
	   (dotimes (i 100)
	     (trigger-all-cameras))
	   (sb-thread:join-thread th)))
    (progn
      (pylon:stop-grabbing *cams*)
      (dotimes (i 3)
	(pylon:set-value-e *cams* i \"TriggerMode\" 0)))))
```

Note that repeated reruns of CREATE (after TERMINATE) will
not necessarily sort the cameras in the same order. 

")

