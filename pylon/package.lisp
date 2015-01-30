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

  FACTORY returns the instance of the transport level factory. It is
  quite an internal construct to the Pylon C++ library and not useful
  in this Common Lisp wrapper except that it is necessary for the call
  to CREATE.

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
(pylon:initialize)
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

An appropriate order can be established by asking each camera for
its serial number: `(parse-integer (pylon:cam-get-serial-number *cams* 2))`.

Other camera specific parameters are also accessible. For my application the
following code returns the parameters I have to set to appropriate values. The
region of interest is defined as `Width x Height + OffsetX + OffsetY`. I use
`ReverseX` to compensate for a reflection on a beam splitter.

The cameras maintain these parameters even when being powercycled. That is why I usually set
them in the graphics PylonViewer application, that comes with pylon.  Then I use the following
code to obtain the settings as an s-expression.

```common-lisp
(loop for j below 3 collect
      (append 
       (loop for e in '(\"BinningHorizontal\" \"BinningVertical\" 
			\"Width\" \"Height\"
			\"OffsetX\" \"OffsetY\" 
			\"ExposureTimeRaw\" \"GainRaw\" 
			\"GevTimestampTickFrequency\" \"GevSCPSPacketSize\") collect
			(pylon:get-value-i *cams* j e t nil)
			)
       (list :trigger-mode (pylon:get-value-e *cams* j \"TriggerMode\")
	     :last-error (pylon:get-value-e *cams* j \"LastError\")
	     :rate-p (pylon:get-value-b *cams* j \"AcquisitionFrameRateEnable\")
	     :reverse-x (pylon:get-value-b *cams* j \"ReverseX\")
	     :rate (pylon:get-value-f *cams* j \"ResultingFrameRateAbs\")
	     :temp (pylon:get-value-f *cams* j \"TemperatureAbs\"))))
```

I then store the camera specific parameters in a hash table that is indexed by the cameras
serial number.


```common-lisp
(defparameter *cam-parameters*
  `((21433565    1    1  280   280 nil        777  337 230  82  90  0   70 \"transmission with polrot (top)\")
    (21433566    1    1  512   512 nil        789  112 482 429  90  0 4200 \"backreflection with polrot\")  
    (21433540    1    1  280   280 t          985  427 206 213  90  0   70 \"transmission same pol\"))
  \"    id      binx  biny  w    h  reverse-x   x    y  kx  ky   d   g   e   name\")

(let ((a nil))
  (defun init-cam-parameter-hash ()
    (setf a (make-hash-table))
    (loop for (id      binx  biny  w   h rev    x    y kx  ky   d  g   e   name)
	  in *cam-parameters* do
	  (setf (gethash id a) (list id      binx  biny  w   h rev    x    y kx  ky   d  g   e   name))))
  (defun get-cam-parameters (cam)
    (gethash (parse-integer (pylon:cam-get-serial-number *cams* cam)) a)))

(init-cam-parameter-hash)

(loop for i below 3 collect
      (destructuring-bind (id      binx  biny  w   h rev    x    y  kx  ky   d   g   e   name)
	  (get-cam-parameters i)
	(let ((inc (pylon:get-inc-i *cams* i \"ExposureTimeRaw\")))
	  (pylon:set-value-i *cams* i \"ExposureTimeRaw\" (* inc (floor e inc))))
       (list e (pylon:get-value-i *cams* i \"ExposureTimeRaw\"))))
```

")

