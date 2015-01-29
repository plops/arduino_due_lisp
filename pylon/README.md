<a name='x-28PYLON-3A-40PYLON-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# Pylon Basler Gig-E Camera library manual

## Table of Contents

- [1 Return values and errors][6a88]
- [2 Example usage][f0b3]

###### \[in package PYLON\]
This package provides a `CFFI` interface to the Basler Pylon library for communicating with their Gig-E vision cameras.

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

<a name='x-28PYLON-3AINITIALIZE-20FUNCTION-29'></a>

- [function] **INITIALIZE** 

    Initializes the pylon runtime system. Call this before any other
    pylon function. Don't forget to call [`terminate`][9d0b] before you close the
    lisp session. Otherwise cameras may need to be power cycled.

<a name='x-28PYLON-3ATERMINATE-20FUNCTION-29'></a>

- [function] **TERMINATE** *CAMS*

    Deletes the opaque pointer to the CInstantCameraArray. Also
    terminates the Pylon runtime and should close the cameras properly, so
    that they can be opened by another Gig-e client program.

<a name='x-28PYLON-3ACREATE-20FUNCTION-29'></a>

- [function] **CREATE** *FACTORY MAX-CAMERAS*

    Given a transport level factory open `n` cameras. Error messages
    and the full names of the cameras are printed on stdout.  Returns an
    opaque pointer to a CInstantCameraArray which needs to be given as a
    handle to all other functions that access the cameras in some way.

<a name='x-28PYLON-3AFACTORY-20FUNCTION-29'></a>

- [function] **FACTORY** 

    Return a transport level factory. Its return value is treated as an
    opaque pointer and only necessary as the first argument of [`create`][98d0].

<a name='x-28PYLON-3ASTART-GRABBING-20FUNCTION-29'></a>

- [function] **START-GRABBING** *CAMS*

    Start acquisition on all cameras of the handle.
    
    Example:
    
    ```common-lisp
    (progn
      (defparameter *cams* (pylon:create (pylon::factory) 3) "Handle to multiple Pylon cameras.")
      (unwind-protect 
          (progn
    	(pylon:start-grabbing *cams*)
    	(let ((th (sb-thread:make-thread 
    		   #'(lambda ()
    		       (loop for i below 100 do
    			     (multiple-value-bind (cam success-p w h framenr timestamp) 
    				 (pylon::grab-store *cams* fds))))
    		   :name "camera-acquisition")))
    	  (sleep .001)
    	  (sb-thread:join-thread th)))
        (pylon:stop-grabbing *cams*)))
    ```


<a name='x-28PYLON-3AGRAB-SF-20FUNCTION-29'></a>

- [function] **GRAB-SF** *CAMS BUF*

    => (values cam success-p w h imagenr timestamp)
    
    Copies one acquired image into an array `buf` which should be a 2D
    array of single-float. With dimensions being at least corresponding to
    the width and height of the currently used region of interst. My
    wrapper code converts the 12-bit packed mono format to single-float. 
    
    When using multiple cameras the image data originates from one of
    these and the return value `CAM` indicates which one.
    
    [`GRAB-SF`][8481] also returns the width and height of the acquired image data,
    allowing to use this function with a larger than necessary array
    `BUF`. This comes handy when several cameras with different regions of
    interest are to be read out. A single array with size to store the
    largest image is sufficient in this case.
    
    The `IMAGENR` and `TIMESTAMP` indicate acquisition time of the image.
    This code allows to reset the counter on the camera:
    
    ```common-lisp
    (pylon::command-execute CAMS 0 "GevTimestampControlReset")
    ```
    
    The data can be fouriertransformed using FFTW:RFTF.
    
    In case of an error, all four return values are -1.

<a name='x-28PYLON-3AGRAB-CDF-20FUNCTION-29'></a>

- [function] **GRAB-CDF** *CAMS BUF*

    =>  (values cam success-p w h frame-nr) 
    
    Like [`GRAB-SF`][8481] but converts the acquired into (complex
    double-float).

<a name='x-28PYLON-3AGRAB-20FUNCTION-29'></a>

- [function] **GRAB** *CAMS BUF*

    => (values cam success-p w h frame-nr)
    
    OBSOLETE: This wrapper doesn't do the conversion from MONO12P. Use
    [`GRAB-CDF`][4caf] or [`GRAB-SF`][8481] instead.
    
    Copies one acquired image into an array `buf` of (unsigned-byte
    8). The length of the array `BUF` must be at least `w*h`.  The image data
    originates from one of the cameras in the handle `CAMS` as indicated by
    the camera index `CAM` of the return values. The returned values `w` and
    `h` indicate the dimensions of the returned image data in `buf`.
    
    In case of an error, all four return values are -1.

<a name='x-28PYLON-3AGET-MAX-I-20FUNCTION-29'></a>

- [function] **GET-MAX-I** *CAMS CAM NODE*

    => int
    
    Return the maximum value of a Genicam integer node. The parameter
    `node` is a string as defined by the Genicam standard. The return
    value is an integer.
    
    Example:
    
    ```common-lisp
    (pylon:get-max-i *cams* 0 "Width")
    ```


<a name='x-28PYLON-3AGET-MIN-I-20FUNCTION-29'></a>

- [function] **GET-MIN-I** *CAMS CAM NODE*

    => int
    
    Return the minimum value of a Genicam integer node. The parameter
    `node` is a string as defined by the Genicam standard. 
    
    Example:
    
    ```common-lisp
    (pylon:get-min-i *cams* 0 "Width")
    ```


<a name='x-28PYLON-3AGET-INC-I-20FUNCTION-29'></a>

- [function] **GET-INC-I** *CAMS CAM NODE*

    => int
    
    Return the increment value of a Genicam integer node. The parameter
    `node` is a string as defined by the Genicam standard. 
    
    Example:
    
    ```common-lisp
    (pylon:get-inc-i *cams* 0 "Width")
    ```


<a name='x-28PYLON-3AGET-VALUE-F-20FUNCTION-29'></a>

- [function] **GET-VALUE-F** *CAMS CAM NODE &OPTIONAL (VERIFY NIL) (IGNORE-CACHE NIL)*

    => single-float
    
    Return the current value of a Genicam float node. The parameter
    `node` is a string as defined by the Genicam standard. 
    
    Example:
    
    ```common-lisp
    (pylon:get-value-f *cams* 0 "ResultingFrameRateAbs")
    ```


<a name='x-28PYLON-3AGET-VALUE-B-20FUNCTION-29'></a>

- [function] **GET-VALUE-B** *CAMS CAM NODE &OPTIONAL (VERIFY NIL) (IGNORE-CACHE NIL)*

    => int
    
    Return the current value of a Genicam boolean node. The parameter
    `node` is a string as defined by the Genicam standard. 
    
    Example:
    
    ```common-lisp
    (pylon:get-value-b *cams* 0 "AcquisitionFrameRateEnable")
    ```


<a name='x-28PYLON-3AGET-VALUE-I-20FUNCTION-29'></a>

- [function] **GET-VALUE-I** *CAMS CAM NODE &OPTIONAL (VERIFY NIL) (IGNORE-CACHE NIL)*

    => int
    
    Return the current value of a Genicam integer node. The parameter
    `node` is a string as defined by the Genicam standard. The return
    value is an integer.
    
    Example: 
    
    ```common-lisp
    (pylon:get-inc-i *cams* 0 "Width")
    ```


<a name='x-28PYLON-3ASET-VALUE-I-20FUNCTION-29'></a>

- [function] **SET-VALUE-I** *CAMS CAM NODE VALUE*

    Set the value of a Genicam integer node.

<a name='x-28PYLON-3AGET-SYMBOLICS-E-20FUNCTION-29'></a>

- [function] **GET-SYMBOLICS-E** *CAMS CAM NODE*

    Prints all the possible strings of a Genicam Enumeration node to
    stdout.  The parameter `node` is a string (e.g. "TriggerMode",
    "PixelFormat").

<a name='x-28PYLON-3ASET-VALUE-E-20FUNCTION-29'></a>

- [function] **SET-VALUE-E** *CAMS CAM NODE VALUE*

    Set a Genicam Enumeration node by using an integer identifier.

<a name='x-28PYLON-3AGET-VALUE-E-20FUNCTION-29'></a>

- [function] **GET-VALUE-E** *CAMS CAM NODE*

    => int
    
    Get a Genicam Enumeration node by using an integer identifier.

<a name='x-28PYLON-3ATO-STRING-E-20FUNCTION-29'></a>

- [function] **TO-STRING-E** *CAMS CAM NODE*

<a name='x-28PYLON-3AFROM-STRING-E-20FUNCTION-29'></a>

- [function] **FROM-STRING-E** *CAMS CAM NODE VALUE*

<a name='x-28PYLON-3ASTOP-GRABBING-20FUNCTION-29'></a>

- [function] **STOP-GRABBING** *CAMS*

<a name='x-28PYLON-3ACAMS-OPEN-20FUNCTION-29'></a>

- [function] **CAMS-OPEN** *CAMS*

    Issue Open command on all the cameras in the CInstantCameraArray hande.

<a name='x-28PYLON-3ACAMS-CLOSE-20FUNCTION-29'></a>

- [function] **CAMS-CLOSE** *CAMS*

    Issue Close command on all the cameras in the CInstantCameraArray hande.

<a name='x-28PYLON-3ACAM-OPEN-20FUNCTION-29'></a>

- [function] **CAM-OPEN** *CAMS CAM*

    Issue the Open command only for the camera with index `cam` in the
    CInstantCameraArray `handle`. If `cam` is bigger than the number of
    available cameras, nothing is done.

<a name='x-28PYLON-3ACAM-CLOSE-20FUNCTION-29'></a>

- [function] **CAM-CLOSE** *CAMS CAM*

    Issue the Close command only for the camera with index `cam` in the
    CInstantCameraArray `handle`. If `cam` is bigger than the number of
    available cameras, nothing is done.

<a name='x-28PYLON-3ACAM-GET-SERIAL-NUMBER-20FUNCTION-29'></a>

- [function] **CAM-GET-SERIAL-NUMBER** *CAMS CAM*

    Returns the uniq serial number of a camera as a string,
    e.g. `21433566`.

<a name='x-28PYLON-3ACAM-GET-FULL-NAME-20FUNCTION-29'></a>

- [function] **CAM-GET-FULL-NAME** *CAMS CAM*

    Returns the full name of the camera as a string, e.g. `Basler
    acA1920-25gm#00305315DFDE#192.168.5.102:3956`

<a name='x-28PYLON-3A-40PYLON-RETURN-20MGL-PAX-3ASECTION-29'></a>

## 1 Return values and errors

Pylon uses C++ exceptions to notify the user of its error
conditions. Each C wrapper function catches the exceptions and prints
the error on stdout. In Emacs these notifications are shown in the
`*inferior-lisp*` buffer. In case of an error functions that should
return a pointer will return `NULL`. Functions that should return
`int` will return -1.

Here is an example of the *inferior-lisp* buffer in SBCL:

```
(progn (load "/home/martin/quicklisp/dists/quicklisp/software/slime-2.7/swank-loader.lisp" :verbose t) 
       (funcall (read-from-string "swank-loader:init")) (funcall (read-from-string "swank:start-server") "/tmp/slime.28966"))

This is SBCL 1.2.0.129-c06b8c1, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
*
; loading #P"/home/martin/quicklisp/dists/quicklisp/software/slime-2.7/swank-loader.lisp"
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
```


<a name='x-28PYLON-3A-40PYLON-EXAMPLE-20MGL-PAX-3ASECTION-29'></a>

## 2 Example usage

The following code will open three cameras, reset their frame
  timers, set them to external trigger and acquire 100 images (overall
  300 calls to grab-sf). The triggers are generated by the function
  TRIGGER-ALL-CAMERAS and isn't documented here. In my code I instruct
  the femtolisp running on an Arduino Due to create the trigger
  pulses with the correct timing.

[`FACTORY`][74c0] returns the instance of the transport level factory. It is
  quite an internal construct to the Pylon C++ library and not useful
  in this Common Lisp wrapper except that it is necessary for the call
  to [`CREATE`][98d0].

([`CREATE`][98d0] `FUNCTION`) tries to open at most `MAX-CAMERAS` and returns an
  opaque pointer handle to an array of these cameras. Some other
  functions of this wrapper can be given this
  handle (e.g. `COMMAND-EXECUTE`, [`CAM-OPEN`][09b8], [`CAM-CLOSE`][d446], [`START-GRABBING`][1ba2],
  [`TERMINATE`][9d0b], [`GRAB-SF`][8481], [`GET-VALUE-I`][d41b], [`CAM-GET-SERIAL-NUMBER`][db43],
  [`CAM-GET-FULL-NAME`][b63e]) in this case the function parameter is called
  `CAMS`. Most of these functions also have a following integer
  parameter `CAM` which indicates which camera of the array is to be
  accessed.

```common-lisp
(defparameter *fact* (pylon::factory) "Handle to Factory, which is needed for call to PYLON:CREATE.")
(defparameter *cams* (pylon:create *fact* 3) "Handle to multiple Pylon cameras.")
(dotimes (i 3)
  (pylon:set-value-e *cams* i "TriggerMode" 1))
(let* ((old 0)
       (buf-s (make-array (list 1024 1024) :element-type 'single-float)))
  (unwind-protect 
       (progn
         (dotimes (i 3) ;; reset frame timers on the cameras ;
           (pylon::command-execute *cams* i "GevTimestampControlReset"))
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
                                     (format t "acquisition error.~%"))))))
                    :name "camera-acquisition")))
           (sleep .001)
           (dotimes (i 100)
             (trigger-all-cameras))
           (sb-thread:join-thread th)))
    (progn
      (pylon:stop-grabbing *cams*)
      (dotimes (i 3)
        (pylon:set-value-e *cams* i "TriggerMode" 0)))))
```

Note that repeated reruns of [`CREATE`][98d0] (after [`TERMINATE`][9d0b]) will
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
       (loop for e in '("BinningHorizontal" "BinningVertical" 
			"Width" "Height"
			"OffsetX" "OffsetY" 
			"ExposureTimeRaw" "GainRaw" 
			"GevTimestampTickFrequency" "GevSCPSPacketSize") collect
			(pylon:get-value-i *cams* j e t nil)
			)
       (list :trigger-mode (pylon:get-value-e *cams* j "TriggerMode")
	     :last-error (pylon:get-value-e *cams* j "LastError")
	     :rate-p (pylon:get-value-b *cams* j "AcquisitionFrameRateEnable")
	     :reverse-x (pylon:get-value-b *cams* j "ReverseX")
	     :rate (pylon:get-value-f *cams* j "ResultingFrameRateAbs")
	     :temp (pylon:get-value-f *cams* j "TemperatureAbs"))))
```

I then store the camera specific parameters in a hash table that is indexed by the cameras
serial number.

```common-lisp
(defparameter *cam-parameters*
  `((21433565    1    1  280   280 nil        777  337 230  82  90  0   70 "transmission with polrot (top)")
    (21433566    1    1  512   512 nil        789  112 482 429  90  0 4200 "backreflection with polrot")  
    (21433540    1    1  280   280 t          985  427 206 213  90  0   70 "transmission same pol"))
  "    id      binx  biny  w    h  reverse-x   x    y  kx  ky   d   g   e   name")

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
	(let ((inc (pylon:get-inc-i *cams* i "ExposureTimeRaw")))
	  (pylon:set-value-i *cams* i "ExposureTimeRaw" (* inc (floor e inc))))
       (list e (pylon:get-value-i *cams* i "ExposureTimeRaw"))))
```


  [09b8]: #x-28PYLON-3ACAM-OPEN-20FUNCTION-29 "(PYLON:CAM-OPEN FUNCTION)"
  [1ba2]: #x-28PYLON-3ASTART-GRABBING-20FUNCTION-29 "(PYLON:START-GRABBING FUNCTION)"
  [4caf]: #x-28PYLON-3AGRAB-CDF-20FUNCTION-29 "(PYLON:GRAB-CDF FUNCTION)"
  [6a88]: #x-28PYLON-3A-40PYLON-RETURN-20MGL-PAX-3ASECTION-29 "(PYLON:@PYLON-RETURN MGL-PAX:SECTION)"
  [74c0]: #x-28PYLON-3AFACTORY-20FUNCTION-29 "(PYLON:FACTORY FUNCTION)"
  [8481]: #x-28PYLON-3AGRAB-SF-20FUNCTION-29 "(PYLON:GRAB-SF FUNCTION)"
  [98d0]: #x-28PYLON-3ACREATE-20FUNCTION-29 "(PYLON:CREATE FUNCTION)"
  [9d0b]: #x-28PYLON-3ATERMINATE-20FUNCTION-29 "(PYLON:TERMINATE FUNCTION)"
  [b63e]: #x-28PYLON-3ACAM-GET-FULL-NAME-20FUNCTION-29 "(PYLON:CAM-GET-FULL-NAME FUNCTION)"
  [d41b]: #x-28PYLON-3AGET-VALUE-I-20FUNCTION-29 "(PYLON:GET-VALUE-I FUNCTION)"
  [d446]: #x-28PYLON-3ACAM-CLOSE-20FUNCTION-29 "(PYLON:CAM-CLOSE FUNCTION)"
  [db43]: #x-28PYLON-3ACAM-GET-SERIAL-NUMBER-20FUNCTION-29 "(PYLON:CAM-GET-SERIAL-NUMBER FUNCTION)"
  [f0b3]: #x-28PYLON-3A-40PYLON-EXAMPLE-20MGL-PAX-3ASECTION-29 "(PYLON:@PYLON-EXAMPLE MGL-PAX:SECTION)"
