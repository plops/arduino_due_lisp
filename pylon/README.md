# pylon Common Lisp binding

## introduction

I ran into problems when I tried to read out the three Gig-E cameras
simultaneously using the free software aravis. 

The cameras (Basler) come with a proprietary development kit Pylon
that contains a rather primitive Viewer application. This application
seems to work well with all three cameras but doesn't allow to store
the images.

I decided to call the Pylon SDK from Common Lisp. Unfortunately, Pylon
only comes with a C++ interface and I had to create a few C wrapper
functions first. I ran into trouble with Lisp again because I forgot
to open the cameras with cameras->Open(). This call seems to be
unnecessary in C but is crucial when calling from Lisp. The
documentation is not as detailed as I would have hoped.

Before finding this solution I have already started to call the C++
library directly from cling (a LLVM based C++ REPL). Using this
command line interface it is quite easy to test the
library. Unfortunately, Cling is not very well integrated with emacs
and I didn't learn enough about it to understand how to do
introspection. Therefore, I still prefer Common Lisp.

## usage:

``` initialize ```

Initializes the pylon runtime system. Call this before any other pylon
function. Don't forget to call `terminate` before you close the lisp
session. Otherwise cameras may need to be power cycled.

``` factory => factory ```

Return a transport level factory. Its return value is treated as an
opaque pointer and only necessary as the first argument of `create`.


``` create factory n => handle ```

Given a transport level factory open `n` cameras. Error messages and
the full names of the cameras are printed on stdout.  Returns an
opaque pointer to a CInstantCameraArray which needs to be given as a
handle to all other functions that access the cameras in some way.

``` terminate handle ```

Deletes the opaque pointer to the CInstantCameraArray. Also terminates
the Pylon runtime and should close the cameras properly, so that they
can be opened by another Gig-e client program.


```
cams-open  handle 
cams-close handle
```

Issue the Open or Close command on all the cameras in the
CInstantCameraArray `handle`.


```
cam-open  handle cam 
cam-close handle cam
```

Issue the Open or Close command only for the camera with index `cam`
in the CInstantCameraArray `handle`. If `cam` is bigger than the
number of available cameras, nothing is done.


```
cam-get-full-name     handle cam => string
```

Returns the full name of the camera as a string, e.g. "Basler
acA1920-25gm#00305315DFDE#192.168.5.102:3956"


```
cam-get-serial-number handle cam => string
```

Returns the uniq serial number of a camera as a string,
e.g. "21433566".


``` 
get-max-i   handle cam node => int
get-min-i   handle cam node => int
get-inc-i   handle cam node => int
get-value-i handle cam node &optional verify ignore-cache  => int
```

Return the maximum, minimum, increment or current value of a Genicam
integer node. The parameter `node` is a string as defined by the
Genicam standard. The return value is an integer.

example: 
```common-lisp
(pylon:get-max-i *cams* 0 "Width")
(loop for e in '("Width" "Height" "OffsetX" "OffsetY") collect
  (pylon:get-value-i *cams* 2 e t nil))
```

```
get-symbolics-e handle cam node
```

Prints all the possible strings of a Genicam Enumeration node to
stdout.  The parameter `node` is a string (e.g. "TriggerMode",
"PixelFormat").

```
get-value-e     handle cam node  => int
set-value-e     handle cam node value
```

Get or set an Genicam Enumeration node by using an integer identifier.


```
to-string-e     handle cam node => int
from-string-e   handle cam node value
```

`to-string-e` reads the current value of a Genicam Enumeration node
and prints the string representation to stdout. It returns the
corresponding integer identifier.

`from-string-e` sets the Genicam Enumeration node to the string
`value`, e.g. the node "PixelFormat" to value "Mono8".

```
command-execute handle cam node => int
command-isdone  handle cam node => int
```

Execute a command on the device. E.g.:
```(pylon::command-execute *cams* 1 "ClearLastError")```
```
start-grabbing handle
stop-grabbing  handle
```

Start acquisition on all cameras of the handle.


```
grab     cams w h buf => (values cam success-p w h frame-nr)
grab-cdf cams w h buf => (values cam success-p w h frame-nr)
```

`grab` copies one acquired image into an array `buf` of (unsigned-byte
8). The length of the array buf must be at least `w*h`. The image data
originates from one of the cameras in the handle `cams` as indicated
by the camera index `cam` of the return values. The returned values
`w` and `h` indicate the dimensions of the returned image data in
`buf`.

In case of an error, all four return values are -1.

`grab-cdf` converts the data into (complex double-float) to simplify
further processing with fftw.


```
grab-store cams fds => (values cam success-p w h frame-nr)
```

Acquire one image and store in one of the filedescriptors in the list
`fds`. 

example use with external trigger:

```common-lisp
(defparameter *cams* (pylon:create *fact* 3)) ;; open 3 cameras

(pylon:cams-open *cams*)

(let ((fds nil))
  (unwind-protect 
       (progn 
	 (setf fds
	       (loop for i below 3 collect
		    (sb-unix::unix-open (format nil "/dev/shm/raw~a.raw" i) 
					(logior sb-unix:o_creat sb-unix:o_trunc sb-unix:o_wronly) 
					#o666)))
	 (pylon:start-grabbing *cams*)
	 (dotimes (j 100)
	   ;; start a thread that waits for the 3 images 
	   (let ((th (sb-thread:make-thread 
		      #'(lambda ()
			  (loop for i below 3 do
			       (destructuring-bind (cam success-p w h framenr) 
				   (multiple-value-list (pylon::grab-store *cams* fds))
				 (unless (= 1 success-p)
				   (format t "acquisition error. ~a~%" success-p)))))
		      :name "camera-acquisition")))
	     (sleep .01)
	     ;; send trigger signal to the cameras
	     (trigger-all-cameras)
             (sleep .01)
	     ;; wait for the data to arrive
	     (sb-thread:join-thread th))))
    (progn (pylon:stop-grabbing *cams*)
	   (loop for e in fds do
		(sb-unix::unix-close e)))))
```




##  return values and errors:

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