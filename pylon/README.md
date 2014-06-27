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
opaque pointer and only necessary as the first argument of `create`
and the second of `terminate`.


``` create factory n => handle ```

Given a transport level factory open `n` cameras. Error messages and
the full names of the cameras are printed on stdout.  Returns an
opaque pointer to a CInstantCameraArray which needs to be given as a
handle to all other functions that access the cameras in some way.

``` terminate handle factory ```

Deletes the opaque pointers to the CInstantCameraArray and the
transport level factory. Also terminates the Pylon runtime and should
close the cameras properly, so that they can be opened by another
Gig-e client program.


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
start-grabbing handle
stop-grabbing  handle
```

Start acquisition on all cameras of the handle.


```
grab     cams w h buf => (values cam success-p w h)
grab-cdf cams w h buf => (values cam success-p w h)
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



##  return values and errors:

Pylon uses C++ exceptions to notify the user of its error
conditions. Each C wrapper function catches the exceptions and prints
the error on stdout. In Clozure Common Lisp these notifications are
visible in emacs' `*inferior-lisp*` buffer. In SBCL they are not shown
in Emacs. In case of an error functions that should return a pointer
will return `NULL`. Functions that should return `int` will return -1.