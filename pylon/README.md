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

``` factory => factory ```

``` create factory n => handle ```

``` terminate handle factory ```


```
cams-open handle 
cams-close 
```

```
cam-open handle cam 
cam-close 
```



``` 
 get-max-i   handle cam node 
 get-min-i   handle cam node 
 get-inc-i   handle cam node 
 get-value-i handle cam  
```

example: 
```common-lisp
(pylon:get-max-i *cams* 0 "Width")
(loop for e in '("Width" "Height" "OffsetX" "OffsetY") collect
  (pylon:get-value-i *cams* 2 e t nil))
```

```
 get-symbolics-e
 get-value-e
 to-string-e
 set-value-e
 from-string-e
```

```
start-grabbing
stop-grabbing
```

```
grab
grab-cdf
```

