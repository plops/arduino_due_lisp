# Common Lisp image processing

## introduction

This library contains a few functions that are useful for image
processing.

## usage:

```
.linear multi-d-array => 1d array 
```

```
.max a => number
.min a => number
.mean a => number
 ```

Reduce an array.


```
.log    a
.uint8  a
.uint16 a
```

Apply a function on each element of an array, or convert its type.


```
extract a &key x y w h
```

Copy a subimage from an array.  By default it will copy the data from
the center and increases the width and height the next power of two.
Border wraps around as a discrete fourier transform.


```
.rr
.phiphi
```

Create an array, filled with radial or azimuthal function.


```
.write-pgm8  filename a
.write-pgm16 filename a
```

Store image data in gray level PGM file format.