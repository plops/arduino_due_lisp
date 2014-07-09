(defpackage :image-processing
  (:use :cl)
  (:export
   #:.linear
   #:.max
   #:.min
   #:.abs
   #:.abs*
   #:.log
   #:.uint8
   #:.uint16
   #:next-power-of-two
   #:extract
   #:extract-cdf*
   #:.rr
   #:.*
   #:.+
   #:.phiphi
   #:.mean
   #:write-pgm8
   #:write-pgm16
   #:.realpart
   #:.accum))

