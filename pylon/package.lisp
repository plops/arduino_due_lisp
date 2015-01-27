#+generate-pylon-doc
(mgl-pax:define-package #:pylon
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

  Connect will send a request to open a connection to the X-Server and
  parses its response to obtain the constants *RESOURCE-ID-BASE*,
  *RESOURCE-ID-MASK* and *ROOT*. These are stored in dynamic variables
  and are later used by other functions, e.g. by MAKE-WINDOW to create
  a new window."
  (initialize function)
  (terminate function)
  (create function)
  (factory function)
  (start-grabbing function)
  (grab function)
  (grab-cdf function)

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

  #+nil(@pylon-examples section))

