(push :generate-pylon-doc cl:*features*)
(asdf:defsystem pylon
  :version "0"
  :description "Pylon (Gig-E vision library by Basler) bindings for Common Lisp."
  :maintainer "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :author "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :licence "GPL"
  :depends-on (:cffi #+generate-pylon-doc :mgl-pax)
  :components ((:file "package")
               (:file "libraries" :depends-on ("package"))
               (:file "bindings" :depends-on ("package" "libraries"))
	       (:file "wrapper" :depends-on ("package" "libraries" "bindings"))))
