(asdf:defsystem image-processing
  :version "0"
  :description "Simple image processing algorithms for Common Lisp."
  :maintainer "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :author "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :licence "GPL"
  :components ((:file "package")
	       (:file "functions" :depends-on ("package"))))
