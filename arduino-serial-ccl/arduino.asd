(asdf:defsystem arduino
  :version "0"
  :description "Library to communicate with Arduino over serial connection from Clozure Common Lisp."
  :maintainer "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :author "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :licence "GPL"
  :depends-on (:cffi)
  :components ((:file "arduino")))
