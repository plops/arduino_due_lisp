
(asdf:defsystem arv
  :version "0"
  :description "Aravis (Gig-E vision library) bindings for Clozure Common Lisp."
  :maintainer "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :author "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :licence "GPL"
  :depends-on (:cffi)
  :components ((:file "package")
               (:file "libraries" :depends-on ("package"))
               (:file "wrapper" :depends-on ("package"))
	       (:file "wrapper-gc" :depends-on ("package" "wrapper"))
	       (:file "wrapper-temperatures" :depends-on ("package" "wrapper-gc"))
	       (:file "wrapper-acquisition" :depends-on ("package" "wrapper-gc"))
	       (:file "helper" :depends-on ("package"))))
