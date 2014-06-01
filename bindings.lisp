(in-package :arv)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :arv)) 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-arv-libraries))
