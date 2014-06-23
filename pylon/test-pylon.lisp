(load "~/quicklisp/setup.lisp")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry* '(*default-pathname-defaults*
                                  #p"/home/martin/arduino_due_lisp/pylon/"))
  (asdf:load-system "pylon"))
