(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry* '(*default-pathname-defaults*
				  #p"/home/martin/stage/cl-cffi-fftw3/"
				  #p"/home/martin/arduino_due_lisp/"))
  (asdf:load-system "fftw"))

(asdf:load-system "arv")

(fftw:prepare-threads)

(defpackage :test-arv
  (:use :cl :arv))

(in-package :test-arv)

(defparameter *cam1* (make-instance 'arv:camera :name "Basler-21433540"))
(defparameter *cam2* (make-instance 'arv:camera :name "Basler-21433565"))
(defparameter *cam3* (make-instance 'arv:camera :name "Basler-21433566"))


(progn
 (loop for c in (list *cam1* *cam2* *cam3*) do
      (set-acquisition-mode c 'single-frame)
      (set-pixel-format c "Mono12Packed")
      )
 (set-region *cam2* :keep-old nil :h 1024 :w 1024 :x 452 :y 21)
 (set-region *cam1* :keep-old nil :h 1024 :w 1024 :x 135 :y 0)
 (set-region *cam3* :keep-old nil :h 600 :w 600 :x 520 :y 213))
