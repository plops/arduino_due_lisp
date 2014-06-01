(in-package :arv)

(defun load-arv-libraries ()
  (cffi:load-foreign-library "libaravis-0.4.so")
  ;; before any other function we have to call g_type_init from
  ;; /usr/include/glib-2.0/gobject/gtype.h
  (#_g_type_init))




