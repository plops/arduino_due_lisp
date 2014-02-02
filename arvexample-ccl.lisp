(load "/home/martin/quicklisp/setup.lisp")
(ql:quickload :cffi)

#+nil 
(progn ;; this only needs to be called once, to run h-to-ffi.sh
       ;; (ffigen) and generate the cdb foreign function database
       ;; files
  (require :parse-ffi)
  (ccl::parse-standard-ffi-files :arv)
  (ccl::parse-standard-ffi-files :v4l2))

(ccl:use-interface-dir :arv)
(cffi:load-foreign-library "libaravis-0.4.so")
;ccl::*shared-libraries*
;ccl::*eeps* ;; hash table with external functions

(cffi:with-foreign-string (s "Fake")
  (#_arv_enable_interface s))

(#_)


(#_arv_g_type_init)
(defparameter *cam* (#_arv_camera_new (cffi:null-pointer)))

(cffi:defcfun (%arv-camera-get-available-pixel-formats "arv_camera_get_available_pixel_formats")
    #_gint64)
