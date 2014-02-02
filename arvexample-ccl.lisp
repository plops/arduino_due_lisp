;; run C-c C-k to load this file into slime
#.(load "/home/martin/quicklisp/setup.lisp")
(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cffi))

#+nil 
(progn ;; this only needs to be called once, to run h-to-ffi.sh
       ;; (ffigen) and generate the cdb foreign function database
       ;; files
  (require :parse-ffi)
  (ccl::parse-standard-ffi-files :arv)
  (ccl::parse-standard-ffi-files :v4l2))

#.(ccl:use-interface-dir :arv)
(cffi:load-foreign-library "libaravis-0.4.so")
;ccl::*shared-libraries*
;ccl::*eeps* ;; hash table with external functions

;; before any other function we have to call g_type_init from
;; /usr/include/glib-2.0/gobject/gtype.h
(#_g_type_init)
#+nil
(cffi:with-foreign-string (s "Fake")
  (#_arv_enable_interface s))

#+nil
(#_arv_get_n_devices)

(defparameter *fake* (cffi:with-foreign-string (s "bla")
   (#_arv_fake_camera_new s)))

(#_arv_fake_camera_get_acquisition_status *fake*)

(cffi:with-foreign-object (buf :uchar :size (#_arv_fake_camera_get_payload *fake*))
  (#_arv_fake_camera_wait_for_next_frame *fake*)
  (#_arv_fake_))

(#_arv_fake_camera_get_type *fake*)

(defun get-sensor-size (cam)
 (cffi:with-foreign-objects ((w :int)
			     (h :int))
   (#_arv_camera_get_sensor_size cam w h)
   (list  (cffi:mem-ref w :int)
	  (cffi:mem-ref h :int))))

(defun set-region (cam)
 (cffi:with-foreign-objects ((x :int)
			     (y :int)
			     (w :int)
			     (h :int))
   (setf (cffi:mem-ref x :int) 0
	 (cffi:mem-ref y :int) 0
	 (cffi:mem-ref w :int) 512
	 (cffi:mem-ref h :int) 512)
   (#_arv_camera_set_region cam x y w h)))

#+nil
(defparameter *cam* (#_arv_camera_new (cffi:null-pointer))))

#+nil
(cffi:defcfun (%arv-camera-get-available-pixel-formats "arv_camera_get_available_pixel_formats")
    #_gint64)
