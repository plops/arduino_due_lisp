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
#+nil
(#_arv_get_n_interfaces)

#+nil
(defparameter *fake*
  (cffi:with-foreign-string (s "Fake")
			    (#_arv_camera_new s)))

(defparameter *cam1* (#_arv_camera_new (cffi:null-pointer)))


;; apparently, i can download the xml files from the cameras using
;; this:
;;arv_device_get_genicam_xml


(defun get-interface-ids ()
  (let ((n (#_arv_get_n_interfaces)))
    (loop for i below n collect
	  (let* ((str-pointer (#_arv_get_interface_id i))
		 (name (loop for j from 0 and c = (ccl:%get-unsigned-byte str-pointer j) until (= c 0)
			     collect (code-char c)))) ;; first char is collected twice
	    (make-array (1- (length name)) :element-type 'character :initial-contents (rest name))))))
#+nil
(get-interface-ids)



(defun interface-get-n-devices ()
  (#_arv_interface_get_n_devices ))

(defun camera-get-genicam-xml (cam)
  (rletz ((size :size_t))
	 (let* ((str-pointer (#_arv_device_get_genicam_xml (#_arv_camera_get_device cam) size))
		(n (pref size :size_t))
		(s (make-string n)))
	   (dotimes (i n)
	     (setf (elt s i) (code-char (ccl:%get-unsigned-byte str-pointer i))))
	   s)))

#+nil
(with-open-file 
 (s "/dev/shm/cam1.xml" :direction :output
    :if-exists :supersede :if-does-not-exist :create)
 (defparameter *xml1* (camera-get-genicam-xml *cam1*))
 (write-sequence *xml1* s))

#+nil
(defparameter *fake* (cffi:with-foreign-string (s "bla")
   (#_arv_fake_camera_new s)))
#+nil
(#_arv_fake_camera_ *fake*)
#+nil
(#_arv_fake_camera_get_acquisition_status *fake*)
#+nil
(defun wait-and-get-image (cam)
 (let* ((n (#_arv_fake_camera_get_payload cam))
	(buf (#_arv_buffer_new n (cffi:null-pointer))))
   (#_arv_fake_camera_wait_for_next_frame cam)
   (#_arv_fake_camera_fill_buffer cam buf (cffi:null-pointer))
   (destructuring-bind (h w) (fake-camera-get-size cam)
     (assert (= n (* h w)))
     (let* ((a (make-array (list h w) :element-type 'unsigned-byte))
	    (a1 (make-array n :element-type 'unsigned-byte
			    :displaced-to a)))
       (dotimes (i n)
	 (setf (aref a1 i)
	       (cffi:mem-aref (pref buf :<A>rv<B>uffer.data) :uchar i)))
       a))))
#+nil
(defparameter *img* (wait-and-get-image *fake*))

#+nil
(defun fake-camera-get-size (cam)
  "returns list (height width)"
  (list
   (cffi:with-foreign-object (val :uint32)
     (#_arv_fake_camera_read_register cam #$ARV_FAKE_CAMERA_REGISTER_HEIGHT val)
     (cffi:mem-ref val :uint32))
   (cffi:with-foreign-object (val :uint32)
     (#_arv_fake_camera_read_register cam #$ARV_FAKE_CAMERA_REGISTER_WIDTH val)
     (cffi:mem-ref val :uint32))
   ))



(defun get-sensor-size (cam)
 (cffi:with-foreign-objects ((w :int)
			     (h :int))
   (#_arv_camera_get_sensor_size cam w h)
   (list  (cffi:mem-ref w :int)
	  (cffi:mem-ref h :int))))

#+nil
(get-sensor-size *cam1*)

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
(defparameter *cam* (#_arv_camera_new (cffi:null-pointer)))


#+nil
(cffi:defcfun (%arv-camera-get-available-pixel-formats "arv_camera_get_available_pixel_formats")
    #_gint64)
