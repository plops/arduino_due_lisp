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

(defun camera-new (&key (name nil))
  (if name
      (cffi:with-foreign-string (s name)
				(#_arv_camera_new s))
    (#_arv_camera_new (cffi:null-pointer))))

#+nil
(defparameter *fake* (camera-new "Fake"))
#+nil
(defparameter *cam1* (camera-new))



#+nil
(defparameter *cam2* (camera-new :name "Basler-21211553"))

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


#+nil
(defun interface-get-n-devices ()
  (#_arv_interface_get_n_devices ))

(defun camera-get-genicam-xml (cam)
  (rletz ((size :size_t))
	 (let* ((str-pointer (#_arv_device_get_genicam_xml (#_arv_camera_get_device cam) size))
		(n (pref size :size_t))
		(s (make-string n)))
	   (dotimes (i n)
	     (setf (elt s i) (code-char (ccl:%get-unsigned-byte str-pointer i))))
	   (values s str-pointer n))))

(defun gc-new (cam)
 (multiple-value-bind (xml xmlp n) (camera-get-genicam-xml cam)
   (declare (ignorable xml))
   (#_arv_gc_new (#_arv_camera_get_device cam) xmlp n)))

(defparameter *gc1* (gc-new *cam1*))
(defparameter *gc2* (gc-new *cam2*))

(defun gc-get-node (genicam str)
  (cffi:with-foreign-string (s str)
			    (#_arv_gc_get_node genicam s)))

(defparameter *temp-node2*  (gc-get-node *gc1* "TemperatureAbs"))

(#_arv_gc_enumeration_get_int_value 
 (gc-get-node *gc2* "TemperatureSelector")
 (cffi:null-pointer))


(defparameter *al* )

(defun basler-temperatures ()
  (loop for (i name) in '((0 sensor)
			  (1 core)
			  (2 framegrabber)
			  (3 case))
	collect 
	(progn
	  (#_arv_gc_enumeration_set_int_value 
	   (gc-get-node *gc2* "TemperatureSelector")
	   i (cffi:null-pointer))
	  (list name
		(#_arv_gc_float_get_value (gc-get-node *gc2* "TemperatureAbs") 
				    (cffi:null-pointer))))))


#+nil
(basler-temperatures)

(defun photonfocus-temperatures ()
  (loop for (i name) in '((0 sensor)
			 (1 sensor-board)
			 (2 adc-board))
       collect 
       (progn
	 (#_arv_gc_integer_set_value
	  (gc-get-node *gc1* "DeviceTemperatureSelectorVal")
	  i (cffi:null-pointer))
	 (#_arv_gc_command_execute
	  (gc-get-node *gc1* "DeviceTemperature_Update")
	  (cffi:null-pointer))
	 (list name
	  (#_arv_gc_float_get_value (gc-get-node *gc1* "DeviceTemperature") 
				    (cffi:null-pointer))))))

 

#+nil
(photonfocus-temperatures)



(#_arv_gc_float_get_value *temp-node* (cffi:null-pointer))
(defun store-genicam-xml (fn cam)
  (with-open-file (s fn :direction :output :if-exists :supersede :if-does-not-exist :create)
		  (write-sequence (camera-get-genicam-xml cam) s)))

#+nil
(store-genicam-xml "/dev/shm/cam2.xml" *cam2*)

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
