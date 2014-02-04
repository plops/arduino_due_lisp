;; run C-c C-k to load this file into slime
#.(load "/home/martin/quicklisp/setup.lisp")
(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cffi))

(declaim (optimize (speed 0) (safety 3) (debug 3)))

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


(defun get-interface-ids ()
  (let ((n (#_arv_get_n_interfaces)))
    (loop for i below n collect
	  (let* ((str-pointer (#_arv_get_interface_id i))
		 (name (loop for j from 0 and c = (ccl:%get-unsigned-byte str-pointer j) until (= c 0)
			     collect (code-char c)))) ;; first char is collected twice
	    (make-array (1- (length name)) :element-type 'character :initial-contents (rest name))))))
#+nil
(get-interface-ids)

(defun camera-new (&key (name nil))
  (if name
      (cffi:with-foreign-string (s name)
	(#_arv_camera_new s))
      (#_arv_camera_new (cffi:null-pointer))))

(defclass camera ()
  ((name :reader camera-name :initarg :name :type string)
   (arv-camera :reader arv-camera)
   (arv-device :reader arv-device)
   (arv-gc :reader arv-gc)
   (arv-xml :reader arv-xml)
   (arv-xml-size :reader arv-xml-size :type fixnum)
   (sensor-width :reader sensor-width)
   (sensor-height :reader sensor-height)
   ))

#+nil
(defparameter *cam2*
 (make-instance 'camera :name "Basler-21211553"))

(defmethod camera-get-genicam-xml ((camera camera))
  (with-slots (arv-device) camera
   (rletz ((size :size_t))
     (let* ((str-pointer (#_arv_device_get_genicam_xml arv-device size))
	    (n (pref size :size_t))
	    (s (make-string n)))
       (assert (not (cffi:null-pointer-p str-pointer)))
       (dotimes (i n)
	 (setf (elt s i) (code-char (ccl:%get-unsigned-byte str-pointer i))))
       (values s str-pointer n)))))

(defmethod initialize-instance :after ((cam camera) &key)
  (with-slots (arv-camera arv-device name arv-xml arv-xml-size
			  sensor-width sensor-height arv-gc) cam
    (setf arv-camera (camera-new :name name)
	  arv-device (#_arv_camera_get_device arv-camera))
    (assert (not (cffi:null-pointer-p arv-camera)))
    (assert (not (cffi:null-pointer-p arv-device)))
    (cffi:with-foreign-objects ((w :int)
				(h :int))
      (#_arv_camera_get_sensor_size arv-camera w h)
      (setf sensor-width (cffi:mem-ref w :int)
	    sensor-height (cffi:mem-ref h :int)))
    (multiple-value-bind (sxml xml n) (camera-get-genicam-xml cam)
      (declare (ignorable sxml))
      (setf arv-xml xml
	    arv-xml-size n
	    arv-gc (#_arv_gc_new arv-device xml n))
      (assert (not (cffi:null-pointer-p arv-gc))))))

(defmethod gc-get-node ((cam camera) str)
  (cffi:with-foreign-string (s str)
    (#_arv_gc_get_node (arv-gc cam) s)))

(defmethod basler-temperatures ((cam camera))
  (loop for (i name) in '((0 sensor)
			  (1 core)
			  (2 framegrabber)
			  (3 case))
	collect 
	(progn
	  (#_arv_gc_enumeration_set_int_value 
	   (gc-get-node cam "TemperatureSelector") i (cffi:null-pointer))
	  (list name
		(#_arv_gc_float_get_value (gc-get-node cam "TemperatureAbs") 
					  (cffi:null-pointer))))))

(defmethod photonfocus-temperatures ((cam camera))
  (loop for (i name) in '((0 sensor)
			  (1 sensor-board)
			  (2 adc-board))
     collect 
       (progn
	 (#_arv_gc_integer_set_value
	  (gc-get-node cam "DeviceTemperatureSelectorVal")
	  i (cffi:null-pointer))
	 (#_arv_gc_command_execute
	  (gc-get-node cam "DeviceTemperature_Update") (cffi:null-pointer))
	 (list
	  name
	  (#_arv_gc_float_get_value (gc-get-node cam "DeviceTemperature") 
				    (cffi:null-pointer))))))

 

#+nil
(photonfocus-temperatures)

(defmethod set-region ((cam camera) &key (x 0) (y 0)
				      (w (- (sensor-width cam) x))
				      (h (- (sensor-height cam) y)))
  (assert (<= 0 x (1- (sensor-width cam))))
  (assert (<= 0 y (1- (sensor-height cam))))
  (assert (<= 0 (+ x w) (1- (sensor-width cam))))
  (assert (<= 0 (+ y h) (1- (sensor-height cam))))
  (cffi:with-foreign-objects ((fx :int)
			      (fy :int)
			      (fw :int)
			      (fh :int))
    (setf (cffi:mem-ref fx :int) x
	  (cffi:mem-ref fy :int) y
	  (cffi:mem-ref fw :int) w
	  (cffi:mem-ref fh :int) h)
    (#_arv_camera_set_region (arv-camera cam) fx fy fw fh)))
