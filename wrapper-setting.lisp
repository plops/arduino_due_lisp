(in-package :arv)
(defmethod set-region ((cam camera) &key (x 0) (y 0)
				      (w (- (sensor-width cam) x))
				      (h (- (sensor-height cam) y))
				      (keep-old t))
  (assert (<= 0 x (1- (sensor-width cam))))
  (assert (<= 0 y (1- (sensor-height cam))))
  (assert (<= 0 (+ x w) (sensor-width cam)))
  (assert (<= 0 (+ y h) (sensor-height cam)))
  (multiple-value-bind (ox oy ow oh) (get-region cam)
    (when (or keep-old (and (= ox x) (= oy y) (= ow w) (= oh h)))
      (return-from set-region (get-region cam))))
  (gc-integer-set-value cam "Width" w)
  (gc-integer-set-value cam "Height" h)
  (gc-integer-set-value cam "OffsetX" x)
  (gc-integer-set-value cam "OffsetY" y)
  (get-region cam)
  (ensure-at-least-one-buffer-in-stream cam)
  (with-slots (arv-stream) cam
    (destroy-stream cam)
    (setf arv-stream (create-stream cam))
    (push-buffer cam)))


(defun next-dividable-by-4 (n)
  (* 4 (ceiling n 4)))

(defmethod set-packet-size ((cam camera) mtu)
  ;; ensure packet size to be multiple of 32bit
  (let ((mtu4 (next-dividable-by-4 mtu)))
    (unless (= mtu4 (gc-integer-get-value cam "GevSCPSPacketSize"))
      (gc-integer-set-value cam "GevSCPSPacketSize" mtu4)))
  (gc-integer-get-value cam "GevSCPSPacketSize"))

(defmethod get-packet-size ((cam camera))
  (gc-integer-get-value cam "GevSCPSPacketSize"))


(defmethod get-region ((cam camera))
  (let ((x (gc-integer-get-value cam "OffsetX"))
	(y (gc-integer-get-value cam "OffsetY"))
	(w (gc-integer-get-value cam "Width"))
	(h (gc-integer-get-value cam "Height")))
    (with-slots (aoi-width aoi-height aoi-x aoi-y) cam
	(setf aoi-width w
	      aoi-height h
	      aoi-x x
	      aoi-y y))
    (values x y w h)))

#+nil
(get-region *cam1*)

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
			  sensor-width sensor-height arv-gc
			  arv-model-name arv-vendor-name
			  arv-device-id arv-stream pixel-formats) cam
    (setf arv-camera (camera-new :name name)
	  arv-stream (create-stream cam)
	  arv-device (#_arv_camera_get_device arv-camera)
	  arv-vendor-name (char*-to-lisp (#_arv_camera_get_vendor_name arv-camera)) 
	  arv-model-name (char*-to-lisp (#_arv_camera_get_model_name arv-camera)) 
	  arv-device-id (char*-to-lisp (#_arv_camera_get_device_id arv-camera)))
    (assert (not (cffi:null-pointer-p arv-camera)))
    (loop while (cffi:null-pointer-p arv-stream) do
	 (sleep 1)
	 (setf arv-stream (create-stream cam)))
    (assert (not (cffi:null-pointer-p arv-stream)))
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
      (assert (not (cffi:null-pointer-p arv-gc))))
    
    (set-region cam)))



(defmethod get-payload ((cam camera))
  (#_arv_gc_integer_get_value 
   (gc-get-node cam "PayloadSize")
   (cffi:null-pointer)))

(defmethod set-exposure ((cam camera) time-us)
  (#_arv_gc_float_set_value (gc-get-node cam "ExposureTimeAbs") 
   time-us
   (cffi:null-pointer)))
;; photometrics 10us.. 0.41s 25ns steps

(defmethod get-exposure ((cam camera))
  (#_arv_gc_float_get_value (gc-get-node cam "ExposureTimeAbs") 
			    (cffi:null-pointer)))


(defmethod get-acquisition-mode ((cam camera))
  (acquisition-code-mode cam
			 (#_arv_gc_enumeration_get_int_value 
			  (gc-get-node cam "AcquisitionMode")
			  (cffi:null-pointer))))


(defmethod set-region-centered ((c camera) &key cx cy w h)
  (set-region c :x (- cx (floor w 2))
	      :y (- cy (floor h 2))
	      :w w
	      :h h))
