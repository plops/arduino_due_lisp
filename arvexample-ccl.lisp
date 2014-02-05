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


(defun char*-to-lisp (str-pointer &key (max-length 100))
  (let* ((name (loop for j from 0 below max-length
		  and c = (ccl:%get-unsigned-byte str-pointer j) until (= c 0)
		  collect (code-char c)))) ;; first char is collected twice
    (make-array (1- (length name)) :element-type 'character :initial-contents (rest name))))


(defun get-interface-ids ()
  (let ((n (#_arv_get_n_interfaces)))
    (loop for i below n collect
	 (let* ((str-pointer (#_arv_get_interface_id i)))
	   (char*-to-lisp str-pointer)))))
#+nil
(get-interface-ids)

(defun camera-new (&key (name nil))
  (if name
      (cffi:with-foreign-string (s name)
	(#_arv_camera_new s))
      (#_arv_camera_new (cffi:null-pointer))))



(defclass camera ()
  ((name :reader arv-camera-name :initarg :name :type (or null string) :initform nil)
   (arv-model-name :reader arv-model-name :type string)
   (arv-vendor-name :reader arv-vendor-name :type string)
   (arv-device-id :reader arv-device-id :type string)
   (arv-camera :reader arv-camera)
   (arv-device :reader arv-device)
   (arv-stream :reader arv-stream)
   (arv-gc :reader arv-gc)
   (arv-xml :reader arv-xml)
   (arv-xml-size :reader arv-xml-size :type fixnum)
   (sensor-width :reader sensor-width)
   (sensor-height :reader sensor-height)
   (aoi-x :accessor aoi-x :initform 0 :type fixnum)
   (aoi-y :accessor aoi-y :initform 0 :type fixnum)
   (aoi-width :accessor aoi-width :type fixnum)
   (aoi-height :accessor aoi-height :type fixnum)
   (pixel-formats :reader pixel-formats)
   ))

(defmethod set-region ((cam camera) &key (x 0) (y 0)
				      (w (- (sensor-width cam) x))
				      (h (- (sensor-height cam) y)))
  (assert (<= 0 x (1- (sensor-width cam))))
  (assert (<= 0 y (1- (sensor-height cam))))
  (assert (<= 0 (+ x w) (sensor-width cam)))
  (assert (<= 0 (+ y h) (sensor-height cam)))
  (gc-integer-set-value cam "Width" w)
  (gc-integer-set-value cam "Height" h)
  (gc-integer-set-value cam "OffsetX" x)
  (gc-integer-set-value cam "OffsetY" y)
  (get-region cam))

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
    `((x . ,x)
      (y . ,y)
      (width . ,w)
      (height . ,h))))

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



(defmethod create-stream ((cam camera))
  (#_arv_camera_create_stream (arv-camera cam) (cffi:null-pointer)
			      (cffi:null-pointer)))

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
    (set-region cam)
    (setf pixel-formats  (gc-enumeration-get-available-string-values cam "PixelFormat"))))

(defmethod gc-get-node ((cam camera) str)
  (declare (type string str))
  (cffi:with-foreign-string (s str)
    (#_arv_gc_get_node (arv-gc cam) s)))

(defmethod gc-enumeration-set-int-value ((cam camera) name val)
  (#_arv_gc_enumeration_set_int_value (gc-get-node cam name) val (cffi:null-pointer)))

(defmethod gc-integer-set-value ((cam camera) name val)
  (#_arv_gc_integer_set_value (gc-get-node cam name) val (cffi:null-pointer)))

(defmethod gc-integer-get-value ((cam camera) name)
  (#_arv_gc_integer_get_value (gc-get-node cam name) (cffi:null-pointer)))

(defmethod gc-float-get-value ((cam camera) name)
  (#_arv_gc_float_get_value (gc-get-node cam name) (cffi:null-pointer)))

(defmethod gc-command-execute ((cam camera) name)
  (#_arv_gc_command_execute (gc-get-node cam name) (cffi:null-pointer)))


(defmethod gc-enumeration-get-available-string-values ((cam camera) name)
  (cffi:with-foreign-object (n-values :unsigned-int)
    (let* ((c-strs (#_arv_gc_enumeration_get_available_string_values (gc-get-node cam name) n-values (cffi:null-pointer)))
	   (n (cffi:mem-ref n-values :unsigned-int)))
      (loop for i below n collect
	   (let ((c-str (cffi:mem-aref c-strs :pointer i)))
	     (prog1 
		 (char*-to-lisp c-str)
	       (#_g_free c-str)))))))

#+nil
(gc-enumeration-get-available-string-values *cam1* "PixelFormat")

(defmethod gc-enumeration-set-string-value ((cam camera) name val)
  (declare (type string val))
  (cffi:with-foreign-string (s val)
    (#_arv_gc_enumeration_set_string_value (gc-get-node cam name) s (cffi:null-pointer)))) 

(defmethod gc-enumeration-get-string-value ((cam camera) name)
  ;; call arv_gc_feature_node_get_name which returns node->priv->name
  ;; i believe i must not call g_free on this string. perhaps there is
  ;; a bug in aravis, because it's not returning the expected values
  (char*-to-lisp
   (#_arv_gc_enumeration_get_string_value (gc-get-node cam name) (cffi:null-pointer))))

(defmethod gc-enumeration-get-int-value ((cam camera) name)
  (#_arv_gc_enumeration_get_int_value (gc-get-node cam name) (cffi:null-pointer)))
(defmethod gc-enumeration-set-int-value ((cam camera) name val)
  (#_arv_gc_enumeration_set_int_value (gc-get-node cam name) val (cffi:null-pointer)))

(defmethod gc-enumeration-get-available-int-values ((cam camera) name)
  (cffi:with-foreign-object (n-values :unsigned-int)
    (let* ((int-ptr (#_arv_gc_enumeration_get_available_int_values (gc-get-node cam name) n-values (cffi:null-pointer)))
	   (n (cffi:mem-ref n-values :unsigned-int)))
      (loop for i below n collect (cffi:mem-aref int-ptr :unsigned-long-long i)))))

#+nil
(gc-enumeration-get-available-int-values *cam1* "PixelFormat")


(defmethod set-pixel-format ((cam camera) format)
  (unless (member format (pixel-formats cam) :test #'string=)
    (error "This camera only supports the formats ~a. You requested '~a'." 
	   (pixel-formats cam)
	   format))
  (gc-enumeration-set-string-value cam "PixelFormat" format))

(defmethod %basler-temperatures ((cam camera))
  (loop for (i name) in '((0 sensor)
			  (1 core)
			  (2 framegrabber)
			  (3 case))
	collect 
	(progn
	  (gc-enumeration-set-int-value cam "TemperatureSelector" i)
	  (list name
		(gc-float-get-value cam "TemperatureAbs")))))

(defmethod %photonfocus-temperatures ((cam camera))
  (loop for (i name) in '((0 sensor)
			  (1 sensor-board)
			  (2 adc-board))
     collect 
       (progn
	 (gc-integer-set-value cam "DeviceTemperatureSelectorVal" i)
	 (gc-command-execute cam "DeviceTemperature_Update")
	 (list
	  name
	  (gc-float-get-value cam "DeviceTemperature")))))

(defmethod temperatures ((cam camera))
  (cond
    ((string= "Photonfocus AG" (arv-vendor-name cam))
     (%photonfocus-temperatures cam))
    ((string= "Basler" (arv-vendor-name cam))
     (%basler-temperatures cam))))

#+nil
(temperatures *cam1*)

(defmethod get-payload ((cam camera))
  (#_arv_gc_integer_get_value 
   (gc-get-node cam "PayloadSize")
   (cffi:null-pointer)))

(defmethod start-acquisition ((cam camera))
  (#_arv_camera_start_acquisition (arv-camera cam)))

(defmethod stop-acquisition ((cam camera))
  (#_arv_camera_stop_acquisition (arv-camera cam)))

(defparameter *basler-acquisition-modes*
  '((single-frame . 0)
    (multi-frame . 1)
    (continuous . 2)))

(defparameter *photonfocus-acquisition-modes*
  '((continuous . 0)
    (single-frame . 1)
    (multi-frame . 2)
    (continuous-recording . 3)
    (continuous-readout . 4)
    (single-frame-recording . 5)
    (single-frame-readout . 6)))

(defun all-possible-acquisition-modes ()
  (let ((res nil))
    (loop for (mode . code) in (append *basler-acquisition-modes* *photonfocus-acquisition-modes*) do
	 (setf res (adjoin mode res)))
    res))

#+nil
(all-possible-acquisition-modes)

(defmethod acquisition-mode-code ((cam camera) mode)
  (unless (member mode (all-possible-acquisition-modes))
    (break "invalid mode. use one of ~a."
	   (all-possible-acquisition-modes)))
  (cdr (assoc mode (cond
		      ((string= "Photonfocus AG" (arv-vendor-name cam))
		       *photonfocus-acquisition-modes*)
		      ((string= "Basler" (arv-vendor-name cam))
		       *basler-acquisition-modes*)))))

(defmethod acquisition-code-mode ((cam camera) code)
  (car (rassoc code (cond
		      ((string= "Photonfocus AG" (arv-vendor-name cam))
		       *photonfocus-acquisition-modes*)
		      ((string= "Basler" (arv-vendor-name cam))
		       *basler-acquisition-modes*)))))

(defmethod set-acquisition-mode ((cam camera) mode)
  (#_arv_gc_enumeration_set_int_value 
   (gc-get-node cam "AcquisitionMode") (acquisition-mode-code cam mode)
   (cffi:null-pointer)))

(defmethod get-acquisition-mode ((cam camera))
  (acquisition-code-mode cam
			 (#_arv_gc_enumeration_get_int_value 
			  (gc-get-node cam "AcquisitionMode")
			  (cffi:null-pointer))))

;; 			for (i = 0; i < 50; i++)
;; 				arv_stream_push_buffer (stream, arv_buffer_new (payload, NULL));

(defmethod push-buffer ((cam camera) &optional buffer)
  (#_arv_stream_push_buffer (arv-stream cam)
			    (or buffer
				(let ((b (#_arv_buffer_new (get-payload cam) (cffi:null-pointer))))
				  (assert (not (cffi:null-pointer-p b)))
				  b))))



(defmethod pop-buffer-blocking ((cam camera))
  (#_arv_stream_pop_buffer (arv-stream cam)))

(defmethod pop-block-copy-push-buffer ((cam camera) &optional out)
  (let ((b (pop-buffer-blocking cam)))
    (when (cffi:null-pointer-p b)
      (error "pop-buffer returned NULL."))
    (prog1
	(let* ((a (or out
		      (make-array (list (aoi-height cam)
					(aoi-width cam))
				  :element-type '(unsigned-byte 16))))
	       (a1 (make-array (reduce #'* (array-dimensions a))
			       :element-type '(unsigned-byte 16)
			       :displaced-to a))
	       (data (pref b #>ArvBuffer.data)))
	  (dotimes (i (min (length a1) (floor (get-payload cam) 2)))
	    (setf (aref a1 i) (%get-unsigned-word data i)))
	  a)
	(push-buffer cam b))))


#+nil
(ccl:%get-unsigned-byte *bsafe* 5)
(defmethod get-statistics ((cam camera))
  (cffi:with-foreign-objects ((completed :uint64)
			      (failures :uint64)
			      (underruns :uint64))
    (#_arv_stream_get_statistics (arv-stream cam) completed failures underruns)
    `((completed . ,(cffi:mem-ref completed :uint64))
      (failures . ,(cffi:mem-ref failures :uint64))
      (underruns . ,(cffi:mem-ref underruns :uint64)))))


#+nil
(progn
  (defparameter *cam2*
    (make-instance 'camera :name "Basler-21211553"))

  (defparameter *cam1*
    (make-instance 'camera)))


#+nil (aoi-height *cam2*)

#+nil
(sensor-width *cam2*)
#+nil
(set-region *cam2*)
#+nil
(get-payload *cam2*)
#+nil
(push-buffer *cam2*)
#+nil
(start-acquisition *cam2*)
#+nil
(get-region *cam2*)
#+nil
(set-region *cam2*)
#+nil
(defparameter *bla*
  (pop-buffer-blocking *cam2*))
#+nil
(pop-buffer-blocking *cam2*)

#+nil
(set-pixel-format *cam2* "Mono8")

#+nil
(gc-enumeration-get-string-value *cam2* "PixelFormat")
#+nil
(gc-enumeration-get-int-value *cam2* "PixelFormat")

;; (defparameter *a* (make-array (list (aoi-height *cam2*)
;; 				    (aoi-width *cam2*))
;; 			      :element-type '(unsigned-byte 16)))
;; (defparameter *a1* (make-array (reduce #'* (array-dimensions *a*))
;; 			       :element-type '(unsigned-byte 16)
;; 			       :displaced-to *a*))

;; (list (* 2 (length *a1*)) (get-payload *cam2*))
;; (dotimes (i (min (length *a1*)))
;; 	    (setf (aref *a1* i) (cffi:mem-aref *bla* :uint16 i
;; 					       )))
;; #+nil

#+nil
(get-statistics *cam2*)
#+nil
(defparameter *img* (pop-block-copy-push-buffer *cam2*))
#+nil
(let* ((c *cam2*)
       (n (get-payload c)))
  (dotimes (i 1) (push-buffer c))
  (start-acquisition c)
  (defparameter *img* (pop-block-copy-push-buffer c))
  (format t "statistics: ~a~%" (get-statistics c))
  (sleep .3)
  (format t "statistics: ~a~%" (get-statistics c))
  (stop-acquisition c))
#+nil
(stop-acquisition *cam2*)
#+nil
(progn
 (set-acquisition-mode *cam2* 'single-frame)
 (get-acquisition-mode *cam2*))

