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

#+nil
(#_arv_update_device_list)
#+nil
(#_arv_get_n_devices)
#+nil
(#_arv_get_n_interfaces)
#+nil
(char*-to-lisp
 (#_arv_get_interface_id 0))
#+nil
(char*-to-lisp
 (#_arv_get_device_id 0))



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
   (pixel-format :accessor pixel-format :type string :initform "none")
   (dark-image :accessor dark-image :initform nil)))

(defmethod create-stream ((cam camera))
  (#_arv_camera_create_stream (arv-camera cam) (cffi:null-pointer)
			      (cffi:null-pointer)))

(defmethod destroy-stream ((cam camera))
  (with-slots (arv-stream) cam
    (unless (cffi:null-pointer-p arv-stream)
      (#_g_object_unref arv-stream)
      (setf arv-stream (cffi:null-pointer)))))


(defmethod set-region ((cam camera) &key (x 0) (y 0)
				      (w (- (sensor-width cam) x))
				      (h (- (sensor-height cam) y)))
  (assert (<= 0 x (1- (sensor-width cam))))
  (assert (<= 0 y (1- (sensor-height cam))))
  (assert (<= 0 (+ x w) (sensor-width cam)))
  (assert (<= 0 (+ y h) (sensor-height cam)))
  (multiple-value-bind (ox oy ow oh) (get-region cam)
    (when (and (= ox x) (= oy y) (= ow w) (= oh h))
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
      (prog1
	  (loop for i below n collect
	       (let ((c-str (cffi:mem-aref c-strs :pointer i)))
		 (char*-to-lisp c-str)))
	(#_g_free c-strs)))))

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

(defmethod gc-enumeration-get-available-int-values ((cam camera) name)
  (cffi:with-foreign-object (n-values :unsigned-int)
    (let* ((int-ptr (#_arv_gc_enumeration_get_available_int_values (gc-get-node cam name) n-values (cffi:null-pointer)))
	   (n (cffi:mem-ref n-values :unsigned-int)))
      (loop for i below n collect (cffi:mem-aref int-ptr :unsigned-long-long i)))))

(defmethod set-pixel-format ((cam camera) format)
  (declare (type string format))
  (let ((formats (gc-enumeration-get-available-string-values cam "PixelFormat")))
   (unless (member format formats :test #'string=)
     (error "This camera only supports the formats ~{#x~x~}. You requested #x~x." 
	    formats format)))
  (gc-enumeration-set-string-value cam "PixelFormat" format)
  (setf (pixel-format cam) format))

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

(defmethod push-buffer ((cam camera) &optional buffer)
  (#_arv_stream_push_buffer 
   (arv-stream cam)
   (or buffer
       (let ((b (#_arv_buffer_new (get-payload cam) (cffi:null-pointer))))
	 (assert (not (cffi:null-pointer-p b)))
	 b))))

(defmethod pop-buffer-blocking ((cam camera))
  (#_arv_stream_pop_buffer (arv-stream cam)))

(defmethod timeout-pop-buffer-blocking ((cam camera) timeout-us)
  (#_arv_stream_timeout_pop_buffer (arv-stream cam) timeout-us))

(defmethod try-pop-buffer ((cam camera))
  (#_arv_stream_try_pop_buffer (arv-stream cam)))

#+nil
(defparameter *bla* (try-pop-buffer *cam2*))

#+nil
(push-buffer *cam1*)

(defmethod pop-block-copy-push-buffer ((cam camera) &key out (use-dark t))
  (ensure-no-threads-waiting-for-buffer cam)
  (ensure-at-least-one-buffer-in-stream cam)
  (let ((b (timeout-pop-buffer-blocking cam 10000))
	(dark1 (when (and use-dark (dark-image cam))
		 (let ((d (dark-image cam)))
		   (assert (equal (list (aoi-height cam) (aoi-width cam))
				  (array-dimensions d)))
		      (make-array (reduce #'* (array-dimensions d))
				  :element-type (array-element-type d)
				  :displaced-to d)))))
    (loop while (or (cffi:null-pointer-p b)
		    (and (not (cffi:null-pointer-p b))
			 (/= #$ARV_BUFFER_STATUS_SUCCESS (pref b #>ArvBuffer.status)))
		    (and (not (cffi:null-pointer-p b))
			 (not (pref b #>ArvBuffer.data))))
	 for i from 0 below 1000 do
	 (when (= 0 (mod i 10))
	   (format t "popped buffer not satisfactory ~a~%" (list (and (not (cffi:null-pointer-p b))
								      (pref b #>ArvBuffer.status))
								 (get-statistics cam)
								 (multiple-value-list
								  (get-n-buffers cam))
								 (temperatures cam))))
	 (ensure-no-threads-waiting-for-buffer cam)
	 (ensure-at-least-one-buffer-in-stream cam)
	 (when (= i 999)
	   (if (cffi:null-pointer-p b)
	       (error "pop-buffer returned NULL.")
	       (unless (= #$ARV_BUFFER_STATUS_SUCCESS (pref b #>ArvBuffer.status))
		 (error "pop-buffer didnt succeed."))))
	 (setf b (timeout-pop-buffer-blocking cam 10000)))
    ;; (when (cffi:null-pointer-p b)
    ;;   (error "pop-buffer returned NULL."))
    ;; (unless (= #$ARV_BUFFER_STATUS_SUCCESS (pref b #>ArvBuffer.status))
    ;;   (error "buffer status not success"))
    (prog1
	(let* ((a (or out
		      (make-array (list (aoi-height cam)
					(aoi-width cam))
				  :element-type '(unsigned-byte 16))))
	       (n (reduce #'* (array-dimensions a)))
	       (np (get-payload cam))
	       (a1 (make-array n
			       :element-type (array-element-type a)
			       :displaced-to a))
	       (data (pref b #>ArvBuffer.data)))
	  (cond
	    ((string= (pixel-format cam) "Mono12")
	     (dotimes (i (min (length a1) (floor np 2)))
	       (setf (aref a1 i) (%get-unsigned-word data (* 2 i)))))
	    ((string= (pixel-format cam) "Mono12Packed")
	     (loop for byte below (get-payload cam) by 3
		and short from 0 below n by 2
		do
		;; 3 bytes in the data stream correspond to two data
		;; elements: AB CD EF -> ABD, EFC 
		;; A is the most signifcant nibble
		  (let ((ab (%get-unsigned-byte data byte))
			(c (ldb (byte 4 0) (%get-unsigned-byte data (+ 1 byte))))
			(d (ldb (byte 4 4) (%get-unsigned-byte data (+ 1 byte))))
			(ef (%get-unsigned-byte data (+ 2 byte))))
		    (setf (aref a1 short) (ash (+ (ash ab 4) d) 4)
			  (aref a1 (1+ short)) (ash (+ (ash ef 4) c) 4)))))
	    (t (error "datatype is undefined.")))
	  (when dark1 
	    (dotimes (i n)
	      (setf (aref a1 i) (min 65535 (max 0 (+ (aref a1 i)  (- (aref dark1 i)) 100))))))
	  a)
	(push-buffer cam b))))

(defmethod get-statistics ((cam camera))
  (cffi:with-foreign-objects ((completed :uint64)
			      (failures :uint64)
			      (underruns :uint64))
    (#_arv_stream_get_statistics (arv-stream cam) completed failures underruns)
    `((completed . ,(cffi:mem-ref completed :uint64))
      (failures . ,(cffi:mem-ref failures :uint64))
      (underruns . ,(cffi:mem-ref underruns :uint64)))))

#+nil
(get-statistics *cam1*)

#+nil
(cdr (second (get-statistics *cam1*)))
#+nil
(get-statistics *cam2*)

(defmethod get-n-buffers ((cam camera))
  (cffi:with-foreign-objects ((n-in :int)
			      (n-out :int))
    (#_arv_stream_get_n_buffers (arv-stream cam) n-in n-out)
    (values (cffi:mem-ref n-in :int)
	    (cffi:mem-ref n-out :int))))

#+nil
(get-n-buffers *cam1*)
#+nil
(get-n-buffers *cam2*)


#+nil
(defparameter *bla* (pop-block-copy-push-buffer *cam2*))


(defmethod ensure-at-least-one-buffer-in-stream ((cam camera))
  (multiple-value-bind (in out) (get-n-buffers cam)
    (declare (ignorable out))
    (when (= in 0)
      (push-buffer cam))))

(defmethod ensure-no-threads-waiting-for-buffer ((cam camera))
  (multiple-value-bind (in out) (get-n-buffers cam)
    (declare (ignorable in))
    (when (<= out 0)
      (start-acquisition cam))))

#+nil
(start-acquisition *cam2*)
#+nil
(stop-acquisition *cam2*)

#+nil
(push-buffer *cam1*)
;; => "Photonfocus AG-030300019307"
#+nil
(push-buffer *cam2*)
(defmethod acquire-single-image ((c camera) &key (use-dark t))
  ;(dotimes (i 1) (push-buffer c))
  (start-acquisition c)
  (prog1
      (pop-block-copy-push-buffer c :use-dark use-dark)
    (stop-acquisition c)))

(defun write-pgm (filename img)
  (declare (type simple-string filename)
           ((array (unsigned-byte 16) 2) img)
           #+sbcl (values null &optional))
  (destructuring-bind (h w) (array-dimensions img)
    (declare (type fixnum w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%65535~%~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 16)
                       :direction :output
                       :if-exists :append) ;; FIXME: i think this append doesn't work as expected and sometimes eats one of the new lines
      (let ((data-1d (make-array 
                      (* h w)
                      :element-type '(unsigned-byte 16)
                      :displaced-to img)))
        (write-sequence data-1d s)))
    nil))

#+nil
(defparameter *cam1*
	   (make-instance 'camera))


#+nil
(progn  (set-region *cam1* :x 712 :y 712 :w 600 :h 600)
	(loop for c in (list *cam1*) and i from 1 do 
	     (set-exposure c 4000d0)
	     (set-acquisition-mode c 'single-frame)
	     (set-pixel-format c "Mono12Packed")
	     (push-buffer c)
	     (write-pgm (format nil "/dev/shm/~d.pgm" i)
			(acquire-single-image c :use-dark nil))))


#+nil
(dotimes (i 100)
 (write-pgm (format nil "/dev/shm/~d.pgm" 1)
	    (acquire-single-image *cam1* :use-dark t)))

#+nil
(defparameter *cam2*
	   (make-instance 'camera :name "Basler-21211553"))

;; network card has to be configured like this:
;; ifconfig enp0s7 169.254.140.1
#+nil
(progn
  (progn (defparameter *cam2*
	   (make-instance 'camera :name "Basler-21211553"))
	 
	 (defparameter *cam1*
	   (make-instance 'camera)))
  (set-region-centered *cam1* :cx 800 :cy 1024 :w 512 :h 512)
  (gc-enumeration-set-int-value *cam1* "Correction_Mode" 0)
  (set-region *cam2* :x (- 659 512) :w 512  :h 494)
  (loop for c in (list *cam1* *cam2*) and i from 1 do 
       (set-exposure c 1000d0)
       (set-acquisition-mode c 'single-frame)
       (set-pixel-format c "Mono12Packed")
       (ensure-at-least-one-buffer-in-stream c)
       (write-pgm (format nil "/dev/shm/~d.pgm" i)
		  (acquire-single-image c :use-dark nil))))

#+nil
(set-region-centered *cam1* :cx 790 :cy 1000 :w 256 :h 256)
#+nil
(set-region-centered *cam2* :cx 400 :cy 270 :w 390  :h 390)


#+nil
(loop for c in (list *cam1* *cam2*) and i from 1 do 
       (set-exposure c 4000d0)
       (set-acquisition-mode c 'single-frame)
       (set-pixel-format c "Mono12Packed")
       (push-buffer c)
       (write-pgm (format nil "/dev/shm/~d.pgm" i)
		  (acquire-single-image c :use-dark t)))

(defmethod set-region-centered ((c camera) &key cx cy w h)
  (set-region c :x (- cx (floor w 2))
	      :y (- cy (floor h 2))
	      :w w
	      :h h))
#+nil

#+nil
(list (set-exposure *cam2* 1d0) (get-exposure *cam2*))
#+nil
(get-exposure *cam1*)
#+nil
(set-exposure *cam1* 2000d0)

#+nil
(progn
  (setf (slot-value *cam1* 'dark-image) (acquire-single-image *cam1* :use-dark nil))
 nil)

#+nil
(set-region *cam1*)
#+nil
(set-pixel-format *cam1* "Mono12Packed")
#+nil
(defparameter *bla* (acquire-single-image *cam1*))
#+nil
(set-acquisition-mode *cam1* 'single-frame)
#+nil
(set-acquisition-mode *cam2* 'single-frame)
#+nil
(let ((a (make-array (reduce #'* (array-dimensions *bla*))
		     :element-type (array-element-type *bla*)
		     :displaced-to *bla*)))
  (list
   (reduce #'max a)
   (reduce #'min a)))
#+nil
(write-pgm "/dev/shm/2.pgm" (acquire-single-image *cam2*))

#+nil
(temperatures *cam1*)
#+nil
(progn
  ;(delete-file "/dev/shm/1.pgm")
  ;(delete-file "/dev/shm/2.pgm")
  (loop for c in (list *cam1* *cam2*) and i from 1 do 
       (set-exposure c 3000d0)
       (set-acquisition-mode c 'single-frame)
       (set-pixel-format c "Mono12Packed")
       (talk-arduino "(dac 1000 2000)")
       (write-pgm (format nil "/dev/shm/~d.pgm" i)
		  (acquire-single-image c))))

#+nil
(set-exposure *cam2* 300d0)
#+nil
(set-exposure *cam1* 900d0)
#+nil 
(+ 19 25 15)
#+nil
(+ 30 3.5 2.5 13.5 9) ;; path camera 2 (good interference)

#+nil
(talk-arduino "(dac 2000 2047)")

;; fsm moves +/- 1.5 degree, (26.2 mrad) for voltages in +/- 10v
;; (* 150 (tan 26.2e-3)) corresponds to 3.9 mm after 150mm



#+nil
(progn
  (talk-arduino "(dac 2047 2047)")
 (dotimes (j 128)
   (loop for c in (list *cam1* *cam2*) and i from 1 do 
	(format t ".")
	(write-pgm (format nil "/dev/shm/~3,'0d_~d.pgm" j i)
		   (if nil
		       (average-images c :number 100 :use-dark t)
		       (acquire-single-image c :use-dark t))))))

(+ 21 24 11)

#+nil
(gc-enumeration-get-int-value *cam1* "Correction_Mode")
#+nil
(gc-enumeration-set-int-value *cam1* "Correction_Mode" 0)

#.(load "/home/martin/src/ccl/library/serial-streams.lisp")

(defvar *serial* nil)


;; in gentoo user should be in group uucp for access to ACM device
(defparameter *serial*
  (ccl::make-serial-stream (concatenate 'string "/dev/" (pathname-name (first (directory "/dev/ttyACM*"))))
					;:format 'character
                           :baud-rate 115200
                           :parity nil
                           :char-bits 8
                           :stop-bits 1 
                           :flow-control nil))
#+nil
(progn
  (format *serial* "(dac ~d ~d)~%" (+ -900 2048) (+ 130 2048)) 
  (force-output *serial*)
  (sleep .1)
  (list 
   (read-line *serial*)))

#+nil
(read-line *serial*)

(defun talk-arduino (cmd)
  (format *serial* cmd) 
  (force-output *serial*)
  (sleep .1)
  (list 
   (read-line *serial*)))

#+nil
(read-line *serial*)

#+nil
(talk-arduino "(+ 1 2)")

;; pin 8 is connected to thorlab shutter controller (shows x-gate and enabled)

#+nil
(talk-arduino "(pin-mode 8 1)") ;; set pin 8 to output

#+nil
(talk-arduino "(digital-write 8 0)") ;; set pin 8 to high

#+nil
(talk-arduino "(dac 2047 2047)")

;; length of fiber 85cm
;; 18.5 36
(+ 18.5 3.6 (* 1.5 85) 32) ;; red camera through fiber
(+ 18.5 3.6 (* 1.5 85) 11 22)

;; with reflector prism at 8.3cm
(+ 28 2.5 17.5 60 11.5 27.2 11) ;; red camera reference 
(+ 28 2.5 17.5 60 13.5 27.7 9.3) ;; small black camera reference

(+ 8.3 (* .5 (- 181.2 157.7))) 

#+nil
(talk-arduino (format nil "(progn (dac ~d ~d) (delay 10) (print (adc 0)))" (+ 2048) (+ 2048)))
#+nil
(talk-arduino (format nil "(adc 0)"))


#+nil
(dotimes (i 10)
 (let ((c (complex (+ 1200d0) 2047d0))
       (r 1150d0)
       (n (* 12)))
   (prog1
       (loop for i below n collect 
	    (let ((z (+ c (* r (exp (complex 0d0 (* 2 pi i (/ 1d0 n))))))))
					;(sleep .05)
	      (let ((cmd (format nil "(dac ~d ~d)" 
				 (floor (min 4095 (max 0 (realpart z))))
				 (floor (min 4095 (max 0 (imagpart z)))))))
		(talk-arduino cmd)
		cmd)))
     #+nil
     (talk-arduino  (format nil "(dac ~d ~d)" (floor (realpart c)) (floor (imagpart c)))))))

#+nil
(let ((res nil)
      (ic 1200)
      (ir 1100)
      (jc 2047)
      (jr 1100))
 (loop for j from (- jc jr) upto (+ jc jr) by 100 do
      (loop for i from (- ic ir) upto (+ ic ir) by 100 do
	   (talk-arduino (format nil "(dac ~d ~d)" i j))
	   (let* ((a (acquire-single-image *cam1*))
		  (a1 (make-array (reduce #'* (array-dimensions a))
				  :element-type (array-element-type a)
				  :displaced-to a))
		  (val (reduce #'+ a1)))
	     (push (list i j val) res)
	     (format t "~a~%" (list i j val)))))
 (defparameter *scan* (reverse res)))

#+nil
(sort (copy-list *scan*) #'< :key #'third)

#+nil
(defparameter *scan1* *scan*)

(defun .linear (a)
  (make-array (array-total-size a)
	      :element-type (array-element-type a)
	      :displaced-to a))

(defun .max (a)
  (reduce #'max (.linear a)))

(defmethod average-images ((c camera) &key (number 100) (use-dark t))
  (let* ((a (acquire-single-image c))
	 (a1 (.linear a))
	 (accum (make-array (array-dimensions a)
			    :element-type '(unsigned-byte 32)
			    :initial-element 0))
	 (accum1 (.linear accum)))
    ;(push-buffer c)
    (format t "averaging ")
    (dotimes (i number)
      (format t ".")
      (progn 
	(progn (start-acquisition c)
	       (prog1 ;; acquire raw images
		   (pop-block-copy-push-buffer c :use-dark use-dark :out a)
		 (stop-acquisition c)))
	(dotimes (i (length a1))
	 (incf (aref accum1 i) (ash (aref a1 i) -4)))))
    (dotimes (i (length accum1))
      (setf (aref a1 i) (floor (* 16 (aref accum1 i)) number)))
    (terpri)
    a))


#+nil
(defparameter *bla*
 (average-images *cam1*))

#+nil
(talk-arduino (format nil "(dac ~d 2047)" (+ (* 40 15) 2047)) )

(defmethod acquire-image-using-full-range ((c camera) &key (use-dark t))
  "vary exposure time to keep gray values within 40000 .. 60000. note: if a dark image is subtracted i introduce an offset of 100. in particular for the cmos camera the dark images can have quite high values."
  (let* ((im (acquire-single-image c :use-dark use-dark))
	 (ma (.max im))
	 (dark-max (.max (dark-image c)))
	 (goal-min (- 40000 dark-max))
	 (goal-max (- 60000 dark-max)))
    (loop for i from 0 while (not (< goal-min ma goal-max)) do
	 (let ((new-exp (* (cond ((< ma goal-min) 1.2)
				 ((< goal-max ma) 0.8)
				 (t 1.0)) 
			   (get-exposure c))))
	   (unless (< 20 new-exp 10000)
	     ;; keep the exposure time in useful limits
	     ;; i'm not sure if i will have to define individual limits for each camera type
	     (return-from acquire-image-using-full-range im))
	   (set-exposure c new-exp)
	   (format t "exposure time is ~a now~%" new-exp)
	   (setf im (acquire-single-image c :use-dark use-dark)
		 ma (.max im))))
    im))
;; in a run that worked cam1 206.22 .. 9930.9 and cam2 22.0 .. 2271


#+nil
(set-exposure *cam2* 400d0)

#+nil
(get-n-buffers *cam2*)
#+nil
(start-acquisition *cam2*)
#+nil
(ensure-no-threads-waiting-for-buffer *cam2*)
#+nil
(push-buffer *cam2*)

#+nil
(dotimes (i 1000)
  ;(sleep .2)
 (progn
   (format t "~a~%" (list (get-statistics *cam1*) (multiple-value-list (get-n-buffers *cam1*))
			  (get-statistics *cam2*) (multiple-value-list (get-n-buffers *cam2*))))
   (write-pgm "/dev/shm/1.pgm" (acquire-single-image *cam1* :use-dark t) #+nil (acquire-image-using-full-range *cam1*))
   (write-pgm "/dev/shm/2.pgm" (acquire-single-image *cam2*) #+nil (acquire-image-using-full-range *cam2*))))


#+nil
(progn
  (talk-arduino "(pin-mode 8 1)")
  (talk-arduino "(digital-write 8 0)")
  (setf (dark-image *cam1*) (average-images *cam1* :number 100 :use-dark nil))
  (setf (dark-image *cam2*) (average-images *cam2* :number 100 :use-dark nil))
  (sleep .2)
  (talk-arduino "(digital-write 8 1)"))

#+nil
(loop for i from 0 below 300 by 3 do
     (format t "~a~%" i)
     (talk-arduino (format nil "(dac ~d 2048)" (floor (+ 2000 (* 1000 (sin (* 2 pi (/ i 100)))))))))


#+nil
(talk-arduino (format nil "(dac 657 2048)" ))
#+nil
(talk-arduino (format nil "(+ 1 2)" ))



#+nil
(loop for dir-num from 3 below 15 do
 (let ((ic 2047)
       (ir 1500)
       (jc 2047)
       (jr 1600))
   (ensure-directories-exist (format nil "/home/martin/dat/~d/" dir-num))
   (loop for j from (- jc jr) upto (+ jc jr) by 40 do
	(loop for i from (- ic ir) upto (+ ic ir) by 40 do
	     (format t "~a~%" (list 'i i 'j j))
	     (talk-arduino (format nil "(dac ~d ~d)~%" i j))
					;(sleep 2)
	     (loop for c in (list *cam1* *cam2*) and k from 1 do 
		  (format t "acquire ~d ~a~%" k (list (get-statistics c) (multiple-value-list (get-n-buffers c))))
		  
		  (let ((im (if nil
				(average-images c :number 10 :use-dark t)
					;(acquire-single-image c :use-dark t)
				(acquire-image-using-full-range c)
				)))
		    (write-pgm (format nil "/dev/shm/~d.pgm" k) im)
		    (write-pgm (format nil "/home/martin/dat/~d/i~4,'0d_j~4,'0d_~d_~2,6$.pgm" dir-num i j k (get-exposure c)) im)))))))

;; network usage is 2.2Mbytes/s
;; (/ (* 11 1024) 2.2) => 5100 seconds for one scan

 





