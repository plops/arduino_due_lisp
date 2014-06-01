(in-package :arv)
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
