(in-package :pylon)

(cffi:defcfun ("pylon_wrapper_initialize" initialize) :void)
(cffi:defcfun ("pylon_wrapper_terminate" terminate) :void (cams :pointer))
(cffi:defcfun ("pylon_wrapper_factory" factory) (:pointer :void))
(cffi:defcfun ("pylon_wrapper_create" %create) (:pointer :void)
  (factory :pointer)
  (max-cameras :unsigned-int))

(defun create (factory max-cameras)
  #+sbcl
  (sb-int:with-float-traps-masked (:invalid)
    (%create factory max-cameras))
  #-sbcl
  (%create factory max-cameras))

(cffi:defcfun ("pylon_wrapper_start_grabbing" start-grabbing) :void
  (cams :pointer))

(cffi:defcfun ("pylon_wrapper_grab" %grab) :void
  (cams :pointer)
  (ww :int)
  (hh :int)
  (buf (:pointer :unsigned-char))
  (camera (:pointer :int))
  (success-p (:pointer :int))
  (w (:pointer :int))
  (h (:pointer :int)))


(defun grabp (cams w h buf)
  ;; buf is returned by foreign-alloc
  (cffi:with-foreign-objects ((cam :int)
			      (success-p :int)
			      (wout :int)
			      (hout :int))
    (%grab cams w h buf
	   cam success-p wout hout)
    (values (cffi:mem-ref cam :int)
	    (cffi:mem-ref success-p :int)
	    (cffi:mem-ref wout :int)
	    (cffi:mem-ref hout :int))))

(defun grab (cams buf)
  (declare (type (simple-array (unsigned-byte 8) 2) buf))
  ;; buf must be displaced to a 1d simple-array
  (destructuring-bind (h w) (array-dimensions buf)
    (sb-sys:with-pinned-objects (buf)
      (let ((bufp (sb-sys:vector-sap 
		   (sb-ext:array-storage-vector
		    buf))))
	(cffi:with-foreign-objects ((cam :int)
				    (success-p :int)
				    (wout :int)
				    (hout :int))
	  (%grab cams w h bufp
		 cam success-p wout hout)
	  (values (cffi:mem-ref cam :int)
		  (if (= (cffi:mem-ref success-p :int) 1) t nil)
		  (cffi:mem-ref wout :int)
		  (cffi:mem-ref hout :int)))))))

(cffi:defcfun ("pylon_wrapper_grab_cdf" %grab-cdf) :void
  (cams :pointer)
  (ww :int)
  (hh :int)
  (buf (:pointer :double))
  (camera (:pointer :int))
  (success-p (:pointer :int))
  (w (:pointer :int))
  (h (:pointer :int))
  (frame-nr (:pointer :int)))

(defun grab-cdf (cams buf)
  (declare (type (or (array (complex double-float) 2)
		     (simple-array (complex double-float) 2)) buf))
  ;; buf must be displaced to a 1d simple-array or a simple
  ;; multi-dimensional array
  (destructuring-bind (h w) (array-dimensions buf)
    (sb-sys:with-pinned-objects (buf)
      (let ((bufp (sb-sys:vector-sap 
		   (handler-case
		       (sb-ext:array-storage-vector buf)
		     (simple-error ()
		       (array-displacement buf))))))
	(cffi:with-foreign-objects ((cam :int)
				    (success-p :int)
				    (wout :int)
				    (hout :int)
				    (framenr :int))
	  (%grab-cdf cams w h bufp
		 cam success-p wout hout framenr)
	  (values (cffi:mem-ref cam :int)
		  (if (= (cffi:mem-ref success-p :int) 1) t nil)
		  (cffi:mem-ref wout :int)
		  (cffi:mem-ref hout :int)
		  (cffi:mem-ref framenr :int)
		  ))))))

(cffi:defcfun ("pylon_wrapper_grab_store" %grab-store) :void
  (cams :pointer)
  (nfd :int)
  (fd (:pointer :int))
  (camera (:pointer :int))
  (success-p (:pointer :int))
  (w (:pointer :int))
  (h (:pointer :int))
  (frame-nr (:pointer :int)))

(defun grab-store (cams fds)
  "grab one image and write it into one of the file descriptors"
  (let* ((n (length fds))
	 (fda (make-array n :element-type '(signed-byte 32)
			  :initial-contents fds)))
    (sb-sys:with-pinned-objects (fda)
      (cffi:with-foreign-objects ((cam :int)
				  (success-p :int)
				  (wout :int)
				  (hout :int)
				  (framenr :int))
	(%grab-store cams n (sb-sys:vector-sap fda)
		     cam success-p wout hout framenr)
	(values (cffi:mem-ref cam :int)
		(cffi:mem-ref success-p :int)
		(cffi:mem-ref wout :int)
		(cffi:mem-ref hout :int)
		(cffi:mem-ref framenr :int))))))


(cffi:defcfun ("pylon_wrapper_get_max_i" get-max-i) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_get_min_i" get-min-i) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_get_inc_i" get-inc-i) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_get_value_f" %get-value-f) :float
  (cams :pointer)
  (cam :int)
  (node :string)
  (verify :int)
  (ignore-cache :int))
(cffi:defcfun ("pylon_wrapper_get_value_b" %get-value-b) :int
  (cams :pointer)
  (cam :int)
  (node :string)
  (verify :int)
  (ignore-cache :int))
(cffi:defcfun ("pylon_wrapper_get_value_i" %get-value-i) :int
  (cams :pointer)
  (cam :int)
  (node :string)
  (verify :int)
  (ignore-cache :int))

(defun get-value-f (cams cam node &optional (verify nil) (ignore-cache nil))
  (%get-value-f cams cam node (if verify 1 0) (if ignore-cache 1 0)))
(defun get-value-b (cams cam node &optional (verify nil) (ignore-cache nil))
  (%get-value-b cams cam node (if verify 1 0) (if ignore-cache 1 0)))
(defun get-value-i (cams cam node &optional (verify nil) (ignore-cache nil))
  (%get-value-i cams cam node (if verify 1 0) (if ignore-cache 1 0)))

(cffi:defcfun ("pylon_wrapper_set_value_i" set-value-i) :void
  (cams :pointer)
  (cam :int)
  (node :string)
  (value :int))

(cffi:defcfun ("pylon_wrapper_get_symbolics_e" get-symbolics-e) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_set_value_e" set-value-e) :void
  (cams :pointer)
  (cam :int)
  (node :string)
  (value :int))

(cffi:defcfun ("pylon_wrapper_get_value_e" get-value-e) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_command_isdone" command-isdone) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_command_execute" command-execute) :int
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_to_string_e" to-string-e) :void
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_from_string_e" from-string-e) :void
  (cams :pointer)
  (cam :int)
  (node :string)
  (value :string))

(cffi:defcfun ("pylon_wrapper_stop_grabbing" stop-grabbing) :void
  (cams :pointer))

(cffi:defcfun ("pylon_wrapper_cams_open" cams-open) :void
  (cams :pointer))

(cffi:defcfun ("pylon_wrapper_cams_close" cams-close) :void
  (cams :pointer))

(cffi:defcfun ("pylon_wrapper_cam_open" cam-open) :void
  (cams :pointer)
  (cam :int))

(cffi:defcfun ("pylon_wrapper_cam_close" cam-close) :void
  (cams :pointer)
  (cam :int))

(cffi:defcfun ("pylon_wrapper_cam_get_serial_number" cam-get-serial-number) :string
  (cams :pointer)
  (cam :int))
(cffi:defcfun ("pylon_wrapper_cam_get_full_name" cam-get-full-name) :string
  (cams :pointer)
  (cam :int))


