(in-package :pylon)

(cffi:defcfun ("pylon_wrapper_initialize" initialize) :void
  "Initializes the pylon runtime system. Call this before any other
pylon function. Don't forget to call `terminate` before you close the
lisp session. Otherwise cameras may need to be power cycled.")
(cffi:defcfun ("pylon_wrapper_terminate" terminate) :void
  "Deletes the opaque pointer to the CInstantCameraArray. Also
terminates the Pylon runtime and should close the cameras properly, so
that they can be opened by another Gig-e client program."
  (cams :pointer))
(cffi:defcfun ("pylon_wrapper_factory" factory) (:pointer :void)
  "Return a transport level factory. Its return value is treated as an
opaque pointer and only necessary as the first argument of `create`.")
(cffi:defcfun ("pylon_wrapper_create" %create) (:pointer :void)
  (factory :pointer)
  (max-cameras :unsigned-int))

(defun create (factory max-cameras)
  "Given a transport level factory open `n` cameras. Error messages
and the full names of the cameras are printed on stdout.  Returns an
opaque pointer to a CInstantCameraArray which needs to be given as a
handle to all other functions that access the cameras in some way."
  #+sbcl
  (sb-int:with-float-traps-masked (:invalid)
    (%create factory max-cameras))
  #-sbcl
  (%create factory max-cameras))

(cffi:defcfun ("pylon_wrapper_start_grabbing" start-grabbing) :void
  "Start acquisition on all cameras of the handle.

Example:
```common-lisp
(progn
  (defparameter *cams* (pylon:create (pylon::factory) 3) \"Handle to multiple Pylon cameras.\")
  (unwind-protect 
      (progn
	(pylon:start-grabbing *cams*)
	(let ((th (sb-thread:make-thread 
		   #'(lambda ()
		       (loop for i below 100 do
			     (multiple-value-bind (cam success-p w h framenr timestamp) 
				 (pylon::grab-store *cams* fds))))
		   :name \"camera-acquisition\")))
	  (sleep .001)
	  (sb-thread:join-thread th)))
    (pylon:stop-grabbing *cams*)))
```
"
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
  "=> (values cam success-p w h frame-nr)

OBSOLETE: This wrapper doesn't do the conversion from MONO12P. Use
GRAB-CDF or GRAB-SF instead.

Copies one acquired image into an array `buf` of (unsigned-byte
8). The length of the array BUF must be at least `w*h`.  The image data
originates from one of the cameras in the handle CAMS as indicated by
the camera index CAM of the return values. The returned values `w` and
`h` indicate the dimensions of the returned image data in `buf`.

In case of an error, all four return values are -1."
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
  "=>  (values cam success-p w h frame-nr) 

Like GRAB-SF but converts the acquired into (complex
double-float)."
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

(cffi:defcfun ("pylon_wrapper_grab_sf" %grab-sf) :void
  (cams :pointer)
  (ww :int)
  (hh :int)
  (buf (:pointer :float))
  (camera (:pointer :int))
  (success-p (:pointer :int))
  (w (:pointer :int))
  (h (:pointer :int))
  (image-nr :pointer)
  (timestamp :pointer))

(defun grab-sf (cams buf)
  "=> (values cam success-p w h imagenr timestamp)

Copies one acquired image into an array `buf` which should be a 2D
array of single-float. With dimensions being at least corresponding to
the width and height of the currently used region of interst. My
wrapper code converts the 12-bit packed mono format to single-float. 

When using multiple cameras the image data originates from one of
these and the return value CAM indicates which one.

GRAB-SF also returns the width and height of the acquired image data,
allowing to use this function with a larger than necessary array
BUF. This comes handy when several cameras with different regions of
interest are to be read out. A single array with size to store the
largest image is sufficient in this case.

The IMAGENR and TIMESTAMP indicate acquisition time of the image.
This code allows to reset the counter on the camera:
```common-lisp
(pylon::command-execute CAMS 0 \"GevTimestampControlReset\")
```

The data can be fouriertransformed using FFTW:RFTF.

In case of an error, all four return values are -1."
  (declare (type (or (array single-float 2)
		     (simple-array single-float 2)) buf))
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
				    (imagenr :int64)
				    (timestamp :int64))
	  (%grab-sf cams w h bufp
		 cam success-p wout hout imagenr timestamp)
	  (values (cffi:mem-ref cam :int)
		  (if (= (cffi:mem-ref success-p :int) 1) t nil)
		  (cffi:mem-ref wout :int)
		  (cffi:mem-ref hout :int)
		  (cffi:mem-ref imagenr :int64)
		  (cffi:mem-ref timestamp :int64)
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
  "Grab one image and write it into one of the file descriptors. 

Example usage:

```common-lisp
(let* ((fda (loop for i below 3 collect (open (format nil \"/media/sdd3/b/cam~d\" i)
					      :direction :output
					      :if-exists :supersede)))
       (fds (mapcar #'sb-sys:fd-stream-fd fda)))
  (defparameter *fact* (pylon::factory) \"Handle to Factory, which is needed for call to PYLON:CREATE.\")
  (defparameter *cams* (pylon:create *fact* 3) \"Handle to multiple Pylon cameras.\")
  (unwind-protect 
      (progn
	(dotimes (i 3) ;; reset timers on the cameras
	  (pylon::command-execute *cams* i \"GevTimestampControlReset\"))
	(pylon:start-grabbing *cams*)
	(let ((th (sb-thread:make-thread 
		   #'(lambda ()
		       (loop for i below 100 do
			     (multiple-value-bind (cam success-p w h framenr timestamp) 
				 (pylon::grab-store *cams* fds))))
		   :name \"camera-acquisition\")))
	  (sleep .001)
	  (sb-thread:join-thread th)))
    (pylon:stop-grabbing *cams*))
  (mapcar #'close fda))
```
"
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
  "=> int

Return the maximum value of a Genicam integer node. The parameter
`node` is a string as defined by the Genicam standard. The return
value is an integer.

example: 
```common-lisp
(pylon:get-max-i *cams* 0 \"Width\")
```
"
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_get_min_i" get-min-i) :int
  "=> int

Return the minimum value of a Genicam integer node. The parameter
`node` is a string as defined by the Genicam standard. 

example: 
```common-lisp
(pylon:get-min-i *cams* 0 \"Width\")
```
"
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_get_inc_i" get-inc-i) :int
  "=> int

Return the increment value of a Genicam integer node. The parameter
`node` is a string as defined by the Genicam standard. 

example: 
```common-lisp
(pylon:get-inc-i *cams* 0 \"Width\")
```
"
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
  "=> single-float

Return the current value of a Genicam float node. The parameter
`node` is a string as defined by the Genicam standard. 

example: 
```common-lisp
(pylon:get-value-f *cams* 0 \"ResultingFrameRateAbs\")
```
"
  (%get-value-f cams cam node (if verify 1 0) (if ignore-cache 1 0)))
(defun get-value-b (cams cam node &optional (verify nil) (ignore-cache nil))
  "=> int

Return the current value of a Genicam boolean node. The parameter
`node` is a string as defined by the Genicam standard. 

example: 
```common-lisp
(pylon:get-value-b *cams* 0 \"AcquisitionFrameRateEnable\")
```
"
  (%get-value-b cams cam node (if verify 1 0) (if ignore-cache 1 0)))
(defun get-value-i (cams cam node &optional (verify nil) (ignore-cache nil))
  "=> int

Return the current value of a Genicam integer node. The parameter
`node` is a string as defined by the Genicam standard. The return
value is an integer.

example: 
```common-lisp
(pylon:get-inc-i *cams* 0 \"Width\")
```
"
  (%get-value-i cams cam node (if verify 1 0) (if ignore-cache 1 0)))

(cffi:defcfun ("pylon_wrapper_set_value_i" set-value-i) :void
  "Set the value of a Genicam integer node."
  (cams :pointer)
  (cam :int)
  (node :string)
  (value :int))

(cffi:defcfun ("pylon_wrapper_get_symbolics_e" get-symbolics-e) :int
  "Prints all the possible strings of a Genicam Enumeration node to
stdout.  The parameter `node` is a string (e.g. \"TriggerMode\",
\"PixelFormat\")."
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_set_value_e" set-value-e) :void
  "Set a Genicam Enumeration node by using an integer identifier."
  (cams :pointer)
  (cam :int)
  (node :string)
  (value :int))

(cffi:defcfun ("pylon_wrapper_get_value_e" get-value-e) :int
  "=> int

Get a Genicam Enumeration node by using an integer identifier."
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_command_isdone" command-isdone) :int
  "Check if a command that has been issued for execution has finished.

Example:
```common-lisp
(pylon::command-execute *cams* 1 \"ClearLastError\")
(pylon::command-isdone *cams* 1 \"ClearLastError\")
```
"
  (cams :pointer)
  (cam :int)
  (node :string))

(cffi:defcfun ("pylon_wrapper_command_execute" command-execute) :int
  "Execute a command on the device. If it is finished can be checked with COMMAND-ISDONE

Example:
```common-lisp
(pylon::command-execute *cams* 1 \"ClearLastError\")
```
"
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
  "Issue Open command on all the cameras in the CInstantCameraArray hande."
  (cams :pointer))

(cffi:defcfun ("pylon_wrapper_cams_close" cams-close) :void
  "Issue Close command on all the cameras in the CInstantCameraArray hande."
  (cams :pointer))

(cffi:defcfun ("pylon_wrapper_cam_open" cam-open) :void
  "Issue the Open command only for the camera with index `cam` in the
CInstantCameraArray `handle`. If `cam` is bigger than the number of
available cameras, nothing is done."
  (cams :pointer)
  (cam :int))

(cffi:defcfun ("pylon_wrapper_cam_close" cam-close) :void
  "Issue the Close command only for the camera with index `cam` in the
CInstantCameraArray `handle`. If `cam` is bigger than the number of
available cameras, nothing is done."
  (cams :pointer)
  (cam :int))

(cffi:defcfun ("pylon_wrapper_cam_get_serial_number" cam-get-serial-number) :string
  "Returns the uniq serial number of a camera as a string,
e.g. `21433566`."
  (cams :pointer)
  (cam :int))
(cffi:defcfun ("pylon_wrapper_cam_get_full_name" cam-get-full-name) :string
  "Returns the full name of the camera as a string, e.g. `Basler
acA1920-25gm#00305315DFDE#192.168.5.102:3956`"
  (cams :pointer)
  (cam :int))

(cffi:defcfun ("helper_subtract_bg_multiply_window" helper-subtract-bg-multiply-window) :void
  (a :pointer)
  (bg :pointer)
  (win :pointer)
  (n :int))

(cffi:defcfun ("helper_extract_csf" %helper-extract-csf) :void
  (in :pointer)
  (out :pointer)
  (x :int)
  (y :int)
  (iw :int)
  (ih :int)
  (w :int)
  (h :int))


