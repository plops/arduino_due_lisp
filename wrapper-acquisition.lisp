(in-package :arv)
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
    (loop for e in (append *basler-acquisition-modes* *photonfocus-acquisition-modes*) do
	 (destructuring-bind (mode . code) e
	   (declare (ignore code))
	   (setf res (adjoin mode res))))
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



(defmethod pop-block-copy-push-buffer ((cam camera) &key out (use-dark t))
  (ensure-no-threads-waiting-for-buffer cam)
  (ensure-at-least-one-buffer-in-stream cam)
  (let ((b (timeout-pop-buffer-blocking cam 10000))
	(dark1 (when (and use-dark (dark-image cam))
		 (let ((d (dark-image cam)))
		   ; (declare (type (simple-array (unsigned-byte 16) 2) d))
		   (assert (equal (list (aoi-height cam) (aoi-width cam))
				  (array-dimensions d)))
		      (make-array (reduce #'* (array-dimensions d))
				  :element-type (array-element-type d)
				  :displaced-to d)))))
    (declare (type (or null (array (unsigned-byte 16) 1)) dark1))
    (loop while (or (cffi:null-pointer-p b)
		    (and (not (cffi:null-pointer-p b))
			 (/= #$ARV_BUFFER_STATUS_SUCCESS (pref b #>ArvBuffer.status)))
		    (and (not (cffi:null-pointer-p b))
			 (not (pref b #>ArvBuffer.data))))
	 for i from 0 below 1000 do
	 (when (and (/= i 0) (= 0 (mod i 10)))
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
	  (declare
	   (optimize (speed 3))
	   (type (simple-array (unsigned-byte 16) 2) a)
	   (type (array (unsigned-byte 16) 1) a1)
	   )
	  (cond
	    ((string= (pixel-format cam) "Mono8")
	     (dotimes (i (min (length a1) np))
	       (setf (aref a1 i) (* 256 (%get-unsigned-byte data i)))))
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


(defmethod pop-block-copy-push-buffer-mono12p-cdf ((cam camera) out)
  (declare (type (array double-float 3) out))
  (ensure-no-threads-waiting-for-buffer cam)
  (ensure-at-least-one-buffer-in-stream cam)
  (let ((b (timeout-pop-buffer-blocking cam 10000)))
    (loop while (or (cffi:null-pointer-p b)
		    (and (not (cffi:null-pointer-p b))
			 (/= #$ARV_BUFFER_STATUS_SUCCESS (pref b #>ArvBuffer.status)))
		    (and (not (cffi:null-pointer-p b))
			 (not (pref b #>ArvBuffer.data))))
       for i from 0 below 1000 do
	 (when (and (/= i 0) (= 0 (mod i 10)))
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
    (assert (= (aoi-width cam) (array-dimension out 1)))
    (assert (= (aoi-height cam) (array-dimension out 0)))
    (assert (= 2 (array-dimension out 2))) ;; double-float array with
					   ;; 2 dimensions in the fast
					   ;; dimension
    (assert (string= (pixel-format cam) "Mono12Packed"))
    (prog1
	(let* ((a out)
	       (n (array-total-size out))
	       (np (get-payload cam))
	       (a1 (make-array n :element-type 'double-float
			       :displaced-to out))
	       (data (pref b #>ArvBuffer.data)))
	  (declare
	   (optimize (speed 3))
	   (type (array double-float 3) a)
	   (type (array double-float 1) a1))
	  (loop for byte below np by 3
	     and short from 0 below n by 2
	     do
	     ;; 3 bytes in the data stream correspond to two data
	     ;; elements: AB CD EF -> ABD, EFC 
	     ;; A is the most signifcant nibble
	       (let ((ab (%get-unsigned-byte data byte))
		     (c (ldb (byte 4 0) (%get-unsigned-byte data (+ 1 byte))))
		     (d (ldb (byte 4 4) (%get-unsigned-byte data (+ 1 byte))))
		     (ef (%get-unsigned-byte data (+ 2 byte))))
		 (setf (aref a1 (* 2 short)) (* 1d0 (ash (+ (ash ab 4) d) 4))
		       (aref a1 (* 2 (1+ short))) (* 1d0 (ash (+ (ash ef 4) c) 4)))))
	  a)
	(push-buffer cam b))))

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
  (start-acquisition c)
  (prog1
      (pop-block-copy-push-buffer c :use-dark use-dark)
    (stop-acquisition c)))

(defmethod acquire-continuous-images ((c camera) &key (use-dark t) (fun #'(lambda (x)
									    (declare (ignore x)) nil)))
  "continuously acquire image and pass each to the function fun. acquisition stops when fun returns non-t value."
  (start-acquisition c)
  (let ((do-acquire-p t))
    (loop while do-acquire-p do
	 (setf do-acquire-p (funcall fun (pop-block-copy-push-buffer c :use-dark use-dark)))))
  (stop-acquisition c))


(defmethod acquire-image-using-full-range ((c camera) &key (use-dark t))
  "vary exposure time to keep gray values within 40000 .. 60000. note: if a dark image is subtracted i introduce an offset of 100. in particular for the cmos camera the dark images can have quite high values."
  (let* ((im (acquire-single-image c :use-dark use-dark))
	 (ma (.max im))
	 (dark-max (if use-dark 
		       (.max (dark-image c))
		       0))
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
