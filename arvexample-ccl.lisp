(load "/home/martin/quicklisp/setup.lisp")
(ql:quickload :cffi)
(require :parse-ffi)
(ccl::parse-standard-ffi-files :arv)
(ccl::parse-standard-ffi-files :v4l2)
(use-interface-dir :arv)
(open-shared-library "libaravis-0.4.so")
ccl::*shared-libraries*
ccl::*eeps* ;; hash table with external functions

(cffi:with-foreign-string (s "Fake")
  (#_arv_enable_interface s))

;; the following code opens a cdb file, a read-only data structure,
;; that ccl uses to store foreign function definitions. 
(defparameter *fun*
  (ccl::cdb-open "/home/martin/src/ccl/x86-headers64/arv/functions.cdb"))
;; using cdb-enumerate-keys, all function names can be obtained (as
;; strings) and db-lookup-function gives access to function arguments
;; and return type
(ccl::cdb-enumerate-keys *fun*)
(ccl::db-lookup-function *fun* "arv_camera_new")
;; ftd .. foreign type data, this contains a list of the directories
;; to search, can be expanded with use-interface-dir (which in turn
;; calls move-dll-nodes from compiler/dll-node.lisp)
ccl::*target-ftd*
ccl::*parse-ffi-target-ftd*
;; the interfaces are stored in the datastructure dll-header. it is a
;; doubly-linked list, therefore, i think it's not comfortable to look
;; at using the slime inspector
(defparameter *my-dll-header* (ccl::ftd-dirlist ccl::*parse-ffi-target-ftd*))
;; with this call we can see how many interfaces were loaded
(ccl::dll-header-length *my-dll-header*)
;; and using the internal function do-dll-nodes, we can collect them into a list
(defparameter *my-dll-header-list*
 (let ((res nil))
   (ccl::do-dll-nodes (n *my-dll-header*)
     (push n res))
   (reverse res)))


(#_arv_g_type_init)
(defparameter *cam* (#_arv_camera_new (cffi:null-pointer)))

(cffi:defcfun (%arv-camera-get-available-pixel-formats "arv_camera_get_available_pixel_formats")
    #_gint64)
