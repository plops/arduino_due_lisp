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
  (ccl::cdb-open "/home/martin/src/ccl/x86-headers64/libc/functions.cdb"))
;; using cdb-enumerate-keys, all function names can be obtained (as
;; strings) and db-lookup-function gives access to function arguments
;; and return type
(ccl::cdb-enumerate-keys *fun*)
;; this is a part of the strings that are returned:
;; ... "nis_ping_3_svc" "__roundl" "__y1f" "fchownat"
;; "__rpc_thread_createerr" "fexecve" "getpwent" "htons"
;; "nfsproc_rename_2_svc" "wscanf" "atan2f" "__powf" "wcsnlen"
;; "__lround" "__pow10f" "la_preinit" "getmsg" "alloca"
;; "td_thr_getgregs" "sigqueue" "crypt" "getpgid" "truncate64"
;; "daemon" "pipe" "yp_unbind" "ypproc_domain_nonack_2" ...

(ccl::db-lookup-function *fun* "qsort")
;; => #S(CCL::EXTERNAL-FUNCTION-DEFINITION :ENTRY-NAME "qsort"
;; :ARG-SPECS (:ADDRESS :SIZE_T :SIZE_T :__COMPAR_FN_T) :RESULT-SPEC
;; :VOID :MIN-ARGS 4)

;; compare this to the definition from the /usr/include/stdlib.h:

;; /* Sort NMEMB elements of BASE, of SIZE bytes each,
;;    using COMPAR to perform the comparisons.  */
;; extern void qsort (void *__base, size_t __nmemb, size_t __size,
;;                    __compar_fn_t __compar) __nonnull ((1, 4));

;; and the ffi definition ccl/x86-headers64/libc/C/usr/include/stdlib.ffi:
;; (function ("/usr/include/stdlib.h" 762)
;;  "qsort"
;;  (function
;;   ((pointer (void ())) (typedef "size_t") (typedef "size_t") (typedef "__compar_fn_t") )
;;   (void ())) (extern))

;; apparently the attribute __nonnull((1,4)) got lost. i would guess
;; this indicates that the start of the array (__base) and the
;; comparison function (__compar) should not be null

;; the function type definition of __compar_fn_t loses the __const:

;; typedef int (*__compar_fn_t) (__const void *, __const void *);

;; (type ("/usr/include/stdlib.h" 742)
;;  "__compar_fn_t"
;;  (pointer (function
;;   ((pointer (void ())) (pointer (void ())) )
;;   (int ()))))


;; let's have a look at a function with a variable number of
;; arguments (snprintf):

;; this is the definition in stdio.h:

;; __BEGIN_NAMESPACE_C99
;; /* Maximum chars of output to write in MAXLEN.  */
;; extern int snprintf (char *__restrict __s, size_t __maxlen,
;;                      __const char *__restrict __format, ...)
;;      __THROW __attribute__ ((__format__ (__printf__, 3, 4)));

;; extern int vsnprintf (char *__restrict __s, size_t __maxlen,
;;                       __const char *__restrict __format, _G_va_list __arg)
;;      __THROW __attribute__ ((__format__ (__printf__, 3, 0)));
;; __END_NAMESPACE_C99

;; again the attributes are lost. i think they help the c compiler to
;; check if the supplied arguments correspond to the format string

;; more importantly, the variable number of arguments don't seem to be
;; present in the ffi definition. but i wouldn't know how this should
;; be written, anyway.

(ccl::db-lookup-function *fun* "snprintf")
;; => #S(CCL::EXTERNAL-FUNCTION-DEFINITION :ENTRY-NAME "snprintf"
;; :ARG-SPECS (:ADDRESS :SIZE_T :ADDRESS :VOID) :RESULT-SPEC
;; :SIGNED-FULLWORD :MIN-ARGS 4)

(ccl::db-lookup-function *fun* "vsnprintf")
;; => #S(CCL::EXTERNAL-FUNCTION-DEFINITION :ENTRY-NAME "vsnprintf"
;; :ARG-SPECS (:ADDRESS :SIZE_T :ADDRESS :ADDRESS) :RESULT-SPEC
;; :SIGNED-FULLWORD :MIN-ARGS 4)


;; a search for 'variable' and 'arg' in the ccl
;; code points to compiler/X86/X8664/x8664-vinsns.lisp but i'm not
;; sure how they use this

;; (define-x8664-vinsn save-lisp-context-variable-arg-count (()
;;                                                           ()
;;                                                           ((temp :u64)))
;;   (movl (:%l x8664::nargs) (:%l temp))
;;   (subq (:$b (* $numx8664argregs x8664::node-size)) (:%q temp))
;;   (jle :push)
;;   (movq (:%q x8664::rbp) (:@ x8664::node-size (:%q x8664::rsp) (:%q temp)))
;;   (leaq (:@ x8664::node-size (:%q x8664::rsp) (:%q temp)) (:%q x8664::rbp))
;;   (popq  (:@ 8 (:%q x8664::rbp)))
;;   (jmp :done)
;;   :push
;;   (pushq (:%q x8664::rbp))
;;   (movq (:%q x8664::rsp) (:%q x8664::rbp))
;;   :done)






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
