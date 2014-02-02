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

;; i can make ccl open the function database by applying db-functions:
(ccl::db-functions (first *my-dll-header-list*))

;; we can then use cdb-enumerate-keys to find all possible ffi functions:
(ccl::cdb-enumerate-keys (ccl::db-functions (first *my-dll-header-list*)))

;; i want to do this for all elements of *my-dll-header-list*
(reduce #'append
	(mapcar #'(lambda (x)
		    (ccl::cdb-enumerate-keys (ccl::db-functions x)))
		*my-dll-header-list*))

;; now, get rid of global variables. also prepend #_
(defun get-all-ffi-function-names ()
 (let* ((my-dll-header (ccl::ftd-dirlist ccl::*parse-ffi-target-ftd*))
	(my-dll-header-list (let ((res nil))
			      (ccl::do-dll-nodes (n my-dll-header)
				(push n res))
			      (reverse res))))
   (mapcar #'(lambda (x) (concatenate 'string "#_|" x "|"))
	   (reduce #'append
		   (mapcar #'(lambda (x)
			       (ccl::cdb-enumerate-keys (ccl::db-functions x)))
			   my-dll-header-list)))))
#+nil
(get-all-ffi-function-names)


;; now i try to understand swank
;; defun all-completions (prefix package) calls tokenize symbol
;; multiple-value-bind (name pname intern) (tokenize-symbol prefix)
(swank::tokenize-symbol "cffi:sy")
;; => "sy", "cffi", NIL
(swank::tokenize-symbol "cffi::sy")
;; => "sy", "cffi", T
(swank::all-completions "cffi:defc" "cffi")
;; => ("cffi:defcallback" "cffi:defcenum" "cffi:defcfun" "cffi:defcstruct" "cffi:defctype" "cffi:defcunion" "cffi:defcvar")
(swank::all-completions "defc" "cffi")
;; => ("defcallback" "defcenum" "defcfun" "defcfun-helper-forms" "defclass" "defconstant" "defcstruct" "defctype" "defctype*" "defcunion" "defcvar")

;; this is the original definition of all-completions, i'm going to add a check for ffi functions
;; (in-package :swank)
;; (defun all-completions (prefix package)
;;   (multiple-value-bind (name pname intern) (tokenize-symbol prefix)
;;     (let* ((extern (and pname (not intern)))
;; 	   (pkg (cond ((equal pname "") keyword-package)
;;                       ((not pname) (guess-buffer-package package))
;;                       (t (guess-package pname))))
;; 	   (test (lambda (sym) (prefix-match-p name (symbol-name sym))))
;; 	   (syms (and pkg (matching-symbols pkg extern test)))
;;            (strings (loop for sym in syms
;; 		       for str = (unparse-symbol sym)
;; 		       when (prefix-match-p name str) ; remove |Foo|
;; 		       collect str)))
;;       (format-completion-set strings intern pname))))


;; search for a prefix in all ffi function names:
(loop for s in (get-all-ffi-function-names) 
   when (prefix-match-p "#_|fprin" s) collect s)
;; => ("#_|fprintf|")


;; i will append these results to the slime function:
(in-package :swank)
(defun all-completions (prefix package)
  (multiple-value-bind (name pname intern) (tokenize-symbol prefix)
    (let* ((extern (and pname (not intern)))
	   (pkg (cond ((equal pname "") keyword-package)
                      ((not pname) (guess-buffer-package package))
                      (t (guess-package pname))))
	   (test (lambda (sym) (prefix-match-p name (symbol-name sym))))
	   (syms (and pkg (matching-symbols pkg extern test)))
           (strings (append
		     (loop for sym in syms
			for str = (unparse-symbol sym)
			when (prefix-match-p name str) ; remove |Foo|
			collect str)
		     (loop for s in (get-all-ffi-function-names) 
			when (prefix-match-p name s) collect s))))
      (format-completion-set strings intern pname))))

;; this works now:
(swank::all-completions "#_|fpr" "cffi")

;; unfortunately, it doesn't work in my current slime. i would have
;; hoped that i can just redefine this particular function without
;; restarting the lisp.

;; turns out, this only works, if i disable slime-fancy in quicklisp/slime-helper.el
;; this code was added to swank.lisp
;; #+ccl
;; (require :parse-ffi)
;; #+ccl
;; (defun get-all-ffi-function-names ()
;;  (let* ((my-dll-header (ccl::ftd-dirlist ccl::*parse-ffi-target-ftd*))
;; 	(my-dll-header-list (let ((res nil))
;; 			      (ccl::do-dll-nodes (n my-dll-header)
;; 				(push n res))
;; 			      (reverse res))))
;;    (mapcar #'(lambda (x) (concatenate 'string "#_" x))
;; 	   (reduce #'append
;; 		   (mapcar #'(lambda (x)
;; 			       (ccl::cdb-enumerate-keys (ccl::db-functions x)))
;; 			   my-dll-header-list)))))

;; (defun all-completions (prefix package)
;;   (multiple-value-bind (name pname intern) (tokenize-symbol prefix)
;;     (let* ((extern (and pname (not intern)))
;; 	   (pkg (cond ((equal pname "") keyword-package)
;;                       ((not pname) (guess-buffer-package package))
;;                       (t (guess-package pname))))
;; 	   (test (lambda (sym) (prefix-match-p name (symbol-name sym))))
;; 	   (syms (and pkg (matching-symbols pkg extern test)))
;;            (strings (append
;; 		     (loop for sym in syms
;; 			for str = (unparse-symbol sym)
;; 			when (prefix-match-p name str) ; remove |Foo|
;; 			collect str)
;; 		     #+ccl (loop for s in (get-all-ffi-function-names) 
;;                               when (prefix-match-p name s) collect s))))
;;       (format-completion-set strings intern pname))))



;; i think i will instead try to modify swank-fuzzy.lisp
;; i read through this but it's too complicated.

;; what i really would like to have as well is the argument
;; list. unfortunately, ffigen only stores the types of the arguments
;; and drops their names. the names are very useful, when working with
;; most c libraries.
