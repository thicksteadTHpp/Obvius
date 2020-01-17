;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: lucid-hacks.lisp
;;;  Author: Heeger
;;;  Description: Lucid dependent extensions to common-lisp
;;;  Creation Date: 9/92
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;changes: tho 2016-08-08
;;remove quote before package name
;;removed dependencies from lcl package
;; use cl-fad for file operations
;; use asdf-utils:run-program
;;use cffi for foreign pointers
;;use slime swank for arglist of functions

(in-package obvius)
(export '())

;;; Functions are listed by the lucid-independent source files that use them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Setf iref defined in image.lisp and bit-image.lisp is
;;; lucid-dependent.  These methods must be defined in those files,
;;; after the image and bit-image classes are defined.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Used by obvius-init and obvius-window-init
;;; tho: don't know what this does
;; turned it off
(defun environment-variable (thing)
  (declare (ignore thing)))
;;  (lcl:environment-variable thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quit ())
;;  (lcl:quit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Used to be defined in misc.lisp.

;;; ***  This is supposed to be defined in CLOS, but is currently not there!
;;; Macro which replaces symbols in body by s-expressions.  
#|
(defmacro symbol-macrolet (sym-list . body)
  (cons 'progn
	(mapcar #'(lambda (body-sexpr) 
		    (mapcar-tree #'(lambda (x) (or (cadr (assoc x sym-list))
						   x))
				 (list body-sexpr)))
		body)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Used by control-panel (and probably elsewhere)
;;[tho] 2016 this is a hack
;; use swank backend

(defun arglist (func)
  (error "function arglist Not implemented"))
  ;;(swank/backend:arglist func))
  ;;(lcl::arglist func))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Used by fileio.lisp
;;tho 2016-08-08 changed to use cl-fad

(defun create-directory (path)
  (cl-fad::ensure-directories-exist path))

  ;; (let ((path-string (namestring path)))
  ;;   (LCL:run-program "mkdir" :arguments (list path-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Used by hardcopy.lisp

;;; Both paths are strings (e.g., using namestring or format nil)
;;;tho 2016-08-08
;;changed to use cl-fad for file operations

(defun copy-file (path1 path2)
  (cl-fad::copy-file path1 path2))

;;  (LCL:run-program "cp" :arguments (list path1 path2)))


;;TODO use asdf-utils:run-program
(defun ship-to-printer (path printer)
  (uiop:run-program (list "-c" (format nil *print-command-string* path printer)) :output T))

  ;; (with-status-message "Printing postscript file"
  ;;   (LCL:run-program "sh" :wait nil :arguments 
  ;; 		     (list "-c" (format nil *print-command-string* path printer))))
  ;;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Used by memory.lisp

;;; Careful not to do extra consing in here!


;;;tho 2016-08-08 for now switched off;
;; maybe use static arrays for that
;;on sbcl we can used pinned arrays
;;this is only called once im memory.lisp
;;static vaectors may be an portable option
(defun make-static-array (size type)
  (let ((result (static-vectors:make-static-vector size :element-type type)))
;;    (trivial-garbage:finalize result (lambda ()
;;				       (vom:info "[gc] remove static array of size ~d" size)))
    result))
  


;;;(static-vectors:make-static-vector size :element-type type))
  
  
;; mcl-solution.->  (make-array size :element-type type))

  ;; (let ((total-size (if (listp size) (reduce #'* size) size)))
  ;;   (let ((static (static-vectors:make-static-vector total-size :element-type type)))
  ;;     (make-array <size :element-type type :displaced-to static))))

  ;; (LCL:with-static-area
  ;;     (make-array size :element-type type)))



(defun static-array-p (arr)
  (allocated-array-p arr))
   
;;  (and (arrayp arr) (LCL:stationary-object-p arr)))

(defun displaced-array-p (arr)
  (array-displacement arr))
  
;;  (lcl:displaced-array-p arr))


;; tho TODO find a solution
(defmacro with-scheduling-inhibited (&body body)
  `(progn
     ,@body))

;;  `(LCL:with-scheduling-inhibited ,@body))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;TODO: use bordeaux-threasd fot that
;; tho 2016-08-08
;;; Multiprocessing stuff

;;; Only allow one process at a time to access the pane.  Nested calls
;;; to with-locked-pane to this by the same process are ok.  Very very
;;; ity bitty chance of a collision bug here between the time the lock
;;; is read and when it is set.  **** BUG: this may cause the
;;; mouse process to go into a wait state.  We should probably start
;;; another process to execute immediate mouse events.
;; (defmacro with-locked-pane (pane . body)
;;   (let ((the-pane (gensym))
;; 	(original-lock (gensym))
;; 	(original-process (gensym)))
;;     `(let* ((,the-pane ,pane)
;; 	    (,original-process LCL:*current-process*)
;; 	    (,original-lock (locked ,the-pane)))
;;       (cond ((not ,original-lock) (setf (locked ,the-pane) ,original-process))
;; 	    ((not (eq ,original-lock ,original-process))
;; 	     ;; Wait until pane becomes unlocked by the process that locked it.
;; 	     (status-message "Waiting for locked pane ...")
;; 	     (LCL:process-wait 
;; 	      "Waiting for locked pane"
;; 	      #'(lambda () (and (not (locked ,the-pane))
;; 				(setf (locked ,the-pane) ,original-process))))))
;;       (unwind-protect
;; 	   (progn ,@body)
;; 	;;CURRENT lock created by this process so undo it!
;; 	(when (not (eq ,original-lock ,original-process)) 
;; 	  (setf (locked ,the-pane) nil)))))) 
;;[THO]this is a hack
(defmacro with-locked-pane (pane &body body)
  (declare (ignore pane))
  `(progn
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Used by fileio.lisp

(defun directory-p (path)
  (let ((path-string (namestring path)))
    (probe-file (pathname (concatenate 'string path-string "/.")))))

; Does the thing right for both Symbolics and Sun pathname strings.
(defun directory-path (path)
  (let ((path-string (namestring path)))
    (pathname (concatenate 'string path-string "/"))))

(defun trim-right-delimiter (path)
  (let ((path-string (namestring path)))
    (pathname (string-right-trim "/>" path-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Used by conversion.lisp (could use obvius functions obv::array-read and
;;; obv::array-write instead).

;;tho 2016-08-08 commented out

(defun write-array (&rest args)
  (declare (ignore args))
  (error "write-array not implemented"))

;;  (apply 'lcl:write-array args))

(defun read-array (&rest args)
  (declare (ignore args))
  (error "read-array not implemented"))

;;  (apply 'lcl:read-array args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; stepit.lisp.

;;;tho 2016-08-08 use cffi for that
;;TODO
;;let's see wwht this is good for
;; we can use cffi's callback
;;; Need to acquire a function pointer to the foreign callable.
(defun foreign-function-pointer (symbol)
  (error "foreign pointers not implmentded"))
  ;; (let* ((name (string-downcase (symbol-name symbol)))
  ;; 	 (address (LCL:foreign-variable-address name)))
  ;;   (LCL:make-foreign-pointer :address address)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
