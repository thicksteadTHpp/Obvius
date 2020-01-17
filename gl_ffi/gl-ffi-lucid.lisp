;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  ffi-lucid.lisp
;;;
;;;  Foreign function interface routines
;;;  (Lucid Implementation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package 'gl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *define-foreign-function* t)
(defparameter *print-foreign-function-definition* nil)
(defparameter *generate-c-macros-where-necessary* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(export '(load-gl-foreign-interface
	  malloc-foreign-string
	  def-exported-constant
	  def-exported-foreign-synonym-type
	  def-exported-foreign-struct
	  def-exported-f-function))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load GL and other libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-gl-foreign-interface ()
  (let* ((libraries
	  (list "-lgl" "-lfm" "-lX11" "-lsun" "-lm" "-lc")))
    (format t ";;; Loading libraries ~{~A ~}...~%" libraries)
    (load-foreign-libraries nil libraries)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous useful functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun malloc-foreign-string (s)
  (if (null s) (LCL:make-foreign-pointer :address 0 :type '(:pointer :character))
      (let ((fs (LCL:malloc-foreign-pointer :type `(:pointer (:array :character (,(1+ (length s))))))))
	(setf (LCL:foreign-string-value fs) s
	      (LCL:foreign-pointer-type fs) '(:pointer :character))
	fs)))

(defun c-id-to-lisp-id (string)
  (substitute #\- #\_ (string-upcase string) :start 1 :end (max 1 (1- (length string)))))

(defun lisp-symbol-to-c-id (symbol)
  (format nil "~A" (substitute #\_ #\- (string-downcase (string symbol)))))

(defmacro strcat (&rest strings)
  `(concatenate 'string ,@strings))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to define exported constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-exported-constant (c-string value)
  (let ((c-symbol (intern (c-id-to-lisp-id c-string))))
    `(progn
      (export '(,c-symbol))
      (defconstant ,c-symbol ,value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to define foreign types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *lisp-to-c-map* nil)
(defparameter *lisp-synonym-type-map* nil)

(defun record-c-type (lisp-symbol c-string)
  (if (null (assoc lisp-symbol *lisp-to-c-map*))
      (push (cons lisp-symbol c-string) *lisp-to-c-map*)
      (error "Lisp equivalent type not unique."))
  t)

(defun record-synonym-type (lisp-foreign-type syn-type)
  (let ((prev-syn-type (assoc syn-type *lisp-synonym-type-map* :test #'equal)))
    (if (null prev-syn-type)
	(push (cons lisp-foreign-type syn-type) *lisp-synonym-type-map*)
	(push (cons lisp-foreign-type (cdr prev-syn-type)) *lisp-synonym-type-map*)))
  t)

(defun get-c-type (lisp-symbol)
  (let ((c-type-string (cdr (assoc lisp-symbol *lisp-to-c-map*))))
    (if (stringp c-type-string) c-type-string
	(lisp-symbol-to-C-id lisp-symbol))))

(defun get-primitive-lisp-foreign-type (lisp-symbol)
  (cdr (assoc lisp-symbol *lisp-synonym-type-map*)))

(defmacro def-exported-foreign-synonym-type (c-string &rest args)
  (let ((c-symbol (intern (c-id-to-lisp-id c-string))))
    `(progn
      (export '(,c-symbol))
      (record-c-type (quote ,c-symbol) (quote ,c-string))
      (record-synonym-type (quote ,c-symbol) (quote ,@args))
      (LCL:def-foreign-synonym-type ,c-symbol ,@args))))

(defmacro def-exported-foreign-struct (c-string &rest args)
  (let ((c-symbol (intern (c-id-to-lisp-id c-string))))
    `(progn
      (export '(,c-symbol))
      (record-c-type (quote ,c-symbol) (quote ,c-string))
      (LCL:def-foreign-struct ,c-symbol ,@args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to define foreign functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *lucid-unsupported-types*
  '(
    ;; unsigned 8-bit types are not supported and we can't use 16-bit either,
    ;; so, we'll use integers.
    (:unsigned-8bit . :signed-32bit)
    
    ;; 16-bit types are not supported by Lucid, so we have to use 32-bit integers
    (:unsigned-16bit . :signed-32bit)
    (:signed-16bit . :signed-32bit)

    ;; single-floats aren't directly supported either, so we use double-float's
    (:single-float . :double-float)
    
    ))

(defun lucid-unsupported-type-p (arg)
  (cdr (assoc (get-primitive-lisp-foreign-type arg) *lucid-unsupported-types* :test #'equal)))

(defun replace-unsupported-type (arg)
  (let ((new-type (lucid-unsupported-type-p arg)))
    (if (null new-type) arg new-type)))

(defun c-macroify (c-name) (strcat c-name "_macro"))

(defun lisp-equiv-type-to-c-type (symbol)
  (let ((res-pair (assoc symbol *lisp-to-c-map*)))
    (if (consp res-pair)
	(cdr res-pair)
	(error "Unable to obtain equivalent c-type."))))

(defun c-typify (c-type c-arg)
  (cond ((symbolp c-type)
	 (strcat (get-c-type c-type) " " c-arg))
	((eq :pointer (first c-type))
	 (c-typify (second c-type) (strcat "*" c-arg)))
	((eq :array (first c-type))
	 (if (null (cddr c-type))
	     (strcat (c-typify (second c-type) c-arg) "[]")
	     (eval `(strcat ,(c-typify (second c-type) c-arg)
		     ,@(mapcar #'(lambda (dim) (format nil "[~d]" dim)) (third c-type))))))
	(t (error "Unable to make c-type string."))))
		 
(defun c-castify (c-type)
  (cond ((symbolp c-type)
	 (strcat "(" (get-c-type c-type) ")"))
	((eq :pointer (first c-type))
	 (strcat "(" (c-castify (second c-type)) " *)"))
	((eq :array (first c-type))
	 (if (null (cddr c-type))
	     (strcat "(" (c-castify (second c-type)) " [])")
	     (eval `(strcat "(" ,(c-castify (second c-type))
		     ,@(mapcar #'(lambda (dim) (format nil "[~d]" dim)) (third c-type))
		     ")"))))
	(t (error "Unable to make c-cast string."))))
	 


;;;
;;; This is a generic macro that will define, print and generate the
;;; required c-macros if necessary.  
;;;
(defmacro def-exported-c-function (name-and-options &optional (args nil))
  (when *define-foreign-function* (eval `(def-export-c-function ,name-and-options ,args)))
  (when *print-foreign-function-definition* (eval `(print-exported-c-function ,name-and-options ,args)))
  (when *generate-c-macros-where-necessary* (eval `(generate-c-macro-function ,name-and-options ,args))))



;;;
;;; This macro prints the definitions of the functions we define
;;;
(defmacro print-exported-c-function (name-and-options &optional (args nil))
  (let* ((name (if (consp name-and-options) (first name-and-options) name-and-options))
	 (c-name (if (and (consp name-and-options) (consp (assoc :name (cdr name-and-options))))
		     (second (assoc :name (cdr name-and-options))) name))
	 (lisp-name (intern (C-id-to-lisp-id name)))
	 (slots
	  (do ((i 0 (1+ i)) (args-list args (cdr args-list)) (ret-args-list nil))
	      ((null args-list) (reverse ret-args-list))
	    (push `(,(make-symbol (format nil "arg~d" i)) ,(replace-unsupported-type (car args-list)))
		  ret-args-list))))
    `(progn 
      (format t "(def-foreign-function ~s"
                '(,lisp-name (:name ,(if (some #'lucid-unsupported-type-p args) (c-macroify c-name) c-name))
		  ,@(if (consp name-and-options) (remove-if #'(lambda (option) (eq (car option) :name)) (cdr name-and-options)) nil)))
      (dolist (slot ',slots) (format t "~%~2T~s" slot))
      (format t ")~%~%"))))


;;;
;;; This macro defines the foreign function
;;;
(defmacro def-export-c-function (name-and-options &optional (args nil))
  (let* ((name (if (consp name-and-options) (first name-and-options) name-and-options))
	 (c-name (if (and (consp name-and-options) (consp (assoc :name (cdr name-and-options))))
		     (second (assoc :name (cdr name-and-options))) name))
	 (lisp-name (intern (C-id-to-lisp-id name)))
	 (slots
	  (do ((i 0 (1+ i)) (args-list args (cdr args-list)) (ret-args-list nil))
	      ((null args-list) (reverse ret-args-list))
	    (push `(,(make-symbol (format nil "arg~d" i)) ,(replace-unsupported-type (car args-list)))
		  ret-args-list))))
    `(progn
      (export '(,lisp-name))
      (def-foreign-function
	  (,lisp-name (:name ,(if (some #'lucid-unsupported-type-p args) (c-macroify c-name) c-name))
		  ,@(if (consp name-and-options)
			(remove-if #'(lambda (option) (eq (first option) :name)) (cdr name-and-options))
			nil))
	  ,@slots))))


;;;
;;; Use this macro to automatically generate the C-code for the macros
;;;
(defmacro generate-c-macro-function (name-and-options &optional (args nil))
  (when (or (some #'lucid-unsupported-type-p args)
	    (and (consp name-and-options) (lucid-unsupported-type-p (second (assoc :return-type (cdr name-and-options))))))
    (let* ((name (if (consp name-and-options) (first name-and-options) name-and-options))
	   (c-name (if (and (consp name-and-options) (consp (assoc :name (cdr name-and-options))))
		       (second (assoc :name (cdr name-and-options))) name))
	   (c-args (mapcar #'(lambda (arg) (cons arg (replace-unsupported-type arg))) args))
	   (return-type (if (and (consp name-and-options) (consp (assoc :return-type (cdr name-and-options))))
			    (second (assoc :return-type (cdr name-and-options))) 'void))
	   (c-return-type (cons return-type (replace-unsupported-type return-type))))
      
      `(progn
	;;; print original function prototype as a comment  
	(format t "/* original prototype: ~a(" (c-typify ',(car c-return-type) ,c-name))
	(unless ,(null c-args)
	  (format t "~a" (c-typify ',(caar c-args) "arg0"))
	  (do ((i 1 (1+ i)) (args-list ',(cdr c-args) (cdr args-list)))
	      ((null args-list))
	    (format t ", ~a" (c-typify (caar args-list) (format nil "arg~d" i)))))
	(format t ") */~%")

	;;; print macro function prototype
	(format t "~a(" (c-typify ',(cdr c-return-type) ',(c-macroify c-name)))
	(unless ,(null c-args)
	  (format t "~a" (c-typify ',(cdar c-args) "arg0"))
	  (do ((i 1 (1+ i)) (args-list ',(cdr c-args) (cdr args-list)))
	      ((null args-list))
	    (format t ", ~a" (c-typify (cdar args-list) (format nil "arg~d" i)))))
	(format t ")~%{~%")

	;;; make function call to original function
	(format t "~5T~a~a~a("
	 (if (eq  ',(cdr c-return-type) 'void) "" "return( ")
	 (if (not (equal ',(car c-return-type) ',(cdr c-return-type)))
	     (c-castify  ',(cdr c-return-type)) "")
	 ,c-name)
	(unless ,(null c-args)
	  (format t "~a arg0"
		  (if (not (equal ',(caar c-args) ',(cdar c-args)))
		      (c-castify ',(caar c-args)) ""))
	  (do ((i 1 (1+ i)) (args-list ',(cdr c-args) (cdr args-list)))
	      ((null args-list))
	    (format t ", ~a arg~d"
		    (if (not (equal (caar args-list) (cdar args-list)))
			(c-castify (caar args-list)) "")
		    i)))
	(format t "~a);~%}~%~%"
	 (if (eq  ',(cdr c-return-type) 'void) "" ") "))))))

	  
      

				     
				 

	 

