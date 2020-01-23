(defpackage #:lcl
  (:use #:cl #:cffi)
;;  (:import-from :obv obv-double-float obv-single-float)
  (:export
   :def-foreign-function
   :def-foreign-callable))


(format t "~&----->Setting up lcl-compat<---------~&")

(in-package :lcl)

;;(defctype :array :pointer)
(cffi:defctype :single-float :float)
(cffi:defctype :double-float :double)
(cffi:defctype :fixnum :int)
(cffi:defctype :unsigned-32bit :unsigned-long)
(cffi:defctype :signed-32bit :int32)
;;(defctype :string (:pointer :char))


(cffi:define-foreign-type obv-double-float ()
  ()
  (:actual-type :double))

(cffi:define-foreign-type obv-single-float ()
  ()
  (:actual-type :float))

(cffi:define-foreign-type lcl-compat-array (cffi::foreign-array-type)
    ;;  ((obv-foreign-array-type :reader obv-foreign-array-type :initarg :type))
    ((foreign-type :accessor foreign-type :initarg :foreign-type :initform nil)
     (copy-strategy :accessor copy-strategy :initarg :copy-strategy :initform :copy-in-out ));;:type (member :copy-in-out :copy-in :copy-out :no-copy)))
    (:actual-type :pointer))

(cffi:define-foreign-type lcl-compat-string ()
  ()
  (:actual-type :pointer))


(cffi:define-parse-method obv-double-float ()
  (make-instance 'obv-double-float))

(cffi:define-parse-method obv-single-float ()
  (make-instance 'obv-single-float))


;;returns cffi compatible types
;;or nil if type is nil

(defun corresponding-foreign-type (type)
  (when type
    (if  (listp type)
	 (progn
;;	   (vom:info "type: ~d ~d ~d" type (eq (car type) 'unsigned-byte) (second type))
	   (or  (and (eq (car type) 'unsigned-byte)
		     (case (second type)
		       (8 :uint8)
		       (16 :uint16)
		       (32 :uint32)
		       (64 :uint64)
		       (otherwise :uint)))
		(and (eq (car type) 'signed-byte)
		     (case (second type)
		       (8 :int8)
		       (16 :int16)
		       (32 :int32)
		       (64 :int64)
		       (otherwise :int)))
		(error "no corresponding type found")))
	 (case type
	   ((float :float single-float :single-float)  :float)
	   ((double-float :double double :double-float)  :double)
	   ((fixnum :fixnum) :int)
	   (bit  :int)
	   (otherwise :POINTER)))))


(defmethod cffi:translate-to-foreign (val (type obv-double-float))
  (cl:float val 1d0))

(defmethod cffi:translate-to-foreign (val (type obv-single-float))
  (cl:float val 1f0))




;; startegy is one of :copy-in :copy-out :copy-in-ou or :no-copy
(cffi:define-parse-method lcl-array (&optional type (strategy :copy-in-out))
  (make-instance 'lcl-compat-array
		 :element-type (or type 'integer) 
		 :dimensions nil
		 :foreign-type (if type (corresponding-foreign-type type))
		 :copy-strategy (if strategy strategy :copy-in-out)))

(cffi:define-parse-method lcl-string (&optional encoding)
  (declare (ignore encoding))
  (make-instance 'lcl-compat-string))

  ;; (cffi:define-parse-method :array (&optional type)
  ;;   (make-instance 'lcl-compat-array :element-type type :dimensions nil))

  ;; (cffi:define-parse-method :string (&optional encoding)
  ;;   (declare (ignore encoding))
  ;;   (make-instance 'lcl-compat-string))


;;[tho] 2016-08-18 changed to use static vectors
(defun get-static-array-pointer (array base-array)
  (static-vectors:static-vector-pointer base-array :offset (* (obv::total-displacement array)
							      (obv::sizeof (array-element-type array)))))
					
	
  
  ;; (let ((offset (obv::total-displacement array))
  ;; 	(foreign-size (obv::sizeof (array-element-type array))))
  ;;   (sb-sys:with-pinned-objects (base-array)
  ;;     (assert (<= offset (array-total-size base-array)))
  ;;     (cffi:inc-pointer (sb-sys:vector-sap
  ;; 			 (sb-ext:array-storage-vector base-array))
  ;; 			(* offset
  ;; 			   foreign-size)))))

;;returns base-array if array is in static-array-heap
;; or nil
(defun static-base-array (array)
  (let* ((lisp-array-type (array-element-type array))
	 ;;look if it is in the obvius static heaps
	 (static-heap (obv::find-heap-of-type lisp-array-type :if-not-found :warn))
	 (base-array (if static-heap (find (obv::undisplaced-parent array) (obv::static-heap-free-arrays-by-parent static-heap) :test #'eq))))
    base-array))

;;[THO] 2019-12-13 we dont' need nthis as we use expand-to-foreign-dyn
;;the second value determines if the pointer returned by this function
;; will be freed by free-translated-object
;; nil means dont' free
;; 
;;   ;;is this a typed array in the definiton of foreign function???
;;   (vom:info "[CFFI]translating array to foreign")
;;   (setf (copy-strategy type) :copy-in-out) ;;set back to original value
;;   (if (cffi:pointerp array)
;;       array
;;       (let* ((declared-foreign-type (cffi::element-type type))
;; 	     (lisp-array-type (array-element-type array))
;; 	     ;;look if it is in the obvius static heaps
;; 	     ;;(static-heap (obv::find-heap-of-type lisp-array-type :if-not-found :warn))
;; 	     (base-array (static-base-array array)));;(if static-heap (find (obv::undisplaced-parent array) (obv::static-heap-free-arrays-by-parent static-heap) :test #'eq))))
;; 	(cond (base-array  (progn
;; 			     (vom:info "[CFFI]found static array")
;; 			     (setf (copy-strategy type) :no-copy) ;;to prevent copying in and out
;; 			     (values (get-static-array-pointer array base-array) nil)))
;; 	      (lisp-array-type (progn
;; 				 (vom:info "[CFFI]translate conventional lisp array")
;; 				 (setf (slot-value type 'cffi::dimensions)
;; 				       (array-dimensions array)
;; 				       (slot-value type 'cffi::element-type)
;; 				       (array-element-type array))
;; 				 (vom:info "[CFFI]creating foreign array lisp-type: ~d correspond: ~d proposed type: ~d" (array-element-type array)
;; 					   (corresponding-foreign-type (array-element-type array)) (foreign-type type))
;; 				 (values (let ((this-foreign-type (or (foreign-type type)
;; 								      (corresponding-foreign-type (array-element-type array)))))
;; 					   (setf (foreign-type type) this-foreign-type)
;; 					   (vom:info "[CFFI]copy-strategy: ~d" (copy-strategy type))
;; 					   (ecase (copy-strategy type)
;; 					     ((:copy-in :copy-in-out) (copy-to-foreign-array array
;; 											     (array-total-size array)
;; 											     this-foreign-type))
;; 					     ((:copy-out :no-copy) (cffi:foreign-alloc this-foreign-type 
;; 										       :count
;; 										     (array-total-size array)))))
;; 				   T))) ;;t as a second value to indicate that we want to free the pointer after foreign-funcall
;; 	      (T (error "untyped lisp array or undeclared foreing poiner"))))))



;;;[tho] version from 2016-08-12
  ;; (vom:info "translating array to foreign")
  ;; (if (obv-foreign-array-type type)
  ;;     (progn
  ;; 	;;is this array typed in lisp
  ;; 	(if (not (eq T (array-element-type array)))
  ;; 	    (let* ((lisp-array-type (array-element-type array))
  ;; 		   ;;look if it is in the obvius static heaps
  ;; 		   (static-heap (obv::find-heap-of-type lisp-array-type :if-not-found :warn))
  ;; 		   (base-array (if static-heap (find (obv::undisplaced-parent array) (obv::static-heap-free-arrays-by-parent static-heap) :test #'eq)))
  ;; 		   (offset (obv::total-displacement array))
  ;; 		   (foreign-size (obv::sizeof lisp-array-type)))
  ;; 	      (if base-array
  ;; 		  (progn
  ;; 		    ;;now pin array and calculate offset
  ;; 		    (sb-sys:with-pinned-objects (base-array)
  ;; 		      (assert (<= offset (array-total-size base-array)))
  ;; 		      (cffi:inc-pointer (sb-sys:vector-sap
  ;; 					  (sb-ext:array-storage-vector base-array))
  ;; 					 (* offset
  ;; 					    foreign-size))))
  ;; 		  (error "no base array found")))
  ;; 	    (error "not a typed array")))
  ;;     (error "no type given to array in foreign function definition")))



;; (defmethod cffi:translate-from-foreign (pointer (type lcl-compat-array))
;;   (vom:info "[translate-from-foreign] array, strategy: ~d" (copy-strategy type))
;;   (ecase (copy-strategy type)
;;     ((:copy-in :no-copy) (progn (setf (copy-strategy type) :copy-in-out)
;; 				pointer))
;;     ((:copy-out :copy-in-out) (let* ((dims (slot-value type 'cffi::dimensions))
;; 				     (total-dim (if (listp dims) (apply #'* dims) dims)))
;; 				(copy-from-foreign-array pointer total-dim (foreign-type type)
;; 							 (make-array (cffi::dimensions type)
;; 								     :element-type (cffi::element-type type)))))))


;; (defmethod cffi:free-translated-object (pointer (type lcl-compat-array) param)
;;   (vom:info "[free-translated-object] param: ~a" param)
;;   (when param
;;     (vom:info "freeing (mandela) foreign object")
;;     (cffi:foreign-free pointer)))




;; [THO] version from 2019-12-13
;;;test
(defmethod cffi:expand-to-foreign-dyn (value var body (type lcl-compat-array))
  (let ((base-array (gensym "BASE")))
    `(let ((,base-array (static-base-array ,value)))
       (let ((,var (if ,base-array
		       (get-static-array-pointer ,value ,base-array)
		       (progn
			 (vom:warn "[expand-to-foreign] copying array")
			 (copy-to-foreign-array ,value (array-total-size ,value) (corresponding-foreign-type (array-element-type ,value)))))))
	 (prog1 ,@body
	   (when (not ,base-array)
	     (prog1 
		 (copy-from-foreign-array ,var (array-total-size ,value) (corresponding-foreign-type (array-element-type ,value)) ,value)
	       (cffi:foreign-array-free ,var)))))))) 


  ;; [THO] we now use ffa to get array pointers
  ;;this creates so much overhead that compilation takes very long
;; (defmethod cffi:expand-to-foreign-dyn (value var body (type lcl-compat-array))
;;   (let ((base-array (gensym "BASE"))
;; 	(val         (gensym "ARRAY-VAL-")))
;;     `(let* ((,val ,value))
;;        (ffa:with-pointer-to-array (,val ,var ,(foreign-type type) (array-total-size ,val) :copy-in-out)
;; 	 ,@body))))

       
       
;;	 (ffa:with-pointer-to-array (,value ,var ,(foreign-type type) (array-total-size ,value) ,(copy-strategy type))
;;	   ,@body))))


(defmethod cffi:translate-to-foreign (string (type lcl-compat-string))
  (if (cffi:pointerp string)
      string
      (cffi:foreign-string-alloc string)))

 (defmethod cffi:translate-from-foreign (pointer (type lcl-compat-string))
   (cffi:foreign-string-to-lisp pointer))

(defmethod cffi:free-translated-object (pointer (type lcl-compat-string) param)
    (declare (ignore param))
    (cffi:foreign-string-free pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; array copying from lisp to foreign and back
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns a foreign pointer which has to be freed
(defun copy-to-foreign-array (lisp-array size foreign-type)
  (let ((fp (cffi:foreign-alloc foreign-type :count size)))
    (dotimes (i size fp)
      (setf (cffi:mem-aref fp foreign-type i)
	    (row-major-aref lisp-array i)))))

;;does not free foeign pointer
(defun copy-from-foreign-array (pointer size foreign-type lisp-array)
  (dotimes (i size lisp-array)
    (setf (row-major-aref lisp-array i)
	  (cffi:mem-aref pointer foreign-type i))))




;;(in-package :lcl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lcl-type-translation-base-types (type)
    (case type
      ;;(:string 'lcl-string)
      (:array  'lcl-array)
      (:double-float 'obv-double-float)
      (:single-float 'obv-single-float)
      (:float        'obv-single-float)
      (otherwise type)))
  (defun lcl-type-translation-array-types (type)
    (case type
      ;;(:string 'lcl-string)
      (:array  'lcl-array)
      (otherwise type))))

;;name def may be a single name or a list with (name return-type)
;;if no return type is given :int is used instead
;;if arguments have no type then :int is used
;; as of 2019-12-13
(defmacro def-foreign-function (name-def &rest args)
  (let* ((func-name (if (listp name-def) (first name-def) name-def))
	 ;;look if c-name is different from lisp name
	 (c-name (or (if (listp name-def) (second (assoc :name (rest name-def))))
		     (cffi:translate-name-to-foreign func-name *package*)))
	 (lisp-name func-name)
	 ;;look if we got an return type
	 (return-type (if (listp name-def) (or (second (assoc :return-type (rest name-def))) :int) :int))
	 ;;parse args for no type
	 ;;if no type then set it to :int
	 (typed-args (mapcar (lambda (arg)
			       (if (listp arg)
				   (list (first arg) (if (listp (second arg))
							 (mapcar #'lcl-type-translation-array-types (second arg))
							 (lcl-type-translation-base-types (second arg))))
				   (list arg :int))) args)))
    `(defcfun (,c-name ,lisp-name) ,return-type ,@typed-args)))



;;(ffa:with-pointer-to-array (,value ,var ,(foreign-type type) (array-total-size ,value) ,(copy-strategy type))
;;	   ,@body))))

;; [tho] needs more work
;; to differentiate between static arrays and non-static
;; if this is a static array use the static vector
;; if non static use ffa to get a pointer to a possibly pinned array
;; this is a different approch
;; it looks which variables of the foreign functions ar arrays
;; of there are more than one array then ffa will take care of the
;; array pointers via an extra function between cffi:defcfun and the
;; actual calling lisp code
;; (defmacro def-foreign-function (name-def &rest args)
;;   (labels ((array-type-p (arg)
;; 	     (if (listp arg)
;; 		 (if (listp (second arg))
;; 		     (eq :array (first (second arg)))
;; 		     (eq :array (second arg)))
;; 		 (eq :array arg))))
		 
;;     (let* ((func-name (if (listp name-def) (first name-def) name-def))
;; 	   ;;look if c-name is different from lisp name
;; 	   (c-name (or (if (listp name-def) (second (assoc :name (rest name-def))))
;; 		       (cffi:translate-name-to-foreign func-name *package*)))
;; 	   (lisp-name func-name)
;; 	   ;;look if we got an return type
;; 	   (return-type (if (listp name-def) (or (second (assoc :return-type (rest name-def))) :int) :int))
;; 	   ;;parse args for no type
;; 	   ;;if no type then set it to :int
;; 	   (typed-args (mapcar (lambda (arg)
;; 				 (if (listp arg)
;; 				     (list (first arg) (if (listp (second arg))
;; 							   (mapcar #'lcl-type-translation (second arg))
;; 							   (lcl-type-translation (second arg))))
;; 				     (list arg :int))) args))
;; 	   (number-of-array-args (count-if #'array-type-p args))
;; 	   (pos-of-array-args (mapcar #'array-type-p args))
;; 	   (arg-names         (mapcar (lambda (arg)
;; 					(if (listp arg)
;; 					    (first arg)
;; 					    arg)) args))
;; 	   (array-arg-types  (mapcar (lambda (array-p arg)
;; 				       (when array-p
;; 					 (if (listp arg)
;; 					     (if (listp (second arg))
;; 						 (second (second arg))
;; 						 (second arg))
;; 					     :void))) pos-of-array-args typed-args))
;; 	   (alt-cffi-args    (mapcar (lambda (array-p typed-arg name)
;; 				       (if array-p
;; 					   (list name :pointer)
;; 					   typed-arg)) pos-of-array-args typed-args arg-names))
;; 	   (array-args-names-pos (mapcar (lambda (pred name)
;; 					   (if pred name nil)) pos-of-array-args arg-names))
;; 	   (array-pointer-vars-pos (mapcar (lambda (arg)
;; 					    (if arg
;; 						(gensym))) array-args-names-pos))
;; 	   (calling-args (mapcar (lambda (arrayp pointer arg)
;; 				   (if arrayp pointer arg)) pos-of-array-args array-pointer-vars-pos arg-names))
;; 	   (array-args-names (loop for arg in arg-names
;; 				   for pred in pos-of-array-args
;; 				   if pred collect arg))
;; 	   (array-pointer-vars (remove-if #'null array-pointer-vars-pos))
;; 	   (alternative-func-name (intern (concatenate 'string "++" (symbol-name func-name) "++")))) 
;;       (if (> number-of-array-args 1)
;; 	  `(progn
;; 	     (defcfun (,c-name ,alternative-func-name) ,return-type ,@alt-cffi-args)
;; 	     (defun ,lisp-name ,arg-names
;; 	       (ffa:with-pointers-to-arrays ,(mapcar  (lambda (name pointer type)
;; 							(list name pointer type (list 'array-total-size name) :copy-in-out)) array-args-names array-pointer-vars array-arg-types)
;; 		 (,alternative-func-name ,calling-args))))
;; 	  `(defcfun (,c-name ,lisp-name) ,return-type ,@typed-args)))))





;; count the array args and if it s more than 2 preare a form with
;; ffa and pointer args nstead of array args
(defmacro def-foreign-callable (name-def args &body body)
  (let* ((func-name (if (listp name-def) (first name-def) name-def))
	 ;;look if c-name is different from lisp name
	 (c-name (or (if (listp name-def) (second (assoc :name (rest name-def))))
		     (string-downcase (symbol-name func-name))))
	 (lisp-name func-name)
	 ;;look if we got an return type
	 (return-type (if (listp name-def) (or (second (assoc :return-type (rest name-def))) :int) :int))
	 ;;parse args for no type
	 ;;if no type then set it to :int
	 (typed-args (mapcar (lambda (arg)
			       (if (listp arg)
				   (list (first arg) (if (listp (second arg))
							 (mapcar #'lcl-type-translation (second arg))
							 (lcl-type-translation (second arg))))
				   (list arg :int))) args)))
    (declare (ignore c-name))
    `(defcallback ,lisp-name ,return-type ,typed-args
       ,@body)))


				



