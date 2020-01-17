;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: misc.lisp
;;;  Author: David Heeger/Eero Simoncelli
;;;  Description: miscellaneous routines used by obvius
;;;  Creation Date: summer '88
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)

(export '(2-pi pi-over-2 *div-by-zero-result* 
	  /-0  atan-0 log-0 sqr  clip  sinc  eq-t-p
	  x-dim y-dim z-dim row-dim col-dim
	  multiple-sort
	  tree-leaves  mapcar-tree  collapse-tree
	  list-of-length drop-last-of-list
	  list-from-array array-from-list minimize
	  factor constructor
	  with-local-arrays  catch-errors
	  obvius-commands obvius-parameters
	  *tolerance* *machine-tolerance* tolerance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; some constants

(defconstant 2-pi (* 2.0 pi))
(defconstant pi-over-2 (/ pi 2.0))

;;; print-db for debugging
(defmacro print-db (&rest forms)
  `(prog1 (progn
	    ,@(loop for form in forms
		    nconc (list `(print (quote ,form) *debug-io*)
				`(write-string "  " *debug-io*)
				`(prin1 ,form *debug-io*))))
    (terpri *debug-io*)))

;;; macros for numerical ops

(defmacro nround (x n)
  `(the fixnum (- ,x (the fixnum (mod ,x ,n)))))

(defvar *div-by-zero-result* 0.0
  "Default result used by /-0 and div functions when the divisor is zero.")
(eval-when (load eval) (setf (get '*div-by-zero-result* :type) 'float))

(defun /-0 (x y &optional (zero-val *div-by-zero-result*))
  (if (zerop y) (* (if (< x 0) -1.0 1.0) zero-val) (/ x y)))

(defun atan-0 (y x &optional (zero-val *div-by-zero-result*))
  (if (and (zerop x) (zerop y))
      zero-val
      (atan y x)))

(defun log-0 (val &optional base (zero-val *div-by-zero-result*))
  (if (<= val 0.0)
      zero-val
      (if base (log val base) (log val))))

(defun sinc (x &optional (bandwidth pi))
  (/-0 (sin (* x bandwidth)) (* x bandwidth) 1.0))

(defun sqr (x) (* x x))

(defun xor (x y)
  (or (and x (not y))
      (and y (not x))))

(defmethod similar ((val number) &rest keys)
  (declare (ignore keys))
  val)

(defun factorial (int)
  (loop with val = 1
	for x from int downto 2
	do (setq val (* val x))
	finally (return val)))

;; Factor an integer.
(defun factor (x)
  (unless (integerp x)
    (error "Can only factor integers"))
  (let ((list (loop with num = (abs x)
		    while (> num 1)
		    for factor = (loop for i from 2 to (/ num 2)
				       when (zerop (mod num i))
				       return i
				       finally (return num))
		    do (setq num (/ num factor))
		    collect factor)))
    (if (minusp x) (setf (elt list 0) (- (elt list 0))))
    list))


(defmethod clip ((x number) min max &key &allow-other-keys)
  (cond ((< x min) min)
	((> x max) max)
	(t x)))

(defmethod range ((thing t))
  (let ((min (minimum thing))
	(max (maximum thing)))
    (values (- max min) min max)))

(defmethod div ((num1 number) (num2 number) &key (zero-val *div-by-zero-result*)
		suppress-warning ->)
  (declare (ignore suppress-warning ->))
  (/-0 num1 num2 zero-val))

(defmethod add ((num1 number) (num2 number) &key ->)
  (declare (ignore ->))
  (+ num1 num2))

(defmethod mul ((num1 number) (num2 number) &key ->)
  (declare (ignore ->))
  (* num1 num2))

(defmethod sub ((num1 number) (num2 number) &key ->)
  (declare (ignore ->))
  (- num1 num2))

;;; Predicate checks if thing is eq to t
(defun eq-t-p (thing) (eq thing t))


;;; Method should return an s-expression such that (eval (constructor
;;; thing)) (in the current lexical environment) returns a copy of
;;; thing.
;;;  *** Need to write this for arrays, etc.
(defmethod constructor ((thing t))
  thing)

(defmethod constructor ((list list))
  (cons 'list (mapcar #'constructor list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; list hacks

(deftype num-list-2 ()
  `(satisfies num-list-2-p))

(defun num-list-2-p (thing)
  (and (listp thing)
       (= (list-length thing) 2)
       (every #'numberp thing)))

;;; Collect non-nil args into a list
(defun collect (&rest args)
  (loop for arg in args
	when arg
	collect arg))

;;; Create list of given length, filled with value.
#| Replace with this:
(defmacro list-of-length (length value)
  `(make-list ,length :initial-element ,value))
|#

(defun list-of-length (length value)
  (loop for i from 0 below length collect value))

;;; Create list of given length, filled with values from the
;;; base-list.  If from-end is non-nil, fill from end of base-list
;;; backwards (i.e. the end of the resulting list will match the end
;;; of base-list).
(defun sublist-of-length (length base-list value &key from-end)
  (let ((old-length (list-length base-list)))
    (if (> length old-length)
	(if from-end
	    (nconc (list-of-length (- length old-length) value) base-list)
	    (append base-list (list-of-length (- length old-length) value)))
	(if from-end
	    (subseq base-list (- old-length length))
	    (subseq base-list 0 length)))))

;;; Default version just calls list-x-dim.  Assumes there is a
;;; dimensions method.
(defmethod x-dim (thing)
  (list-x-dim (dimensions thing)))

(defmethod y-dim (thing)
  (list-y-dim (dimensions thing)))

(defmethod z-dim (thing)
  (list-z-dim (dimensions thing)))

(defmethod row-dim (thing)
  (list-y-dim (dimensions thing)))

(defmethod col-dim (thing)
  (list-x-dim (dimensions thing)))

;;; These functions count from the end of the list, starting at a
;;; value of :offset, to find the appropriate element.  
(defun list-x-dim (l &optional (default 1))
  (declare (list l))
  (let ((len (list-length l)))
    (if (< len 1)
	default
	(nth (1- len) l))))

(defun list-y-dim (l &optional (default 1))
  (declare (list l))
  (let ((len (list-length l)))
    (if (< len 2)
	default
	(nth (- len 2) l))))

(defun list-z-dim (l &optional (default 1))
  (declare (list l))
  (let ((len (list-length l)))
    (if (< len 3)
	default
	(nth (- len 3) l))))

;;; Get Nth value from end of list, defaulting to default if list is too short.
(defun list-nth-dim (n l &optional (default 1))
  (declare (list l) (number default n))
  (let ((len (list-length l)))
    (if (<= len n)
	default
	(nth (- len n 1) l))))

(defmethod rank ((the-list list))
  (loop for sub-list = the-list then (car sub-list)
	for count from 0
	while (consp sub-list)
	finally (return count)))

;;; See also list-from-array in this file.  *** This should try to
;;; figure out the element-type automatically if it is not passed.
(defun array-from-list (nested-list &rest make-array-kewords &key element-type
				    &allow-other-keys)
  (let ((dims (loop for sub-list = nested-list then (car sub-list)
		    while (consp sub-list)
		    collect (list-length sub-list))))
    (when element-type
      (setq nested-list
	    (mapcar-tree #'(lambda (x) (coerce x element-type))
			 (list nested-list))))
    (apply 'make-array dims :initial-contents nested-list make-array-kewords)))

(defun minimize (seq &key key)
  (if (consp seq)
      (if key
	  (loop with min-pos = 0
		with min-val = (funcall key (car seq))
		for item in (cdr seq)
		for val = (funcall key item)
		for pos from 1
		do (when (< val min-val) (setq min-pos pos  min-val val))
		finally (return (values min-val min-pos )))
	  (loop with min-pos = 0
		with min-val = (car seq)
		for val in (cdr seq)
		for pos from 1
		do (when (< val min-val) (setq min-pos pos  min-val val))
		finally (return (values min-val min-pos ))))
      (if key
	  (loop with min-pos = 0
		with min-val = (funcall key (aref seq 0))
		for pos from 1 below (array-total-size seq)
		for val = (funcall key (aref seq pos))
		do (when (< val min-val) (setq min-pos pos  min-val val))
		finally (return (values min-val min-pos )))
	  (loop with min-pos = 0
		with min-val = (aref seq 0)
		for pos from 1 below (array-total-size seq)
		for val = (aref seq pos)
		do (when (< val min-val) (setq min-pos pos  min-val val))
		finally (return (values min-val min-pos ))))))

(defmethod multiple-sort ((keys vector) (vector vector) predicate)
  (let ((pairs (map 'vector #'vector keys vector)))
    (sort pairs predicate :key #'(lambda(x) (aref x 0)))
    (dotimes (i (length vector))
      (setf (aref vector i)
	    (aref (aref pairs i) 1)))))

(defmethod multiple-sort ((keys list) (data list) predicate)
  (let ((pairs (map 'vector #'vector keys data)))
    (sort pairs predicate :key #'(lambda(x) (aref x 0)))
    (map 'list #'(lambda(x) (aref x 1)) pairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; tree hacks

#|
;;; bug in lucid compiler
(defun test (list-or-nil)
  (let ((result (bar list-or-nil)))
    (if list-or-nil list-or-nil result)))

(defun bar (list-or-nil)
  (if list-or-nil (rplaca list-or-nil 'it) (list 'it)))
|#

;;; Extract the leaves of the tree recursively. Return  in a list.
(defun tree-leaves (tree)
  (if (or (null (cdr tree))
	  (every #'null (cdr tree)))
      (list (car tree))
      (mapcan 'tree-leaves (cdr tree))))

;;; Mapcar-Tree applies the given function to trees and returns a tree.
;;; If any tree entry is nil, returned tree has a nil entry at that place.
;;;   func: bound to function or method (e.g., 'foo or #'foo)
;;;   list-of-tree-args: list of trees, e.g., '((1 (2 3)) (4 (5 6)))
;;;   args: list non-tree arguments, can be keyword specifications
;;;   result-tree: destructively modified
;;; If result-tree is not nil (e.g., if it is a tree of nils), then func
;;; gets called with :-> to put the result in the right place.
(defun mapcar-tree (func list-of-tree-args 
		    &key (args nil) (result-tree nil))
  (let (result)
    (setq result (mapcar-tree-internal func list-of-tree-args 
				      :args args :result-tree result-tree))
    (if result-tree result-tree result)))

(defun mapcar-tree-internal (func list-of-tree-args 
			     &key (args nil) (result-tree nil))
   (if (notany #'(lambda (x) (and (stringp x) (string= x "EOLIST"))) list-of-tree-args)
      (let ((first-args (mapcar #'mycar list-of-tree-args))
	    (rest-args (mapcar #'mycdr list-of-tree-args))
	    (first-result (mycar result-tree))
	    (rest-result (mycdr result-tree)))
	(if (some #'listp first-args)
	    (if (every #'consp first-args)
		(cons (mapcar-tree-internal func first-args 
					    :args args :result-tree first-result) 
		      (mapcar-tree-internal func rest-args 
					    :args args :result-tree rest-result))
		(cond (result-tree
		       (rplaca result-tree nil)
		       (mapcar-tree-internal func rest-args 
					     :args args :result-tree rest-result))
		      (t
		       (cons nil (mapcar-tree-internal func rest-args :args args)))))
	    (cond (result-tree 
		   (rplaca result-tree 
			   (apply func (nconc first-args args (list :-> first-result))))
		   (mapcar-tree-internal func rest-args 
					 :args args :result-tree rest-result))
		  (t
		   (cons (apply func (nconc first-args args))
			 (mapcar-tree-internal func rest-args :args args))))))
      nil))

#|
(defun old-mapcar-tree (func list-of-tree-args &optional (list-of-other-args nil))
   (if (notany #'(lambda (x) (and (stringp x) (string= x "EOLIST"))) list-of-tree-args)
      (let ((first-args (mapcar #'mycar list-of-tree-args))
	    (rest-args (mapcar #'mycdr list-of-tree-args)))
	(if (some #'listp first-args)
	    (if (every #'consp first-args)
		(cons (mapcar-tree func first-args list-of-other-args) 
		      (mapcar-tree func rest-args list-of-other-args))
		(cons nil (mapcar-tree func rest-args list-of-other-args)))
	    (cons (apply func (nconc first-args list-of-other-args))
		  (mapcar-tree func rest-args list-of-other-args))))
      nil))
|#

(defun mycar (l)
  (if (consp l)
      (car l)
      l))

(defun mycdr (l)
  (if (consp l)
      (if (null (cdr l)) "EOLIST" (cdr l))
      l))

(defun describe-tree-elements (tree)
  (mapcar-tree #'(lambda (x) (describe x)) (list tree))
  t)

#|
;;; Examples of using mapcar-tree

(mapcar-tree '+ '((1 2 (3)) (1 2 (3))))

(defun foo (x &rest args) x)
(setq test '(nil (nil nil (nil nil) nil nil) nil))
(mapcar-tree 'foo '((1 (21 22 (231 nil) nil 25) 3)) :result-tree test)

(defun foo1 (x &rest args) (- x 1))
(setq test1 '(1 (21 22 (231 nil) nil 25) 3))
(mapcar-tree 'foo1 (list test1) :result-tree test1)

(mapcar-tree 'square (list (list im)) :result-tree (list im))

|#

;;; collapses a tree into a list ignoring nils
(defun collapse-tree (tree)
  (cond ((consp tree)
	 (apply 'append (mapcar 'collapse-tree tree)))
	((null tree)
	 nil)
	((atom tree)
	 (list tree))))

#|
(collapse-tree '(6 7))
(collapse-tree '(1 2 nil (3 4 5 (6 7) 8)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ARRAY HACKS
;;; should all work on arrays with any number of dimensions
;;; except for print-array, print-vector
;;; WARNING: these routines do not do error checking.

;;; Syntax is like let: (with-displaced-vectors ((sym1 arr1) (sym2
;;; arr2)) . body)
;;; *** Might be more efficient to use the parent array (in-case array
;;; is displaced).
(defmacro with-displaced-vectors (array-list . body)
  `(let ,(loop for pair in array-list 
	  collect `(,(car pair) (vectorize ,(cadr pair))))
     (declare (type (vector * *) ,@(loop for pair in array-list collect (car pair))))
    ,@body))

;;; Macro to be used in place of let* which frees arrays that are locally
;;; bound.
(defmacro with-local-arrays (arr-list . body)
  (let* ((vars (loop for item in arr-list
		     for var = (if (symbolp item) item (car item))
		     collect var)))
    `(let* ,vars
      (unwind-protect
	(progn ,@(loop for item in arr-list
		       when (listp item) collect `(setq ,@item))
	       ,@body)
	(free-local-arrays ,@vars)))))

(defun free-local-arrays (&rest arr-list)
  (loop for arr in arr-list
	do (when (allocated-array-p arr) (free-array arr))))

;;; *** This is a constructor
(defun list-from-array (arr &optional (num-indices 0) indices)
  (cons 'list
	(if (> (- (array-rank arr) num-indices) 1)
	    (loop with new-num-indices = (1+ num-indices)
		  with last = (list 0)
		  for i from 0 below (array-dimension arr num-indices)
		  for new-indices = (append indices last)
		  then (progn (rplaca last i) new-indices)
		  collect (list-from-array arr new-num-indices new-indices))
	    (loop with last = (list 0)
		  for i from 0 below (array-dimension arr num-indices)
		  for new-indices = (append indices last)
		  then (progn (rplaca last i) new-indices)
		  collect (apply #'aref arr new-indices)))))

;;;[tho] 2016-08-10
;;; changed to actual cffi types
;;;
;;; This is ugly, but necessary for many of our foreign function
;;; calls.  This may only be valid on Suns.
(defun sizeof (type)
  (or (and (listp type)
  	   (or (eq (car type) 'unsigned-byte)
  	       (eq (car type) 'signed-byte))
  	   (/ (cadr type) 8))
      (case type
	(float  (cffi:foreign-type-size :float))
	(double-float  (cffi:foreign-type-size :double)) 
	(single-float  (cffi:foreign-type-size :float))
	(bit  1/8)
	(otherwise (cffi:foreign-type-size :POINTER)))))
					;pointer?

  
  ;; (or (and (listp type)
  ;; 	   (or (eq (car type) 'unsigned-byte)
  ;; 	       (eq (car type) 'signed-byte))
  ;; 	   (/ (cadr type) 8))
  ;;     (cdr (assoc type '((float . 8)
  ;; 			 (double-float . 8)
  ;; 			 (single-float . 4)
  ;; 			 (bit . 1/8))
  ;; 		  :test #'equal))
  ;;     4))				;pointer?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Generic error-catching macro

;;; Wasteful call to multiple-value-list, but it works in generic Common Lisp!
(defmacro catch-errors (form . error-forms)
  "Execute form, returning its value.  If an error occurs, execute error-forms."
  (let ((error-p (gensym)) (res (gensym)))
    `(let* ((,error-p t)
	    ,res)
      (unwind-protect
	   (progn
	     (setq ,res (multiple-value-list ,form))
	     (setq ,error-p nil)
	     (apply 'values ,res))
	(when ,error-p ,@error-forms)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tolerance stuff:

(defvar *tolerance* 1e-6
  "User-settable tolerance for certain floating-point calculations and comparisons")
(eval-when (load eval) (setf (get '*tolerance* :type) 'float))

(defvar *machine-tolerance* (/ least-positive-single-float single-float-epsilon)
  "Machine limit on floating point calculations")
(eval-when (load eval) (setf (get '*machine-tolerance* :type) 'float))

;;; Tolerance methods are for rounding off floating point numbers.  I
;;; tried everything I could imagine to fix the basic method, and
;;; ended up with a hack.  The problem is that it makes no sense for a
;;; tolerance that is not an integer power of 10.
(defmethod tolerance ((value number) &optional (tolerance *tolerance*) &key)
  (let ((scale (expt 10 (round (- (log tolerance 10))))))
    (unless (almost-equal (/ scale) tolerance)
      (warn "Tolerance is not a power of 10"))
    (/ (ffloor (+ 0.5 (* value scale))) scale)))

(defmethod tolerance ((list list) &optional (tolerance *tolerance*)
		      &key ((:-> result) (similar list)))
  (loop for i from 0 below (length result)
	do (setf (elt result i) (tolerance (elt list i) tolerance)))
  result)

;; *** Sadly, this does not do the right thing. Below is an example of the problem.
;; It is something strange about setf method on arrays I think.
#|
(setf foo (make-array 1 :element-type 'single-float))
(setf (aref foo 0) 3.999)
(aref foo 0)
(tolerance (aref foo 0))
(aref (tolerance foo) 0)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A helpful utility which prints out the exported OBVIUS parameters
;;; and their values.
(defun obvius-parameters (&optional with-documentation)
  (let ((parameters (obvius-parameter-list)))
    (loop for sym in parameters
	  do
	  (if (and with-documentation (documentation sym 'variable))
	      (format t "~A = ~S,~%   ~A~%" sym (symbol-value sym) (documentation sym 'variable))
	      (format t "~A = ~S~%"     sym (symbol-value sym)))
	  (when with-documentation (format t "~%")))))

;;; List all exported variables in OBVIUS package, starting with "*"
(defun obvius-parameter-list ()
  (let (parameters)
    (do-external-symbols (sym 'OBVIUS)
      (when (and (boundp sym) (char= #\* (char (symbol-name sym) 0)))
	(pushnew sym parameters)))
    parameters))

(defun obvius-commands (&optional with-documentation)
  (do-external-symbols (sym 'OBVIUS)
    (when (fboundp sym)
      (if (and with-documentation (documentation sym 'function))
	  (format t "~A:~%   ~A~%~%" sym (documentation sym 'function))
	  (format t "~A~%" sym)))))


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
