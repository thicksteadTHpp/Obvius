;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: viewable-matrix.lisp
;;;  Author: Heeger
;;;  Description: Matrices with elements that are viewables.
;;;  Creation Date: 1/94
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)
(export '(viewable-matrix-p make-viewable-matrix
	  make-viewable-matrix normalize-cols matrix-inverse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Viewable-Matrix Object

(defmacro viewable-matrix-p (obj)
  `(typep ,obj 'viewable-matrix))

(defmethod matrix ((mtx viewable-matrix))
  (slot-value mtx 'data))

(defmethod (setf matrix) (arr (mtx viewable-matrix))
  (unless (and (typep arr '(array t (* *)))
	       (every #'(lambda (x) (viewable-p x)) (vectorize arr)))
    (error "Arg must be a 2D array containing viewables"))
  (setf (slot-value mtx 'data) arr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Dimensions:

;;; Dimensions, x-dim, and y-dim return dimensions of underlying
;;; viewable.  row-dim and col-dim return matrix size.

(defmethod dimensions ((mat viewable-matrix))
  (dimensions (aref (data mat) 0 0)))

(defmethod x-dim ((mat viewable-matrix))
  (x-dim (aref (data mat) 0 0)))

(defmethod y-dim ((mat viewable-matrix))
  (y-dim (aref (data mat) 0 0)))

(defmethod row-dim ((mat viewable-matrix))
  (row-dim (data mat)))

(defmethod col-dim ((mat viewable-matrix))
  (col-dim (data mat)))

(defmethod size ((mat viewable-matrix))
  (list (row-dim mat) (col-dim mat)))

(defmethod total-size ((mat viewable-matrix))
  (* (row-dim mat) (col-dim mat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Various required methods

;;; Just check size of the matrix.  Size of individual viewables will
;;; be checked by the functions that are called on them!
(defmethod check-size ((mat viewable-matrix) &rest mat-list)
  (cond ((null mat-list) mat)
	((or (/= (row-dim mat) (row-dim (car mat-list)))
	     (/= (col-dim mat) (col-dim (car mat-list))))
	 (error "~As have different sizes." (object-class-name mat)))
	(t (apply 'check-size mat-list))))

(defmethod print-object ((mat viewable-matrix) stream)
  (format stream "#<(~A of ~A) " (object-class-name mat)
	  (object-class-name (aref (data mat) 0 0)))
  (format stream "~S " (name mat))
  (format stream "(~A,~A)>" (row-dim mat) (col-dim mat)))

(defmethod inferiors-of ((mat viewable-matrix))
  (listify (data mat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make-viewable-matrix

;;; Data can be array of vbls, nested-list of vbls, or nil.  If nil,
;;; must specify :size and :sub-viewable-spec.

;;; When data, returns a data array, otherwise returns nil.
(defun setup-data-for-make-viewable-matrix (data)
  (when data
    (setq data (cond
		 ;; data is an array
		 ((typep data '(array t (* *))) data)
		 ;; data is a nested-vbl-list
		 ((and (listp data)
		       (= (rank data) 2)
		       (every #'(lambda (x) (every #'(lambda (y) (viewable-p y)) x))
			      data))
		  (make-array (list (length data) (length (car data)))
			      :initial-contents data))
		 (t (error "Bad data argument: ~a must be a 2-dimensional array of viewables or a nested list of viewables" data))))
    (apply 'check-size (listify data)))
  data)

(defun make-viewable-matrix (data &rest initargs &key name -> display-type)
  (declare (ignore name display-type))
  (when -> (setf (getf initargs :name) ->))
  (setq data (setup-data-for-make-viewable-matrix data))
  (with-result ((result nil)
		`(:class viewable-matrix
		  :data ,data ,@initargs)
		'apply 'make-viewable-matrix data initargs)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set-result, initialize-instance, and various other methods
;;; required for viewable-matrices.

;;; When calling with-result with a model-plist, the programmer must
;;; provide either a (1) :data or (2) :size and :sub-viewable-spec.
;;; These specify what kind of sub-viewables are to be created.  The
;;; :sub-viewable-spec is like a model-plist for the sub-viewable.

(defmethod set-result ((name t) (model viewable-matrix))
  (make-instance (class-of model)
		 :data (make-array (list (row-dim model) (col-dim model))
				   :initial-contents
				   (loop for y from 0 below (row-dim model)
					 collect
					 (loop for x from 0 below (col-dim model)
					       collect
					       (set-result nil (aref (data model) y x)))))
		 :name name
		 :display-type (display-type model)))

;;; If user passes :size in model-plist, then check that result is
;;; right size.  If user passes a :data, call set-result on each
;;; sub-viewable to make sure they are compatible.  If user passes a
;;; :sub-viewable-spec, also call set-result... (the
;;; :sub-viewable-spec should look like a model-plist for the
;;; sub-viewable).
(defmethod set-result ((res viewable-matrix) (model-plist list))
  (when (and (getf model-plist :size)
	     (not (equal (getf model-plist :size)
			 (list (row-dim res) (col-dim res)))))
    (error "Result matrix is wrong size: ~A" res))
  ;; Check that result sub-viewables are compatible with data/sub-viewable-spec.
  (cond ((getf model-plist :data)
	 (loop for y from 0 below (row-dim res)
	       do
	       (loop for x from 0 below (col-dim res)
		     do
		     (set-result (aref (data res) y x)
				 (aref (getf model-plist :data) y x)
				 ))))
	((getf model-plist :sub-viewable-spec)
	 (setf (data res)
	       (loop for y from 0 below (row-dim res)
		     do
		     (loop for x from 0 below (col-dim res)
			   do
			   (set-result (aref (data res) y x)
				       (aref (getf model-plist :sub-viewable-spec) y x)
				       ))))))
  (call-next-method))

;;; When :data, check that :data is an array of viewables consistent
;;; with :size.  When :sub-viewable-spec, make the sub-viewables (note
;;; that :size must be specified).
(defmethod initialize-instance ((mat viewable-matrix) &rest initargs
				&key size data sub-viewable-spec)
  (cond (data
	 (unless (typep data '(array t (* *)))
	   (error "Bad data argument: ~a must be a 2-dimensional array of viewables" data))
	 (unless (every #'(lambda (thing) (viewable-p thing)) (listify data))
	   (error "Bad data argument: ~a must be an array of viewables" data))
	 (when size
	   (unless (and (= (list-y-dim size) (row-dim data))
			(= (list-x-dim size) (col-dim data)))
	     (error "Conflicting initialization arguments: dimensions=~a  data=~a"
		    size data))))
	(sub-viewable-spec
	 (unless size
	   (error "Must provide :size parameter to initialize viewable-matrix"))
	 (unless (and (listp size) (= (length size) 2))
	   (error "Improper :size parameter, must be list of length 2"))
	 (setf (getf initargs :data) (setq data (make-array size
	       :initial-contents
	       (loop for y from 0 below (list-y-dim size)
		     collect
		     (loop for x from 0 below (list-x-dim size)
			   collect
			   (set-result nil sub-viewable-spec))))))
	 (remf initargs :size)
	 (remf initargs :sub-viewable-spec))
	(t (error "Must provide a :data or :sub-viewable-spec and :size args")))
  (loop for vbl in (listify data) do (pushnew mat (superiors-of vbl)))
  (apply #'call-next-method mat initargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Viewable matrix operations

;;; unary-viewable-matrix-op applies a function to a viewable matrix
;;; and a list of arguments binary-viewable-matrix-op applies a
;;; function to two viewable matrices and a list of arguments

(defmacro unary-viewable-matrix-op (function mat res &rest args)
  (let ((result-mat (gensym)) (y (gensym)) (x (gensym)))
    `(with-result ((,result-mat ,res)
		   ,mat
		   ,function ,mat ,@args)
      (loop for ,y from 0 below (row-dim ,mat) do
	    (loop for ,x from 0 below (col-dim ,mat)
		  do
		  (funcall ,function (aref (data ,mat) ,y ,x) ,@args
			   :-> (aref (data ,result-mat) ,y ,x))))
      ,result-mat)))

;; like unary-viewable-matrix-op, but reverses order of viewable matrix and args
(defmacro unary-viewable-reverse-matrix-op (function mat res &rest args)
  (let ((result-mat (gensym)) (y (gensym)) (x (gensym)))
    `(with-result ((,result-mat ,res)
		   ,mat
		   ,function ,mat ,@args)
      (loop for ,y from 0 below (row-dim ,mat) do
	    (loop for ,x from 0 below (col-dim ,mat)
		  do
		  (funcall ,function ,@args (aref (data ,mat) ,y ,x)
			   :-> (aref (data ,result-mat) ,y ,x))))
      ,result-mat)))

(defmacro binary-viewable-matrix-op (function mat1 mat2 res &rest args)
  (let ((result-mat (gensym)) (y (gensym)) (x (gensym)))
    `(with-result ((,result-mat ,res)
		   ,mat1
		   ,function ,mat1 ,mat2 ,@args)
      (loop for ,y from 0 below (row-dim ,mat1) do
	    (loop for ,x from 0 below (col-dim ,mat1)
		do
		(funcall ,function (aref (data ,mat1) ,y ,x) (aref (data ,mat2) ,y ,x)
			 ,@args :-> (aref (data ,result-mat) ,y ,x))))
      ,result-mat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; arith ops: matrix,matrix

(defmethod add ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (binary-viewable-matrix-op 'add mat1 mat2 ->))

(defmethod mul ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (binary-viewable-matrix-op 'mul mat1 mat2 ->))

(defmethod sub ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (binary-viewable-matrix-op 'sub mat1 mat2 ->))

(defmethod div ((mat1 viewable-matrix) (mat2 viewable-matrix)
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (binary-viewable-matrix-op 'div mat1 mat2 -> :zero-val zero-val
			       :suppress-warning suppress-warning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; arith ops: matrix,number

(defmethod add ((mat viewable-matrix) (const number) &key ->)
  (unary-viewable-matrix-op 'add mat -> const))

(defmethod add ((const number) (mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'add mat -> const))

(defmethod sub ((mat viewable-matrix) (const number) &key ->)
  (add mat (- const) :-> ->))

(defmethod sub ((const number) (mat viewable-matrix) &key ->)
  (with-result ((result ->) mat 'sub const mat)
    (loop for vbl in (listify (data mat))
	  for res in (listify (data result))
	  do (sub const vbl :-> res))
    result))

(defmethod mul ((mat viewable-matrix) (const number) &key ->)
  (unary-viewable-matrix-op 'mul mat -> const))

(defmethod mul ((const number) (mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'mul mat -> const))

(defmethod div ((mat viewable-matrix) (const number) 
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (unary-viewable-matrix-op 'div mat -> const :zero-val zero-val :suppress-warning suppress-warning))

(defmethod div ((num number) (mat viewable-matrix)
		&key (zero-val *div-by-zero-result*) suppress-warning
		((:-> res)))
  (with-result ((result res) mat 'div num mat :zero-val zero-val)
    (loop for arg-vbl in (listify (data mat))
	  for res-vbl in (listify (data result))
	  do (div num arg-vbl :zero-val zero-val :suppress-warning suppress-warning :-> res-vbl))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; arith ops: matrix,array

(defmethod data ((arr array))
  arr)

(defmethod mul ((mat viewable-matrix) (arr array) &key ->)
  (binary-viewable-matrix-op 'mul mat arr ->))

(defmethod mul ((arr array) (mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'mul arr -> mat))

(defmethod div ((mat viewable-matrix) (arr array) &key -> &allow-other-keys)
  (unary-viewable-matrix-op 'div arr -> mat))

(defmethod div ((arr array) (mat viewable-matrix) &key -> &allow-other-keys)
  (unary-viewable-reverse-matrix-op 'div arr -> mat))

(defmethod add ((mat viewable-matrix) (arr array) &key ->)
  (unary-viewable-matrix-op 'add arr -> mat))

(defmethod add ((arr array) (mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'add arr -> mat))

(defmethod sub ((mat viewable-matrix) (arr array) &key ->)
  (unary-viewable-matrix-op 'sub mat -> arr))

(defmethod sub ((arr array) (mat viewable-matrix) &key ->)
  (unary-viewable-reverse-matrix-op 'sub mat -> arr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; unary matrix ops

(defmethod linear-xform ((mat viewable-matrix) scale offset &key ->)
  (unary-viewable-matrix-op 'linear-xform mat -> scale offset))

(defmethod negate ((mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'negate mat ->)) 

(defmethod clip ((mat viewable-matrix) below above &key ->)
  (unary-viewable-matrix-op 'clip mat -> below above))

(defmethod abs-value ((mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'abs-value mat ->))

(defmethod square ((mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'square mat ->))

(defmethod square-root ((mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'square-root mat ->))

(defmethod power ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (binary-viewable-matrix-op 'power mat1 mat2 ->))

(defmethod power ((mat viewable-matrix) (val number) &key ->)
  (unary-viewable-matrix-op 'power mat -> val))

(defmethod natural-logarithm ((mat viewable-matrix) &key (zero-val *div-by-zero-result*) ->)
  (unary-viewable-matrix-op 'natural-logarithm mat -> :zero-val zero-val))

;;; Since this doesn't take a -> arg, can't use unary-viewable-matrix-op.
(defmethod fill! ((mat viewable-matrix) val)
  (dolist (vbl (listify (data mat)))
    (fill! vbl val))
  (set-not-current mat)
  (set-history mat 'fill! mat val)
  mat)

(defmethod copy ((mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'copy mat ->))

(defmethod transpose ((mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'transpose mat ->))

(defmethod point-operation ((mat viewable-matrix) (func t) &key binsize ->) 
  (unary-viewable-matrix-op 'point-operation mat -> func :binsize binsize))

(defmethod periodic-point-operation ((mat viewable-matrix) (func t) period &key binsize ->)
  (unary-viewable-matrix-op 'periodic-point-operation mat -> func period :binsize binsize))

(defmethod round. ((mat viewable-matrix) &key (divisor 1.0) ->)
  (unary-viewable-matrix-op 'round. mat -> :divisor divisor))

(defmethod truncate. ((mat viewable-matrix) &key (divisor 1.0) ->)
  (unary-viewable-matrix-op 'truncate. mat -> :divisor divisor))

(defmethod floor. ((mat viewable-matrix) &key (divisor 1.0) ->)
  (unary-viewable-matrix-op 'floor. mat -> :divisor divisor))

(defmethod quantize ((mat viewable-matrix) &key ->
		     (binsize (/ (range mat) (get-default 'discrete-function :size)))
		     (origin (mean mat)))
  (unary-viewable-matrix-op 'quantize mat -> :origin origin :binsize binsize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; *** crop, paste, side-by-side, subsample, downsample, upsample
;;; (operate on each sub-viewable).

(defmethod circular-shift ((mat viewable-matrix) &key
			   (y-shift 0) (x-shift 0)
			   (y y-shift) (x x-shift) ->)
  (unary-viewable-matrix-op 'circular-shift mat -> :y y :x x
			    :y-shift y-shift :x-shift x-shift))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; apply-filter, expand-filter defined in filter.lisp.  Blur,
;;; gauss-out, gauss-in defined here.  All of these operate on each
;;; sub-viewable.

(defmethod blur ((mat viewable-matrix)
		 &key 
		 (level 1)
		 (kernel (mapcar #'(lambda (x) (* x (sqrt 2))) gauss-5))
		 (edge-handler :reflect1)
		 ->)
  (unary-viewable-matrix-op 'blur mat ->
			    :level level
			    :kernel kernel
			    :edge-handler edge-handler))

;;; keys: :edge-handler :resample :kernel
(defmethod gauss-out ((mat viewable-matrix) &rest keys &key -> &allow-other-keys)
  (remf keys :->)
  (let ((res-data (make-array (list (row-dim mat) (col-dim mat))
			      :initial-contents
			      (loop for y from 0 below (row-dim mat)
				    collect
				    (loop for x from 0 below (col-dim mat)
					  for vbl = (aref (data mat) y x)
					  for res = (and (viewable-p ->) (aref (data ->) y x))
					  collect
					  (apply 'gauss-out vbl :-> res keys))))))
    (with-result ((result ->)
		  (list :class (clos::class-of mat)
			:data res-data
			:display-type (display-type mat))
		  'apply 'gauss-out mat keys)
      result)))

(defmethod gauss-in ((mat viewable-matrix) &rest keys &key -> &allow-other-keys)
  (remf keys :->)
  (let ((res-data (make-array (list (row-dim mat) (col-dim mat))
			      :initial-contents
			      (loop for y from 0 below (row-dim mat)
				    collect
				    (loop for x from 0 below (col-dim mat)
					  for vbl = (aref (data mat) y x)
					  for res = (and (viewable-p ->) (aref (data ->) y x))
					  collect
					  (apply 'gauss-in vbl :-> res keys))))))
    (with-result ((result ->)
		  (list :class (clos::class-of mat)
			:data res-data
			:display-type (display-type mat))
		  'apply 'gauss-in mat keys)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; comparison ops

(defmethod point-minimum ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (binary-viewable-matrix-op 'point-minimum mat1 mat2 ->))

(defmethod point-minimum ((mat viewable-matrix) (val number) &key ->)
  (unary-viewable-matrix-op 'point-minimum mat -> val))

(defmethod point-minimum ((val number) (mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'point-minimum mat -> val))

(defmethod point-maximum ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (binary-viewable-matrix-op 'point-maximum mat1 mat2 ->))

(defmethod point-maximum ((mat viewable-matrix) (val number) &key ->)
  (unary-viewable-matrix-op 'point-maximum mat -> val))

(defmethod point-maximum ((val number) (mat viewable-matrix) &key ->)
  (unary-viewable-matrix-op 'point-maximum mat -> val))

(defmethod square-error ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (binary-viewable-matrix-op 'square-error mat1 mat2 ->))

(defmethod abs-error ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (binary-viewable-matrix-op 'abs-error mat1 mat2 ->))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Ops that return scalars

;;; *** Need to write: minimum-location, maximum-location,
;;; mean-square-error, mean-abs-error, max-abs-error

(defmethod minimum ((mat viewable-matrix))
  (loop for im in (listify (data mat)) 
	minimize (the single-float (minimum im))))

(defmethod maximum ((mat viewable-matrix))
  (loop for im in (listify (data mat))
	maximize (the single-float (maximum im))))

(defmethod mean ((mat viewable-matrix) &key ignore-zeros)
  (loop for im in (listify (data mat))
	summing (mean im :ignore-zeros ignore-zeros) into total
	counting t into num
	finally (return (/ total num))))

;;; assumes that all of the sub-viewables are identical size
(defmethod mean-square-error ((mat1 viewable-matrix) (mat2 viewable-matrix))
  (loop for im1 in (listify (data mat1))
	for im2 in (listify (data mat2))
	summing (mean-square-error im1 im2) into total
	counting t into num
	finally (return (/ total num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; matrix-mul methods for viewable-matrix,viewable-matrix
#|
;; Removed this since it's already in matrix.lisp
(defmacro check-matrix-mul-compatibility (row1 col1 row2 col2 row3 col3)
  `(unless (and (= ,col1 ,row2) (= ,row1 ,row3) (= ,col2 ,col3))
    (error "Arrays are incompatible for multiplication")))
|#

;;; internal function to save repeating loop code over and over.
(defmacro make-viewable-array-data (dims model)
  `(make-array ,dims :initial-contents
    (loop for y from 0 below (car ,dims)
	  collect
	  (loop for x from 0 below (cadr ,dims)
		collect
		(similar ,model)))))

;;; Vectorize and columnize (as for the methods defined on arrays)
;;; make displaced arrays.  They do not copy the data.
(defmethod vectorize ((mat viewable-matrix)
		      &key (size (total-size mat)) (x-offset 0) (y-offset 0) ->)
  (if (viewable-p ->)
      (error "Vectorize doesn't copy, result ~a must be a symbol or a string" ->)
      (with-result ((result ->)
		    (list :class (class-of mat)
			  :data (make-array (list 1 size)
					    :displaced-to (data mat)
					    :displaced-index-offset
					    (+ (* y-offset (col-dim mat)) x-offset))))
	result)))

(defmethod columnize ((mat viewable-matrix)
		      &key (size (total-size mat)) (x-offset 0) (y-offset 0) ->)
  (if (viewable-p ->)
      (error "Vectorize doesn't copy, result ~a must be a symbol or a string" ->)
      (with-result ((result ->)
		    (list :class (class-of mat)
			  :data (make-array (list size 1)
					    :displaced-to (data mat)
					    :displaced-index-offset
					    (+ (* y-offset (col-dim mat)) x-offset))))
	result)))

(defmethod matrix-transpose ((mat viewable-matrix) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (make-viewable-array-data
					 (list (col-dim mat) (row-dim mat))
					 (aref (data mat) 0 0))))
    (when (eq result mat) (error "Result cannot be eq to argument."))
    (loop for j from 0 below (row-dim result) do
	  (loop for i from 0 below (col-dim result) do
		(copy (aref (data mat) i j) :-> (aref (data result) j i))))
    result))

;;; *** Need to write determinant for arbitrary size matrices ***
(defmethod determinant ((mat viewable-matrix) &key ->)
  (cond ((equal (size mat) '(2 2))
	 (viewable-matrix-determinant-2by2 mat :-> ->))
	((equal (size mat) '(3 3))
	 (viewable-matrix-determinant-3by3 mat :-> ->))
	(t (error "Determinant only defined for 2by2 or 3by3 viewable-matrices"))))

(defmethod matrix-inverse ((mat viewable-matrix) &key ->)
  (when (eq mat ->)
    (error "Matrix-inverse can't do destructive modification"))
  (cond ((equal (size mat) '(2 2))
	 (viewable-matrix-inverse-2by2 mat :-> ->))
	((equal (size mat) '(3 3))
	 (viewable-matrix-inverse-3by3 mat :-> ->))
	(t (error "Determinant only defined for 2by2 or 3by3 viewable-matrices"))))

(defun viewable-matrix-determinant-2by2 (mat &key ->)
  (with-result ((result ->) (aref (data mat) 0 0))
    (let ((data (data mat)))
      (with-local-viewables
	  ((tmp1 (mul (aref data 0 0) (aref data 1 1)))
	   (tmp2 (mul (aref data 0 1) (aref data 1 0))))
	(sub tmp1 tmp2 :-> result)))))

(defun viewable-matrix-determinant-3by3 (mat &key ->)
  (with-result ((result ->) (aref (data mat) 0 0))
    (let ((data (data mat)))
      (with-local-viewables
	  ((tmp1 (similar result))
	   (tmp2 (similar result)))
	(mul (aref data 1 1) (aref data 2 2) :-> tmp1)
	(mul (aref data 2 1) (aref data 1 2) :-> tmp2)
	(sub tmp1 tmp2 :-> tmp1)
	(mul (aref data 0 0) tmp1 :-> result)
	(mul (aref data 2 0) (aref data 1 2) :-> tmp1)
	(mul (aref data 1 0) (aref data 2 2) :-> tmp2)
	(sub tmp1 tmp2 :-> tmp1)
	(mul (aref data 0 1) tmp1 :-> tmp1)
	(add tmp1 result :-> result)
	(mul (aref data 1 0) (aref data 2 1) :-> tmp1)
	(mul (aref data 2 0) (aref data 1 1) :-> tmp2)
	(sub tmp1 tmp2 :-> tmp1)
	(mul (aref data 0 2) tmp1 :-> tmp1)
	(add tmp1 result :-> result)))
    result))

(defun viewable-matrix-inverse-2by2 (mat &key ->)
  (with-result ((result ->) mat)
    (let ((mdata (data mat))
	  (rdata (data result)))
      (copy (aref mdata 1 1) :-> (aref rdata 0 0))
      (copy (aref mdata 0 0) :-> (aref rdata 1 1))
      (negate (aref mdata 0 1) :-> (aref rdata 0 1))
      (negate (aref mdata 1 0) :-> (aref rdata 1 0)))	
    (with-local-viewables ((determinant (determinant mat)))
      (div result determinant :-> result))
    result))

(defun viewable-matrix-inverse-3by3 (mat &key ->)
  (with-result ((result ->) mat)
    (let ((mdata (data mat))
	  (rdata (data result)))
      (with-local-viewables ((determinant (determinant mat))
			     (tmp1 (similar (aref mdata 0 0)))
			     (tmp2 (similar (aref mdata 0 0))))
	(sub (mul (aref mdata 1 1) (aref mdata 2 2) :-> tmp1)
	     (mul (aref mdata 2 1) (aref mdata 1 2) :-> tmp2)
	     :-> (aref rdata 0 0))
	(sub (mul (aref mdata 2 0) (aref mdata 1 2) :-> tmp1)
	     (mul (aref mdata 1 0) (aref mdata 2 2) :-> tmp2)
	     :-> (aref rdata 1 0))
	(sub (mul (aref mdata 1 0) (aref mdata 2 1) :-> tmp1)
	     (mul (aref mdata 2 0) (aref mdata 1 1) :-> tmp2)
	     :-> (aref rdata 2 0))
	(sub (mul (aref mdata 2 1) (aref mdata 0 2) :-> tmp1)
	     (mul (aref mdata 0 1) (aref mdata 2 2) :-> tmp2)
	     :-> (aref rdata 0 1))
	(sub (mul (aref mdata 0 0) (aref mdata 2 2) :-> tmp1)
	     (mul (aref mdata 2 0) (aref mdata 0 2) :-> tmp2)
	     :-> (aref rdata 1 1))
	(sub (mul (aref mdata 2 0) (aref mdata 0 1) :-> tmp1)
	     (mul (aref mdata 0 0) (aref mdata 2 1) :-> tmp2)
	     :-> (aref rdata 2 1))
	(sub (mul (aref mdata 0 1) (aref mdata 1 2) :-> tmp1)
	     (mul (aref mdata 1 1) (aref mdata 0 2) :-> tmp2)
	     :-> (aref rdata 0 2))
	(sub (mul (aref mdata 1 0) (aref mdata 0 2) :-> tmp1)
	     (mul (aref mdata 0 0) (aref mdata 1 2) :-> tmp2)
	     :-> (aref rdata 1 2))
	(sub (mul (aref mdata 0 0) (aref mdata 1 1) :-> tmp1)
	     (mul (aref mdata 1 0) (aref mdata 0 1) :-> tmp2)
	     :-> (aref rdata 2 2))
	(div result determinant :-> result)))
    result))


(defmethod matrix-mul ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (with-result ((result ->) (list :class (class-of mat1)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list (row-dim mat1) (col-dim mat2))
					     (aref (data mat1) 0 0))))
		'matrix-mul mat1 mat2)
    (when (or (eq result mat1) (eq result mat2))
      (error "Result cannot be eq to argument."))
    (check-matrix-mul-compatibility (row-dim mat1) (col-dim mat1)
				    (row-dim mat2) (col-dim mat2)
				    (row-dim result) (col-dim result))
    (with-local-viewables ((temp (similar (aref (data mat1) 0 0))))
      (let ((m1 (data mat1))
	    (m2 (data mat2))
	    (res (data result)))
	(loop for i from 0 below (row-dim m1) do
	      (loop for k from 0 below (col-dim m2) do
		    (zero! (aref res i k))
		    (loop for j from 0 below (col-dim m1) do
			  (add (aref res i k)
			       (mul (aref m1 i j) (aref m2 j k) :-> temp)
			       :-> (aref res i k)))))
	))
    result))

(defmethod matrix-mul-transpose
    ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (with-result ((result ->) (list :class (class-of mat1)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list (row-dim mat1) (row-dim mat2))
					     (aref (data mat1) 0 0))))
		'matrix-mul mat1 mat2)
    (when (or (eq result mat1) (eq result mat2))
      (error "Result cannot be eq to argument."))
    (check-matrix-mul-compatibility (row-dim mat1) (col-dim mat1)
				    (col-dim mat2) (row-dim mat2)
				    (row-dim result) (col-dim result))
    (with-local-viewables ((temp (similar (aref (data mat1) 0 0))))
      (let ((m1 (data mat1))
	    (m2 (data mat2))
	    (res (data result)))
	(loop for i from 0 below (row-dim m1) do
	      (loop for k from 0 below (row-dim m2) do
		    (zero! (aref res i k))
		    (loop for j from 0 below (col-dim m1) do
			  (add (aref res i k)
			       (mul (aref m1 i j) (aref m2 k j) :-> temp)
			       :-> (aref res i k)))))
	))
    result))

(defmethod matrix-transpose-mul
    ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (with-result ((result ->) (list :class (class-of mat1)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list (col-dim mat1) (col-dim mat2))
					     (aref (data mat1) 0 0))))
		'matrix-mul mat1 mat2)
    (when (or (eq result mat1) (eq result mat2))
      (error "Result cannot be eq to argument."))
    (check-matrix-mul-compatibility (col-dim mat1) (row-dim mat1)
				    (row-dim mat2) (col-dim mat2)
				    (row-dim result) (col-dim result))
    (with-local-viewables ((temp (similar (aref (data mat1) 0 0))))
      (let ((m1 (data mat1))
	    (m2 (data mat2))
	    (res (data result)))
	(loop for i from 0 below (col-dim m1) do
	      (loop for k from 0 below (col-dim m2) do
		    (zero! (aref res i k))
		    (loop for j from 0 below (row-dim m1) do
			  (add (aref res i k)
			       (mul (aref m1 j i) (aref m2 j k) :-> temp)
			       :-> (aref res i k)))))
	))
    result))

(defmethod dot-product
    ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (unless (equal (array-dimensions (data mat1)) (array-dimensions (data mat2)))
    (error "Argument viewable-matrices are of different dimensions"))
  (with-result ((result ->) (aref (data mat1) 0 0)
		'dot-product mat1 mat2)
    (when (or (eq result mat1) (eq result mat2))
      (error "Result cannot be eq to argument."))
    (zero! result)
    (with-local-viewables ((temp (similar (aref (data mat1) 0 0))))
      (let ((m1 (vectorize (data mat1)))
	    (m2 (vectorize (data mat2))))
	(loop for i from 0 below (col-dim m1) do
	      (add result (mul (aref m1 i) (aref m2 i) :-> temp) :-> result))))
    result))

(defmethod normalize ((mat viewable-matrix) &key (norm 1.0) ->)
  (with-result ((result ->) mat 'normalize mat)
    (with-local-viewables ((length (dot-product mat mat)))
      (square-root length :-> length)
      (div length norm :-> length)
      (div mat length :-> result)
      result)))

;;; can do destructively into itself
(defmethod normalize-cols ((mat viewable-matrix) &key ->)
  (with-result ((result ->) mat)
    (with-local-viewables
	((norm (similar (aref (obv::data mat) 0 0)))
	 (tmp (similar norm)))
      (loop for col from 0 below (col-dim mat) do
	    (zero! norm)
	    (loop for row from 0 below (row-dim mat) do
		  (add (square (aref (obv::data mat) row col) :-> tmp) norm :-> norm))
	    (square-root norm :-> norm)
	    (loop for row from 0 below (row-dim mat) do
		  (div (aref (obv::data mat) row col) norm
		       :-> (aref (obv::data result) row col)))))
    result))

(defmethod outer-product ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (warn "Outer-product on ~a and ~a is calling matrix-mul-transpose" mat1 mat2)
  (matrix-mul-transpose mat1 mat2 :-> ->))

;;; *** Not finished ***
(defmethod cross-product ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (declare (ignore ->))
  (error "Method not written"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; matrix-mul methods for viewable-matrix,array

(defmethod matrix-mul ((mat viewable-matrix) (arr array) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list (row-dim mat) (col-dim arr))
					     (aref (data mat) 0 0))))
		'matrix-mul mat arr)
    (when (eq result mat) (error "Result cannot be eq to argument."))
    (check-matrix-mul-compatibility (row-dim mat) (col-dim mat)
				    (row-dim arr) (col-dim arr)
				    (row-dim result) (col-dim result))
    (with-local-viewables ((temp (similar (aref (data mat) 0 0))))
      (let ((m1 (data mat))
	    (m2 arr)
	    (res (data result)))
	(loop for i from 0 below (row-dim m1) do
	      (loop for k from 0 below (col-dim m2) do
		    (zero! (aref res i k))
		    (loop for j from 0 below (col-dim m1)
			  for m2-val = (aref m2 j k) do
			  (cond ((zerop m2-val) nil)
				((= m2-val 1.0)
				 (add (aref res i k) (aref m1 i j) :-> (aref res i k)))
				(t (add (aref res i k)
					(mul (aref m1 i j) m2-val :-> temp)
					:-> (aref res i k)))))))))
    result))

(defmethod matrix-mul-transpose ((mat viewable-matrix) (arr array) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list (row-dim mat) (row-dim arr))
					     (aref (data mat) 0 0))))
		'matrix-mul mat arr)
    (when (eq result mat) (error "Result cannot be eq to argument."))
    (check-matrix-mul-compatibility (row-dim mat) (col-dim mat)
				    (col-dim arr) (row-dim arr)
				    (row-dim result) (col-dim result))
    (with-local-viewables ((temp (similar (aref (data mat) 0 0))))
      (let ((m1 (data mat))
	    (m2 arr)
	    (res (data result)))
	(loop for i from 0 below (row-dim m1) do
	      (loop for k from 0 below (row-dim m2) do
		    (zero! (aref res i k))
		    (loop for j from 0 below (col-dim m1)
			  for m2-val = (aref m2 k j) do
			  (cond ((zerop m2-val) nil)
				((= m2-val 1.0)
				 (add (aref res i k) (aref m1 i j) :-> (aref res i k)))
				(t (add (aref res i k)
					(mul (aref m1 i j) m2-val :-> temp)
					:-> (aref res i k)))))))))
    result))

(defmethod matrix-transpose-mul ((mat viewable-matrix) (arr array) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list (col-dim mat) (col-dim arr))
					     (aref (data mat) 0 0))))
		'matrix-mul mat arr)
    (when (eq result mat) (error "Result cannot be eq to argument."))
    (check-matrix-mul-compatibility (col-dim mat) (row-dim mat)
				    (row-dim arr) (col-dim arr)
				    (row-dim result) (col-dim result))
    (with-local-viewables ((temp (similar (aref (data mat) 0 0))))
      (let ((m1 (data mat))
	    (m2 arr)
	    (res (data result)))
	(loop for i from 0 below (col-dim m1) do
	      (loop for k from 0 below (col-dim m2) do
		    (zero! (aref res i k))
		    (loop for j from 0 below (row-dim m1)
			  for m2-val = (aref m2 j k) do
			  (cond ((zerop m2-val) nil)
				((= m2-val 1.0)
				 (add (aref res i k) (aref m1 j i) :-> (aref res i k)))
				(t (add (aref res i k)
					(mul (aref m1 j i) m2-val :-> temp)
					:-> (aref res i k)))))))))
    result))

(defmethod matrix-mul ((arr array) (mat viewable-matrix) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list (row-dim arr) (col-dim mat))
					     (aref (data mat) 0 0))))
		'matrix-mul mat arr)
    (when (eq result mat) (error "Result cannot be eq to argument."))
    (check-matrix-mul-compatibility (row-dim arr) (col-dim arr)
				    (row-dim mat) (col-dim mat)
				    (row-dim result) (col-dim result))
    (with-local-viewables ((temp (similar (aref (data mat) 0 0))))
      (let ((m1 arr)
	    (m2 (data mat))
	    (res (data result)))
	(loop for i from 0 below (row-dim m1) do
	      (loop for k from 0 below (col-dim m2) do
		    (zero! (aref res i k))
		    (loop for j from 0 below (col-dim m1)
			  for m1-val = (aref m1 i j) do
			  (cond ((zerop m1-val) nil)
				((= m1-val 1.0)
				 (add (aref res i k) (aref m2 j k) :-> (aref res i k)))
				(t (add (aref res i k)
					(mul m1-val (aref m2 j k) :-> temp)
					:-> (aref res i k)))))))))
    result))

(defmethod matrix-mul-transpose ((arr array) (mat viewable-matrix) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list (row-dim arr) (row-dim mat))
					     (aref (data mat) 0 0))))
		'matrix-mul mat arr)
    (when (eq result mat) (error "Result cannot be eq to argument."))
    (check-matrix-mul-compatibility (row-dim arr) (col-dim arr)
				    (col-dim mat) (row-dim mat)
				    (row-dim result) (col-dim result))
    (with-local-viewables ((temp (similar (aref (data mat) 0 0))))
      (let ((m1 arr)
	    (m2 (data mat))
	    (res (data result)))
	(loop for i from 0 below (row-dim m1) do
	      (loop for k from 0 below (row-dim m2) do
		    (zero! (aref res i k))
		    (loop for j from 0 below (col-dim m1)
			  for m1-val = (aref m1 i j) do
			  (cond ((zerop m1-val) nil)
				((= m1-val 1.0)
				 (add (aref res i k) (aref m2 k j) :-> (aref res i k)))
				(t (add (aref res i k)
					(mul m1-val (aref m2 k j) :-> temp)
					:-> (aref res i k)))))))))
    result))

(defmethod matrix-transpose-mul ((arr array) (mat viewable-matrix) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list (col-dim arr) (col-dim mat))
					     (aref (data mat) 0 0))))
		'matrix-mul mat arr)
    (when (eq result mat) (error "Result cannot be eq to argument."))
    (check-matrix-mul-compatibility (col-dim arr) (row-dim arr)
				    (row-dim mat) (col-dim mat)
				    (row-dim result) (col-dim result))
    (with-local-viewables ((temp (similar (aref (data mat) 0 0))))
      (let ((m1 arr)
	    (m2 (data mat))
	    (res (data result)))
	(loop for i from 0 below (col-dim m1) do
	      (loop for k from 0 below (col-dim m2) do
		    (zero! (aref res i k))
		    (loop for j from 0 below (row-dim m1)
			  for m1-val = (aref m1 j i) do
			  (cond ((zerop m1-val) nil)
				((= m1-val 1.0)
				 (add (aref res i k) (aref m2 j k) :-> (aref res i k)))
				(t (add (aref res i k)
					(mul m1-val (aref m2 j k) :-> temp)
					:-> (aref res i k)))))))))
    result))

(defmethod dot-product ((mat viewable-matrix) (arr array) &key ->)
  (with-result ((result ->) (aref (data mat) 0 0)
		'dot-product mat arr)
    (when (eq result mat) (error "Result cannot be eq to argument."))
    (zero! result)
    (with-local-viewables ((temp (similar (aref (data mat) 0 0))))
      (let ((m1 (vectorize (data mat)))
	    (m2 (vectorize arr)))
	(loop for i from 0 below (col-dim m1)
	      for m2-val = (aref m2 i) do
	      (cond ((zerop m2-val) nil)
		    ((= m2-val 1.0)
		     (add result (aref m1 i) :-> result))
		    (t (add result (mul (aref m1 i) m2-val :-> temp) :-> result))))))
    result))

(defmethod dot-product ((arr array) (mat viewable-matrix) &key ->)
  (dot-product mat arr :-> ->))

(defmethod outer-product ((arr array) (mat viewable-matrix) &key ->)
  (warn "Outer-product on ~a and ~a is calling matrix-mul-transpose" arr mat)
  (matrix-transpose-mul arr mat :-> ->))

(defmethod outer-product ((mat viewable-matrix) (arr array) &key ->)
  (warn "Outer-product on ~a and ~a is calling matrix-mul-transpose" mat arr)
  (matrix-transpose-mul mat arr :-> ->))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; matrix-mul methods for viewable-matrix,vector

;;; dot-product and outer-product inherit from (viewable-matrix,array)
;;; methods.

;;; *** Not finished ***
(defmethod cross-product ((vec vector) (mat viewable-matrix) &key ->)
  (declare (ignore ->))
  (error "Method not written"))

;;; *** Not finished ***
(defmethod cross-product ((mat viewable-matrix) (vec vector) &key ->)
  (declare (ignore ->))
  (error "Method not written"))

;;; *** Not finished ***
(defmethod matrix-mul ((vec vector) (mat viewable-matrix) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list 1 (col-dim mat))
					     (aref (data mat) 0 0)))))
    (check-matrix-mul-compatibility (row-dim vec) (col-dim vec)
				    (row-dim mat) (col-dim mat)
				    (row-dim result) (col-dim result))
    (error "Method not finished yet")
    result))

;;; *** Not finished ***
(defmethod matrix-mul-transpose ((mat viewable-matrix) (vec vector) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list (row-dim mat) 1)
					     (aref (data mat) 0 0)))))
    (check-matrix-mul-compatibility (row-dim mat) (col-dim mat)
				    (col-dim vec) (row-dim vec)
				    (row-dim result) (col-dim result))
    (error "Method not finished yet")
    result))

;;; *** Not finished ***
(defmethod matrix-mul-transpose ((vec vector) (mat viewable-matrix) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list 1 (col-dim mat))
					     (aref (data mat) 0 0)))))
    (check-matrix-mul-compatibility (row-dim vec) (col-dim vec)
				    (col-dim mat) (row-dim mat)
				    (row-dim result) (col-dim result))
    (error "Method not finished yet")
    result))

;;; These methods give continuable error as in matrix.lisp
(defmethod matrix-mul ((mat viewable-matrix) (vec vector) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list 1 (row-dim mat))
					     (aref (data mat) 0 0)))))
    (cerror "(matrix-mul mat (columnize vec) :-> (columnize res))"
	    "matrix times row-vector doesn't make sense")
    (matrix-mul mat (columnize vec) :-> (columnize result))
    result))

(defmethod matrix-transpose-mul ((vec vector) (mat viewable-matrix) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list 1 (col-dim mat))
					     (aref (data mat) 0 0)))))
    (cerror "(matrix-mul vec mat :-> (vectorize res))"
	    "column-vector times matrix doesn't make sense")
    (matrix-mul vec mat :-> (vectorize result))
    result))

(defmethod matrix-transpose-mul ((mat viewable-matrix) (vec vector) &key ->)
  (with-result ((result ->) (list :class (class-of mat)
				  :data (if (viewable-matrix-p ->)
					    (data ->)
					    (make-viewable-array-data
					     (list 1 (col-dim mat))
					     (aref (data mat) 0 0)))))
    (cerror "(matrix-transpose-mul mat (columnize vec) :-> (columnize res))"
	    "matrix times row-vector doesn't make sense")
    (matrix-transpose-mul mat (columnize vec) :-> (columnize result))
    result))

(defmethod regress ((observed viewable) (predictor viewable) &rest keys)
  (let ((o-data (data observed))
	(p-data (data predictor)))
    (apply 'regress o-data p-data keys)))


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
