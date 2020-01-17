;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: matrix.lisp
;;;  Author:  Simoncelli/Heeger/Chichilnisky
;;;  Description:  Non-pointwise floating point matrix operations 
;;;                (pointwise matrix operations are in array-ops.lisp).
;;;  Creation Date:  Fall, 1988
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)
(export '(matrix-mul  matrix-transpose-mul  matrix-mul-transpose outer-product
	  matrix-transpose normalize make-identity-matrix make-diagonal-matrix
	  cross-product dot-product vector-length vector-distance
	  diagonal make-matrix matrix-trace matrix-inverse
	  square-p diagonal-p identity-p unitary-p symmetric-p anti-symmetric-p
	  orthogonal-p quadratic-form singular-p
	  make-constant-vector vector-length2 weighted-vector-length2))

;;; NOTE: This file provides operations that only work on floating point 
;;; simple-vectors and simple-arrays of rank 2.

;;; Declare matrices to be 2-D arrays of type float.  Declare the vectors 
;;; to be 1-D arrays of type float.

#+MCL
(defmacro declare-matrices (array-list vector-list)
  )

#+Lucid
(defmacro declare-matrices (array-list vector-list)
  `(declare (type (array single-float (* *))
	     ,@(loop for arr in array-list collect arr))
            (type (array single-float (*))
	     ,@(loop for vect in vector-list collect vect))))

;;; Check that the arrays and vectors are as defined by the above declarations.
(defmacro checktype-matrices (array-list)
  `(unless (and ,@(loop for arr in array-list collect
			`(typep ,arr '(array single-float))))
    (error "Matrix operations only defined for floating point arrays.")))

(defmacro check-matrix-mul-compatibility (row1 col1 row2 col2 row3 col3)
  `(unless (and (= ,col1 ,row2) (= ,row1 ,row3) (= ,col2 ,col3))
    (error "Arrays are incompatible for multiplication")))


;;; Note: can matrix-mul back into one of the original arrays, but that conses.
(defmethod matrix-mul ((m1 array) (m2 array)
		       &key ((:-> res) (make-array (list (array-dimension m1 0)
							 (array-dimension m2 1))
						   :element-type 'single-float)))
  (check-matrix-mul-compatibility (row-dim m1) (col-dim m1)
				  (row-dim m2) (col-dim m2)
				  (row-dim res) (col-dim res))
  (checktype-matrices (m1 m2 res))
  (if (or (eq res m1) (eq res m2))
      (progn (warn "matrix multiplication: source and destination are the same...consing a copy" )
	     (copy (matrix-mul m1 m2) :-> res))
      (internal-matrix-mul m1 (row-dim m1) (col-dim m1)
			   m2 (row-dim m2) (col-dim m2)
			   res (row-dim res) (col-dim res)))
  
  res)


(defmethod matrix-mul-transpose ((m1 array) (m2 array) 
				 &key ((:-> res) (make-array (list (array-dimension m1 0) 
								   (array-dimension m2 0))
							     :element-type 'single-float)))
  (check-matrix-mul-compatibility (row-dim m1) (col-dim m1)
				  (col-dim m2) (row-dim m2)
				  (row-dim res) (col-dim res))
  (checktype-matrices (m1 m2 res))
  (if (or (eq res m1) (eq res m2))
      (progn (warn "matrix multiplication: source and destination are the same...consing a copy")
	     (copy (matrix-mul-transpose m1 m2) :-> res))
      (internal-matrix-mul-transpose m1 (row-dim m1) (col-dim m1)
				     m2 (row-dim m2) (col-dim m2)
				     res (row-dim res) (col-dim res)))
      
  res)

(defmethod matrix-transpose-mul ((m1 array) (m2 array) 
				 &key ((:-> res) (make-array (list (array-dimension m1 1) 
								   (array-dimension m2 1))
							     :element-type 'single-float)))
  (check-matrix-mul-compatibility (col-dim m1) (row-dim m1)
				  (row-dim m2) (col-dim m2)
				  (row-dim res) (col-dim res))
  (checktype-matrices (m1 m2 res))
  (if (or (eq res m1) (eq res m2))
      (progn (warn "matrix multiplication: source and destination are the same...consing a copy")
	     (copy (matrix-transpose-mul m1 m2) :-> res))
      (internal-matrix-transpose-mul m1 (row-dim m1) (col-dim m1)
				     m2 (row-dim m2) (col-dim m2)
				     res (row-dim res) (col-dim res)))
  res)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matrix multiplication

(defmethod matrix-mul
    ((v vector) (m array)
     &key ((:-> res) (make-array (col-dim m) :element-type (array-element-type v))))
  (check-matrix-mul-compatibility (row-dim v) (col-dim v)
				  (row-dim m) (col-dim m)
				  (row-dim res) (col-dim res))
  (checktype-matrices (m v res))
  (if (eq res v)
      (progn (warn "matrix multiplication: source and destination are the same...consing a copy")
	     (copy (matrix-mul v m) :-> res))
      (internal-matrix-mul v (row-dim v) (col-dim v)
			   m (row-dim m) (col-dim m)
			   res (row-dim res) (col-dim res)))
  res)

(defmethod matrix-mul-transpose
    ((m array) (v vector)
     &key ((:-> res) (make-array (list (row-dim m) 1) :element-type (array-element-type v))))
  (check-matrix-mul-compatibility (row-dim m) (col-dim m)
				  (col-dim v) (row-dim v)
				  (row-dim res) (col-dim res))
  (checktype-matrices (m res v))
  (internal-matrix-mul-transpose m (row-dim m) (col-dim m)
				 v (row-dim v) (col-dim v)
				 res (row-dim res) (col-dim res))
  res)

(defmethod matrix-mul-transpose
    ((v vector) (m array)
     &key ((:-> res) (make-array (row-dim m) :element-type (array-element-type v))))
  (check-matrix-mul-compatibility (row-dim v) (col-dim v)
				  (col-dim m) (row-dim m)
				  (row-dim res) (col-dim res))
  (checktype-matrices (m v res))
  (if (eq res v)
      (progn (warn "matrix multiplication: source and destination are the same...consing a copy")
	     (copy (matrix-mul v m) :-> res))
      (internal-matrix-mul-transpose v (row-dim v) (col-dim v)
				     m (row-dim m) (col-dim m)
				     res (row-dim res) (col-dim res)))
  res)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quadratics

;; Return A * B * A^t.
(defmethod quadratic-form ((vector vector) (matrix array) &key ->)
  (declare (ignore ->))
  (check-size-rows vector matrix)
  (internal-qf-row-arr vector matrix (row-dim matrix) (col-dim matrix)))

;; M0 * M1 * M0^t
;; *** should use a C function
(defmethod quadratic-form ((matrix-0 array) (matrix-1 array) &key
			   ((:-> result) (similar matrix-0))
			   (static (allocated-array-p result)))
  (check-size matrix-0 matrix-1)
  (with-static-arrays ((tmp (similar result :static static)))
    (matrix-mul matrix-0 matrix-1 :-> tmp)
    (matrix-mul-transpose tmp matrix-0 :-> result)))


#|
(setf mat (make-matrix '((1 2 3) (4 5 6) (7 8 9))))
(setf vec (make-matrix 1 4 2))

;; These must give the same answer!
(dot-product (matrix-mul vec mat) vec)
(quadratic-form vec mat)
|#


(defmethod matrix-transpose
    ((v vector) &key ((:-> res) (make-array (list (col-dim v) 1)
					    :element-type (array-element-type v))))
  (unless (and (= (row-dim v) (col-dim res)) (= (col-dim v) (row-dim res)))
    (error "Arrays are incompatible for transposing"))
  (copy v :-> (vectorize res))
  res)

;;; Put in this new method 5.22.92 since earlier one failed with
;;; non-float matrices. EJC.
(defmethod matrix-transpose
    ((m array) &key ((:-> res) (make-array (list (col-dim m) (row-dim m))
					   :element-type (array-element-type m))))
  (cond ((float-arrays-p m res)
	 (unless (and (= (row-dim m) (col-dim res)) (= (col-dim m) (row-dim res)))
	   (error "Arrays are incompatible for transposing"))
	 (internal-matrix-transpose m (array-dimension m 0) (array-dimension m 1) res))
	(t (if (eq res m)
	       (progn (warn "Transpose: source and destination are the same...consing a copy")
		      (copy (matrix-transpose m) :-> res))
	       (let ((element-type (array-element-type res)))
		 (dotimes (row (row-dim m))
		   (declare (fixnum row))
		   (dotimes (col (col-dim m))
		     (declare (fixnum col))
		     (setf (aref res col row) (coerce (aref m row col) element-type))))))))
  res)

#|
(defmethod matrix-transpose
    ((m array) &key ((:-> res) (make-array (list (col-dim m) (row-dim m))
					   :element-type 'single-float)))
  (declare-matrices (m res) ())
  (checktype-matrices (m res))
  (unless (and (= (row-dim m) (col-dim res)) (= (col-dim m) (row-dim res)))
    (error "Arrays are incompatible for transposing"))
  (internal-matrix-transpose m (array-dimension m 0) (array-dimension m 1) res)
  res)
|#

(defmethod matrix-mul-transpose
    ((v1 vector) (v2 vector)
     &key ((:-> res) (make-array 1 :element-type (array-element-type v1))))
  (matrix-mul v1 (columnize v2) :-> res))

(defmethod matrix-transpose-mul
    ((v1 vector) (v2 vector)
     &key ((:-> res) (make-array (list (length v1) (length v2))
				 :element-type (array-element-type v1))))
  (declare-matrices (res) (v1 v2))
  (checktype-matrices (res v1 v2))
  (check-matrix-mul-compatibility (col-dim v1) (row-dim v1)
				  (row-dim v2) (col-dim v2)
				  (row-dim res) (col-dim res))
  (internal-matrix-transpose-mul v1 (row-dim v1) (col-dim v1)
				 v2 (row-dim v2) (col-dim v2)
				 res (row-dim res) (col-dim res))
  res)

(defmethod outer-product
    ((v1 vector) (v2 vector)  
     &key ((:-> res) (make-array (list (length v1) (length v2))
				 :element-type 'single-float)))
  (matrix-transpose-mul v1 v2 :-> res))


;;; Returns the norm as a second value.
(defmethod normalize ((v vector) &key (norm 1.0) ((:-> res) (similar v)))
  (declare-matrices () (v res))
  (checktype-matrices (v res))
  (let ((divisor (/ (sqrt (loop for i from 0 below (length v)
				summing (sqr (aref v i))))
		    norm)))
    (internal-sc-mul v res (array-total-size v) (float (/-0 1.0 divisor)))
    (values res divisor)))

(defmethod normalize ((arr array) &key ((:-> result) (similar arr)))
  (checktype-matrices (arr result))
  (multiple-value-bind (vec mag) (normalize (vectorize arr) :-> (vectorize result))
    (declare (ignore vec))
    (values result mag)))

(defmethod cross-product ((v1 vector) (v2 vector) 
		      &key ((:-> res) 
			    (make-array 3 :element-type 'single-float)))
  (declare-matrices () (v1 v2 res))
  (checktype-matrices (v1 v2 res))
  (when (/= 3 (car (dimensions v1))
	      (car (dimensions v2))
	      (car (dimensions res)))
    (error "Vectors must be three-dimensional"))
  (setf (aref res 0) (- (* (aref v1 1) (aref v2 2))
			(* (aref v1 2) (aref v2 1))))
  (setf (aref res 1) (- (* (aref v1 2) (aref v2 0))
			(* (aref v1 0) (aref v2 2))))
  (setf (aref res 2) (- (* (aref v1 0) (aref v2 1))
			(* (aref v1 1) (aref v2 0))))
  res)

(defmethod vector-length ((arr array))
  (checktype-matrices (arr))
  (sqrt (internal-dot-product arr arr (total-size arr))))

(defmethod vector-length2 ((arr array))
  (dot-product arr arr))

(defmethod weighted-vector-length2 ((vect vector) (mat array))
  (dot-product (matrix-mul vect mat) vect))

(defun vector-distance (v1 v2)
  (sqrt (sum-square-error v1 v2)))

(defmethod dot-product ((arr-1 array) (arr-2 array) &key ->)
  (declare (ignore ->))
  (check-size arr-1 arr-2)
  (cond ((float-arrays-p arr-1 arr-2)
	 (internal-dot-product arr-1 arr-2 (total-size arr-1)))
	((1bit-arrays-p arr-1 arr-2)
	 (internal1-dot-product arr-1 arr-2 (total-size arr-1)))
	(t (error "Dot-product only implemented for single-float or bit arrays"))))

;; *** should be C code 
(defun internal1-dot-product (arr-1 arr-2 size)
  (declare (ignore size))
  (let ((result 0))
    (with-displaced-vectors ((vec-1 arr-1)
			     (vec-2 arr-2))
      (declare (type (array bit (*)) vec-1 vec-2))
      (dotimes (i (length vec-1))
	(declare (fixnum i))
	(incf result (* (aref vec-1 i) (aref vec-2 i)))))
    (float result)))

(defmethod tolerance ((arr array) &optional (tolerance *tolerance*)
		      &key ((:-> result) (similar arr)))
  (checktype-matrices (arr result))
  (check-size arr result)
  (with-displaced-vectors ((d-res result)
			   (d-arr arr))
    (dotimes (i (length d-res))
      (setf (aref d-res i) (tolerance (Aref d-arr i) tolerance))))
  result)

(defmethod make-matrix ((x vector) &rest vectors)
  (setq vectors (cons x vectors))
  (apply 'check-size vectors)
  (let* ((dimensions (list (length vectors) (length (first vectors))))
	 (result (make-array dimensions :element-type 'single-float)))
    (loop for vec in vectors
	  for row from 0
	  do (copy vec :-> (displaced-row row result)))
    result))

(defmethod make-matrix ((x array) &rest arrays)
  (setq arrays (cons x arrays))
  (apply 'check-size arrays)
  (let* ((rows (* (row-dim (first arrays)) (length arrays)))
         (cols (col-dim (first arrays)))
         (result (make-array (list rows cols) :element-type 'single-float)))
    (loop for arr in arrays
          with y-offset = 0
          do
          (paste arr result :-> result :y y-offset)
          (incf y-offset (row-dim arr)))
     result))

#|
;;; old version replaced 6/11/93, bug in calling check-size
(defmethod make-matrix ((x array) &rest arrays)
  (setq arrays (cons x arrays))
  (apply 'row-check-size arrays)
  (let* ((rows (* (row-dim (first arrays)) (length arrays)))
	 (cols (col-dim (first arrays)))
	 (result (make-array (list rows cols) :element-type 'single-float)))
    (loop for arr in arrays
	  with y-offset = 0
	  do
	  (paste arr result :-> result :y y-offset)
	  (incf y-offset (row-dim arr)))
    result))
|#

(defmethod make-matrix ((x number) &rest numbers)
  (setq numbers (cons x numbers))
  (fill! (make-array (length numbers) :element-type 'single-float)  numbers))

;; *** There is a hack here to deal with numbers,
;; since the more elegant method fails when the list is longer
;; than (apply) can work with
(defmethod make-matrix ((x list) &rest lists)
  (if lists
      (apply 'make-matrix (mapcar 'make-matrix (cons x lists)))
      (if (numberp (first x))
	  (fill! (make-array (length x) :element-type 'single-float)  x)
	  (apply 'make-matrix x))))
#|
;; test code
(make-matrix '(1 2 3))
(make-matrix 1 2 3)
(make-matrix '((1 2) (3 4)))
(make-matrix (list (make-matrix 1 2) (make-matrix 3 4)))
|#

(defun make-identity-matrix (size &key ((:-> result) (make-array (list size size) 
								 :element-type 'single-float))
				  (zero t))
  (when zero (zero! result))
  (dotimes (i (min (row-dim result) (col-dim result)))
    (declare (fixnum i))
    (setf (aref result i i) 1.0))
  result)

(defun make-constant-vector (size value &key ((:-> res)))
  (cond ((null res)
	 (make-array size :element-type 'single-float
		     :initial-element (coerce value 'single-float)))
	((and (typep res '(array single-float (*)))
	      (= (length res) size))
	 (fill! res value)
	 res)
	(t (error "result argument is not a single-float vector of length ~A"
		  size))))

(defmethod make-diagonal-matrix ((diagonals list) &key (zero t) ((:-> result)))
  (let ((size (length diagonals)))
    (unless result (setq result (make-array (list size size) :element-type 'single-float)))
    (when zero (zero! result))
    (dotimes (index size)
      (setf (aref result index index) (float (elt diagonals index))))
    result))

(defmethod make-diagonal-matrix ((diagonals vector) &key (zero t) ((:-> result)))
  (declare-matrices (result) (diagonals))
  (let ((size (length diagonals)))
    (unless result (setq result (make-array (list size size) :element-type 'single-float)))
    (when zero (zero! result))
    (dotimes (index size)
      (declare (fixnum index))
      (setf (aref result index index) (float (aref diagonals index))))
    result))

;;; Return a vector of the diagonal elements
(defun diagonal (matrix &key ((:-> result) (similar matrix :dimensions
						    (min (x-dim matrix) (y-dim matrix)))))
  (dotimes (index (length result))
    (setf (aref result index) (aref matrix index index)))
  result)

;; faster than old version
(defun matrix-trace (arr)
  (let ((result 0.0)
	(rows (row-dim arr))
	(cols (col-dim arr)))
    (assert (= rows cols))
    (dotimes (i rows)
      (declare (fixnum i))
      (incf result (aref arr i i)))
    result))
#|
(defun matrix-trace (arr)
  (assert (= (row-dim arr) (col-dim arr)))
  (loop for i from 0 below (col-dim arr)
	sum (aref arr i i)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful predicates for handling matrices.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun square-p (matrix)
  (when (= (row-dim matrix) (col-dim matrix))
    matrix))

(defun diagonal-p (matrix &key (tolerance *tolerance*))
  (with-static-arrays ((diagonal (diagonal matrix))
		       (diagonal-matrix (make-diagonal-matrix diagonal)))
    (when (almost-equal diagonal-matrix matrix :tolerance tolerance)
      matrix)))

(defun identity-p (matrix &key (tolerance *tolerance*))
  (when (and (square-p matrix)
	     (diagonal-p matrix :tolerance tolerance)
	     (with-static-arrays ((diagonal (diagonal matrix)))
	       (almost-equal 1.0 diagonal :tolerance tolerance)))
    matrix))

(defun unitary-p (matrix &key (tolerance *tolerance*))
  (with-static-arrays ((product (matrix-mul-transpose matrix matrix)))
    (when (identity-p product :tolerance tolerance)
      matrix)))

(defmethod symmetric-p ((matrix array) &key (tolerance *tolerance*))
  (when (square-p matrix)
    (with-static-arrays ((transpose (similar matrix)))
      (matrix-transpose matrix :-> transpose)
      (when (almost-equal matrix transpose :tolerance tolerance)
	matrix))))

(defmethod symmetric-p ((v vector) &rest args)
  (declare (ignore args))
  (loop with symm = t
	for i from 0 below (floor (length v) 2)
	for N-i = (- (length v) 1) then (- N-i 1)
	while (setq symm (almost-equal (aref v i) (aref v N-i)))
	finally (return (and symm t))))

(defmethod anti-symmetric-p ((v vector) &rest args)
  (declare (ignore args))
  (loop with asymm = t
	for i from 0 below (floor (length v) 2)
	for N-i = (- (length v) 1) then (- N-i 1)
	while (setq asymm (almost-equal (- (aref v i)) (aref v N-i)))
	finally (return (and asymm t))))

(defmethod orthogonal-p ((v1 vector) (v2 vector) &key (tolerance *tolerance*))
  (when (almost-equal 0.0 (dot-product v1 v2) :tolerance tolerance)
    v1))

(defmethod orthogonal-p ((arr array) (arr2 array) &key (tolerance *tolerance*))
  (with-static-arrays ((product (matrix-mul-transpose arr arr2)))
    (when (almost-equal 0.0 product :tolerance tolerance)
      arr)))

(defmethod orthogonal-p ((arr array) (vec vector) &key (tolerance *tolerance*))
  (with-static-arrays ((product (matrix-mul arr vec)))
    (when (almost-equal 0.0 product :tolerance tolerance)
      arr)))

(defmethod orthogonal-p ((vec vector) (arr array) &key (tolerance *tolerance*))
  (with-static-arrays ((product (matrix-mul arr vec)))
    (when (almost-equal 0.0 product :tolerance tolerance)
      vec)))

(defun singular-p (matrix &key (tolerance *tolerance*))
  (when (< (/ 1 (condition-number matrix)) tolerance)
    matrix))



;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
