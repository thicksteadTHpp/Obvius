;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: svd.lisp
;;;  Author: Chichilnisky
;;;  Description: Interface to Linpack SVD. Various SVD-based utilities.
;;;  Creation Date: August, 1992
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '(singular-value-decomposition svd singular-values with-static-svd with-svd
	  principal-components row-space row-null-space col-space col-null-space
	  singular-values left-singular-matrix right-singular-matrix
	  solve-eigenvalue matrix-inverse condition-number determinant
	  quadratic-decomposition))

(defmacro svd (&rest args)
  `(singular-value-decomposition  ,@args))

(defun left-singular-matrix (matrix)
  (multiple-value-bind (d u v) (svd matrix)
    (declare (ignore d v))
    u))

(defun right-singular-matrix (matrix)
  (multiple-value-bind (d u v) (svd matrix)
    (declare (ignore d u))
    (with-static-arrays ((vt (copy v)))
      (matrix-transpose vt :-> v))))

;; *** need a better way to handle SVD arguments, here and other places
;; *** note that SVD ignores sign of determinant!!
(defmethod determinant ((matrix array) &key (ignore-zeros nil) q r u v
                        suppress-warning)
  (declare-matrices (matrix) nil)
  (when (and (or q r) (not suppress-warning))
    (warn "QR decomposition not used now, using SVD and ignoring q,r keywords. "))
  (let ((case (cond ((not (square-p matrix)) 0)
                    ((not ignore-zeros) (row-dim matrix))
                    (t t))))
    (case case
      (0 0.0)
      (1 (aref matrix 0 0))
      (2 (- (* (aref matrix 0 0) (aref matrix 1 1))
            (* (aref matrix 1 0) (aref matrix 0 1))))
      (3 (- (+ (* (aref matrix 0 0) (aref matrix 1 1) (aref matrix 2 2))
               (* (aref matrix 1 0) (aref matrix 2 1) (aref matrix 0 2))
               (* (aref matrix 2 0) (aref matrix 0 1) (aref matrix 1 2)))
            (+ (* (aref matrix 0 2) (aref matrix 1 1) (aref matrix 2 0))
               (* (aref matrix 1 2) (aref matrix 2 1) (aref matrix 0 0))
               (* (aref matrix 2 2) (aref matrix 0 1) (aref matrix 1 0)))))
      (t (unless suppress-warning
           (warn "Bug in determinant: absolute value of determinant returned"))
         (let* ((s (svd matrix :u u :v v))
                (val 1.0)
                elt)
           (dotimes (i (length s))
             (setq elt (aref s i))
             (when (or (> (abs elt) short-float-epsilon) (not ignore-zeros))
               (setq val (* val elt))))
           val)))))
#|
(determinant (make-matrix '((1 2) (2 1))))
(determinant (make-matrix '((1 2) (2 1))) :ignore-zeros t)
(determinant (make-matrix '((1 0) (0 1))))
(determinant (make-matrix '((-1 0) (0 1))))
(determinant (make-matrix '((1 0 0) (0 1 0))))
(determinant (make-matrix '((1 0) (0 0) (1 0))))
(determinant (randomize (make-array '(4 4) :element-type 'single-float) 1.0))
(determinant (make-diagonal-matrix (vector 1 2 3 4)))
(determinant-qr (make-diagonal-matrix (vector -1 2 3 4)))

(determinant (make-diagonal-matrix (vector 1 2 3)))
(determinant (make-diagonal-matrix (vector -1 2 3)))

(determinant (make-matrix '((8 2 3) (4 5 6) (7 20 9))))
(determinant (mul -1.0 (make-matrix '((8 2 3) (4 5 6) (7 20 9)))))
|#


#|
;; ** old version
;;; Implemented with dummy q and r keywords to avoid breaking code
;;; that relies on teh qr decomposition.
(defmethod determinant ((matrix array) &key (ignore-zeros nil) u v q r)
  (when (or q r)
    (warn "QR decomposition not used now, using SVD and ignoring q,r keywords. "))
  (if (and (not ignore-zeros) (> (col-dim matrix) (row-dim matrix)))
      0.0
      (loop with singular-values = (svd matrix :u u :v v)
	    with val = 1.0
	    for i from 0 below (length singular-values)
	    for elt = (aref singular-values i)
	    do (when (or (> (abs elt) short-float-epsilon) (not ignore-zeros))
		 (setq val (* val elt)))
	    finally return val)))
|#

(defmacro with-svd ((s u v) matrix . body)
  `(let* ((rows (row-dim ,matrix))
	  (cols (col-dim ,matrix)))
    (with-static-arrays ((,s (similar ,matrix :dimensions (min rows cols)))
			 (,u (similar ,matrix :dimensions (list rows rows)))
			 (,v (similar ,matrix :dimensions (list cols cols))))
      (svd ,matrix :s ,s :u ,u :v ,v)
      (unwind-protect (progn ,@body)))))

(defmacro with-static-svd ((s u v) matrix . body)
  `(let* ((rows (row-dim ,matrix))
	  (cols (col-dim ,matrix)))
    (with-static-arrays ((,s (similar ,matrix :static t :dimensions (min rows cols)))
			 (,u (similar ,matrix :static t :dimensions (list rows rows)))
			 (,v (similar ,matrix :static t :dimensions (list cols cols))))
      (svd ,matrix :s ,s :u ,u :v ,v)
      (unwind-protect (progn ,@body)))))

(defun singular-values (matrix)
  (with-svd (s u v) matrix
    (copy s :-> (similar matrix :dimensions (dimensions s)))))

(defun singular-value-decomposition (matrix &key u v s)
  
  (let ((rows (row-dim matrix))
	(cols (col-dim matrix))
	first-good-value)
    
    ;; Make space for the results
    (unless u (setq u (similar matrix :dimensions (list rows rows))))
    (unless v (setq v (similar matrix :dimensions (list cols cols))))
    (unless s (setq s (similar matrix :dimensions (min rows cols))))

    (checktype-matrices (u s v))
    (unless (and (= (row-dim u) (col-dim u) rows)
		 (= (row-dim v) (col-dim v) cols)
		 (= (length s) (min rows cols)))
      (error "Dimensions of result arrays passed to SVD are invalid"))
    
    (with-static-arrays ((u-t (allocate-array (reverse (dimensions u)) :element-type 'single-float))
			 (v-t (allocate-array (reverse (dimensions v)) :element-type 'single-float))
			 (matrix-t (allocate-array (list cols rows)  :element-type 'single-float))
			 (tmp-col (allocate-array rows :element-type 'single-float))
			 (tmp-row (allocate-array cols :element-type 'single-float))
			 (tmp-s (allocate-array (+ 1 (length s))  :element-type 'single-float)))
      
      (matrix-transpose matrix :-> matrix-t)
			  
      ;; Call the C routine with all the transposed tmp arrays.
      (setq first-good-value (internal-svd matrix-t rows cols tmp-s u-t v-t tmp-col tmp-row))
      (matrix-transpose u-t :-> u)
      (matrix-transpose v-t :-> v)
      (copy (vectorize tmp-s :size (length s)) :-> s)

      (unless (zerop first-good-value)
	(warn "SVD only good beyond ~ath singular value; proceed with caution" first-good-value))
      (values s u v))))

#|
(defun singular-value-decomposition (matrix &key u v s)
  
  (let ((rows (row-dim matrix))
	(cols (col-dim matrix))
	first-good-value)
    
    ;; Setup and error-checking
    (unless u (setq u (similar matrix :dimensions (list rows rows))))
    (unless v (setq v (similar matrix :dimensions (list cols cols))))
    (unless s (setq s (similar matrix :dimensions (min rows cols))))

    (checktype-matrices (matrix u v s))
    (unless (and (= (row-dim u) (col-dim u) rows)
		 (= (row-dim v) (col-dim v) cols)
		 (= (length s) (min rows cols)))
      (error "Dimensions of working arrays passed to SVD are invalid"))
    
    ;; The real work
    (setq first-good-value (internal-svd matrix rows cols u s v))
    
    (if (zerop first-good-value)
	(values s u v)
	(warn "SVD only good beyond ~ath singular value, nil returned" first-good-value))))
|#
#|
;;; Test SVD

(setq A (make-array '(10 5) :element-type 'single-float))
(randomize A 1.0 :-> A)

(time (multiple-value-setq (W U V) (singular-value-decomposition A :u nil :v nil)))
(time (multiple-value-setq (W U V) (singular-value-decomposition A)))

;;; rows and cols of V and U should be orthogonal
(progn
  (assert (identity-p (matrix-transpose-mul V V)))
  (assert (identity-p (matrix-mul-transpose V V)))
  (assert (identity-p (matrix-transpose-mul U U)))
  (assert (identity-p (matrix-mul-transpose U U))))

;;; compare original array with decomposition
(setq Wmat (make-array (dimensions A) :element-type 'single-float :initial-element 0.0))
(loop for i from 0 below (x-dim W) do
      (setf (aref Wmat i i) (aref W i)))
(range (sub A (matrix-mul-transpose (matrix-mul U Wmat) V)))

;;; Test of standard svd bug
(setq A (make-array '(3 3) :element-type 'single-float
		    :initial-contents '((0.0 1.0 0.0)
					(0.0 1.0 1.0)
					(0.0 0.0 0.0))))
(multiple-value-setq (W U V) (singular-value-decomposition A))
(setq Wmat (make-array '(3 3) :element-type 'single-float :initial-element 0.0))
(loop for i from 0 below (array-dimension W 0) do
      (setf (aref Wmat i i) (aref W i)))
(range (sub A (matrix-mul-transpose (matrix-mul U Wmat) V)))


(setq A (make-array '(3 3) :element-type 'single-float
		    :initial-contents '((1.0 2.0 3.0)
					(4.0 5.0 6.0)
					(7.0 8.0 9.0))))
(multiple-value-setq (W U V) (singular-value-decomposition A))
(setq Wmat (make-array '(3 3) :element-type 'single-float :initial-element 0.0))
(loop for i from 0 below (array-dimension W 0) do
      (setf (aref Wmat i i) (aref W i)))
(range (sub A (matrix-mul-transpose (matrix-mul U Wmat) V)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Utilities that make use of SVD:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod col-space ((m array) &key ((:-> result)) (singular-value-limit *tolerance*)
		      (if-does-not-exist nil))
  (with-static-svd (s u v) m
   (let ((col-dim (or (position singular-value-limit s :test #'>)
		      (length s))))
     (cond ((plusp col-dim)
	    (unless result
	      (setq result (similar m :dimensions (list (row-dim m) col-dim))))
	    (crop u :x 0 :x-dim col-dim :-> result))
	   ((equal if-does-not-exist :error)
	    (error "Matrix is degenerate"))
	   ((equal if-does-not-exist :zero)
	    (unless result
	      (setq result (similar m :dimensions (list (row-dim m) 1))))
	    (zero! result))
	   (t nil)))))

(defmethod row-space ((m array) &key ((:-> result)) (singular-value-limit *tolerance*)
		      (if-does-not-exist nil))
  (with-static-svd (s u v) m
   (let ((row-dim (or (position singular-value-limit s :test #'>)
		      (length s))))
     (cond ((plusp row-dim)
	    (unless result
	      (setq result (similar m :dimensions (list row-dim (col-dim m)))))
	    (with-static-arrays ((vt (copy v)))
	      (matrix-transpose vt :-> v)
	      (crop v :y 0 :y-dim row-dim :-> result)))
	   ((equal if-does-not-exist :error)
	    (error "Matrix is degenerate"))
	   ((equal if-does-not-exist :zero)
	    (unless result
	      (setq result (similar m :dimensions (list 1 (col-dim m)))))
	    (zero! result))
	   (t nil)))))
  
(defmethod row-null-space ((m array) &key ((:-> result)) (singular-value-limit *tolerance*)
			   (if-does-not-exist nil))
  (with-static-svd (s u v) m
   (let* ((row-dim (or (position singular-value-limit s :test #'>)
		       (length s)))
	  (row-null-dim (- (col-dim m) row-dim)))
     (cond ((plusp row-null-dim)
	    (unless result
	      (setq result (similar m :dimensions (list row-null-dim (col-dim m)))))
	    (with-static-arrays ((vt (copy v)))
	      (matrix-transpose vt :-> v)
	      (crop v :y row-dim :y-dim row-null-dim :-> result)))
	   ((equal if-does-not-exist :error)
	    (error "Matrix is degenerate"))
	   ((equal if-does-not-exist :zero)
	    (unless result
	      (setq result (similar m :dimensions (list 1 (col-dim m)))))
	    (zero! result))
	   (t nil)))))

(defmethod col-null-space ((m array) &key ((:-> result)) (singular-value-limit *tolerance*)
			   (if-does-not-exist nil))
  (with-static-svd (s u v) m
   (let* ((col-dim (or (position singular-value-limit s :test #'>)
		       (length s)))
	  (col-null-dim (- (row-dim m) col-dim)))
     (cond ((plusp col-null-dim)
	    (unless result
	      (setq result (similar m :dimensions (list (row-dim m) col-null-dim))))
	    (crop u :x col-dim :x-dim col-null-dim :-> result))
	   ((equal if-does-not-exist :error)
	    (error "Matrix is degenerate"))
	   ((equal if-does-not-exist :zero)
	    (unless result
	      (setq result (similar m :dimensions (list (row-dim m) 1))))
	    (zero! result))
	   (t nil)))))

#|
;;; Old versions (4/94) -DH

(defmethod col-space ((m array) &key ((:-> result)) (singular-value-limit *tolerance*))
  (with-static-svd (s u v) m
   (let ((col-dim (or (position singular-value-limit s :test #'>)
		      (length s))))
     (unless (plusp col-dim) (error "Matrix is degenerate"))
     (unless result
       (setq result (similar m :dimensions (list (row-dim m) col-dim))))
     (crop u :x 0 :x-dim col-dim :-> result))))

(defmethod row-space ((m array) &key ((:-> result)) (singular-value-limit *tolerance*))
  (with-static-svd (s u v) m
   (let ((row-dim (or (position singular-value-limit s :test #'>)
		      (length s))))
     (unless (plusp row-dim) (error "Matrix is degenerate"))
     (unless result
       (setq result (similar m :dimensions (list row-dim (col-dim m)))))
     (with-static-arrays ((vt (copy v)))
       (matrix-transpose vt :-> v)
       (crop v :y 0 :y-dim row-dim :-> result)))))

(defmethod row-null-space ((m array) &key ((:-> result)) (singular-value-limit *tolerance*))
  (with-static-svd (s u v) m
   (let* ((row-dim (or (position singular-value-limit s :test #'>)
		       (length s)))
	  (row-null-dim (- (col-dim m) row-dim)))
     (unless (plusp row-null-dim) (error "Matrix has full row rank"))
     (unless result
	(setq result (similar m :dimensions (list row-null-dim (col-dim m)))))
     (with-static-arrays ((vt (copy v)))
       (matrix-transpose vt :-> v)
       (crop v :y row-dim :y-dim row-null-dim :-> result)))))

(defmethod col-null-space ((m array) &key ((:-> result)) (singular-value-limit *tolerance*))
  (with-static-svd (s u v) m
   (let* ((col-dim (or (position singular-value-limit s :test #'>)
		       (length s)))
	  (col-null-dim (- (row-dim m) col-dim)))
     (unless (plusp col-null-dim) (error "Matrix has full column rank"))
     (unless result
       (setq result (similar m :dimensions (list (row-dim m) col-null-dim))))
     (crop u :x col-dim :x-dim col-null-dim :-> result))))
|#

#|
;; Bunch o' matrices
(print-values (setq foo (make-matrix '((1 0 0)
				       (0 1 0)))))
(print-values (setq foo (make-matrix '((1 0 0)
				       (1 0 0)
				       (0 1 0)))))
(print-values (setq foo (make-matrix '((1 0 0)
				       (0 0 1)
				       (0 1 0)))))
(print-values (setq foo (make-matrix '((2 0 0 0)
				       (0 3 0 0)))))
;; This last one tests :singular-value-limit assuming you are using
;; the default value of *tolerance* = 1e-6.
(print-values (setq foo (make-matrix '((1 0)
				       (1 1e-6)))))

;; Don't forget to test the transposes of each of them
(print-values (setq foo (matrix-transpose foo)))

;; test code
(print-values (row-space foo :if-does-not-exist :zero))
(print-values (row-null-space foo :if-does-not-exist :zero))
(print-values (col-space foo :if-does-not-exist :zero))
(print-values (col-null-space foo :if-does-not-exist :zero))
|#


(defmethod condition-number ((matrix array))
  (with-svd (s u v) matrix
      (/-0 (maximum s) (minimum s) most-positive-single-float)))

;;; Fixes: didn't used to pass the U,V,S matrices, minimization code
;;; consed and was slow.
;;; General, fairly efficient function to find minimum item in a sequence.
;;; returns minimum-value item and its position in the sequence.
(defun solve-eigenvalue (matrix
			 &key
			 (U (make-array (dimensions matrix) :element-type 'single-float))
			 (V (make-array (dimensions matrix) :element-type 'single-float))
			 (S (make-array (x-dim matrix) :element-type 'single-float))
			 ((:-> e-vector) (make-array (x-dim matrix) :element-type 'single-float)))
  (singular-value-decomposition matrix :u U :v V :s S)
  (multiple-value-bind (min-ev min-ev-pos) (minimize S)
    (declare (ignore min-ev))
    ;;Fill the e-vector with elements from the pos-of-min column of U.
    (dotimes (i (array-total-size e-vector))
      (setf (aref e-vector i) (aref U i min-ev-pos))))
  (values e-vector S))


#|
;;; *** replaced by Eero's version, 11.20.92
;;; Find unit vector x such that x minimizes x A x,
;;; for square symmetric matrix A.  
;;; Find eigenvector corresponding to the smallest eigenvalue of A.
;;; Use SVD to get eigenvals and eigenvects of A.
(defun solve-eigenvalue (matrix
			 &key
			 (U-matrix (make-array (dimensions matrix) :element-type 'single-float))
			 (V-matrix (make-array (dimensions matrix) :element-type 'single-float))
			 (W-vector (make-array (x-dim matrix) :element-type 'single-float))
			 ((:-> e-vector) (make-array (x-dim matrix) :element-type 'single-float)))
  (multiple-value-setq (W-vector U-matrix V-matrix) 
    (singular-value-decomposition matrix))
  (let* ((temp-list (concatenate 'list W-vector))
	 (min-of-temp-list (apply 'min temp-list))
	 ;;Position of minimum element.
	 (pos-of-min (position min-of-temp-list temp-list)))
    ;;Fill the e-vector with elements from the pos-of-min column of U-matrix.
    (loop for i from 0 below (length e-vector) do
	  (setf (aref e-vector i) (aref U-matrix i pos-of-min))))
  (values e-vector W-vector))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Principal Components:

;;; Assumes the data to be explained lie in the rows.
;;; *** Is it right to weight the components by the singular values?
(defun principal-components (matrix &key (dimension (min (row-dim matrix) (col-dim matrix)))
					scale)
  (with-svd (s u v) matrix
    (when (> dimension (length s))
      (error "Too many principal components requested"))

    (let ((principal-components (similar matrix :dimensions (list dimension (col-dim matrix))))
	  (principal-values (similar matrix :dimensions (list dimension (row-dim matrix))))
	  (singular-values (similar matrix :dimensions dimension))
	  (diagonal (when scale (make-diagonal-matrix (vectorize s :size dimension)))))

      ;; Calculate the principal components
      (crop (matrix-transpose v) :y-dim dimension :-> principal-components)
      (when scale (with-static-arrays ((product (matrix-mul diagonal principal-components)))
		    (copy product :-> principal-components)))

      ;; Calculate the principal values
      (crop u :y-dim dimension :-> principal-values)
      (when scale
	(with-static-arrays ((product (matrix-mul principal-values diagonal)))
	  (copy product :-> principal-values)))

      ;; Calculate the shortened singular values
      (crop s :x-dim dimension :-> singular-values)
      
      (values principal-components principal-values singular-values))))

#|
;; Test the principal components code
;; Set up a matrix with timecourse in rows
(setf dimensions (list 6 30))
(dimensions (setf matrix (make-array dimensions :element-type 'single-float)))

;; Set up a simple signal
(dimensions (setf signal (make-array (col-dim matrix) :element-type 'single-float)))
(loop with length = (length signal)
      for i from 0 below length
      with scale = (/ (* 3 2-pi) length)
      do (setf (aref signal i) (cos (* i scale))))
(image-from-array signal)

;; Add the signal into each row of the matrix
(dimensions (zero! matrix))
(dimensions (add-rows matrix signal :-> matrix))
(image-from-array (row 0 matrix))
(image-from-array (row 1 matrix))

;; Add noise to the matrix
(dimensions (randomize matrix 1.0 :-> matrix))
(image-from-array (row 0 matrix))
(image-from-array (row 1 matrix))

(null (multiple-value-setq (principal-components principal-values singular-values)
	(row-principal-components matrix :scale t)))

;; Look at the principal components
(mapcar 'display (mapcar 'image-from-array (displaced-rows principal-components 0 3)))
(image-from-array singular-values)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Given symmetric matrix M, want to get a matrix T so that Tt M T = I.
;;; To do this, write M = A At, take the SVD, and use what you get back.
#|
;; Is this different from quadratic-decomp? 
(defun symmetric-decomposition (matrix &key ((:-> result) (similar matrix)))
  (unless (symmetric-p matrix)
    (error "Symmetric decomposition only possible on symmetric matrices"))
  (multiple-value-bind (s u v) (svd matrix)
    (declare (ignore v))
    (square-root s :-> s)
    (div 1.0 s :-> s :zero-val 0.0)
    (matrix-mul u (make-diagonal-matrix s) :-> result)))
|#

#|
(pprint (setf foo (make-matrix '((1 2 3) (4 5 6) (7 9 13)))))
(pprint (setf cov (matrix-mul-transpose foo foo)))
(pprint (setf sim (symmetric-decomposition cov)))
(assert (identity-p (setf result (matrix-transpose-mul sim (matrix-mul cov sim)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Given a matrix M, returns a matrix A such that:
;;; M = A * A^t
(defmethod quadratic-decomposition ((matrix array) &key ((:-> result) (similar matrix))
				    (verify t) transpose (tolerance *tolerance*))
  (multiple-value-bind (s u v) (svd matrix)
    (when verify
      (with-static-arrays ((scratch (similar matrix)))
	(mul-rows u s :-> scratch)
	(matrix-mul-transpose scratch u :-> result)
	(unless (almost-equal result matrix :tolerance tolerance)
	  (error "Quadratic decomposition is impossible"))))
    (square-root s :-> s)
    (mul-rows v s :-> result)
    (if transpose
	(matrix-transpose result :-> result)
	result)))

#|
(setf matrix (make-matrix '((1 2) (3 4))))
(setf product (matrix-mul-transpose matrix matrix))
(setf decomposition (quadratic-decomposition product))
(matrix-mul-transpose decomposition decomposition)

(setf decomposition (quadratic-decomposition product :transpose t))
(matrix-transpose-mul decomposition decomposition)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calculate the Moore-Penrose left pseudo-inverse using the SVD.
;;; SVD returns (s u v) such that m = u s v^t.  This function returns
;;; m^# = v 1/s u^t.  Allows the calling function to restrict the
;;; dimensions of the pseudo-inverse.  If dimension-limit is non-nil
;;; (it must be a whole number N), throw away all but the first N
;;; dimensions (set the singular values to 0).  If
;;; condition-number-limit or singular-value-limit is non-nil
;;; (must be a numbers), zero those singular-values that are too small.
;;; Return the min and max singular values, and the number of dimensions preserved.
;;; TODO
;;;    tmp matrices should be keywords

(defmethod matrix-inverse ((matrix array)
			   &key dimension-limit condition-number-limit
			   (singular-value-limit *machine-tolerance*)
			   suppress-warning (zero-val 0.0)
			   verify (tolerance *tolerance*)
			   ((:-> result) (similar matrix :dimensions (reverse (dimensions matrix)))))
  (declare-matrices (matrix result) nil)

  (unless (>= (row-dim matrix) (col-dim matrix))
    (error "Left pseudo-inverse doesn't make sense for short, fat matrices"))

  (with-static-arrays (s u v diagonal scratch)
    (multiple-value-setq (s u v) (svd matrix))
    (setq diagonal (similar matrix :dimensions (list (col-dim v) (col-dim u))))
    (setq scratch (similar matrix :dimensions (list (col-dim v) (row-dim u))))

    (let* ((dimensions (length s))
	   (max (aref s 0)))
    
      ;; throw away smallest singular values
      (when dimension-limit
	
	(decf dimension-limit)		; User specifies dimensions from 1...n
	(dotimes (index (length s))
	  (declare (fixnum index))
	  (when (> index dimension-limit)
	    (setf (aref s index) 0.0))))
      (when condition-number-limit
	(dotimes (index (length s))
	  (declare (fixnum index))
	  (when (> (/-0 max (aref s index) most-positive-single-float)
		   condition-number-limit)
	    (setf (aref s index) 0.0))))
      (when singular-value-limit
	(dotimes (index (length s))
	  (declare (fixnum index))
	  (when (< (aref s index) singular-value-limit)
	    (setf (aref s index) 0.0))))

      ;; Keep track of the dimension of the output.
      (decf dimensions (count 0.0 s :test #'=))
      
      ;; Compute the inverse. 
      (div 1.0 s :zero-val zero-val :suppress-warning suppress-warning :-> s)
      (make-diagonal-matrix s :-> diagonal)
      (matrix-mul v (matrix-mul-transpose diagonal u :-> scratch) :-> result)

      ;; If desired, check that inversion worked.
      (when verify
	(with-static-arrays ((id (matrix-mul result matrix)))
	  (unless (identity-p id :tolerance tolerance)
	    (warn "Matrix inversion failed at tolerance ~a" tolerance))))

      ;; Return inverse and conditioning information.
      (values result (if (plusp dimensions) (aref s (- dimensions 1)) 0.0)
	      max dimensions))))

#|
;; Method before 6-27-93
(defmethod matrix-inverse ((matrix array)
			   &key dimension-limit condition-number-limit
			   (singular-value-limit *machine-tolerance*)
			   suppress-warning
			   ((:-> result) (similar matrix :dimensions (reverse (dimensions matrix)))))
  (declare-matrices (matrix result) nil)

  (unless (>= (row-dim matrix) (col-dim matrix))
    (error "Left pseudo-inverse doesn't make sense for short, fat matrices"))

  (with-static-arrays (s u v diagonal scratch)
    (multiple-value-setq (s u v) (svd matrix))

    (let* ((dimensions (length s))
	   (max (aref s 0)))
      (setq diagonal (similar matrix :dimensions (list (col-dim v) (col-dim u))))
      (setq scratch (similar matrix :dimensions (list (col-dim v) (row-dim u))))
    
      ;; throw away smallest singular values
      (when dimension-limit
	;; Decrement since the user specifies dimensions from 1...n
	(decf dimension-limit)		
	(dotimes (index (length s))
	  (declare (fixnum index))
	  (when (> index dimension-limit)
	    (setf (aref s index) 0.0))))
      (when condition-number-limit
	(dotimes (index (length s))
	  (declare (fixnum index))
	  (when (> (/-0 max (aref s index) most-positive-single-float)
		   condition-number-limit)
	    (setf (aref s index) 0.0))))
      (when singular-value-limit
	(dotimes (index (length s))
	  (declare (fixnum index))
	  (when (< (aref s index) singular-value-limit)
	    (setf (aref s index) 0.0))))

      ;; Keep track of the dimension of the output.
      (decf dimensions (count 0.0 s :test #'=))
      
      (div 1.0 s :zero-value 0.0 :suppress-warning suppress-warning :-> s)
      (make-diagonal-matrix s :-> diagonal)
	      
      ;; Compute the inverse. Provide conditioning information too.
      (values
       (matrix-mul v (matrix-mul-transpose diagonal u :-> scratch) :-> result)
       (if (plusp dimensions) (aref s (- dimensions 1)) 0.0)
       max dimensions))))
|#
       
#|
;;; Test the matrix-inverse code
(setf *print-array* t)

(setf matrix (make-matrix  '((1.0 0.0 0.0) (0.0 2.0 0.0) (0.0 0.0 3.0))))
(setf matrix-inverse (matrix-inverse matrix :verify t))
(print-values (matrix-mul matrix matrix-inverse))
(setf matrix (make-matrix  '((1.0 2.0 3.0) (-1.0 2.0 3.0) (1.0 -2.0 3.0))))
(setf matrix-inverse (matrix-inverse matrix :verify t))
(print-values (matrix-mul matrix matrix-inverse))
(setf matrix (make-matrix '((1.0 2.0 3.0) (-1.0 2.0 3.0) (1.0 -2.0 3.0) (1.0 1.0 1.0))))
(setf matrix-inverse (matrix-inverse matrix :verify t))
(print-values (matrix-mul matrix-inverse matrix))

;; Use degenerate matrices, limit singular values, 
(setf matrix (make-matrix  '((1.0 2.0 3.0) (4.0 5.0 6.0) (7.0 8.0 9.0))))
(setq matrix-inverse (matrix-inverse matrix :singular-value-limit 1e-6 :verify t))
(print-values (matrix-mul matrix-inverse matrix)) ; Inversion does not work

(setf matrix (make-matrix '((1.0 0.0 0.0) (1.0  0.00001 0.0) (0.0 0.0 3.0))))
(setf matrix-inverse (matrix-inverse matrix :singular-value-limit 1e-4 :verify t))
(print-values (matrix-mul matrix-inverse matrix))

(setf matrix (make-matrix '((1.0 0.0 0.0) (1.0  0.00 0.0) (0.0 0.0 3.0))))
(matrix-inverse matrix)

|#



;;; Local Variables:
;;; buffer-read-only: t 
;;; End:



