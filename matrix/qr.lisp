;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: qr.lisp
;;;  Author: Heeger
;;;  Description: QR-decomposition and related utilities
;;;  Creation Date:
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '(qr-decomposition
	  col-space-qr row-space-qr
	  col-null-space-qr row-null-space-qr
	  determinant-qr))

(defmacro qrd (&rest args)
  `(qr-decomposition ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; QR decomposition

;;; returns q and r matrices, q is orthonormal, r is upper triangular
(defun qr-decomposition (a &key (r (make-array (list (x-dim a) (x-dim a))
					       :element-type 'single-float))
			   (q (copy a)))
  (declare (type (array single-float (* *)) a r q))
  (unless (eq a q) (copy a :-> q))
  (loop for k from 0 below (array-dimension a 1) do
	;; compute norm of col k
	(setf (aref r k k) (sqrt (col-dot-arr-arr q k q k))) 
	;; normalize col k
	(col-div-arr-const q k (aref r k k)) 
	(loop for j from (1+ k) below (array-dimension q 1) do
	      (setf (aref r k j) (col-dot-arr-arr q k q j))
	      (col-submul-arr-arr-const q j q k (aref r k j))))
  (values q r))

;;; dot-product between col j of array a and col k of array b
(defun col-dot-arr-arr (a j b k)
  (declare (type (array single-float (* *)) a b)
	   (type fixnum j k))
  (loop for i from 0 below (array-dimension a 0) sum
	(* (aref a i j) (aref b i k))))

;;; dot-product between col j of array a and vector v
(defun col-dot-arr-vec (a j v)
  (declare (type (array single-float (* *)) a)
	   (type (array single-float (*)) v)
	   (type fixnum j))
  (loop for i from 0 below (array-dimension a 0) sum
	(* (aref a i j) (aref v i))))

;;; divides col j by constant c
;;; if quotient is zero, returns 0
(defun col-div-arr-const (a j c)
  (declare (type (array single-float (* *)) a)
	   (type single-float c)
	   (type fixnum j))
  (loop for i from 0 below (array-dimension a 0) do
	(setf (aref a i j) (/-0 (aref a i j) c 0.0))))

(defun col-submul-arr-arr-const (a j b k c)
  (declare (type (array single-float (* *)) a b)
	   (type single-float c)
	   (type fixnum j k))  
  (loop for i from 0 below (array-dimension a 0) do
	(setf (aref a i j) (- (aref a i j) (* (aref b i k) c)))))

(defun norm-of-arr-col (a j)
  (declare (type (array single-float (* *)) a)
	   (type fixnum j))
  (let ((norm 0.0))
    (declare (type single-float norm))
    (loop for i from 0 below (array-dimension a 0) do
	  (setq norm (+ norm (sqr (aref a i j)))))
    (sqrt norm)))

#|
;;; Test It
(compiler-options :show-optimizations t)

(setq A (make-array '(10 5) :element-type 'single-float 
	    :initial-contents 
	    (list (list (random 1.0) (random 1.0) (random 1.0) (random 1.0) (random 1.0))
		  (list (random 1.0) (random 1.0) (random 1.0) (random 1.0) (random 1.0))
		  (list (random 1.0) (random 1.0) (random 1.0) (random 1.0) (random 1.0))
		  (list (random 1.0) (random 1.0) (random 1.0) (random 1.0) (random 1.0))
		  (list (random 1.0) (random 1.0) (random 1.0) (random 1.0) (random 1.0))
		  (list (random 1.0) (random 1.0) (random 1.0) (random 1.0) (random 1.0))
		  (list (random 1.0) (random 1.0) (random 1.0) (random 1.0) (random 1.0))
		  (list (random 1.0) (random 1.0) (random 1.0) (random 1.0) (random 1.0))
		  (list (random 1.0) (random 1.0) (random 1.0) (random 1.0) (random 1.0))
		  (list (random 1.0) (random 1.0) (random 1.0) (random 1.0) (random 1.0)))))

(multiple-value-setq (Q R) (qr-decomposition A))

;;; cols of Q should be orthonormal
(print-values (matrix-transpose-mul Q Q))

;;; compare original array with decomposition
(print-values (sub A (matrix-mul Q R)))

;;; destructively modifies A to be orthonormal
(qr-decomposition A :q A)
(print-values (matrix-transpose-mul A A))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun col-space-qr (a &key
		     ((:-> col-space) (similar a))
		     (r (make-array (list (col-dim a) (col-dim a))
				    :element-type 'single-float)))
  (copy a :-> col-space)
  (qr-decomposition a :r r :q col-space)
  col-space)

(defun row-space-qr (a
		     &key
		     ((:-> row-space) (make-array (list (row-dim a) (col-dim a))
						  :element-type 'single-float))
		     (q (make-array (list (col-dim a) (row-dim a))
				    :element-type 'single-float))
		     (r (make-array (list (row-dim q) (row-dim q))
				    :element-type 'single-float)))
  (matrix-transpose a :-> q)
  (qr-decomposition q :r r :q q)
  (matrix-transpose q :-> row-space)
  row-space)

(defun row-null-space-qr
    (a
     &key
     (q (make-array (list (col-dim a) (row-dim a)) :element-type 'single-float))
     (r (make-array (list (row-dim q) (row-dim q)) :element-type 'single-float))
     (tmp (make-array (row-dim q) :element-type 'single-float))
     (tol 1e-6)
     (q-col-null (make-array (list (row-dim q) (- (row-dim q) (col-dim q)))
			     :element-type 'single-float))
     ((:-> row-null) (make-array (list (- (col-dim a) (row-dim a)) (col-dim a))
				 :element-type 'single-float)))
  (matrix-transpose a :-> q)
  (col-null-space-qr q :q q :r r :tmp tmp :tol tol :-> q-col-null)
  (matrix-transpose q-col-null :-> row-null)
  row-null)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; finds basis for left nullspace using QR decomposition

;;; A is MxN for M > N
;;; returns col-null
;;; Q-matrix is orthonormalized.
(defun col-null-space-qr
    (a
     &key
     (q (copy a))
     (r (make-array (list (x-dim a) (x-dim a)) :element-type 'single-float))
     (tmp (make-array (y-dim a) :element-type 'single-float))
     (tol 1e-6)
     ((:-> col-null) (make-array (list (y-dim a) (- (y-dim a) (x-dim a)))
				  :element-type 'single-float)))
  (declare (type (array single-float (* *)) a r col-null)
	   (type (array single-float (*)) tmp)
	   (type single-float tol))
  (qr-decomposition a :r r :q q)
  (let ((end-loop nil)
	(norm 0.0))
;    (declare (type single-float) norm)
    (loop for l from 0 below (x-dim col-null) do
	  (setq end-loop nil)
	  (loop while t do
		(when end-loop (loop-finish))
		;; choose a random vector, values between -1.0 and 1.0
		(loop for i from 0 below (total-size tmp) do
		      (setf (aref tmp i) (- (random 2.0) 1.0)))
		(loop for i from 0 below (y-dim a) do
		      (setf (aref col-null i l) (aref tmp i)))
		;; make it orthog to cols of q
		(loop for k from 0 below (x-dim a) do
		      (col-submul-arr-arr-const col-null l q k
						(col-dot-arr-vec q k tmp)))
		;; make it orthog to previous cols of col-null
		(loop for k from 0 below l do
		      (col-submul-arr-arr-const col-null l col-null k
						(col-dot-arr-vec col-null k tmp)))
		;; if norm is big enough, keep it; otherwise choose a different random vector
		(setq norm (sqrt (col-dot-arr-arr col-null l col-null l)))
		(when (> norm tol)
		  (setq end-loop t)
		  (col-div-arr-const col-null l norm)))))
  col-null)

#|
;;; Test col spaces

(setq A (randomize (zero! (make-array '(10 7) :element-type 'single-float)) 10.0))
(setq col-space (col-space-qr A))
(setq col-null (col-null-space-qr A))
;;; cols of col-space are orthonormal
(print-values (matrix-transpose-mul col-space col-space))
;;; cols of col-null are orthonormal
(print-values (matrix-transpose-mul col-null col-null))
;;; cols of col-null perp to cols of A
(print-values (*. 1e4 (matrix-transpose-mul A col-null)))
;;; cols of col-null perp to cols of col-space
(print-values (*. 1e4 (matrix-transpose-mul col-space col-null)))

;;; Test row spaces

(setq A (randomize (zero! (make-array '(7 10) :element-type 'single-float)) 10.0))
(setq row-space (row-space-qr A))
(setq row-null (row-null-space-qr A))
;;; rows of row-space are orthonormal
(print-values (matrix-mul-transpose row-space row-space))
;;; rows of row-null are orthonormal
(print-values (matrix-mul-transpose row-null row-null))
;;; rows of row-null perp to rows of A
(print-values (*. 1e4 (matrix-mul-transpose A row-null)))
;;; rows of row-null perp to rows of row-space
(print-values (*. 1e4 (matrix-mul-transpose row-space row-null)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Option to ignores any zeros along the diagonal, implicitly
;;; assuming that the matrix is nonsingular.
;;; Q-matrix is orthonormalized.
(defun determinant-qr
    (matrix
     &key
     (ignore-zeros nil)
     (q (make-array (dimensions matrix) :element-type 'single-float))
     (r (make-array (list (x-dim matrix) (x-dim matrix))
		    :element-type 'single-float)))
  (let ((val 1.0))
    (qr-decomposition matrix :r r :q q)
    (loop for i from 0 below (x-dim matrix)
	  for elt = (aref r i i)
	  do (when (or (> (abs elt) short-float-epsilon) (not ignore-zeros))
	       (setq val (* val elt))))
    val))

#|
(setq A (make-array '(3 3) :element-type 'single-float
		    :initial-contents '((0.0 1.0 0.0)
					(0.0 1.0 1.0)
					(0.0 0.0 0.0))))
(determinant A)
(determinant A :ignore-zeros t)
|#


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
