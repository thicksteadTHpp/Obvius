;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: regress.lisp
;;;  Author: Chichilnisky
;;;  Description: Least squares regression, based on pseudo-inverse from SVD
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :obvius)
(export '(regress))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Solve the regression problem: (observed) = (predictor) * M.
;;; (observed) and (predictor) are the data, M is the least-square-error solution.
;;; We speak of "regressing the observed against the predictor", hence the argument order.
;;; The transpose keyword chooses M to approximate: (observed) = M * (predictor)
;;; The data are thought of as observations, each observation is a row.
;;; In the transpose case, the observations are thought of as lying in the columns.
;;; *** diagonal-affine fit needs to be implemented
(defmethod regress ((observed array) (predictor array) &key diagonal affine transpose transform)
  (cond ((and affine diagonal) (error "Diagonal affine regression not implemented"))
	(transform (with-static-arrays ((t-observed (matrix-mul observed transform))
					(t-predictor (matrix-mul predictor transform))
					(inverse (matrix-inverse transform)))
		     (matrix-mul transform
				 (matrix-mul (regress t-observed t-predictor :diagonal diagonal
						      :affine affine) inverse))))
	(transpose (with-static-arrays ((t-observed (matrix-transpose observed))
					(t-predictor (matrix-transpose predictor)))
		     (regress t-observed t-predictor :diagonal diagonal :affine affine)))
		     
	(affine (let* ((dimensions (list (row-dim predictor) (1+ (col-dim predictor))))
		       (affine-predictor (similar predictor :dimensions dimensions)))
		  (fill! affine-predictor 1)
		  (paste predictor affine-predictor :-> affine-predictor)
		  (regress observed affine-predictor)))
	(diagonal (make-diagonal-matrix (loop for observed-col in (cols observed)
					      for predictor-col in (cols predictor)
					      collect (regress (vectorize observed-col)
							       (vectorize predictor-col)))))
	(t (matrix-mul (matrix-inverse predictor) observed))))

(defmethod regress ((observed vector) (predictor vector) &key affine)
  (if affine
      (vectorize (regress (columnize observed) (columnize predictor) :affine t))
      (div (dot-product predictor observed) (dot-product predictor predictor))))
		      
;;; Regression on lists of numbers.
(defmethod regress ((observed cons) (predictor cons) &rest keys)
  (let ((predictor-matrix (make-matrix predictor))
	(observed-matrix (make-matrix observed)))
    (apply 'regress observed-matrix predictor-matrix keys)))

#|
;; Test regression.
;; Set up noise-free data
(setf matrix (randomize (make-array '(3 3) :element-type 'single-float) 5.0))
(dimensions (setf predictor (randomize (make-array '(6 3) :element-type 'single-float) 10.0)))
(dimensions (setf observed (matrix-mul predictor matrix)))

(range (div (regress observed predictor) matrix))

;; Regress in another space
(Setq transform (randomize matrix 5.0))
(setq matrix (matrix-mul (matrix-mul transform (make-diagonal-matrix '(1 3 5)))
			 (matrix-inverse transform)))
(dimensions (setf observed (matrix-mul predictor matrix)))

(range (div (regress observed predictor :transform transform :diagonal t) matrix))



;; Check the transposed problem
(range (div (regress (matrix-transpose observed) (matrix-transpose predictor) :transpose t)
	    matrix))

;; Add noise and repeat
(dimensions (randomize observed (/ (abs (mean observed)) 4.0) :-> observed))
(range (div (regress observed predictor) matrix))

;; Do diagonal regression
(make-diagonal-matrix '(1 2 3) :-> matrix)
(dimensions (matrix-mul predictor matrix :-> observed))
(range (sub (regress observed predictor) matrix))

|#
;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
