(in-package 'user)

;; Rebuild a matrix from its SVD using fewer dimensions
;; Default behavior is a full reconstruction
(defun singular-value-reconstruction
    (matrix &key (dimension (min (row-dim matrix) (col-dim matrix)))
	    ((:-> result) (similar matrix)))
  (with-static-svd (s u v) matrix
    (with-static-arrays ((tmp-arr (similar matrix)))
      ;; Fill the diagonal matrix
      (fill! result 0.0)
      (dotimes (index (length s))
	(setf (aref result index index)
	      (if (< index dimension) (aref s index) 0.0)))
      (matrix-mul-transpose result v :-> tmp-arr)
      (matrix-mul u tmp-arr :-> result))))

;;; Array is an Nx3 array of float rgb values.  This function returns
;;; an mxn color image with those rgb values arranged in a checker.
(defun make-color-checker (array &key (rows 4) (cols 6) ->)
  (with-result ((result ->) (list :class 'color-image
				  :dimensions (list rows cols)
				  :length 3))
    (loop for row from 0 below rows do
	  (loop for col from 0 below cols
		for i = (+ col (* cols row))
		do
		(loop for band from 0 below 3
		      do
		      (when (< i (row-dim array))
			(setf (iref (aref (matrix result) 0 band) row col)
			      (aref array i band))))))
    result))

#|
(defun one-mode-reconstruction
    (surfaces sensors &key (dimension (min (row-dim surfaces) (col-dim surfaces)))
	      ((:-> result) (similar surfaces)))
  (with-static-arrays ((responses (matrix-mul sensors surfaces)))
    (with-static-svd (s u v) responses
      (with-static-arrays ((w (crop v :y-size dimension))
			   (Ls (matrix-mul w (matrix-inverse surfaces)))
			   (Lb ))

	;; Fill the diagonal singular matrix (use result for this)
	(fill! result 0.0)
	(dotimes (index (length s))
	  (setf (aref result index index)
		(if (< index dimension) (aref s index) 0.0)))
      
      
	(matrix-mul-transpose result v :-> tmp-arr)
	(matrix-mul u tmp-arr :-> result)))))

|#

