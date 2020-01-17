;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: row-ops.lisp
;;;  Author: Chichilnisky
;;;  Description: Functions for row operations on matrices
;;;  Creation Date: March 1992
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '(row rows displaced-rows col cols
	  normalize-rows normalize-cols vector-length-rows
	  add-rows sub-rows mul-rows div-rows add-cols sub-cols mul-cols div-cols
	  paste-rows paste-cols append-rows append-cols shuffle-rows swap-rows sort-rows
	  random-row random-rows sum-rows mean-rows covariance-rows sum-cols
	  mean-cols covariance-cols minimum-rows maximum-rows))

;; Functions for row operations on matrices
(defun displaced-rows (matrix &optional (start 0) (end (row-dim matrix)))
  (loop for row from start below end
	collect (displaced-row row matrix)))

(defun row (row matrix)
  (copy (displaced-row row matrix)))

(defun rows (matrix  &optional (start 0) (end (row-dim matrix)))
  (displaced-rows (copy matrix) start end))

(defun col (col matrix)
  (columnize (crop matrix :x col :x-dim 1)))

(defun cols (matrix  &optional (start 0) (end (col-dim matrix)))
  (loop for col from start below end
	collect (col col matrix)))

(defun random-row (arr &key ((:-> result) (similar (displaced-row 0 arr))))
  (copy (displaced-row (random (row-dim arr)) arr) :-> result))

(defun random-rows (arr rows &key ((:-> result)
				   (similar arr :dimensions (list rows (col-dim arr)))))
  (with-static-arrays ((shuffle-arr (similar arr :static t)))
    (shuffle-rows arr :-> shuffle-arr)
    (crop arr :x-size rows :-> result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Row and column arithmetic on matrices.

(defmethod check-size-rows ((a array) &rest a-list)
  (cond ((null a-list) a)
	((not (equal (col-dim a) (col-dim (car a-list))))
	 (error "Rows have different dimensions." ))
	(t (apply 'check-size-rows a-list))))

(defmethod check-size-cols ((a array) &rest a-list)
  (cond ((null a-list) a)
	((not (equal (row-dim a) (row-dim (car a-list))))
	 (error "Columns have different dimensions." ))
	(t (apply 'check-size-cols a-list))))



(defun add-rows (first second &key ((:-> result)))
  (check-size-rows first second)
  (checktype-matrices (first second))
  (cond ((= (row-dim second) 1)
	 (unless result (setq result (similar first)))
	 (check-size first result)
	 (checktype-matrices (result))
	 (internal-add-arr-row first second result (row-dim result) (col-dim result)))
	((= (row-dim first) 1)
	 (unless result (setq result (similar second)))
	 (check-size second result)
	 (checktype-matrices (result))
	 (internal-add-arr-row second first result (row-dim result) (col-dim result)))
	(t
	 (unless result (setq result (similar first)))
	 (add first second :-> result)))
  result)

(defun sub-rows (first second &key ((:-> result)))
  (check-size-rows first second)
  (checktype-matrices (first second))
  (cond ((= (row-dim second) 1)
	 (unless result (setq result (similar first)))
	 (check-size first result)
	 (checktype-matrices (result))
	 (internal-sub-arr-row first second result (row-dim result) (col-dim result)))
	((= (row-dim first) 1)
	 (unless result (setq result (similar second)))
	 (check-size second result)
	 (checktype-matrices (result))
	 (internal-sub-row-arr first second result (row-dim result) (col-dim result)))
	(t
	 (unless result (setq result (similar first)))
	 (sub first second :-> result)))
  result)

(defun mul-rows (first second &key ((:-> result)))
  (check-size-rows first second)
  (checktype-matrices (first second))
  (cond ((= (row-dim second) 1)
	 (unless result (setq result (similar first)))
	 (check-size first result)
	 (checktype-matrices (result))
	 (internal-mul-arr-row first second result (row-dim result) (col-dim result)))
	((= (row-dim first) 1)
	 (unless result (setq result (similar second)))
	 (check-size second result)
	 (checktype-matrices (result))
	 (internal-mul-arr-row second first result (row-dim result) (col-dim result)))
	(t
	 (unless result (setq result (similar first)))
	 (mul first second :-> result)))
  result)

(defun div-rows (first second &key ((:-> result)) zero-val)

  (when zero-val (error ":zero-val keyword not implemented yet for div-rows"))
  (check-size-rows first second)
  (checktype-matrices (first second))
  (cond ((= (row-dim second) 1)
	 (unless result (setq result (similar first)))
	 (check-size first result)
	 (checktype-matrices (result))
	 (internal-div-arr-row first second result (row-dim result) (col-dim result)))
	((= (row-dim first) 1)
	 (unless result (setq result (similar second)))
	 (check-size second result)
	 (checktype-matrices (result))
	 (internal-div-row-arr first second result (row-dim result) (col-dim result)))
	(t
	 (unless result (setq result (similar first)))
	 (div first second :-> result)))
  result)



(defun add-cols (first second &key ((:-> result)))
  (check-size-cols first second)
  (checktype-matrices (first second))
  (cond ((= (col-dim second) 1)
	 (unless result (setq result (similar first)))
	 (check-size first result)
	 (checktype-matrices (result))
	 (internal-add-arr-col first second result (row-dim result) (col-dim result)))
	((= (col-dim first) 1)
	 (unless result (setq result (similar second)))
	 (check-size second result)
	 (checktype-matrices (result))
	 (internal-add-arr-col second first result (row-dim result) (col-dim result)))
	(t
	 (unless result (setq result (similar first)))
	 (add first second :-> result)))
  result)

(defun sub-cols (first second &key ((:-> result)))
  (check-size-cols first second)
  (checktype-matrices (first second))
  (cond ((= (col-dim second) 1)
	 (unless result (setq result (similar first)))
	 (check-size first result)
	 (checktype-matrices (result))
	 (internal-sub-arr-col first second result (row-dim result) (col-dim result)))
	((= (col-dim first) 1)
	 (unless result (setq result (similar second)))
	 (check-size second result)
	 (checktype-matrices (result))
	 (internal-sub-col-arr first second result (row-dim result) (col-dim result)))
	(t
	 (unless result (setq result (similar first)))
	 (sub first second :-> result)))
  result)

(defun mul-cols (first second &key ((:-> result)))
  (check-size-cols first second)
  (checktype-matrices (first second))
  (cond ((= (col-dim second) 1)
	 (unless result (setq result (similar first)))
	 (check-size first result)
	 (checktype-matrices (result))
	 (internal-mul-arr-col first second result (row-dim result) (col-dim result)))
	((= (col-dim first) 1)
	 (unless result (setq result (similar second)))
	 (check-size second result)
	 (checktype-matrices (result))
	 (internal-mul-arr-col second first result (row-dim result) (col-dim result)))
	(t
	 (unless result (setq result (similar first)))
	 (mul first second :-> result)))
  result)

(defun div-cols (first second &key ((:-> result)) zero-val)

  (when zero-val (error ":zero-val keyword not implemented yet for div-cols"))
  (check-size-cols first second)
  (checktype-matrices (first second))
  (cond ((= (col-dim second) 1)
	 (unless result (setq result (similar first)))
	 (check-size first result)
	 (checktype-matrices (result))
	 (internal-div-arr-col first second result (row-dim result) (col-dim result)))
	((= (col-dim first) 1)
	 (unless result (setq result (similar second)))
	 (check-size second result)
	 (checktype-matrices (result))
	 (internal-div-col-arr first second result (row-dim result) (col-dim result)))
	(t
	 (unless result (setq result (similar first)))
	 (div first second :-> result)))
  result)

#|
;; Test row arithmetic

(setf row (make-matrix '(1 2 3)))
(setf arr (make-matrix '((1 2 3)
			 (4 5 6)
			 (7 8 9))))

(pprint (add-rows arr row))
(pprint (add-rows row arr))
(pprint (add-rows arr arr))
(pprint (add-rows row row))

(pprint (sub-rows arr row))
(pprint (sub-rows row arr))
(pprint (sub-rows arr arr))
(pprint (sub-rows row row))

(pprint (mul-rows arr row))
(pprint (mul-rows row arr))
(pprint (mul-rows arr arr))
(pprint (mul-rows row row))

(pprint (div-rows arr row))
(pprint (div-rows row arr))
(pprint (div-rows arr arr))
(pprint (div-rows row row))


(setq arr (matrix-transpose arr))
(setq col (matrix-transpose row))

(pprint (matrix-transpose (add-cols arr col)))
(pprint (matrix-transpose (add-cols col arr)))
(pprint (matrix-transpose (add-cols arr arr)))
(pprint (matrix-transpose (add-cols col col)))

(pprint (matrix-transpose (sub-cols arr col)))
(pprint (matrix-transpose (sub-cols col arr)))
(pprint (matrix-transpose (sub-cols arr arr)))
(pprint (matrix-transpose (sub-cols col col)))

(pprint (matrix-transpose (mul-cols arr col)))
(pprint (matrix-transpose (mul-cols col arr)))
(pprint (matrix-transpose (mul-cols arr arr)))
(pprint (matrix-transpose (mul-cols col col)))

(pprint (matrix-transpose (div-cols arr col)))
(pprint (matrix-transpose (div-cols col arr)))
(pprint (matrix-transpose (div-cols arr arr)))
(pprint (matrix-transpose (div-cols col col)))

|#



(defun sum-rows (matrix &key ((:-> result) (similar matrix :dimensions (col-dim matrix))))
  (checktype-matrices (matrix))
  (check-size-rows matrix result)
  (internal-row-sum matrix result (row-dim matrix) (col-dim matrix))
  result)

(defun mean-rows (matrix &key ((:-> result) (similar matrix :dimensions (col-dim matrix))))
  (sum-rows matrix :-> result)
  (div result (row-dim matrix) :-> result))

(defun sum-cols (matrix &key ((:-> result) (similar matrix :dimensions (list (row-dim matrix) 1))))
  (checktype-matrices (matrix))
  (check-size-cols matrix result)
  (internal-col-sum matrix result (row-dim matrix) (col-dim matrix))
  result)

(defmethod normalize-rows ((arr array) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (dotimes (row (row-dim arr))
    (normalize (displaced-row row arr) :-> (displaced-row row result)))
  result)

(defun vector-length-rows (arr &key ((:-> result) (similar arr :dimensions (row-dim arr))))
  (unless (= (total-size result) (row-dim arr))
    (error "Invalid result dimensions"))
  (internal-row-sum-of-square arr result (row-dim arr) (col-dim arr))
  (internal-sqrt result result (total-size result))
  result)

(defmethod normalize-cols ((arr array) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (with-static-arrays ((arr-t (matrix-transpose arr))
		       (res-t (matrix-transpose result)))
    (normalize-rows arr-t :-> res-t)
    (matrix-transpose res-t :-> result))
  result)

(defun mean-cols (matrix &key ((:-> result)
			       (similar matrix :dimensions (list (row-dim matrix) 1))))
  (sum-cols matrix :-> result)
  (div result (col-dim matrix) :-> result))

(defun covariance-rows (matrix &key ((:-> result) (similar matrix :dimensions
							  (list (col-dim matrix) (col-dim matrix))))
			       sample)
  (checktype-matrices (matrix))
  (with-static-arrays ((offset (similar matrix :static t))
		       (mean (mean-rows matrix)))
    (sub-rows matrix mean :-> offset)
    (matrix-transpose-mul offset offset :-> result)
    (div result (if sample (max 1 (- (row-dim matrix) 1)) (row-dim matrix))
	 :-> result)))

(defun covariance-cols (matrix &key ((:-> result)
				    (similar matrix :dimensions
					     (list (row-dim matrix) (row-dim matrix))))
			       sample)
  (checktype-matrices (matrix))
  (with-static-arrays ((offset (similar matrix :static t))
		       (mean (mean-cols matrix)))
    (sub-cols matrix mean :-> offset)
    (matrix-mul-transpose offset offset :-> result)
    (div result (if sample (max 1 (- (col-dim matrix) 1)) (col-dim matrix))
	 :-> result)))

(defun shuffle-rows (arr &key ((:-> result) (similar arr)))
  (check-size arr result)
  (unless (eq arr result)
    (copy arr :-> result))
  (if (= (col-dim arr) 1)
      (with-displaced-vectors ((vec arr))
	(shuffle vec :-> vec))
      (loop with rows = (row-dim result)
	    with cols = (col-dim result)
	    for i from 0 below rows
	    for j from rows by -1
	    for rand = (+ i (random j))
	    do (internal-row-swap result i rand cols)
	    ))
  result)

(defun sort-rows (arr predicate &key (key #'vector-length)
		     ((:-> result) (similar arr)))
  (with-static-arrays ((copy (copy arr)))
    (let* ((row-list (displaced-rows copy))
	   (rows (make-array (length row-list) :initial-contents row-list)))
      (sort rows predicate :key key)
      (dotimes (i (length rows))
	(copy (aref rows i) :-> (displaced-row i result)))
      result)))

#|
(dimensions (setf mat (make-array '(30 3) :element-type 'single-float)))
(dimensions (randomize mat 100.0 :-> mat))
(image-from-array (make-matrix (mapcar 'vector-length (displaced-rows mat))))
(dimensions (sort-rows mat '> :-> mat))
(image-from-array (make-matrix (mapcar 'vector-length (displaced-rows (sort-rows mat '>)))))
|#

(defun swap-rows (arr row-1 row-2 &key ((:-> result) (copy arr)))
  (check-size arr result)
  (let ((rows (row-dim arr)))
    (setq row-1 (floor row-1))
    (setq row-2 (floor row-2))
    (unless (and (< -1 row-1 rows) (< -1 row-2 rows))
      (error "Rows specified out of range")))
  (internal-row-swap result row-1 row-2 (col-dim result))
  result)

(defun paste-cols (arr-seq &key ((:-> result)))
  (let* ((rows (row-dim (elt arr-seq 0)))
	 (cols 0)
	 (offset 0)
	 arr col)
    (dotimes (i (length arr-seq))
      (setq arr (elt arr-seq i))
      (unless (= (row-dim arr) rows)
	(error "All arrays must have same row dimension"))
      (incf cols (col-dim arr)))
    (unless result (setf result (similar arr :dimensions (list rows cols))))
    (dotimes (i (length arr-seq))
      (setq arr (elt arr-seq i))
      (setq col (col-dim arr))
      (unless (zerop col)
	(paste arr result :-> result :x offset)
	(incf offset col)))
    result))

(defun paste-rows (arr-seq &key ((:-> result)))
  (let* ((cols (col-dim (elt arr-seq 0)))
	 (rows 0)
	 (offset 0)
	 row arr)
    (dotimes (i (length arr-seq))
      (setq arr (elt arr-seq i))
      (unless (= (col-dim arr) cols)
	(error "All arrays must have same column dimension"))
      (incf rows (row-dim arr)))
    (unless result (setf result (similar arr :dimensions (list rows cols))))
    (dotimes (i (length arr-seq))
      (setq arr (elt arr-seq i))
      (setq row (row-dim arr))
      (unless (zerop row)
	(paste arr result :-> result :y offset)
	(incf offset row)))
    result))

#|
(setq foo (make-matrix 1 2))
(setq fee (make-matrix 3 4))
(paste-cols (list foo fee))
(paste-rows (list foo fee))
(paste-cols (vector foo fee))
(paste-rows (vector foo fee))

(setq fum (make-matrix 0 0 0 0))
(paste-cols (list foo fee) :-> fum)
(paste-cols (vector foo fee) :-> fum)

|#

(defun append-cols (&rest arr-list)
  (paste-cols arr-list))

(defun append-rows (&rest arr-list)
  (paste-rows arr-list))

(defun minimum-rows (arr &key ((:-> result)))
  (unless result (setq result (similar arr :dimensions (col-dim arr))))
  (dotimes (i (col-dim arr))
    (let ((val (aref arr 0 i))
	  tmp)
      (dotimes (j (row-dim arr))
	(setq tmp (aref arr j i))
	(when (< tmp val) (setq val tmp)))
      (setf (aref result i) val)))
  result)

(defun maximum-rows (arr &key ((:-> result)))
  (unless result (setq result (similar arr :dimensions (col-dim arr))))
  (dotimes (i (col-dim arr))
    (let ((val (aref arr 0 i))
	  tmp)
      (dotimes (j (row-dim arr))
	(setq tmp (aref arr j i))
	(when (> tmp val) (setq val tmp)))
      (setf (aref result i) val)))
  result)
#|
(setq foo (make-matrix '((1 2 3)
			 (-1 2 5)
			 (0 3 4))))
(elements (minimum-rows foo))
(elements (maximum-rows foo))
|#




;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
