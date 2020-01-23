;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: image-matrix.lisp
;;;  Author: Heeger
;;;  Description: Matrices with elements that are images
;;;  Creation Date: 11/91
;;;  Modified: 1/94 Put generic viewable matrix stuff into separate file
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obv)
(export '(image-matrix-p matrix
	  make-image-matrix diagonal-image-matrix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Image matrix class

(defmacro image-matrix-p (obj)
  `(typep ,obj 'image-matrix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make-image-matrix

;;; Data can be a nested-list of images or a dim-list or nil.  If data
;;; is nil, must pass either (1) :size and :sub-viewable-spec or (2)
;;; :size and :dimensions.  If data is a dim-list, then :size must
;;; also be specified.
(defun make-image-matrix (data &rest initargs
			  &key sub-viewable-spec dimensions size name ->
			  &allow-other-keys)
  (declare (ignore name size))
  (when -> (setf (getf initargs :name) ->))
  (when (and data (listp data) (every #'integerp data))
    (setf (getf initargs :dimensions) (setq dimensions data))
    (setq data nil))
  (setq data (setup-data-for-make-viewable-matrix data))
  (unless data
    (setf (getf initargs :sub-viewable-spec)
	  (cond (sub-viewable-spec sub-viewable-spec)
		(dimensions (list :class 'image
				  :dimensions dimensions))
		(t (error "Must specify either :dimensions or :sub-viewable-spec")))))
  (with-result ((result nil)
		`(:class image-matrix
		  :data ,data
		  ,@initargs)
		'apply 'make-image-matrix data initargs)
    result))

(defun diagonal-image-matrix (im-list &rest initargs)
  (let ((data (make-array (list (length im-list) (length im-list)))))
    (loop for j from 0 below (row-dim data) do
	  (loop for i from 0 below (col-dim data) do
		(if (= i j)
		    (setf (aref data j i) (nth i im-list))
		    (setf (aref data j i) (similar (car im-list))))))
    (apply 'make-image-matrix data initargs)))


#|
(setq foo1 (make-instance 'image-matrix :size '(2 2)
			  :sub-viewable-spec (list :class 'image
						   :dimensions '(10 10))))
(setq foo2 (make-instance
	    'image-matrix
	    :data (make-array '(2 2) :initial-contents
			      (list (list (make-image '(10 10)) (make-image '(10 10)))
				    (list (make-image '(10 10)) (make-image '(10 10)))))))
(setq foo3 (make-image-matrix nil
			      :size '(2 2)
			      :sub-viewable-spec (list :class 'image
						       :dimensions '(10 10))))
(setq foo4 (make-image-matrix (list (list (make-image '(10 10)) (make-image '(10 10)))
				    (list (make-image '(10 10)) (make-image '(10 10))))))
(setq foo5 (make-image-matrix nil :size '(2 2) :dimensions '(10 10)))
(setq foo6 (make-image-matrix '(10 10) :size '(2 2)))

(setq bar1 (make-viewable-matrix (list (list (make-image '(2 2)) (make-image '(2 2)))
				       (list (make-image '(2 2)) (make-image '(2 2))))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; arith ops: image-matrix,image

(defmethod mul ((mat image-matrix) (im image) &key ->)
  (unary-viewable-matrix-op 'mul mat -> im))

(defmethod mul ((im image) (mat image-matrix) &key ->)
  (unary-viewable-matrix-op 'mul mat -> im))

;;; *** Missing zero-val
(defmethod div ((mat image-matrix) (im image) &key -> &allow-other-keys)
  (unary-viewable-matrix-op 'div mat -> im))

(defmethod div ((im image) (mat image-matrix) &key -> &allow-other-keys)
  (unary-viewable-reverse-matrix-op 'div mat -> im))

(defmethod add ((mat image-matrix) (im image) &key ->)
  (unary-viewable-matrix-op 'add mat -> im))

(defmethod add ((im image) (mat image-matrix) &key ->)
  (unary-viewable-matrix-op 'add mat -> im))

(defmethod sub ((mat image-matrix) (im image) &key ->)
  (unary-viewable-matrix-op 'sub mat -> im))

(defmethod sub ((im image) (mat image-matrix) &key ->)
  (unary-viewable-reverse-matrix-op 'sub mat -> im))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; comparison ops

(defmethod equal-to
    ((mat1 image-matrix) (mat2 image-matrix) &key ->)
  (with-result ((result-mat ->)
		(list :class (cond ((viewable-sequence-p mat1) 'viewable-sequence)
				   ((viewable-matrix-p mat1) 'viewable-matrix))
		      :size (list (row-dim mat1) (col-dim mat1))
		      :sub-viewable-spec (list :class 'bit-image
					       :dimensions (dimensions mat1)))
		'equal-to mat1 mat2)
    (loop for arg-vbl1 in (listify (data mat1))
	  for arg-vbl2 in (listify (data mat2))
	  for res-vbl in  (listify (data result-mat))
	  do (equal-to arg-vbl1 arg-vbl2 :-> res-vbl))
    result-mat))

(defmethod equal-to ((mat image-matrix) (const number) &key ->)
  (with-result ((result-mat ->)
		(list :class (cond ((viewable-sequence-p mat) 'viewable-sequence)
				   ((viewable-matrix-p mat) 'viewable-matrix))
		      :size (list (row-dim mat) (col-dim mat))
		      :sub-viewable-spec (list :class 'bit-image
					       :dimensions (dimensions mat)))
		'equal-to mat const)
    (loop for arg-vbl in (listify (data mat))
	  for res-vbl in  (listify (data result-mat))
	  do (equal-to arg-vbl const :-> res-vbl))
    result-mat))

(defmethod equal-to ((const number) (mat image-matrix) &key ->)
  (equal-to mat const :-> ->))

(defmethod greater-than
    ((mat1 image-matrix) (mat2 image-matrix) &key ->)
  (with-result ((result-mat ->)
		(list :class (cond ((viewable-sequence-p mat1) 'viewable-sequence)
				   ((viewable-matrix-p mat1) 'viewable-matrix))
		      :size (list (row-dim mat1) (col-dim mat1))
		      :sub-viewable-spec (list :class 'bit-image
					       :dimensions (dimensions mat1)))
		'greater-than mat1 mat2)
    (loop for arg-vbl1 in (listify (data mat1))
	  for arg-vbl2 in (listify (data mat2))
	  for res-vbl in  (listify (data result-mat))
	  do (greater-than arg-vbl1 arg-vbl2 :-> res-vbl))
    result-mat))

(defmethod greater-than ((mat image-matrix) (const number) &key ->)
  (with-result ((result-mat ->)
		(list :class (cond ((viewable-sequence-p mat) 'viewable-sequence)
				   ((viewable-matrix-p mat) 'viewable-matrix))
		      :size (list (row-dim mat) (col-dim mat))
		      :sub-viewable-spec (list :class 'bit-image
					       :dimensions (dimensions mat)))
		'greater-than mat const)
    (loop for arg-vbl in (listify (data mat))
	  for res-vbl in  (listify (data result-mat))
	  do (greater-than arg-vbl const :-> res-vbl))
    result-mat))

(defmethod greater-than ((const number) (mat image-matrix) &key ->)
  (less-than mat const :-> ->))

(defmethod less-than
    ((mat1 image-matrix) (mat2 image-matrix) &key ->)
  (with-result ((result-mat ->)
		(list :class (cond ((viewable-sequence-p mat1) 'viewable-sequence)
				   ((viewable-matrix-p mat1) 'viewable-matrix))
		      :size (list (row-dim mat1) (col-dim mat1))
		      :sub-viewable-spec (list :class 'bit-image
					       :dimensions (dimensions mat1)))
		'less-than mat1 mat2)
    (loop for arg-vbl1 in (listify (data mat1))
	  for arg-vbl2 in (listify (data mat2))
	  for res-vbl in  (listify (data result-mat))
	  do (less-than arg-vbl1 arg-vbl2 :-> res-vbl))
    result-mat))

(defmethod less-than ((mat image-matrix) (const number) &key ->)
  (with-result ((result-mat ->)
		(list :class (cond ((viewable-sequence-p mat) 'viewable-sequence)
				   ((viewable-matrix-p mat) 'viewable-matrix))
		      :size (list (row-dim mat) (col-dim mat))
		      :sub-viewable-spec (list :class 'bit-image
					       :dimensions (dimensions mat)))
		'less-than mat const)
    (loop for arg-vbl in (listify (data mat))
	  for res-vbl in  (listify (data result-mat))
	  do (less-than arg-vbl const :-> res-vbl))
    result-mat))

(defmethod less-than ((const number) (mat image-matrix) &key ->)
  (greater-than mat const :-> ->))

(defmethod greater-than-or-equal-to
    ((mat1 image-matrix) (mat2 image-matrix) &key ->)
  (with-result ((result-mat ->)
		(list :class (cond ((viewable-sequence-p mat1) 'viewable-sequence)
				   ((viewable-matrix-p mat1) 'viewable-matrix))
		      :size (list (row-dim mat1) (col-dim mat1))
		      :sub-viewable-spec (list :class 'bit-image
					       :dimensions (dimensions mat1)))
		'greater-than-or-equal-to mat1 mat2)
    (loop for arg-vbl1 in (listify (data mat1))
	  for arg-vbl2 in (listify (data mat2))
	  for res-vbl in  (listify (data result-mat))
	  do (greater-than-or-equal-to arg-vbl1 arg-vbl2 :-> res-vbl))
    result-mat))

(defmethod greater-than-or-equal-to ((mat image-matrix) (const number) &key ->)
  (with-result ((result-mat ->)
		(list :class (cond ((viewable-sequence-p mat) 'viewable-sequence)
				   ((viewable-matrix-p mat) 'viewable-matrix))
		      :size (list (row-dim mat) (col-dim mat))
		      :sub-viewable-spec (list :class 'bit-image
					       :dimensions (dimensions mat)))
		'greater-than-or-equal-to mat const)
    (loop for arg-vbl in (listify (data mat))
	  for res-vbl in  (listify (data result-mat))
	  do (greater-than-or-equal-to arg-vbl const :-> res-vbl))
    result-mat))

(defmethod greater-than-or-equal-to ((const number) (mat image-matrix) &key ->)
  (less-than-or-equal-to mat const :-> ->))

(defmethod less-than-or-equal-to
    ((mat1 image-matrix) (mat2 image-matrix) &key ->)
  (with-result ((result-mat ->)
		(list :class (cond ((viewable-sequence-p mat1) 'viewable-sequence)
				   ((viewable-matrix-p mat1) 'viewable-matrix))
		      :size (list (row-dim mat1) (col-dim mat1))
		      :sub-viewable-spec (list :class 'bit-image
					       :dimensions (dimensions mat1)))
		'less-than-or-equal-to mat1 mat2)
    (loop for arg-vbl1 in (listify (data mat1))
	  for arg-vbl2 in (listify (data mat2))
	  for res-vbl in  (listify (data result-mat))
	  do (less-than-or-equal-to arg-vbl1 arg-vbl2 :-> res-vbl))
    result-mat))

(defmethod less-than-or-equal-to ((mat image-matrix) (const number) &key ->)
  (with-result ((result-mat ->)
		(list :class (cond ((viewable-sequence-p mat) 'viewable-sequence)
				   ((viewable-matrix-p mat) 'viewable-matrix))
		      :size (list (row-dim mat) (col-dim mat))
		      :sub-viewable-spec (list :class 'bit-image
					       :dimensions (dimensions mat)))
		'less-than-or-equal-to mat const)
    (loop for arg-vbl in (listify (data mat))
	  for res-vbl in  (listify (data result-mat))
	  do (less-than-or-equal-to arg-vbl const :-> res-vbl))
    result-mat))

(defmethod less-than-or-equal-to ((const number) (mat image-matrix) &key ->)
  (greater-than-or-equal-to mat const :-> ->))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;; Matrix-inverse and matrix-mul test code

(progn
  (load-image "/home/heeger/images/reagan")
  (load-image "/home/heeger/images/einstein")
  (setq al (gauss-out einstein))
  (setq bo (gauss-out reagan))
  nil)

(set-default 'pasteup :independent-parameters nil)

(setq mat (make-image-matrix (list (list al bo)
				   (list (negate bo) al))))
(setq inv (matrix-inverse mat))
(matrix-mul mat inv)
(matrix-mul inv mat)

(setq mat3 (make-image-matrix (list (list al bo (mul al bo))
				    (list (negate bo) (mul al bo) al)
				    (list (div al bo) bo (negate al)))))
(setq inv3 (matrix-inverse mat3))
(matrix-mul mat3 inv3)
(matrix-mul inv3 mat3)


(setq m1 (make-image-matrix (list (list al bo) (list (negate bo) (negate al)))))
(setq m2 (make-image-matrix (list (list al bo) (list al bo))))
(setq m3 (similar m1))
(setq v1 (make-image-matrix (list (list al) (list bo)))) ;col vec
(setq v2 (make-image-matrix (list (list al bo)))) ;row vec

(matrix-mul m1 m1)
(matrix-mul m1 m2)
(matrix-mul m1 v1)
(matrix-mul m1 v2) ;error
(matrix-mul v2 m1)
(matrix-mul-transpose m1 v2)
(matrix-mul-transpose m1 m1)
(matrix-transpose-mul v2 m1) ;error
(matrix-transpose-mul v1 m1)
(matrix-transpose-mul m1 m1)
(matrix-mul m1 m1 :-> m3)
(dot-product v1 v1)
|#


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
