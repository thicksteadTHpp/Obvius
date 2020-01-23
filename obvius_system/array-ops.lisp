;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: array-ops.lisp
;;;  Author: Heeger/Simoncelli
;;;  Description: Operations on arrays.  
;;;  Creation Date: Fall, 1988
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)

;;; Many of the exports are done in generic-fns.lisp
(export '(with-displaced-vectors vectorize columnize displaced-matrix displaced-row
	  total-size dimensions check-size rank
	  similar copy coerce-to-float
	  print-values
	  verify-monotone-increasing
	  ;;mean variance
	  covariance correlation 
	  third-moment fourth-moment
	  ;;skew kurtosis
	  sum-of product-of logarithm
	  ;; maximum minimum range
	  ;; mean-square-error mean-abs-error max-abs-error
	  ;; point-minimum point-maximum
	  ;; square-error abs-error
	  ;; add mul sub div negate square abs-value
	  ;; linear-xform square-root power natural-logarithm
	  half-square logarithm
	  ;; point-operation periodic-point-operation
	  ;; quantize clip
	  ;; circular-shift crop paste
	  ;; greater-than less-than equal-to
	  ;; greater-than-or-equal-to less-than-or-equal-to
	  almost-equal almost-zero randomize shuffle
	  scalar-multiple
	  array-magnitude array-abs-value array-square-magnitude
	  array-complex-phase
	  correlate convolve
	  array-cross-product
	  modulo sgn
	  sum-square-error
	  ;; *** Still need to implement these on 2d arrays and vectors:
	  ;;upsample downsample subsample
	  ;;flip-x flip-y transpose side-by-side
	  *x-print-range* *y-print-range* *max-print-vals*
	  write-ascii-matrix read-ascii-matrix))

;;; *** Shouldn't we get rid of the lucid checking? YES!

;;; Handle single-float in C.  Handle all else in Lisp.  If handled by
;;; lisp, no coercion and no error checking.

;;; NOTE: It would be preferable to write these operations as pairs of
;;; methods.  One method would be for general arrays.  The other would
;;; call C code to operate on float arrays.  Unfortunately, CLOS does
;;; not work with array sub-types (it only works for the general class
;;; "array").
 
(defmacro float-arrays-p (&rest arrays)
  `(and ,@(loop for arr in arrays collect `(typep ,arr '(array single-float)))))

(defmacro 8bit-arrays-p (&rest arrays)
  `(and ,@(loop for arr in arrays collect `(typep ,arr '(array (unsigned-byte 8))))))

(defmacro 1bit-arrays-p (&rest arrays)
  `(and ,@(loop for arr in arrays collect `(typep ,arr '(array bit)))))

;; *** Note that this is signed. Is this OK? Should coercion handle either?
(defmacro 16bit-arrays-p (&rest arrays)
  `(and ,@(loop for arr in arrays collect `(typep ,arr '(array (signed-byte 16))))))

;;; Returns 1D array displaced to the arg.  Allows you to displace
;;; into an array to pull out part of it as a vector.
(defmethod vectorize ((arr array) &key (x-offset 0) (y-offset 0)
		      (x x-offset) (y y-offset)
		      (size (- (array-total-size arr) (+ (* y (x-dim arr)) x))))
  (make-array size :displaced-to arr
	      :displaced-index-offset (+ (* y (x-dim arr)) x)
	      :element-type (array-element-type arr)))

;;; Make it more like vectorize. EJC 10.23.91
(defmethod columnize ((arr array) &key (size (array-total-size arr))
		      (x-offset 0) (y-offset 0) (x x-offset) (y y-offset))
  (make-array (list size 1) :displaced-to arr
	      :displaced-index-offset (+ (* y (x-dim arr)) x)
	      :element-type (array-element-type arr)))

(defun displaced-row (row matrix)
  (make-array (col-dim matrix) :element-type (array-element-type matrix)
	      :displaced-to matrix :displaced-index-offset (* row (col-dim matrix))))

#|
;;; old versions

(defun vectorize (arr)
  (make-array (array-total-size arr)
	      :displaced-to arr
	      :element-type (array-element-type arr)))

(defun columnize (vec &key (size (array-total-size vec)))
  (make-array (list size 1)
	      :displaced-to vec
	      :displaced-index-offset 0
	      :element-type (array-element-type vec)))
|#

(defun displaced-matrix (arr &key (y-offset 0) (x-offset 0)
			     (y-size (y-dim arr)) (x-size (x-dim arr))
			     (y y-offset) (x x-offset) (y-dim y-size) (x-dim x-size))
  (unless (< (rank arr) 3)
    (error "Displaced-matrix implemented only for vectors or two-dimensional arrays."))
  (setq y (max (round y) 0)
	x (max (round x) 0)
	x-dim (min (round x-dim) (- (x-dim arr) x)) ;clip to image boundary
	y-dim (min (round y-dim) (- (y-dim arr) y)))
  (make-array (list y-dim x-dim) :displaced-to arr :element-type (array-element-type arr)
	      :displaced-index-offset (+ x (* y (x-dim arr)))))

#|
(defun displaced-matrix (arr &key (y-offset 0) (x-offset 0)
			     (y-size (y-dim arr)) (x-size (x-dim arr)))
  (unless (< (rank arr) 3)
    (error "Displaced-matrix implemented only for vectors or two-dimensional arrays."))
  (setq y-offset (max (round y-offset) 0)
	x-offset (max (round x-offset) 0)
	x-size (min (round x-size) (- (x-dim arr) x-offset)) ;clip to image boundary
	y-size (min (round y-size) (- (y-dim arr) y-offset)))
  (make-array (list y-size x-size) :displaced-to arr :element-type (array-element-type arr)
	      :displaced-index-offset (+ x-offset (* y-offset (x-dim arr)))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Utilities:

;;; For compatability with images.
(defmethod total-size ((a array))
  (array-total-size a))

(defmethod dimensions ((a array))
  (array-dimensions a))

(defmethod check-size ((a array) &rest a-list)
  (cond ((null a-list) a)
	((not (equal (array-dimensions a) (array-dimensions (car a-list))))
	 (error "Arrays have different dimensions." ))
	(t (apply 'check-size a-list))))

(defmethod rank ((a array))
  (array-rank a))

(defmethod depth ((a array))
  (let ((type (array-element-type a)))
    (cond ((eq type 'bit) 1)
	  ((and (listp type) (or (eq (car type) 'unsigned-byte)
				 (eq (car type) 'signed-byte)))
	   (cadr type))
	  (t (error "Don't know how to compute depth of array ~A" a)))))

(defvar *max-print-vals* 12
  "Maximum number of values which the print-values method prints in a line of the image.")
(eval-when (load eval) (setf (get '*max-print-vals* :type) '(integer 1 50)))

(defvar *x-print-range* '(0.0 1.0)
  "Default range of x values in an image to be printed by the print-values
function.  Should be a list of two numbers between 0.0 and 1.0")       
(eval-when (load eval) (setf (get '*x-print-range* :type) 'list))

(defvar *y-print-range* '(0.0 1.0)
    "Default range of y values in an image to be printed by the print-values
function.  Should be a list of two numbers between 0.0 and 1.0")       
(eval-when (load eval) (setf (get '*y-print-range* :type) 'list))

;;; *** Shouldn't this use x-print-range and y-print-range
(defmethod print-values ((arr array) &key (x 0) (y 0) 
			 (x-size (array-dimension arr 1)) 
			 (y-size (array-dimension arr 0)))
  (declare (special *max-print-vals*))
  (let ((x-step (ceiling (1+ x-size) *max-print-vals*))
	(y-step (ceiling (1+ y-size) *max-print-vals*))
	(x-start (round x))
	(x-stop (min (round (+ x x-size)) (x-dim arr)))
	(y-start (round y))
	(y-stop (min (round (+ y y-size)) (y-dim arr))))
    (format t "~%~5A" "")
    (do ((x-pos x-start (+ x-pos x-step)))
	((>= x-pos x-stop) (format t "~%~6A" ""))
      (format t "~5D " x-pos))
    (do ((x-pos x-start (+ x-pos x-step)))
	((>= x-pos x-stop) (format t "~%"))
      (format t "~6A" "-----"))
    (do ((y-pos y-start (+ y-pos y-step)))
	((>= y-pos y-stop) (format t "~%"))
      (format t "~4D|" y-pos)
      (do ((x-pos x-start (+ x-pos x-step)))
	  ((>= x-pos x-stop) (format t "~%"))
	(format t " ~5,1,,'*F" (aref arr y-pos x-pos))))))

(defmethod print-values ((arr vector)
			 &key (x 0) (x-size (length arr))
			 y y-size)
  (declare (special *max-print-vals*)
	   (ignore y y-size))
  (let ((x-step (ceiling (1+ x-size) *max-print-vals*))
	(x-start (round x))
	(x-stop (min (round (+ x x-size)) (x-dim arr))))
    (format t "~%~5A" "")
    (do ((x-pos x-start (+ x-pos x-step)))
	((>= x-pos x-stop) (format t "~%~6A" ""))
      (format t "~5D " x-pos))
    (do ((x-pos x-start (+ x-pos x-step)))
	((>= x-pos x-stop) (format t "~%"))
      (format t "~6A" "-----"))
    (format t "~4D|" 1)
    (do ((x-pos x-start (+ x-pos x-step)))
	((>= x-pos x-stop) (format t "~%"))
      (format t " ~5,1,,'*F" (aref arr x-pos)))))

(defmethod verify-monotone-increasing ((arr vector))
  (let ((max-drop 0))
    (loop for i from 1 below (length arr) do
	  (when (< (aref arr i) (aref arr (- i 1)))
	    (setq max-drop (max (- (aref arr (- i 1)) (aref arr i))))))
    (if (zerop max-drop)
	arr
	(warn "arr ~a not monotone, max drop=~a" arr max-drop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Similar, Copy, Coercion, Fill!:

(defmethod similar ((arr array) &rest keywords
		    &key
		    displaced-to
		    (static (and (allocated-array-p arr) (not displaced-to)))
		    (dimensions (dimensions arr))
                    (element-type (array-element-type arr))
		    &allow-other-keys)
  (remf keywords :dimensions)
  (remf keywords :static)
  (if static
      (apply #'allocate-array dimensions :element-type element-type keywords)
      (apply #'make-array dimensions :element-type element-type keywords)))


(defmethod copy ((arr array) &key (static (allocated-array-p arr))
		 ((:-> result) (similar arr :static static)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-copy-array arr result (total-size arr)))
	((8bit-arrays-p arr result)
	 (memcpy result arr (total-size arr)))
	((1bit-arrays-p arr result)
	 (bit-not arr result)
	 (bit-not result result))	;faster than doing bit-and and bit-ior
	(t
	 (with-displaced-vectors ((displaced-arr arr)
				  (displaced-result result))
	   (loop for i from 0 below (total-size arr)
		 with elt-type = (array-element-type result) do
		 (setf (aref displaced-result i) (coerce (aref displaced-arr i) elt-type))))))
  result)

(defmethod coerce-to-float ((arr array) &key
			    ((:-> result) (similar arr :element-type 'single-float)))
  (check-size arr result)
  (cond ((16bit-arrays-p arr)
	 (internal-16bit-to-float arr result (total-size arr)))
	((8bit-arrays-p arr)
	 (internal-8bit-to-float arr result (total-size arr)))
	(t (with-displaced-vectors ((displaced-arr arr)
				    (displaced-result result))
	     (declare (type (vector single-float) displaced-result)
		      (type vector displaced-arr))
	     (dotimes (i (length displaced-arr))
	       (declare (fixnum i))
	       (setf (aref displaced-result i) (float (aref displaced-arr i)))))))
  result)

(defmethod zero! ((arr array))
  (fill! arr 0)
  arr)

(defmethod fill! ((arr-1 array) (arr-2 array))
  (copy arr-2 :-> arr-1))

(defmethod fill! ((arr array) (vec vector))
  (with-displaced-vectors ((d-arr arr))
    (copy vec :-> d-arr))
  arr)

;; [tho] 2019-12-04 make sure internal const receives a double-float
;;       i dont' know why but the c-function uses double
;;;[tho] 2016-08-10
;; changed call to fill! with val arg to double-float
;; changed back because all function use (float val)
;; maybe change general float format
(defmethod fill! ((arr array) val)
  (let ((element-type (array-element-type arr))
	(arr-size (array-total-size arr)))
    (cond ((float-arrays-p arr)
	   (internal-const arr (float val 1.0d0) arr-size))
	  ((and (listp element-type) (or (eq (car element-type) 'unsigned-byte)
					 (eq (car element-type) 'signed-byte)))
	   (memset arr (round val) (* (ceiling (cadr element-type) 8) arr-size)))
	  ((equal element-type 'bit)
	   (if (zerop val)
	       (bit-xor arr arr arr)
	       (bit-eqv arr arr arr)))
	  #+lucid
	  (t (multiple-value-bind (sv offset total-size)
		 (sys:underlying-simple-vector arr)
	       (declare (type (vector * *) sv))
	       (setq val (coerce val element-type))
	       (do ((i offset (1+ i)))
		   ((>= i total-size) arr)
		 (setf (aref sv i) val))))
	  #-lucid
	  (t (with-displaced-vectors ((displaced-vect arr))
	       (setq val (coerce val element-type))
	       (dotimes (i (total-size arr))
		 (declare (fixnum i))
		 (setf (aref displaced-vect i) val))))))
  arr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Statistics:

(defmethod mean ((arr array) &key ignore-zeros)
  (cond ((float-arrays-p arr)
	 (internal-mean arr (total-size arr) (if ignore-zeros 1 0)))
	(t (with-displaced-vectors ((displaced-arr arr))
	     (let ((sum 0.0)
		   (num 0))
	       (loop for i from 0 below (total-size arr) do
		     (incf sum (aref displaced-arr i))
		     (if (or (not (zerop (aref displaced-arr i))) (not ignore-zeros))
			 (incf num)))
	       (/ sum num))))))

(defmethod variance ((arr array) &key ignore-zeros)
  (cond ((float-arrays-p arr)
	 (internal-variance arr (total-size arr) (if ignore-zeros 1 0)))
	(t (error "Variance implemented only for single-float or fixnum arrays"))))

(defmethod covariance ((vec1 vector) (vec2 vector))
  (- (/ (dot-product vec1 vec2) (length vec1))
     (* (mean vec1) (mean vec2))))

(defmethod correlation ((vec1 vector) (vec2 vector) &key (zero-val *div-by-zero-result*))
  (div (covariance vec1 vec2) (sqrt (* (variance vec1) (variance vec2))) :zero-val zero-val))

;;; Skew and Kurtosis not defined on arrays, but Third-moment and
;;; Fourth-moment are defined for arrays.

(defmethod third-moment ((arr array) &optional (mean (mean arr)))
  (cond ((float-arrays-p arr)
	 (internal-third-moment arr (total-size arr) mean))
	(t (error "Third-moment implemented only for single-float or fixnum arrays"))))

(defmethod fourth-moment ((arr array) &optional (mean (mean arr)))
  (cond ((float-arrays-p arr)
	 (internal-fourth-moment arr (total-size arr) mean))
	(t (error "Fourth-moment implemented only for single-float or fixnum arrays"))))


(defmethod sum-of ((arr array))
  (cond ((float-arrays-p arr)
	 (internal-sum-of arr (total-size arr)))
	(t (with-displaced-vectors ((displaced-arr arr))
	     (loop for i from 0 below (total-size arr)
		   sum (aref displaced-arr i))))))

(defmethod product-of ((arr array))
  (cond ((float-arrays-p arr)
	 (internal-product-of arr (total-size arr)))
	(t (with-displaced-vectors ((displaced-arr arr))
	     (let ((prod 1))
	       (dotimes (i (length displaced-arr))
		 (setq prod (* prod (aref displaced-arr i))))
	       prod)))))

;;; returns multiple values (min-value, 1d-location)
(defmethod minimum ((arr array))
  (let (vals)
    (cond ((float-arrays-p arr)
	   (setq vals (make-array 2 :element-type 'single-float))
	   (internal-min-of arr (total-size arr) vals))
	(t (with-displaced-vectors ((displaced-arr arr))
	   (setq vals (make-array 2))
	     (let ((min (aref displaced-arr 0))
		   (location 0))
	       (loop for i from 0 below (total-size arr) do
		     (when (< (aref displaced-arr i) min)
		       (setq min (aref displaced-arr i))
		       (setq location i)))
	       (setf (aref vals 0) min)
	       (setf (aref vals 1) location)))))
    (values (aref vals 0) (round (aref vals 1)))))

;;; returns multiple values (min-value, 1d-location)
(defmethod maximum ((arr array))
  (let (vals)
    (cond ((float-arrays-p arr)
	   (setq vals (make-array 2 :element-type 'single-float))
	   (internal-max-of arr (total-size arr) vals))
	(t (with-displaced-vectors ((displaced-arr arr))
	   (setq vals (make-array 2))
	     (let ((max (aref displaced-arr 0))
		   (location 0))
	       (loop for i from 0 below (total-size arr) do
		     (when (> (aref displaced-arr i) max)
		       (setq max (aref displaced-arr i))
		       (setq location i)))
	       (setf (aref vals 0) max)
	       (setf (aref vals 1) location)))))
    (values (aref vals 0) (round (aref vals 1)))))

(defmethod minimum-location ((arr array))
  (multiple-value-bind (min-value min-location)
      (minimum arr)
    (declare (ignore min-value))
    (multiple-value-list (floor min-location (x-dim arr)))))

(defmethod maximum-location ((arr array))
  (multiple-value-bind (max-value max-location)
      (maximum arr)
    (declare (ignore max-value))
    (multiple-value-list (floor max-location (x-dim arr)))))

(defmethod minimum-location ((vec vector))
  (multiple-value-bind (min-value min-location)
      (minimum vec)
    (declare (ignore min-value))
    min-location))

(defmethod maximum-location ((vec vector))
  (multiple-value-bind (max-value max-location)
      (maximum vec)
    (declare (ignore max-value))
    max-location))


;;; Returns multiple values, (rng, mn, mx)
(defmethod range ((arr array))
  (let (vals)
    (cond ((float-arrays-p arr)
	   (setq vals (make-array 2 :element-type 'single-float))
	   (internal-range arr (total-size arr) vals))
	(t (with-displaced-vectors ((displaced-arr arr))
	     (setq vals (make-array 2))
	     (let* ((mn (aref displaced-arr 0))
		    (mx mn) 
		    val)
	       (loop for i from 0 below (total-size arr) do
		     (setq val (aref displaced-arr i))
		     (cond ((< val mn) (setq mn val))
			   ((> val mx) (setq mx val))))
	       (setf (aref vals 0) mn)
	       (setf (aref vals 1) mx)))))
    (values (- (aref vals 1) (aref vals 0)) (aref vals 0) (aref vals 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comparison Operations:

(defmethod almost-equal ((x number) (y number) &key (tolerance *tolerance*))
  (unless (> (abs (- x y)) tolerance)
    x))


(defmethod almost-equal ((arr-1 array) (arr-2 array) &key (tolerance *tolerance*))
  (cond ((float-arrays-p arr-1 arr-2)
	 (when (or (eq arr-1 arr-2)
		   (zerop (internal-almost-equal arr-1 arr-2 (total-size arr-1) tolerance)))
	   arr-1))
	(t (error "almost-equal only implemented for single-float arrays"))))
		 
		 
(defmethod almost-equal ((arr array) (val number) &key (tolerance *tolerance*))
  (cond ((float-arrays-p arr)
	 (when (zerop (internal-sc-almost-equal arr (float val) (total-size arr) tolerance))
	   arr))
	(t (error "almost-equal only implemented for single-float arrays"))))

(defmethod almost-equal ((val number) (arr array) &key (tolerance *tolerance*))
  (cond ((float-arrays-p arr)
	 (when (zerop (internal-sc-almost-equal arr (float val) (total-size arr) tolerance))
	   val))
	(t (error "almost-equal only implemented for single-float arrays"))))

(defun almost-zero (thing &rest keys)
  (apply #'almost-equal thing 0.0 keys))

(defmethod scalar-multiple ((vec-1 vector) (vec-2 vector) &key (tolerance *tolerance*))
  (cond ((float-arrays-p vec-1 vec-2)
	 (let ((ratio (div (dot-product vec-2 vec-1) (dot-product vec-2 vec-2))))
	   (when (zerop (internal-scalar-multiple vec-1 vec-2 (float ratio) (length vec-1)
						  (float (max tolerance (abs (* ratio tolerance))))))
	     ratio)))
	(t (error "scalar-multiple only implemented for single-float arrays"))))

(defmethod scalar-multiple ((arr-1 array) (arr-2 array) &key (tolerance *tolerance*))
  (cond ((float-arrays-p arr-1 arr-2)
	 (scalar-multiple (vectorize arr-1) (vectorize arr-2) :tolerance tolerance))
	(t (error "scalar-multiple only implemented for single-float arrays"))))



(defmethod mean-square-error ((arr1 array) (arr2 array))
  (check-size arr1 arr2)
  (cond ((float-arrays-p arr1 arr2)
         (internal-mean-sq-err arr1 arr2 (total-size arr1)))
        (t (with-displaced-vectors ((displaced-arr1 arr1)
                                    (displaced-arr2 arr2))
             (/ (loop for i from 0 below (length displaced-arr1)
                      sum (sqr (- (aref displaced-arr1 i) (aref displaced-arr2 i))))
                (total-size arr1))))))

(defun sum-square-error (thing1 thing2)
  (* (mean-square-error thing1 thing2) (total-size thing1)))

(defmethod mean-abs-error ((arr1 array) (arr2 array))
  (check-size arr1 arr2)
  (cond ((float-arrays-p arr1 arr2)
	 (internal-mean-abs-err arr1 arr2 (total-size arr1)))
	(t (error "Mean-square-error implemented only for single-float or fixnum arrays"))))

;;; *** Should write C code for this...
(defmethod max-abs-error ((arr1 array) (arr2 array))
  (with-static-arrays ((temp (similar arr1)))
    (abs-error arr1 arr2 :-> temp)
    (maximum temp)))

;; *** should write C code for this...
(defmethod modulo ((arr array) number &key ((:-> result) (similar arr)))
  (with-displaced-vectors ((d-arr arr)
			   (d-res result))
    (dotimes (i (length d-res))
      (declare (fixnum i))
      (setf (aref d-res i)
	    (mod (aref d-arr i) number)))
    result))

(defmethod sgn ((x number) &key (zero 0.0))
  (cond ((> x zero) 1.0)
	((= x zero) 0.0)
	(t -1.0)))

(defmethod sgn ((arr array) &key (zero 0.0) ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-sgn arr result zero (total-size arr)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   for x = (aref displaced-arr i)
		   do (setf (aref displaced-result i)
			    (cond ((plusp x) 1.0)
				  ((zerop x) 0.0)
				  (t -1.0)))))))
  result)

;;; Minimum as a pointop on two arrays
(defmethod point-minimum ((arr1 array) (arr2 array) &key ((:-> result) (similar arr1)))
  (check-size arr1 arr2 result)
  (cond ((float-arrays-p arr1 arr2 result)
	 (internal-im-min arr1 arr2 result (total-size result)))
	(t (error "Point-minimum implemented only for single-float or fixnum arrays")))
  result)

(defmethod point-minimum ((arr array) (val number) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-sc-im-min arr result (total-size result) (float val)))
	(t (error "Point-minimum implemented only for single-float or fixnum arrays")))
  result)

(defmethod point-minimum ((val number) (arr array) &key ->)
  (point-minimum arr val :-> ->))

(defmethod point-maximum ((arr1 array) (arr2 array) &key ((:-> result) (similar arr1)))
  (check-size arr1 arr2 result)
  (cond ((float-arrays-p arr1 arr2 result)
	 (internal-im-max arr1 arr2 result (total-size result)))
	(t (error "Point-maximum implemented only for single-float or fixnum arrays")))
  result)

(defmethod point-maximum ((arr array) (val number) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-sc-im-max arr result (total-size result) (float val)))
	(t (error "Point-maximum implemented only for single-float or fixnum arrays")))
  result)

(defmethod point-maximum ((val number) (arr array) &key ->)
  (point-maximum arr val :-> ->))

(defmethod square-error ((arr1 array) (arr2 array) &key ((:-> result) (similar arr1)))
  (check-size arr1 arr2 result)
  (cond ((float-arrays-p arr1 arr2 result)
	 (internal-sq-err arr1 arr2 result (total-size result)))
	(t (error "Square-error implemented only for single-float or fixnum arrays")))
  result)

(defmethod abs-error ((arr1 array) (arr2 array) &key ((:-> result) (similar arr1)))
  (check-size arr1 arr2 result)
  (cond ((float-arrays-p arr1 arr2 result)
	 (internal-abs-err arr1 arr2 result (total-size result)))
	(t (error "Abs-error implemented only for single-float or fixnum arrays")))
  result)

;;; Greater-than, less-than, etc
;;; Arg arrays and result array can be any type.
;;; If no result is passed, then these guys cons up a bit array.

(defun internal-compare-arr-arr (arr1 arr2 result op)
  (with-displaced-vectors ((displaced-arr1 arr1)
			   (displaced-arr2 arr2)
			   (displaced-result result))
    (loop for i from 0 below (length displaced-arr1)
	  with elt-type = (array-element-type result) do
	  (setf (aref displaced-result i)
		(coerce (if (funcall op (aref displaced-arr1 i) (aref displaced-arr2 i))
			    1 0)
			elt-type))))
  result)

(defun internal-compare-arr-num (arr num result op)
  (with-displaced-vectors ((displaced-arr arr)
			   (displaced-result result))
    (loop for i from 0 below (length displaced-arr)
	  with elt-type = (array-element-type result) do
	  (setf (aref displaced-result i)
		(coerce (if (funcall op (aref displaced-arr i) num)
			    1 0)
			elt-type))))
  result)

(defun internal-compare-num-arr (num arr result op)
  (with-displaced-vectors ((displaced-arr arr)
			   (displaced-result result))
    (loop for i from 0 below (length displaced-arr)
	  with elt-type = (array-element-type result) do
	  (setf (aref displaced-result i)
		(coerce (if (funcall op num (aref displaced-arr i))
			    1 0)
			elt-type))))
  result)

(defmethod greater-than ((arr1 array) (arr2 array) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr1)
	      (allocate-array (dimensions arr1) :element-type 'bit)
	      (make-array (dimensions arr1) :element-type 'bit))))
  (check-size arr1 arr2 result)
  (internal-compare-arr-arr arr1 arr2 result '>)
  result)

(defmethod less-than ((arr1 array) (arr2 array) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr1)
	      (allocate-array (dimensions arr1) :element-type 'bit)
	      (make-array (dimensions arr1) :element-type 'bit))))
  (check-size arr1 arr2 result)
  (internal-compare-arr-arr arr1 arr2 result '<)
  result)

(defmethod equal-to ((arr1 array) (arr2 array) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr1)
	      (allocate-array (dimensions arr1) :element-type 'bit)
	      (make-array (dimensions arr1) :element-type 'bit))))
  (check-size arr1 arr2 result)
  (internal-compare-arr-arr arr1 arr2 result '=)
  result)

(defmethod greater-than-or-equal-to ((arr1 array) (arr2 array) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr1)
	      (allocate-array (dimensions arr1) :element-type 'bit)
	      (make-array (dimensions arr1) :element-type 'bit))))
  (check-size arr1 arr2 result)
  (internal-compare-arr-arr arr1 arr2 result '>=)
  result)

(defmethod less-than-or-equal-to ((arr1 array) (arr2 array) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr1)
	      (allocate-array (dimensions arr1) :element-type 'bit)
	      (make-array (dimensions arr1) :element-type 'bit))))
  (check-size arr1 arr2 result)
  (internal-compare-arr-arr arr1 arr2 result '<=)
  result)

(defmethod greater-than ((arr array) (num number) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr)
	      (allocate-array (dimensions arr) :element-type 'bit)
	      (make-array (dimensions arr) :element-type 'bit))))
  (check-size arr result)
  (internal-compare-arr-num arr num result '>)
  result)

(defmethod less-than ((arr array) (num number) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr)
	      (allocate-array (dimensions arr) :element-type 'bit)
	      (make-array (dimensions arr) :element-type 'bit))))
  (check-size arr result)
  (internal-compare-arr-num arr num result '<)
  result)

(defmethod equal-to ((arr array) (num number) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr)
	      (allocate-array (dimensions arr) :element-type 'bit)
	      (make-array (dimensions arr) :element-type 'bit))))
  (check-size arr result)
  (internal-compare-arr-num arr num result '=)
  result)

(defmethod greater-than-or-equal-to ((arr array) (num number) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr)
	      (allocate-array (dimensions arr) :element-type 'bit)
	      (make-array (dimensions arr) :element-type 'bit))))
  (check-size arr result)
  (internal-compare-arr-num arr num result '>=)
  result)

(defmethod less-than-or-equal-to ((arr array) (num number) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr)
	      (allocate-array (dimensions arr) :element-type 'bit)
	      (make-array (dimensions arr) :element-type 'bit))))
  (check-size arr result)
  (internal-compare-arr-num arr num result '<=)
  result)

(defmethod greater-than ((num number) (arr array) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr)
	      (allocate-array (dimensions arr) :element-type 'bit)
	      (make-array (dimensions arr) :element-type 'bit))))
  (check-size arr result)
  (internal-compare-num-arr num arr result '>)
  result)

(defmethod less-than ((num number) (arr array) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr)
	      (allocate-array (dimensions arr) :element-type 'bit)
	      (make-array (dimensions arr) :element-type 'bit))))
  (check-size arr result)
  (internal-compare-num-arr num arr result '<)
  result)

(defmethod equal-to ((num number) (arr array) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr)
	      (allocate-array (dimensions arr) :element-type 'bit)
	      (make-array (dimensions arr) :element-type 'bit))))
  (check-size arr result)
  (internal-compare-num-arr num arr result '=)
  result)

(defmethod greater-than-or-equal-to ((num number) (arr array) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr)
	      (allocate-array (dimensions arr) :element-type 'bit)
	      (make-array (dimensions arr) :element-type 'bit))))
  (check-size arr result)
  (internal-compare-num-arr num arr result '>=)
  result)

(defmethod less-than-or-equal-to ((num number) (arr array) &key ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p arr)
	      (allocate-array (dimensions arr) :element-type 'bit)
	      (make-array (dimensions arr) :element-type 'bit))))
  (check-size arr result)
  (internal-compare-num-arr num arr result '<=)
  result)

#|
;;; Comparison to see how slow it is.  These generic methods on images
;;; are significantly slower (factor of 2) than the special cases
;;; (using loop-over-image-pixels with declarations) implemented in
;;; bit-image.lisp.
(defmethod greater-than ((im1 image) (im2 image) &key ->)
  (with-result ((result ->) 
		(list :class 'bit-image
		      :dimensions (dimensions (check-size im1 im2)))
		'greater-than im1 im2)
    (greater-than (data im1) (data im2) :-> (data result))
    result))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Arithmetic Operations:

(defmethod add ((const number) (arr array) &key ((:-> result) (similar arr)))
  (add arr const :-> result))

(defmethod add ((arr array) (const number)
		&key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-sc-add arr result (total-size arr) (dfloat const)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		 with elt-type = (array-element-type result) do
		 (setf (aref displaced-result i)
		       (coerce (+ const (aref displaced-arr i)) elt-type))))))
  result)

(defmethod add ((arr1 array) (arr2 array) &key ((:-> result) (similar arr1)))
  (check-size arr1 arr2 result)
  (cond ((float-arrays-p arr1 arr2 result)
	 (internal-add arr1 arr2  result (total-size arr1)))
	(t (with-displaced-vectors ((displaced-arr1 arr1) 
				    (displaced-arr2 arr2) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr1)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) 
			 (coerce (+ (aref displaced-arr1 i) (aref displaced-arr2 i)) elt-type))))))
  result)


(defmethod mul ((const number) (arr array)
		&key ((:-> result) (similar arr)))
  (mul arr const :-> result))

(defmethod mul ((arr array) (const number) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-sc-mul arr result (total-size arr) (dfloat const)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) (coerce (* const (aref displaced-arr i)) elt-type))))))
  result)

(defmethod mul ((arr1 array) (arr2 array) &key ((:-> result) (similar arr1)))
  (check-size arr1 arr2 result)
  (cond ((float-arrays-p arr1 arr2 result)
	 (internal-mul arr1 arr2  result (total-size arr1)))
	(t (with-displaced-vectors ((displaced-arr1 arr1) 
				    (displaced-arr2 arr2) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr1)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) 
			 (coerce (* (aref displaced-arr1 i) (aref displaced-arr2 i)) elt-type))))))
  result)

(defmethod sub ((arr array) (const number) &key ((:-> res) (similar arr)))
  (add arr (- const) :-> res))

(defmethod sub ((const number) (arr array) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-sc-sub arr result (total-size arr) (float const)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i)
			 (coerce (- const (aref displaced-arr i)) elt-type))))))
  result)

(defmethod sub ((arr1 array) (arr2 array) &key ((:-> result) (similar arr1)))
  (check-size arr1 arr2 result)
  (cond ((float-arrays-p arr1 arr2 result)
	 (internal-sub arr1 arr2  result (total-size arr1)))
	(t (with-displaced-vectors ((displaced-arr1 arr1) 
				    (displaced-arr2 arr2) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr1)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) 
			 (coerce (- (aref displaced-arr1 i) (aref displaced-arr2 i)) elt-type))))))
  result)


(defmethod div ((arr array) (val number)
                &key (zero-val *div-by-zero-result*) ((:-> result) (similar arr))
		suppress-warning)
  (check-size arr result)
  (if (zerop val)
      (progn
	(unless suppress-warning
	  (warn "Divide by zero: ~A of ~A values set to plus or minus ~A"
		(total-size arr) (total-size arr) zero-val))
	(cond ((float-arrays-p arr result)
	       (internal-const result (float zero-val) (total-size result)))
	      (t (with-displaced-vectors ((displaced-result result))
		   (loop for i from 0 below (total-size arr)
			 with elt-type = (array-element-type result) do
			 (setf (aref displaced-result i) (coerce zero-val elt-type)))))))
      (mul arr (/ 1.0 val) :-> result))
  result)

;;; Updated warning message to tell the user that the sign of the zero-val
;;; result may be plus or minus. EJC 7.25.91
(defmethod div ((val number) (arr array)
                &key (zero-val *div-by-zero-result*) ((:-> result) (similar arr))
		suppress-warning)
  (check-size arr result)
  (let ((num-zeros 0))
    (cond ((float-arrays-p arr result)
           (setq zero-val (float zero-val))
           (setq num-zeros (internal-sc-div arr result (total-size arr) (float val) zero-val)))
          (t (with-displaced-vectors ((displaced-arr arr)
                                      (displaced-result result))
               (loop for i from 0 below (total-size arr)
                     with elt-type = (array-element-type result) do
                     (setf (aref displaced-result i)
                           (coerce (if (zerop (aref displaced-arr i))
                                       (progn (incf num-zeros)
                                              (* (signum val) zero-val))
                                       (/ val (aref displaced-arr i)))
                                   elt-type))))))
    (when (and (> num-zeros 0) (not suppress-warning))
      (warn "Divide by zero: ~A of ~A values set to plus or minus ~A"
            num-zeros (total-size arr) zero-val)))
  result)

;;; Updated warning message to tell the user that the sign of the zero-val
;;; result may be plus or minus. EJC 7.25.91
(defmethod div ((arr1 array) (arr2 array)
                &key suppress-warning
                (zero-val *div-by-zero-result*) ((:-> result) (similar arr1)))
  (check-size arr1 arr2 result)
  (let ((num-zeros 0))
    (cond ((float-arrays-p arr1 arr2 result)
           (setq zero-val (float zero-val))
           (setq num-zeros (internal-div arr1 arr2  result (total-size arr1) zero-val)))
          (t (with-displaced-vectors ((displaced-arr1 arr1)
                                      (displaced-arr2 arr2)
                                      (displaced-result result))
               (loop for i from 0 below (total-size arr1)
                     with elt-type = (array-element-type result) do
                     (setf (aref displaced-result i)
                           (coerce (if (zerop (aref displaced-arr2 i))
                                       (progn (incf num-zeros)
                                              (* (signum (aref displaced-arr1 i)) zero-val))
                                       (/ (aref displaced-arr1 i) (aref displaced-arr2 i)))
                                   elt-type))))))
    (when (and (> num-zeros 0) (not suppress-warning))
      (warn "Divide by zero: ~A of ~A values set to plus or minus ~A"
            num-zeros (total-size arr1) zero-val)))
  result)

(defmethod negate ((arr array) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-negate arr result (total-size arr)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (dotimes (i (total-size arr))
	       (declare (fixnum i))
	       (setf (aref displaced-result i)
		     (- (aref displaced-arr i)))))))
  result)


(defmethod linear-xform ((arr array) scale offset &key ((:-> result) (similar arr)))
  (cond ((float-arrays-p arr result)
	 (internal-linear-xform arr result (total-size arr) (float scale) (float offset)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) (coerce (+ (* (aref displaced-arr i) scale) offset)
							   elt-type))))))
  result)

(defmethod square ((arr array) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-square arr result (total-size arr)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) (coerce (sqr (aref displaced-arr i)) elt-type))))))
  result)

(defmethod square-root ((arr array) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-sqrt arr result (total-size arr)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) (coerce (sqrt (aref displaced-arr i)) elt-type))))))
  result)

(defmethod half-square ((arr array) &key ((:-> result) (similar arr)))
  (clip result 0.0 most-positive-single-float :-> result)
  (square arr :-> result))

(defmethod abs-value ((arr array) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-abs arr result (total-size arr)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) (coerce (abs (aref displaced-arr i)) elt-type))))))
  result)

(defmethod power ((arr array) (const number) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-pow-sc arr result (total-size arr) (float const)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) (coerce (expt (aref displaced-arr i) const) elt-type))))))
  result)

(defmethod power ((const number) (arr array) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-sc-pow arr result (total-size arr) (float const)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) (coerce (expt const (aref displaced-arr i)) elt-type))))))
  result)

(defmethod power ((arr1 array) (arr2 array) &key ((:-> result) (similar arr1)))
  (check-size arr1 arr2 result)
  (cond ((float-arrays-p arr1 arr2 result)
	 (internal-pow arr1 arr2 result (total-size arr1)))
	(t (with-displaced-vectors ((displaced-arr1 arr1) 
				    (displaced-arr2 arr2) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr1)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) 
			 (coerce (expt (aref displaced-arr1 i) (aref displaced-arr2 i)) elt-type))))))
  result)

(defmethod natural-logarithm ((arr array)
			      &key (zero-val *div-by-zero-result*)
			      suppress-warning
			      ((:-> result) (similar arr)))
  (check-size arr result)
  (let ((num-zeros 0))
    (setq zero-val (float zero-val))
    (cond ((float-arrays-p arr result)
	   (setq num-zeros (internal-ln arr result (total-size arr) zero-val)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i) 
			 (coerce (if (zerop (aref displaced-arr i))
				     (progn (incf num-zeros)
					    (* (signum (aref displaced-arr i)) zero-val))
				     (log (aref displaced-arr i)))
				 elt-type))))))
    (when (and (> num-zeros 0) (not suppress-warning))
      (warn "Log of zero: ~A of ~A values set to ~A"
	    num-zeros (total-size arr) zero-val)))
  result)



(defmethod logarithm ((x number) (base number) &key)
  (log x base))

(defmethod logarithm ((arr array) (base number) &key ((:-> result) (similar arr))
		      suppress-warning
		      (zero-val *div-by-zero-result*))
  (natural-logarithm arr :zero-val zero-val :suppress-warning suppress-warning :-> result)
  (div result (log base) :-> result))

(defmethod logarithm ((x number) (base array) &key ((:-> result) (similar base))
		      suppress-warning (zero-val *div-by-zero-result*))
  (natural-logarithm base :zero-val zero-val :suppress-warning suppress-warning :-> result)
  (div (log x) result :-> result))

;;; If binsize is nil, just apply func to each element of the array.
;;; If a number, build a lookup table of the function, and apply using
;;; linear interpolation.  *** Must be kept consistent with
;;; make-discrete-function.
(defmethod point-operation ((arr array) (func t) &key binsize
			    ((:-> result) (similar arr)))
  (check-size arr result)
  (let ((a-size (total-size arr))
	(a-elt-type (array-element-type result)))
    (cond ((null binsize)
	   (with-displaced-vectors ((displaced-arr arr)  (displaced-result result))
	     (dotimes (i a-size)
	       (setf (aref displaced-result i)
		     (coerce (funcall func (aref displaced-arr i)) a-elt-type)))))
	  ((<= 0.0 binsize (range arr))
	   (setq binsize (float binsize))
	   (let* ((start (minimum arr))
		  (t-size (1+ (ceiling (/-0 (range arr) binsize 1)))))
	     (with-static-arrays
		 ((table (allocate-array t-size :element-type a-elt-type)))
	       (loop for i from 0 below t-size
		     for arg = start then (+ arg binsize) do
		     (setf (aref table i) (coerce (funcall func arg) a-elt-type)))
	       (cond ((float-arrays-p arr result table)
		      (internal-pointop arr result a-size table t-size start binsize))
		     (t (error "Point-operation implemented only for single-float ~
                              and fixnum arrays"))))))
	  (t (error "Bad binsize: ~A" binsize))))
  result)

(defmethod periodic-point-operation ((arr array) func period
				     &key binsize
				     ((:-> result) (similar arr)))
  (check-size arr result)
  (let ((a-size (total-size arr))
	(a-elt-type (array-element-type result)))
    (cond ((null binsize)
	   (with-displaced-vectors ((displaced-arr arr)  (displaced-result result))
	     (dotimes (i a-size)
	       (setf (aref displaced-result i)
		     (coerce (funcall func (mod (aref displaced-arr i) period))
			     a-elt-type)))))
	  ((<= 0.0 binsize period)
	   (let ((t-size (1+ (ceiling (abs (/-0 period binsize 1))))))
	     ;; adjust binsize to divide period exactly
	     (setq binsize (float (/-0 period (- t-size 1) 1)))
	     (with-static-arrays
		 ((table (allocate-array t-size :element-type a-elt-type)))
	       (loop for i from 0 below t-size
		     for arg = 0.0 then (+ arg binsize)
		     do (setf (aref table i) (coerce (funcall func arg) a-elt-type)))
	       (cond ((float-arrays-p arr result table)
		      (internal-periodic-pointop arr result a-size table t-size (float* binsize)))
		     (t (error "Periodic-point-operation implemented only for ~
                                single-float and fixnum arrays"))))))
	  (t (error "Bad binsize: ~A" binsize))))
  result)

;;; Gamma correct an array.  NOTE: Only gamma corrects the range from
;;; pedestal to (+ pedestal scale), since outside this range is
;;; clipped No longer called by display code, but a useful function to
;;; have around.
;;; *** should probably modify binsize according to *tolerance* and gamma.
(defmethod gamma-correct
    ((arr array) gamma
     &key ((:-> result) (similar arr))
     (below (minimum arr))
     (above (maximum arr))
     (binsize (/ (- above below) (get-default 'discrete-function :size))))
  (let ((range (- above below)))
    (with-static-arrays ((clipped-arr (clip arr below above)))
      (point-operation
       clipped-arr
       #'(lambda (x) (+ below (* range (expt (/ (- x below) range) gamma))))
       :binsize binsize
       :-> result))))

(defmethod clip ((arr array) below above &key ((:-> result) (similar arr)))
  (cond ((float-arrays-p arr result)
	 (internal-clip arr result (total-size arr) (float below) (float above)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i)
			 (coerce (cond ((< (aref displaced-arr i) below) below)
				       ((> (aref displaced-arr i) above) above)
				       (t (aref displaced-arr i)))
				 elt-type))))))
  result)

(defmethod round. ((arr array) &key (divisor 1.0) ((:-> result) (similar arr)))
  (cond ((float-arrays-p arr result)
	 (internal-round arr result (total-size arr) (float divisor)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i)
			 (coerce (round (aref displaced-arr i) divisor)
				 elt-type))))))
  result)

(defmethod truncate. ((arr array) &key (divisor 1.0) ((:-> result) (similar arr)))
  (cond ((float-arrays-p arr result)
	 (internal-truncate arr result (total-size arr) (float divisor)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i)
			 (coerce (truncate (aref displaced-arr i) divisor)
				 elt-type)))))))

(defmethod floor. ((arr array) &key (divisor 1.0) ((:-> result) (similar arr)))
  (cond ((float-arrays-p arr result)
	 (internal-floor arr result (total-size arr) (float divisor)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (loop for i from 0 below (total-size arr)
		   with elt-type = (array-element-type result) do
		   (setf (aref displaced-result i)
			 (coerce (floor (aref displaced-arr i) divisor)
				 elt-type)))))))

(defmethod quantize ((arr array) &key 
		     (binsize (/ (range arr) (get-default 'discrete-function :size))) 
		     (origin (mean arr))
		     ((:-> result) (similar arr)))
  (setq binsize (if (> binsize 0.0) (float binsize) 1.0))
  (decf origin (* binsize (round (- origin (minimum arr)) binsize)))
  (cond ((float-arrays-p arr result)
	 (internal-quantize arr result (total-size arr) origin binsize))
	(t (error "Quantize implemented only for single-float or fixnum arrays")))
  result)

(defmethod shuffle ((vec vector) &key ((:-> result) (similar vec)))
  (unless (eq vec result)
    (copy vec :-> result))
  (loop with size = (length result)
	for i from 0 below size
	for j from size by -1
	for rand = (+ i (random j))
	for tmp = (aref result i)
	do
	(setf (aref result i) (aref result rand))
	(setf (aref result rand) tmp))
  result)

(defmethod shuffle ((arr array) &key ((:-> result) (similar arr)))
  (unless (eq arr result)
    (copy arr :-> result))
  (with-displaced-vectors ((v-res result))
    (shuffle v-res :-> v-res))
  result)

(defmethod randomize ((signal array) (noise array) &key ((:-> result) (similar signal)))
  (check-size signal noise result)
  (unless (eq signal result)
    (copy signal :-> result))
  (with-displaced-vectors ((d-noise noise)
			   (d-result result))
    (let (random)
      (dotimes (index (length d-result))
	(declare (fixnum index))
	(setq random (abs (aref d-noise index)))
	(when (plusp random)
	  (incf (aref d-result index)
		(random (* 2.0 random)))))))
  (sub result noise :-> result)
  result)

(defmethod randomize ((signal array) (noise number) &key ((:-> result) (similar signal)))
  (check-size signal result)
  (unless (eq signal result)
    (copy signal :-> result))
  (let ((twice-noise (* 2 (abs noise))))
    (when (plusp twice-noise)
      (with-displaced-vectors ((d-result result))
	(dotimes (index (length d-result))
	  (declare (fixnum index))
	  (incf (aref d-result index)
		(- (random twice-noise) noise))))))
  result)

;;; Some internal functions that operate on pairs of arrays/vectors
;;; (e.g., interpreting one array as real part and the other as
;;; imaginary part):
(defun array-magnitude (R-arr I-arr &key ((:-> result) (similar R-arr)))
  (cond ((float-arrays-p R-arr I-arr result)
	 (internal-sqrt-sum-of-squares R-arr I-arr result (total-size R-arr)))
	(t (array-square-magnitude R-arr I-arr :-> result)
	   (square-root result :-> result)))
  result)

(defun array-abs-value (R-arr I-arr &key ((:-> result) (similar R-arr)))
  (cond ((float-arrays-p R-arr I-arr result)
	 (internal-sqrt-sum-of-squares R-arr I-arr result (total-size R-arr)))
	(t (array-square-magnitude R-arr I-arr :-> result)
	   (square-root result :-> result)))
  result)

(defun array-square-magnitude (R-arr I-arr &key ((:-> result) (similar R-arr)))
  (cond ((float-arrays-p R-arr I-arr result)
	 (internal-sum-of-squares R-arr I-arr result (total-size R-arr)))
	(t (with-static-arrays
	       ((R-sqr (allocate-array (dimensions R-arr)
				       :element-type (array-element-type result)))
		(I-sqr (allocate-array (dimensions R-arr)
				       :element-type (array-element-type result))))
	     (square R-arr :-> R-sqr)
	     (square I-arr :-> I-sqr)
	     (add R-sqr I-sqr :-> result))))
  result)

(defun array-complex-phase (R-arr I-arr &key ((:-> result) (similar R-arr)))
  (cond ((float-arrays-p R-arr I-arr result)
	 (internal-phase R-arr I-arr result (total-size R-arr)))
	(t (error "Complex-phase implemented only for single-float arrays")))
  result)


(defmethod correlate ((arr1 array) (arr2 array) &key 
		      (x 0) (y 0) (x-dim (x-dim arr1)) (y-dim (y-dim arr1))
		      ((:-> result) (similar arr1 :element-type 'single-float
					     :dimensions (list y-dim x-dim))))
  (check-size arr1 arr2)
  (with-static-arrays ((tmp (similar arr2)))
    (dotimes (j y-dim)
      (declare (fixnum j))
      (dotimes (i x-dim)
	(declare (fixnum i))
	(setf (aref result j i)
	      (dot-product arr1 (circular-shift arr2 :y (+ j y) :x (+ i x) :-> tmp))))))
    result)

;; To make correlate on one-d-images work, and added displaced-result.  DH 2/10/93
(defmethod correlate ((vec1 vector) (vec2 vector) &key 
		      (x 0) y (x-dim (x-dim vec1)) y-dim
		      ((:-> result) (similar vec1 :element-type 'single-float
					     :dimensions x-dim)))
  (check-size vec1 vec2)
  ;;(when (or y y-dim) (error "Cannot operate on y dimension of ~a or ~a" vec1 vec2))
  (let ((displaced-result (vectorize result)))
    (with-static-arrays ((tmp (similar vec2)))
      (dotimes (i x-dim)
	(declare (fixnum i))
	(setf (aref displaced-result i) (dot-product vec1 (circular-shift vec2 :x (+ i x) :-> tmp))))))
  result)

(defmethod convolve ((arr1 array) (arr2 array) &key 
		     (x 0) (y 0) (x-dim (x-dim arr1)) (y-dim (y-dim arr1))
		     ((:-> result) (similar arr1 :element-type 'single-float
					    :dimensions (list y-dim x-dim))))
  (error "Convolve not yet implemented for 2d arrays"))

(defmethod convolve ((vec1 vector) (vec2 vector) &key 
		     (x 0) y (x-dim (x-dim vec1)) y-dim
		     ((:-> result) (similar vec1 :element-type 'single-float
					    :dimensions x-dim)))
  (check-size vec1 vec2)
  ;;(when (or y y-dim) (error "Cannot operate on y dimension of ~a or ~a" vec1 vec2))
  (let ((displaced-result (vectorize result)))
    (with-static-arrays ((tmp (similar vec2)))
      (dotimes (i x-dim)
	(declare (fixnum i))
	(setf (aref displaced-result i) (dot-product vec1 (circular-shift vec2 :x (- x i) :-> tmp))))))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Geometric Operations:

;;; Many of these only make sense for 2D arrays or vectors.

(defmethod circular-shift ((arr array) &key
			   (y-shift 0) (x-shift 0)
			   (y y-shift) (x x-shift)
			   ((:-> result) (similar arr)))

  (unless (= (rank arr) 2)
    (error "Circular-shift implemented only for vectors or two-dimensional arrays."))
  (let* ((ysh (mod (round y) (y-dim arr)))
	 (xsh (mod (round x) (x-dim arr)))
	 (ygcf (gcd ysh (y-dim arr)))
	 (xgcf (gcd xsh (x-dim arr))))
    (cond ((float-arrays-p arr result)
	   (internal-circular-shift arr result (x-dim arr) (y-dim arr) xsh ysh xgcf ygcf))
	  ((1bit-arrays-p arr result)
	   (internal1-circular-shift arr result (x-dim arr) (y-dim arr) xsh ysh xgcf ygcf))
	  ((8bit-arrays-p arr result)
	   (internal8-circular-shift arr result (x-dim arr) (y-dim arr) xsh ysh xgcf ygcf))
	  (t (error "Circular-shift implemented only for single-float, 8bit, or bit arrays"))))
  result)

(defmethod circular-shift ((vec vector) &key 
			   (x-shift 0) (x x-shift)
			   ((:-> result) (similar vec))
			   &allow-other-keys)
  (let* ((xsh (mod (round x) (x-dim vec)))
	 (xgcf (gcd xsh (x-dim vec)))
	 (ysh 0)
	 (ygcf 1))
    (cond ((float-arrays-p vec result)
	   (internal-circular-shift vec result (x-dim vec) 1 xsh ysh xgcf ygcf))
	  ((1bit-arrays-p vec result)
	   (internal1-circular-shift vec result (x-dim vec) 1 xsh ysh xgcf ygcf))
	  (t (error "Circular-shift implemented only for single-float or fixnum vectors"))))
  result)

;; *** should be C code 
(defmethod internal1-circular-shift ((arr array) res x-dim y-dim xsh ysh xgcf ygcf)
  (declare (ignore xgcf ygcf))
  (declare (type (array bit (* *)) arr res))
  (dotimes (j y-dim)
    (declare (fixnum j))
    (dotimes (i x-dim)
      (declare (fixnum i))
      (setf (aref res j i)
	    (aref arr (mod (- j ysh) y-dim) (mod (- i xsh) x-dim)))))
  0)

;; *** should be C code 
(defmethod internal1-circular-shift ((vec vector) res x-dim y-dim xsh ysh xgcf ygcf)
  (declare (ignore xgcf ygcf y-dim ysh))
  (declare (type (array bit (*)) vec res))
  (dotimes (i x-dim)
    (declare (fixnum i))
    (setf (aref res i)
	  (aref vec (mod (- i xsh) x-dim))))
  0)

#|
(display (setf foo (make-random-dots '(5 5))) t :zoom 20)
(display (setf fee (circular-shift foo :x 1)) t :zoom 20)
(display (setf fee (circular-shift foo :y 1)) t :zoom 20)
(display (setf fee (circular-shift foo :x 20)) t :zoom 20)

(setf foo (make-array 5 :element-type 'bit :initial-contents '(1 0 1 1 1)))
(setf fee (circular-shift foo :x 1))
(setf fee (circular-shift foo :x 2))
(setf fee (circular-shift foo :x 3))
(setf fee (circular-shift foo :x 8))

|#

(defmethod paste ((arr array) (base-arr array)
                  &key (y-offset 0) (x-offset 0)
		  (y y-offset) (x x-offset)
                  ((:-> result) (similar base-arr)))
  (setq y (round y) x (round x))
  (unless (= (rank arr) 2)
    (error "Paste implemented only for vectors or two-dimensional arrays."))
  (unless (eq result base-arr) (copy base-arr :-> result))

  (let ((res-y-size (y-dim result))
	(res-x-size (x-dim result))
	(y-size (y-dim arr))
	(x-size (x-dim arr)))

    ;; Check that the offset is not totally out of bounds
    (unless (and (< -1 x res-x-size) (< -1 y res-y-size))
      (error "Cannot paste ~a into ~a with offset (~a,~a)" arr result y x))

    ;; If it overhangs, paste a smaller region into result
    (when (or (> (+ y-size y) res-y-size) (> (+ x-size x) res-x-size))
      (warn "PASTE: cropping overhanging region")
      (setq y-size (min y-size (- res-y-size y)))
      (setq x-size (min x-size (- res-x-size x))))
    
    (cond ((float-arrays-p arr result)
           (internal-paste arr (x-dim arr) 0 0 y-size x-size result res-x-size y x))
          ((1bit-arrays-p arr result)
           (internal1-paste arr result 0 0 y-size x-size y x))
          (t (let ((elt-type (array-element-type result)))
	       (dotimes (y-pos y-size)
		 (declare (fixnum y-pos))
		 (dotimes (x-pos x-size)
		   (declare (fixnum x-pos))
		   (setf (aref result (+ y-pos y) (+ x-pos x))
			 (coerce (aref arr y-pos x-pos) elt-type))))))))
  result)

(defmethod paste ((vec vector) (base-arr array)
		  &key (y-offset 0) (x-offset 0)
		  (y y-offset) (x x-offset)
		  ((:-> result) (similar base-arr)))
  (setq y (round y) x (round x))
  (unless (eq result base-arr) (copy base-arr :-> result))

  (let ((res-y-size (y-dim result))
	(res-x-size (x-dim result))
	(y-size 1)
	(x-size (x-dim vec)))

    ;; Check that the offset is not totally out of bounds
    (unless (and (< -1 x res-x-size) (< -1 y res-y-size))
      (error "Cannot paste ~a into ~a with offset (~a,~a)" vec result y x))

    ;; If it overhangs, paste a smaller region into result
    (when (or (> (+ y-size y) res-y-size) (> (+ x-size x) res-x-size))
      (warn "PASTE: cropping overhanging region")
      (setq y-size (min y-size (- res-y-size y)))
      (setq x-size (min x-size (- res-x-size x))))
    
    (cond ((float-arrays-p vec result)
	   (internal-paste vec (x-dim vec) 0 0 y-size x-size
			   result res-x-size y x))
	  ((1bit-arrays-p vec result)
	   (internal1-paste vec result 0 0 y-size x-size y x))
	  (t (with-displaced-vectors ((displaced-result result))
	       (loop for i1 from 0 below (x-dim vec)
		     for i2 from (+ (* y res-x-size) x)
		     with elt-type = (array-element-type result) do
		     (setf (aref displaced-result i2) (coerce (aref vec i1) elt-type)))))))
  result)


(defmethod paste ((vec vector) (base-arr vector)
		  &key (x-offset 0) (x x-offset)
		  y-offset (y y-offset)
		  ((:-> result) (similar base-arr)))
  (check-size base-arr result)
  (setq x (round x))
  ;;(if y (error "No y offset permitted for pasting vectors"))
      
  (unless (eq result base-arr) (copy base-arr :-> result))

  (let ((res-x-size (x-dim result))
	(res-y-size 1)
	(y 0)
	(y-size 1)
	(x-size (x-dim vec)))

    ;; Check that the offset is not totally out of bounds
    (unless (and (< -1 x res-x-size) (< -1 y res-y-size))
      (error "Cannot paste ~a into ~a with offset (~a,~a)" vec result y x))

    ;; If it overhangs, paste a smaller region into result
    (when (or (> (+ y-size y) res-y-size) (> (+ x-size x) res-x-size))
      (warn "PASTE: cropping overhanging region")
      (setq y-size (min y-size (- res-y-size y)))
      (setq x-size (min x-size (- res-x-size x))))
    
    (cond ((float-arrays-p vec result)
	   (internal-paste vec (x-dim vec) 0 0 y-size x-size
			   result res-x-size y x))
	  ((1bit-arrays-p vec result)
	   (internal1-paste vec result 0 0 y-size x-size y x))
	  (t (with-displaced-vectors ((displaced-result result))
	       (loop for i1 from 0 below (x-dim vec)
		     for i2 from (+ (* y res-x-size) x)
		     with elt-type = (array-element-type result) do
		     (setf (aref displaced-result i2) (coerce (aref vec i1) elt-type)))))))
  result)

#|
(setf elt-type 'single-float)

(setf foo (make-matrix '((1 2 3) (4 5 6) (7 8 9))))
(setf fee (make-matrix '((0 0) (0 0))))

(paste fee foo :y 0)
(paste fee foo :y 1)
(paste fee foo :y 2)
(paste fee foo :y 3)
(paste fee foo :y -1)

(paste fee foo :x 0)
(paste fee foo :x 1)
(paste fee foo :x 2)
(paste fee foo :x 3)
(paste fee foo :x -1)

(setf fee (make-matrix 0 0))

(paste fee foo :y 0)
(paste fee foo :y 1)
(paste fee foo :y 2)
(paste fee foo :y 3)
(paste fee foo :y -1)

(paste fee foo :x 0)
(paste fee foo :x 1)
(paste fee foo :x 2)
(paste fee foo :x 3)
(paste fee foo :x -1)

|#


#|

(defmethod paste ((arr array) (base-arr array)
                  &key (y-offset 0) (x-offset 0)
		  (y y-offset) (x x-offset)
                  ((:-> result) (similar base-arr)))
  (unless (= (rank arr) 2)
    (error "Paste implemented only for vectors or two-dimensional arrays."))
  (unless (eq result base-arr) (copy base-arr :-> result))
  (setq y (round y) x (round x))
  (let ((y-size (min (y-dim arr) (- (y-dim result) y)))
        (x-size (min (x-dim arr) (- (x-dim result) x))))
    (cond ((float-arrays-p arr result)
           (internal-paste arr (x-dim arr) 0 0 y-size x-size
                           result (x-dim result) y x))
          ((1bit-arrays-p arr result)
           (internal1-paste arr result 0 0 y-size x-size y x))
          (t (loop for y-pos from 0 below y-size do
                   (loop for x-pos from 0 below x-size
                         with elt-type = (array-element-type result) do
                         (setf (aref result (+ y-pos y) (+ x-pos x))
                               (coerce (aref arr y-pos x-pos) elt-type)))))))
  result)

(defmethod paste ((vec vector) (base-arr array)
		  &key (y-offset 0) (x-offset 0)
		  (y y-offset) (x x-offset)
		  ((:-> result) (similar base-arr)))
  (unless (eq result base-arr) (copy base-arr :-> result))
  (setq y (round y)  x (round x))
  (let ((y-size 1)
	(x-size (min (x-dim vec) (- (x-dim result) x))))
    (cond ((float-arrays-p vec result)
	   (internal-paste vec (x-dim vec) 0 0 y-size x-size
			   result (x-dim result) y x))
	  ((1bit-arrays-p vec result)
	   (internal1-paste vec result 0 0 y-size x-size y x))
	  (t (with-displaced-vectors ((displaced-result result))
	       (loop for i1 from 0 below (x-dim vec)
		     for i2 from (+ (* y (x-dim result)) x)
		     with elt-type = (array-element-type result) do
		     (setf (aref displaced-result i2) (coerce (aref vec i1) elt-type)))))))
  result)
|#


;;; pastes a 1bit array into a 1bit array
(defmethod internal1-paste ((arr array) (result array)
			    from-y from-x
			    to-y to-x
			    y-offset x-offset)
  (declare (type (array bit (* *)) arr result))
  (if (and (equal (dimensions arr) (dimensions result)) (= y-offset 0) (= x-offset 0))
      (copy arr :-> result)
      (loop for j1 from from-y below to-y
	    for j2 from y-offset do
	    (loop for i1 from from-x below to-x
		  for i2 from x-offset do
		  (setf (aref result j2 i2) (aref arr j1 i1)))))
  result)

(defmethod internal1-paste ((vec vector) (result vector)
			    from-y from-x
			    to-y to-x
			    y-offset x-offset)
  (declare (type (array bit (* *)) vec result))
  (declare (ignore from-y from-x to-y to-x))
  (if (and (equal (dimensions vec) (dimensions result)) (= y-offset 0) (= x-offset 0))
      (copy vec :-> result)
      (with-displaced-vectors ((displaced-result result))
	       (loop for i1 from 0 below (x-dim vec)
		     for i2 from (+ (* y-offset (x-dim result)) x-offset) do
		     (setf (aref displaced-result i2) (aref vec i1)))))
  result)

(defmethod crop ((arr array)
		 &key (y 0) (x 0)
		 (y-size (- (y-dim arr) y)) (x-size (- (x-dim arr) x))
		 (y-dim y-size) (x-dim x-size)
		 ((:-> result)))
  (unless (= (rank arr) 2)
    (error "Crop implemented only for vectors or two-dimensional arrays."))
  (setq y (max (round y) 0)
	x (max (round x) 0)
	x-dim (min (round x-dim) (- (x-dim arr) x)) ;clip to image boundary
	y-dim (min (round y-dim) (- (y-dim arr) y)))
  (unless result
    (setq result
	  (if (allocated-array-p arr)
	      (allocate-array `(,y-dim ,x-dim) :element-type (array-element-type arr))
	      (make-array `(,y-dim ,x-dim) :element-type (array-element-type arr)))))
  (cond ((float-arrays-p arr result)
	 (internal-paste arr (x-dim arr) y x (+ y y-dim) (+ x x-dim)
			 result (x-dim result) 0 0))
	((1bit-arrays-p arr result)
	 (internal1-paste arr result y x (+ y y-dim) (+ x x-dim) 0 0))
	(t (error "Crop implemented only for single-float, bit, or fixnum arrays")))
  result)

(defmethod crop ((vec vector)
		 &key (y 0) (x 0)
		 (y-size (- (y-dim vec) y)) (x-size (- (x-dim vec) x))
		 (y-dim y-size) (x-dim x-size)
		 ((:-> result)))
  (unless result
    (setq result
	  (if (allocated-array-p vec)
	      (allocate-array x-dim :element-type (array-element-type vec))
	      (make-array x-dim :element-type (array-element-type vec)))))
  (setq y 0 y-dim 1
	x (max (round x) 0)
	x-dim (min (round x-dim) (- (x-dim vec) x))) ;clip to image boundary
  (cond ((float-arrays-p vec result)
	 (internal-paste vec (x-dim vec) y x (+ y y-dim) (+ x x-dim)
			 result (x-dim result) 0 0))
	((1bit-arrays-p vec result)
	 (internal1-paste vec result y x (+ y y-dim) (+ x x-dim) 0 0))
	(t (error "Crop implemented only for single-float, bit, or fixnum arrays")))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun array-cross-product
    (arr1 arr2
     &key ((:-> res) (make-array (append (dimensions arr1) (dimensions arr2))
				 :element-type (array-element-type arr1))))
  "Computes a generalized cross-product of arrays:
a[i,j,...,l,m,...] = b[i,j,...] * c[l,m,...]"
  (loop with v1 = (vectorize arr1)
	with v2 = (vectorize arr2)
	with vres = (make-array (list (total-size v1) (total-size v2))
				:displaced-to res
				:element-type (array-element-type res))
	for i1 from 0 below (total-size v1)
	do
	(loop for i2 from 0 below (total-size v2)
	      do
	      (setf (aref vres i1 i2) (* (aref v1 i1) (aref v2 i2)))))
  res)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read and write matrices in tab-delimited ascii.  Write vectors as a
;; single row, read single row as a vector.  Control the number of
;; decimal places for writing by keyword. Reading expects file to be
;; tab-delimited, and does not check that row lengths are consistent

(defun write-ascii-matrix (arr path &key (comment nil) (digits 6))
  (declare (type (array single-float) arr))
  (let* ((rows (row-dim arr))
	 (cols (col-dim arr))
	 (cols-1 (- cols 1))
	 (format-string (string-cat "~," (format nil "~D" digits) "f~A")))
    (with-open-file (file path :direction :output :if-does-not-exist :create
			  :if-exists :rename-and-delete)
      (when comment (format file ";;; ~S~%" comment))
      (if (= rows 1)
	  (let ((row (vectorize arr)))
	    (dotimes (col cols)
	      (format file format-string (aref row col)
		      (if (= col cols-1) #\Newline #\Tab))))
	  (dotimes (row rows)
	    (declare (fixnum row))
	    (dotimes (col cols)
	      (declare (fixnum col))
	      (format file format-string (aref arr row col)
		      (if (= col cols-1) #\Newline #\Tab)))))
      arr)))

(defun read-ascii-matrix (path)
  (with-open-file (stream path :direction :input)
    (let ((rows 0)
	  (cols 1)
	  result)
      ;; Count the rows
      (loop for char = (read-char stream nil nil)
	    for eol = (equal char #\Newline)
	    until (null char)
	    do (when eol (incf rows)))
      (file-position stream 0)
      (let ((line (read-line stream))
	    (start 0)
	    (item nil))
	(setq cols (loop do (multiple-value-setq (item start)
			      (read-from-string line nil nil :start start))
			 while item
			 count item)))
      (file-position stream 0)
      ;; Read the data
      (cond ((= rows 1)
	     (setq result (make-array cols :element-type 'single-float))
	     (dotimes (col cols)
	       (setf (aref result col) (float (read stream)))))
	    (t
	     (setq result (make-array (list rows cols) :element-type 'single-float))
	     (dotimes (row rows)
	       (dotimes (col cols)
		 (setf (aref result row col) (float (read stream)))))))
      result)))

#|
(setq foo (make-matrix '((1 2 3) (4 5 6) (7 8 9))))
(write-ascii-matrix foo "/tmp/foo")
(read-ascii-matrix "/tmp/foo")

(write-ascii-matrix foo "/tmp/foo" :digits 2)
(read-ascii-matrix "/tmp/foo")

(setq foo (make-matrix '(1 2 3 4 5 6)))
(write-ascii-matrix foo"/tmp/foo")
(read-ascii-matrix "/tmp/foo")
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
