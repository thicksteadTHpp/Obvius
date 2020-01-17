;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code for Gamma correction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :obvius)

(export '(gamma-lut rgb-gamma-lut make-gamma-lut make-rgb-gamma-lut
	  check-in-gamut in-gamut at-gamut gamma-lut-correct
	  maximum-excursion maximum-modulation))

#|
(user::my-compile-load "obvius/psychophysics/gamma")
|#

;;; ToDo:
;; Function for reading ascii file
;; Write some comments about the decisions process that got us here. -EJ

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Gamma Correction.  By gamma correction, we mean converting 8bit
;;; values <-> float values via a nonlinear lookup table.

;;; Gamma-lut class contains 1 vector, a lookup table with
;;; corresponding origin and increment. In this respect they are
;;; analogous to discrete functions.  These are built from
;;; measurements, which are two arrays of corresponding linear and
;;; 8bit values.  The increment for the 8bit table is always 1. The
;;; origin (and maximum) is that of the measurements used to construct
;;; the LUT.  Rgb-lut is made of three gamma-luts, one for each color
;;; (RGB) band.  For a monitor, typically, the 8bit->float values are
;;; measured using a calibration device.  For a camera/scanner,
;;; typically, the float->8bit values would be measured.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Gamma-lut classes and functions for making gamma-luts from data
;;; arrays.
(def-simple-class gamma-lut ()
  (data origin increment in-type out-type))

(defmethod print-object ((lut gamma-lut) stream)
  (format stream "#<~A ~A->~A>" (object-class-name lut)
	  (when (slot-boundp lut 'in-type) (in-type lut))
	  (when (slot-boundp lut 'out-type) (out-type lut))))

(def-simple-class rgb-gamma-lut ()
  (r-lut g-lut b-lut))

;;; *** might be too slow, maybe make slots in rgb-gamma-lut and/or write
;;; initialize instance to check consistency
(defmethod in-type ((lut rgb-gamma-lut))
  (with-slots (r-lut g-lut b-lut) lut
    (with-slots (in-type) r-lut
    (if (and (equal in-type (in-type g-lut))
	     (equal in-type (in-type b-lut)))
	in-type
	(error "Luts in ~a have different in-type" lut)))))

(defmethod out-type ((lut rgb-gamma-lut))
  (with-slots (r-lut g-lut b-lut) lut
    (with-slots (out-type) r-lut
    (if (and (equal out-type (out-type g-lut))
	     (equal out-type (out-type b-lut)))
	out-type
	(error "Luts in ~a have different out-type" lut)))))

(defun make-rgb-gamma-lut (&key r-in g-in b-in r-out g-out b-out size)
  (unless (and r-in g-in b-in r-out g-out b-out)
    (error "Must pass calibration data"))
  (make-instance 'rgb-gamma-lut
		 :r-lut (make-gamma-lut r-in r-out :size size)
		 :g-lut (make-gamma-lut g-in g-out :size size)
		 :b-lut (make-gamma-lut b-in b-out :size size)))

;;; Take x-vals and y-vals in correspondence and construct a lookup
;;; table.  One is a vector of floats, the other a vector of 8bits.
;;; X-vals and y-vals are assumed to be monotone. The result lut is a
;;; vector with corresponding origin and increment. The contents of
;;; the vector are y-values that correspond to evenly sampled
;;; x-values: origin, origin+increment, origin+2*increment, etc. If
;;; the x-values are 8bit (i.e., when building 8-f lut), the increment
;;; is 1.  The lut vector is constructed using linear interpolation,
;;; and rounding in the case of 8.
(defun make-gamma-lut (x-vals y-vals &key  (tolerance 1e-6) size)
  (let* ((float  'single-float)
	 (8bit '(unsigned-byte 8))
	 (in-type (array-element-type x-vals))
	 (out-type (array-element-type y-vals))
	 (float->8bit-p (cond ((and (equal out-type 8bit)
				    (equal in-type float))
			       t)
			      ((and (equal in-type 8bit)
				    (equal out-type float))
			       nil)
			      (t (error "Arrays must be of types ~a and ~a" 8bit float))))
	 (origin (minimum x-vals))
	 (maximum (maximum x-vals))
	 (range (- maximum origin))
	 (size (if float->8bit-p (or size 1024) (+ 1 range)))
	 (increment (/ range (- size 1)))
	 (data (make-array size :element-type (array-element-type y-vals)))
	 (tol (* tolerance maximum))
	 (test #'(lambda(x1 x2) (or (<= x1 x2) (almost-equal x1 x2 :tolerance tol))))
	 y-val slope above-pos below-pos
	 above-x-val above-y-val below-x-val below-y-val)
    (verify-monotone-increasing x-vals)
    (verify-monotone-increasing y-vals)
    (loop for i from 0 below (length data)
	  for x from origin by increment
	  do
	  (setq above-pos (position x x-vals :test test))
	  (unless above-pos
	    (error "No value above ~a in array ~a" x x-vals))
	  (setq below-pos (max 0 (- above-pos 1)))
	  (setq above-x-val (aref x-vals above-pos)
		above-y-val (aref y-vals above-pos)
		below-x-val (aref x-vals below-pos)
		below-y-val (aref y-vals below-pos))
	  (setq slope (/-0 (- above-y-val below-y-val) (- above-x-val below-x-val)))
	  (setq y-val (+ below-y-val (* slope (- x below-x-val))))
	  (setf (aref data i) 
		(if float->8bit-p (floor (+ 0.5 y-val)) y-val )))
    (make-instance 'gamma-lut :data data :origin origin :increment increment
		   :in-type in-type
		   :out-type out-type)))

(defmacro internal-evaluate-gamma-lut (val data origin increment)
  `(aref ,data (floor (+ 0.5 (/ (- ,val ,origin) ,increment)))))

(defmethod evaluate ((lut gamma-lut) val &key &allow-other-keys)
  (with-slots (data origin increment) lut
    (internal-evaluate-gamma-lut val data origin increment)))

(defmethod domain ((lut gamma-lut))
  (with-slots (data origin increment) lut
    (let* ((min origin)
	   (domain (* (1- (length data)) increment))
	   (max (+ min domain)))
      (values domain min max))))

(defmethod range ((lut gamma-lut))
  (range (data lut)))

(defun check-in-gamut (thing lut &key (tolerance *tolerance*))
  (unless (in-gamut thing lut :tolerance tolerance)
    (cerror "Continue with the value(s) given"
	    (format nil "Value(s) ~a out of gamut or wrong type or size for ~a" thing lut))
    thing))

#|
;;; test make-lut

(setq 8bit-list (loop for i from 3 below 45 collect (* i 4)))
(setq float-list (loop for i from 3 below 45 collect (float (sqr (/ i 51)))))
(setq 8bit (make-array (length 8bit-list) :element-type '(unsigned-byte 8)
		       :initial-contents 8bit-list))
(setq float (make-array (length float-list) :element-type 'single-float
		       :initial-contents float-list))

(setq 8bit->float (make-gamma-lut 8bit float))
(describe 8bit->float)
(with-slots (data origin increment) 8bit->float
  (make-discrete-function data origin (+ origin (* (- (length data) 1) increment))))

;;; check 8bit->float against data
(dotimes (i (length (data 8bit)))
  (unless (zerop (- (evaluate 8bit->float (elt 8bit i)) (elt float i)))
    (warn "Correspondence check failed - core dumped")))
(let ((i 20)
      (j 21))
  (- (/ (+ (elt float i) (elt float j)) 2)
     (evaluate 8bit->float (/ (+ (elt 8bit i) (elt 8bit j)) 2))))


(setq float->8bit (make-gamma-lut float 8bit))
(describe float->8bit)
(with-slots (data origin increment) float->8bit
  (make-discrete-function data origin (+ origin (* (- (length data) 1) increment))))
;;; check float->8bit against data
(dotimes (i (length (data float)))
  (unless (zerop (- (evaluate float->8bit (elt float i)) (elt 8bit i)))
    (warn "Correspondence check failed - core dumped")))
(let ((i 20)
      (j 21)
      (fraction 0.5))
  (list (floor (+ 0.5 (+ (elt 8bit i) (* fraction (- (elt 8bit j) (elt 8bit i))))))
	(evaluate float->8bit (+ (elt float i) (* fraction (- (elt float j) (elt float i)))))))
  
;;; check float->8bit against 8bit->float
(loop for i from (minimum 8bit) below (maximum 8bit) do
      (unless (zerop (- i (evaluate float->8bit (evaluate 8bit->float i))))
	(error "Inversion failed ax00465")))

(loop for i from (minimum float) below (maximum float) by (/ (range float) 1000) do
      (unless (almost-equal i (evaluate 8bit->float (evaluate float->8bit i))
			    :tolerance 0.005)
	(error "Inversion failed: ~a ~a" i (evaluate 8bit->float (evaluate float->8bit i)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods for gamma correcting without a lut.
;;; If lut is not a lut, then check against [0,1]

(defmethod in-gamut ((val number) (lut t) &key (tolerance *tolerance*))
  (when (<= (- (abs tolerance)) val (+ 1 (abs tolerance)))
    val))

(defmethod at-gamut ((val number) (lut t) &key (tolerance *tolerance*))
  (when (or (almost-equal 0 val :tolerance tolerance)
	    (almost-equal 1 val :tolerance tolerance))
    val))

(defmethod in-gamut ((arr array) (lut t) &key (tolerance *tolerance*))
  (when (<= (- (abs tolerance)) (minimum arr) (maximum arr) (+ 1 (abs tolerance)))
    arr))

(defmethod at-gamut ((arr array) (lut t) &key (tolerance *tolerance*))
  (when (or (almost-equal 0 (minimum arr) :tolerance tolerance)
	    (almost-equal 1 (maximum arr) :tolerance tolerance))
    arr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods for gamma correcting one monochrome value

(defmethod in-gamut ((val number) (lut gamma-lut) &key (tolerance *tolerance*))
  (when (typep val (in-type lut))
    (multiple-value-bind (domain min max) (domain lut)
      (declare (ignore domain))
      (when (<= (- min (abs tolerance)) val (+ max (abs tolerance)))
	val))))

(defmethod at-gamut ((val number) (lut gamma-lut) &key (tolerance *tolerance*))
  (when (typep val (in-type lut))
    (multiple-value-bind (domain min max) (domain lut)
      (declare (ignore domain))
      (when (or (almost-equal min val :tolerance tolerance)
		(almost-equal max val :tolerance tolerance))
	val))))

(defmethod gamma-lut-correct ((val number) (lut gamma-lut)
			      &key (check-in-gamut t) ->)
  (declare (ignore ->))
  (when check-in-gamut
    (check-in-gamut val lut))
  (evaluate lut val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods for gamma correcting multiple monochrome values with a single lut.
;;; arr is multiple monochrome values

(defmethod in-gamut ((arr array) (lut gamma-lut) &key (tolerance *tolerance*))
  (when (equal (array-element-type arr) (in-type lut))
    (multiple-value-bind (lut-domain lut-min lut-max) (domain lut)
      (declare (ignore lut-domain))
      (multiple-value-bind (arr-range arr-min arr-max) (range arr)
	(declare (ignore arr-range))
	(when (<= (- lut-min (abs tolerance)) arr-min arr-max (+ lut-max (abs tolerance)))
	  arr)))))

(defmethod at-gamut ((arr array) (lut gamma-lut) &key (tolerance *tolerance*))
  (when (equal (array-element-type arr) (in-type lut))
    (multiple-value-bind (lut-domain lut-min lut-max) (domain lut)
      (declare (ignore lut-domain))
      (multiple-value-bind (arr-range arr-min arr-max) (range arr)
	(declare (ignore arr-range))
	(when (or (almost-equal lut-min arr-min :tolerance tolerance)
		  (almost-equal lut-max arr-max :tolerance tolerance))
	  arr)))))

(defmethod gamma-lut-correct ((arr array) (lut gamma-lut)
			      &key (check-in-gamut t)
			      ((:-> result)))
  (when check-in-gamut
    (check-in-gamut arr lut))
  (unless result
    (setq result (similar arr :element-type (out-type lut))))
  (let ((d-arr (vectorize arr))
	(d-result (vectorize result)))
    (with-slots (data origin increment) lut
      (dotimes (i (length d-arr))
	(declare (fixnum i))
	(setf (aref d-result i) (internal-evaluate-gamma-lut
				 (aref d-arr i) data origin increment)))))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods for gamma correcting a single rgb triplet.
;;; vec is an rgb triplet

(defmethod in-gamut ((vec vector) (lut rgb-gamma-lut) &key (tolerance *tolerance*))
  (when (and (= (length vec) 3)
	     (equal (array-element-type vec) (in-type lut)))
    (with-slots (r-lut g-lut b-lut) lut
      (when (and (in-gamut (elt vec 0) r-lut :tolerance tolerance)
		 (in-gamut (elt vec 1) g-lut :tolerance tolerance)
		 (in-gamut (elt vec 2) b-lut :tolerance tolerance))
	vec))))

(defmethod at-gamut ((vec vector) (lut rgb-gamma-lut) &key (tolerance *tolerance*))
  (when (and (= (length vec) 3)
	     (equal (array-element-type vec) (in-type lut)))
    (with-slots (r-lut g-lut b-lut) lut
      (when (or (at-gamut (elt vec 0) r-lut :tolerance tolerance)
		(at-gamut (elt vec 1) g-lut :tolerance tolerance)
		(at-gamut (elt vec 2) b-lut :tolerance tolerance))
	vec))))

(defmethod gamma-lut-correct ((vec vector) (lut rgb-gamma-lut)
			      &key (check-in-gamut t)
			      ((:-> result)))
  (when check-in-gamut
    (check-in-gamut vec lut))
  (unless result
    (setq result (similar vec :element-type (out-type lut))))
  (with-slots (r-lut g-lut b-lut) lut
    (setf (aref result 0) (evaluate r-lut (aref vec 0)))
    (setf (aref result 1) (evaluate g-lut (aref vec 1)))
    (setf (aref result 2) (evaluate b-lut (aref vec 2))))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods for gamma correcting a bunch of rgb triplets.
;;; arr must be Nx3, each row is an rgb triplet

(defmethod in-gamut ((arr array) (lut rgb-gamma-lut) &key (tolerance *tolerance*))
  (when (and (= (col-dim arr) 3)
	     (equal (array-element-type arr) (in-type lut)))
    (with-slots (r-lut g-lut b-lut) lut
      (when (and (in-gamut (minimum-rows arr) lut :tolerance tolerance)
		 (in-gamut (maximum-rows arr) lut :tolerance tolerance))
	arr))))

(defmethod at-gamut ((arr array) (lut rgb-gamma-lut) &key (tolerance *tolerance*))
  (when (and (= (col-dim arr) 3)
	     (equal (array-element-type arr) (in-type lut)))
    (with-slots (r-lut g-lut b-lut) lut
      (when (or (at-gamut (minimum-rows arr) lut :tolerance tolerance)
		(at-gamut (maximum-rows arr) lut :tolerance tolerance))
	arr))))

(defmethod gamma-lut-correct ((arr array) (lut rgb-gamma-lut)
			      &key (check-in-gamut t)
			      ((:-> result)))
  (when check-in-gamut
    (check-in-gamut arr lut))
  (unless result
    (setq result (similar arr :element-type (out-type lut))))
  (loop for i from 0 below (col-dim arr)
	for lut in (list (r-lut lut) (g-lut lut) (b-lut lut))
	do
	(with-slots (data origin increment) lut
	  (loop for j from 0 below (row-dim arr) do
		(setf (aref result j i) (internal-evaluate-gamma-lut
					 (aref arr j i) data origin increment)))))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods for gamma correcting images and color images.

#|
(defmethod gamma-lut-correct ((im image) (lut gamma-lut)))
(defmethod gamma-lut-correct ((im color-image) (lut rgb-gamma-lut)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;; Some test code for in-gamut and gamma-lut-correct

(obv-require :conversion)
(progn
  (setq lut-data (matrix-transpose (read-ascii-matrix
				    "/usr/src/obvius/tutorials/psychophysics/data/gamma-lut-data")))
  (setq rfloats (displaced-row 0 lut-data))
  (setq gfloats (displaced-row 1 lut-data))
  (setq bfloats (displaced-row 2 lut-data))
  (setq 8bits (make-array 256 :element-type '(unsigned-byte 8)
			  :initial-contents (loop for i from 0 below 256 collect i)))

  (setq rgb-lut (make-rgb-gamma-lut :r-out 8bits :g-out 8bits :b-out 8bits
				    :r-in rfloats :g-in gfloats :b-in bfloats)))
(describe rgb-lut)
(in-type rgb-lut)
(out-type rgb-lut)
(multiple-value-bind (domain min max)
    (domain (r-lut rgb-lut))
  (make-discrete-function (data (r-lut rgb-lut)) min max))

;;; test monochrome val
(in-gamut 0 (r-lut rgb-lut))		; should be nil
(in-gamut 0.5 (r-lut rgb-lut))		; should be t
(in-gamut 1.2 (r-lut rgb-lut))		; should be nil
(in-gamut -0.1 (r-lut rgb-lut))		; should be nil
(make-discrete-function #'(lambda (x) (gamma-lut-correct x (r-lut rgb-lut)))
			0.0 1.0)

;;; test arr of monochrome vals
;; should be t
(in-gamut (randomize (make-array 100 :element-type 'single-float 
				 :initial-element 0.5) 0.5)
	  (r-lut rgb-lut))
;; should be nil
(in-gamut (randomize (make-array 100 :element-type 'single-float 
				 :initial-element 25.0) 1e3)
	  (r-lut rgb-lut))
;; should look like gamma curve
(let* ((in (randomize (make-array 100 :element-type 'single-float 
				  :initial-element 0.5) 0.49))
       (out (coerce-to-float (gamma-lut-correct in (r-lut rgb-lut)))))
  (scatter-plot out in))

;;; test triplet
(in-gamut (make-matrix '(0.1 0.5 0.6)) rgb-lut)	; should be t
(in-gamut (make-matrix '(-0.1 0.5 0.6)) rgb-lut)	; should be nil
(in-gamut (make-matrix '(0.1 0.5)) rgb-lut)	; should be nil
(in-gamut #(0.1 0.1 0.1) rgb-lut)	; should be nil
(let ((in (randomize (make-matrix '(0.5 0.5 0.5)) 0.49)))
  (write (gamma-lut-correct in rgb-lut) :array t)
  (pprint (gamma-lut-correct (aref in 0) (r-lut rgb-lut)))
  (pprint (gamma-lut-correct (aref in 1) (g-lut rgb-lut)))
  (pprint (gamma-lut-correct (aref in 2) (b-lut rgb-lut))))

;;; test bunch-o-triplets
(setq in (randomize (make-array '(256 3) :element-type 'single-float
				:initial-element 0.5) 0.49))
(in-gamut in rgb-lut)	; should be t
(in-gamut (mul 5 in) rgb-lut)	; should be nil
(time (setq out (gamma-lut-correct in rgb-lut)))
(setq test-out (paste-rows (loop for row in (displaced-rows in)
				 collect (gamma-lut-correct row rgb-lut))))
(mean-square-error out test-out)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Provide minimum, maximum, and starting vectors, as well as a
;;; vector direction Return the maximum allowable excursion in that
;;; direction that keeps us inside the range defined by :minimum and
;;; :maximum.  The excursion returned is one-sided, i.e. going in the
;;; opposite direction is not attempted.
(defmethod maximum-excursion ((start vector) (direction vector) &key
			      ((:-> result) (similar start))
			      (minimum (fill! (similar direction) 0.0))
			      (maximum (fill! (similar direction) 1.0)))
  (when (zerop (vector-length start)) (error "Cannot modulate a zero signal"))
  (let ((scale (min (minimum (abs-value (div (sub maximum start)
					     (clip direction 0.0 most-positive-single-float)
					     :zero-val most-positive-single-float
					     :suppress-warning t)))
		    (minimum (abs-value (div (sub minimum start)
					     (clip direction most-negative-single-float 0.0)
					     :zero-val most-positive-single-float
					     :suppress-warning t))))))
    (mul direction scale :-> result)
    (values result scale)))


;;; Much like maximum-excursion, except allow for modulation both
;;; positive and negative, and return the smaller of the two
;;; modulations.
(defmethod maximum-modulation ((start vector) (direction vector) &key
			       ((:-> result) (similar start))
			       (minimum (fill! (similar direction) 0.0))
			       (maximum (fill! (similar direction) 1.0)))
  (multiple-value-bind (pos-res pos-scale)
      (maximum-excursion start direction
			 :minimum minimum :maximum maximum)
    (multiple-value-bind (neg-res neg-scale)
	(maximum-excursion start (negate direction)
			   :minimum minimum :maximum maximum)
      (if (> (vector-length pos-res) (vector-length neg-res))
	  (values (copy neg-res :-> result) neg-scale)
	  (values (copy pos-res :-> result) pos-scale)))))

#|
(setq start (make-matrix '(0.5 0.5 0.5)))
(setq start (make-matrix '(0.4 0.5 0.5)))
(setq direction (make-matrix '(10.0 0.0 0.0)))
(write (maximum-excursion start direction) :array t)
(maximum-excursion start direction)
(write (maximum-modulation start direction) :array t)
(maximum-modulation start direction)

;;; for scalars
(maximum-modulation (make-matrix '(0.5)) (make-matrix '(1.0)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
