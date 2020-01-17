;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: discrete-function.lisp
;;;  Author: Eero Simoncelli
;;;  Description: Discrete function viewables (includes histograms).
;;;  Creation Date:  July, 1988
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)
(export '(discrete-function-p
	  make-discrete-function fill-discrete-function
	  make-inverse-discrete-function
	  evaluate linear-interpolate
	  complex-discrete-function-p
	  histogram-p
	  make-histogram  binsize bin-size  image-of
	  periodic-discrete-function-p
	  make-periodic-discrete-function fill-periodic-discrete-function
	  periodic-linear-interpolate
	  log-discrete-function-p
	  make-log-discrete-function
	  zero! copy
	  mean variance skew kurtosis
	  integral energy
	  maximum minimum maximum-location minimum-location
	  domain range
	  mean-square-error mean-abs-error max-abs-error
	  point-minimum point-maximum
	  square-error abs-error
	  add mul sub div negate square abs-value
	  linear-x-form square-root power natural-logarithm
	  quantize clip
	  correlate
	  distribution-mean distribution-variance distribution-mean-and-variance 
	  point-operation entropy  
	  circular-shift paste crop
	  fft magnitude square-magnitude abs-value
	  complex-phase complex-conjugate))

(defmethod settable-parameters ((class-name (eql 'discrete-function)))
  (append '(interpolator endpoint :size) (call-next-method)))

(defmacro discrete-function-p (obj)
  `(typep ,obj 'discrete-function))

(defmethod total-size ((df discrete-function))
  (array-total-size (slot-value df 'data)))

(defmethod dimensions ((df discrete-function))
  (array-dimensions (slot-value df 'data)))

#|
(defmethod x-dim ((df discrete-function))
  (array-total-size (data df)))
|#

(defmethod static-arrays-of ((df discrete-function))
  (list (slot-value df 'data)))

(defmethod iref ((df discrete-function) &rest args)
  (apply 'aref (data df) args))

(defmethod size ((df discrete-function))
  (array-total-size (slot-value df 'data)))

(defmethod (setf size) ((df discrete-function) new-size)
  (let ((old-data (data df))
	(new-data (allocate-array new-size :element-type 'single-float)))
    (loop for i from 0 below new-size
	  for pos from (origin df) by (increment df)
	  do
	  (setf (aref new-data i)
		(evaluate df pos)))
    (setf (data df) new-data)
    (free-array old-data)))

;;; *** should try to use with-result here.
#|
(defun make-discrete-function (func min max &rest initargs
			       &key size interpolator endpoint name ->)
  (declare (ignore size interpolator name))
  (when -> (setf (getf initargs :name) ->))
  (when (not (functionp func))
    (error "Argument must be a function or symbol with function binding"))
  (when (symbolp func) (setq func (symbol-function func))) ;for history purposes
  (let ((df (apply 'make-instance 'discrete-function initargs)))
    (fill-discrete-function df func min max)
    df))
|#

(defmethod make-discrete-function ((func t) min max &rest initargs
					&key size interpolator endpoint name ->)
  (declare (ignore size interpolator endpoint name))
  (when -> (setf (getf initargs :name) ->))
  (unless (functionp func)
    (error "Argument must be a function or symbol with function binding"))
  (when (symbolp func) (setq func (symbol-function func))) ;for history purposes
  (let ((df (apply 'make-instance 'discrete-function initargs)))
    (fill-discrete-function df func min max)
    df))

(defmethod make-discrete-function ((arr vector) min max &rest initargs
				   &key (size (length arr)) interpolator endpoint name  ->)
  (declare (ignore endpoint interpolator name))
  (when -> (setf (getf initargs :name) ->))
  (setf (getf initargs :size) size)
  (let ((df (apply 'make-instance 'discrete-function :data arr initargs)))
    (setf (origin df) (float min))
    (setf (increment df)  (if (endpoint df)
			      (float (/-0 (abs (- max min)) (1- size)))
			      (float (/-0 (abs (- max min)) size))))
    df))



(defmethod initialize-instance ((df discrete-function)
				&rest initargs
				&key size data)
  (cond (data (when (and size (not (= size (length data))))
		(error "Size parameter ~D incompatible with data ~A" size data)))
	(size (setf (getf initargs :data) (allocate-array size :element-type 'single-float)))
	(t (error "Must pass either :data or :size when creating a 'discrete-function")))
  (remf initargs :size)
  (apply #'call-next-method df initargs))

(defmethod fill-discrete-function ((df discrete-function) func min max)
  (let* ((size (total-size df))
	 (data (data df))
	 (incr (if (endpoint df)
		   (float (/-0 (abs (- max min)) (1- size)))
		   (float (/-0 (abs (- max min)) size))))
	 (val (float (min min max))))
    (declare (fixnum size)
	     (single-float val incr)
	     (type (array single-float (*)) data))
    (setf (origin df)    val  
	  (increment df) incr)
    ;;(format t "~a ~a~%" (origin df) (increment df))
    (loop for i from 0 below size do
	  (setf (aref data i) (float (funcall func val)))
	  (incf val incr)))
  (set-history df 'fill-discrete-function func min max)
  (set-not-current df)
  df)

;;; Lookup with linear interpolation
(defmethod evaluate ((df discrete-function) val &key ->)
  (declare (ignore ->))
  (with-slots (origin increment data interpolator) df
    (funcall interpolator data origin increment val)))

(defun linear-interpolate (ar origin increment value)
  ;;(declare (type (array single-float (*)) ar))
  (let* ((pos (float (/-0 (- value origin) increment)))
	 (index (min (- (length ar) 2) 
		    (max 0 (floor pos)))))
    ;;(declare (single-float pos) (fixnum index))
    (+ (aref ar index) (* (- (aref ar (1+ index)) (aref ar index)) 
			 (- pos index)))))

;;; Point operations using discrete functions (lookup tables) with
;;; linear interpolation.  This method is also defined on images and
;;; regular functions in imops.lisp.  That method creates a lookup
;;; table and calls this one.  
;;;; **** NOTE: this ignores the interpolator slot!
(defmethod point-operation ((im image) (df discrete-function) &key -> &allow-other-keys)
  (with-slots (increment origin data) df
    (with-result ((result ->) im 'point-operation im df)
      (internal-pointop (data im) (data result) (total-size im)
			(data df) (total-size data) 
			(float origin) (float increment))
      result)))

(defmethod make-inverse-discrete-function ((df discrete-function)
					   &key -> (tolerance 1e-6))
  (with-result ((res ->) df)
    (let* ((df-data (data df))
	   (res-data (data res))
	   (df-min (minimum df))
	   (df-max (maximum df))
	   (df-origin (origin df))
	   (df-increment (increment df))
	   (size (total-size res-data))
	   (res-origin df-min)
	   (res-increment (if (endpoint res)
			      (float (/-0 (abs (- df-max df-min)) (1- size)))
			      (float (/-0 (abs (- df-max df-min)) size))))
	   (tol (* tolerance df-max))
	   (test #'(lambda(x1 x2) (or (<= x1 x2) (almost-equal x1 x2 :tolerance tol))))
	   above-pos below-pos above-x-val above-y-val below-x-val below-y-val slope)
      (setf (origin res) res-origin)
      (setf (increment res) res-increment)
      (verify-monotone-increasing df-data)
      (loop for i from 0 below (total-size res)
	    for x from res-origin by res-increment
	    do
	(setq above-pos (position x df-data :test test))
	(unless above-pos
	  (error "No value above ~a in ~a" x df))
	(setq below-pos (max 0 (- above-pos 1)))
	(setq above-y-val (+ df-origin (* above-pos df-increment))
	      below-y-val (+ df-origin (* below-pos df-increment))
	      above-x-val (aref df-data above-pos)
	      below-x-val (aref df-data below-pos))
	(setq slope (/-0 (- above-y-val below-y-val) (- above-x-val below-x-val)))
	(setf (aref res-data i) (+ below-y-val (* slope (- x below-x-val))))))
    res))

#|
(setq df (make-discrete-function #'log 1.0 100.0))
(setq inv-df (make-inverse-discrete-function df))
(let* ((x (random 100.0))
       (x-est (evaluate inv-df (evaluate df x))))
  (list x x-est))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set-result:

(defmethod set-result ((name t) (model discrete-function))
  (check-type name viewable-name)
  (make-instance (class-of model)
		 :size (size model)
		 :origin (origin model)
		 :increment (increment model)
		 :endpoint (endpoint model)
		 :interpolator (interpolator model)
		 :name name
		 :display-type (display-type model)))

;;; model-plist for discrete-function must include size
(defmethod set-result ((res discrete-function) (model-plist list))
  (cond ((not (typep res (getf model-plist :class)))
	 (error "Result ~a is incompatible with argument type ~a"
		res (getf model-plist :class)))
	((not (equal (getf model-plist :size) (size res)))
	 (error "Result size ~a and argument size ~a are incompatible"
		(size res) (getf model-plist :size)))
	((and (getf model-plist :increment)
	      (not (equal (getf model-plist :increment) (increment res)))
	      (error "Result increment ~a and argument increment ~a are incompatible"
		     (increment res) (getf model-plist :increment))))
	((and (getf model-plist :endpoint)
	      (not (equal (getf model-plist :endpoint) (endpoint res)))
	      (error "Result endpoint ~a and argument endpoint ~a are incompatible"
		     (endpoint res) (getf model-plist :endpoint))))
	(t res)))

(defmethod check-size ((df discrete-function) &rest df-list)
  (cond ((null df-list) df)
	((not (equal (size df) (size (car df-list))))
	 (error "~As have different size." (object-class-name df)))
	((not (equal (increment df) (increment (car df-list))))
	 (error "~As have different increment." (object-class-name df)))
	(t (when (not (equal (origin df) (origin (car df-list))))
	     (warn "~As have different origin." (object-class-name df)))
	   (apply 'check-size df-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-values ((df discrete-function) &key
			 y y-size
			 (x (* (car *x-print-range*) (size df)))
			 (x-size (- (* (cadr *x-print-range*) (size df)) x)))
  (print-values (data df) :y y :x x :y-size y-size :x-size x-size))

(defmethod copy ((df discrete-function) &key ->)
  (with-result ((result ->) df 'copy df)
    (copy (data df) :-> (data result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Arithmetic Operations:

(defmethod add ((df1 discrete-function) (df2 discrete-function) &key ->)
  (with-result ((result ->) (check-size df1 df2) 'add df1 df2)
    (add (data df1) (data df2) :-> (data result))
    result))

(defmethod add ((df discrete-function) (val number) &key ->)
  (with-result ((result ->) df 'add df val)
    (add (data df) val :-> (data result))
    result))

(defmethod add ((val number) (df discrete-function) &key ->)
  (with-result ((result ->) df 'add val df)
    (add (data df) val :-> (data result))
    result))

(defmethod sub ((df1 discrete-function) (df2 discrete-function) &key ->)
  (with-result ((result ->) (check-size df1 df2) 'sub df1 df2)
    (sub (data df1) (data df2) :-> (data result))
    result))

(defmethod sub ((val number) (df discrete-function) &key ->)
  (with-result ((result ->) df 'sub val df)
    (sub val (data df) :-> (data result))
    result))

(defmethod sub ((df discrete-function) (val number) &key ->)
  (with-result ((result ->) df 'sub df val)
    (add (data df) (- val) :-> (data result))
    result))

(defmethod mul ((df1 discrete-function) (df2 discrete-function) &key ->)
  (with-result ((result ->) (check-size df1 df2) 'mul df1 df2)
    (mul (data df1) (data df2) :-> (data result))
    result))

(defmethod mul ((df discrete-function) (val number) &key ->)
  (with-result ((result ->) df 'mul df val) 
    (mul (data df) val :-> (data result))
    result))

(defmethod mul ((val number) (df discrete-function) &key ->)
  (with-result ((result ->) df 'mul val df)
    (mul (data df) val :-> (data result))
    result))

(defmethod div ((df1 discrete-function) (df2  discrete-function) 
		&key (zero-val *div-by-zero-result*) suppress-warning ->) 
  (with-result ((result ->) (check-size df1 df2) 'div df1 df2 :zero-val zero-val)
    (div (data df1) (data df2) :zero-val zero-val :suppress-warning suppress-warning
	 :-> (data result))
    result))

(defmethod div ((val number) (df discrete-function) 
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) df 'div val df :zero-val zero-val)
    (div val (data df) :zero-val zero-val :suppress-warning suppress-warning :-> (data result))
    result))

(defmethod div ((df discrete-function) (val number)
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) df 'div df val :zero-val zero-val)
    (div (data df) val :zero-val zero-val :suppress-warning suppress-warning :-> (data result))
    result))

(defmethod negate ((df discrete-function) &key ->)
  (with-result ((result ->) df 'negate df)
    (negate (data df) :-> (data result))
    result))

(defmethod linear-xform ((df discrete-function) scale offset &key ->)
  (with-result ((result ->) df 'linear-xform df scale offset)
    (linear-xform (data df) scale offset :-> (data result))
    result))

(defmethod square ((df discrete-function) &key ->)
  (with-result ((result ->) df 'square df)
    (square (data df) :-> (data result))
    result))

(defmethod square-root ((df discrete-function) &key ->)
  (with-result ((result ->) df 'square-root df)
    (square-root (data df) :-> (data result))
    result))

(defmethod abs-value ((df discrete-function) &key ->)
  (with-result ((result ->) df 'abs-value df)  
    (abs-value (data df) :-> (data result))
    result))

;;; Very slow!! -- But if we rewrite using point-operation, it will be very
;;; inaccurate for powers very different from 1.0.
(defmethod power ((df discrete-function) (val number) &key ->)
  (with-result ((result ->) df 'power df val)
    (power (data df) val :-> (data result))
    result))

(defmethod power ((val number) (df discrete-function) &key ->)
  (with-result ((result ->) df 'power val df)
    (power val (data df) :-> (data result))
    result))

(defmethod power ((df1 discrete-function) (df2 discrete-function) &key ->)
  (with-result ((result ->) (check-size df1 df2) 'power df1 df2)
    (power (data df1) (data df2) :-> (data result))
    result))

(defmethod natural-logarithm ((df discrete-function) &key (zero-val *div-by-zero-result*) ->)
  (with-result ((result ->) df 'natural-logarithm df :zero-val zero-val)
    (natural-logarithm (data df) :zero-val zero-val :-> (data result))
    result))

(defmethod clip ((df discrete-function) below above &key ->)
  (with-result ((result ->) df 'clip df below above)
    (clip (data df) below above :-> (data result))
    result))

;;; Default args are the same as for entropy and make-histogram (see
;;; discrete-function.lisp)
(defmethod quantize ((df discrete-function) &key 
		     (binsize (/ (range df) (get-default 'discrete-function :size)))
		     (origin (mean df)) ->)
  (with-result ((result ->) df 'quantize df binsize :origin origin)
    (quantize (data df) :binsize binsize :origin origin :-> (data result))
    result))

(defmethod correlate ((df1 discrete-function) (df2 discrete-function) &key 
		      (x 0) y (x-dim (total-size df1)) y-dim ->)
  (check-size df1 df2)
  (when (or y y-dim)
    (error "Cannot operate on y dimension of ~a or ~a" df1 df2))
  (with-result ((result ->) (list :class (class-of df1) :size x-dim
				  :origin (origin df1) :increment (increment df1))
		'correlate df1 df2)
    (correlate (data df1) (data df2) :x x :x-dim x-dim :-> (data result))
    result))

(defmethod integral ((df discrete-function))
  (* (sum-of (data df)) (increment df)))

;;; *** not tested ***
(defmethod energy ((df discrete-function))
  (* (sqr (vector-length (data df))) (increment df)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Comparison Operations

(defmethod mean-square-error ((df1 discrete-function) (df2 discrete-function))
  (check-size df1 df2)
  (mean-square-error (data df1) (data df2)))

(defmethod mean-abs-error ((df1 discrete-function) (df2 discrete-function))
  (check-size df1 df2)
  (mean-abs-error (data df1) (data df2)))
	 
(defmethod max-abs-error ((df1 discrete-function) (df2 discrete-function))
  (check-size df1 df2)
  (max-abs-error (data df1) (data df2)))

;;; Minimum as a pointop on two discrete-functions
(defmethod point-minimum ((df1 discrete-function) (df2 discrete-function) &key ->)
  (with-result ((result ->) (check-size df1 df2) 'point-minimum df1 df2)
    (point-minimum (data df1) (data df2) :-> (data result))
    result))

(defmethod point-minimum ((df discrete-function) (val number) &key ->)
  (with-result ((result ->) df 'point-minimum df val)
    (point-minimum (data df) val :-> (data result))
    result))

(defmethod point-minimum ((val number) (df discrete-function) &key ->)
  (with-result ((result ->) df 'point-minimum val df)
    (point-minimum (data df) val :-> (data result))
    result))

(defmethod point-maximum ((df1 discrete-function) (df2 discrete-function) &key ->)
  (with-result ((result ->) (check-size df1 df2) 'point-maximum df1 df2)
    (point-maximum (data df1) (data df2) :-> (data result))
    result))

(defmethod point-maximum ((df discrete-function) (val number) &key ->)
  (with-result ((result ->) df 'point-maximum df val)
    (point-maximum (data df) val :-> (data result))
    result))

(defmethod point-maximum ((val number) (df discrete-function) &key ->)
  (with-result ((result ->) df 'point-maximum val df)
    (point-maximum (data df) val :-> (data result))
    result))

(defmethod square-error ((df1 discrete-function) (df2 discrete-function) &key ->)
  (with-result ((result ->) (check-size df1 df2) 'square-error df1 df2)
    (square-error (data df1) (data df2) :-> (data result))
    result))

(defmethod abs-error ((df1 discrete-function) (df2 discrete-function) &key ->)
  (with-result ((result ->) (check-size df1 df2) 'abs-error df1 df2)
    (abs-error (data df1) (data df2) :-> (data result))
    result))

;;; *** Need to define > < etc.  First need to define these on arrays.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Statistics:

;;; If ignore-zeros is t, don't use the info-list
(defmethod mean ((df discrete-function) &key ignore-zeros)
  (or (info-get df :mean)
      (info-set df :mean (mean (data df) :ignore-zeros ignore-zeros))))

(defmethod variance ((df discrete-function) &key ignore-zeros)
  (or (info-get df :variance)
      (info-set df :variance (variance (data df) :ignore-zeros ignore-zeros))))

(defmethod skew ((df discrete-function))  
  (or (info-get df :skew)
      (info-set df :skew
		(/ (third-moment (data df) (mean df))
		   (expt (variance df) 1.5)))))

(defmethod kurtosis ((df discrete-function))
  (or (info-get df :kurtosis)
      (info-set df :kurtosis
		(/ (fourth-moment (data df) (mean df))
		   (sqr (variance df))))))

(defmethod minimum ((df discrete-function))
  (or (info-get df :minimum)
      (info-set df :minimum (minimum (data df)))))

(defmethod minimum-location ((df discrete-function))
  (multiple-value-bind (min-value min-location)
      (minimum (data df))
    (info-set df :minimum min-value)
    (+ (origin df) (* (increment df) min-location))))

(defmethod maximum ((df discrete-function))
  (or (info-get df :maximum)
      (info-set df :maximum (maximum (data df)))))

(defmethod maximum-location ((df discrete-function))
  (multiple-value-bind (max-value max-location)
      (maximum (data df))
    (info-set df :maximum max-value)
    (+ (origin df) (* (increment df) max-location))))

;;; Returns range (max-min, mn, mx) of discrete-function 
(defmethod range ((df discrete-function))
  (let (rng mn mx)
    (if (and (info-get df :maximum) (info-get df :minimum))
	(setq mx (info-get df :maximum)
	      mn (info-get df :minimum)
	      rng (- mx mn))
	(progn
	  (multiple-value-setq (rng mn mx)
	    (range (data df)))
	  (info-set df :minimum mn)  
	  (info-set df :maximum mx)))
    (values rng mn mx)))

;;; returns domain of x-values
(defmethod domain ((df discrete-function))
  (let* ((min (origin df))
	 (rng (if (endpoint df)
		  (* (1- (size df)) (increment df))
		  (* (size df) (increment df))))
	 (max (+ min rng)))
    (values rng min max)))


#|
;;; Old Code:

;;; We use Common Lisp reduce here rather than call C code because the data
;;; may not be single-float.  Could make this faster by checking...
(defmethod minimum ((df  discrete-function))
  (reduce #'min (data df)))

(defmethod maximum ((df  discrete-function))
  (reduce #'max (data df)))

(defmethod peak ((df discrete-function))
  (with-slots (data origin increment) df
    (let ((max (aref data 0))
	  (max-pos 0))
      (loop for i from 0 below (total-size data) do
	    (when (> (aref data i) max)
	      (setq max-pos i
		    max (aref data i))))
      (values (+ origin (* increment max-pos)) max))))
|#

;;; Mean of a df, treated as a probability distribution.
;;; *** ignore-zeros keyword not currently used *** -DH
(defmethod distribution-mean ((df discrete-function) &key ignore-zeros)
  (declare (ignore ignore-zeros))
  (with-slots (origin increment data) df
    (let ((M-sum 0.0)
	  (sum 0.0)
	  (val 0.0)
	  (pos origin))
      (declare (single-float M-sum sum val pos))
      (loop for i from 0 below (total-size df) do
	    (setq val (aref data i))
	    (incf sum val)
	    (incf M-sum (* val pos))
	    (incf pos increment))
      (/ M-sum sum))))

;;; *** ignore-zeros keyword not used yet *** -DH
(defmethod distribution-variance ((df discrete-function) &key ignore-zeros)
  (declare (ignore ignore-zeros))
  (with-slots (origin increment data) df
    (let ((V-sum 0.0)
	  (M-sum 0.0)
	  (sum 0.0)
	  (val 0.0)
	  (pos origin))
      (declare (single-float V-sum M-sum sum val pos))
      (loop for i from 0 below (total-size df) do
	    (setq val (aref data i))
	    (incf sum val)
	    (incf M-sum (* val pos))
	    (incf V-sum (* val pos pos))
	    (incf pos increment))
      (setq M-sum (/ M-sum sum))
      (- (/ V-sum sum) (* M-sum M-sum)))))

;;; Return mean and variance of a df, treated as a probability distribution.
(defmethod distribution-mean-and-variance ((df discrete-function))
  (with-slots (origin increment data) df
    (let ((V-sum 0.0)
	  (M-sum 0.0)
	  (sum 0.0)
	  (val 0.0)
	  (pos origin))
      (declare (single-float V-sum M-sum sum val pos))
      (loop for i from 0 below (total-size df) do
	    (setq val (aref data i))
	    (incf sum val)
	    (incf M-sum (* val pos))
	    (incf V-sum (* val pos pos))
	    (incf pos increment))
      (setq M-sum (/ M-sum sum))
      (setq V-sum (/ V-sum sum))
      (values M-sum (- V-sum (* M-sum M-sum))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Geometric Operations:

;;; *** Need to define append subsample downsample upsample, but first
;;; define these on arrays.

;;; *** define these in terms of real numbers...

;;; Assumes a periodic function, origin reset to be (- origin (*
;;; x-shift increment)).  :x-shift for back-compatibility
(defmethod circular-shift ((df discrete-function) &key
			   (x-shift 0) (x x-shift) -> &allow-other-keys)
  (with-result ((result ->) df 'circular-shift df :x x)
    (circular-shift (data df) :x x :-> (data result))
    (setf (origin result) (- (origin result) (* x (increment result))))
    result))

#|
(setq df (make-discrete-function '(lambda (x) (sin x))
				 (/ pi 15.0) (* 2.0 pi)
				 :size 30))
(setq df2 (circular-shift df :x 16))
(origin df2)
(evaluate df2 (origin df2))
(evaluate df2 0.0)
|#

(defmethod paste ((df discrete-function) (base-df discrete-function)
		  &key (x-offset 0) (x x-offset) -> &allow-other-keys)
  (unless (equal (increment df) (increment base-df))
    (error "~A and ~a have different increment." df base-df))
  (unless (equal (origin df) (origin base-df))
    (warn "~A and ~a have different origin." df base-df))
  (with-result ((result ->) base-df 'paste df :x x)
    (paste (data df) (data base-df) :x x :-> (data result))
    result))

;; x-size for back-compatibility
(defmethod crop ((df discrete-function) &key (x 0) (x-size (- (size df) x)) 
		 (x-dim x-size) -> &allow-other-keys)
  (with-result ((result ->) 
		(list :class (clos::class-of df)
		      :size x-dim
		      :increment (increment df)
		      :origin (+ (origin df) (* x (increment df))))
		'crop df :x x :x-dim x-dim)
    (crop (data df) :x x :x-dim x-dim :-> (data result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Histograms:

(defmacro histogram-p (obj)
  `(typep ,obj 'histogram))

(defmethod binsize ((histo histogram))
  (increment histo))

;;; Bincenter is the position of the center of any of the bins, i.e.,
;;; {x : x = (bincenter + i * binsize)}, for integer i, is the set of
;;; bin centers.
;;; *** Can't we use with-result here? -EPS
(defmethod make-histogram ((im image) 
			   &key
			   (range (list (minimum im) (maximum im)))
			   (binsize nil binsize-specified-p)
			   (bincenter (mean im))
			   (size (get-default (find-class 'discrete-function) :size)
				 size-specified-p)
			   (name (format nil "Histogram of ~A" (name im)))
			   (-> name))
  (let ((interval (- (apply '- range)))
	data origin)
    (cond ((and binsize-specified-p size-specified-p)
	   (error "Can't specify both binsize and size of histogram"))
	  (binsize-specified-p
	   (setq size (+ (/-0 interval binsize) 2)))
	  (t (setq binsize (if (zerop interval) 1.0 (/ interval (- size 2))))))
    ;; Origin is bincenter minus a multiple of binsize such that
    ;; origin is <= minimum of image.
    (setq origin (- bincenter (* binsize (round (- bincenter (car range)) binsize))))
    (setq data (compute-histogram im origin binsize (floor size)))
    (make-instance 'histogram 
		   :data data
		   :size (total-size data)
		   :origin origin 
		   :increment binsize
		   :image im
		   :name ->)))

(defmethod make-histogram ((list list) &rest args)
  (with-local-viewables ((im (make-image (make-matrix list))))
    (apply 'make-histogram im args)))

(defmethod make-histogram ((arr array) &rest args)
  (with-local-viewables ((im (make-image arr)))
    (apply 'make-histogram im args)))


#|
;;; Old version, replaced 10-23-91.  -DH
(defmethod make-histogram ((im image) 
			   &key 
			   (size (get-default (find-class 'discrete-function) :size))
			   ;; one extra bin, just in case...
			   (binsize (if (plusp (range im)) (/ (range im) (- size 2)) 1.0))
			   (origin (mean im))
			   (name (format nil "Histogram of ~A" (name im))))
  ;; Adjust origin to be below minimum.
  (setq origin (- origin (* binsize (round (- origin (minimum im)) binsize))))
  (let ((data (compute-histogram im origin binsize size)))
    (make-instance 'histogram 
		   :data data
		   :size (total-size data)
		   :origin origin 
		   :increment binsize
		   :image im
		   :name name)))
|#

(defmethod entropy ((hist histogram) &key &allow-other-keys)
  (let* ((data (data hist))
	 (d-size (length data))
	 (total-count (sum-of hist))
	 (val 0.0)) (declare (single-float val))
    (loop for i from 0 below d-size 
	  for temp = (/ (aref data i) total-count) do
	  (when (> temp 0.0) (incf val (- (* temp (log temp 2.0))))))
    val))

#|
;;; old version, changed 5/11/93 -DH
(defmethod entropy ((hist histogram) &key &allow-other-keys)
  (let* ((data (data hist))
	 (d-size (length data))
	 (im-size (total-size (image hist)))
	 (val 0.0)) (declare (single-float val))
    (loop for i from 0 below d-size 
	  for temp = (/ (aref data i) im-size) do
	  (when (> temp 0.0) (incf val (- (* temp (log temp 2.0))))))
    val))
|#
 
(defmethod entropy ((im image) &rest args)
  (with-local-viewables ((hist (apply 'make-histogram im args)))
    (entropy hist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Complex-Discrete-Functions

(defmacro complex-discrete-function-p (obj)
  `(typep ,obj 'complex-discrete-function))

(defmethod real-part ((df complex-discrete-function) &key ((:-> result)))
  (frame df 1 :-> result))

(defmethod imaginary-part ((df complex-discrete-function) &key ((:-> result)))
  (frame df 0 :-> result))

(defmethod switch-components ((df complex-discrete-function))
  (let ((tmp (aref (data df) 0 0)))
    (setf (aref (data df) 0 0) (aref (data df) 0 1))
    (setf (aref (data df) 0 1) tmp)))

(defmethod size ((df complex-discrete-function))
  (size (real-part df)))

(defmethod origin ((df complex-discrete-function))
  (origin (real-part df)))

(defmethod increment ((df complex-discrete-function))
  (increment (real-part df)))

(defmethod domain ((df complex-discrete-function))
  (domain (real-part df)))

(defmethod fft ((df discrete-function) &key inverse ->)
  (let* ((padded-size (power-of-two-ceiling (size df)))
	 (model-plist (list :class 'complex-discrete-function))
	 real-part imag-part)
    (unless (viewable-p ->)
      (setq real-part (make-instance (class-of df)
				     :size padded-size
				     :origin 0.0
				     :increment (/ 1 (increment df) padded-size)))
      (setq imag-part (make-instance (class-of df)
				     :size padded-size
				     :origin 0.0
				     :increment (/ 1 (increment df) padded-size)))
      (setq model-plist (append model-plist (list :viewable-list (list imag-part real-part)))))
    (with-result ((result ->) model-plist 'fft df :inverse inverse)
      (zero! result)
      (paste (data df) (data (real-part result)) :x 0 :-> (data (real-part result)))
      (array-fft (data (real-part result)) (data (imaginary-part result)) :inverse inverse)
      result)))

(defmethod fft ((df complex-discrete-function) &key inverse ->)
  (let* ((padded-size (power-of-two-ceiling (size df)))
	 (model-plist (list :class 'complex-discrete-function))
	 real-part imag-part)
    (unless (viewable-p ->)
      (setq real-part (make-instance (class-of (real-part df))
				     :size padded-size
				     :origin 0.0
				     :increment (/ 1 (increment df) padded-size)))
      (setq imag-part (make-instance (class-of (imaginary-part df))
				     :size padded-size
				     :origin 0.0
				     :increment (/ 1 (increment df) padded-size)))
      (setq model-plist (append model-plist (list :viewable-list (list imag-part real-part)))))
    (with-result ((result ->) model-plist 'fft df :inverse inverse)
      (zero! result)
      (paste (data (real-part df)) (data (real-part result)) :x 0
	     :-> (data (real-part result)))
      (paste (data (imaginary-part df)) (data (imaginary-part result)) :x 0
	     :-> (data (imaginary-part result)))
      (array-fft (data (real-part result)) (data (imaginary-part result)) :inverse inverse)
      result)))

#|
FFT example:

(setq freq 8.0)
(setq df (make-discrete-function '(lambda (x) (sin (* 2.0 pi freq x)))
				 (/ 1.0 128.0) 1.0 :size 128))
(setq foo (fft df))
(setq mag (magnitude foo))
(evaluate mag freq)
(setq phase (complex-phase foo))
|#

;;; *** This is not debugged yet.  Breaks for histograms.
(defmethod hilbert-transform ((df discrete-function) &key ((:-> result)) &allow-other-keys)
  (with-local-viewables ((dft (fft df)))
    (circular-shift dft :x (floor (size dft) 2) :-> dft)
    (with-local-viewables ((signum (make-discrete-function
				    #'signum
				    (origin dft)
				    (+ (origin dft) (* (increment dft) (1- (size dft))))
				    :size (size dft))))
      (mul dft signum :-> dft)
      (circular-shift dft :x (- (floor (size dft) 2)) :-> dft)
      (switch-components dft)
      (with-local-viewables ((inverse-dft (fft dft :inverse t)))
	(copy (real-part inverse-dft) :-> result)))))

#|
(setq freq 1.0)
(setq gabor (make-discrete-function '(lambda (x) (* (exp (- (sqr x)))
						  (sin (* 2 pi freq x))))
				    -2.0 2.0 :size 128))
(setq even (hilbert-transform gabor))
|#

(defmethod mul ((df1 complex-discrete-function) (df2 complex-discrete-function) &key ->)
  (with-result ((result ->) df1 'mul df1 df2)
    (sub (mul (real-part df1) (real-part df2) :-> (real-part result))
	 (mul (imaginary-part df1) (imaginary-part df2) :-> (imaginary-part result))
	 :-> (real-part result))
    (add (mul (real-part df1) (imaginary-part df2) :-> (imaginary-part result))
	 (mul (imaginary-part df1) (real-part df2)) ;temp df created here
	 :-> (imaginary-part result))
    result))

(defmethod mul ((df1 complex-discrete-function) (df2 discrete-function) &key ->)
  (with-result ((result ->) df1 'mul df1 df2)
    (mul (real-part df1) df2 :-> (real-part result))
    (mul (imaginary-part df1) df2 :-> (imaginary-part result))
    result))

(defmethod mul ((df2 discrete-function) (df1 complex-discrete-function) &key ->)
  (mul df1 df2 :-> ->))

(defmethod div ((df1 complex-discrete-function) (df2 complex-discrete-function)
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) df1 'div df1 df2)
    (mul df1 (complex-conjugate df2) :-> result)
    (div result (square-magnitude df2) :suppress-warning suppress-warning
	 :zero-val zero-val :-> result)))

(defmethod div ((df1 complex-discrete-function) (df2 discrete-function) 
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) df1 'div df1 df2)
    (div (real-part df1) df2 :zero-val zero-val :suppress-warning suppress-warning
	 :-> (real-part result))
    (div (imaginary-part df1) df2 :zero-val zero-val :suppress-warning suppress-warning
	 :-> (imaginary-part result))
    result)) 

(defmethod complex-conjugate ((df complex-discrete-function) &key ->)
  (with-result ((result ->) df 'complex-conjugate df)
    (unless (eq (real-part df) (real-part result))
      (copy (real-part df) :-> (real-part result)))
    (sub 0.0 (imaginary-part df) :-> (imaginary-part result))
    result))

(defmethod magnitude ((df complex-discrete-function) &key ->)
  (with-result ((result ->) 
		(list :class 'discrete-function
		      :size (size df)
		      :origin (origin df)
		      :increment (increment df))
		'magnitude df)
    (array-magnitude (data (real-part df)) (data (imaginary-part df)) :-> (data result))
    result))

(defmethod abs-value ((df complex-discrete-function) &key ->)
  (with-result ((result ->)
		(list :class 'discrete-function
		      :size (size df)
		      :origin (origin df)
		      :increment (increment df))
		'magnitude df)
    (array-magnitude (data (real-part df)) (data (imaginary-part df)) :-> (data result))
    result))

(defmethod square-magnitude ((df complex-discrete-function) &key ->)
  (with-result ((result ->)
		(list :class 'discrete-function
		      :size (size df)
		      :origin (origin df)
		      :increment (increment df))
		'square-magnitude df)
    (array-square-magnitude (data (real-part df)) (data (imaginary-part df)) :-> (data result))
    result))

(defmethod complex-phase ((df complex-discrete-function) &key ->)
  (with-result ((result ->)
		(list :class 'discrete-function
		      :size (size df)
		      :origin (origin df)
		      :increment (increment df))
		'complex-phase df)
    (array-complex-phase (data (real-part df)) (data (imaginary-part df)) :-> (data result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Periodic-discrete-function

(defmacro periodic-discrete-function-p (obj)
  `(typep ,obj 'periodic-discrete-function))

(defun make-periodic-discrete-function
    (func min max &rest initargs &key size interpolator name ->)
  (declare (ignore size interpolator name))
  (when -> (setf (getf initargs :name) ->))
  (when (not (functionp func))
    (error "Argument must be a function or symbol with function binding"))
  (when (symbolp func) (setq func (symbol-function func))) ;for history purposes
  (let ((df (apply 'make-instance 'periodic-discrete-function initargs)))
    (fill-discrete-function df func min max)
    df))

;;; evaluates, assuming periodic function, using mod
(defun periodic-linear-interpolate (ar origin increment value)
  ;;(declare (type (array single-float (*)) ar))
  (let* ((pos (float (/-0 (- value origin) increment)))
	 (mod-pos (mod pos (length ar)))
	 (index (min (- (length ar) 2) (floor mod-pos))))
    ;;(declare (single-float pos) (fixnum index))
    ;;(print-db pos mod-pos index)
    (+ (aref ar index) (* (- (aref ar (1+ index)) (aref ar index)) 
			  (- mod-pos index)))))

(defmethod point-operation ((im image) (df periodic-discrete-function) &key ->
			    &allow-other-keys)
  (with-slots (increment origin data) df
    (with-result ((result ->) im 'point-operation im df)
      (internal-periodic-pointop (data im) (data result) (total-size im)
				 (data df) (total-size data) 
				 (float origin) (float increment))
      result)))

#|
(setq sine-grating (make-periodic-discrete-function
		    '(lambda (x) (sin (* 8 pi x)))
		    0 1 :size 32))
(evaluate sine-grating 1.0)
(evaluate sine-grating (- 1 (increment sine-grating)))
(evaluate sine-grating (- (increment sine-grating) 1))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Log-discrete-function

(defmacro log-discrete-function-p (obj)
  `(typep ,obj 'log-discrete-function))

(defmethod set-result ((name t) (model log-discrete-function))
  (check-type name viewable-name)
  (make-instance (class-of model)
		 :size (size model)
		 :origin (origin model)
		 :increment (increment model)
		 :endpoint (endpoint model)
		 :interpolator (interpolator model)
		 :base (base model)
		 :name name
		 :display-type (display-type model)))
#|
(defun make-log-discrete-function (func min max &rest initargs)
  (when (not (functionp func))
    (error "Argument must be a function or symbol with function binding"))
  (when (symbolp func) (setq func (symbol-function func))) ;for history purposes
  (let ((df (apply 'make-instance 'log-discrete-function initargs)))
    (fill-discrete-function df func min max)
    df))
|#
(defmethod make-log-discrete-function ((func t) min max &rest initargs
					&key size interpolator endpoint name ->)
  (declare (ignore size interpolator endpoint name))
  (when -> (setf (getf initargs :name) ->))
  (unless (functionp func)
    (error "Argument must be a function or symbol with function binding"))
  (when (symbolp func) (setq func (symbol-function func))) ;for history purposes
  (let ((df (apply 'make-instance 'log-discrete-function initargs)))
    (fill-discrete-function df func min max)
    df))

(defmethod make-log-discrete-function ((arr vector) min max &rest initargs
				   &key (size (length arr)) (base 10.0)
				       interpolator endpoint name  ->)
  (declare (ignore endpoint interpolator name))
  (when -> (setf (getf initargs :name) ->))
  (setf (getf initargs :size) size)
  (let* ((df (apply 'make-instance 'log-discrete-function :data arr :base base initargs))
	 (log-max (log max (base df)))
	 (log-min (log min (base df))))
    (setf (origin df) (min log-min log-max))
    (setf (increment df)  (if (endpoint df)
			      (float (/-0 (abs (- log-max log-min)) (1- size)))
			      (float (/-0 (abs (- log-max log-min)) size))))
    df))


(defmethod fill-discrete-function ((df log-discrete-function) func min max)
  (let* ((log-max (log max (base df)))
	 (log-min (log min (base df)))
	 (size (total-size df))
	 (data (data df))
	 (incr (if (endpoint df)
		   (float (/-0 (abs (- log-max log-min)) (1- size)))
		   (float (/-0 (abs (- log-max log-min)) size))))
	 (val (float (min log-min log-max))))
    (declare (fixnum size) 
	     (single-float val incr)
	     (type (array single-float (*)) data))
    (setf (origin df) val  
	  (increment df) incr)
    (loop for i from 0 below size do
      (setf (aref data i) (float (funcall func (expt (base df) val))))
      (incf val incr)))
  (set-history df 'fill-discrete-function func min max)
  (set-not-current df)
  df)

(defmethod domain ((df log-discrete-function))
  (let* ((min (expt (base df) (origin df)))
	 (max (expt (base df)
		    (+ (origin df)
		       (if (endpoint df)
			   (* (1- (size df)) (increment df))
			   (* (size df) (increment df))))))
	 (rng (- max min)))
    (values rng min max)))

#|
(setq foo (make-log-discrete-function
	   #'(lambda (x) (/ (sqr x) (+ (sqr x) (sqr .1))))
	   0.01 1.0 :size 30 :base 10))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| ;;;; code from ~heeger/lisp-code/image-hacks.lisp.  Needs to be rewritten. 
(defun image-histogram-equalize (im
				 &key
				 (into-image (img:similar-image im :element-type 'sys:art-q))
				 (rescale nil)
				 (zeros-invalid nil)
				 (number-of-buckets nil))
  (multiple-value-bind
    (histogram-array bucket-size bucket-offset)
      (img:image-histogram im :max-buckets number-of-buckets :zeros-invalid zeros-invalid)
    (with-local-arrays ((map-array (make-array (array-dimensions histogram-array)
					       :element-type (array-element-type histogram-array))))
      (make-histogram-equalize-map-array im histogram-array bucket-size bucket-offset map-array)
      (if rescale
	  (img:with-local-images ((equalized-image
				    (image-nonlinear-xform im map-array bucket-size bucket-offset)))
	    (multiple-value-bind (min max) (img:image-min-max equalized-image)
	      (img:image-linear-xform equalized-image (/ 2.0 (- max min)) 0.0 :into-image into-image :result-type 'sys:art-q)))
	  (image-nonlinear-xform im map-array bucket-size bucket-offset :into-image into-image)))))

(defun image-symetrical-histogram-equalize (im
					    &key
					    (into-image (img:similar-image im :element-type 'sys:art-q))
					    (rescale nil)
					    (number-of-buckets nil))
  (img:with-local-images ((neg-im (img:similar-image im :element-type 'sys:art-q :initial-value 0.0))
			  (abs-neg-im (img:similar-image im :element-type 'sys:art-q :initial-value 0.0))
			  (pos-im (img:similar-image im :element-type 'sys:art-q :initial-value 0.0))
			  (abs-im (img:similar-image im :element-type 'sys:art-q :initial-value 0.0)))
    (image-truncate-max im 0.0 :into-image neg-im)
    (image-truncate-min im 0.0 :into-image pos-im)
    (image-abs im :into-image abs-im)
    (image-abs neg-im :into-image abs-neg-im)
    (multiple-value-bind
      (histogram-array bucket-size bucket-offset)
	(img:image-histogram abs-im :max-buckets number-of-buckets :zeros-invalid t)
      (with-local-arrays ((map-array (make-array (array-dimensions histogram-array)
						 :element-type (array-element-type histogram-array))))
	(make-histogram-equalize-map-array abs-im histogram-array bucket-size bucket-offset map-array)
	(image-nonlinear-xform pos-im map-array bucket-size bucket-offset :zeros-invalid t :into-image pos-im)
	(image-nonlinear-xform abs-neg-im map-array bucket-size bucket-offset :zeros-invalid t :into-image abs-neg-im)
	(img:image-negate abs-neg-im :into-image neg-im)))
    (if rescale
	(img:with-local-images ((equalized-image (img:image-add pos-im neg-im)))
	  (multiple-value-bind (min max) (img:image-min-max equalized-image)
	    (img:image-linear-xform equalized-image (/ 2.0 (- max min)) 0.0 :into-image into-image :result-type 'sys:art-q)))
	(img:image-add pos-im neg-im :into-image into-image))))

(defun make-histogram-equalize-map-array (im histogram-array bucket-size bucket-offset map-array)
  (let* ((number-of-grey-levels (array-dimension histogram-array 0))
	 (number-of-pixels (* (img:image-x-dim im) (img:image-y-dim im)))
	 (factor (/ number-of-grey-levels number-of-pixels))
	 (cumulative 0.0))
    (loop for i from 0 below number-of-grey-levels do
      (setq cumulative (+ cumulative (* bucket-size (aref histogram-array i))))
      (setf (aref map-array i) (+ (* factor cumulative) bucket-offset))))
  map-array)

|#


;;; -*- Local Variables: -*-
;;; -*- buffer-read-only: t -*-
;;; -*- End: -*-
