;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  file: imops.lisp
;;;  author:  eero simoncelli
;;;  description: operations on images.
;;;  creation date: spring, '88
;;;  ----------------------------------------------------------------
;;;    object-based vision and image understanding system (obvius),
;;;      copyright 1988, vision science group,  media laboratory,  
;;;              massachusetts institute of technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define lots of point-by-point operations on images.  Most of these
;;; operations call c functions which run 1.3-1.5 times faster than
;;; the lisp code (on a sun3).

(in-package :obvius)

;;; exports are done in generic-fns.lisp
(export '(rotate resample make-8bit-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fill! ((im image) value)
  (with-result ((res im) im 'fill! value)
    (fill! (data res) value)
    res))

(defmethod copy ((im image) &key ->)
  (with-result ((result ->) im 'copy im)
    (copy (data im) :-> (data result))
    result))

#|
;;; *** work this out once we've written images with general element-types
(defmethod coerce-to-float ((im image) &key ->)
  (with-result ((result ->) im 'coerce-to-float im)
    (coerce-to-float (data im) :-> (data result))
    result))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; arithmetic operations:

(defmethod add ((im1 image) (im2 image) &key ->)
  (with-result ((result ->) im1 'add im1 im2)
    (add (data im1) (data im2) :-> (data result))
    result))

(defmethod add ((im image) (val number) &key ->)
  (with-result ((result ->) im 'add im val)
    (add (data im) val :-> (data result))
    result))

(defmethod add ((val number) (im image) &key ->)
  (with-result ((result ->) im 'add val im)
    (add (data im) val :-> (data result))
    result))

(defmethod sub ((im1 image) (im2 image) &key ->)
  (with-result ((result ->) im1 'sub im1 im2)
    (sub (data im1) (data im2) :-> (data result))
    result))

(defmethod sub ((val number) (im image) &key ->)
  (with-result ((result ->) im 'sub val im)
    (sub val (data im) :-> (data result))
    result))

(defmethod sub ((im image) (val number) &key ->)
  (with-result ((result ->) im 'sub im val)
    (add (data im) (- val) :-> (data result))
    result))

(defmethod mul ((im1 image) (im2 image) &key ->)
  (with-result ((result ->) im1 'mul im1 im2)
    (mul (data im1) (data im2) :-> (data result))
    result))

(defmethod mul ((im image) (val number) &key ->)
  (with-result ((result ->) im 'mul im val) 
    (mul (data im) val :-> (data result))
    result))

(defmethod mul ((val number) (im image) &key ->)
  (with-result ((result ->) im 'mul val im)
    (mul (data im) val :-> (data result))
    result))

(defmethod div ((im1 image) (im2  image) 
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) im1 'div im1 im2 :zero-val zero-val)
    (div (data im1) (data im2) :zero-val zero-val :suppress-warning suppress-warning :-> (data result))
    result))

(defmethod div ((val number) (im image) 
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) im 'div val im :zero-val zero-val)
    (div val (data im) :zero-val zero-val :suppress-warning suppress-warning :-> (data result))
    result))

(defmethod div ((im image) (val number)
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) im 'div im val :zero-val zero-val)
    (div (data im) val :zero-val zero-val :suppress-warning suppress-warning :-> (data result))
    result))

(defmethod negate ((im image) &key ->)
  (with-result ((result ->) im 'negate im)
    (negate (data im) :-> (data result))
    result))

(defmethod linear-xform ((im image) scale offset &key ->)
  (with-result ((result ->) im 'linear-xform im scale offset)
    (linear-xform (data im) scale offset :-> (data result))
    result))

(defmethod square ((im image) &key ->)
  (with-result ((result ->) im 'square im)
    (square (data im) :-> (data result))
    result))

(defmethod square-root ((im image) &key ->)
  (with-result ((result ->) im 'square-root im)
    (square-root (data im) :-> (data result))
    result))

(defmethod abs-value ((im image) &key ->)
  (with-result ((result ->) im 'abs-value im)  
    (abs-value (data im) :-> (data result))
    result))

;;; slow!! -- but if we rewrite using point-operation, it will be very
;;; inaccurate for powers very different from 1.0.
(defmethod power ((im image) (val number) &key ->)
  (with-result ((result ->) im 'power im val)
    (power (data im) val :-> (data result))
    result))

(defmethod power ((val number) (im image) &key ->)
  (with-result ((result ->) im 'power val im)
    (power val (data im) :-> (data result))
    result))

(defmethod power ((im1 image) (im2 image) &key ->)
  (with-result ((result ->) im1 'power im1 im2)
    (power (data im1) (data im2) :-> (data result))
    result))

(defmethod natural-logarithm ((im image) &key (zero-val *div-by-zero-result*) ->)
  (with-result ((result ->) im 'natural-logarithm im :zero-val zero-val)
    (natural-logarithm (data im) :zero-val zero-val :-> (data result))
    result))

(defmethod logarithm ((im image) (base number) &key (zero-val *div-by-zero-result*) ->)
  (with-result ((result ->) im 'natural-logarithm im :zero-val zero-val)
    (logarithm (data im) base :zero-val zero-val :-> (data result))
    result))

;;; if binsize is a number, a fast point operation using a discrete
;;; function (lookup table) is performed.  if nil, the function is
;;; called at each point in the image.  
(defmethod point-operation ((im image) (func t) &key binsize ->)
  (with-result ((result ->) im 'point-operation im func :binsize binsize)
    (point-operation (data im) func :binsize binsize :-> (data result))
    result))

;;; apply function with given period to image using a lookup table and
;;; linear interpolation.  lookup table is built to cover one period,
;;; with an extra point on the end.  see synth.lisp for useful examples.
(defmethod periodic-point-operation ((im image) func period &key binsize ->)
  (with-result ((result ->) im
		'periodic-point-operation im func period :binsize binsize)
    (periodic-point-operation (data im) func period :binsize binsize :-> (data result))
    result))

(defmethod clip ((im image) below above &key ->)
  (with-result ((result ->) im 'clip im below above)
    (clip (data im) below above :-> (data result))
    result))

;;; default args are the same as for entropy and make-histogram (see
;;; discrete-function.lisp)
(defmethod quantize ((im image) &key 
		     (binsize (/ (range im) (get-default 'discrete-function :size))) 
		     (origin (mean im)) ->)
  (with-result ((result ->) im 'quantize im binsize :origin origin)
    (quantize (data im) :binsize binsize :origin origin :-> (data result))
    result))

;;; EJ added in keywords to control range of correlation.
(defmethod correlate ((im1 image) (im2 image) &key
		      (x 0) (y 0) (x-dim (x-dim im1)) (y-dim (y-dim im1)) ->)
  (check-size im1 im2)
  (let ((dims (cond ((= (length (dimensions im1)) 2) (list y-dim x-dim))
		    (t x-dim))))
    (with-result ((result ->) (list :class (class-of im1) :dimensions dims)
		  'correlate im1 im2)
      (correlate (data im1) (data im2) :x x :y y :x-dim x-dim :y-dim y-dim :-> (data result))
      result)))

(defmethod convolve ((im1 image) (im2 image) &key
		     (x 0) (y 0) (x-dim (x-dim im1)) (y-dim (y-dim im1)) ->)
  (check-size im1 im2)
  (let ((dims (cond ((= (length (dimensions im1)) 2) (list y-dim x-dim))
		    (t x-dim))))
    (with-result ((result ->) (list :class (class-of im1) :dimensions dims)
		  'convolve im1 im2)
      (convolve (data im1) (data im2) :x x :y y :x-dim x-dim :y-dim y-dim :-> (data result))
      result)))

#|
(defmethod correlate ((im1 image) (im2 image) &key
		      (x 0) (y 0) (x-dim (x-dim im1)) (y-dim (y-dim im1)) ->)
  (check-size im1 im2)
  (with-result ((result ->) (list :class (class-of im1) :dimensions (list y-dim x-dim))
		'correlate im1 im2)
    (correlate (data im1) (data im2) :x x :y y :x-dim x-dim :y-dim y-dim :-> (data result))
    result))

(defmethod correlate ((im1 image) (im2 image) &key ->)
  (with-result ((result ->) im1 'correlate im1 im2)
    (correlate (data im1) (data im2) :-> (data result))
    result))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; comparison operations

(defmethod mean-square-error ((im1 image) (im2 image))
  (check-size im1 im2)
  (mean-square-error (data im1) (data im2)))

(defmethod mean-abs-error ((im1 image) (im2 image))
  (check-size im1 im2)
  (mean-abs-error (data im1) (data im2)))
	 
(defmethod max-abs-error ((im1 image) (im2 image))
  (check-size im1 im2)
  (max-abs-error (data im1) (data im2)))

;;; minimum as a pointop on two images
(defmethod point-minimum ((im1 image) (im2 image) &key ->)
  (with-result ((result ->) (check-size im1 im2) 'point-minimum im1 im2)
    (point-minimum (data im1) (data im2) :-> (data result))
    result))

(defmethod point-minimum ((im image) (val number) &key ->)
  (with-result ((result ->) im 'point-minimum im val)
    (point-minimum (data im) val :-> (data result))
    result))

(defmethod point-minimum ((val number) (im image) &key ->)
  (with-result ((result ->) im 'point-minimum val im)
    (point-minimum (data im) val :-> (data result))
    result))

(defmethod point-maximum ((im1 image) (im2 image) &key ->)
  (with-result ((result ->) (check-size im1 im2) 'point-maximum im1 im2)
    (point-maximum (data im1) (data im2) :-> (data result))
    result))

(defmethod point-maximum ((im image) (val number) &key ->)
  (with-result ((result ->) im 'point-maximum im val)
    (point-maximum (data im) val :-> (data result))
    result))

(defmethod point-maximum ((val number) (im image) &key ->)
  (with-result ((result ->) im 'point-maximum val im)
    (point-maximum (data im) val :-> (data result))
    result))

(defmethod square-error ((im1 image) (im2 image) &key ->)
  (with-result ((result ->) (check-size im1 im2) 'square-error im1 im2)
    (square-error (data im1) (data im2) :-> (data result))
    result))

(defmethod abs-error ((im1 image) (im2 image) &key ->)
  (with-result ((result ->) (check-size im1 im2) 'abs-error im1 im2)
    (abs-error (data im1) (data im2) :-> (data result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; image statistics:

;;; most of these return things other than images.  note that some of
;;; these functions put their results on the image info-list for
;;; future reference.  this is safe, since the info-list is blown away
;;; whenever the image is destructively modified.

;;; if ignore-zeros is t, don't use the info-list
(defmethod mean ((im image) &key ignore-zeros)
  (or (info-get im :mean)
      (info-set im :mean (mean (data im) :ignore-zeros ignore-zeros))))

(defmethod variance ((im image) &key ignore-zeros)
  (or (info-get im :variance)
      (info-set im :variance (variance (data im) :ignore-zeros ignore-zeros))))

(defmethod skew ((im image))  
  (or (info-get im :skew)
      (info-set im :skew
		(/ (third-moment (data im) (mean im))
		   (expt (variance im) 1.5)))))

(defmethod kurtosis ((im image))
  (or (info-get im :kurtosis)
      (info-set im :kurtosis
		(/ (fourth-moment (data im) (mean im))
		   (sqr (variance im))))))

(defmethod minimum ((im image))
  (or (info-get im :minimum)
      (info-set im :minimum (minimum (data im)))))

;;;*** why is floor here? -EPS
(defmethod minimum-location ((im image))
  (multiple-value-bind (min-value min-location)
      (minimum (data im))
    (info-set im :minimum min-value)
    (multiple-value-list (floor min-location (x-dim im)))))

(defmethod maximum ((im image))
  (or (info-get im :maximum)
      (info-set im :maximum (maximum (data im)))))

(defmethod maximum-location ((im image))
  (multiple-value-bind (max-value max-location)
      (maximum (data im))
    (info-set im :maximum max-value)
    (multiple-value-list (floor max-location (x-dim im)))))

(defun compute-histogram (im origin binsize numbins)
  (let ((ar (allocate-array numbins
			    :element-type 'single-float
			    :initial-element 0.0)))
    (with-status-message "computing histogram"
      (internal-histogram (data im) (total-size im) ar
			  (total-size ar) (float origin) (float binsize)))
    ar))

#|
;;; old definition.  replaced by dh 2/14/91.
(defun compute-histogram (im origin binsize)
  (let ((ar (make-array (1+ (ceiling (- (maximum im) origin) binsize))
			:element-type 'fixnum
			:initial-element 0)))
    (with-status-message "computing histogram"
      (internal-histogram (data im) (total-size im) ar
			  (total-size ar) (float origin) (float binsize)))
    ar))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; geometric operations

(defmethod flip-x ((im image) &key ->)
  (with-result ((result ->) im 'flip-x im)
    (internal-flip-x (data im) (data result) (y-dim im) (x-dim im))
    result))

(defmethod flip-y ((im image) &key ->)
  (with-result ((result ->) im 'flip-y im)
    (internal-flip-y (data im) (data result) (y-dim im) (x-dim im))
    result))

(defmethod flip-xy ((im image) &key ->)
  (with-result ((res ->) im 'flip-xy im)
    (internal-flip-xy (data im) (data res) (y-dim im) (x-dim im))
    res))

;;; *** need to write an affine-warp function (in c) and call it here
;;; instead of consing  a temporary image-pair!
(defmethod rotate ((image image) &key ->
		   (angle (/ pi 2))
		   (interpolator :cubic)
		   (origin (mapcar #'(lambda (x) (/ (1- x) 2.0))
				   (dimensions image))))
  (with-result ((res ->) image
		'rotate :angle angle :interpolator interpolator :origin origin)
    (with-local-viewables
	((warper (make-image-pair
		  (list (make-ramp (dimensions image) :origin origin
				   :orientation (/ angle -2)
				   :slope (* 2 (sin (/ angle 2))))
			(make-ramp (dimensions image) :origin origin
				   :orientation (- (/ pi 2) (/ angle 2))
				   :slope  (* 2 (sin (/ angle 2)))))
		  :display-type 'vector-field)))
      (round. warper :-> warper)
      (warp image warper :interpolator interpolator :-> res))))

;; expansion-factor can be a number or a list of two numbers.  it is
;; redundant with dimensions (the dimensions of the result image).
(defmethod resample ((im image) &key ->
		     dimensions expansion-factor
		     (interpolator :cubic))
  (cond ((numberp expansion-factor)
	 (setq dimensions (mapcar #'(lambda (x) (ceiling (* expansion-factor x)))
				  (dimensions im))))
	((and (consp expansion-factor) (every #'numberp expansion-factor))
	 (setq dimensions
	       (mapcar #'(lambda (x f) (ceiling (* f x)))
		       (dimensions im)
		       expansion-factor)))
	((and (consp dimensions) (every #'integerp dimensions)) nil)
	(t (error "must supply :expansion-factor as number or dimlist, or :dimensions")))
  (setq expansion-factor (mapcar #'/ dimensions (dimensions im)))
  (let* ((im-dims (dimensions im))
	 (mx-dims (mapcar #'max dimensions im-dims))
	 (slopes (mapcar #'(lambda (e) (if (> e 1.0) (- e 1.0) (/ (- e 1.0) e)))
			 expansion-factor)))
    (with-result ((res ->) (list :class (class-of im) :dimensions dimensions)
		  'resample im :dimensions dimensions
		  :expansion-factor expansion-factor :interpolator interpolator)
      (with-local-viewables
	  ((mx-im (paste im (make-image mx-dims)))
	   (warper (make-image-pair
		    (list
		     (make-ramp mx-dims :slope (car slopes) :origin '(0 0)
				:orientation (/ pi -2))
		     (make-ramp mx-dims :slope (cadr slopes) :origin '(0 0)))))
	   (warped (warp mx-im warper :interpolator interpolator)))
	(crop warped :x-dim (cadr dimensions) :y-dim (car dimensions) :-> res)))))

(defmethod transpose ((im image) &key ->)
  (with-result ((result ->) (list :class (clos::class-of im)
				  :dimensions (reverse (dimensions im)))
		'transpose im)
    (internal-transpose (data im) (data result) (y-dim im) (x-dim im))
    result))

;;; very fast function shifts the image by this many pixels, wrapping around
;;; at the edges.  ok for destructive mod, too.  :x-shift and :y-shift for
;;; back-compatibility.
(defmethod circular-shift ((im image) &rest keys &key
			   (offset '(0 0))
			   (y-shift (car offset)) (x-shift (cadr offset))
			   (y y-shift) (x x-shift)
			   ->)
  (remf keys :->)
  (with-result ((result ->) im 'apply 'circular-shift im keys)
    (circular-shift (data im) :-> (data result) :x x :y y)
    result))

;;; pastes im into (a copy of) base-im, clipping at borders.
;;; :x-offset :y-offset for back compatibility
(defmethod paste ((im image) (base-im image) &rest keys &key
		  (offset '(0 0))
		  (y-offset (car offset)) (x-offset (cadr offset))
		  (y y-offset) (x x-offset)
		  ->)
  (remf keys :->)
  (with-result ((result ->) base-im
		'apply 'paste im base-im keys)
    (paste (data im) (data base-im) :-> (data result) :y y :x x)
    result))

;;; y-size and x-size for back-compatibility:
(defmethod crop ((im image) &rest keys &key
		 (offset '(0 0)) 
		 (y (car offset)) (x (cadr offset))
		 (y-size (- (y-dim im) y)) (x-size (- (x-dim im) x))
		 (y-dim y-size) (x-dim x-size)
		 ->)
  (remf keys :->)
  (with-result ((result ->) 
		(list :class (clos::class-of im)
		      :dimensions (list y-dim x-dim))
		'apply 'crop im keys)
    (crop (data im) :-> (data result) :x x :y y :x-dim x-dim :y-dim y-dim)
    result))

#|
;;; old version as of 3/93, broken for 1d-images. -DH
(defmethod crop ((im image) &rest keys &key
		 (offset '(0 0)) (dimensions (dimensions im))
		 (y (car offset)) (x (cadr offset))
		 (y-size (- (car dimensions) y)) (x-size (- (cadr dimensions) x))
		 (y-dim y-size) (x-dim x-size)
		 ->)
  (remf keys :->)
  (with-result ((result ->) 
		(list :class (clos::class-of im)
		      :dimensions (list y-dim x-dim))
		'apply 'crop im keys)
    (crop (data im) :-> (data result) :x x :y y :x-dim x-dim :y-dim y-dim)
    result))
|#

#| 8/15/92:
(defmethod crop ((im image) &rest keys
		 &key (y 0) (x 0)
		 (y-size (- (y-dim im) y)) (x-size (- (x-dim im) x))
		 ->)
  (with-result ((result ->) 
		(list :class (clos::class-of im)
		      :dimensions (list y-size x-size))
		'apply 'crop im keys)
    (apply 'crop (data im) :-> (data result) keys)
    result))
|#

(defmethod side-by-side ((im1 image) (im2 image) &key (space 0) ->)
  (setq space (round space))
  (with-result ((result ->)
		(list :class (clos::class-of im1)
		      :dimensions (list (max (y-dim im1) (y-dim im2)) 
					(+ space (x-dim im1) (x-dim im2))))
		'side-by-side im1 im2 :space space)
    (let ((xsize/2 (round (x-dim result) 2)))
      (paste im1 result :y 0 :x 0 :-> result)
      (paste im2 result :y 0 :x (+ space xsize/2) :-> result))
    result))

(defmethod subsample (im &key (start-vector '(0 0)) (step-vector '(1 1))
			 hex-start ((:-> res)))
  (downsample im :start-vector start-vector :step-vector step-vector 
    :hex-start hex-start :-> res))

;;; *** Need to write some C code here:
(defmethod downsample ((im image) &key (start-vector '(0 0)) (step-vector '(1 1))
		       hex-start ->)
  (when (vectorp (data im))
    (error "Downsample does not work on vectors.  Use a 1xN image instead"))
  (let ((y-step (car step-vector))
	(x-step (cadr step-vector))
	(y-start (car start-vector))
	(x-start (cadr start-vector)))
    (declare (fixnum x-step y-step x-start y-start))
    (with-result ((result ->) 
		  (list :class (clos::class-of im)
			:dimensions (list (ceiling (- (y-dim im) y-start) y-step)
					  (ceiling (- (x-dim im) x-start) x-step)))
		  'downsample im :step-vector step-vector 
		  :start-vector start-vector :hex-start hex-start)
      (let ((arr (data im))
	    (y-pos (- y-start y-step)) 
	    (x-pos x-start)
	    (compute 1))
	(declare (type (array single-float (* *)) arr) 
		 (fixnum y-pos x-pos compute))
	(loop-over-image-positions ((rval result)) (y x)
	  (cond ((= x 0) 
		 (setq x-pos x-start) 
		 (incf y-pos y-step)
		 (when hex-start (setq compute (mod (+ 1 y hex-start) 2))))
		(t (incf x-pos x-step)
		   (when hex-start (setq compute (- 1 compute)))))
	  (setf rval (if (= compute 1) (aref arr y-pos x-pos) 0.0))))
      result)))

;;; CAREFUL: result dimensions are non-unique!
(defmethod upsample ((im image) &key (start-vector '(0 0)) (step-vector '(1 1))
		     hex-start ->)
  (when (vectorp (data im))
    (error "Upsample does not work on vectors.  Use a 1xN image instead"))
  (if (not (image-p ->))
      (setq -> (make-image (list (* (y-dim im) (list-y-dim step-vector))
				  (* (x-dim im) (list-x-dim step-vector)))
			    :display-type (display-type im)
			    :name ->))
      (when (not (equal (subsampled-dimensions
			 (dimensions ->) start-vector step-vector)
			(dimensions im)))
	(error "Result argument size is incompatible.")))
  (let ((y-step (list-y-dim step-vector 1))
	(x-step (list-x-dim step-vector 1))
	(y-start (list-y-dim start-vector 0))
	(x-start (list-x-dim start-vector 0)))
    (with-result ((result ->) -> 'upsample im 
			:start-vector start-vector :step-vector step-vector
			:hex-start hex-start)
      (let ((arr (data result))
	    (y-pos (- y-start y-step)) 
	    (x-pos x-start)
	    (compute 1))
	(declare (type (array single-float (* *)) arr)
		 (fixnum y-pos x-pos compute))
	(loop-over-image-positions ((val im))  (y x)
	  (cond ((= x 0) 
		 (setq x-pos x-start)
		 (incf y-pos y-step)
		 (when hex-start (setq compute (mod (+ 1 y hex-start) 2))))
		(t (incf x-pos x-step)
		   (when hex-start (setq compute (- 1 compute)))))
	  (setf (aref arr y-pos x-pos) (if (= compute 1) val 0.0))))
      result)))


;;;; ----------- Internal functions used by display code -------------

;;;; *** Are these really necessary?  They are just wrappers for the
;;;; foreign functions! -EPS

;;; Convert floating point image data to 8bit, using linear transform
;;; (/ (im - pedestal) scale), and clipping values below floor, or
;;; above ceiling.  Redundant with convert-to-8bit!
(defmethod make-8bit-array ((im image) 
			    &key
			    (pedestal (minimum im))
			    (scale (/-0 (range im) 255.0))
			    (floor 0)
			    (ceiling 255))
  (let ((res (make-array (dimensions im) :element-type '(unsigned-byte 8)))
	(new-scale (/-0 1.0 scale)))
    (obv::internal-f-to-8bit (data im) res (total-size im)
			     (float pedestal) (float new-scale)
			     (round floor) (round ceiling))
    res))

;;; This copies into an 8bit array, scaling by the given scale, and clipping.
;;; x-offset and y-offset needed for rescaling images into big pictures
;;; for example, each image in a pyramid is offset into the picture array
;;; *** Is this used any more?  -EPS
(defmethod convert-to-8bit ((im image) 8bit-array pedestal scale
			    &key (ceiling 255) (floor 0) (y-offset 0) (x-offset 0))
  (setq x-offset (round x-offset) y-offset (round y-offset))
  (internal-f-into-8bit (data im) (x-dim im) (y-dim im) 
			8bit-array  (array-dimension 8bit-array 1)
			(float pedestal) (float scale)
			x-offset y-offset
			floor ceiling)
  8bit-array)

(defmethod convert-from-8bit ((im image) 8bit-array)
  (internal-8bit-to-f 8bit-array (data im) (total-size im))
  im)

(defmethod convert-to-32bit ((im image) 32bit-array pedestal scale)
  (internal-f-to-32bit (data im) 32bit-array
		       (total-size im) (float pedestal) (float scale))
  32bit-array)

(defmethod convert-from-32bit ((im image) 32bit-array)
  (internal-32bit-to-f 32bit-array (data im) (total-size im))
  im)


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
