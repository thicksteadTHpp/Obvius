;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: synth.lisp
;;;  Author: Simoncelli and Heeger
;;;  Description: Synthetic image generators
;;;  Creation Date: May, 1988
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)
(export '(make-synthetic-image
	  make-impulse make-ramp
	  make-1-over-r make-r-squared make-atan
	  make-grating make-sin-grating make-saw-tooth-grating make-square-grating
	  make-zone-plate make-pinwheel make-disc make-left make-top 
	  make-soft-threshold-df
	  make-uniform-noise make-uniform-random-image
	  make-gaussian-noise make-gaussian-noise-image
	  make-random-dots make-random-dot-image make-fractal))

;;; The following functions have been roughly optimized to generate
;;; various synthetic images.  The general function make-synthetic-image
;;; generates synthetic images by sampling any 2-D function.

;;; General hints about efficient image generation:
;;; 0) Don't use loop-over-image-pixels unless necessary - it is MUCH slower
;;;    than C code.
;;; 1) Use standard OBVIUS functions where possible (these call C code).
;;; 2) When creating 1D functions, make a ramp first and then call
;;;    point operation code.  Use periodic-point-operation for periodic
;;;    functions, and point-operation otherwise.

;;; Make an empty image, with an impulse in the middle.
(defun make-impulse (dims &key
			  (amplitude 1.0)
			  (origin (mapcar #'(lambda (x) (floor x 2.0)) dims))
			  ->)
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-impulse dims :amplitude amplitude :origin origin)
    (zero! result)
    (setf (iref result (car origin) (cadr origin)) (float amplitude))
    result))

(defun make-ramp (dims &key (orientation 0) (pedestal 0.0) (slope 1.0)
		       (origin (mapcar #'(lambda (x) (/ (1- x) 2.0)) dims))
		       ->)
  "Make a ramp image of the given dimensions.  Pedestal is the value at the
origin of the image.  Slope is measured in change per pixel.  Orientation 
is in radians relative to the +X axis."
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-ramp dims :orientation orientation :slope slope
		:pedestal pedestal :origin origin)
    (let* ((x-slope (* slope (cos orientation))) ;actually, these are
	   (y-slope (- (* slope (sin orientation))))     ;negative slopes.
	   (ul-corner-value (+ (* (list-y-dim origin) (- y-slope))
			       (* (list-x-dim origin) (- x-slope))
			       pedestal)))
      (internal-make-ramp (data result) (total-size result) (x-dim result)
			  (float* ul-corner-value) (float* y-slope) (float* x-slope)))
    result))

;;; For maximal speed, we call the C functions directly...
(defun make-r-squared (dims &key
			    (origin (mapcar #'(lambda (x) (/ (1- x) 2.0)) dims))
			    ->)
  "Make an image with the given dimensions.  Each pixel will contain the
squared distance (in pixels) from the origin of the image."
  (with-result ((result ->) (list :class 'image :dimensions dims)
		'make-r-squared dims :origin origin)
    (with-local-viewables 
	((x-ramp (make-ramp dims :orientation 0.0 :slope 1.0 :origin origin))
	 (y-ramp (make-ramp dims :orientation (/ pi 2) :slope 1.0 :origin origin)))
      (internal-sum-of-squares (data x-ramp) (data y-ramp) (data result) (total-size result)))
    result))

;;; singularity at zero set to zero-val
(defun make-1-over-r (dims &key
			   (origin (mapcar #'(lambda (x) (/ (1- x) 2)) dims))
			   (exponent 1.0) (amplitude 1.0) (zero-val 0.0) ->)
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-1-over-r :origin origin :exponent exponent
		:amplitude amplitude :zero-val zero-val)
    (make-r-squared (dimensions result) :origin origin :-> result)
    (internal-pow-sc (data result) (data result) (total-size result)
		  (float (/ exponent 2.0)))
    (internal-sc-div (data result) (data result) (total-size result)
		     (float amplitude) (float zero-val))
    (when (and (integerp (car origin)) (integerp (cadr origin)))
      (setf (aref (data result) (car origin) (cadr origin)) (float zero-val)))
    result))

;;; Make an angle (arc-tangent) image (ie - each pixel contains its
;;; angular coordinate).  From -pi/2 to pi/2.  *** THis needs a
;;; branch-center keyword arg with default 0.0 -EPS
;;; **** ALso, use origin keyword of make-ramp.
(defun make-atan (dims &key
		       (origin (mapcar #'(lambda (x) (/ (1- x) 2)) dims))
		       (phase 0.0) ->)
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		dims 'make-atan :origin origin :phase phase)
    (with-local-viewables
	((x-center (- (/ (1- (x-dim result)) 2.0) (list-x-dim origin)))
	 (y-center (- (list-y-dim origin) (/ (1- (y-dim result)) 2.0)))
	 (x-pedestal (+ (* x-center (cos phase)) (* y-center (sin phase))))
	 (y-pedestal (- (* y-center (cos phase)) (* x-center (sin phase))))
	 (x-ramp (make-ramp dims :orientation phase :slope 1.0
			    :pedestal x-pedestal))
	 (y-ramp (make-ramp dims :orientation (+ phase (/ pi 2)) :slope 1.0
			    :pedestal y-pedestal)))
      (internal-phase (data x-ramp) (data y-ramp) (data result) (total-size result))
      (when (every 'integerp origin)
	(setf (iref result (car origin) (cadr origin)) 0.0)))
    result))

;;; Basically, applies a 1D discrete-function to a ramp image.  ***
;;; Not very useful: args are a bit strange...
(defun make-grating (dims func period &key (orientation 0.0) (phase 0.0) ->)
  "Make an image of a 1D periodic function.  FUNC must be a valid argument
to funcall.  PERIOD indicates the desired period of the function in pixels.
FUNC will be evaluated at (get-default 'discrete-function :size) points
between 0 and PERIOD.  Linear interpolation will be used between these points.
PHASE should be in the interval [0,PERIOD]."
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-grating dims func period :orientation orientation :phase phase)
    (make-ramp dims :orientation orientation :slope 1.0
	       :pedestal phase :-> result)
    (periodic-point-operation
     result func period
     :binsize (/-0 period (get-default 'discrete-function :size) 1)
     :-> result)))
  
;;; make saw-tooth at any orientation by rotating coordinate frame
;;; period is in pixels.  orientation is angle away from positive x
;;; axis.  phase is in pixels - between 0 and period - relative to
;;; image center.
(defun make-saw-tooth-grating (dims &key (period 8.0) (orientation 0.0) (phase 0.0) ->)
  (with-result ((result ->)
		(list :class 'image :dimensions dims) 
		'make-saw-tooth-grating :period period
		:orientation orientation :phase phase)
    (make-ramp dims :orientation orientation :slope 1.0
	       :pedestal (/ phase period) :-> result)
    (mod. result period :-> result)))

;;; Transition-width is in pixels, nil indicates a simple threshold.
(defun make-square-grating (dims &key (period 8.0) (orientation 0.0) (phase 0.0) ->
				 (duty-cycle 0.5) (transition-width 1.0))
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-square-grating dims :period period :orientation orientation
		:phase phase)
    (make-ramp dims :orientation orientation :slope (/ 1.0 period)
	       :pedestal (/ phase period) :-> result)
    (let* ((t1 (/ (- 1.0 duty-cycle) 2.0))
	   (t2 (- 1.0 t1))
	   (w (and transition-width (/-0 (/ transition-width 2) period))))
      (periodic-point-operation
       result 
       (if w
	   #'(lambda (x) (cond ((< x (- t1 w)) 0.0)
			       ((< x (+ t1 w))
				(+ 0.5 (* 0.5 (cos (* (- (+ t1 w) x) (/ pi 2 w))))))
			       ((< x (- t2 w)) 1.0)
			       ((< x (+ t2 w))
				(+ 0.5 (* 0.5 (cos (* (- x (- t2 w)) (/ pi 2 w))))))
			       (t 0.0)))
	   #'(lambda (x) (cond ((< x t1) 0.0)
			       ((< x t2) 1.0)
			       (t 0.0))))
       1.0
       :binsize (/-0 1.0 (obv::get-default 'discrete-function :size) 1)
       :-> result))))

(defun make-sin-grating (dims &key (period 8.0) (orientation 0.0) 
			      (phase 0.0) (amplitude 1.0) (dc 0.0)
			      frequency x-freq y-freq ->)
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-sin-grating dims :period period :orientation orientation
		:dc dc :amplitude amplitude :phase phase)
    (cond (frequency t)
	  ((and x-freq y-freq)
	   (setq frequency (sqrt (+ (sqr x-freq) (sqr y-freq))))
	   (setq orientation (atan-0 y-freq x-freq 0.0)))
	  (t (setq frequency (/-0 2-pi period))))
    (make-ramp dims :orientation orientation :slope frequency
	       :pedestal phase :-> result)
    (periodic-point-operation
     result #'(lambda (x) (+ dc (* amplitude (sin x)))) 2-pi
     :binsize (/-0 2-pi (get-default 'discrete-function :size) 1)
     :-> result)))

(defun make-zone-plate (dims &key (amplitude 1.0) (phase 0.0) ->)
  "Make a zone plate (cosine of r-squared) with given amplitude and phase.
Phase is in radians and refers to the origin."
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-zone-plate dims :amplitude amplitude :phase phase)
    (let* ((cx (/ pi (cadr dims))))
      (make-r-squared (dimensions result) :-> result)
      (periodic-point-operation
       result
       #'(lambda (x) (* amplitude (cos (+ (* cx x) phase))))
       (/ 2-pi cx)
       :binsize (/-0 (/ 2-pi cx) (get-default 'discrete-function :size) 1)
       :-> result))))

;;; Should write a faster one for integer random values.
;;; This is 26 times faster than using loop-over-image-pixels on a SPARC.
;;; *** Need a :range keyword.
(defun make-uniform-noise (dims &key ->)
  (with-result ((result ->) 
		(list :class 'image :dimensions dims)
		'make-uniform-noise dims)
    (internal-random (data result) (total-size result) 
                     (mod (random (get-universal-time)) most-positive-fixnum))
    result))

;;; For back-compatibility
(defun make-uniform-random-image (dims &rest keys)
  (warn "Outdated function: use MAKE-UNIFORM-NOISE instead.")
  (apply 'make-uniform-noise dims keys))

(defun make-random-dots (dims &key (density 0.25) ((:-> res)))
  (with-local-viewables ((im (make-uniform-noise dims)))
    (less-than im density :-> res)))

(defun make-random-dot-image (dims &rest keys)
  (warn "Outdated function: use MAKE-RANDOM-DOTS instead.")
  (apply 'make-random-dots dims keys))

;;; Compute gaussian distribution by adding together uniform
;;; distributions.  Precision of distribution is determined by
;;; iterations.  With 6 iterations, this is a fine approximation.
(defun make-gaussian-noise (dims &key -> (mean 0.0) (variance 1.0) (iterations 6))
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-gaussian-noise dims :mean mean :variance variance)
    (setq iterations (round iterations))
    (with-local-viewables ((temp (similar result))
			   (stdev (sqrt (* (/ 12 iterations) variance))))
      (internal-const (data result) (/ iterations -2.0) (total-size result))
      (loop for i from 0 below iterations do 
	    (add (make-uniform-noise dims :-> temp)
		 result :-> result))
      (mul stdev result :-> result))
      (add mean result :-> result)
    result))

(defun make-gaussian-noise-image (dims &rest keys)
  (warn "Outdated function: use MAKE-GAUSSIAN-NOISE instead.")
  (apply 'make-gaussian-noise dims keys))

;;; *** Rewrite using ramp and soft-thresholds.
(defun make-top (dims &key (y (/ (car dims) 2.0)) ->)
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-top dims)
    (loop-over-image-positions ((val result)) (y-pos x-pos)
      (setf val (the single-float (if (< y-pos y) 1.0 0.0))))
    result))

(defun make-left (dims &key (x (/ (cadr dims) 2.0)) ->)
  (with-result ((result ->) 
		(list :class 'image :dimensions dims)
		'make-left dims)
    (loop-over-image-positions ((val result)) (y-pos x-pos)
      (setf val (the single-float (if (< x-pos x) 1.0 0.0))))
    result))

;;; *** SHould allow transition-width = nil.
(defun make-disc (dims &key ->
		       (radius (* 0.25 (apply #'min dims)))
		       (origin (mapcar #'(lambda (x) (/ (1- x) 2.0)) dims))
		       (transition-width 2))
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-disc dims :radius radius :origin origin
		:transition-width transition-width)
    (with-local-viewables 
	((x-ramp (make-ramp dims :orientation 0.0 :slope 1.0 :origin origin))
	 (y-ramp (make-ramp dims :orientation (/ pi 2) :slope 1.0 :origin origin)))
      (internal-square (data x-ramp) (data x-ramp) (total-size x-ramp))
      (internal-square (data y-ramp) (data y-ramp) (total-size y-ramp))
      (internal-add (data x-ramp) (data y-ramp) (data result) (total-size result)))
    (internal-sqrt (data result) (data result) (total-size result))
    (point-operation result (make-soft-threshold-df radius transition-width :invert t)
		     :-> result)))

(defun make-soft-threshold-df (cutoff width &key (below-val 0.0) (above-val 1.0) invert ->)
  (when invert (rotatef above-val below-val))
  (let ((min-x (- cutoff (/ width 2.0)))
	(max-x (+ cutoff (/ width 2.0)))
	(scale (* 0.5 (- above-val below-val))))
    (declare (single-float min-x max-x scale))
    (make-discrete-function 
     #'(lambda (x)
	 (declare (single-float x))
	 (cond ((<= x min-x) below-val)
	       ((>= x max-x) above-val)
	       (t (+ below-val
		     (* scale (1+ (cos (* (- max-x x) (/ pi width)))))))))
     (- cutoff width) (+ cutoff width)
     :-> ->)))

;;; *** Ugly function!  What is it for?  Needs to be sped up!
(defun make-pinwheel (dims &key ->)
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-pinwheel dims)
    (let* ((ysize (car dims))
	   (xsize (cadr dims))
	   (xsize/2 (* 0.5 xsize))
	   (ysize/2 (* 0.5 ysize))
	   (slope (/ ysize xsize))
	   (line 0.0))
      (loop-over-image-positions ((val result)) (y x)
	(setq line (* slope x))
	(setf val (if (or (and (> y ysize/2) (> x xsize/2) (> line y))
			  (and (> y ysize/2) (< x xsize/2) (> y (- ysize line)))
			  (and (< y ysize/2) (< x xsize/2) (< line y))
			  (and (< y ysize/2) (> x xsize/2) (< y (- ysize line))))
		      1.0 0.0))))
    result))

(defun make-fractal (dims &key (fractal-dimension 2.5) ->)
  (with-result ((result ->)
		(list :class 'image :dimensions dims)
		'make-fractal :fractal-dimension fractal-dimension)
    (with-local-viewables ((rand (make-gaussian-noise dims))
			   (transform (fft rand)))
      (let* ((pow (- 4.0 fractal-dimension))
	     (xshift (floor (x-dim rand) 2))
	     (yshift (floor (y-dim rand) 2))
	     (real-part (real-part transform))
	     (imaginary-part (imaginary-part transform)))
	(with-local-viewables ((1-over-r (make-1-over-r dims :exponent pow)))
	  (circular-shift 1-over-r :y yshift :x xshift :-> 1-over-r)
	  (mul real-part 1-over-r :-> real-part)
	  (mul imaginary-part 1-over-r :-> imaginary-part)
	  (real-part (fft transform :-> transform) :-> result))))
    result))


;;; General synthetic image generating facility.  MAKE-SYNTHETIC-IMAGE
;;; creates a synthetic image by computing the function (func) over
;;; the ranges x-range and y-range sampled at a density determined by
;;; the dimensions of the image.  func should take two float arguments
;;; (y x) and return a float.  NOTE: this does not use
;;; loop-over-image-positions because we don't want to do unecessary
;;; multiplications to compute the args to func.  See below for examples.
;;; *** someday, try putting the declarations back in.  For now,
;;; they cause bus errors.  - DH and ES
(defmethod make-synthetic-image ((dims list) the-function
				 &key 
				 ((:-> res))
				 (x-range '(-1.0 1.0))
				 (y-range x-range))
  (with-result ((result res)
		(list :class 'image :dimensions dims)
		'make-synthetic-image the-function :x-range x-range :y-range y-range)
    (when (symbolp the-function) (setq the-function (symbol-function the-function)))
    (when (not (compiled-function-p the-function))
      (warn "Make-synthetic-image called with non-compiled function.  Be prepared to wait!"))
    (let* ((arr (parent-array (data result)))
	   (y-inc (float (/-0 (- (apply #'- y-range)) (1- (y-dim result)) 0.0)))
	   (x-inc (float (/-0 (- (apply #'- x-range)) (1- (x-dim result)) 0.0)))
	   (i-max (x-dim result))	;Doing this makes it much faster
	   (pos-min (displaced-start (data result)))
	   (pos-max (+ (total-size result) pos-min)))
      (declare (type (simple-array single-float (*)) arr))
      ;; (declare (float y-inc x-inc) (fixnum i-max pos-max pos-min))
      (do ((pos pos-min)
	   (y (float (car y-range)) (+ y y-inc)))
	  ((>= pos pos-max))
	;; (declare (fixnum pos) (float y))
	(do ((i 0 (1+ i))
	     (x (float (car x-range)) (+ x x-inc)))
	    ((>= i i-max))
	  ;; (declare (fixnum i) (float x))
	  (setf (aref arr pos) (float (funcall the-function y x)))
	  (setq pos (1+ pos)))))
    result))	;This is slightly faster than (incf pos)

(defmethod make-synthetic-image ((dim number) the-function
				 &key 
				 ((:-> res))
				 (x-range '(-1.0 1.0)))
  (with-result ((result res)
		(list :class 'image :dimensions dim)
		'make-synthetic-image the-function :x-range x-range)
    (when (symbolp the-function) (setq the-function (symbol-function the-function)))
    (when (not (compiled-function-p the-function))
      (warn "Make-synthetic-image called with non-compiled function.  Be prepared to wait!"))
    (let* ((arr (parent-array (data result)))
	   (x-inc (float (/-0 (- (apply #'- x-range)) (1- (x-dim result)) 0.0)))
	   (pos-min (displaced-start (data result)))
	   (pos-max (+ (total-size result) pos-min)))
      (declare (type (simple-array single-float (*)) arr))
      ;; (declare (float x-inc) (fixnum i-max pos-max pos-min))
      (do ((i pos-min (1+ i))
	   (x (float (car x-range)) (+ x x-inc)))
	  ((>= i pos-max))
	;; (declare (fixnum i) (float x))
	(setf (aref arr i) (float (funcall the-function x)))))
    result))

#|
;;; ---------------- Example Synthetic Image Functions ---------------
;;; The func argument to make-synthetic-image must be something that
;;; funcall can be applied to.  It can assume that it will recieve two
;;; floating point arguments (see the declarations in the functions
;;; defined below).  It must return a floating point result.  Some
;;; examples of calls to make-synthetic-image are:
;;;     (make-synthetic-image '(32 32) '(lambda (y x) (/ 1.0 (+ 1.0 (* x x) (* y y)))))
;;; or
;;;     (make-synthetic-image '(32 32) 'expt :x-range '(0.0 1.0))
;;; Any of the functions defined below can be passed to
;;; make-synthetic-image preceded by a ' or #'.  For example:
;;;     (make-synthetic-image '(32 32) 'disc :x-range '(-1.5 1.5) :y-range '(-4.0 4.0))
;;; creates a filled ellipse, and
;;;     (make-synthetic-image '(32 32) #'squ :y-range '(2.0 4.0))
;;; creates an off-center rectangle.  Where it is reasonable, all
;;; functions return values between 0.0 and 1.0

;;; Unit disc function
(defun disc (y x)
  (declare (single-float y x))
  (if (< (+ (* x x) (* y y)) 1.0)  1.0  0.0))

;;; Unit square function
(defun squ (y x)
  (declare (single-float y x))
  (if (and (< -1.0 x 1.0) (< -1.0 y 1.0))  1.0  0.0))

;;; Gaussian blob, variance = 1.0
(defun gaussian (y x)
 (declare (single-float x y))
 (exp (/ (+ (* x x) (* y y)) -2.0)))
 
;;; Rotate function 90 degrees
(defun rot1 (func)
  #'(lambda (y x) (declare (single-float y x)) (funcall func x y)))

;;; Returns a lambda expression representing the given function rotated by
;;; an arbitrary orientation.
(defmacro rot (func orientation)
  (let* ((orientation-val (eval orientation))
	 (s (sin orientation-val))
	 (c (cos orientation-val))
	 (x-var (gensym))
	 (y-var (gensym)))
    (declare (single-float s c))
    `(list 'lambda '(,x-var ,y-var) 
      '(declare (single-float ,x-var ,y-var))
      '(funcall ,func (- (* ,c ,x-var) (* ,s ,y-var))
	(+ (* ,c ,y-var) (* ,s ,x-var))))))

(make-synthetic-image '(256 256) (rot #'(lambda (y x) x) (/ pi 6)))
|#


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
