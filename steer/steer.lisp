;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: steer.lisp
;;;  Author: David Heeger and Bill Freeman
;;;  Description: steerable filters and quadrature-steerable filters
;;;  Creation Date: summer '89
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revisions history:

;;; Feb. 2, 1991  Freeman added steer-filter-list.
;;; March 26, 1991  freeman added make-avg-steerable-filters and *avg-steerable-filters*.
;;; July 11, 1991  freeman commented-out *avg-steerable-filters*, because I don't use
;;;   it and it slowed-down load-in.
;;; Oct. 8, 1991  Freeman allowed make-quadrature... to take existing even and odd
;;;   steerable basis.

;;; Sept 16, 1992 Simoncelli ported from obvius-1.2 to obvius-2.2.  Changes:
;;;   Added :startx key to sample-1d.
;;;   Added my-gauss to bottleneck this fn.
;;;   Removed steer-even, steer-odd, sum-even, sum-odd
;;;   Wrote generic-steer method: Steers any set of filters that are
;;;      separable or directional derivatives of a common base filter.  It
;;;      is slightly slower than the hand-coded version.
;;;   Changed names magnitude -> directional-magnitude,
;;;      squared-magnitude -> directional-energy, complex-phase -> directional-phase.
;;;   Wrote separable-to-directional and directional-to-separable conversion functions, but
;;;      these can't be used unless we make a new class or add a basis-type slot.
;;;   **** Would be nice to provice the following:
;;;      (make-directional-steerable-filters dims :order :base-function :delx :dely)

(in-package :obvius)
(export '(make-g1-steerable-filters
	  make-g2-steerable-filters make-h2-steerable-filters
	  make-g4-steerable-filters make-h4-steerable-filters
	  steerable-basis steerable-basis-p separable-steerable-basis-p
	  make-steerable-basis 
	  filter-list image-list order
	  dimensions x-dim y-dim minimum maximum range
	  steer sum
	  quadrature-steerable-basis make-quadrature-steerable-basis
	  even-steerable-basis odd-steerable-basis
	  ;;steer-even steer-odd sum-even sum-odd
	  directional-magnitude directional-energy directional-phase
	  average-energy steer-filter-list
	  sample-1d sample-2d))

;(obv-require :matrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STEERABLE BASIS OBJECT

;;; *** Why do we need to hang on to the filter-list?  It is only used
;;; to determine if the basis is separable.  So either make a new
;;; basis-type slot, or a new object.
(def-simple-class steerable-basis (image-sequence)
  (filter-list)
  (:default-initargs :display-type 'pasteup))

(defmacro steerable-basis-p (obj)
  `(typep ,obj 'steerable-basis))

(defmacro separable-steerable-basis-p (obj)
  `(and (typep ,obj 'steerable-basis)
        (every #'(lambda (x) (separable-filter-p x))
	       (filter-list ,obj))))
		      
(defun make-steerable-basis
    (image &rest initargs
	   &key 
	   (filter-list (make-g2-steerable-filters))
	   image-list
	   display-type name ->)
  (declare (ignore name display-type))
  (unless (image-p image) (error "First arg must be an image."))
  (when -> (setf (getf initargs :name) ->))
  (unless image-list
    (setq image-list (mapcar #'(lambda (f) (apply-filter f image)) filter-list)))
  (with-result ((result nil)
		`(:class steerable-basis
		  :image-list ,image-list
		  :filter-list ,filter-list
		  ,@initargs)
		'apply 'make-steerable-basis image initargs)
    result))

(defmethod inferiors-of ((steerable steerable-basis))
  (image-list steerable))

(defmethod order ((steerable steerable-basis))
  ;(1- (length (filter-list steerable)))
  (1- (sequence-length steerable)))

;;; *** Should we define this on image-sequence?
(defmethod sum ((steerable steerable-basis) &key ->)
  (with-result ((res ->) (aref (data steerable) 0 0)
		'sum steerable)
    (loop for im in (image-list steerable) do
	  (add im res :-> res))
    res))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; NEW GENERIC STEERING CODE.  Written 9/92 by EPS.

;;; Interpolate responses of filters at the given angle.  ANGLE can be
;;; a number or an image.  Call hand-coded more efficient versions for
;;; specific order filters.  Otherwise, call generic version.

;;; Assumes basis contains either separable derivative images (x^N,
;;; x^N-1y, ... y^N), or directional derivative images oriented at
;;; (npi/N), n=0..N.  ANGLE can be a number or an image.
(defmethod steer ((basis steerable-basis) angle &key ->)
  (with-result ((result ->)
		(aref (data basis) 0 0)
		'steer basis angle)
    (with-local-viewables
	((sep-steer-vect (separable-to-directional-weights angle (order basis))))
      (if (or (separable-steerable-basis-p basis) (= (order basis) 1))
	  (dot-product basis sep-steer-vect :-> result)
	  (with-local-viewables
	      ((dir-to-sep (directional-to-separable-mtx (order basis)))
	       (dir-steer-vect (matrix-mul sep-steer-vect dir-to-sep)))
	    (dot-product basis dir-steer-vect :-> result))))))

;;; a steering method for filters.  This is more efficient sometimes than 
;;; applying each filter to an image, and then steering the images (if you
;;; only have one angle to which you are going to steer the results).
(defun steer-filter-list (filter-list angle &key ->)
  (with-result ((result ->)
		`(:class filter		;make sure result is NOT a separable-filter
		  :kernel ,(similar (kernel (car filter-list)))
		  :step-vector ,(step-vector (car filter-list))
		  :start-vector ,(start-vector (car filter-list)))
		'steer-filter-list)
    (with-local-viewables
	((order (1- (length filter-list)))
	 (filt-seq (make-viewable-sequence filter-list))
	 (sep-steer-vect (separable-to-directional-weights angle order)))
      (if (or (every #'separable-filter-p filter-list) (= order 1))
	  (dot-product filt-seq sep-steer-vect :-> result)
	  (with-local-viewables
	      ((dir-to-sep (directional-to-separable-mtx order))
	       (dir-steer-vect (matrix-mul sep-steer-vect dir-to-sep)))
	    (dot-product filt-seq dir-steer-vect :-> result))))
    result))

#|
;;; Default Steer method for images:  DO we really need this?  -EPS
(defmethod steer ((im image) angle &key ->)
  (with-result ((res ->)
		im
		'steer im angle)
    (copy im :-> res)))
|#

;;; *** would be nice to make these methods.  Would have to add a
;;; basis-type slot, or make new classes.
(defun separable-to-directional (basis &key ->)
  (unless (steerable-basis-p basis)
    (error "Argument is not a steerable basis: %a" basis))
  (with-result ((res ->)
		basis
		'separable-to-directional basis)
    (matrix-mul (separable-to-directional-mtx (order basis))
		basis
		:-> res)))

(defun directional-to-separable (basis &key ->)
  (unless (steerable-basis-p basis)
    (error "Argument is not a steerable basis: %a" basis))
  (with-result ((res ->)
		basis
		'directional-to-separable basis)
    (matrix-mul (directional-to-separable-mtx (order basis))
		basis
		:-> res)))

;;; Compute an (order+1)x(order+1) matrix, that converts from
;;; directional basis to a separable one.  
(defun directional-to-separable-mtx (order)
  (matrix-inverse (separable-to-directional-mtx order)))

;;; Compute separable-to-directional weights for a set of
;;; uniformly distributed directions, and paste them into the rows of
;;; a square matrix.
(defun separable-to-directional-mtx (order)
  (let ((res (make-array (list (1+ order) (1+ order))
			 :element-type 'single-float)))
    (loop for index from 0 to order
	  for angle = (* index (/ pi (1+ order)))
	  for row = (displaced-row index res)
	  do
	  (separable-to-directional-weights angle order :-> row))
    res))

;;; Computes the coefficients in the polynomial (x cos + y sin)^order.
;;; *** Currently hardwired for 2D.  Should be generalized.
(defmethod separable-to-directional-weights
    ((angle number) order &key
     ((:-> res) (make-array (1+ order) :element-type 'single-float)))
  (unless (and (typep res '(array single-float *))
	       (= (total-size res) (1+ order)))
    (error "Result arg must be a single-float array of size ~A" (1+ order)))
  (let* ((cos (cos angle))
	 (sin (sin angle))		;negated below!
	 (order-factorial  (factorial order)))
    (setq sin (- sin))
    (loop for index from (- order 1) downto 0
	  for c^n = cos then (* c^n cos)
	  do (setf (aref res index) c^n))
    (loop for index from 1 to order
	  for weight = (/ order-factorial 
			  (* (factorial index) (factorial (- order index))))
	  for s^n = sin then (* s^n sin)
	  do
	  (if (< index order)
	      (setf (aref res index) (* weight (aref res index) s^n))
	      (setf (aref res index) s^n)))
    res))

(defmethod separable-to-directional-weights ((angle image) order &key ->)
  (with-result ((res ->)
		`(:class image-sequence
		  :length ,(1+ order)
		  :dimensions ,(dimensions angle))
		'separable-to-directional-weights angle order)
    (with-local-viewables ((cos (cos. angle))
			   (sin (sin. angle)) ;this is negated below!
			   (order-factorial  (factorial order))
			   (temp (similar cos)))
      (-. sin :-> sin)
      (loop for index from (- order 1) downto 0
	    for c^n = (copy cos :-> (aref (data res) 0 index))
	    then (*. c^n cos :-> (aref (data res) 0 index)))
      (loop for index from 1 to order
	    for weight = (/ order-factorial 
			    (* (factorial index) (factorial (- order index))))
	    for s^n = sin then (*. s^n sin :-> temp)
	    do
	    (if (< index order)
		(*. weight (aref (data res) 0 index) s^n :-> (aref (data res) 0 index))
		(*. weight s^n :-> (aref (data res) 0 index))))
      res)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; QUADRATURE STEERABLE BASIS OBJECT

(DEF-simple-class quadrature-steerable-basis (viewable)
  (even-steerable-basis
   odd-steerable-basis)
  (:default-initargs :display-type nil))

;;; note: no display type for these guys for now
;;; look at the even- or odd-steerable-basis inferiors 
;;; to see the basis images.
;;; "image" can be an image, or a list of even and odd steerable bases.
(defun make-quadrature-steerable-basis 
    (image &rest initargs
	   &key (even-filters (make-g2-steerable-filters))
	   (odd-filters (make-h2-steerable-filters))
	   name ->)
  (declare (ignore name))
  (when -> (setf (getf initargs :name) ->))
  (let* ((even-steerable (if (listp image)
			     (car image)
			     (make-steerable-basis image :filter-list even-filters)))
	 (odd-steerable (if (listp image)
			    (cadr image)
			    (make-steerable-basis image :filter-list odd-filters))))
    (with-result ((result nil)
		  `(:class quadrature-steerable-basis
		    :even-steerable-basis ,even-steerable
		    :odd-steerable-basis ,odd-steerable
		    ,@initargs)
		  'apply 'make-quadrature-steerable-basis initargs)
      result)))

(defmethod initialize-instance ((basis quadrature-steerable-basis) &rest initargs)
  (let ((even (getf initargs :even-steerable-basis))
	(odd  (getf initargs :odd-steerable-basis)))
    (unless (and even odd) (error "must provide even and odd steerable bases"))
    (call-next-method)
    (push basis (superiors-of even))
    (push basis (superiors-of odd))))

(defmethod inferiors-of ((basis quadrature-steerable-basis))
  (list (even-steerable-basis basis) (odd-steerable-basis basis)))

(defmethod dimensions ((steerable quadrature-steerable-basis))
  (dimensions (even-steerable-basis steerable)))

(defmethod order ((steerable quadrature-steerable-basis))
  (min (order (even-steerable-basis steerable))
       (order (odd-steerable-basis steerable))))

(defmethod x-dim ((steerable quadrature-steerable-basis))
  (x-dim (even-steerable-basis steerable)))

(defmethod y-dim ((steerable quadrature-steerable-basis))
  (y-dim (even-steerable-basis steerable)))

#| These seem unnecessary:
(defmethod steer-even ((steerable quadrature-steerable-basis) angle &key ->)
  (with-result ((res ->) (aref (data steerable) 0 0)
		'steer-even steerable angle)
    (steer (even-steerable-basis steerable) angle :-> res)))

(defmethod steer-odd ((steerable quadrature-steerable-basis) angle &key ->)
  (with-result ((res ->) (aref (data steerable) 0 0)
		'steer-odd steerable angle)
    (steer (odd-steerable-basis steerable) angle :-> res)))
|#

(defmethod steer ((steerable quadrature-steerable-basis) angle &key ->)
  (with-result ((result ->)
		(list :class 'complex-image
		      :image-list
		      (list (steer (odd-steerable-basis steerable) angle) 
			    (steer (even-steerable-basis steerable) angle)))
		'steer steerable angle)
    result))

#| Unnecessary
(defmethod sum-even ((steerable quadrature-steerable-basis) &key ((:-> result)))
  (sum (even-steerable-basis steerable) :-> result))

(defmethod sum-odd ((steerable quadrature-steerable-basis) &key ((:-> result)))
  (sum (odd-steerable-basis steerable) :-> result))
|#
    
(defmethod directional-magnitude ((steerable quadrature-steerable-basis) angle  &key ->)
  (with-result ((result ->) (aref (data (even-steerable-basis steerable)) 0 0)
		'directional-magnitude steerable angle)
    (with-local-viewables ((pair (steer steerable angle)))
      (magnitude pair :-> result))))

(defmethod directional-energy ((steerable quadrature-steerable-basis) angle &key ->)
  (with-result ((result ->) (aref (data (even-steerable-basis steerable)) 0 0)
		'directional-energy steerable angle)
    (with-local-viewables ((pair (steer steerable angle)))
      (square-magnitude pair :-> result))))

(defmethod directional-phase ((steerable quadrature-steerable-basis) angle &key ->)
  (with-result ((result ->) (aref (data (even-steerable-basis steerable)) 0 0)
		'directional-phase steerable angle)
    (with-local-viewables ((pair (steer steerable angle)))
      (complex-phase pair :-> result))))

;;; gives the integral over all angles of the orientated energy
(defmethod average-energy  ((qsb quadrature-steerable-basis) &key ->)
  (with-result ((result ->) (aref (data qsb) 0 0)
		'average-energy qsb)
    (cond ((and (= (order (even-steerable-basis qsb)) 2) 
		(= (order (odd-steerable-basis qsb)) 3)) 
	   (get-average-energy-2-3 (image-list (even-steerable-basis qsb))
				   (image-list (odd-steerable-basis qsb)) result))
	  (t (error "Only written for 2nd order basis")))
    result))

(defun get-average-energy-2-3 (even-list odd-list result)
  (with-local-viewables ((a (mul (nth 2 even-list) (nth 2 even-list)))
			 (b (mul (nth 0 even-list) (nth 0 even-list)))
			 (c (add a b))
			 (total (mul c 0.375)))  
    (mul (nth 0 odd-list) (nth 0 odd-list) :-> a)
    (mul (nth 3 odd-list) (nth 3 odd-list) :-> b)
    (add a b :-> c)
    (mul c 0.3125 :-> a)
    (add a total :-> total)
    (mul (nth 1 odd-list) (nth 1 odd-list) :-> a)
    (mul (nth 2 odd-list) (nth 2 odd-list) :-> b)
    (add a b :-> c)
    (mul c 0.5625 :-> a)
    (add a total :-> total)
    (mul (nth 0 odd-list) (nth 2 odd-list) :-> a)
    (mul (nth 1 odd-list) (nth 3 odd-list) :-> b)
    (add a b :-> c)
    (mul c 0.375 :-> a)
    (add a total :-> total)
    (mul (nth 1 even-list) (nth 1 even-list) :-> a)
    (mul a 0.5 :-> b)
    (add b total :-> total)
    (mul (nth 0 even-list) (nth 2 even-list) :-> a)
    (mul a 0.25 :-> b)
    (add b total :-> result)))

#|
;;; *** Bill, please write this ***  
(defmethod total-energy ((steerable quadrature-steerable-basis) &key ->)
  (with-result ((result ->) (aref (data steerable) 0 0)
		      'total-energy steerable)
    stuff))

;;; *** we also need to write these

(defmethod energy-vs-angle ((steerable quadrature-steerable-basis) j i
			    &key ->)
  ;; returns a discrete function
  )

(defmethod energy-peak ((steerable quadrature-steerable-basis)
			&key ->)
  ;; returns image-pair with vector-field display-type
  )
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DEFAULT STEERABLE FILTERS
;;; 2nd derivs of Gaussians 
;;; and their Hilbert transforms.

;;; sample a function in 2-d and make a filter out of the result.
(defun sample-2d (function  &key (y-window-size 9) (x-window-size 9) 
			    (delx 1.0) (dely 1.0)
			    (startx (- (* (1- x-window-size) delx 0.5)))
			    (starty (- (* (1- y-window-size) dely 0.5))))
  (let ((kernel (make-array (list y-window-size x-window-size) :element-type 'single-float)))
    (loop for j from 0 below y-window-size
	  for y = starty then (+ y dely)
 	  do
 	  (loop for i from 0 below x-window-size
		for x = startx then (+ x delx)
 		do
 		(setf (aref kernel j i) (float (funcall function y x )))))
    kernel))

;;; return a vector of samples of the function. window-size is
;;; dimension of vector returned.  delx is the space between samples.
;;; startx is the position of the first sample.
;;; *** Args not consistent with make-synthetic-image or make-discrete-function.
(defun sample-1d (function &key (window-size 9) (delx 1.0)
			   (startx (- (* (1- window-size) delx 0.5))))
  (let ((kernel (make-array (list window-size) :element-type 'single-float)))
    (loop for i from 0 below window-size
	  for x = startx then (+ x delx)
	  do
	  (setf (aref kernel i) (funcall function x)))
    kernel))

;;; Standard gaussian: **** NOTE: this is not univariate!
(defun my-gauss (x) (exp (* x x -1.0)))

(defun g2.gauss (x) (my-gauss x))

;;; 2nd derv of gaussian filters.   The polynomial functions:
;;; What is the 0.92132?
(defun g2.poly (x)
  (* (my-gauss x)
     0.92132
     (- (* 2.0 (expt x 2)) 1.0)))

;;; What is the normalizer?
(defun g2.diag (x)
  (* (my-gauss x)
     1.35744
     x))

;;; hilbert transforms of 2nd deriv of gaussian filters.  Polynomial forms.
(defun h3.hila (x)
  (* (my-gauss x)
     0.97796 
     (+ (* -2.2544 x)
	(expt x 3))))

(defun h3.hilb (x) (my-gauss x))

(defun h3.minusx (x)
  (* (my-gauss x)
     x))

(defun h3.minusy (x)
  (* (my-gauss x)
     0.97796
     (+ (expt x 2) -0.751465)))

(defun make-g2-steerable-filters (&key (edge-handler :reflect1))
  (let ((n2dgpoly (make-filter (sample-1d 'g2.poly :window-size 9 :delx 0.67)))
	(n2dggauss (make-filter (sample-1d 'g2.gauss :window-size 9 :delx 0.67)))
	(n2diag (make-filter (sample-1d 'g2.diag :window-size 9 :delx 0.67))))
    (list (make-separable-filter n2dggauss n2dgpoly :edge-handler edge-handler)
	  (make-separable-filter n2diag n2diag :edge-handler edge-handler)
	  (make-separable-filter n2dgpoly n2dggauss :edge-handler edge-handler))))

(defun make-h2-steerable-filters (&key (edge-handler :reflect1))
  (let ((n3hila (make-filter (sample-1d 'h3.hila :window-size 9 :delx 0.67)))
	(n3hilb (make-filter (sample-1d 'h3.hilb :window-size 9 :delx 0.67)))
	(n3minusy (make-filter (sample-1d 'h3.minusy :window-size 9 :delx 0.67)))
	(n3minusx (make-filter (sample-1d 'h3.minusx :window-size 9 :delx 0.67))))
    (list (make-separable-filter n3hilb n3hila :edge-handler edge-handler)
	  (make-separable-filter n3minusx n3minusy :edge-handler edge-handler)
	  (make-separable-filter n3minusy n3minusx :edge-handler edge-handler)
	  (make-separable-filter n3hila n3hilb :edge-handler edge-handler))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEERABLE 1ST DERIVATIVE OF GAUSSIAN
;;; Sept. 27, 1989

;;; (Ix Iy)
(defun make-g1-steerable-filters (&key (edge-handler :reflect1))
  (let ((n1dgpoly (make-filter (sample-1d 'g1.poly :window-size 9 :delx 0.67)))
	(n1dggauss (make-filter (sample-1d 'g1.gauss :window-size 9 :delx 0.67))))
    (list (make-separable-filter n1dggauss n1dgpoly :edge-handler edge-handler)
	  (make-separable-filter n1dgpoly n1dggauss :edge-handler edge-handler)))) 
;;; from loading  ~/mtca/defns.m  into mathematica, and evaluating N[normderivgauss2d[x,y,1]],
;;; we find the normalization required for the integral of 1st deriv of Gaussian to equal 1.
(defun g1.poly (x)
  (* (my-gauss x)
     (* x -1.59577)))

(defun g1.gauss (x)
  (my-gauss x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DEFINE STEERABLE 4TH DERIVATIVE OF GAUSSIAN, AND STEERABLE 5TH ORDER FIT TO
;;; ITS HILBERT TRANSFORM
;;; August 27, 1989 Bill Freeman created from ~freeman/lisp/steer/steerables.lisp,
;;;                              and ~freeman/mtca/sepout.  See also Vision Science
;;;                              Technical Report #118.
;;; x-y separable Gaussian derivatives, order 4
(defun g4.4y (y)
  (* 1.24585
     (my-gauss y)
     (+ 0.75 (* -3.0 y y) (expt y 4))))
(defun g4.4x (x)
  (my-gauss x))

(defun g4.3x (x)
  (* 1.24585 x (my-gauss x)))
(defun g4.3y (y)
  (* (my-gauss y) (+ (* -1.5 y) (expt y 3))))

(defun g4.2x (x)
  (* 1.116176 (my-gauss x) (+ (* x x) -0.5)))
(defun g4.2y (y)
  (* 1.116176 (my-gauss y) (+ (* y y) -0.5)))

(defun g4.1y (y)
  (* 1.24585 y (my-gauss y)))
(defun g4.1x (x)
  (* (my-gauss x) (+ (* x -1.5) (expt x 3))))

(defun g4.0x (x)
  (* 1.24585
     (my-gauss x)
     (+ 0.75 (* -3.0 x x) (expt x 4))))
(defun g4.0y (y)
  (my-gauss y))

;;; x-y separable order 5 Hilbert transforms of order 4 Gaussian derivatives

(defun h4.5y (y)
  (* 0.39752
     (my-gauss y)
     (+ (expt y 5) (* (expt y 3) -7.5014) (* y 7.1891))))
(defun h4.5x (x)
  (my-gauss x))

(defun h4.4y (y)
  (* 0.39752
     (my-gauss y)
     (+ (expt y 4) (* (expt y 2) -4.501) 1.4378)))
(defun h4.4x (x)
  (* x (my-gauss x)))

;;; ;; non-separable version of h4.3
;;; (defun h4.3yx (y x)(* 0.39752 (exp (+ (* x x -1.0) (* y y -1.0))) 
;;; 		      (+
;;; 		       (* 1.4378 y)
;;; 		       (* x x y -2.25043)
;;; 		       (* (expt y 3) -.750143)
;;; 		       (* (expt y 3) (expt x 2)))))
;;; 

;; separable (approximate) version of h4.3
(defun h4.3y (y)
  (* 0.39752
     (my-gauss y)
     (+ (expt y 3) (* y -2.225))))
(defun h4.3x (x)
  (* (my-gauss x)
     (+ (expt x 2) -0.6638)))

;;; ;; non-separable version of h4.2
;;; (defun h4.2yx (y x)(* 0.39752 (exp (+ (* y y -1.0) (* x x -1.0))) 
;;; 		      (+
;;; 		       (* 1.4378 x)
;;; 		       (* y y x -2.25043)
;;; 		       (* (expt x 3) -.750143)
;;; 		       (* (expt x 3) (expt y 2)))))
;;; 

;;; approximately separable version of h4.2
(defun h4.2x (x)
  (* 0.39752
     (my-gauss x)
     (+ (expt x 3) (* x -2.225))))
(defun h4.2y (y)
  (* (my-gauss y)
     (+ (expt y 2) -0.6638)))

(defun h4.1x (x)
  (* 0.39752
     (my-gauss x)
     (+ (expt x 4) (* (expt x 2) -4.501) 1.4378)))
(defun h4.1y (y)
  (* y (my-gauss y)))

(defun h4.0x (x)
  (* 0.39752
     (my-gauss x)
     (+ (expt x 5) (* (expt x 3) -7.5014) (* x 7.1891))))
(defun h4.0y (y)
  (my-gauss y))

(defun make-g4-steerable-filters (&key (edge-handler :reflect1))
  (list 
   (make-separable-filter 
    (make-filter (sample-1d 'g4.0y :window-size 13 :delx 0.5))
    (make-filter (sample-1d 'g4.0x :window-size 13 :delx 0.5))
    :edge-handler edge-handler)
   (make-separable-filter 
    (make-filter (sample-1d 'g4.1y :window-size 13 :delx 0.5))
    (make-filter (sample-1d 'g4.1x :window-size 13 :delx 0.5))
    :edge-handler edge-handler)
   (make-separable-filter 
    (make-filter (sample-1d 'g4.2y :window-size 13 :delx 0.5))
    (make-filter (sample-1d 'g4.2x :window-size 13 :delx 0.5))
    :edge-handler edge-handler)
   (make-separable-filter 
    (make-filter (sample-1d 'g4.3y :window-size 13 :delx 0.5))
    (make-filter (sample-1d 'g4.3x :window-size 13 :delx 0.5))
    :edge-handler edge-handler)
   (make-separable-filter 
    (make-filter (sample-1d 'g4.4y :window-size 13 :delx 0.5))
    (make-filter (sample-1d 'g4.4x :window-size 13 :delx 0.5))
    :edge-handler edge-handler)))

(defun make-h4-steerable-filters (&key (edge-handler :reflect1))
  (list 
   (make-separable-filter 
    (make-filter (sample-1d 'h4.0y :window-size 13 :delx 0.5))
    (make-filter (sample-1d 'h4.0x :window-size 13 :delx 0.5))
    :edge-handler edge-handler)
   (make-separable-filter 
    (make-filter (sample-1d 'h4.1y :window-size 13 :delx 0.5))
    (make-filter (sample-1d 'h4.1x :window-size 13 :delx 0.5))
    :edge-handler edge-handler)
   (make-separable-filter 
    (make-filter (sample-1d 'h4.2y :window-size 13 :delx 0.5))
    (make-filter (sample-1d 'h4.2x :window-size 13 :delx 0.5))
    :edge-handler edge-handler)
   (make-separable-filter 
    (make-filter (sample-1d 'h4.3y :window-size 13 :delx 0.5))
    (make-filter (sample-1d 'h4.3x :window-size 13 :delx 0.5))
    :edge-handler edge-handler)
   (make-separable-filter 
    (make-filter (sample-1d 'h4.4y :window-size 13 :delx 0.5))
    (make-filter (sample-1d 'h4.4x :window-size 13 :delx 0.5))
    :edge-handler edge-handler)
   (make-separable-filter 
    (make-filter (sample-1d 'h4.5y :window-size 13 :delx 0.5))
    (make-filter (sample-1d 'h4.5x :window-size 13 :delx 0.5))
    :edge-handler edge-handler)))

