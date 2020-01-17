;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: fft.lisp
;;;  Author: Eero Simoncelli
;;;  Description: FFT routines in C.
;;;  Package:         'OBVIUS
;;;  Creation Date: Spring, 1988
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

(export '(power-spectrum fft hilbert-transform derivative))

;;; **** SHould we crop  to the size of the original image?
(defmethod power-spectrum ((im image) &key -> (center t) dimensions)
  (with-result ((result ->)
		(list :class (class-of im)
		      :dimensions (cond (dimensions dimensions)
					((image-p ->) (dimensions ->))
					(t (padded-dimensions im))))
		'power-spectrum im :center center)
    (with-local-viewables ((fft (fft im :dimensions dimensions)))
      (square-magnitude fft :-> result))
    (when center
      (circular-shift result
		      :x (floor (x-dim result) 2)
		      :y (floor (y-dim result) 2)
		      :-> result))
    result))

(defmethod power-spectrum ((filt filter) &key (center t) (dimensions (dimensions filt)) ->)
  (unless (every #'>= dimensions (dimensions filt))
    (error "Dimensions must be larger than filter dimensions"))
  (with-local-viewables ((im (make-image dimensions)))
    (paste (kernel filt) (data im) :-> (data im))
    (power-spectrum im :center center :-> ->)))

;;; *** Inefficient -- should call a real-valued fft routine which will 
;;; take half the time !!!! -EPS
(defmethod fft ((im image) &key inverse center
		(pre-center center) (post-center center)
		dimensions ->)
  (with-result ((result ->) 
		(list :class 'complex-image
		      :dimensions (cond (dimensions dimensions)
					((complex-image-p ->) (dimensions ->))
					(t (padded-dimensions im))))
		'fft im :inverse inverse
		:pre-center pre-center :post-center post-center)
    (cond ((eq im (imaginary-part result))
	   (error "input image cannot be eq to imaginary part of result."))
	  ((eq im (real-part result))
	   (warn "input image is eq to real-part of result: it will be modified.")
	   (zero! (imaginary-part result))
	   (set-not-current (real-part result)))
	  (t
	   (zero! result)
	   (paste im (real-part result) :-> (real-part result))))
    (when pre-center
      (circular-shift (real-part result)
		      :x (truncate (x-dim im) -2)
		      :y (truncate (y-dim im) -2)
		      :-> (real-part result)))
    (array-fft (data (real-part result)) (data (imaginary-part result)) :inverse inverse)
    (when post-center
      (circular-shift result
		      :x (floor (x-dim result) 2)
		      :y (floor (y-dim result) 2)
		      :-> result))
    result))

;;; FFT on complex images.  Note: result and im can be the same image.
(defmethod fft ((im complex-image) &key
		inverse center dimensions
		(pre-center center) (post-center center)
		->)
  (with-result ((result ->)
		(list :class (clos::class-of im)
		      :dimensions (cond (dimensions dimensions)
					((complex-image-p ->) (dimensions ->))
					(t (padded-dimensions im))))
		'fft im :inverse inverse
		:pre-center pre-center :post-center post-center)
    (cond ((eq result im)
	   (set-not-current (real-part result))
	   (set-not-current (imaginary-part result)))
	  (t (zero! result)
	     (paste im result :-> result)))
    (when pre-center
      (circular-shift result
		      :x (truncate (x-dim im) -2)
		      :y (truncate (y-dim im) -2)
		      :-> result))
    (array-fft (data (real-part result)) (data (imaginary-part result)) :inverse inverse)
    (when post-center
      (circular-shift result
		      :x (floor (x-dim result) 2)
		      :y (floor (y-dim result) 2)
		      :-> result))
    result))

;;; General FFT works on arrays of any rank
(defun array-fft (R-arr I-arr &key inverse)
  (unless (and (equal (dimensions R-arr) (padded-dimensions R-arr))
	       (equal (dimensions I-arr) (padded-dimensions I-arr)))
    (error "Arrays ~a and ~a must be padded to power of 2" R-arr I-arr))
  (unless (equal (dimensions I-arr) (dimensions R-arr))
    (error "Arrays ~a and ~a are different sizes" R-arr I-arr))
  (let ((dims (make-array (rank R-arr) :element-type 'fixnum
			  :initial-contents (dimensions R-arr)))
	(norm (sqrt (total-size R-arr))))
    (cond ((float-arrays-p R-arr I-arr)
	   (internal-fft R-arr I-arr dims (rank R-arr) (if inverse -1 1)))
	  (t (error "FFT implemented only for 'single-float arrays")))
    (div R-arr norm :-> R-arr)
    (div I-arr norm :-> I-arr))
  (values R-arr I-arr))

(defun padded-dimensions (im) 
  (mapcar 'power-of-two-ceiling (dimensions im)))

(defun power-of-two-ceiling (num)
  (do ((val 1 (* val 2))) ((>= val num) val))) 

;;; Phase shift by ninety degrees, along a line at angle theta.
;;; still broken for some frequencies? 
;;; *** Speed this up by making a ramp, applying a point-op to make a step, and
;;; multiplying!
(defmethod hilbert-transform ((im image) &key (orientation 0.0) ->)
  (with-result ((result ->)
		   (list :class (clos::class-of im)
		      :dimensions (padded-dimensions im))
		'hilbert-transform im :orientation orientation)
    (setq orientation (- (rem (+ orientation pi) 2-pi) pi)) ;make between -pi and pi
    (let* ((dft (fft im))
	   (pi/2 (/ pi 2.0))
	   (im1 (if (< (- pi/2) orientation pi/2)
		    (real-part dft)
		    (imaginary-part dft)))
	   (im2 (if (< (- pi/2) orientation pi/2)
		    (imaginary-part dft)
		    (real-part dft)))
	   (xsize (x-dim dft))
	   (xsize/2 (floor (x-dim dft) 2))
	   (ysize/2 (floor (y-dim dft) 2))
	   (tan (tan (- pi/2 orientation)))
	   ;; slope must be float
	   (slope (float (if (= 0.0 tan) xsize (/ 1.0 tan)))) ;take perpendicular line	
	   (linepos (- xsize/2 (* slope ysize/2))))
      (declare (single-float slope linepos) (fixnum xsize/2 ysize/2))
      (circular-shift dft :y ysize/2 :x xsize/2 :-> dft)
      (loop-over-image-positions ((val1 im1) (val2 im2)) (y x)
	(cond ((= y 0)
	       (if (< x xsize/2) 
		   (if (>= x linepos) (setf val2 (- val2)) (setf val1 (- val1)))
		   (if (< (- xsize x) linepos) (setf val2 (- val2)) (setf val1 (- val1)))))
	      ((< y ysize/2)
	       (cond ((= x 0)
		      (setq linepos (+ (* slope (- y ysize/2)) xsize/2))
		      (if (>= 0 linepos) (setf val2 (- val2)) (setf val1 (- val1))))
		     (t (if (>= x linepos) (setf val2 (- val2)) (setf val1 (- val1))))))
	      (t 
	       (cond ((= x 0) 
		      (setq linepos (+ (* slope (- y ysize/2)) xsize/2))
		      (if (> xsize linepos) (setf val2 (- val2)) (setf val1 (- val1))))
		     (t (if (> x linepos) (setf val2 (- val2)) (setf val1 (- val1))))))))
      (circular-shift dft :y (- ysize/2) :x (- xsize/2) :-> dft)
      (switch-components dft)
      (real-part (fft dft :inverse t :-> dft) :-> result))))

;;; Multiply the FFT by jw.
;;; *** Busted?
(defmethod derivative ((im image) &key (orientation 0.0) ->)
  (with-result ((result ->)
		(list :class (clos::class-of im)
		      :dimensions (padded-dimensions im))
		'derivative im :orientation orientation)
    (with-local-viewables
	((dft (fft im))
	 (ramp (make-image (dimensions dft)))
	 (x-slope (/ (* (cos orientation) 2 pi) (x-dim dft)))
	 (y-slope (/ (* (sin orientation) -2 pi) (y-dim dft)))
	 (y-strip (make-image (list (y-dim ramp) 1)))
	 (x-strip (make-image (list 1 (x-dim ramp)))))
      (internal-make-ramp (data ramp) (total-size ramp) (x-dim ramp)
			  (- (+ (* x-slope (floor (x-dim ramp) 2))
				(* y-slope (floor (y-dim ramp) 2))))
			  y-slope x-slope)
      (circular-shift ramp
		      :y (- (floor (y-dim ramp) 2))
		      :x (- (floor (x-dim ramp) 2))
		      :-> ramp)
      (when (evenp (x-dim ramp)) (paste y-strip ramp
					:y 0 :x (/ (x-dim ramp) 2) :-> ramp))
      (when (evenp (y-dim ramp)) (paste x-strip ramp
					:y (/ (y-dim ramp) 2) :x 0 :-> ramp))
      (mul ramp (real-part dft) :-> (real-part dft))
      (sub 0.0 (real-part dft) :-> (real-part dft)) ;cos -> -sin
      (mul ramp (imaginary-part dft) :-> (imaginary-part dft))
      (switch-components dft)
      (real-part (fft dft :inverse t :-> dft) :-> result))))

#| 
;;; from ~heeger/lispm-code/image-hacks.lisp
(defun dct (image)
  (let* ((x-dim (img:image-x-dim image))
	 (y-dim (img:image-y-dim image))
	 (big-image (img:make-image (list (* 2 x-dim) (* 2 y-dim))
			       :element-type 'sys:art-q)))
    (copy-image-into-sub-image image big-image 0 0)
    (copy-image-into-sub-image (img:image-neg-y image) big-image 0 y-dim)
    (copy-image-into-sub-image (img:image-neg-x image) big-image x-dim 0)
    (copy-image-into-sub-image (img:image-neg-x (img:image-neg-y image)) big-image x-dim y-dim)
    (multiple-value-bind (real imagin)
	(img:image-fft big-image)
      (img:image-sqrt (img:complex-image-magnitude (img:image-window real 0 0 x-dim y-dim)
						   (img:image-window imagin 0 0 x-dim y-dim))))))

(defun dst (image)
  (let* ((x-dim (img:image-x-dim image))
	 (y-dim (img:image-y-dim image))
	 (big-image (img:make-image (list (* 2 x-dim) (* 2 y-dim))
			       :element-type 'sys:art-q)))
    (copy-image-into-sub-image image big-image 0 0)
    (copy-image-into-sub-image (img:image-negate (img:image-neg-y image)) big-image 0 y-dim)
    (copy-image-into-sub-image (img:image-negate (img:image-neg-x image)) big-image x-dim 0)
    (copy-image-into-sub-image (img:image-neg-x (img:image-neg-y image)) big-image x-dim y-dim)
    (multiple-value-bind (real imagin)
	(img:image-fft big-image)
      (img:image-sqrt (img:complex-image-magnitude (img:image-window real 0 0 x-dim y-dim)
						   (img:image-window imagin 0 0 x-dim y-dim))))))

(defun odct (image)
  (let* ((x-dim (img:image-x-dim image))
	 (y-dim (img:image-y-dim image))
	 (big-image (img:make-image (list (* 2 x-dim) (* 2 y-dim))
			       :element-type 'sys:art-q
			       :initial-value 0.0)))
    (copy-image-into-sub-image image big-image 0 0)
    (copy-image-into-sub-image (img:image-neg-x (img:image-neg-y image)) big-image x-dim y-dim)
    (multiple-value-bind (real imagin)
	(img:image-fft big-image)
      (img:image-sqrt (img:complex-image-magnitude (img:image-window real 0 0 x-dim y-dim)
						   (img:image-window imagin 0 0 x-dim y-dim))))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is FFT package for lisp machine -- entry functions are
;;; (internal-fft <real-array> <imaginary-array> 
;;;   <dimensions> <rank> <inverse -1 or 1> <small-float-flag t or nil>)
;;; 	computation of DFT is done in place  
;;;	dimension of these arrays should be a power of 2
;;; 	result is DFT times N where N is dimension of array
;;;	inverse defaults to 1, when -1 inverse fft is computed
;;;	small-float-flag defaults to T, when NIL computation is done with regular flonums
;;;	   when possible. This speeds up computation by a factor of about 2 or 3
;;;	   and reduces garbage generation in exchange for reduced precision.
;;;	   Note the small-flonum option is only useful
;;;	   when the input arrays have small-flonums or integers in them.

;;;	takes about 10 minutes to do 256^2 fft in small-flonum mode
;;;	      about 1 hour to do 512^2 fft in small-flonum mode 
;;;	note there maybe problems with doing fft on large arrays in regular flonum mode

#+Symbolics
(defvar 2d-fft-r nil)
(defvar 2d-fft-c nil)

#+Symbolics
(defmacro setxdim (arr)
    `(setq xdim (array-dimension ,arr 0))) 

#+Symbolics
(defmacro dofor (index beg end &rest body)
  `(do ((,index ,beg (1+ ,index))
	(*!doforend ,end))
       ((>= ,index *!doforend))
     ,@body))

#+Symbolics
(defun internal-fft (rdata &optional (idata nil) dims (ndims 2)
			   (inverse 1) (small-float-flag t) (statsflag t)
			   &aux (xdim (aref dims 0)) rw cw)
  (if (= inverse 1)
      (setq inverse nil)
      (setq inverse t))
  (cond ((null idata)
	 (setq idata (make-array dims :initial-element 0.0))))
  (setq 2d-fft-r rdata 2d-fft-c idata)
  (multiple-value-setq (rw cw) (get-roots-of-unity xdim small-float-flag))
  ;;; first compute 1d ffts along x scan lines
  (2d-bit-reversal-map rdata idata t statsflag)
  (do ((mod 2 (* mod 2)))
      ((> mod xdim))
    (2d-fft-mod rdata idata t mod rw cw inverse statsflag))
  ;;; then compute 1d ffts along y scan lines
  (2d-bit-reversal-map rdata idata nil statsflag)
  (do ((mod 2 (* mod 2)))
      ((> mod xdim))
    (2d-fft-mod rdata idata nil mod rw cw inverse statsflag))
  (values rdata idata))

#+Symbolics
(defun 2d-bit-reversal-map (rarr carr xscan &optional (statsflag t)
				 &aux xdim bit-map xmax i-val j-val)
  (format statsflag "~%  beginning bit reversal")
  (setxdim rarr)
  (setq bit-map (get-bit-reversal-map xdim)
	xmax (1- xdim))
  (cond (xscan
	  (do ((i 1 (1+ i))
	       (j 0))
	      ((= i xmax))
	    (setq j (aref bit-map i))
	    (cond ((> j i) ;;; exchange values if mapping upward
		   (dofor x 0 xdim
			  (setq i-val (aref rarr x i)
				j-val (aref rarr x j))
			  (setf (aref rarr x i) j-val
				(aref rarr x j) i-val)
			  (setq i-val (aref carr x i)
				j-val (aref carr x j))
			  (setf (aref carr x i) j-val
				(aref carr x j) i-val))))))
	(t (dofor y 0 xdim
		  (do ((i 1 (1+ i))
		       (j 0))
		      ((= i xmax))
		    (setq j (aref bit-map i))
		    (cond ((> j i) ;;; exchange values if mapping upward
			   (setq i-val (aref rarr i y)
				 j-val (aref rarr j y))
			   (setf (aref rarr i y) j-val
				 (aref rarr j y) i-val)
			   (setq i-val (aref carr i y)
				 j-val (aref carr j y))
			   (setf (aref carr i y) j-val
				 (aref carr j y) i-val))))))))

#+Symbolics
(defun get-bit-reversal-map (dim &aux bit-mapn bit-map 2^m resetflag)
  (setq bit-mapn (make-atom 'bit-reversal-map dim)
	bit-map (cond ((and (boundp bit-mapn) (eval bit-mapn)))
		      (t (setq resetflag t) (set bit-mapn (make-array dim))))
  	2^m (floor dim 2))
  (cond (resetflag	;only if bit-reversal-map of this size has
	 (setf (aref bit-map 0) 0)		;not been defined before
	 (do ((i 1 (1+ i))
	      (j 0))
	     ((= i dim))
	   (setq j (bit-reversal-next i j 2^m))
	   (setf (aref bit-map i) j))))
  bit-map)

#+Symbolics
(defun bit-reversal-next (i j-prev 2^m)
  (+ j-prev (bit-reversal-delta i 2^m)))

#+Symbolics
(defun bit-reversal-delta (indx 2^m)
  (cond ((oddp indx) 2^m)
	(t (- (bit-reversal-delta (floor indx 2) (floor 2^m 2)) 2^m))))

#+Symbolics
(defun 2d-fft-mod (r c xscan mod rw cw &optional (inverseflag nil) (statsflag t)
		  &aux (mod/2 (floor mod 2)) xdim xdim/2 wr wc r1 c1 r2 c2 rw*v2 cw*v2)
  (format statsflag " ~d" mod)
  (setxdim r)
  (setq xdim/2 (floor xdim 2))
  (cond (xscan 
	 (loop for org from 0 below xdim by mod
	       for int = (/ xdim mod)
	       do (loop for i0 from 0 by int below xdim/2
			for i1 upfrom org 
			for i2 upfrom (+ org mod/2)
			do (dofor x 0 xdim
				  (setq wr (aref rw i0)
					wc (cond (inverseflag (- (aref cw i0))) (t (aref cw i0)))
					r1 (aref r x i1)
					r2 (aref r x i2)
					c1 (aref c x i1)
					c2 (aref c x i2))
				  (setq rw*v2 (- (* r2 wr) (* c2 wc))
					cw*v2 (+ (* r2 wc) (* c2 wr)))
				  (setf (aref r x i1) (+ r1 rw*v2)
					(aref c x i1) (+ c1 cw*v2)
					(aref r x i2) (- r1 rw*v2)
					(aref c x i2) (- c1 cw*v2))))))
	(t
	 (dofor y 0 xdim
		(loop for org from 0 by mod below xdim
		      for int = (/ xdim mod)
		      do (loop for i0 from 0 by int below xdim/2
			       for i1 upfrom org
			       for i2 upfrom (+ org mod/2)
			       do (setq wr (aref rw i0)
					wc (cond (inverseflag (- (aref cw i0))) (t (aref cw i0)))
					r1 (aref r i1 y)
					r2 (aref r i2 y)
					c1 (aref c i1 y)
					c2 (aref c i2 y))
			       (setq rw*v2 (- (* r2 wr) (* c2 wc))
				     cw*v2 (+ (* r2 wc) (* c2 wr)))
			       (setf (aref r i1 y) (+ r1 rw*v2)
				     (aref c i1 y) (+ c1 cw*v2)
				     (aref r i2 y) (- r1 rw*v2)
				     (aref c i2 y) (- c1 cw*v2))))))))

#+Symbolics
(defun make-atom (header &rest params)
  ;;; (make-atom 'foo 10 -20 'bar) => foo+10-20-bar
;;; creates a symbol in the package
  (intern (apply #'string-append
		 (cons (string header)
		       (mapcar #'(lambda (atm)
				   (format nil "~a~a"
					   (cond ((and (numberp atm) (not (minusp atm))) "+")
						 ((numberp atm) "")
						 (t "-"))
					   atm))
			       params)))))

#+Symbolics
(defun get-roots-of-unity (xdim &optional (smallfloatflag nil) &aux (xdim/2 (floor xdim 2))
				thetaint theta wrn wcn wr wc (reset-flag nil))
  (setq wrn (make-atom 'r-roots xdim smallfloatflag)
	wcn (make-atom 'c-roots xdim smallfloatflag))
  (setq wr (cond ((and (boundp wrn) (eval wrn)))
		 (t (setq reset-flag t) (set wrn (make-array xdim/2))))
	wc (cond ((and (boundp wcn) (eval wcn)))
		 (t (setq reset-flag t) (set wcn (make-array xdim/2)))))
  (cond (reset-flag
	  (setq thetaint (/ (atan 0 -1) xdim/2))
	  (setf (aref wr 0) 1)
	  (setf (aref wc 0) 0)
	  (loop for i from 1 below xdim/2
		do (setq theta (* thetaint i))
		(setf (aref wr i)
		      (cond (smallfloatflag (float (cos theta))) 
			    (t (cos theta)))
		      (aref wc i)
;;; To make the results close to those returned by the c fft,
;;; change (sin theta) to (- (sin theta))
		      (cond (smallfloatflag (float (sin theta)))
			    (t (sin theta)))))))
  (values wr wc))


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
