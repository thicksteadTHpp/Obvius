;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: flow-utilities.lisp
;;;  Author: Simoncelli
;;;  Description: Utilities for computing/manipulating flow fields.
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(obv-require :warp)

(eval-when (load)
  (set-default 'image-pair :display-type 'vector-field))

(defvar *flow-parameters* nil)
(dolist (p '(*sigma-1* *sigma-2* *prior-offset* *blur-kernel* *blur-edges* *integer-warp*))
      do (pushnew p *flow-parameters*))

(defun params ()
  (dolist (p *flow-parameters*)
    (format t "~%::> ~16S  " p)
    (prin1 (eval p)))
  (terpri))

(defun image-pair-sequence-p (thing)
  (and (viewable-sequence-p thing)
       (every #'(lambda (im) (image-pair-p im)) (viewable-list thing))))

(defun timestamp (vbl)
  (let ((nm (obv::name vbl)))
    (when (stringp nm) (parse-integer nm :junk-allowed t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This stuff copied from separable-flow.lisp:

(defvar *sigma-1* 0.0)			;energy normalization multiplier
(defvar *sigma-2* 1.0)			;constant in denominator
(defvar *prior-offset* 0.0)		;homogeneous prior variance

(defvar *blur-kernel* '(0.0625 0.25 0.375 0.25 0.0625)) ;to blur energies
;;(defvar *blur-kernel*
;;   (mapcar #'(lambda (x) (/ x 256.0)) '(1.0 8.0 28.0 56.0 70.0 56.0 28.0 8.0 1.0)))
(defvar *blur-edges* :reflect1)

;;; Standard solution, with two gaussian noise terms and a prior variance.
;;; Given products of derivative images or sequences, compute optical
;;; flow and associated covariance matrix.
;;; *** SHould this return inverse-covariance?
;;; *** Incorrect covariance handling when energies are quadrature
(defun compute-flow-dist-from-separable-energies
    (dy-2 dx-dy dx-2 dy-dt dx-dt &key
	  (blur-kernel *blur-kernel*)
	  (sigma-1 *sigma-1*)		;gain-control
	  (sigma-2 *sigma-2*)		;scale factor
	  (prior-offset *prior-offset*)
	  prior-vx prior-vy prior-vxy	;prior variance IMAGES
	  -y> -x> -vy> -vxy> -vx>)
  (with-result ((vy-image -y>) dy-2)
    (with-result ((vx-image -x>) dy-2)
      (with-result ((vy-sigma-image -vy>) dy-2)
	(with-result ((vxy-sigma-image -vxy>) dy-2)
	  (with-result ((vx-sigma-image -vx>) dy-2)
	    (with-local-viewables ((temp-x (similar dx-2 :-> "temp-x"))
				   (temp-xy (similar temp-x :-> "temp-xy"))
				   (temp-y (similar temp-x :-> "temp-y"))
				   (filt (make-separable-filter blur-kernel blur-kernel
								:edge-handler *blur-edges*))
				   norm)
	      ;; Compute energy normalizer:
	      (cond ((zerop sigma-1)
		     (setq norm sigma-2))
		    (t (setq norm (add dy-2 dx-2))
		       ;;(add (div (mul dx-dt dy-dt :-> temp-x) dx-dy :-> temp-x)
		       ;;  norm :-> norm)   ;dt^2 ******
		       (mul norm sigma-1 :-> norm)
		       (add norm sigma-2 :-> norm)))
	      ;; Compute covariance matrix:
	      (apply-filter filt (div dx-2 norm :-> dx-2) :-> temp-x)
	      (apply-filter filt (div dx-dy norm :-> dx-dy) :-> temp-xy)
	      (apply-filter filt (div dy-2 norm :-> dy-2) :-> temp-y)
	      (if (and prior-vx prior-vy prior-vxy)
		  (with-local-viewables ((inv-x (similar prior-vx))
					 (inv-xy (similar prior-vx))
					 (inv-y (similar prior-vx)))
		    (invert-2x2-symmetric-matrix
		     prior-vx prior-vxy prior-vy :-1> inv-x :-2> inv-xy :-3> inv-y)
		    (incf. temp-x inv-x)
		    (incf. temp-xy inv-xy)
		    (incf. temp-y inv-y))
		  (progn
		    (incf. temp-x prior-offset)
		    (incf. temp-y prior-offset)))
	      (invert-2x2-symmetric-matrix
	       temp-x temp-xy temp-y
	       :-1> vx-sigma-image :-2> vxy-sigma-image :-3> vy-sigma-image)
	      ;; Compute mean vector. Temp-xy holds vx-image here:
	      (setq norm (sub 0.0 norm :-> norm))
	      (apply-filter filt (div dx-dt norm :-> dx-dt) :-> temp-xy) ;normalized dx-dt
	      (apply-filter filt (div dy-dt norm :-> dy-dt) :-> vy-image) ;normalized dy-dt
	      (add (mul vx-sigma-image temp-xy :-> temp-x)
		   (mul vxy-sigma-image vy-image :-> temp-y)
		   :-> vx-image)
	      (add (mul vxy-sigma-image temp-xy :-> temp-x)
		   (mul vy-sigma-image vy-image :-> temp-y)
		   :-> vy-image))
	    (values vy-image vx-image vy-sigma-image vxy-sigma-image vx-sigma-image)))))))

;;; Used for invering the image-matrices for computing flow vectors.  Faster
;;; than making a viewable-matrix.
(defun invert-2x2-symmetric-matrix (im1 im2 im3 &key -1> -2> -3>)
  "Pointwise 2x2 symmetric matrix inversion.  im1 is an image containing the upper-left
component.  im2 is the upper-right or lower-left component.  im3 is the lower-right.
Result is 3 images, returned as multiple values.  Result ims can be eq to args!"
  (with-result ((r1 -1>) im1 'invert-2x2-symmetric-matrix im1 im2 im3)
    (with-result ((r2 -2>) im1 'invert-2x2-symmetric-matrix im1 im2 im3)
      (with-result ((r3 -3>) im1 'invert-2x2-symmetric-matrix im1 im2 im3)
	(with-local-viewables ((temp1 (similar im1))
			       (temp2 (similar temp1))
			       (det (similar temp1)))
	  (sub (mul im1 im3 :-> temp1) (square im2 :-> temp2) :-> det)
	  (div im3 det :-> temp1)	;use temp, since r1 could be im1 or im2!
	  (div im2 det :-> temp2)	;use temp, since r2 could be im1!
	  (div im1 det :-> r3)
	  (values (copy temp1 :-> r1) (sub 0.0 temp2 :-> r2) r3))))))

(defun determinant-2x2-symmetric-matrix (im1 im2 im3 &key ->)
  (with-result ((result ->) im1
		'determinant-2x2-symmetric-matrix im1 im2 im3)
    (with-local-viewables ((temp1 (similar im1))
			   (temp2 (similar im1)))
      (sub (mul im1 im3 :-> temp1) (square im2 :-> temp2) :-> result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Full Warping code:

;;; ONLY FOR SEQUENCES!  If warper-seq is nil, then just apply the
;;; filter.  Otherwise, warper-seq should be a sequence of
;;; image-pairs, which will be used to pre-warp the sequence before
;;; applying the filter.  The nth warper-seq frame is used to warp the
;;; (n-1)th frame to the nth.
;;; NOTE: edges are handled by repeating last frame....
(defun apply-filter-with-prewarping (filt seq warper-seq &key ->
					  (start-frame 0)
					  (end-frame (z-dim seq)))
  (check-type filt filter) (check-type seq image-sequence)
  (check-type warper-seq (or null (satisfies image-pair-sequence-p)))
  (when (and warper-seq (/= (sequence-length warper-seq) (sequence-length seq)))
    (error "Warper seq ~A has different length from seq ~A" warper-seq seq))
  (with-result ((result ->)
		(list :class 'image-sequence
		      :dimensions (obvius::subsampled-dimensions
				   (cons (- end-frame start-frame) (dimensions seq))
				   (start-vector filt) (step-vector filt)))
		'apply-filter-with-prewarping filt seq warper-seq
		:start-frame start-frame :end-frame end-frame)
    (if warper-seq
	(with-local-viewables
	    ((warped-seq (make-image-sequence (dimensions seq) :length (z-dim filt)))
	     (z-filt-ctr (floor (z-dim filt) 2)))
	  (loop with temp-result = (make-image-sequence (list (frame 0 seq)))
		for res-frame from 0 below (z-dim result)
		for ctr-frame from (+ (obvius::z-start filt) start-frame)
		by (obvius::z-step filt) below end-frame
		for start-frame = (- ctr-frame z-filt-ctr)
		do
		(status-message "Apply-filter-with-prewarping: frame ~A" res-frame)
		;; Warp the appropriate sub-sequence:
		(unwarp seq warper-seq
			:start-frame start-frame
			:center-frame ctr-frame
			:end-frame (+ start-frame (z-dim filt))
			:-> warped-seq)
		;; Apply filter to warped subsequence:
		(setf (image-list temp-result) (list (frame result res-frame)))
		(let ((*status-messages* nil)) (declare (special *status-messages*))
		     (apply-filter filt warped-seq
				   :start-frame z-filt-ctr :end-frame (1+ z-filt-ctr)
				   :-> temp-result))
		finally (progn (setf (image-list temp-result) nil)
			       (status-message ""))))
	(apply-filter filt seq :start-frame start-frame :end-frame end-frame :-> result))
    result))

;;; Unwarp frames of seq between start-frame and end-frame using the
;;; sequence of warp maps (image-pairs) in warper.  Center frame is
;;; left unwarped.  All others are warped toward it.
(defmethod unwarp ((seq image-sequence) warper
		   &key ->
		   (start-frame 0)
		   (end-frame (sequence-length seq))
		   (center-frame (clip (ceiling (+ start-frame end-frame) 2)
				       0 (1- (sequence-length seq)))))
  (unless (<= 0 center-frame (1- (sequence-length seq)))
    (error "Bad center frame number: ~A" center-frame))
  (unless (<= start-frame center-frame (1- end-frame))
    (error "Bad start/center/end frame number arguments: ~A ~A ~A"
	   start-frame center-frame end-frame))
  (with-result ((result ->) (list :class (object-class-name seq)
				  :dimensions (dimensions seq)
				  :length (- end-frame start-frame))
		'unwarp seq warper :start-frame start-frame
		:center-frame center-frame :end-frame end-frame)
    (let ((slength (sequence-length seq))
	  (rlength (sequence-length result))
	  (*status-messages* nil))
      (declare (special *status-messages*))
      ;; blur center frame into result (to match linear interpolation in warping).
      (apply-filter (frame seq center-frame)
		    (make-separable-filter '(0.05 0.9 0.05) '(0.05 0.9 0.05)
					   :edge-handler :reflect1)
		    :-> (frame result (- center-frame start-frame)))
      (with-local-viewables ((temp-warp (make-image-pair (dimensions warper)))
			     (neg-warp (make-image-pair (dimensions warper))))
	;; Warp frames before ctr frame into ctr frame
	(loop for seq-frame from (1- center-frame) downto 0 ;don't go past 0
	      for res-frame from (1- (- center-frame start-frame)) downto 0
	      for sub-warper = (copy (frame center-frame warper) :-> temp-warp)
	      then (compose-warps (frame (1+ seq-frame) warper) sub-warper :-> sub-warper)
	      do
	      (warp (frame seq seq-frame) sub-warper :-> (frame result res-frame)))
	;; Warp frames after ctr frame into ctr frame
	(loop for seq-frame from (1+ center-frame) below slength
	      for res-frame from (1+ (- center-frame start-frame)) below rlength
	      for sub-warper = (copy (frame (1+ center-frame) warper) :-> temp-warp)
	      then (compose-warps (frame seq-frame warper) sub-warper :-> sub-warper)
	      do
	      (negate sub-warper :-> neg-warp)
	      (warp (frame seq seq-frame) neg-warp :-> (frame result res-frame))))
      ;; if start-frame < 0, just repeat initial frame of sequence
      (loop with first-good-frame = (- start-frame)
	    for res-frame from 0 below first-good-frame
	    do (copy (frame result first-good-frame) :-> (frame result res-frame)))
      ;; if end-frame > length. just repeat last frame of sequence
      (loop with last-good-frame = (- rlength 1 (- end-frame slength))
	    for res-frame from (1+ last-good-frame) below rlength
	    do (copy (frame result last-good-frame) :-> (frame result res-frame)))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Simpler (and faster) linearized warping: apply warped temporal filter

(defvar *integer-warp* nil)

;;; Like single-frame-correlate, but assumes a 1D temporal filter, and
;;; warps frames of SUBSEQ toward middle frame using the (single
;;; frame) image-pair specified by WARPER.  If warper is nil, doews
;;; single-frame-correlate.
(defun single-frame-tfilt-with-prewarping (t-filt subseq warper &key ->)
  (with-result ((res ->) (frame 0 subseq))
    (let ((im-vect (make-vector-of-images-for-C subseq))
	  (tfilt-size (total-size t-filt)))
      (unless (= (length. subseq) (total-size t-filt))
	(error "subseq length must be equal to t-filt length"))
      (if warper
	  (funcall (if *integer-warp* 'int-warped-add 'bilinear-warped-add)
		   im-vect
		   (data (x-component warper)) (data (y-component warper))
		   (data res) (x-dim res) (y-dim res)
		   (kernel t-filt) tfilt-size (floor tfilt-size 2))
	  (dot-product subseq (vectorize (kernel t-filt)) :-> res)))
    res))
  
(defun make-vector-of-images-for-C (image-mtx)
  (loop with im-vect = (vectorize (matrix image-mtx))
	with len = (length im-vect)
	with ptr-vect = (make-array len :element-type '(unsigned-byte 32))
	for i from 0 below len
	do (install-foreign-pointer (data (aref im-vect i)) ptr-vect i)
	finally (return ptr-vect)))

;;;; Foreign functions (these are used in integer-warping.lisp)
;;;; (Compile these, or they won't work!!!). *** Alternatively, could
;;;; use (lucid::array-data-address array).

(LCL:def-foreign-function int-warped-add
    (image-vector :array) (xwarp (:array :single-float)) (ywarp (:array :single-float))
    (res (:array :single-float)) (xdim :fixnum) (ydim :fixnum)
    (filt (:array :single-float)) (filt-dim :fixnum) (filt-ctr :fixnum))

(LCL:def-foreign-function bilinear-warped-add
    (image-vector :array) (xwarp (:array :single-float)) (ywarp (:array :single-float))
    (res (:array :single-float)) (xdim :fixnum) (ydim :fixnum)
    (filt (:array :single-float)) (filt-dim :fixnum) (filt-ctr :fixnum))

(LCL:def-foreign-function install-foreign-pointer
    (image (:array :single-float)) (image-vector :array) (index :fixnum))

(LCL:def-foreign-function internal-acos
    (im (:array :single-float)) (result (:array :single-float)) (size :fixnum))

(load-foreign-files (list (merge-pathnames "tutorials/motion/warped-add.o"
					   obv::*obvius-directory-path*))
		    '("-lm" "-lc"))

#| warped-add test:
(setq foo (make-image-sequence
	   (loop for i from 1 to 3
		 collect (make-impulse '(5 5) :origin (list i i)))))

;;; examine moving impulse
(loop with filt = (make-matrix '(1 2 4)) ;binary encoding!
      with ivect = (make-vector-of-images-for-C foo)
      with warpx = (make-image (dimensions foo))
      with warpy = (make-image (dimensions foo))
      with res = (similar warpx)
      for wy = -7/4 then (+ wy 1/2) until (> wy 7/4)
      do
      (fill! warpy wy)
      (loop for wx = -7/4 then (+ wx 1/2) until (> wx 7/4)
	    do
	    (fill! warpx wx)
	    (bilinear-warped-add ivect (data warpx) (data warpy) (data res)
			    (x-dim res) (y-dim res) filt 3 1)
	    (format t "~,3,-2G  " (iref res 2 2)))
      (format t "~%"))

;;; Compare to standard warper:
(load-image (merge-pathnames "images/reagan" obv::*obvius-directory-path*)
(load-image (merge-pathnames "images/einstein" obv::*obvius-directory-path*)
(setq warper 
      (let ((w (make-image-pair (dimensions reagan) :display-type 'vector-field)))
	(fill! (x-component w) 1.0)
	w))

(setq warper
      (let ((w (make-image-pair
		(list (make-gaussian-noise-image (dimensions reagan) :variance 20.0)
		      (make-gaussian-noise-image (dimensions reagan) :variance 20.0))
		:display-type 'vector-field)))
	(internal-round (obv::data (frame 0 w)) (obv::data (frame 0 w)) (total-size w) 1.0)
	(internal-round (obv::data (frame 1 w)) (obv::data (frame 1 w)) (total-size w) 1.0)
	w))

;;; Compare to standard interpolating warper:
(setq res1 (similar einstein))
(setq res2 (similar einstein))
(let ((im-array (make-array 2  :element-type '(array single-float)))
      (filt (make-array 2 :element-type 'single-float
			:initial-contents '(1.0 0.3)))
      (f-ctr 0))
  (zero! res1)
  (install-foreign-pointer (obv::data reagan) im-array 0)
  (install-foreign-pointer (obv::data einstein) im-array 1)
  (int-warped-add im-array
		  (obv::data (x-component warper)) (obv::data (y-component warper))
		  (obv::data res1) (x-dim res1) (y-dim res1)
		  filt 2 f-ctr)
  (set-not-current res1)
  (with-local-viewables ((tmp (similar res1)) (neg-w (-. warper)))
    (mul (aref filt f-ctr) (if (= f-ctr 1) einstein reagan) :-> (sym-or-vbl 'res2))
    (warp (if (= f-ctr 1) reagan einstein) (if (= f-ctr 1) warper neg-w) :-> tmp)
    (mul tmp (aref filt (- 1 f-ctr)) :-> tmp)
    (add tmp res2 :-> res2))
  (values res1 res2 (-. res1 res2 :-> (sym-or-vbl 'diff))))

;;; test single-frame-tfilt-with-prewarping
(setq *seq*
      (with-local-viewables ((im (make-fractal '(32 32) :fractal-dimension 3.0))) ;1/r
	(make-image-sequence
	 (loop for i from 0 below 3 collect (circular-shift im :x-shift i)))))
(setq actual-flow
      (let ((pr (make-image-pair (dimensions *seq*))))
	(fill! (x-component pr) 1.0)
	pr))
;; Should be zero:
(single-frame-tfilt-with-prewarping
 (make-filter '(-0.25 0.5 -0.25)) *seq* actual-flow)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Error measuv-v-res:

;; Angular distance (ala Fleet/Jepson) in degrees between two vector fields
(defun v-angular-error (est actual &key ->)
  (with-result ((res ->)
		`(:class image :dimensions ,(dimensions actual))
		'v-angular-error est actual)
    (with-local-viewables ((ones (make-image-sequence
				  (list (let ((im (similar res))) (fill! im 1.0)))))
			   (v1 (append. est ones)) ; (v1x v1y 1.0)
			   (v2 (append. actual ones)) ; (v2x v2y 1.0)
			   (len1 (vector-length. v1))
			   (len2 (vector-length. v2)))
      (dot-product v1 v2 :-> res)
      (/. res len1 :-> res)
      (/. res len2 :-> res)
      (clip res -1 1 :-> res)		;avoid floating point errors here!
      (internal-acos (data res) (data res) (total-size res))
      (mul res (/ 180 pi) :-> res))))

(defun v-square-error (est actual &key ->)
  (with-result ((res ->)
		`(:class image :dimensions ,(dimensions actual))
		'v-square-error est actual)
    (with-local-viewables ((diff (sub est actual)))
      (square-magnitude diff :-> res))))

;;; Return components of error in direction of and perpendicular to the actual flow.
(defun v-error-components (est actual)
  (with-local-viewables ((mag-a (magnitude actual))
			 (a-unit (/. actual mag-a))
			 (diff (-. est actual)))
    (values (dot-product a-unit diff)
	    (progn (rotate-seq! a-unit)	;compute (destructively) perpendicular to a-unit
		   (-. (y-component a-unit) :-> (y-component a-unit))
		   (dot-product a-unit diff)))))

;;; Component of error in direction of actual.
(defun v-bias (est actual &key ->)
  (with-result ((res ->)
		`(:class image :dimensions ,(dimensions actual))
		'v-bias est actual)
    (with-local-viewables ((diff (-. est actual))
			   (mag (magnitude actual)))
      (dot-product actual diff :-> res)
      (div res mag :-> res))))

(defun v-percent-error (est actual &key ->)
  (with-result ((res ->)
		`(:class image :dimensions ,(dimensions actual))
		'v-percent-error est actual)
    (with-local-viewables ((err (-. est actual))
			   (mag (magnitude actual)))
      (magnitude err :-> res)
      (add mag (mean mag) :-> mag)
      (div res mag :-> res))))

(defun v-error-summary (est actual &rest crop-args)
  (with-local-viewables ((e (apply 'crop est crop-args))
			 (a (apply 'crop actual crop-args))
			 (angular-error-in-degrees (v-angular-error e a))
			 (diff (sub e a))
			 (square-err (square-magnitude diff))
			 (bias (dot-product a diff))
			 (mag-a (magnitude a)))
    (div bias mag-a :-> bias)
;    (display (make-histogram angular-error-in-degrees :binsize 1.0
;			     :name "angular-err"))
    (pdb (mean square-err)		;pdb defined in .lisp
	 (mean angular-error-in-degrees)
	 (sqrt (variance angular-error-in-degrees))
	 (mean bias)))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Gaussian distribution stuff:

;;; Measure deviation (Chi statistic) from the given
;;; velocity (actual-vy, actual-vx) at each point.  
(defun compute-deviation (actual-flow
			  est-flow
			  cov-y cov-xy cov-x &key ->)
  (with-local-viewables ((dev-y (sub (y-component est-flow) (y-component actual-flow)))
			 (dev-x (sub (x-component est-flow) (x-component actual-flow)))
			 (icov-y (similar cov-y))
			 (icov-xy (similar cov-xy))
			 (icov-x (similar cov-x))
			 (temp (similar dev-y)))
    (invert-2x2-symmetric-matrix cov-y cov-xy cov-x :-1> icov-y :-2> icov-xy :-3> icov-x)
    (with-result ((res ->) dev-y
		  'compute-deviation actual-flow est-flow icov-y icov-xy icov-x)
      (mul (square dev-y :-> temp) icov-y :-> res)
      (mul (square dev-x :-> temp) icov-x :-> temp)
      (add res temp :-> res)
      (mul (mul (mul dev-y dev-x :-> temp) icov-xy :-> temp) 2.0 :-> temp)
      (add res temp :-> res)  ; (ddb (<. res 0.0))
      (div res 2 :-> res)
      (square-root res :-> res))))

(defun make-chi-square-histogram (&key (stdev 1.0)
				       (max (* 5 stdev))
				       (size (obv::get-default 'discrete-function :size))
				       (binsize (/-0 max (1- size))))
  (let ((fn (make-discrete-function #'(lambda (x)
					(* x (exp (/ (sqr (/ x stdev)) -2))))
				    0 max :size (1+ (ceiling max binsize))
				    :name (format nil "Chi-square,dev=~A" stdev))))
    (display fn 'graph :graph-type :bar)
    fn))

(defun make-gaussian-distribution-image (dims mean-and-var
					     &key
					     (x-range '(-2.0 2.0))
					     (y-range x-range) ->)
  (obv-require :image-lines)
  (with-result ((res ->) (list :class 'image :dimensions dims)
		      'display-flow-distribution dims mean-and-var)
    (let* ((mean (make-array 2 :element-type 'single-float
			     :initial-contents (butlast mean-and-var 3)))
	   (var (make-array '(2 2) :element-type 'single-float
			    :initial-contents
			    (list (list (nth 2 mean-and-var) (nth 3 mean-and-var))
				  (list (nth 3 mean-and-var) (nth 4 mean-and-var)))))
	   (ivar (matrix-inverse var))
	   x-diff y-diff)
      (zero! res)
      (make-synthetic-image dims
			    #'(lambda (y x) 
				(setq y-diff (- y (aref mean 0)))
				(setq x-diff (- x (aref mean 1)))
				(- (max 0.0 (+ (* 0.5 (sqr y-diff) (aref ivar 0 0))
					       (* y-diff x-diff (aref ivar 0 1))
					       (* 0.5 (sqr x-diff) (aref ivar 1 1))))))
			    :x-range x-range
			    :y-range y-range
			    :-> res)
      (exp. res :-> res)
      (let ((val (/ (maximum res) 2)))
	(draw-line res (floor (y-dim res) 2) 0 (floor (y-dim res) 2) (1- (x-dim res))
		   :val val :-> res)
	(draw-line res 0 (floor (x-dim res) 2) (1- (y-dim res)) (floor (x-dim res) 2)
		   :val val :-> res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Things that should be in OBVIUS:

;;; Make a sequence from a base image by warping according to the
;;; warp-seq.  For testing the warping code (compose warps).  Warper
;;; that gets passed is a sequence of image-pairs.  Result is an image
;;; sequence in which the first image is base-im warped by first frame
;;; of warper, seconde frame of result is base-im warped by
;;; composition of first two frames of warper...
(defmethod make-warp-sequence ((base-im image) warper
			       &key (length (sequence-length warper)) ->)
  (unless (<= 0 length (sequence-length warper))
    (error "Bad length parameter: ~A" length))
  (with-result ((result ->)
		(list :class 'image-sequence
		      :dimensions (dimensions base-im)
		      :length length)
		'make-warp-sequence base-im warper :length length)
    (with-local-viewables ((temp-warper (copy (frame 0 warper))))
      (loop for res in (viewable-list result)
	    for the-frame from 0
	    for the-warper = temp-warper
	    then (compose-warps the-warper (frame the-frame warper) :-> the-warper)
	    do (warp base-im the-warper :-> res)))
    result))

(defmethod shift-seq! ((seq viewable-sequence) new-vbl &key forward-p)
  (unless (eq (type-of (aref (matrix seq) 0 0)) (type-of new-vbl))
    (error "~A cannot be incorporated into a viewable-sequence with element type ~A"
	   new-vbl (type-of (aref (matrix seq) 0 0))))
  (shift-seq! (vectorize (matrix seq)) new-vbl :forward-p forward-p)
  seq)

(defmethod rotate-seq! ((seq viewable-sequence) &key forward-p)
  (rotate-seq! (vectorize (matrix seq)) :forward-p forward-p)
  seq)

(defmethod shift-seq! ((seq sequence) new-val &key forward-p)
  (loop with shift = (if forward-p 1 -1)
	with x-dim = (x-dim seq)
	with prev-val = new-val
	for i = (if forward-p 0 (- (length seq) 1)) then (mod (+ i shift) x-dim)
	for count from 0 below x-dim
	do (rotatef (elt seq i) prev-val))
  seq)

(defmethod rotate-seq! ((seq sequence) &key forward-p)
  (loop with shift = (if forward-p 1 -1)
	with x-dim = (x-dim seq)
	with prev-val = (elt seq (mod (- shift) x-dim))
	for i = 0 then (mod (+ i shift) x-dim)
	for count from 0 below x-dim
	do (rotatef (elt seq i) prev-val))
  seq)

(defmethod vector-length. ((seq viewable-sequence) &key ->)
  (with-result ((res ->) (aref (matrix seq) 0 0))
    (sqrt. (dot-product seq seq :-> res) :-> res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
