;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: motion-tutorial.lisp
;;;  Author: Simoncelli
;;;  Description:
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This tutorial will take you through a simple derivation of an
;; algorithm for computing optical flow, the projection of motion onto
;; the image plane.  The algorithm is based on measurements of the
;; gradient, but, as we will show, may also be thought of as a
;; spatio-temporal "Energy" algorithm (ala Adelson/Bergen).

;; First load in some required code, and define a few simple functions
;; (using C-c e).

(progn
  (obv-require :warp)
  (obv-require :gaussian-pyramid)
  (set-default 'gray :zoom 4)
  (set-default 'filter :display-type nil)
  (set-default 'image-pair :display-type 'vector-field)
  (set-default 'image-sequence :display-type 'flipbook)
  ;; Load some software to run the full-blown multi-scale algorithm:
  (compile-load (merge-pathnames "tutorials/motion/loop-derivs"
				 obv::*obvius-directory-path*))
  (compile-load (merge-pathnames "tutorials/motion/flow-utilities"
				 obv::*obvius-directory-path*))
  (compile-load (merge-pathnames "tutorials/motion/flow"
				 obv::*obvius-directory-path*)))

;;; We'll need this function, too:
(defvar *blur-kernel* '(0.25 0.5 0.25))
(defun blur2 (im &key (kernel *blur-kernel*))
  (blur im :kernel kernel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The first portion of the tutorial will use only 1D examples.  These
;; are easiar to visualize and easier to understand, and many of the
;; important issues in the 2D case are also present in 1D.

(setq *xdim* 32
      *tdim* 16)

;; Make a sequence of 1D impulses, translating to the right at one
;; pixel per frame.

(setq impulse-sequence
      (make-image-sequence
       (loop with impulse = (make-impulse (list 1 *xdim*)
					 :origin (list 0 (/ *xdim* 4)))
	     for i from 0 below *tdim*
	     collect (circular-shift impulse :x-shift i))))

;; Motion is orientation.  We can view the sequence of signals in the
;; X (space) and T (time) dimensions, with amplitude corresponding to
;; intensity (time axis points down).
(setq xt-impulse (make-slice impulse-sequence :y-coord 0))

;; [Software puzzler: why does a slice with :y-coord 0 produce this image?]

;; Now to compute motion, we can use the gradient method:

;; Ix . v + It = 0

;; We make two filters to compute the X and T derivatives:
(setq *dx-filt* (make-separable-filter '(0.233 0.534 0.233) '(-0.459 0.0 0.459)
				       :edge-handler :dont-compute))
(setq *dt-filt* (make-separable-filter '(-0.459 0.0 0.459) '(0.233 0.534 0.233)
				       :edge-handler :dont-compute))

(setq dx-impulse (apply-filter *dx-filt* xt-impulse))
(setq dt-impulse (apply-filter *dt-filt* xt-impulse))

;; We can compute an estimate of velocity at each point in space and
;; time using the gradient constraint.
(setq v-impulse (-. (/. dt-impulse dx-impulse)))

;; What you see is an x-t image of VELOCITY ESTIMATES.  Each point in
;; the image corresponds to a different spatial position at a
;; different time.  The intensity at each point is proportional to
;; estimated speed.  Look at the values in the result by clicking
;; right mouse on the image and move the mouse around while holding
;; down the right mouse button.  The white pixels have a value of one,
;; the correct speed.  But, there is an oblique line of zeroes in
;; between the two lines of ones, and the remainder of the image is
;; filled with zeros.

;; Now notice the warning that should have appeared in the *lisp*
;; buffer.  The problem is that the spatial derivative dx-impulse is
;; zero at some locations and we cannot compute the velocity at those
;; points.  When OBVIUS encounters a division by zero, it hands back a
;; result of 0.0.

;; To fix the strip of zeroes between the lines of ones, we can
;; combine the derivative measurements over a local neighborhood (by
;; blurring the squared filter outputs):

(setq v-blur-impulse
      (-. (/. (blur2 (*. dt-impulse dx-impulse))
	      (blur2 (sqr. dx-impulse)))))

;; The velocity estimate is now correct (v=1) in the vicinity of the
;; moving impulse.  You still get the warnings about division by zero.
;; This is because there is NO motion information in the portion of
;; the images away from the impulse.  The computation is thus singular
;; at these points.  As we will see, this problem occurs in an even
;; more serious way in 2D.

;; To save typing, let's package this mini-algorithm into a function.
;; We'll set up keyword arguments to pass the parameters that control
;; the algorithm, letting them default to the values we've used thus
;; far.  Note that "with-local-viewables" behaves like "let", but it
;; frees the memory used by the images when it exits.  Compile the
;; expression with "C-c c".
(defun compute-1D-flow (xt-image &key
				(blur-kernel *blur-kernel*)
				(dt-filter *dt-filt*)
				(dx-filter *dx-filt*))
  (with-local-viewables ((dt-image (apply-filter dt-filter xt-image))
			 (dx-image (apply-filter dx-filter xt-image))
			 (dx-dt (*. dt-image dx-image))
			 (dx-2  (sqr. dx-image))
			 (blurred-dx-dt (blur dx-dt :kernel blur-kernel))
			 (blurred-dx-2 (blur dx-2 :kernel blur-kernel))
			 (neg-v (/. blurred-dx-dt blurred-dx-2)))
    (-. neg-v)))

;; Now we'll compute velocity on a one-dimensional image with more
;; interesting content.  We'll make a 1D fractal noise pattern moving
;; one pixel to the left per time-step:
(setq *xdim* 64)
(setq *tdim* 16)
(setq fract
      (let ((fract (make-fractal (list 1 *xdim*) :fractal-dimension 3.5)))
	(make-image-sequence
	 (loop for i from 0 below *tdim*
	       collect (circular-shift fract :x-shift i)))))

(setq xt-fract (make-slice fract :y-coord 0))

(setq v-blur-fract (compute-1D-flow xt-fract))
;; OBVIUS auto-scales this velocity image, but if you look at the
;; scaling information, you will see that the error is infinitesimal
;; (the velocity estimate is 1 plus or minus 1e-7 everywhere).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Motion in the frequency domain: a connection between the gradient
;;; and spatio-temporal energy mechanisms.

;; A translating one-dimensional pattern has a fourier spectrum lying
;; on a line through the origin.  The slope of the line corresponds to
;; the pattern velocity.

(setq fract-fft (fft xt-fract :center t :dimensions (list *xdim* *xdim*)))
(setq fract-mag (magnitude fract-fft))

;; The Fourier magnitude image is plotted over a range of [-pi, pi] in
;; both the wx and wy directions.  Most of the energy in this stimulus
;; lies along a line in wx-wt.  [But why doesn't it lie EXACTLY on a
;; line?]

;; Remember that convolution corresponds to multiplication in the
;; frequency domain.  Therefore, the Fourier Transform of the
;; derivative images (dx-fract and dt-fract) corresponds to the
;; product of the DFT's of the filter and fract.  Let's look at the
;; Fourier magnitude of the filters (first the time derivative filter,
;; the x-derivative filter):

(setq dt-mag
      (sqrt. (power-spectrum *dt-filt* :dimensions (list *xdim* *xdim*))))
(setq dx-mag
      (sqrt. (power-spectrum *dx-filt* :dimensions (list *xdim* *xdim*))))

;; The derivative operators are Spatio-temporal filters!  Not only
;; that, we can write the temporal derivative as a sum of two other
;; spatio-temporal filters oriented at +/- 45 degrees (that is,
;; filters that are most sensitive to either rightward or leftward
;; motion):

(setq dr-filt (div (add *dx-filt* *dt-filt*) 2))
(setq dl-filt (div (sub *dx-filt* *dt-filt*) 2)) 

;; We can build the time-derivative filter from these two direction
;; selective filters.  Let's check that:
(display (setq sum-r-l (-. dr-filt dl-filt)))
(display *dt-filt*)

;; Now look at the FTs of these two direction selective filters.  They
;; are Rightward and Leftward Spatio-temporal energy filters.
(setq r-mag
      (sqrt. (power-spectrum dr-filt :dimensions (list *xdim* *xdim*))))
(setq l-mag
      (sqrt. (power-spectrum dl-filt :dimensions (list *xdim* *xdim*))))

;; We can now rewrite the velocity calculation to look like a standard
;; Adelson-Bergen style spatio-temporal energy calculation,
;; ((R - L)/S):

(setq v-blur-ste-fract
      (let* ((dr-fract (apply-filter dr-filt xt-fract))
	     (dl-fract (apply-filter dl-filt xt-fract))
	     (dx-fract (apply-filter *dx-filt* xt-fract)))
	(/. (-. (blur2 (sqr. dl-fract))
		(blur2 (sqr. dr-fract)))
	    (blur2 (sqr. dx-fract)))))

(mean-square-error  V-BLUR-STE-FRACT V-BLUR-FRACT) ;should be zero

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Temporal Aliasing:

;; The patterns above were moving only one pixel per frame.  Now
;; consider a pattern moving two pixels per frame:

(setq fract2
      (let ((fract (make-fractal (list 1 *xdim*) :fractal-dimension 3.5)))
	(make-image-sequence
	 (loop for i from 0 below *tdim*
	       collect (circular-shift fract :x-shift (* 2 i))))))

;; Notice in the x-t slice that the orientation is much more oblique,
;; corresponding to faster motion.
(setq xt-fract2 (make-slice fract2 :y-coord 0))

;; Now let's try to estimate the velocity of this stimulus:
(setq v-blur-fract2 (compute-1D-flow xt-fract2))

;;; Look at a histogram of the estimated velocities:
(make-histogram v-blur-fract2)

;; The speed estimates have quite a lot of variability.  What is
;; happening?  The filters are too small for an image displacement of
;; 2 pixels, so the algorithm fails badly.  To see this another way,
;; look at the Fourier transform of the signal:

(setq fract2-mag
      (sqrt. (power-spectrum xt-fract2 :dimensions (list *xdim* *xdim*))))
(reduce-contrast)

;; You should be able to see aliased copies of the spectrum, parallel
;; to the central "stripe".  Why does this happen?  How does it affect
;; the derivative filters (look back at dx-spectrum and dt-spectrum)?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A Coarse-to-fine algorithm:

;; One way to get around the temporal aliasing problem is to use
;; filters of lower spatial frequency response.  These will not "see"
;; the aliased copies, and thus will be able to get a better velocity
;; estimate.  Alternatively, we can blur and subsample the images
;; spatially, thus reducing the motion displacement per frame.

(setq xt-fract2-blur
      (apply-filter (make-filter (mapcar #'(lambda (x) (* x 2)) gauss-5)
				 :step-vector '(2))
		    xt-fract2))

;;; Note that the aliasing in the power spectrum will now have a much
;;; smaller effect on the filters (look back at dx-mag and dt-mag):
(setq xt-fract2-blur-spectrum
      (sqrt. (power-spectrum xt-fract2-blur  :dimensions (list *xdim* *xdim*))))

;;; Now we can compute motion on this blurred and subsampled image.
;;; Rember that the resulting velocities must now be multiplied by 2
;;; to get them in units of pixels/frame relative to the sampling of
;;; the original images.
(setq v2-blur-fract2 (compute-1D-flow xt-fract2-blur))

(display (make-histogram  V2-BLUR-FRACT2) 'graph :x-range '(0.99 1.01))

;; We could stop here. But the current velocity field is computed at
;; reduced resolution, and we may want higher resolution velocity
;; fields.  In particular, for real images, the motion is often NOT
;; uniform translation.  Thus we want to use the smallest filters
;; possible to compute the most LOCAL velocity possible.

;; One solution is to use a coarse-to-fine procedure.  We get an
;; initial estimate at the coarse scale using big filters (or
;; equivalently, using blurred and subsampled images).  Then we refine
;; that estimate at successively finer scales.  After computing the
;; initial coarse scale estimate of the velocity, we try to "undo"
;; motion by aligning the images according to the coarse-scale
;; estimate.  Then the finer scale filters are used to compute a
;; correction to the coarse-scale flow field.

;; To undo the motion, we translate or "warp" each frame (each scan
;; line of the X-T diagram) back toward the center frame according to
;; our current motion estimate.  We construct a warp vector field to
;; do this.  For this example, we just use the mean velocity estimate
;; (mean of v2-blur-fract2) to construct this warp field.  In general
;; the warp field will be spatially varying according to the velocity
;; estimates.

(setq warper
      (make-image-pair
       (list (zero! (similar xt-fract2))
	     (make-ramp (dimensions xt-fract2)
			:orientation (/ pi 2) :slope (* 2 (mean v2-blur-fract2))))))

;; Now, we use this warper to "undo" the motion of the sequence.  The
;; orientation of this XT slice is now much closer to vertical, and
;; the smaller set of filters could be used to compute a "correction"
;; vector field.

(setq warped-fract2 (warp xt-fract2 warper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2 Dimensional imagery.

;; The velocity estimation problem becomes a bit more difficult, both
;; conceptually and computationally, when considering 2D images.
;; Nevertheless, the same basic gradient constraint may be used as a
;; basis for a reasonable algorithm.

(defvar *s-kernel* '(3.925323e-2 0.242761 0.435971 0.242761 3.925323e-2))
(defvar *sd-kernel* '(-0.100450 -0.288091 0.00000 0.288091 0.100450))
(defvar *t-kernel* '(0.230366 0.539269 0.230366))
(defvar *td-kernel* '(-0.441419 0.00000 0.441419))

;; Make 3D gradient filters:
(setq *dx-filt*
      (make-separable-filter
       (make-filter *t-kernel* :edge-handler :dont-compute)
       (make-separable-filter *s-kernel* *sd-kernel* :edge-handler :dont-compute)))
(setq *dy-filt*
      (make-separable-filter
       (make-filter *t-kernel* :edge-handler :dont-compute)
       (make-separable-filter *sd-kernel* *s-kernel* :edge-handler :dont-compute)))
(setq *dt-filt*
      (make-separable-filter
       (make-filter *td-kernel* :edge-handler :dont-compute)
       (make-separable-filter *s-kernel* *s-kernel* :edge-handler :dont-compute)))

;; Compile the following function, using C-c c.  This function takes
;; the x-, y-, and t-derivative measurements and combines them to
;; compute velocities at each spatial position.
(defun compute-2D-flow-from-derivs (dt dy dx)
  (with-local-viewables
      ((y-2 (sqr. dy))
       (y-2-inc (+. y-2 *prior-offset*))
       (x-2 (sqr. dx))
       (x-2-inc (+. x-2 *prior-offset*))
       (x-y (*. dx dy))
       (y-t (*. dy dt))
       (x-t (*. dx dt))
       (M (make-image-matrix (list (list (blur2 y-2-inc) (blur2 x-y))
				   (list (blur2 x-y) (blur2 x-2-inc)))))
       (b (make-image-pair (list (blur2 y-t) (blur2 x-t))))
       (neg-v (matrix-mul b (matrix-inverse M))))
    (-. neg-v)))

;; First we look at sinusoidal gratings:
(setq *xdim* 32
      *tdim* 16
      *prior-offset* 1e-3)

(setq sine
      (make-image-sequence
       (loop with period = 8.0		;in pixels
	     with motion = 0.8		;in pixels
	     with angle = (/ pi 6) ;radians
	     for i from 0 below *tdim*
	     collect
	     (make-sin-grating (list *xdim* *xdim*)
			       :period period  :orientation angle
			       :phase (* 2-pi i (/ motion period))))))

;; Compute one frame of derivatives.  Why do these look the same?
;; What is the difference between them?
(setq dx-sine
      (frame 0 (apply-filter *dx-filt* sine :start-frame 8 :end-frame 9)))
(setq dy-sine
      (frame 0 (apply-filter *dy-filt* sine :start-frame 8 :end-frame 9)))
(setq dt-sine
      (frame 0 (apply-filter *dt-filt* sine :start-frame 8 :end-frame 9)))

;; Now, let's compute the flow field:
(setq sine-velocity (compute-2D-flow-from-derivs dt-sine dy-sine dx-sine))

;; In the 1D case, we found that there were singularities in places
;; where the derivatives were zero.  In 2D, there are also
;; singularities wherever the structure of the image is
;; one-dimensional (this is often called the "aperture" problem).  In
;; the case of the sine grating, the entire image is one-dimensional.
;; We eliminated the singularity by adding a small offset (named
;; *prior-offset* above) to the diagonals of the matrix M.  This
;; *prior-offset* biases the velocity estimate slightly to slower
;; speeds.

;; Now, construct a plaid of gratings:
(setq plaid
      (make-image-sequence
       (loop with period1 = 8.0
	     with period2 = 8.0
	     with motion1 = 0.8
	     with motion2 = 0.8
	     with angle1 = (/ pi 6) ;radians
	     with angle2 = (/ pi -6) ;radians
	     for i from 0 below *tdim*
	     collect
	     (with-local-viewables
		 ((sine1 (make-sin-grating (list *xdim* *xdim*)
					   :period period1  :orientation angle1
					   :phase (* 2-pi i (/ motion1 period1))))
		  (sine2 (make-sin-grating (list *xdim* *xdim*)
					   :period period2  :orientation angle2
					   :phase (* 2-pi i (/ motion2 period2)))))
	       (+. sine1 sine2)))))

;; Compute one frame of derivatives.
(setq dx-plaid
      (frame 0 (apply-filter *dx-filt* plaid :start-frame 8 :end-frame 9)))
(setq dy-plaid
      (frame 0 (apply-filter *dy-filt* plaid :start-frame 8 :end-frame 9)))
(setq dt-plaid
      (frame 0 (apply-filter *dt-filt* plaid :start-frame 8 :end-frame 9)))

;; And compute the flow field:
(display (setq plaid-velocity 
	       (compute-2D-flow-from-derivs dt-plaid dy-plaid dx-plaid))
	 'vector-field :scale 1.5)

;; Last, try a random-dot image:
(setq dots
      (make-image-sequence
       (loop with im = (coerce-to-float
			(make-random-dots (list *xdim* *xdim*) :density 0.2))
	     for i from 0 below *tdim*
	     collect
	     (circular-shift im :x i))))

(setq dx-dots
      (frame 0 (apply-filter *dx-filt* dots :start-frame 1 :end-frame 2)))
(setq dy-dots
      (frame 0 (apply-filter *dy-filt* dots :start-frame 1 :end-frame 2)))
(setq dt-dots
      (frame 0 (apply-filter *dt-filt* dots :start-frame 1 :end-frame 2)))

;; And compute the flow field:
(display (setq dots-velocity 
	       (compute-2D-flow-from-derivs dt-dots dy-dots dx-dots))
	 'vector-field :scale 1.5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Some examples on more interesting image sequences.

;; Clean up:
(purge!)
(set-default 'gray :zoom 1)

;; Yosemite: This sequence is not a "real" image sequence.  It was
;; created by taking a real aerial photograph of the yosemite valley,
;; and a real range-image (height map) of the yosemite valley, and
;; rendering the photograph onto the range image for a sequence of
;; camera positions.  The whole sequence of image was rendered from a
;; single photograph.  This makes the motion computation a bit easier
;; because there are no confounding "photometric" factors like changes
;; in the lighting over time.  Using this sequence, therefore, is a
;; mild form of cheating!  The advantage is that we can compute the
;; ACTUAL flow fields, since we know the camera parameters (focal
;; length) and position.

;; First, we load the image sequence.  Evaluate the following
;; expression to set the path for the yosemite images.  If you don't
;; have these images on your machine, you can get them via anonymous
;; ftp from white.stanford.edu.

(setq *yosemite-path* "/home/heeger/images/yosemite/")

;; Now use C-c c to compile this function:

(defun load-yos-image (path &key ->)
  (let* ((im (apply 'load-image
		    (merge-pathnames path *yosemite-path*)
		    (when -> (list :-> ->))))
	 (base-info (car (obvius::info-list im))))
    (setf (obvius::info-list im)
	  (cons (car base-info) (list (cdr base-info))))
    im))

;; Now, load the images:

(setq *tdim* 6)				;max: 15
(setq yosemite
      (make-image-sequence
       (loop for i from 0 below *tdim*
	     for name = (format nil "yos-images/yos~D" i)
	     for filename = name
	     collect (load-yos-image filename :-> name))))

;; Load the actual flow field corresponding to frame #2.
(setq actual-flow-2 (load-image (merge-pathnames
				 "yos-flows/actual-flow2"
				 *yosemite-path*)))

;; Set some parameters (see multi-scale-motion.lisp for others)
(setq *prior-offset* 1.0
      *flow-scale* 3.5
      *start-load* 0)

;; Maximum Motions in this sequence are roughly 4 pixels, so build a
;; 2-level pyramid.
(setq pyr
      (make-gaussian-pyramid
       yosemite
       :filter (make-separable-filter *pyr-kernel* *pyr-kernel*
				      :step-vector '(2 2)
				      :edge-handler *pyr-edges*)
       :level 3
       :display-type nil))

;; Look at the top level (mini-yosemite):
(access pyr 2)

;; Run the multi-scale algorithm.  It pops up intermediate results, as
;; it goes from one scale to the next.  WARNING: lots-o-computin' !
(display (setq flows (compute-multi-scale-flow pyr :display-frame 0))
	 'flipbook
	 :sub-display-type 'vector-field
	 :scale 3.5)

;; Look at the error between the actual flow and the estimated flow,
;; and display it at the same scale:
(-. (frame flows 2)  ACTUAL-FLOW-2)
(setp scale 3.5)			;set scale

;; Note that the worst errors occur near the boundaries.  We can't do
;; anything about that.  The derivative measurements can not be
;; computed correctly near the boundaries, due to edge effects of the
;; convolutions.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Trees sequence: a real sequence (from a camera).  See
;; images/trees/README for information about this image
;; sequence.

;; Clean up:
(purge!)

;; Load sequence:

(setq *trees-path* "/home/heeger/images/trees/")

(setq trees
      (make-image-sequence
       (loop for i from 0 below *tdim*
	     for name = (format nil "trees-04~D.pic" i)
	     for filename = (merge-pathnames (format nil "~A" name)
					     *trees-path*)
	     collect (load-image filename :xsize 256 :ysize 233 :-> name))))

(setq pyr
      (make-gaussian-pyramid
       trees
       :filter (make-separable-filter *pyr-kernel* *pyr-kernel*
				      :step-vector '(2 2)
				      :edge-handler *pyr-edges*)
       :level 2
       :display-type nil))

(display (setq flows (compute-multi-scale-flow pyr :display-frame 0))
	 'flipbook
	 :sub-display-type 'vector-field
	 :scale 3.5)

;; How do we test the quality of this optical flow?  We do not know
;; the "correct" flow.  The only thing we can do (besides qualitiative
;; judgement) is to warp a given frame using the corresponding flow
;; field in order to predict the following frame.

(setq predicted-frame3
      (warp (frame trees 2)
	    (frame flows 1)))

;; It's easiest to see the errors by cycling through a movie of the
;; correct frame 3 and the predicted frame 3:
(make-image-sequence (list predicted-frame3 (frame trees 3)))

;; Compare that with the original two frames
(make-image-sequence (list (frame trees 2) (frame trees 3)))

;; Note that many of the errors are again near occlusion boundaries.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
