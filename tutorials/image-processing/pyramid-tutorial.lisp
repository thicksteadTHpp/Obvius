;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: pyramid-tutorial.lisp
;;;  Author: Heeger
;;;  Description:
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(obv-require :matrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; LAPLACIAN PYRAMIDS: Images are composed of information at
;;;; different scales.

(setq im (copy (load-image (merge-pathnames "images/einstein"
					    obv::*obvius-directory-path*))))

;;; Blurring eliminates the fine scale detail:
(setq lo-filt (make-separable-filter gauss-5 gauss-5))
(setq blurred (apply-filter lo-filt im))

;;; Subtracting the blurred image from the original leaves ONLY the fine scale
;;; detail:
(setq fine0 (sub im blurred))

;;; Trivially, adding the blurred image to the fine scale detail will
;;; reconstruct the original:
(setq reconstructed (add fine0 blurred))
(mean-square-error reconstructed im)

;;; Since the filter is a lowpass filter, we might want to subsample the
;;; blurred image.  This may cause some aliasing (depends on the filter), but
;;; the decomposition structure given above will still be possible.
(setq lo-filt (let ((f (make-separable-filter gauss-5 gauss-5 :step-vector '(2 2))))
		(mul f 2 :-> f)))
(setq blurred1 (apply-filter lo-filt im))

;;; Now, to extract fine scale detail, we must interpolate the image back up to
;;; full size, and then subtract.  The "expand-filter" function does upsampling
;;; (padding with zeros between samples) and convolution.  This can be done
;;; using the lowpass filter that was applied before subsampling or it can be
;;; done with a different filter.
(setq fine1 (sub im (expand-filter blurred1 lo-filt)))

;;; Note that this looks similar to the previous fine detail image.  Again,
;;; trivially, the RECONSTRUCTION IS EXACT, even if the blurred1 image
;;; contained aliasing:
(setq reconstructed (add fine1 (expand-filter blurred1 lo-filt)))
(mean-square-error reconstructed im)

;;; clean up some memory:
(ogc)

;;; We now have a technique that takes an image, computes two new images
;;; (blurred1 and fine1 above) that contain the coarse scale information and
;;; the fine scale information.  We can also (trivially) reconstruct the
;;; original rom these two.  That is, we have an invertible IMAGE TRANSFORM.

;;; Often, we will want further subdivisions of scale.  For example, we might
;;; want to decompose the coarse information into medium coarse and very coarse
;;; bands.  We can do this by applying the same splitting technique to blurred
;;; image (blurred1):
(setq blurred2 (apply-filter lo-filt blurred1))
(setq fine2 (sub blurred1 (expand-filter blurred2 lo-filt)))

;;; Since blurred2 and fine2 can be used to reconstruct blurred1, and fine1 and
;;; blurred1 can be used to reconstruct the original image, the set of
;;; {blurred2, fine2, fine1} constitute a complete REPRESENTATION of the
;;; original image.

;;; We can continue this process, recursively splitting the blurred image into
;;; coarser and finer detail (kind of like peeling off layers of an onion.  The
;;; resulting data structure is known as a "Laplacian Pyramid".  We will see
;;; the reason for this name shortly.

;;; First, we want to show exactly what information is stored in each of the
;;; pyramid images.  The reconstruction process involves recursively
;;; interpolating these images and then adding them to the image at the next
;;; finer scale.  To see the contribution of one of the representation images
;;; (say blurred2) to the reconstruction, we imagine filling all the other
;;; representation images with zeros and then following our reconstruction
;;; procedure.  This is equivalent to simply interpolating blurred2 up to full
;;; size:
(setq blurred2-full (expand-filter (expand-filter blurred2 lo-filt) lo-filt))

;;; And similarly for fine2.  Note the difference between fine1 and fine2.
(setq fine2-full (expand-filter fine2 lo-filt) )
(progn (next-pane) (display fine1))

;;; If we did everything correctly, we should be able to literally add together
;;; these three full-size components to reconstruct the original image:
(setq reconstructed (+. blurred2-full fine2-full fine1))
(mean-square-error reconstructed im)	;should be zero

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; OBVIUS interface to Gaussian and Laplacian pyramids:

;;; To make things easier, we have bundled these operations and data
;;; structures into an object in OBVIUS.
(ogc)					; clean up
(obv-require :gaussian-pyramid)		; load the code

;;; Here are some examples of how to use the functions that Obvius provides for
;;; making and manipulating Gaussian and Laplacian pyramids.  Use C-c . and C-c
;;; C-a to get arglists and look at source code.

;;; In OBVIUS, the gauss-out function does filtering-subsampling (reduce) and
;;; the gauss-in function does upsampling-filtering (expand).  The blur
;;; function does these in cascade:
(blur einstein :level 2)
(gauss-in (gauss-in (gauss-out (gauss-out einstein))))

;;; Make a gaussian pyramid and build it to level 3.
(setq gpyr (make-gaussian-pyramid einstein))
(build gpyr 3)
(refresh)

;;; You might need to use C-Sh-right (drag) to see all of the subbands.

;;; Nuke it:
(destroy gpyr)

;;; You can also do:
(setq gpyr (make-gaussian-pyramid einstein :level 2))

;;; and use build to make more levels when you want:
(build gpyr 4)
(refresh)

;;; To access the bands:
(access gpyr 1)
(access gpyr 2)
(access gpyr 3)

;;; To make a Laplacian pyramid:
(setq lpyr (make-laplacian-pyramid einstein :level 2))

;;; You can use build to make more levels:
(build lpyr 4)
(refresh)

;;; Obvius displays only the bandpass images.  To get the low-pass band:
(low-band lpyr)

;;; Note that the fully expanded low-band is truely a low-pass filter,
;;; i.e., it preserves the dc:
(mean (low-band lpyr))
(mean einstein)

;;; To access the other levels:
(access lpyr 0)
(access lpyr 1)
(access lpyr 2)
(access lpyr 3)

;;; To collapse:
(setq reconstructed-einstein (collapse lpyr))
(mean-square-error reconstructed-einstein einstein)

;;; OBVIUS uses 14641 filters by default for both the Gaussian and Laplacian
;;; pyramids.  You can specify other filters:
(setq gauss-3 '(1/4 1/2 1/4))
(setq gpyr3 (make-gaussian-pyramid
	     einstein
	     :level 4
	     :filter (make-separable-filter gauss-3 gauss-3
					    :step-vector '(2 2))))

;;; Here we make a "Laplacian" pyramid using random filters.  Forward-filt is
;;; used for the reduce operations and inverse-filt is used for the expand
;;; operations.  It is a simple fact of the Laplacian pyramid that we can use
;;; ANY filters and we will still be able to reconstruct perfectly.
(progn
  (setq random-kernel-1 (randomize '(0 0 0 0 0) 1.0))
  (setq random-kernel-2 (randomize '(0 0 0 0 0) 1.0))
  (setq random-kernel-3 (randomize '(0 0 0 0 0) 1.0))
  (setq random-kernel-4 (randomize '(0 0 0 0 0) 1.0))
  (setq forward-filt (make-separable-filter random-kernel-1 random-kernel-2
					    :step-vector '(2 2)))
  (setq inverse-filt (make-separable-filter random-kernel-3 random-kernel-4
					    :step-vector '(2 2)))
  (display (setq lpyr-rand (make-laplacian-pyramid einstein
						   :level 3
						   :forward-filter forward-filt
						   :inverse-filter inverse-filt))
	   'pasteup :independent-parameters t))
(mean-square-error (collapse lpyr-rand) einstein)

;;; Clean up:
(ogc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; PROJECTION and BASIS functions:

;;; What about the PROJECTION and BASIS functions of the Laplacian pyramid
;;; transform?  Let's consider these in one dimension for simplicity.  First we
;;; build a pyramid on a 1D image of zeroes:
(setq im (make-image '(1 64)))
(setq pyr (make-laplacian-pyramid im :level 2 :display-type nil 
				  :forward-filter (make-filter gauss-5)))

;;; The BASIS function corresponding to a given coefficient tells us how much
;;; that coefficient contributes to each pixel in the reconstructed image.  We
;;; can construct the basis function by setting the coefficient to 1.0 (and all
;;; others to zero) and reconstructing.  That is, we fill one band of the
;;; pyramid with an impulse, and collapse the pyramid:
(let ((band (access pyr 0)))
  (zero! pyr)
  (setf (iref band 0 (floor (x-dim band) 2)) 1.0)
  (setq basis-fn1 (collapse pyr)))

(let ((band (access pyr 1)))
  (zero! pyr)
  (setf (iref band 0 (floor (x-dim band) 2)) 1.0)
  (setq basis-fn2 (collapse pyr)))

(let ((band (low-band pyr)))
  (zero! pyr)
  (setf (iref band 0 (floor (x-dim band) 2)) 1.0)
  (setq basis-fn-low (collapse pyr)))

;;; Note that all three of these basis functions are lowpass
;;; (Gaussian-like) functions.

;;; Now, to see the PROJECTION functions, we need to ask how much of
;;; each pixel in the input image contributes to a given coefficient.
(loop with res = (similar im)
      with ctr = (floor (total-size im) 2)
      for i from 0 below (total-size im)
      do
      (zero! im)
      (setf (iref im 0 i) 1.0)
      (with-local-viewables ((pyr (make-laplacian-pyramid im :level 1 :forward-filter
							  (make-filter gauss-5))))
	(setf (iref res 0 i) (iref (access pyr 0) 0 ctr)))
      finally (return (setq proj-fn1 res)))

(loop with res = (similar im)
      with ctr = (floor (total-size im) 2)
      for i from 0 below (total-size im)
      do
      (zero! im)
      (setf (iref im 0 i) 1.0)
      (with-local-viewables ((pyr (make-laplacian-pyramid im :level 2 :forward-filter
							  (make-filter gauss-5))))
	(setf (iref res 0 i) (iref (access pyr 1) 0 ctr)))
      finally (return (setq proj-fn2 res)))

(loop with res = (similar im)
      with ctr = (floor (total-size im) 2)
      for i from 0 below (total-size im)
      do
      (zero! im)
      (setf (iref im 0 i) 1.0)
      (with-local-viewables ((pyr (make-laplacian-pyramid im :level 2 :forward-filter
							  (make-filter gauss-5))))
	(setf (iref res 0 i) (iref (low-band pyr) 0 ctr)))
      finally (return (setq proj-fn-low res)))

;;; Let's look at the frequency response of these functions:
(power-spectrum proj-fn1 :center t)
(power-spectrum proj-fn2 :center t)
(power-spectrum proj-fn-low :center t)

;;; The first projection function is highpass, and the second is bandpass.
;;; Both of these look something like the laplacian (2nd derivative) of a
;;; gaussian.  The last is lowpass, as are the basis functions.  The basic
;;; operation used to create each level of the pyramid involves a simple
;;; highpass/lowpass split.

;;; Because they are even symmetric, the projection functions they are the same
;;; as the impulse responses:
(setq impulse (make-impulse '(1 64)))
(setq pyr (make-laplacian-pyramid impulse :level 2 :display-type nil 
				  :forward-filter (make-filter gauss-5)))
(setq impulse-response1 (access pyr 0))
(setq impulse-response2 (access pyr 1))
(setq impulse-response-low (low-band pyr))

(mean-square-error impulse-response1 proj-fn1)
(mean-square-error impulse-response2 proj-fn2)
(mean-square-error impulse-response-low proj-fn-low)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Matrix formulation of the projection functions:

;;; We can bundle these all together in a transform matrix to show that they
;;; behave properly.  Here the reduce matrices do blurring and then
;;; subsampling.  The expand matrices do upsampling and then blurring.  The G
;;; matrices are the product of a bunch of reduce matrices.  Each G matrix
;;; correspond to the transformation from the original signal to a level of the
;;; Gaussian pyramid.  That is, the rows of the G matrices are the projection
;;; functions of the Gaussian pyramid.  The D matrices are made by taking a G
;;; matrix, and subtracting from it a reduced and expanded version of itself.
;;; That is, each D matrix corresponds to the transformation from the original
;;; signal to a level of the Laplacian pyramid.  The rows of the D matrices are
;;; the (highpass and bandpass) projection functions of the Laplacian pyramid.

(progn
  (setq reduce-0 (make-array '(32 64) :element-type 'single-float))
  (setq reduce-1 (make-array '(16 32) :element-type 'single-float))
  (setq reduce-2 (make-array '(8 16) :element-type 'single-float))
  (loop for reduce in (list reduce-0 reduce-1 reduce-2) do
	(loop with row = (circular-shift
			  (paste (make-matrix gauss-5)
				 (make-array (col-dim reduce) :element-type 'single-float))
			  :x -2)
	      for j from 0 below (row-dim reduce)
	      for displaced-row = (displaced-row j reduce) do
	      (paste (circular-shift row :x (* 2 j))
		     displaced-row :-> displaced-row)))
  (setq expand-0 (matrix-transpose reduce-0))
  (setq expand-1 (matrix-transpose reduce-1))
  (setq expand-2 (matrix-transpose reduce-2)))

;;; Projection functions of the Gaussian pyramid:
(progn
  (setq G0 reduce-0)
  (setq G1 (matrix-mul reduce-1 G0))
  (setq G2 (matrix-mul reduce-2 G1)))
(setq slice-G0 (circular-shift (make-slice (make-image G0) :y 0) :x 32))
(setq slice-G1 (circular-shift (make-slice (make-image G1) :y 0) :x 32)) 
(setq slice-G2 (circular-shift (make-slice (make-image G2) :y 0) :x 32))

;;; Projection functions of the Laplacian pyramid:
(progn
  (setq D0 (sub (make-identity-matrix 64) (matrix-mul expand-0 G0)))
  (setq D1 (sub G0 (matrix-mul expand-1 G1)))
  (setq D2 (sub G1 (matrix-mul expand-2 G2))))
(setq slice-D0 (circular-shift (make-slice (make-image D0) :y 0) :x 32))
(setq slice-D1 (circular-shift (make-slice (make-image D1) :y 0) :x 32))
(setq slice-D2 (circular-shift (make-slice (make-image D2) :y 0) :x 32))

;;; The entire Laplacian pyramid can be computed as one BIG matrix multiply,
;;; stacking the D matrices on top of one another with the leftover lowpass G2
;;; matrix at the bottom:
(progn
  (setq lap-mat
	(make-array '(120 64) :element-type 'single-float))
  (loop for j from 0 below 64 do
	(loop for i from 0 below 64 do
	      (setf (aref lap-mat j i) (aref D0 j i))))
  (loop for j from 0 below 32 do
	(loop for i from 0 below 64 do
	      (setf (aref lap-mat (+ j 64) i) (aref D1 j i))))
  (loop for j from 0 below 16 do
	(loop for i from 0 below 64 do
	      (setf (aref lap-mat (+ j 96) i) (aref D2 j i))))
  (loop for j from 0 below 8 do
	(loop for i from 0 below 64 do
	      (setf (aref lap-mat (+ j 112) i) (aref G2 j i)))))
(setq lap-im (make-image lap-mat))

;;; Taking the pseudo-inverse of this matrix is one way to invert the Laplacian
;;; pyramid:
(setq lap-inv (matrix-inverse lap-mat))
(identity-p (matrix-mul lap-inv lap-mat))

;;; The cols of lap-inv are one possible set of basis functions:
(setq lap-inv-im (make-image lap-inv))
(circular-shift (make-slice lap-inv-im :x 0) :x 32)
(circular-shift (make-slice lap-inv-im :x 64) :x 32)
(circular-shift (make-slice lap-inv-im :x 96) :x 32)
(circular-shift (make-slice lap-inv-im :x 112) :x 32)

;;; Notice that these are different from the basis functions we computed
;;; before.  Before the basis functions were all lowpass (Gaussian-like bumps).
;;; Now, some of them are bandpass and some are lowpass.  The Laplacian pyramid
;;; is overcomplete.  There are more coefficients in the transform than there
;;; were samples in the original signal.  The lap-mat matrix has more rows than
;;; columns.  That means that the inverse is not unique.  There is more than
;;; one way to reconstruct the original image from the pyramid coefficients.

;;; clean up:
(ogc)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ALIASING in the Gaussian and Laplacian pyramids:

;;; Unless you are careful, the subsampling operations will introduce aliasing
;;; artifacts in these pyramid transforms.  This is true even though the
;;; Laplacian pyramid can be used to reconstruct the original image perfectly.
;;; When reconstructing, the pyramid is designed in such a way that these
;;; aliasing artifacts cancel out.  So it's not a problem if the only thing we
;;; want to do is collapse/reconstruct.  However, it can be a serious problem
;;; if we intend to process each of the subbands independently.

;;; Here's one way to see the consequences of the aliasing artifacts.  by
;;; checking for shift-invariance.  We start with einstein and shift him by one
;;; pixel.  Then we'll blur (filter-downsample-upsample-filter) the original
;;; einstein and we'll blur the shifted einstein.  Now if there's no aliasing,
;;; then the system is shift invariant, i.e.,
;;; shift-filter-downsample-upsample-filter is the same as
;;; filter-downsample-upsample-filter-shift.  We'll do this for 2 different
;;; filters (the standard 5 tap filter and a 3 tap filter), and see that the
;;; aliasing is much worse for the 3 tap filter.

(setq im (make-gaussian-noise '(128 128) :variance 1e3))

(progn
  (setq gauss-3 '(1/4 1/2 1/4))
  (setq shift-blur3 (circular-shift
		     (blur im
			   :level 1
			   :kernel (*. (sqrt 2) gauss-3)
			   :edge-handler nil)
		     :x 1))
  (setq blur3-shift (blur (circular-shift im :x 1)
			  :level 1
			  :kernel (*. (sqrt 2) gauss-3)
			  :edge-handler nil))
  (mean-square-error shift-blur3 blur3-shift))

(progn
  (setq shift-blur5 (circular-shift
		     (blur im
			   :level 1
			   :kernel (*. (sqrt 2) gauss-5)
			   :edge-handler nil)
		     :x 1))
  (setq blur5-shift (blur (circular-shift im :x 1)
			  :level 1
			  :kernel (*. (sqrt 2) gauss-5)
			  :edge-handler nil))
  (mean-square-error shift-blur5 blur5-shift))

;;; clean up:
(ogc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STOP HERE: You will go through the rest of this tutorial for assignment 4.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; QMF pyramids.

;;; Two things about Laplacian pyramids seem a bit disturbing.  The first is
;;; that there are more pixels (coefficients) in the representation than in the
;;; original image.  Secondly, the "bandpass" images (fineN) do not segregate
;;; information according to orientation.

;;; There are, however, other varieties of pyramid.  We will now discuss a
;;; version based a particular type of filter known as a "Quadrature Mirror
;;; Filter" or QMF.

;;; Recall that the Laplacian pyramid is formed by simple hi/low splitting at
;;; each level.  The lowpass band is subsampled by a factor of 2, but the
;;; highpass band is NOT subsampled.  In the QMF pyramid, we apply two filters
;;; (hi- and lo- pass) and subsample BOTH by a factor of 2.  The two filters
;;; have a special relationship to each other which we will discuss.
(setq lo-qmf (make-filter qmf-9 :step-vector '(2)))
(setq hi-qmf (shift-by-pi lo-qmf :step-vector '(2) :start-vector '(1)))

;;; We split an input signal into two bands as follows:
(setq im (make-fractal '(1 64)))
(setq lo1 (apply-filter lo-qmf im))
(setq hi1 (apply-filter hi-qmf im))

;;; Notice that these are half the size of the original image, due to
;;; subsampling.

;;; Now, we can reconstruct the original image by interpolating these
;;; two subbands USING THE SAME FILTERS:
(progn
  (setq reconstructed (expand-filter lo-qmf lo1))
  (add reconstructed (expand-filter hi-qmf hi1) :-> reconstructed))
(mean-square-error reconstructed im)

;;; So we've fixed one of the problems that we had with Laplacian pyramid.  The
;;; qmf transform is not overcomplete.  The number of transform coefficients
;;; equals the number of samples in the original image.

;;; As in the Laplacian pyramid, we can recursively apply the transform to the
;;; low pass band (see below).

;;; Why does it work?  Let's look at the projection functions.  These are just
;;; copies of the filters, shifted by multiples of two samples:
(progn
  (setq M (make-array '(64 64) :element-type 'single-float))
  (loop with prototype = (paste (kernel lo-qmf) (make-array 64 :element-type 'single-float))
	for row from 0 below 32
	do
	(circular-shift prototype :x (- (* 2 row) 4) :-> (displaced-row row M)))
  (loop with prototype = (paste (kernel hi-qmf) (make-array 64 :element-type 'single-float))
	for row from 32 below 64
	do
	(circular-shift prototype :x (- (* 2 row) 3) :-> (displaced-row row M))))
(setq proj-im (image-from-array M))

;;; The transform matrix is composed of two sub-matrices.  The top half
;;; contains the lowpass kernel, shifted by increments of 2 samples.  The
;;; bottom half contains the highpass.  One subtle point: the lowpass kernels
;;; are centered on the EVEN samples and the highpass kernels are centered on
;;; the ODD samples.

;; Low-pass kernel shifted by 2 samples:
(make-slice proj-im :y 10)
(make-slice proj-im :y 11)

;; High-pass kernel shifted by 2 samples:
(make-slice proj-im :y 42)
(make-slice proj-im :y 43)

;;; Now we compute the inverse of this matrix:
(setq M-inv (matrix-inverse M))
(image-from-array M-inv)

;;; Nicely enough, the inverse is the transpose of the matrix!
(mean-square-error (matrix-transpose M-inv) M)

;;; In other words, the transform is orthonormal:
(identity-p (matrix-transpose-mul M M))

;;; *** Explain why the projection functions are orthonormal?  Lowpass kernel
;;; is designed to be orthogonal to itself shifted by multiples of two samples.
;;; Highpass is derived from lowpass by negating every other pixel, so it has
;;; same property of being orthogonal to itself shifted by multiples of two
;;; samples.  Also, highpass is orthogonal to lowpass, when shifted by an odd
;;; number of samples.  This is true because highpass is derived by negating
;;; every other sample of the lowpass.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Expand-filter and reconstruction:

;;; The projection matrix P is made up of two submatrices.  The top half
;;; contains the lowpass kernel, shifted by increments of 2 samples.  The
;;; bottom half contains the highpass filter, shifted by increments of 2
;;; samples.  We can think of P as two matrices (P0 and P1) stacked on top of
;;; one another:
;;;
;;;    (y0)   (  P0  )
;;;    (  ) = (      ) x
;;;    (y1)   (  P1  )
;;;
;;; where y0 and y1 are the 2 subbands.

;;; We invert the transform by using B=P^t, x' = By.  So we can think of B as
;;; two matrices next to one another:
;;;
;;;           ( |   | )
;;;      x' = ( B0 B1 ) y
;;;           ( |   | )
;;;
;;; where B0 = P0^t and likewise for B1 and P1.  In other words,
;;;
;;;      x' = B0 y0 + B1 y1
;;;
;;; P0 is the product of a circulant (or Toeplitz) convolution matrix, C0,
;;; multiplied by a subsampling matrix, Sd:
;;;
;;;      P0 = Sd C0     and     B0 = C0^t Su
;;;
;;; and likewise for P1 and B1.  

;;; To be more explicit, let's take x to be an N-vector.  Then Sd is an N/2xN
;;; downsampling matrix, Su is an NxN/2 upsampling matrix, and C0 is NxN.

;;; C0 has a simple form -- the cols are flipped copies of the rows.  I.e.,
;;; convolving with a row of C0^t is the same as correlating with a row of C0.
;;; The same is true for C1.  In Obvius, apply-filter does correlation and
;;; subsampling.  Expand-filter does upsampling and convolution:
;;;
;;;     Apply-filter computes:  y0 = Sd C0 x  and  y1 = Sd C1 x
;;;
;;;     Expand-filter computes:   x' = C0^t Su y0 + C1^t Su y1

;;; Thus, expand-filter is used to invert the transform (collapse the
;;; pyramid).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Let's look at the frequency responses of the filters:
(setq lo-response (power-spectrum lo-qmf :center t :dimensions '(64)))
(setq hi-response (power-spectrum hi-qmf :center t :dimensions '(64)))

;;; Notice that the filters TILE the freq domain.  That is, taken together they
;;; are an all-pass system:
(display (mul 64 (add lo-response hi-response))
	 'graph :y-range '(0 2.5))

;;; These are not perfect half-band (ideal) filters so when subsampled by a
;;; factor of two there will be some aliasing in each of the subbands.  See
;;; below for a discussion of the effect of this aliasing.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Analogous to the Laplacian pyramid, we can recursively apply the qmf
;;; band-splitting to the low pass band:
(setq lo2 (apply-filter lo-qmf lo1))
(setq hi2 (apply-filter hi-qmf lo1))
(setq lo3 (apply-filter lo-qmf lo2))
(setq hi3 (apply-filter hi-qmf lo2))

;;; And we can reconstruct, one level at a time:
(setq reconstructed
      (add (expand-filter hi-qmf hi1)
	   (expand-filter lo-qmf
			  (add (expand-filter hi-qmf hi2)
			       (expand-filter lo-qmf
					      (add (expand-filter hi-qmf hi3)
						   (expand-filter lo-qmf lo3)))))))
(mean-square-error reconstructed im)

;;; The reconstruction is not perfect because the design of the filters is not
;;; perfect.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To make things easier, we have bundled these qmf operations and data
;;; structures into an object in OBVIUS.
(setq pyr (make-qmf-pyramid im :level 2 :filters (list lo-qmf hi-qmf)))

;;; We use the ACCESS-BAND method to snarf individual bandpass subbands:
(access-band pyr :band 0 :level 0)
(access-band pyr :band 0 :level 1)

;;; We use the LOW-BAND method to access the final (low pass) band:
(low-band pyr)

;;; Now for 2D, we use separable filters.  There are 4 combinations of the two
;;; filters: (1) low in both x and y; (2) low in x and high in y; (3) low in y
;;; and high in x; (4) high in both x and y.  The pyramid is built by
;;; recursively subdividing the first (low-low) band into these 4 subbands.
(setq im (copy einstein))
(setq pyr (make-separable-qmf-pyramid im :lo-filt lo-qmf :level 4))

;;; As for the Laplacian pyramid, the LOW-BAND function returns the
;;; leftover lowpass band:
(low-band pyr)

;;; The ACCESS-BAND method can be used to pull out a single band:
(access-band pyr :band 0 :level 0 :-> "horizontal stuff")
(access-band pyr :band 1 :level 0 :-> "vertical stuff")
(access-band pyr :band 2 :level 0 :-> "diagonal stuff")

;;; The ACCESS method returns a sequence containing all 3 bands at a
;;; given level.
(access pyr 1)

;;; The COLLAPSE method can be used to reconstruct the entire pyramid,
;;; or to expand a subset of the bands:
(setq reconstructed (collapse pyr))
(mean-square-error reconstructed einstein)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Debauchies wavelet

;;; The qmf filters are not perfectly orthogonal.  In fact, it's impossible to
;;; construct a filter with an odd number of nonzero taps that is perfectly
;;; orthogonal to shifted copies (shifted by multiples of 2) of itself.  For
;;; example, let's say that there are 3 taps.  Shift by two and the right end
;;; of the original lines up with the left end of the shifted one.  You can't
;;; get zero by multiplying these two nonzero numbers.  The same reasoning will
;;; hold for any odd size.

;;; However, it is possible to construct perfectly orthogonal hi-band/lo-band
;;; basis using even-size filters.  Here's one example:
(setq daubechies-lo-taps
      (make-matrix '(.482962913145 .836516303738 .224143868042 -.129409522551)))
(setq daubechies-hi-taps
      (make-matrix '(.129409522551 .224143868042 -.836516303738 .482962913145)))

;;; Lo-taps constructed to be orthogonal to shifted copy of itself:
(dot-product daubechies-lo-taps (circular-shift daubechies-lo-taps :x 2))

;;; Hi-taps constructed from lo-taps by reversing their order and then
;;; multiplying every other tap by -1.  This guarantees that hi-taps will be
;;; orthogonal to lo-taps:
(dot-product daubechies-lo-taps daubechies-hi-taps)

;;; And hi-taps is orthogonal to shifted copies of itself:
(dot-product daubechies-hi-taps (circular-shift daubechies-hi-taps :x 2))

;;; And hi-taps is orthogonal to shifted copies of lo-taps:
(dot-product daubechies-hi-taps (circular-shift daubechies-lo-taps :x 2))

;;; Make filters:
(setq daubechies-lo-filter (make-filter daubechies-lo-taps :step-vector '(2)))
(setq daubechies-hi-filter (make-filter daubechies-hi-taps :step-vector '(2)))

;;; Look at the frequency responses:
(setq lo-response (power-spectrum daubechies-lo-filter :center t :dimensions '(64)))
(setq hi-response (power-spectrum daubechies-hi-filter :center t :dimensions '(64)))

;;; Check that they tile:
(display (mul 64 (add lo-response hi-response))
	 'graph :y-range '(0 2.5))

;;; Make a pyramid using the same code as before:
(setq daubechies-pyr
      (make-qmf-pyramid
       im :level 4
       :filters (list (make-separable-filter daubechies-lo-filter daubechies-lo-filter)
		      (make-separable-filter daubechies-hi-filter daubechies-lo-filter)
		      (make-separable-filter daubechies-lo-filter daubechies-hi-filter)
		      (make-separable-filter daubechies-hi-filter daubechies-hi-filter))))

;;; Perfect reconstruction:
(setq reconstructed (collapse daubechies-pyr))
(mean-square-error reconstructed im)

;;; Show that the projection matrix is perfectly orthonormal:
(progn
  (setq M (make-array '(64 64) :element-type 'single-float))
  (loop with prototype = (paste (kernel daubechies-lo-filter)
				(make-array 64 :element-type 'single-float))
	for row from 0 below 32
	do
	(circular-shift prototype :x (* 2 row) :-> (displaced-row row M)))
  (loop with prototype = (paste (kernel daubechies-hi-filter)
				(make-array 64 :element-type 'single-float))
	for row from 32 below 64
	do
	(circular-shift prototype :x (* 2 row) :-> (displaced-row row M))))
(setq proj-im (image-from-array M))
(identity-p (matrix-transpose-mul M M))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Effect of aliasing:

;;; Get one of the basis functions of the 2D Daubechies wavelet transform:
(progn
  (setq blank (make-image '(64 64)))
  (setq blank-pyr
      (make-qmf-pyramid
       blank :level 4
       :filters (list (make-separable-filter daubechies-lo-filter daubechies-lo-filter)
		      (make-separable-filter daubechies-hi-filter daubechies-lo-filter)
		      (make-separable-filter daubechies-lo-filter daubechies-hi-filter)
		      (make-separable-filter daubechies-hi-filter daubechies-hi-filter))))
  (setf (iref (access-band blank-pyr :level 1 :band 0) 8 8) 1.0)
  (setq im (collapse blank-pyr)))

;;; By construction, building a pyramid using this input will yield all but one
;;; zero coefficient.
(setq pyr1 (make-qmf-pyramid
	    im :level 4
	    :filters (list (make-separable-filter daubechies-lo-filter daubechies-lo-filter)
			   (make-separable-filter daubechies-hi-filter daubechies-lo-filter)
			   (make-separable-filter daubechies-lo-filter daubechies-hi-filter)
			   (make-separable-filter daubechies-hi-filter daubechies-hi-filter))))

;;; Look at the subbands and notice that there is only one nonzero coefficient:
(access pyr1 0)
(access pyr1 1)
(access pyr1 2)
(access pyr1 3)

;;; Now shift the input by one sample and re-build the pyramid.
(setq pyr2 (make-qmf-pyramid
	    (circular-shift im :x 1)
	    :level 4
	    :filters (list (make-separable-filter daubechies-lo-filter daubechies-lo-filter)
			   (make-separable-filter daubechies-hi-filter daubechies-lo-filter)
			   (make-separable-filter daubechies-lo-filter daubechies-hi-filter)
			   (make-separable-filter daubechies-hi-filter daubechies-hi-filter))))

;;; Look at the subbands of this pyramid and notice that many of the
;;; coefficients are non-zero.
(access pyr2 0)
(access pyr2 1)
(access pyr2 2)
(access pyr2 3)

;;; Clearly it is not a good thing when such a small change to the input
;;; results in such a large change in the output.

;;; We'd like it if the non-zero coefficients were confined to one band (all we
;;; did was shift the input by one pixel).  This band should be non-zero.
(range (access-band pyr2 :level 1 :band 0))

;;; But these bands also contain non-zero coefficients:
(range (access-band pyr2 :level 0 :band 1))
(range (access-band pyr2 :level 1 :band 2))

;;; This problem is not unique to the Daubechies transform.  The same is true
;;; for the qmf transform.  Try it...  In fact, the same kind of problem occurs
;;; for almost any orthogonal pyramid transform (the only exceptions are
;;; transforms constructed from sinc filters).

;;; Orthogonal pyramid transforms are not shift invariant.  Although
;;; orthogonality may be an important property for some applications (e.g.,
;;; data compression), orthogonal pyramid transforms are generally not so for
;;; image analysis.

;;; The overcompleteness of the Laplacian pyramid turns out to be a good thing
;;; in the end.  By using an overcomplete representation (and by choosing the
;;; filters properly to avoid aliasing as much as possible), you end up with a
;;; representation that is useful for image analysis.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Steerable Pyramid:

;;; The steerable pyramid has got it all.  The basis/projection functions are
;;; oriented (steerable) filters, localized in space and frequency.  It is
;;; overcomplete to avoid aliasing.  And it is self-inverting (like the qmf and
;;; Daubechies transforms).  For a complete theoretical treatment of the
;;; steerable pyramid see: Simoncelli, Freeman, Heeger, and Adelson, Shiftable
;;; Multiscale Transforms, IEE Trans.  on Info. Th., 38:587-607, 1992.

(obv-require :steer)

;;; First let's construct a pyramid for a zone plate (it takes a little while
;;; to construct the pyramid because some of the filters are big and
;;; non-separable):
(setq zone (make-zone-plate '(128 128)))
(setq zone-pyr (make-steerable-pyramid zone :level 3))

;;; Collapse the pyramid.
(setq reconstructed (collapse zone-pyr))
(mean-square-error reconstructed zone)
;;; The reconstruction is not perfect because there is some error in the filter
;;; design, but it's pretty good.

;;; We can get any orientation from any of the bands:
(steer (access zone-pyr 0) (* 1/4 pi))
(steer (access zone-pyr 0) (* 1/2 pi))
(steer (access zone-pyr 0) 0)
(steer (access zone-pyr 1) 0)
(steer (access zone-pyr 2) 0)

;;; Now do the same on a real image:
(setq ein-pyr (make-steerable-pyramid einstein :level 4))
(steer (access ein-pyr 0) (* 1/2 pi))
(steer (access ein-pyr 0) 0)
(steer (access ein-pyr 1) 0)
(steer (access ein-pyr 2) 0)
(steer (access ein-pyr 3) 0)
(low-band ein-pyr)

;;; The steerable pyramid is overcomplete.  Only the lowpass images are
;;; subsampled.  Note that the level 0 bandpass images are each the same size
;;; as the original image.  The bandpass images are not subsampled to avoid
;;; aliasing.

;;; Since it is overcomplete, it is certainly not orthogonal.  Even so, it is
;;; self-inverting (the basis functions are equal to the projection functions).
;;; Let's look at some of the basis/projection functions:
(setq blank (make-image '(128 128)))
(setq blank-pyr (make-steerable-pyramid blank :level 3))

(let* ((band 0)
       (level 0)
       (band (access-band blank-pyr :level level :band band)))
  (zero! blank-pyr)
  (setf (iref band (floor (y-dim band) 2) (floor (x-dim band) 2)) 1.0)
  (setq basis0 (collapse blank-pyr)))

(let* ((band 0)
       (level 1)
       (band (access-band blank-pyr :level level :band band)))
  (zero! blank-pyr)
  (setf (iref band (floor (y-dim band) 2) (floor (x-dim band) 2)) 1.0)
  (setq basis1 (collapse blank-pyr)))

(let* ((band 0)
       (level 2)
       (band (access-band blank-pyr :level level :band band)))
  (zero! blank-pyr)
  (setf (iref band (floor (y-dim band) 2) (floor (x-dim band) 2)) 1.0)
  (setq basis2 (collapse blank-pyr)))

;;; And the frequency responses:
(setq freq-resp0 (power-spectrum basis0 :center t))
(setq freq-resp1 (power-spectrum basis1 :center t))
(setq freq-resp2 (power-spectrum basis2 :center t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
