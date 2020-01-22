;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: sampling-tutorial.lisp
;;;  Author: Heeger
;;;  Description:
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(obv-require :matrix)
(set-default 'graph :x-tick-step 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Subsampling is best thought of in two steps.  The first step is
;; multiplication by an impulse train.  The second step is to get rid
;; of the zero values in between impulses.

;; The transform of impulse train is itself an impulse train:
(display
 (setq impulse-train (make-synthetic-image
		      64
		      '(lambda (x) (if (zerop (mod x 4)) 1.0 0.0))
		      :x-range '(0 63)))
 'graph :graph-type :bar)
(display
 (setq mag-ft-train (magnitude (fft impulse-train :post-center t)))
 'graph :graph-type :bar :y-axis nil)

;; Multiplication in space yields convolution in frequency.  Thus,
;; multiplying a signal by an impulse train yields a frequency domain
;; represenation with replicas.

(setq gaussian-12 (make-synthetic-image
		   64
		   '(lambda (x) (exp (- (/ (sqr (- x 32)) (sqr 12)))))
		   :x-range '(0 63)))
(setq mag-ft-gaussian-12 (magnitude (fft gaussian-12 :center t)))
(display (setq sampled-gaussian-12 (mul impulse-train gaussian-12))
	 'graph :graph-type :bar)
(display
 (setq mag-ft-sampled-gaussian-12
       (magnitude (fft sampled-gaussian-12 :center t)))
 'graph :y-range '(0.0 0.8))

;; If we make the Gaussian narrower in the space domain, then it
;; becomes broader in the frequency domain.  If we make it narrow
;; enough in space, then the frequency domain replicas start to
;; overlap.

(setq gaussian-4 (make-synthetic-image
		  64
		  '(lambda (x) (exp (- (/ (sqr (- x 32)) (sqr 4)))))
		  :x-range '(0 63)))
(setq mag-ft-gaussian-4 (magnitude (fft gaussian-4 :center t)))
(display (setq sampled-gaussian-4 (mul impulse-train gaussian-4))
	 'graph :graph-type :bar)
(display
 (setq mag-ft-sampled-gaussian-4
       (magnitude (fft sampled-gaussian-4 :center t)))
 'graph :y-range '(0.0 0.5))

;; In this example, the replicas overlap or cross into one another.
;; This situation is called "aliasing". It is called aliasing because
;; the sampled signal is masquerading as a different signal.  For
;; example, let's look at two sinusoids that alias to one another when
;; sampled.

(setq sine-4 (make-synthetic-image
	      64
	      #'(lambda (x) (- (sin (* 2-pi (/ 4 64) x))))
	      :x-range '(0 63)))
(setq sine-12 (make-synthetic-image
	       64
	       #'(lambda (x) (sin (* 2-pi (/ 12 64) x)))
	       :x-range '(0 63)))
(display (setq sampled-sine-4 (mul impulse-train sine-4))
	 'graph :graph-type :bar :x-axis 0.0)
(display (setq sampled-sine-12 (mul impulse-train sine-12))
	 'graph :graph-type :bar :x-axis 0.0)

;; Even though sine-4 and sine-12 are distinct signals, their sampled
;; versions are indistinguishable.  Sampled-sine-4 is an alias for
;; sampled-sine-12.

;; *** For a bandlimited signal, use impulse train at or above Nyquist
;; rate (twice the highest frequency) to avoid aliasing.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Reconstruction:

;; If there is no aliasing, we can reconstruct the original signal
;; from the sampled signal.  One easy way to do this is in the
;; frequency domain, by getting rid of the replicas, and then using
;; the inverse Fourier transform.  Why is that the right thing to do?
;; Well let's go back and compare the Fourier transform of the
;; original signal to that of the sampled signal.

(display mag-ft-gaussian-12)
(display mag-ft-sampled-gaussian-12)

;; The main difference between the two that there are replicas.
;; There's also a scale factor of 4 (note that we subsampled by a
;; factor of 4), but that'll be easy to fix.  To get rid of the
;; replicas in the frequency domain, we multiply by a box (also known
;; as an "ideal" low-pass filter).

(setq box (make-synthetic-image 64 #'(lambda (x) (if (< 24 x 40) 1.0 0.0))
				:x-range '(0 63)))

;; Let's use the box to reconstruct to the (non-aliased) Gaussian:

(progn
  (setq reconstructed-ft-gaussian-12 (mul box (fft sampled-gaussian-12)))
  (setq reconstructed-gaussian-12
	(mul 4.0 (real-part (fft reconstructed-ft-gaussian-12 :pre-center t :inverse t)))))

;; However, for the aliased Gaussian it doesn't work.  The box filter
;; pulls out one section of the frequency domain.  In this case, the
;; section that is pulled out is already contaminated by parts of the
;; neighboring replicas.

(progn
  (setq reconstructed-ft-gaussian-4 (mul box (fft sampled-gaussian-4)))
  (setq reconstructed-gaussian-4
	(mul 4.0 (real-part (fft reconstructed-ft-gaussian-4 :pre-center t :inverse t)))))

;; Multiplying by the box in the frequency domain is the same as
;; convolving with its inverse Fourier transform in the space domain.
;; The inverse Fourier transform of a box is called a sinc filter:

(setq sinc (real-part (fft box :center t :inverse t)))

;; Now you see why it's easier to do reconstruction in the frequency
;; domain.  Convolution with such a big filter would take a long time.

;; We don't always need to use sinc interpolation to reconstruct the
;; original signal.  If there is room to spare between the replicas
;; then we can use a filter with a more gradual fall-off.  For
;; example, let's reconstruct the sine-4 from sampled-sine-4.  First
;; let's use sinc interpolation (as before):

(progn
  (setq reconstructed-ft-sine-4 (mul box (fft sampled-sine-4)))
  (setq reconstructed-sine-4
	(mul 4.0 (real-part (fft reconstructed-ft-sine-4 :pre-center t :inverse t)))))
(mean-square-error reconstructed-sine-4 sine-4)

;; Now let's use a different filter, that falls off smoothly on both
;; sides:

(setq cos-box (make-synthetic-image
	       64
	       #'(lambda (x)
		   (cond ((< x 20) 0.0)
			 ((< 20 x 28) (* 1/2 (+ 1 (cos (* (/ pi 8) (- x 28))))))
			 ((< 27 x 37) 1.0)
			 ((< 36 x 44) (* 1/2 (+ 1 (cos (* (/ pi 8) (- x 20))))))
			 (t 0.0)))
	       :x-range '(0 63)))
(progn
  (setq new-reconstructed-ft-sine-4 (mul cos-box (fft sampled-sine-4)))
  (setq new-reconstructed-sine-4
	(mul 4.0 (real-part (fft new-reconstructed-ft-sine-4 :pre-center t :inverse t)))))
(mean-square-error new-reconstructed-sine-4 sine-4)

;; Now let's look at the impulse response of this cos-box filter:

(setq cos-filter (real-part (fft cos-box :center t :inverse t)))

;; There are two points to be made here.  First, there is in general
;; more than one way to reconstruct a subsampled signal.  We have some
;; freedom in choosing the interpolation filter.  Second, the impulse
;; response of the cos-box filter much more compact (in space) than
;; the sinc filter, so in some cases we might decide to use
;; convolution to reconstruct rather than reconstructing in the
;; Fourier domain.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So far, we have only dealt with the first step of subsampling,
;; multiplying by an impulse train.  The second step is to get rid of the
;; zero values in between impulses.  Let's do this for the gaussian (the
;; one that was not aliased).

(progn
  (setq subsampled-gaussian-12 (make-image 16))
  (loop for i from 0 below 16 do
	(setf (iref subsampled-gaussian-12 i)
	      (iref sampled-gaussian-12 (* 4 i))))
  subsampled-gaussian-12)
(setq mag-ft-subsampled-gaussian-12
      (magnitude (fft subsampled-gaussian-12 :center t)))

;; Notice that this pulls out one cycle of the replicated frequency
;; domain.

;; It is easy to reconstruct the original signal from the fully
;; subsampled signal, just by padding out the frequency domain with
;; zeroes.

(progn
  (setq reconstructed-ft-gaussian-12
	(paste (fft subsampled-gaussian-12 :center t)
	       (make-complex-image (list (make-image 64) (make-image 64)))
	       :x-offset 24 :y-offset 0))
  (setq reconstructed-gaussian-12
	(mul 2.0 (real-part (fft reconstructed-ft-gaussian-12 :center t :inverse t)))))

;; ** Why multiply by 2?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Why is aliasing such a bad thing?  After all, we are not
;; necessarily interested in reconstructing the original signal.
;; Here's an example that demonstrates how serious a problem aliasing
;; can be for motion analysis.  This example also demonstrates how to
;; reduce aliasing by using a pre-filter, that is, applying a low-pass
;; filter before subsampling.

;; Make a slightly tilted line (this may generate an error about
;; static memory space.  If so, just type :c):

(progn
  (setq ramp (make-ramp '(250 250) :orientation (* 1/16 pi)))
  (setq line (coerce-to-float (*. (>. ramp 0.0) (<. ramp 2.0)))))

;; Make an image sequence of the line moving slowly to the right:
(setq line-seq
      (make-image-sequence
       (loop for i from 0 below 10
	     collect (circular-shift line :y-shift 0 :x-shift (* 2 i)))))
;; Use C-M-right to view the sequence.

;; Now we subsample the images in the sequence.  Because of the aliasing,
;; the subsampled sequence appears to move UP!

(setq subsample-filter (make-separable-filter '(0.0 0.0 1.0 0.0 0.0)
					      '(0.0 0.0 1.0 0.0 0.0)
					      :edge-handler :zero
					      :start-vector '(0 0)
					      :step-vector '(5 5)))
(setq subsampled-line-seq (apply-filter subsample-filter line-seq))

;; Let's try prefiltering the sequence before sampling.  When viewing
;; this blurred sequence, notice that there is still a little bit of
;; upward motion.  That is because the original line-seq images were
;; already slightly aliased.

(setq blurred-line-seq (blur line-seq :level 3 :edge-handler :zero))

;; When we subsample the blurred sequence, there isn't much aliasing.
;; The line appears to move mostly rightward.

(setq subsampled-blurred-line-seq (apply-filter subsample-filter blurred-line-seq))

;; What is going on?  It is the high frequency components that are
;; aliasing.  The low-pass (blurring) filter attenuates the high
;; frequency components, thereby reducing the impact of the aliasing.

;; Not any filter will do.  Since we are subsampling by a factor of 5,
;; you might think that averaging over a 5x5 patch will fix things.  NOT!

(setq box-filter (make-separable-filter '(0.2 0.2 0.2 0.2 0.2)
					'(0.2 0.2 0.2 0.2 0.2)
					:edge-handler :zero
					:start-vector '(0 0)
					:step-vector '(5 5)))
(setq subsampled-boxed-line-seq (apply-filter box-filter line-seq))

;; Why doesn't this work.  Let's look at the frequency response of the
;; box filter:
(setq impulse-response (apply-filter
			(make-filter '(0.2 0.2 0.2 0.2 0.2))
			(make-impulse '(1 64))))
(setq mag-freq-response (magnitude (fft impulse-response :center t)))

;; Since we are subsampling by a factor of 5, we want the pre-filter
;; to pull out one-fifth of the frequencies, attenuating the others to
;; zero (or nearly zero).  Clearly, this filter doesn't do that.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Reconstruct uniform samples from irregular samples:

;; Given irregular samples of an appropriately bandlimited signal, you
;; can always compute what the uniform samples would have been (had
;; the sampling been uniform).  The basic idea is that for an
;; appropriately bandlimited signal, you can reconstruct the
;; continuous signal from the irregular samples and then resample it
;; uniformly.  Better yet, you can combine those two steps and go
;; directly from the irregular samples to the uniform samples.  Here,
;; we go through an example of how to do this.

;; First, we construct some sampling matrices.  The first is a uniform
;; subsampling that takes every 4th sample.  The second is a "jittered"
;; subsampling in which each sample is from one of 4 positions.

(progn
  (setq regular-sampling-matrix (make-array '(16 64) :element-type 'single-float))
  (loop for j from 0 below 16 do (setf (aref regular-sampling-matrix j (* j 4)) 1.0))
  (image-from-array regular-sampling-matrix))
(progn
  (setq irregular-sampling-matrix (make-array '(16 64) :element-type 'single-float))
  (loop for j from 0 below 16 do
	(setf (aref irregular-sampling-matrix j (+ (* j 4) (random 4))) 1.0))
  (image-from-array irregular-sampling-matrix))

;; Next, we need to explain what we mean by an appropriately bandlimited
;; signal.  For purposes of this example we use the Hartley basis set,
;; that is a set of cas=sin+cos functions.  The matrix for Hartley
;; transform is:

(progn
  (setq Hartley-mat (make-array '(64 64) :element-type 'single-float))
  (loop for j from 0 below 64 do
	(loop for i from 0 below 64 do
	      (setf (aref Hartley-mat j i) (* (sqrt 1/64)
					      (+ (cos (* 2 pi i (/ j 64)))
						 (sin (* 2 pi i (/ j 64))))))))
  (image-from-array Hartley-mat :-> "Hartley basis"))

;; It is an orthogonal transform (i.e., it is its own inverse)

(image-from-array (matrix-transpose-mul Hartley-mat Hartley-mat) :-> "identity")

;; For a bandlimited signal, we need only some of the basis functions to
;; represent the signal.  Here are the first quarter of the basis
;; functions:

(progn
  (setq Hartley-prime-mat (make-array '(16 64) :element-type 'single-float))
  (loop for j from 0 below 16 do
	(loop for i from 0 below 64 do
	      (setf (aref Hartley-prime-mat j i) (* (sqrt 1/64)
					      (+ (cos (* 2 pi i (/ j 64)))
						 (sin (* 2 pi i (/ j 64))))))))
  (image-from-array Hartley-prime-mat :-> "part of Hartley"))

;; Note that this modified Hartley transform is no longer generally invertible:

(image-from-array (matrix-transpose-mul Hartley-prime-mat Hartley-prime-mat)
		  :-> "not identity")

;; There are some signals that can be adequately represented by the
;; modified Hartley basis set.  We can construct such a signal in the
;; transform domain:

(progn
  (setq H-transform (make-gaussian-noise '(16 1)))
  (setq continuous-signal
	(image-from-array (matrix-transpose-mul Hartley-prime-mat
						(data H-transform)))))

;; Now check that this signal can be reconstructed from its Hartley transform.

(mean-square-error (image-from-array
		    (matrix-transpose-mul
		     Hartley-prime-mat
		     (matrix-mul Hartley-prime-mat
				 (data continuous-signal))))
		   continuous-signal)

;; Finally, we construct a matrix that converts directly from the
;; irregular samples to the regular samples.

(setq irreg-to-reg-matrix
      (matrix-mul (matrix-mul-transpose regular-sampling-matrix Hartley-prime-mat)
		  (matrix-inverse
		   (matrix-mul-transpose irregular-sampling-matrix Hartley-prime-mat))))

;; There is a potential problem in the above calculation if the matrix
;; is not invertible.  Check that the determinant is not too small:

(sqrt (determinant (matrix-mul-transpose irregular-sampling-matrix Hartley-prime-mat)))

;; If this value is smaller than 1e-6 or so, then recompute the
;; irregular-sampling-matrix, and recompute the irreg-to-reg-matrix.

;; *** Why is this matrix singular for certain choices of
;; irregular-sampling-matrix?  What is the condition on
;; irregular-sampling-matrix and Hartley-prime-mat that would
;; guarantee invertibility?

;; Let's see how it works.  Sample the "continuous" signal:

(setq reg-sampled (image-from-array (matrix-mul regular-sampling-matrix
						(data continuous-signal))))
(setq irreg-sampled (image-from-array (matrix-mul irregular-sampling-matrix
						  (data continuous-signal))))

;; Notice how different those subsampled signals are.  Now, construct
;; the regularly subsampled signal from the irregularly subsampled signal:

(setq reconstructed-reg-sampled
      (image-from-array (matrix-mul irreg-to-reg-matrix (data irreg-sampled))))
(mean-square-error reconstructed-reg-sampled reg-sampled)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
