;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: linear-systems-tutorial.lisp
;;;  Author: Heeger
;;;  Description:
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For this tutorial, we need to load the :matrix module:

(obv-require :matrix)
(set-default 'graph :graph-type :line)
(set-default 'graph :plot-symbol :circle)
(set-default 'graph :symbol-size 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Discrete-time sequences are represented as a sequence of numbers
;; f[n], for integer values of n.  There are several important, basic
;; sequences.  Examples of these basic sequences will be plotted
;; below.  We now plot several of the important sequences, from n=0 to
;; n=32.

;; Impulse sequence, delta(n), is 1 only when n=0. Here we plot
;; delta(n-16):
(setq impulse (make-synthetic-image
	       32
	       #'(lambda (x) (if (zerop (- x 16)) 1.0 0.0))
	       :x-range '(0 31)))

;; Step sequence, u(n), is 0 when n<0.  Here we plot u(n-16):
(setq step (make-synthetic-image
	    32
	    #'(lambda (x) (if (minusp (- x 16)) 0.0 1.0))
	    :x-range '(0 31)))

;; Next, we plot a sinusoidal sequence with period 8:
(let* ((amplitude 1)
       (phase 0)
       (period 8)
       (freq (/ 2-pi period)))
  (display
   (setq sinusoid (make-synthetic-image
		   32
		   #'(lambda (x) (* amplitude (sin (+ (* freq x) phase))))
		   :x-range '(0 31)))
   t :x-axis 0.0))

;; Notice that for discrete sinusoids, unlike continous sinusoids,
;; adding 2-pi to the frequency gives the same sinusoid:
(let* ((amplitude 1)
       (phase 0)
       (period 8)
       (freq (+ 2-pi (/ 2-pi period))))
  (display
   (setq sinusoid-shifted-2pi
	 (make-synthetic-image
	  32
	  #'(lambda (x) (* amplitude (sin (+ (* freq x) phase))))
	  :x-range '(0 31)))
   t :x-axis 0.0))

;; The importance of this is that we need only consider frequencies in
;; a frequency interval of length 2-pi such as -pi to pi.  Also notice
;; that although continuous sinusoids with frequency w are periodic
;; with period 2pi/w, this is not necessarily true of discrete
; sinusoids.  For example, a discrete sinusoid with frequency w=1 is
;; NOT periodic:
(let* ((amplitude 1)
       (phase 0)
       (period 2-pi)
       (freq (/ 2-pi period)))
  (display
   (setq non-periodic-sinusoid
	 (make-synthetic-image
	  32
	  #'(lambda (x) (* amplitude (sin (+ (* freq x) phase))))
	  :x-range '(0 31)))
   t :x-axis 0.0))

;; Why isn't this sequence periodic?  Is it because we've plotted only
;; 32 samples?  If we were to plot more samples, would it ever repeat?

;; For a finite length sequence, we have an even more stringent
;; requirement.  By a periodic finite length sequence, we mean
;; circularly periodic.  When you go off the end you start back at the
;; beginning.  To be periodic, the sequence length must be an multiple
;; of the period.

;; Altogether there are only N distinguishable frequencies that are
;; circularly periodic with period (sequence length) N.  These
;; frequencies are: 2 pi k/N for k=0,1,...,N-1.  In our examples,
;; N=32, so the periods are: 0, pi/16, 2pi/16, 3pi/16,..., 31pi/16.

;; This set of discrete (co-)sinusoids can also be indexed in another
;; way: 2 pi k/N for k=-N/2,...,-1,0,1,...,N/2.  In our examples these
;; periods are: -16pi/16,...,-pi/16,0,pi/16,...,15pi/16.  Take a look
;; at some of these sinusoids and cosinusoids to see that these
;; frequencies are all distinct.  Are the sinusoids and cosinusoids
;; with frequencies 0 and pi distinct?  How about with frequencies -pi
;; and pi?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Linear systems:

;; A discrete time system is defined as a transformation or operator
;; that maps an input sequence to an output sequence:
;; f[x] -> g[x]  or g[x] = T{f[x]}

;; Linear systems are defined by the principle of superposition.
;; Superposition has two parts, additivity and homogeneity.

;; Additivity:
;; T{f1[x] + f2[x]} = T{f1[x]} + T{f2[x]}

;; Homogeneity (scaling):
;; T{a f[x]} = a T{f[x]}

;; A linear system can be expressed as a matrix multiplication:
;; g[x] = M f[x]
;; where M is an m-by-n matrix, g[x] is a sequence of length m and
;; f[x] is a sequence of length n.

;; A time-invariant system is one for which a shift or delay of the
;; input sequence causes a corresponding shift in the output sequence.
;; An example of a linear time-VARIANT system is subsampling.  We'll
;; get to that later.

;; For a linear time-invariant system, the rows of the M matrix are
;; all shifted copies of one another.  Such a matrix is called a
;; Toeplitz matrix.  The output of a linear time-invariant system can
;; also be computed using convolution.  Convolution is equivalent to
;; matrix-multiplication when using a Toeplitz matrix.

;; First, let's create a Toeplitz matrix and display it as an image.

(progn
  (setq Tmatrix (make-array '(32 32) :element-type 'single-float))
  (setq 16th-row (make-matrix 0 0 0 0 0 0 0 0 0 0 0 0
			      -.004 -.031 -.047 .031 .102 .031 -.047 -.031 -.004
			      0 0 0 0 0 0 0 0 0 0 0))
  (loop for i from 0 below 32
	for shift = (- i 16)
	for row = (displaced-row i Tmatrix)
	do
	(circular-shift 16th-row :x-shift shift :-> row))
  (display (make-image Tmatrix) 'gray :zoom 4 :pedestal -.1 :scale .2))

;; Now, let's take our impulse signal and multiply it through the matrix:

(progn
  (setq impulse-response (similar impulse))
  (matrix-mul Tmatrix (columnize (data impulse))
	      :-> (columnize (data impulse-response)))
  (display impulse-response))

;; Matrix multiplication is an inefficient way of doing the above
;; computation because most of the entries in the matrix are zeros.
;; Obvius provides facilities for efficient linear filtering.  A
;; linear filter has only the interesting (non-zero) entries of the
;; Toeplitz matrix.  Then the output is computed using convolution,
;; shifting the filter over the input signal.  The efficiency of
;; convolution (over matrix multiplication) will be critical when we
;; get to 2D linear transforms on images, and 3D (space-time)
;; transforms on image sequences.  The matrices would be huge and very
;; sparse (lots of zeros).  Let's do the above transform again, using
;; Obvius filters (Obvius filters are described in detail in the
;; Obvius documentation, Section 4.7).

(progn
  (setq filter (make-filter (list -.004 -.031 -.047 .031 .102 .031 -.047 -.031 -.004)))
  (setq new-impulse-response (apply-filter filter impulse)))

;; A linear time-invariant system is completely characterized by its
;; impulse response, that is, its response to an impulse input.  The
;; response to an impulse is the corresponding column in the Toeplitz
;; matrix.  Given the impulse response, we can compute the response to
;; any input.  Any input can be expressed as the sum of a bunch of
;; scaled impulses.  Since the system is linear, the output is the sum
;; of a bunch of scaled copies of the impulse response.  Example:

(setq impulse-10 (make-synthetic-image
		  32
		  #'(lambda (x) (if (zerop (- x 10)) 1.0 0.0))
		  :x-range (list 0 31)))
(setq impulse-15 (make-synthetic-image
		  32
		  #'(lambda (x) (if (zerop (- x 15)) 1.0 0.0))
		  :x-range (list 0 31)))
(setq signal (+. (*. 5 impulse-10) (*. 13 impulse-15)))
(setq impulse-response-10 (apply-filter filter impulse-10))
(setq impulse-response-15 (apply-filter filter impulse-15))
(setq signal-response (apply-filter filter signal))
(setq sum-of-impulse-responses (+. (*. 5 impulse-response-10)
				   (*. 13 impulse-response-15)))
;; This should return 0:
(mean-square-error signal-response sum-of-impulse-responses)


;; As another example, consider the step response:

(setq step-response (apply-filter filter step))
(setp :y-range '(-.1 .1) :y-tick-step 0.05)
(zero! sum-of-impulse-responses)
(loop for i from 16 below 32
      do (add (apply-filter filter (make-synthetic-image
				    32
				    #'(lambda (x) (if (zerop (- x i)) 1.0 0.0))
				    :x-range (list 0 31)))
	      sum-of-impulse-responses :-> sum-of-impulse-responses))
(refresh)
(setp :y-range '(-.1 .1) :y-tick-step 0.05)
(mean-square-error step-response sum-of-impulse-responses)

;; Note that convolution in Obvius is circular by default.  Circular
;; convolution wraps around from the end the sequence back to the
;; beginning again, as if the sequence was a full period of a longer
;; periodic sequence.  The consequence of circular convolution is
;; evident in the step-response.  Obvius also allows you to handle the
;; endpoints of the sequence in different ways.  For example, the
;; :repeat edge-handler repeats the end sample value (at both ends)
;; rather than wrapping.  Look at the variable
;; OBVIUS::*FILTER-EDGE-HANDLERS* for a list of all edge-handlers.

(apply-filter (make-filter (list -.004 -.031 -.047 .031 .102 .031 -.047 -.031 -.004)
			   :edge-handler :repeat)
	      step)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Properties of linear time-invariant systems:

;; Convolution is commutative (i.e. the order of two consecutive
;; convolution operations is irrelavant):

(progn
  (setq filter1 (make-filter (list -.004 -.031 -.047 .031 .102 .031 -.047 -.031 -.004)))
  (setq filter2 (make-filter (/. (list 1 4 6 4 1) 16.0)))
  (display (setq response-1-2 (apply-filter filter2 (apply-filter filter1 step))))
  (setq response-2-1 (apply-filter filter1 (apply-filter filter2 step)))
  (mean-square-error response-1-2 response-2-1)) ;should be zero!

;; Even though convolution (linear shift-invariant linear systems)
;; commute, not all linear systems commute.  For example, matrix
;; multiplication is not, in general, commutative.

;; Convolution also follows the distributive property (i.e., the sum
;; of convolutions with two filters equals the convolution with the
;; sum of the filters).

(progn
  (setq filter1 (make-filter (list -.004 -.031 -.047 .031 .102 .031 -.047 -.031 -.004)))
  (setq filter2 (make-filter (/. (list 0 0 1 4 6 4 1 0 0) 16.0)))
  (setq response1 (apply-filter filter1 step))
  (setq response2 (apply-filter filter2 step))
  (setq sum-of-responses (add response1 response2))
  (setq sum-of-filters (add filter1 filter2))
  (display (setq response-to-sum-of-filters (apply-filter sum-of-filters step)))
  (mean-square-error sum-of-responses response-to-sum-of-filters))  ;should be zero!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Invertible linear systems:

;; Here's a simple example of a linear filter that shifts the sequence
;; by 2 samples.  This operation can, of course, be inverted by
;; shifting in the other direction.

(setq shift-filter (make-filter '(0 0 0 0 1)))
(setq unshift-filter (make-filter '(1 0 0 0 0)))
(setq shifted-sinusoid (apply-filter shift-filter sinusoid))
(setq unshifted-sinusoid (apply-filter unshift-filter shifted-sinusoid))

;; Another way to think of inverting a linear transform is in terms of
;; inverting the corresponding transform matrix.  The matrix for
;; shifting operation looks like this:

(progn
  (setq Smatrix (make-array '(32 32) :element-type 'single-float))
  (setq 16th-row (make-matrix 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			      0 0 0 0 1
			      0 0 0 0 0 0 0 0 0 0 0 0 0))
  (loop for i from 0 below 32
	for shift = (- i 16)
	for row = (displaced-row i Smatrix)
	do
	(circular-shift 16th-row :x-shift shift :-> row))
  (display (make-image Smatrix) 'gray :zoom 4))

;; Now, lets' recompute the shifted sequence, using the matrix (instead of
;; convolution):

(progn
  (setq new-shifted-sequence (similar sinusoid))
  (matrix-mul Smatrix (columnize (data sinusoid))
	      :-> (columnize (data new-shifted-sequence)))
  (display new-shifted-sequence))

;; To invert the transform, we just invert the matrix

(progn
  (setq Smatrix-inv (matrix-inverse Smatrix))
  (display (make-image Smatrix-inv) 'gray :zoom 4))

;; And, recompute the unshifted sequence:

(progn
  (setq new-unshifted-sequence (similar sinusoid))
  (matrix-mul Smatrix-inv (columnize (data new-shifted-sequence))
	      :-> (columnize (data new-unshifted-sequence)))
  (display new-unshifted-sequence))

;; Here's another example of inverting a linear transform.  In this
;; example, we split a signal into two bands, a high frequency (or
;; "highpass") band and a low frequency (or "lowpass") band.  Each
;; band has the same number of samples as the original sequence, so
;; the entire transform has twice as many samples as the original.
;; The lowpass and highpass filters are carefully chosen so that
;; summing the low and high bands reconstructs the original signal.

(setq lo-filter (make-filter '(.008 .031 .094 .219 .297 .219 .094 .031 .008)
			     :edge-handler :repeat))
(setq hi-filter (make-filter '(-.008 -.031 -.094 -.219 .703 -.219 -.094 -.031 -.008)
			     :edge-handler :repeat))
(setq lo-impulse-response (apply-filter lo-filter impulse))
(setq hi-impulse-response (apply-filter hi-filter impulse))
(setq reconstruct-impulse (add lo-impulse-response hi-impulse-response))
(setq lo-sin-response (apply-filter lo-filter sinusoid)) ;note edges are non-sinusoidal!
(setq hi-sin-response (apply-filter hi-filter sinusoid))
(setq reconstruct-sin (add lo-sin-response hi-sin-response))
(mean-square-error sinusoid reconstruct-sin) ;should be zero!

;; Now, let's see what the matrix looks like for this transform:

(progn
  (setq LHmatrix (make-array '(64 32) :element-type 'single-float))
  (setq 16th-row (make-matrix 0 0 0 0 0 0 0 0 0 0 0 0
			      .008 .031 .094 .219 .297 .219 .094 .031 .008
			      0 0 0 0 0 0 0 0 0 0 0))
  (setq 48th-row (make-matrix 0 0 0 0 0 0 0 0 0 0 0 0
			      -.008 -.031 -.094 -.219 .703 -.219 -.094 -.031 -.008
			      0 0 0 0 0 0 0 0 0 0 0))
  (loop for i from 0 below 32
	for shift = (- i 16)
	for row = (displaced-row i LHmatrix)
	do
	(circular-shift 16th-row :x-shift shift :-> row))
  (loop for i from 32 below 64
	for shift = (- i 48)
	for row = (displaced-row i LHmatrix)
	do
	(circular-shift 48th-row :x-shift shift :-> row))
  (display (make-image LHmatrix) 'gray :zoom 4 :pedestal -1/2 :scale 1))

;; To invert the transform, we can use the pseudo-inverse, (Mt M)^(-1) Mt, 
;; where M is the matrix, Mt is the matrix transpose, and the -1
;; indicates matrix inverse.

(progn
  (setq LHmatrix-inv (matrix-mul-transpose
		      (matrix-inverse (matrix-transpose-mul LHmatrix LHmatrix))
		      LHmatrix))
  (display (make-image LHmatrix-inv :-> "LHmatrix-inv")
	   'gray :zoom 4 :pedestal -1/2 :scale 1))

;; Let's check that this really is the inverse:

(display (make-image (matrix-mul LHmatrix-inv LHmatrix)
			   :-> "should be identity")
	 'gray :zoom 4)

;; Now, recompute the transform:

(progn
  (setq impulse-transform (make-image '(64)))
  (matrix-mul LHmatrix (columnize (data impulse))
	      :-> (columnize (data impulse-transform)))
  (display impulse-transform))

;; The lo-pass and hi-pass transform coefficients are displayed in one
;; double-length sequence, next to each other.  Now, invert the
;; transform:

(progn
  (setq new-reconstruct-impulse (similar impulse))
  (matrix-mul LHmatrix-inv (columnize (data impulse-transform))
	      :-> (columnize (data new-reconstruct-impulse)))
  (display new-reconstruct-impulse 'graph))
(mean-square-error impulse new-reconstruct-impulse)

;; The inverse that we just used is different from just adding
;; together the coefficients of the two bands (used above).  There is
;; more than one way to invert an overcomplete transform.  Let's
;; construct the matrix that corresponds to adding the coefficients
;; from the two bands, and make sure that it also inverts the
;; transform:

(progn
  (setq identity32 (make-identity-matrix 32))
  (display (make-image (setq new-LHmatrix-inv (append-cols identity32 identity32))
		       :-> "new-LHmatrix-inv")
	   'gray :zoom 4))

;; Check that this is also an inverse:

(display (make-image (matrix-mul new-LHmatrix-inv LHmatrix)
			   :-> "should be identity")
	 'gray :zoom 4)

;; Use it to invert the transform of an impulse:

(progn
  (matrix-mul new-LHmatrix-inv (columnize (data impulse-transform))
	      :-> (columnize (data new-reconstruct-impulse)))
  (display new-reconstruct-impulse))
(mean-square-error impulse new-reconstruct-impulse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sinusoidal sequences:

;; Sinusoidal and cosinusoidal sequences play a particularly important
;; role in representing signals, because complex exponential sequences
;; (including sines and cosines) are the eigenfunctions of linear
;; time-invariant systems.  For example, a sinusoidal sequence
;; convolved with a linear filter gives another sinusoidal sequence of
;; the same frequency.  Only the phase and amplitude of the output
;; sinusoid will be different.

(display sinusoid)
(setq filter (make-filter (list -.004 -.031 -.047 .031 .102 .031 -.047 -.031 -.004)))
(display (apply-filter filter sinusoid) 'graph :y-range '(-1.0 1.0))

;; That filter changes only the amplitude, not the phase.  Here's one
;; that also changes the phase (via shift/delay):

(setq shift-filter (make-filter (list 0 0 -.004 -.031 -.047 .031 .102 .031 -.047 -.031 -.004)))
(display (apply-filter shift-filter sinusoid) 'graph :y-range '(-1.0 1.0))

;; Let's try some really weird (in fact random) filters, just to
;; demonstrate that no matter what you use, you still get a sinusoid
;; of the same frequency:

(display (make-image-sequence
	  (loop for i from 0 below 20
		collect
		(progn
		  (randomize (zero! (data filter)) 1.0 :-> (data filter))
		  (apply-filter filter sinusoid))))
	 'flipbook
	 :y-range '(-4.0 4.0) :y-tick-step 1.0)

;; This displays a flipbook sequence of graphs.  Use the C-M mouse
;; clicks to look at the individual graphs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fourier Series Representation:

;; Any signal can be expressed as a (weighted) linear sum of impulses.
;; Likewise, a signal can be expressed as a (weighted) linear sum of
;; sines and cosines.  Example, make a gaussian as a sum of cosines:

(setq gaussian (make-synthetic-image
		32 #'(lambda (x) (exp (- (/ (sqr (- x 16)) (* 2 (sqr 4))))))
		:x-range '(0 31)))
(setq gaussian-series
      (circular-shift
       (+. 0.333
	   (make-synthetic-image
	    32 #'(lambda (x) (* .470 (cos (* 2-pi 1/32 x)))) :x-range '(0 31))
	   (make-synthetic-image
	    32 #'(lambda (x) (* .166 (cos (* 2-pi 2/32 x)))) :x-range '(0 31))
	   (make-synthetic-image
	    32 #'(lambda (x) (* .029 (cos (* 2-pi 3/32 x)))) :x-range '(0 31))
	   (make-synthetic-image
	    32 #'(lambda (x) (* .0025 (cos (* 2-pi 4/32 x)))) :x-range '(0 31)))
       :x-shift 16))
(mean-square-error gaussian gaussian-series) ;should be small

;; The weights in the summation (.470, .166, 029, and .0025) are
;; called the Fourier coefficients.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fourier Transform:

;; The Fourier transform is a particular linear transform that is used
;; to compute the Fourier coefficients of a signal.  The transform
;; matrix looks like this:

(progn
  (setq Fourier-mat (make-array '(64 32) :element-type 'single-float))
  (loop for k from 0 below 32 do
	(loop for n from 0 below 32 do
	      (setf (aref Fourier-mat k n)
		    (* (sqrt 1/32) (cos (* 2/32 pi k n))))))
  (loop for k from 0 below 32 do
	(loop for n from 0 below 32 do
	      (setf (aref Fourier-mat (+ k 32) n)
		    (* (sqrt 1/32) (sin (* 2/32 pi k n))))))
  (setq Fourier-image (make-image Fourier-mat)))

;; Each row is a sine or cosine:

(make-slice Fourier-image :y 1)
(make-slice Fourier-image :y 33)
(make-slice Fourier-image :y 4)
(make-slice Fourier-image :y 36)

;; The top half of the matrix contains cosines of various frequencies,
;; and the bottom half contains sines of various frequencies.  The
;; very top row is a constant that pulls out the average (dc)
;; component of a signal.  The transform is self-inverting, that is,
;; multiplying the matrix by its transpose gives the identity
;; transform.

(display
 (make-image (matrix-transpose-mul Fourier-mat Fourier-mat) :-> "should be identity")
 'gray :zoom 4)

;; Let's look at the rows of the inverse (transpose) matrix.  The
;; first half of each row is a cosine, and the second half of each row
;; is a sine.  These sines and cosines are the same as the rows of the
;; the Fourier matrix.

(setq Fourier-inv-image (image-from-array (matrix-transpose Fourier-mat)))
(crop (make-slice Fourier-inv-image :y 1) :x 0 :x-dim 32)
(crop (make-slice Fourier-inv-image :y 1) :x 32 :x-dim 32)

;; Let's take the Fourier transform of a cosinusoid.  

(let* ((num-cycles 4)
       (size 32))
  (display (setq cosinusoid (make-synthetic-image
			     size
			     #'(lambda (x) (cos (+ (* 2-pi (/ num-cycles size) x))))
			     :x-range (list 0 31)))
	   'graph :x-axis 0.0))
(setq ft-cosinusoid (make-image (matrix-mul Fourier-mat (columnize (data cosinusoid)))))

;; We get a pair of impulses in the transform.  One of the impulses
;; corresponds to the frequency of the signal (4 cycles per image) at
;; position 4 in the transform.  Why is there a second impulse?  The
;; Fourier transform is really set up to analyze complex signals.  For
;; real signals, the transform has certain symmetry properties.  We
;; will go into those in more detail below.

;; Try a different frequency:

(let* ((num-cycles 8)
       (size 32))
  (setq cos-8 (make-synthetic-image
	       size
	       #'(lambda (x) (cos (+ (* 2-pi (/ num-cycles size) x))))
	       :x-range (list 0 31))))
(setq ft-cos-8 (make-image (matrix-mul Fourier-mat (columnize (data cos-8)))))

;; Again, the location of the impulses in the transform corresponds to
;; the frequency of the signal.  For example, a frequency of 8 cycles
;; per image gives an impulse at positions 8 and 32-8=24 in the
;; transform domain.

;; For a sinusoid, we get impulses in the second half (positions > 32)
;; of the output because the sinusoids are in the bottom half of the
;; system matrix.

(setq transform (make-image (matrix-mul Fourier-mat (columnize (data sinusoid)))))

;; The Fourier transform is inverted by using the transpose of the
;; system matrix:

(setq invert-transform
      (make-image (matrix-transpose-mul Fourier-mat (data transform))))
(mean-square-error (columnize (data invert-transform))
		   (columnize (data sinusoid)))	; should be zero

;; The FFT algorithm is an efficient way of computing the Fourier
;; transform, without bothering with the matrix multiplication.

(display (setq ft-sinusoid (fft sinusoid)) 'flipbook :y-range '(-3 3))

;; FFT returns a complex-image, that is displayed here as a "flipbook"
;; (movie).  The imaginary part (numbered 0) contains the sine
;; components and the real-part (numbered 1) contains the cosine
;; components.  Use C-M-left mouse click to flip between the two
;; graphs.  The sample at position 0 corresponds to the "DC" or
;; constant coefficient.  (Note: In Obvius, the fft is implemented
;; only for signals that are a power of 2 in length.  Other signals
;; are padded out with zeroes to the next highest power of two.)  Now,
;; let's compute Fourier transforms of some other signals.

;; Fourier transform of a constant function:

(setq constant (make-synthetic-image 32 #'(lambda (x) 1.0) :x-range '(0 31)))
(display (setq ft-constant (fft constant))
	 'flipbook :sub-display-type 'graph :y-range '(-6 6))

;; Look at the real part (component #1).  There is an impulse at the dc
;; component.

;; Often, people draw graphs of Fourier domain with origin (dc) in the
;; center.  This can be done in Obvius using the circular-shift
;; function or it can be done using the :pre-center, :post-center, and
;; :center keyword arguments to the fft function.  Take a look at the
;; documentation to see what the keywords do.

(display (setq shifted-ft-constant (circular-shift ft-constant :x-shift 16))
	 'flipbook :sub-display-type 'graph :y-range '(-6 6))
(display (setq new-shifted-ft-constant (fft constant :center t))
	 'flipbook :sub-display-type 'graph :y-range '(-6 6))

;; Often, people represent Fourier coefficients in terms of mag and
;; phase, rather than real and imaginary:

(setq mag-sine (magnitude ft-sinusoid))

;; Here's a whole series of Fourier transforms for different frequency
;; sinusoids (try displaying the sequence as a movie with a C-M-right
;; mouse click):

(display
 (make-viewable-sequence 
  (loop for freq from 0 below 32
	collect
	(imaginary-part (fft (make-synthetic-image
			      32 #'(lambda (x) (sin (* 2-pi 1/32 freq x)))
			      :x-range '(0 31))
			     :post-center t))))
 'flipbook :y-range '(-6 6))

;; Fourier transform of a Gaussian function: The imaginary part is
;; zero and the real part is itself a Gaussian.

(setq gaussian (make-synthetic-image
		32 #'(lambda (x) (exp (- (/ (sqr (- x 16)) (sqr 6)))))
		:x-range '(0 31)))
(display
 (setq ft-gaussian (fft gaussian :center t))
 'flipbook :y-range '(-2 2))

;; Making the Gaussian smaller in one domain makes it larger in the
;; other domain:

(setq little-gauss (make-synthetic-image
		    32 #'(lambda (x) (exp (- (/ (sqr (- x 16)) (* 2 (sqr 1.0))))))
		    :x-range '(0 31)))
(display
 (setq ft-little-gauss (fft little-gauss :center t))
 'flipbook :y-range '(-0.5 0.5))

;; This is an example of what is known as the uncertainty principle.
;; When a signal is more localized in one domain, it is less well
;; localized in the other domain.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We'll get rid of the plot symbols in the graphs from now on, but
;; don't forget that these are all discrete (not continuous) signals.

(set-default 'graph :plot-symbol nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Symmetry properties of Fourier Transform:

;; For any real-valued, antisymmetric (odd) function, in which f(x) =
;; -f(-x), the real part of the FT is zero, and the imaginary part of
;; the FT is antisymmetric (odd).  For any real-valued, symmetric
;; (even) function, in which f(x) = f(-x), the imaginary part of the
;; FT is zero and the real part is symmetric (even).


(progn
  (setq random-signal (make-synthetic-image 64 #'(lambda (x) (- 1/2 (random 1.0)))))
  (setq even-signal (make-synthetic-image
		     64 #'(lambda (x) (if (<= x 32)
					  (iref random-signal (floor x))
					  (iref random-signal (- 64 (floor x)))))
		     :x-range '(0 63))))
(progn
  (setq odd-signal (make-synthetic-image
		    64 #'(lambda (x) (if (<= x 32)
					 (iref random-signal (floor x))
					 (- (iref random-signal (- 64 (floor x))))))
		    :x-range '(0 63)))
  (setf (iref odd-signal 0) 0.0)
  odd-signal)
(display
 (setq odd-fft (fft odd-signal :center t))
 'flipbook)
(display
 (setq even-fft (fft even-signal :center t))
 'flipbook)

;; For any real-valued signal, the real part of the FT is even (look
;; at even-fft) and the imaginary part of the FT is odd (look at
;; odd-fft).

(display (setq signal-fft (fft random-signal :center t))
	 'flipbook)
;; Check that these are the same:
(= (iref (real-part signal-fft) 30) (iref (real-part signal-fft) 34))
(= (iref (imaginary-part signal-fft) 30) (- (iref (imaginary-part signal-fft) 34)))

;; Taken together, these symmetry properties mean that there is quite
;; a lot of redundancy in the FT of a real signal.  A simple way to
;; count the amount of redundancy is to compare the number of samples.
;; Take a real-value signal with 64 samples.  Computing its fft gives
;; a total of 128 samples (half in the real part and half in the
;; imaginary part), a factor of 2 redundant.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parseval's Thm: sum of squared values over space domain equals sum
;; of squared values over frequency domain.

(sum-of (square gaussian))
(sum-of (square-magnitude ft-gaussian))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Circular Shifting: If we shift the signal as if it were periodic
;; (i.e., translate the signal, wrapping around at the edges), this
;; does not affect the Fourier transform magnitude:
(progn (setq c-gauss  (circular-shift gaussian :x-shift 16)) ;symmetric about 0
       (setq fft-gauss (fft c-gauss))
       (setq fft-shift-gauss (fft (circular-shift c-gauss :x-shift 3)))
       t)
(setq mag-fft-gauss (magnitude fft-gauss))
(setq mag-fft-shift-gauss (magnitude fft-shift-gauss))
(mean-square-error mag-fft-gauss mag-fft-shift-gauss) ;should be zero

;; But, the phase IS different:
(setq phase-fft-gauss (complex-phase fft-gauss))
(setq phase-fft-shift-gauss (complex-phase fft-shift-gauss))

;; *** Why does the phase function look so jagged/ugly?  Mod 2-pi.
;; Need to fix this.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Differentiation:

;; Taking a derivative of a signal in time is the same as multiplying
;; by an imaginary ramp in frequency. In particular, we multiply by.
;; For example, let's consider a Gaussian and the first derivative of
;; a Gaussian.

(setq gaussian (make-synthetic-image
		32 #'(lambda (x) (exp (- (/ (sqr (- x 16)) (* 2 (sqr 4))))))
		:x-range '(0 31)))
(setq gaussian-deriv (make-synthetic-image
		      32 #'(lambda (x) (* -2 (/ (- x 16) (* 2 (sqr 4)))
					  (exp (- (/ (sqr (- x 16)) (* 2 (sqr 4)))))))
		      :x-range '(0 31)))

(display (setq ft-gaussian (fft gaussian :center t))
	 'flipbook)
(display (setq ft-gaussian-deriv (fft gaussian-deriv :center t))
	 'flipbook)

(setq ramp (make-synthetic-image 32 #'(lambda (k) (* (/ 2-pi 32.0) k)) :x-range '(-16 15)))
(display (setq ramp-i (make-complex-image (list (mul -1 ramp) (make-image 32))))
	 'flipbook)
(display (setq ft-gaussian-times-w (mul ft-gaussian ramp-i))
	 'flipbook)
(mean-square-error ft-gaussian-times-w ft-gaussian-deriv) ; should be zero

;; *** Why is the -1 in there?
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modulation Thm: Multiplication in time domain is the same as
;; convolution in the frequency domain, up to a known scale factor.
;; For example, a Gabor function is a sinusoid multiplied by a
;; Gaussian window.  Thus, the FT of a Gabor is the convolution of the
;; FT of a Gaussian with the FT of a sinusoid.  This is an easy way to
;; gain an intuition for the filtering properties of a Gabor filter.

(setq gabor (mul gaussian sinusoid))
(display
 (setq ft-gabor (fft gabor :pre-center t))
 'flipbook)
(display
 (setq ft-sinusoid  (fft sinusoid :pre-center t))
 'flipbook)
(display (setq ft-gaussian (fft gaussian :pre-center t))
	 'flipbook)

(setq convolution-of-fts (div (convolve (real-part ft-gaussian) (imaginary-part ft-sinusoid))
			      (sqrt 32)))
(mean-square-error (imaginary-part ft-gabor) convolution-of-fts) ;should be zero

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convolution Thm: Convolution in the time domain is the same as
;; multiplication in the frequency domain, up to a known scale factor.
;; This theorem is extremely useful.  Sometimes, you have a filter
;; that is simple to characterized in the frequency domain, but
;; complicated in the time domain.  For example, it may be very
;; compact in the frequency domain (i.e., zero nearly everywhere), but
;; very big (i.e., lots of samples needed) in the time domain.  In
;; such cases, you can do the filtering by Fourier transforming the
;; signal, multiplying in the frequency domain, and then Fourier
;; transforming back.

(setq gabor-filter (make-filter (data gabor)))
(setq impulse-signal (make-synthetic-image 128 #'(lambda (x) (if (= x 64) 1.0 0.0))
					   :x-range '(0 127)))
(setq random-signal (make-synthetic-image 128 #'(lambda (x) (- 1/2 (random 1.0)))))
(setq impulse-response (apply-filter gabor-filter impulse-signal))
(setq filtered-signal (apply-filter gabor-filter random-signal))
(display (setq ft-filtered-signal (fft filtered-signal)) 'flipbook)
      
(display (setq frequency-response (fft impulse-response :pre-center t)) 'flipbook)
(display (setq ft-random-signal (fft random-signal)) 'flipbook)
(display (setq product-of-fts (mul (mul frequency-response ft-random-signal)
				   (sqrt 128)))
	 'flipbook)

(mean-square-error product-of-fts ft-filtered-signal)

(setq filtered-signal-via-fft (real-part (fft product-of-fts :inverse t)))
(mean-square-error filtered-signal-via-fft filtered-signal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Frequency Response:

;; Since the Convolution Thm is so useful, the Fourier transform of
;; the impulse response of a time-invariant linear system has a
;; special name.  It is called the frequency response of the linear
;; system.

;; Remember that for a sinusoidal input, the output of a
;; time-invariant linear system is sinusoidal with the same frequency.
;; Only the amplitude and phase will be changed by filtering.  The
;; frequency response of a filter can be used to "read off" the
;; amplitude attenuation and the phase shift, for each frequency.  For
;; a complicated signal, that can be expressed as the sum of a number
;; of sinusoids, the frequency response can be used to "read off" the
;; attenuation and phase shift for each component.

;; As another example, let's compute the frequency response of the 1
;; sample delay system:

(setq delay-filter (make-filter '(0 0 1)))
(setq impulse-response (apply-filter delay-filter impulse-signal))
(display (setq frequency-response (fft impulse-response :center t))
	 'flipbook)
(display (setq mag-frequency-response (mul (sqrt 128) (magnitude frequency-response)))
	 'graph :y-range '(0.0 6.0))
(setq phase-frequency-response (complex-phase frequency-response))
      
;; For the delay system, the magnitude of the frequency response is constant
;; (1 for all frequencies), and the phase is -w.

;; You may have notice in a few places (like in the above computation
;; of mag-frequency-response), we multiplied by the square root of the
;; number of samples (in this case 128).  This scale factor is needed
;; given the way that the fft is implemented in Obvius.  In some
;; textbooks (e.g., Oppenheim and Schafer), the discrete Fourier
;; transform (DFT) is defined so that you divided by the number of
;; samples (N) when doing the inverse transform (from the frequency
;; domain back into the space/time domain).  In other texts, the DFT
;; is defined so that you divided by N when doing the forward
;; transform.  In still other texts, you divided by sqrt-N when doing
;; both the forward and the inverse transforms.  The Obvius
;; implementation uses this latter convention.

;; If you loose track of which convention is being used it is
;; unfortunately, easy to get confused.  For example, Oppenheim and
;; Schafer write Parseval's theorem as follows (with a factor of 1/N):
;;         sum |x[n]|^2 = 1/N sum |X[w]|^2
;; Using Obvius' convention, Parseval's theorem has no scale factor):
;;         sum |x[n]|^2 = sum |X[w]|^2

;; Using Oppenheim and Schafer's convention the convolution theorem
;; has no scale factor:
;;         F{x1[n] * x2[n]} = X1[k] X2[k]
;; Using Obvius' convention, the convolution theorem has a factor of
;; sqrt-N:
;;         F{x1[n] * x2[n]} = sqrt-N X1[k] X2[k]
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Hartley Transform:

;;; The Hartley transform is an orthonormal transform that is closely related
;;; to the DFT.  In the DFT matrix, each row is either a sinusoid or a
;;; cosinusoid.  In the Hartely transform matrix, each row is: sin+cos.  In
;;; particular, the rows of the Hartley transform matrix are given by:
;;;
;;;      1/(sqrt N) {sin[(2pi/N) k n] + cos[(2pi/N) k n]}

(let* ((Num 16)
       (1/Num (/ 1 Num)))
  (setq Hartley-mat (make-array (list Num Num) :element-type 'single-float))
  (loop for k from 0 below Num do
	(loop for n from 0 below Num do
	      (setf (aref Hartley-mat k n) (* (sqrt 1/Num)
					      (+ (cos (* (/ 2-pi Num) k n))
						 (sin (* (/ 2-pi Num) k n)))))))
  (image-from-array Hartley-mat :-> "Hartley basis"))
(setp :zoom 4)

;;; Unlike the DFT, the Hartley is a square, orthonormal matrix.  Check that
;;; the matrix is orthonormal.  The "identity-p" function returns nil if a
;;; matrix is not the identity matrix.  If it is the identity matrix,
;;; "identity-p" returns the matrix itself.
(identity-p (matrix-transpose-mul Hartley-mat Hartley-mat))
(identity-p (matrix-mul-transpose Hartley-mat Hartley-mat))

;;; Note that the DFT matrix is very similar.  Each row of the top half of the
;;; DFT matrix is given by:
;;;
;;;    1/(sqrt N) cos[(2pi/N) k n]
;;;
;;; Each row of the bottom half of the DFT matrix is given by:
;;;
;;;    1/(sqrt N) sin[(2pi/N) k n]

;;; The square matrix given by the top half of the DFT matrix is singular.  The
;;; function "singular-p" returns the matrix if it is singular.  Otherwise, it
;;; returns nil:
(let* ((Num 16)
       (1/Num (/ 1 Num)))
  (setq DFT-top-mat (make-array (list Num Num) :element-type 'single-float))
  (loop for k from 0 below Num do
	(loop for n from 0 below Num do
	      (setf (aref DFT-top-mat k n) (* (sqrt 1/Num)
					      (cos (* (/ 2-pi Num) k n)))))))

(singular-p DFT-top-mat)
;;; Since DFT-top-mat is singular, it is not invertible.

;;; The same is true for the bottom half of the DFT matrix
(let* ((Num 16)
       (1/Num (/ 1 Num)))
  (setq DFT-bottom-mat (make-array (list Num Num) :element-type 'single-float))
  (loop for k from 0 below Num do
	(loop for n from 0 below Num do
	      (setf (aref DFT-bottom-mat k n) (* (sqrt 1/Num)
						 (sin (* (/ 2-pi Num) k n)))))))
(singular-p DFT-bottom-mat)

;;; However, when we add these two matrices together we get a non-singular
;;; matrix that is the Hartley transform matrix:
(mean-square-error Hartley-mat (add DFT-bottom-mat DFT-top-mat))

;;; And this matrix is of course non-singular (in fact, it is orthonormal as we
;;; showed above):
(singular-p Hartley-mat)

;;; In some ways, particularly for real-valued inputs, the Hartley transform is
;;; better than the DFT.  It is odd that the DFT produces twice as many
;;; transform coefficients as there were input samples; there is redundancy in
;;; the DFT coefficients as evidenced by the symmetry properties listed above.
;;; There is no redundancy in Hartley transform.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Discrete Cosine Transform:

;;; The DCT is another linear transform that is closely related to the DFT.
;;; The rows of the DCT transform matrix are cosines:
;;;
;;;       c(k) 1/sqrt(N) cos[(pi/2N) k (2n+1)]
;;;
;;; where c(k)=1 for k=0 and c(k)=root-2 otherwise.  Here, k indexes the row
;;; and n indexes the column.  In other words, n indexes the sample position of
;;; the original signal and k indexes the transform coefficients.

(let* ((Num 16)
       (1/Num (/ 1 Num)))
  (setq DCT-mat (make-array (list Num Num) :element-type 'single-float))
  (loop for k from 0 below Num
	for c = (if (zerop k) 1 (sqrt 2))
	do
	(loop for n from 0 below Num
	      do
	      (setf (aref DCT-mat k n)
		    (* c (sqrt 1/Num) (cos (* (/ pi (* 2 Num)) k (+ (* 2 n) 1))))))))

;;; Look at it the matrix:
(setq DCT-im (make-image DCT-mat))
(setp :zoom 4)

;;; Look at the individual rows of the matrix.  Note that they are all cosines:
(make-image-sequence
 (loop for k from 0 below (row-dim DCT-im)
       collect (make-slice DCT-im :y k))
 :display-type 'flipbook)
(setp :y-axis 0.0 :x-axis 0 :graph-type :line :plot-symbol :circle :fill-symbol-p t)

;;; Use C-M-middle to click through these graphs.  Each one corresponds to one
;;; row of the DCT matrix.

;;; Like the Hartley matrix, the DCT matrix is square and orthonormal:
(identity-p (matrix-transpose-mul DCT-mat DCT-mat))
(identity-p (matrix-mul-transpose DCT-mat DCT-mat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Hilbert Transform:

;;; *** unfinished ***

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
