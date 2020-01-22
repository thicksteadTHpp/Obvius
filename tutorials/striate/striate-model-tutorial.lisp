
;;; Code for this tutorial is mostly in striate-model.lisp.  Most of
;;; the functions that are described and used below are defined in
;;; that file.  If you want, use C-c dot to take a look at the source
;;; code to figure out how they work.  Evaluate this to setup the
;;; tutorial.

(progn
  (set-default 'flipbook :independent-parameters nil)
  (set-default 'gray :zoom 4)
  (setq *auto-destroy-orphans* t)
  (obv-require :matrix)
  (compile-load (merge-pathnames "tutorials/striate/striate-model"
				 obv::*obvius-directory-path*)))

;;; A particular set of linear operators are used in the model.  Each
;;; operator is tuned to one of four orientations (separated by 45
;;; degrees) and one of three spatial frequencies (with octave
;;; bandwidth and octave spacing).  For each spatial frequency and
;;; orientation there are also three temporal channels, one preferring
;;; static stimuli and the other two preferring stimuli moving in
;;; opposite directions (e.g., rightward and leftward).  For each
;;; spatial frequency, orientation, and temporal band there are
;;; operators with four different phases, in 90 degree steps.  In
;;; total there are 144 linear operators in the model.

;;; The global symbol *filter-list* is a list of the linear operators,
;;; each of which is defined by its frequency response (amplitude and
;;; phase).  For some of the simulations, we use only vertical grating
;;; stimuli.  In those cases, we need only consider the responses of
;;; the vertical filters (there are only 36 of them), making the code
;;; run 4 times faster.  To use the full list of operators (all
;;; orientations):

(length (setq *filter-list* (make-filter-list)))

;;; To use only the vertical filters:

(length (setq *filter-list* (make-filter-list-xt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Frequency responses:

;;; First let's look at the frequency responses of some of the linear
;;; operators.  The function frequency-response gives the amplitude of
;;; an operator's response to a sinusoidal (e.g., drifting
;;; sine-grating) input.  The function is called with a base-function
;;; (e.g., Ixxx-freq) and a frequency-function (e.g.,
;;; raised-cosine-fn). Together, these two functions specify the
;;; operator's amplitude response.  The frequency-reponse function is
;;; also called with the contrast and spatiotemporal-frequency of the
;;; stimulus.

;;; The frequency responses are 3D functions of spatiotemporal
;;; frequency.  It is easiest to look at slices.  For example, this is
;;; a slice (along the wx axis) of the squared amplitude response of
;;; an operator that prefers 1 c/deg stationary vertical gratings.

(let ((wy 0.0) (wt 0.0) (contrast 1.0))
  (make-discrete-function
   #'(lambda (wx)
       (sqr (frequency-response 'Ixxx-freq (raised-cos-fn :ctr-freq 1)
				contrast wx wy wt)))
   -4 4))

;;; This is a slice in the wx-wy plane:

(let ((contrast 1.0) (wt 0.0))
  (make-synthetic-image
   '(65 65)
   #'(lambda (wy wx)
       (sqr (frequency-response 'Ixxx-freq (raised-cos-fn :ctr-freq 1)
				contrast wx wy wt)))
   :x-range '(-4 4) :y-range '(-4 4)))

;;; This is a slice in the wx-wt plane, where wt is represented on the
;;; vertical y-axis of the image:

(let ((contrast 1.0) (wy 0.0))
  (make-synthetic-image
   '(65 65)
   #'(lambda (wt wx)
       (sqr (frequency-response 'Ixxx-freq (raised-cos-fn :ctr-freq 1)
				contrast wx wy wt)))
   :x-range '(-4 4) :y-range '(-12 12)))

;;; Notice that this is a low-pass filter in time and a bandpass
;;; filter in space.  Also, it is space-time separable since it can be
;;; described as a spatial function (a function of wx and wy)
;;; multiplied by a temporal function (a function only of wt).

;;; Now, let's look at the frequency response of an operator that is
;;; space-time separable, but bandpass in both space and time.  This
;;; operator prefers 1 c/deg vertical gratings that are flickering at
;;; about 4 Hz.  This is a slice in the wx-wt plane:

(let ((contrast 1.0) (wy 0.0))
  (make-synthetic-image
   '(65 65)
   #'(lambda (wt wx)
       (sqr (frequency-response 'Ixxt-freq (raised-cos-fn :ctr-freq 1)
				contrast wx wy wt)))
   :x-range '(-4 4) :y-range '(-12 12)))

;;; Finally, let's look at the frequency response of the sum of the
;;; above two filters.  This gives an operator that prefers 1 c/deg
;;; rightward motion of vertical gratings.  Here's the wx-wt slice of
;;; sum of those two filters (note that we can't just add the above
;;; computed wx-wt slices because they are images of the squared
;;; magnitude of the frequency responses):

(let ((contrast 1.0) (wy 0.0))
  (make-synthetic-image
   '(65 65)
   #'(lambda (wt wx)
       (sqr (frequency-response 'Ixxx+Ixxt-freq (raised-cos-fn :ctr-freq 1)
				contrast wx wy wt)))
   :x-range '(-4 4) :y-range '(-10 10)))

;;; Notice that the frequency response is not space-time separable,
;;; even though it is constructed by adding two space-time separable
;;; functions.  It is clear that the operator responds more to
;;; positive temporal frequencies (rightward motion), than to negative
;;; (leftward) temporal frequencies.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Space-time receptive fields:

;;; Let's repeat the construction of the above direction-selective
;;; operator, in the space domain.  We will work in the wx-wt and x-t
;;; planes.  The following function computes x-t slices through the
;;; spatiotemporal weighting functions by Fourier transforming the
;;; frequency-responses.

(defun compute-weighting-function (freq-func)
  (let*
      ((freq-resp (make-synthetic-image
		   '(65 65)
		   #'(lambda (wt wx)
		       (frequency-response freq-func (raised-cos-fn :ctr-freq 1)
					   1.0 wx 0.0 wt))
		   :x-range '(-8 8) :y-range '(-64 64)))
       (cropped-freq-resp (crop freq-resp :x-size 64 :y-size 64))
       (shifted-freq-resp (circular-shift cropped-freq-resp :x-shift 32 :y-shift 32))
       (fft-freq-resp (fft shifted-freq-resp))
       (shifted-weighting-func (imaginary-part fft-freq-resp)))
    (circular-shift shifted-weighting-func :x-shift 32 :y-shift 32)))

;;; To compile the above function, position the cursor somewhere on
;;; the function and type C-c c.  After compiling the function,
;;; evaluate the following to see weighting functions of the three
;;; space-time linear operators.

(setq Gxxx (compute-weighting-function 'Ixxx-freq))
(setq Gxxt (compute-weighting-function 'Ixxt-freq))
(setq Gxxx+xxt (add Gxxx Gxxt))

;;; Gxxx and Gxxt are space-time separable.  Gxxx+xxt is not
;;; space-time separable.  Rather, it is oriented in space-time.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tiling:

;;; The full set of linear operators are chosen so that (with
;;; appropriate weighting factors) they tile (evenly cover) the
;;; spatial frequency plane.  How was the tiling property achieved?
;;; The operators were designed in the frequency domain, so that the
;;; squared frequency-response varies like cosine to the sixth power
;;; with orientation.  Summing four cos^6 functions gives a constant
;;; (flat) result (that is what we mean by tiling orientation).

(make-discrete-function #'(lambda (angle) (expt (cos angle) 6)) 0.0 2-pi)

;;; Now, let's add together two cos^6 functions, shifted by pi/4 radians (45 deg):

(+. (make-discrete-function #'(lambda (angle) (expt (cos angle) 6)) 0.0 2-pi)
    (make-discrete-function #'(lambda (angle) (expt (cos (+ angle (* 1/4 pi))) 6)) 0.0 2-pi))

;;; And now, all four giving a constant (flat or evenly tiled) result:

(display
 (+. (make-discrete-function #'(lambda (angle) (expt (cos angle) 6)) 0.0 2-pi)
     (make-discrete-function #'(lambda (angle) (expt (cos (+ angle (* 1/4 pi))) 6)) 0.0 2-pi)
     (make-discrete-function #'(lambda (angle) (expt (cos (+ angle (* 1/2 pi))) 6)) 0.0 2-pi)
     (make-discrete-function #'(lambda (angle) (expt (cos (+ angle (* 3/4 pi))) 6)) 0.0 2-pi))
 'graph :y-range '(0.0 1.3))

;;; The radial frequency responses were also designed for easy tiling,
;;; in terms of a raised-cosine function.  Raised cosines are
;;; 1+cos(x) = (cos x/2)^2.  So, adding together appropriated shifted
;;; raised cosines will give a constant function.

;;; Compile this using C-c c:

(defun raised-cos (wx &key (ctr-freq 1))
  (let ((r-arg (* pi (- wx ctr-freq))))
    (if (< (- pi) r-arg pi)
	(1+ (cos r-arg))
	0.0)))

;;; Evaluate these:
		      
(setq raised-cos-1 (make-discrete-function
		    #'(lambda (wx) (raised-cos wx :ctr-freq 1))
		    0.0 4.0))
(setq raised-cos-2 (make-discrete-function
		    #'(lambda (wx) (raised-cos wx :ctr-freq 2))
		    0.0 4.0))
(setq raised-cos-3 (make-discrete-function
		    #'(lambda (wx) (raised-cos wx :ctr-freq 3))
		    0.0 4.0))

;;; Add together the above functions to see that they tile:

(+. raised-cos-1 raised-cos-2 raised-cos-3)

;;; The actual linear operators that are used in the model, are
;;; log-warped raised-cosines:

;;; Compile this:

(defun warped-raised-cos (wx &key (ctr-freq 1) (bandwidth 1))
  (let ((r-arg (* (/ pi bandwidth) (log-0 (/ wx ctr-freq) 2 bandwidth))))
    (if (< (- pi) r-arg pi)
	(1+ (cos r-arg))
	0.0)))

;;; Evaluate these:

(setq warped-raised-cos-1/2 (make-discrete-function
			   #'(lambda (wx) (warped-raised-cos wx :ctr-freq 1/2))
			   0.0 4.0))
(setq warped-raised-cos-1 (make-discrete-function
			   #'(lambda (wx) (warped-raised-cos wx :ctr-freq 1))
			   0.0 4.0))
(setq warped-raised-cos-2 (make-discrete-function
			   #'(lambda (wx) (warped-raised-cos wx :ctr-freq 2))
			   0.0 4.0))
(+. warped-raised-cos-1/2 warped-raised-cos-1 warped-raised-cos-2)

;;; With the log warping, the operators all have 1 octave bandwidth,
;;; and they still tile.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Feedback normalization:

;;; In addition to the linear operators, feedback normalization is the
;;; main component of the model.  The idea of normalizatin is to achieve a
;;; particular relationship between output and input (response and
;;; contrast), called the hyperbolic ratio:
;;; 
;;; 	      x^n
;;; R = Rm  ---------------
;;; 	 (x^n + sigma^n)
;;; 
;;; where x is the input, R is the output, sigma is a constant (the
;;; semi-saturation constant) and Rm is the asymptotically maximum
;;; attainable response.  Let's plot the hyperbolic ratio.

;;; For n=1, sigma=0.1
(make-discrete-function
 #'(lambda (x) (hyperbolic-ratio x :Rmax 1 :sigma 0.1 :n 1)) 0 1)

;;; For n=2, sigma=0.1, it rises and saturates faster
(make-discrete-function
 #'(lambda (x) (hyperbolic-ratio x :Rmax 1 :sigma 0.1 :n 2)) 0 1)

;;; On semi-log coordinates:
(make-log-discrete-function
 #'(lambda (x) (hyperbolic-ratio x :Rmax 1 :sigma 0.05 :n 2)) .01 1)

;;; On log-log coordinates:
(display
 (make-log-discrete-function
  #'(lambda (x) (hyperbolic-ratio x :Rmax 1 :sigma 0.05 :n 2)) .01 1)
 'graph :y-axis-type :log :y-range '(.1 1.0) :y-tick-step .1)

;;; Changing sigma shifts the curve laterally on log-log:
(display
 (make-log-discrete-function
  #'(lambda (x) (hyperbolic-ratio x :Rmax 1 :sigma 0.1 :n 2)) .01 1)
 'graph :y-axis-type :log :y-range '(.1 1.0) :y-tick-step .1)
(display
 (make-log-discrete-function
  #'(lambda (x) (hyperbolic-ratio x :Rmax 1 :sigma 0.2 :n 2)) .01 1)
 'graph :y-axis-type :log :y-range '(.1 1.0) :y-tick-step .1)

;;; In the model, this hyperbolic ratio input-output relationship is
;;; achieved in a feedback network.  The function test-feedback in
;;; striate-model.lisp is a one-input/one-output version of the feedback
;;; network.  In the actual model, all of the model cells are feeding back
;;; and suppressing each other.  The one-input/one-output version works as
;;; follows:
;;; 
;;; G(t) = (1 - alpha) G(t-1) + alpha R(t-1)
;;; R(t) = (x/sigma) [Rm - G(t)]
;;;
;;; where R(t) is the output, x is the input, G(t) is the feedback
;;; signal, and alpha controls the time averaging of the feedback.  It
;;; is critical that Rm > G(t).  Take a look at the function
;;; "test-feedback" in striate-model.lisp.  The input is taken to be
;;; constant over time.  The point of this function is to make sure
;;; that the feedback converges to the hyperbolic-ratio, given a
;;; constant input.  The function uses globally defined symbols (with
;;; *'s around them) to set the parameters.  You can change the
;;; parameter settings by calling the function with keywords or by
;;; setting the values of those globals.  We will use the same
;;; parameters below in the full-blown multi-input/multi-output model.

;;; Evaulate the following expression to see how the test-feedback
;;; function works.  First, it computes the hyperbolic-ratio function
;;; and prints the value that the response should converge to.  Then,
;;; it runs the feedback, updating R and G by one time step and
;;; printing their new values.  After several time steps R converges
;;; as we expect.

(let ((x 0.9) (sigma 0.5) (Rmax 1.0) (alpha 0.5))
  (setq response (test-feedback x :sigma sigma :Rmax Rmax
				:alpha alpha :initial-G 0.0 :initial-R 0.0
				:size 10)))

;;; Recall that alpha controls the time averaging of the feedback.  If
;;; we change the value of alpha, then the feedback takes a very long
;;; time to converge.

(let ((x 0.9) (sigma 0.5) (Rmax 1.0) (alpha 0.7))
  (setq response (test-feedback x :sigma sigma :Rmax Rmax
				:alpha alpha :initial-G 0.0 :initial-R 0.0
				:size 100)))

;;; If alpha is too large, then it won't converge at all.  Instead,
;;; the output oscillates:

(let ((x 0.9) (sigma 0.5) (Rmax 1.0) (alpha 0.8))
  (setq response (test-feedback x :sigma sigma :Rmax Rmax
				:alpha alpha :initial-G 0.0 :initial-R 0.0
				:size 20)))

;;; For the feedback to converge, the time constant tau must be > 1/2.  It
;;; is easy to show that:
;;; 
;;; 	      sigma
;;; tau =  -----------------
;;; 	 alpha (sigma + x)
;;; 
;;; In other words, to avoid oscillation:
;;; 
;;; 	    2 sigma
;;; alpha <  ----------
;;; 	    sigma + x
;;; 
;;; Here we choose alpha to be slighly less than that:

(let* ((x 0.9) (sigma 0.5) (Rmax 1.0)
       (alpha (* 0.9 (/ (* 2 sigma) (+ sigma x)))))
  (setq response (test-feedback x :sigma sigma :Rmax Rmax
				:alpha alpha :initial-G 0.0 :initial-R 0.0
				:size 20)))

;;; Here we choose alpha to be slightly greater:

(let* ((x 0.9) (sigma 0.5) (Rmax 1.0)
       (alpha (* 1.1 (/ (* 2 sigma) (+ sigma x)))))
  (setq response (test-feedback x :sigma sigma :Rmax Rmax
				:alpha alpha :initial-G 0.0 :initial-R 0.0
				:size 20)))

;;; In the model, we pick a value for sigma-0 (the smallest that sigma
;;; will ever be), and we pick a value for x-0 (the largest that the input
;;; will ever be - the input to the feedback network is proportional to
;;; contrast so it is bounded).  Finally, we pick alpha to guarantee
;;; convergence for sigma > sigma-0 and x < x-0.
;;; 
;;; Assuming that alpha is chosen to guarantee convergence, note in
;;; the above expression for tau that the time constant depends on the
;;; input.  Large inputs will cause the output to converge relatively
;;; quickly.  Small inputs will take longer.

;;; Slow convergence for small x:

(let* ((x-0 1.0) (sigma-0 0.1)
       (x 0.1) (sigma 0.2) (Rmax 1.0)
       (alpha (* 0.9 (/ (* 2 sigma-0) (+ sigma-0 x-0)))))
  (setq response (test-feedback x :sigma sigma :Rmax Rmax
				:alpha alpha :initial-G 0.0 :initial-R 0.0
				:size 20)))

;;; Fast convergence for large x:

(let* ((x-0 1.0) (sigma-0 0.1)
       (x 0.9) (sigma 0.2) (Rmax 1.0)
       (alpha (* 0.9 (/ (* 2 sigma-0) (+ sigma-0 x-0)))))
  (setq response (test-feedback x :sigma sigma :Rmax Rmax
				:alpha alpha :initial-G 0.0 :initial-R 0.0
				:size 20)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Parameters:

;;; There are a number of parameters in the model that can be used to
;;; configure it in different ways.
;;; 
;;; *sample-rate* Number of samples per second for the simulated
;;; post-stimulus time-histogram (PSTH), the simulated response over time.
;;; Default is 64 samples per sec and there should be no need to change
;;; it.
;;; 
;;; *sigma* Gain parameter.  Initially set to 0.1.
;;; 
;;; *alpha* Initially set to 0.01.  If you set *sigma* much less than 0.1,
;;; then you will have to change *alpha* as well to avoid oscillations in
;;; the responses.
;;;
;;; *length* Length (in secs) of each simulated PSTH.  Default is 4 sec
;;; and there should be no need to change it.
;;;
;;; *skip* With normalization, the model cells have initial transient
;;; behavior.  When you want to examine the transient behavior, set
;;; this to 0.  When you want to ignore the transient behavior, set it
;;; to 1 sec or 2 secs.  The default is 2 secs.
;;;
;;; *base-wx* Spatial-frequency of the center band.  Default is 1
;;; cycle/deg and there should be no need to change it.
;;; 
;;; *base-wt* Peak temporal frequency tuning of the motion-sensitive
;;; operators.  Default is 4 Hz and there should be no need to change it.
;;; 
;;; *band-spacing* Determines spatial bandwidhts and spacing between
;;; bands.  Default is 1, meaning octave bandwidth and octave spacing
;;; between spatial frequency bands.  There should be no need to change
;;; this.
;;; 
;;; *sigma-time* Deterimines temporal bandwidths.  There should be no need
;;; to change this.
;;; 
;;; *Rmax* Asympototically maximum attainable response.  Rmax is just
;;; a scale factor that could be used to convert from the arbitrary
;;; model response units to spikes per sec.  Default is 1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Linear responses:

;;; For the time being, we will work only with x-t operators, those
;;; that prefer vertical orientation.

(length (setq *filter-list* (make-filter-list-xt)))

;;; There are 36 linear operators.  Let's compute their responses for
;;; a drifting sinusoidal stimulus.

(setq responses-no-norm
      (compute-responses-no-norm *filter-list*
				 :length 1 :skip 0
				 :stimulus-func 'drifting-grating-linear-response
				 :stimulus-args '(:wx 1.0 :wy 0.0 :wt 4.0 :contrast 1.0)))

;;; This function returns a sequence of responses, one for each
;;; simulated cell.  There are 36 cells, so the sequence is 36 in
;;; length.  The sequence is displayed as a flipbook of graphs.  Each
;;; graph shows the response over time of a half-squared linear
;;; operator, that is, each is a half-squared sinusoid.  From one cell
;;; to the next only the amplitudes and phases of the output sinusoids
;;; are different.  The first four frames (number 0 to 3) in the
;;; flipbook are for operators with the same amplitude response but
;;; different phases.  These filters prefer rightward motion of a
;;; vertical grating with (wx = 1 and wt = 4).  The next four prefer
;;; leftward motion (wx = 1 and wt = -4).  The next four prefer
;;; stationary stimulus (wx = 1 and wt = 0).  The other 24 filters are
;;; tuned to either higher or lower spatial frequencies.  For this
;;; stimulus, they dont respond at all because their responses cut off
;;; entirely for wx = 1.

;;; Let's try a slightly higher spatial frequency:

(setq responses-no-norm
      (compute-responses-no-norm
       *filter-list* :length 1 :skip 0
       :stimulus-func 'drifting-grating-linear-response
       :stimulus-args (list :wx (sqrt 2) :wy 0.0 :wt 4.0 :contrast 1.0)))

;;; In this case, 24 of the 36 cells give non-zero responses, because
;;; the stimulus excites both the middle and high frequency bands.

;;; In the model, we view simple cells as normalized, half-squared
;;; linear operators and we view complex cells as normalized, energy
;;; mechanisms.  An energy mechanism averages the outputs of four
;;; half-squared linear operators, all with the same amplitude
;;; response, but with phases shifted in 90 degree steps.  For
;;; example, let's compute the response of an energy mechanism
;;; preferring vertical rightward moving stimuli:

(display (setq energy (+. (frame 0 responses-no-norm)
			  (frame 1 responses-no-norm)
			  (frame 2 responses-no-norm)
			  (frame 3 responses-no-norm)))
	 'graph :y-range '(0.0 0.5))

;;; The response is constant over time, as expected.  This property is
;;; important for understanding the behavior of the full model (with
;;; normalization).  It is easy to understand the feedback network,
;;; when the feedback signal is constant over time.  The feedback
;;; signal is the sum of all of the simple cell responses, that is, a
;;; sum of a bunch of energy responses.  For a drifting grating
;;; stimulus, each energy response is constant over time, so the
;;; summed feedback signal is constant over time.  Thus for a drifting
;;; grating stimulus, we can analytically derive the steady state
;;; behavior of the model.  We can also derive the time that it takes
;;; the model to reach steady state.  An expression for the time
;;; constant, tau, is given in the above section on feedback.

;;; Now let's look at responses to a counterphase grating.  "Luckily"
;;; the hairy mess of code in striate-model.lisp is set up to do that.
;;; A counterphase grating is simply a sum of two drifting gratings,
;;; of equal contrast, spatial frequency, and temporal frequency, but
;;; moving in opposite directions.

(setq cp-responses-no-norm
      (compute-responses-no-norm
       *filter-list*
       :length 1 :skip 0
       :stimulus-func 'counterphase-grating-linear-response
       :stimulus-args (list :wx (sqrt 2) :wy 0.0 :wt 4.0 :contrast 1.0)))

;;; And now, let's compute the energies.  We can use the function
;;; "compute-energies" that averages the half-squared responses to
;;; compute all of the energies from all of the responses

(display (setq cp-energies (compute-energies cp-responses-no-norm))
	 'flipbook :y-range '(0.0 0.3))

;;; For a counterphase grating, the energy response is not constant
;;; over time.  Rather, it modulates at twice the temporal frequency
;;; of the stimulus.  For a counterphase grating stimulus, we can not
;;; derive the model's behavior, since there is not "steady state".
;;; There is no signal in the model that is constant over time for a
;;; counterphase grating stimulus.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Now we have all the tools we need to simulate experimental
;;; results. First, let's compute the normalized responses (model
;;; simple cell responses) for a drifting sine grating:

(display (setq simple-responses-100
	       (compute-responses
		*filter-list* :length 1 :skip 0
		:stimulus-func 'drifting-grating-linear-response
		:stimulus-args (list :wx (sqrt 2) :wy 0.0 :wt 4.0 :contrast 1.0)))
	 'flipbook :y-range '(0.0 0.5))

;;; Ignoring the initial transient responses, the model simple cell
;;; responses vary over time as truncated (halfwave-squared),
;;; sinusoids.

;;; And now the normalized energies (model complex cell responses):

(display (setq energies-100 (compute-energies simple-responses-100))
	 'flipbook :y-range '(0.0 0.5))

;;; AFter the initial transient response, the response that rapidly
;;; converges to steady state.  For a lower contrast stimulus the
;;; magnitudes of the steady state responses are smaller.  In
;;; addition, it takes longer for the feedback to reach steady state,
;;; as discussed above in the section introducing feedback
;;; normalization.  For this 30% contrast stimulus, it takes almost a
;;; second (64 samples) to reach steady state, given the current
;;; choices for the *sigma* and *alpha* parameters.

(display (setq simple-responses-30
	       (compute-responses
		*filter-list* :length 2 :skip 0
		:stimulus-func 'drifting-grating-linear-response
		:stimulus-args (list :wx (sqrt 2) :wy 0.0 :wt 4.0 :contrast 0.3)))
	 'flipbook :y-range '(0.0 0.5))

(display (setq energies-30 (compute-energies simple-responses-30))
	 'flipbook :y-range '(0.0 0.5))

;;; For 10% contrast, it takes more than 2 secs to fully reach steady
;;; state.  In later simulations, we will ignore the first 2 secs of
;;; the responses, so that we are examing the steady state behavior.

(display (setq simple-responses-10
	       (compute-responses
		*filter-list* :length 2 :skip 0
		:stimulus-func 'drifting-grating-linear-response
		:stimulus-args (list :wx (sqrt 2) :wy 0.0 :wt 4.0 :contrast 0.1)))
	 'flipbook :y-range '(0.0 0.5))

(display (setq energies-10 (compute-energies simple-responses-10))
	 'flipbook :y-range '(0.0 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Summarizing the response waveforms:

;;; Energy mechanisms, like most complex cells, give a constant
;;; (unmodulated) output for drifting sine gratings.  Since the
;;; response over time is constant, we can easily summarize the
;;; response over time with a single number.  Simple cells give
;;; modulated responses to sine grating stimuli.  The responses
;;; modulate at the same rate as the stimulus temporal frequency.

;;; Many physiologists summarize simple cells responses with the
;;; amplitude and phase of the fundamental component of the response
;;; waveforms.  Since we are trying to simulate physiological
;;; experiments, we will do the same thing.  Keep in mind, however,
;;; that the responses of these nonlinear mechanisms are not fully
;;; described by amplitude and phase of the fundamental.

;;; The amplitude and phase are easily computed by taking the Fourier
;;; transform of the response waveform and picking off the values of
;;; the fundamental frequency component.  Let's look at the Fourier
;;; transform of an unnormalized response.

(setq fourier-mag (magnitude (fft (frame 0 responses-no-norm))))

;;; Since the stimulus modulated at 4Hz, we want to pick off the
;;; amplitude of the 4Hz component.  Since the response waveform is
;;; sampled at 64 samples per sec and there are a total of 128 samples
;;; (2 secs), the 4Hz component corresponds to position (/ (* 128 4)
;;; 64) = 8.

(iref fourier-mag 0 8)

;;; What is the rest of the stuff in the Fourier magnitude of the
;;; response waveform?  Remember, the response waveform is a
;;; half-squared sinusoid.  The half-squaring nonlinearity introduces
;;; distortions (frequency components other than those in the original
;;; signal).  The sinusoidal stimulus and the linear response both
;;; contain only one frequency component, but the half-squared
;;; response contains some dc and some energy at 8Hz, 12Hz, 20Hz, and
;;; 28Hz.  For comparison, let's just look at the Fourier transform of
;;; a half-squared sinusoid.

(magnitude (fft (make-synthetic-image
		 '(1 128)
		 #'(lambda (y x) (sqr (max 0.0 (sin (* 2-pi (/ 8 128) x)))))
		 :x-range '(0 127))))

;;; It's the same as the above fourier-mag, up to a scale factor.

;;; Even though the response waveform is not a pure sinusoid, we will
;;; use the amplitude and phase of the fundamental component as a
;;; summary of the response.  We have a function for computing the
;;; fundamental amplitude and phase of the response waveforms.  This
;;; loop prints the amplitude and phase for each of the unnormalizaed
;;; responses.  It is clear that the responses are in groups of four,
;;; all four with the same response magnitude and with phases in 90
;;; degree steps:

(dotimes (i 36)
      (multiple-value-setq (amp phase)
	(response-amplitude-and-phase (frame i responses-no-norm) 4))
      (format t "amp=~a  phase=~a~%" (tolerance amp) (tolerance (* (/ 180 pi) phase))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Contrast-response:

;;; Now, we will simulate the contrast-response relationship for a
;;; simple cell.  Take a look at the function
;;; "response-vs-log-contrast" in striate-model.lisp.  For each
;;; stimulus contrast, the function computes the normalized responses,
;;; and then computes the amplitude of the fundamental component of
;;; the first cell's response waveform.  It puts the responses in a
;;; log-discrete-function, that samples contrast in log steps.  We
;;; skip the first 2 secs of the response to avoid the transient
;;; behavior.  Here we compute the response for 7 different contrasts
;;; ranging from 1% to 100%.  This takes a while to compute, so please
;;; be patient!  You can watch the progress in the status line of the
;;; control panel, where we print the contrast level that is currently
;;; being simulated.  There will be 7 contrasts starting at 0.01 (1%)
;;; and finishing with 1.0 (100%).

(setq contrast-response-1
      (make-log-discrete-function
       #'(lambda (contrast)
	   (obv::status-message "contrast=~a" contrast)
	   (with-local-viewables
	       ((responses (compute-responses
			    *filter-list*
			    :stimulus-func 'drifting-grating-linear-response
			    :stimulus-args (list :contrast contrast :wx 1.0 :wy 0.0 :wt 4.0))))
	     (response-amplitude-and-phase (frame 0 responses) 4.0)))
       0.01 1.0 :base 10.0 :size 7))

;;; As expected we get a hyperbolic-ratio function.  We can plot it on
;;; log-log axes, or on semi-log to see the sigmoidal shape.

(setp :y-axis-type :log :y-range '(0.001 0.1) :y-tick-step 0.01 :plot-symbol :circle)
(setp :y-axis-type :linear :y-range '(0.0 0.08) :y-tick-step 0.01 :plot-symbol :circle)

;;; Also as expected, since *sigma*=0.1, 10% contrast corresponds to
;;; the half-maximal response.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Relative Responses are Independent of Contrast:

;;; Consider the response of a linear operator when presented with two
;;; different stimuli.  If the contrast of both stimuli are changed by
;;; the same factr then the ratio of the responses to the two stimuli
;;; remains unchanged.  Without normalization, the responses of linear
;;; and energy mechanisms increase with stimulus contrast over the
;;; entire range of contrasts.  With normalization, model cells
;;; saturate at high contrasts.  However, since all of the model cells
;;; are normalized by nearly the same factor, the relative responses
;;; are STILL mostly independent of contrast.

;;; Here we plot contrast-response functions for three different
;;; spatial frequencies on log-log axes.  The curve shifts mostly
;;; downward on log-log axes for non-optimal spatial frequencies.  The
;;; downward shift on log-log means that the contrast-response curve
;;; is scaled (multiplied by some scale factor) when switching to a
;;; non-optimal stimulus.

(setq contrast-response-1.4
      (make-log-discrete-function
       #'(lambda (contrast)
	   (obv::status-message "contrast=~a" contrast)
	   (with-local-viewables
	       ((responses (compute-responses
			    *filter-list*
			    :stimulus-func 'drifting-grating-linear-response
			    :stimulus-args (list :contrast contrast :wx 1.4 :wy 0.0 :wt 4.0))))
	     (response-amplitude-and-phase (frame 0 responses) 4.0)))
       0.01 1.0 :base 10.0 :size 7))
(setq contrast-response-0.6
      (make-log-discrete-function
       #'(lambda (contrast)
	   (obv::status-message "contrast=~a" contrast)
	   (with-local-viewables
	       ((responses (compute-responses
			    *filter-list*
			    :stimulus-func 'drifting-grating-linear-response
			    :stimulus-args (list :contrast contrast :wx 0.6 :wy 0.0 :wt 4.0))))
	     (response-amplitude-and-phase (frame 0 responses) 4.0)))
       0.01 1.0 :base 10.0 :size 7))
(setq cr-varying-sf
      (make-viewable-sequence
       (list contrast-response-1 contrast-response-1.4 contrast-response-0.6)
       :display-type 'overlay))
(progn
  (setp :current-picture 0 :plot-symbol :circle)
  (setp :current-picture 1 :plot-symbol :circle)
  (setp :current-picture 2 :plot-symbol :circle)
  (setp :y-axis-type :log :y-range '(0.001 0.1) :y-tick-step 0.01
	:x-range '(-2 0) :x-tick-step 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Cross-Frequency Suppression:

;;; A number of physiologists have reported that the excitatory
;;; response to a preferred stimulus can be suppressed by
;;; superimposing an additional stimulus.  This suppression has been
;;; found to be largely nonspecific.  It is independent of direction
;;; of motion, largely independent of orientation, broadly tuned for
;;; spatial frequency and it has broad spatial selectivity.  Here, we
;;; will do a simulation of the suppression by superimposing a second
;;; grating of variable spatial frequency.

;;; First, we simulate the spatial frequency tuning curve of one
;;; simple cell, that is, response vs spatial frequency for a single
;;; drifting grating (10% contrast, 2Hz, preferred orientation and
;;; direction of motion):

(setq sf-tuning
      (make-log-discrete-function
       #'(lambda (sf)
	   (obv::status-message "sf=~a" sf)
	   (with-local-viewables
	       ((responses (compute-responses
			    *filter-list*
			    :stimulus-func 'drifting-grating-linear-response
			    :stimulus-args (list :contrast 0.1 :wx sf :wy 0.0 :wt 2.0))))
	     (response-amplitude-and-phase (frame 0 responses) 2.0)))
       0.25 4.0 :base 2.0 :size 7))
(setp :y-range '(0.0 0.025) :y-tick-step 0.005 :plot-symbol :circle)

;;; Now we superimpose two gratings drifting in the same direction,
;;; one of optimal spatial frequency (1 c/deg and 2Hz), the other at
;;; variable sf and 3Hz.  We are looking for how the response to the
;;; first (2Hz base) grating depends on the second (mask) grating, so
;;; we look at the 2Hz component of the response.

(setq cross-sf
      (make-log-discrete-function
       #'(lambda (sf)
	   (obv::status-message "sf=~a" sf)
	   (with-local-viewables
	       ((responses (compute-responses
			    *filter-list*
			    :stimulus-func 'grating-pair-linear-response
			    :stimulus-args (list :contrast1 0.1 :wx1 1.0 :wy1 0.0 :wt1 2.0
						 :contrast2 0.1 :wx2 sf :wy2 0.0 :wt2 3.0))))
	     (response-amplitude-and-phase (frame 0 responses) 2.0)))
       0.25 4.0 :base 2.0 :size 7))
(setp :y-range '(0.0 0.03) :y-tick-step 0.005 :plot-symbol :circle)

;;; For a single grating (sf-tuning computed above), the maximum
;;; response is around .022.  Any drop below that .022 level indicates
;;; suppression from the second mask grating.  For some mask sf's
;;; there is suppression and for others the mask enhances the
;;; response.  The mask grating contributes both to the excitatory
;;; (linear) mechanism and to the suppressive (normalization)
;;; mechanism.

;;; We can get a better measure of the suppression by using an
;;; orthogonal mask grating.  This orthogonal grating will conribute
;;; just as much to the suppressive normalization, but not at all to
;;; the excitatory linear mechanism.  In order to do this simulation,
;;; we have to use all of the model's linear operators.  Until now, we
;;; have used only the vertical orientation.  Unfortunately, using all
;;; orientations takes even longer to compute.  Look at the status
;;; line of the control panel to chart the progress.

(length (setq *filter-list* (make-filter-list)))

(setq cross-sf-orthogonal
      (make-log-discrete-function
       #'(lambda (sf)
	   (obv::status-message "sf=~a" sf)
	   (with-local-viewables
	       ((responses (compute-responses
			    *filter-list*
			    :stimulus-func 'grating-pair-linear-response
			    :stimulus-args (list :contrast1 0.1 :wx1 1.0 :wy1 0.0 :wt1 2.0
						 :contrast2 0.1 :wx2 0.0 :wy2 sf :wt2 3.0))))
	     (response-amplitude-and-phase (frame 0 responses) 2.0)))
       0.25 4.0 :base 2.0 :size 7))
(setp :y-range '(0.0 0.022) :y-tick-step 0.005 :plot-symbol :circle)

;;; The result is a 3 octave wide suppression due to the mask grating.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Contrast-Response Versus Mask Contrast:

;;; The suppression from normalization is contrast dependent.  Higher
;;; mask contrasts yield greater suppression.  Thus, the suppression
;;; from normalization acts as a kind of automatic gain control.  We
;;; can see this most easily by measuring contrast-response functions
;;; for several different mask contrasts.  We measure the 2Hz
;;; component of response, for a 2Hz test grating and a 3Hz mask.  For
;;; a fixed mask contrast, we measure responses for several test
;;; contrasts yielding a contrast-response curve.  Then we vary the
;;; mask contrast to get another contrast-response curve.  These
;;; contrast-response curves shift laterally on log-log axes,
;;; indicative of divisive suppression.  Changing the contrast of the
;;; mask grating is the same as rescaling contrast itself.

(setq contrast-response-mask-0
      (make-log-discrete-function
       #'(lambda (contrast)
	   (obv::status-message "contrast=~a" contrast)
	   (with-local-viewables
	       ((responses (compute-responses
			    *filter-list*
			    :stimulus-func 'grating-pair-linear-response
			    :stimulus-args (list :contrast1 contrast :wx1 1.0 :wy1 0.0 :wt1 2.0
						 :contrast2 0.0 :wx2 0.0 :wy2 1.0 :wt2 3.0))))
	     (response-amplitude-and-phase (frame 0 responses) 2.0)))
       (expt 10.0 -1.5) (expt 10.0 -0.25) :base 10.0 :size 6))
(setq contrast-response-mask-20
      (make-log-discrete-function
       #'(lambda (contrast)
	   (obv::status-message "contrast=~a" contrast)
	   (with-local-viewables
	       ((responses (compute-responses
			    *filter-list*
			    :stimulus-func 'grating-pair-linear-response
			    :stimulus-args (list :contrast1 contrast :wx1 1.0 :wy1 0.0 :wt1 2.0
						 :contrast2 0.2 :wx2 0.0 :wy2 1.0 :wt2 3.0))))
	     (response-amplitude-and-phase (frame 0 responses) 2.0)))
       (expt 10.0 -1.5) (expt 10.0 -0.25) :base 10.0 :size 6))
(setq cr-varying-mask-contrast
      (make-viewable-sequence
       (list contrast-response-mask-0 contrast-response-mask-20)
       :display-type 'overlay))
(progn
  (setp :current-picture 0 :plot-symbol :circle)
  (setp :current-picture 1 :plot-symbol :circle)
  (setp :y-axis-type :log :y-range '(0.001 0.04) :y-tick-step 0.01
	:x-range '(-2.0 0.0) :x-tick-step 0.5
	:zoom 25))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Cross-Orientation Suppression:

(length (setq *filter-list* (make-filter-list)))

(setq *sigma* 0.1)

(progn
  (setq responses (compute-responses
		   *filter-list*
		   :stimulus-func 'drifting-grating-linear-response
		   :stimulus-args (list :contrast 0.1 :wx 1.0 :wy 0.0 :wt 2.0)))
  (setq response-to-base (response-amplitude-and-phase (frame 0 responses) 2.0)))
      
(setq cross-orientation
      (make-discrete-function
       #'(lambda (angle)
	   (obv::status-message "angle=~a" angle)
	   (with-local-viewables
	       ((wx2 (cos (* (/ pi 180) angle)))
		(wy2 (sin (* (/ pi 180) angle)))
		(responses (compute-responses
			    *filter-list*
			    :stimulus-func 'grating-pair-linear-response
			    :stimulus-args (list :contrast1 0.1 :wx1 1.0 :wy1 0.0 :wt1 2.0
						 :contrast2 0.1 :wx2 wx2 :wy2 wy2 :wt2 3.0))))
	     (response-amplitude-and-phase (frame 0 responses) 2.0)))
       -70.0 70.0 :size 15))
(progn
  (setf base-response (similar cross-orientation))
  (fill! (data base-response) response-to-base))
(setq cross-orientation-inhib
      (make-viewable-sequence
       (list cross-orientation base-response)
       :display-type 'overlay))
(progn
  (setp :current-picture 0 :plot-symbol :circle)
  (setp :x-range '(-90 90) :x-tick-step 30
	:y-range '(0.0 0.025) :y-tick-step 0.01
	:zoom 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Direction Selectivity, Counterphase Vs. Drifting Gratings:

;;; *** This also works, but now you're really on your own...
