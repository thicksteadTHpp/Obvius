
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
ToDo:

- Initial responses are not being modeled correctly.  The underlying linear
  operators do not "step" on.  Rather, they come on smoothly depending on the
  temporal integration period of the operator.  Fixing this will make it easier
  to avoid instability in the feedback network.
- Worry about effect of saturating nonlinearity that occurs internally,
  regardless of normalization (see below "internal nonlinearity test").

Done:

- Sample rate, length in secs, temporal freq in Hz.
- Change *base-wt* (i.e., change *sigma-time*) and automatically adjusts
  scale factors so that it still tiles.

Notes:

- Scaling factors, in order to tile:

norm = k (Ixxt)^2 (f) + (Ixxx)^2 (1/f)
     = a1 (Ixxx+Ixxt)^2 + a1 (Ixxx-Ixxt)^2 + a2 (Ixxx)^2

a1 = (k/2) f
a2 = (1/f) - k f
k = 2 is about right (to give smooth falloff vs wt)


- (Rmax - G) must not be negative, particularly if dendrite responses 
  can be negative.  Currently this is implemented by clipping Rmax-G in
  normalize-the-responses, but this is a bit of a hack.


- Time constant:

Alpha must be less than (/ (* 2 *sigma*) (+ *sigma* sum-X))
so that feedback converges.  But, the responses are "cleaner" for
values that are somewhat less.  Picking alpha = sigma is about right
(converges easily for sum-X <= 1).

Time constant,  tau =  (/ *sigma* (* *alpha* (+ *sigma* sum-X)))
For very small sum-X, tau -> (/ 1 *alpha*).

For high contrasts (e.g., sum-X = 1), effective time constant is the same
regardless of sigma.

(setq *sigma* 0.1 *alpha* 0.1 sum-X 1.0
      tau (/ (/ *sigma* (* *alpha* (+ *sigma* sum-X))) *sample-rate*))
(setq *sigma* 0.01 *alpha* 0.01 sum-X 1.0
      tau (/ (/ *sigma* (* *alpha* (+ *sigma* sum-X))) *sample-rate*))

For low contrasts (e.g., sum-X = 0.1), however, effective time constant is
much longer for small sigma.

(setq *sigma* 0.1 *alpha* 0.1 sum-X 0.1
      tau (/ (/ *sigma* (* *alpha* (+ *sigma* sum-X))) *sample-rate*))
(setq *sigma* 0.01 *alpha* 0.01 sum-X 0.1
      tau (/ (/ *sigma* (* *alpha* (+ *sigma* sum-X))) *sample-rate*))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;; Internal nonlinearity test: Worry about effect of saturating
;;; nonlinearity that occurs internally, regardless of normalization.

(defun foo (x)
  (if (< x 1) x (1+ (log x))))

(defun foo (x)
  (if (< x 1) x 1.0))

(make-discrete-function 'foo 0.0 10.0)

(setq sigma 0.1)
(make-log-discrete-function '(lambda (I) (/ I (+ sigma I)))
			    0.01 100.0 :base 10.0)
(overlay-display (make-log-discrete-function '(lambda (I) (/ (foo I) (+ sigma (foo I))))
					     0.01 100.0 :base 10.0))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Space-time separable pre-filter: a spatially-radial raised cosine
;; divided by r^2.5, gaussian in time.
(defun raised-cos-fn (&key (ctr-freq (/ pi 2))
			   (bandwidth *band-spacing*)
			   (sigma-t *sigma-time*))
  (let ((iroot2 (/ (sqrt 2))))
    #'(lambda (wx wy wt)
	(let* ((r (sqrt (+ (sqr wy) (sqr wx))))
	       (r-arg (* (/ pi bandwidth) (log-0 (/ r ctr-freq) 2 bandwidth))))
	  (* iroot2			;this makes norm roughly 1.0
	     (exp (- (/ (sqr wt) (sqr sigma-t))))
	     (if (< (- pi) r-arg pi)
		 (* (sqrt (1+ (cos r-arg))) (sqrt r) (/-0 1.0 (cube r)))
		 0.0))))))

(defun cube (x)
  (* x x x))

(defun Ixxx-freq (wx wy wt)
  (declare (ignore wt wy))
  (* wx wx wx))
(defun Iyyy-freq (wx wy wt)
  (declare (ignore wt wx))
  (* wy wy wy))
(defun Ippp-freq (wx wy wt)
  (declare (ignore wt))
  (/ (+ (* wx wx wx) (* wy wy wy) (* 3.0 wx wx wy) (* 3.0 wx wy wy))
     (* 2 (sqrt 2))))
(defun Iqqq-freq (wx wy wt)
  (declare (ignore wt))
  (/ (+ (* wx wx wx) (- (* wy wy wy)) (* -3.0 wx wx wy) (* 3.0 wx wy wy))
     (* 2 (sqrt 2))))

(defun Ixxt-freq (wx wy wt)
  (declare (ignore wy))
  (* wx wx wt))
(defun Iyyt-freq (wx wy wt)
  (declare (ignore wx))
  (* wy wy wt))
(defun Ippt-freq (wx wy wt)
  (* 1/2 (+ (* wx wx wt) (* wy wy wt) (* 2.0 wx wy wt))))
(defun Iqqt-freq (wx wy wt)
  (* 1/2 (+ (* wx wx wt) (* wy wy wt) (* -2.0 wx wy wt))))

(defun Ixxx+Ixxt-freq (wx wy wt)
  (+ (Ixxx-freq wx wy wt) (Ixxt-freq wx wy wt)))
(defun Ixxx-Ixxt-freq (wx wy wt)
  (- (Ixxx-freq wx wy wt) (Ixxt-freq wx wy wt)))
(defun Iyyy+Iyyt-freq (wx wy wt)
  (+ (Iyyy-freq wx wy wt) (Iyyt-freq wx wy wt)))
(defun Iyyy-Iyyt-freq (wx wy wt)
  (- (Iyyy-freq wx wy wt) (Iyyt-freq wx wy wt)))
(defun Ippp+Ippt-freq (wx wy wt)
  (+ (Ippp-freq wx wy wt) (Ippt-freq wx wy wt)))
(defun Ippp-Ippt-freq (wx wy wt)
  (- (Ippp-freq wx wy wt) (Ippt-freq wx wy wt)))
(defun Iqqq+Iqqt-freq (wx wy wt)
  (+ (Iqqq-freq wx wy wt) (Iqqt-freq wx wy wt)))
(defun Iqqq-Iqqt-freq (wx wy wt)
  (- (Iqqq-freq wx wy wt) (Iqqt-freq wx wy wt)))


;;; Filter-list is list of lists.  Each sublist is of the form:
;;; (base-func freq-func-xt filter-phase scale-factor).
(defun make-filter-list-xt (&key (transient-multiplier 2.0) (total-multiplier 4.0))
  (setq *band-spacing* 1)		; 1 octave
  (setq *sigma-time* (* 3/2 *base-wt*))
  (let* ((ctr-freqs (list *base-wx* (* 2 *base-wx*) (* 1/2 *base-wx*)))
	 (max-ctr-freq (apply 'max ctr-freqs))
	 (num-phases 4)
	 ;; for *sigma-time*=6, largest effect transient-multiplier=9.0
	 (transient-scale-factor (min (/ 1 (sqr max-ctr-freq))
				      (/ transient-multiplier (sqr *sigma-time*))))
	 (total-scale-factor (/ total-multiplier num-phases))
	 (phases (loop for i from 0 below num-phases
		       collect (* i (/ 2-pi num-phases))))
	 (m-scales (mapcar #'(lambda (wx) (* total-scale-factor
					     (/ transient-scale-factor 2) wx))
			   ctr-freqs))
	 (s-scales (mapcar #'(lambda (wx) (* total-scale-factor
					     (- (/ 1 wx) (* transient-scale-factor wx))))
			   ctr-freqs))
	 (filter-list nil))
    (print-db m-scales s-scales)
    (unless (notany 'minusp (append m-scales s-scales))
      (error "Negative scale factors ~a ~a" m-scales s-scales))
    (loop for ctr-freq in ctr-freqs
	  for m-scale in m-scales
	  for s-scale in s-scales
	  do
	  (loop for freq-function in (list 'Ixxx+Ixxt-freq 'Ixxx-Ixxt-freq 'Ixxx-freq)
		for scale in (list m-scale m-scale s-scale)
		do
		(setq filter-list (append filter-list
					  (loop for phase in phases collect
						(list (raised-cos-fn :ctr-freq ctr-freq)
						      freq-function phase scale))))))
    filter-list))

(defun make-filter-list (&key (transient-multiplier 2.0) (total-multiplier 4.0))
  (setq *band-spacing* 1)		; 1 octave
  (setq *sigma-time* (* 3/2 *base-wt*))
  (let* ((ctr-freqs (list *base-wx* (* 2 *base-wx*) (* 1/2 *base-wx*)))
	 (max-ctr-freq (apply 'max ctr-freqs))
	 (num-phases 4)
	 ;; for *sigma-time*=6, largest effect transient-multiplier=9.0
	 (transient-scale-factor (min (/ 1 (sqr max-ctr-freq))
				      (/ transient-multiplier (sqr *sigma-time*))))
	 (total-scale-factor (/ total-multiplier num-phases))
	 (phases (loop for i from 0 below num-phases
		       collect (* i (/ 2-pi num-phases))))
	 (m-scales (mapcar #'(lambda (wx) (* total-scale-factor
					     (/ transient-scale-factor 2) wx))
			   ctr-freqs))
	 (s-scales (mapcar #'(lambda (wx) (* total-scale-factor
					     (- (/ 1 wx) (* transient-scale-factor wx))))
			   ctr-freqs))
	 (filter-list nil))
    (unless (notany 'minusp (append m-scales s-scales))
      (error "Negative scale factors ~a ~a" m-scales s-scales))
    (loop for ctr-freq in ctr-freqs
	  for m-scale in m-scales
	  for s-scale in s-scales
	  do
	  (loop for freq-function in (list 'Ixxx+Ixxt-freq 'Ixxx-Ixxt-freq 'Ixxx-freq
					   'Iyyy+Iyyt-freq 'Iyyy-Iyyt-freq 'Iyyy-freq
					   'Ippp+Ippt-freq 'Ippp-Ippt-freq 'Ippp-freq
					   'Iqqq+Iqqt-freq 'Iqqq-Iqqt-freq 'Iqqq-freq)
		for scale in (list m-scale m-scale s-scale
				   m-scale m-scale s-scale
				   m-scale m-scale s-scale
				   m-scale m-scale s-scale)
		do
		(setq filter-list (append filter-list
					  (loop for phase in phases collect
						(list (raised-cos-fn :ctr-freq ctr-freq)
						      freq-function phase scale))))))
    filter-list))

#|
;;; sigma-t = 6.0 gives peak near 4Hz
(progn
  (setq sigma-t (* 3/2 4))
  (setq wx *base-wx*)
  (setq base-func (raised-cos-fn :ctr-freq wx :sigma-t sigma-t))
  (make-log-discrete-function
   #'(lambda (wt)
       (frequency-response 'Ixxx+Ixxt-freq base-func 1.0 wx 0.0 wt))
   1/32 16 :base 2.0))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Globals:

(defvar *base-wx*)
(defvar *base-wt*)
(defvar *base-contrast*)
(defvar *sample-rate*)
(defvar *length*)
(defvar *skip*)
(defvar *band-spacing*)
(defvar *sigma-time*)
(defvar *filter-list*)
(defvar *clip-max*)
(defvar *over-rectify-threshold*)
(defvar *dendrite-nonlinearity*)
(defvar *freq-resp-func*)
(defvar *output-nonlinearity*)
(defvar *Rmax*)
(defvar *sigma*)
(defvar *alpha*)
(defvar *initial-feedback*)
(defvar *initial-response*)

;;; For stimuli:
(setq *base-wx* 1.0)
(setq *base-wt* 4.0)
(setq *base-contrast* 1.0)
(setq *sample-rate* 64)			; 64 samples per sec (i.e., 4Hz -> period of 16 samples)
(setq *length* 4)			; length is in seconds
(setq *skip* 2)				; skip the first 1 sec of response for analysis

;;; For filters:
(setq *band-spacing* 1)		; one octave spacing
(setq *sigma-time* (* 3/2 *base-wt*))
(setq *filter-list* (make-filter-list-xt))
;;(setq *filter-list* (make-filter-list-xt :transient-multiplier 9.0 :total-multiplier 2.0))

;;; For nonlinearities:
(setq *clip-max* 1.0)
(setq *over-rectify-threshold* 0.0)

;;; For feedback normalization:
(setq *output-nonlinearity* 'halfwave-rectify)
(setq *Rmax* 1.0)
(setq *sigma* 0.1)
(setq *alpha* (sqr *sigma*))
(setq *initial-feedback* 0.0)
(setq *initial-response* 0.0)

;;; For linear response:
;;; Either use 'frequency-response2 with 'identity (linear outputs
;;; tile) OR use 'frequency-response with 'signed-square (sqr'd linear
;;; outputs tile).

(setq *dendrite-nonlinearity* 'signed-square)
(setq *freq-resp-func* 'frequency-response)
;;(setq *dendrite-nonlinearity* 'identity)
;;(setq *freq-resp-func* 'frequency-response2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Linear outputs tile, use (setq *dendrite-nonlinearity* 'identity).
(defun frequency-response2 (freq-func base-func contrast wx wy wt)
  (* contrast (sqr (* (funcall freq-func wx wy wt) (funcall base-func wx wy wt)))))

;;; Sqr'd linear outputs tile, use (setq *dendrite-nonlinearity* 'signed-square).
(defun frequency-response (freq-func base-func contrast wx wy wt)
  (* contrast (funcall freq-func wx wy wt) (funcall base-func wx wy wt)))

;;; One-d-image with time is in the x direction.
(defun drifting-grating-linear-response
    (base-func freq-func filter-phase
     &key (length *length*)
     (sample-rate *sample-rate*)
     (freq-resp-function *freq-resp-func*)
     (wx *base-wx*) (wy 0.0) (wt *base-wt*)
     (stimulus-phase 0.0) (contrast *base-contrast*)
     ->)
  ;; reset contrast according to attenuation factor
  (let ((amplitude (abs (funcall freq-resp-function freq-func base-func contrast wx wy wt))))
    (make-sin-grating (* length sample-rate)
		      :phase (+ stimulus-phase filter-phase)
		      :x-freq (* 2-pi (/ wt sample-rate))
		      :y-freq (sqrt (+ (sqr wx) (sqr wy)))
		      :amplitude amplitude
		      :-> ->)))

(defun counterphase-grating-linear-response
    (base-func freq-func filter-phase
     &key (length *length*)
     (wx *base-wx*) (wy 0.0) (wt *base-wt*)
     (stimulus-phase 0.0) (contrast *base-contrast*)
     ->)
  (grating-pair-linear-response
   base-func freq-func filter-phase
   :length length
   :wx1 wx :wy1 wy :wt1 wt
   :stimulus-phase1 stimulus-phase
   :contrast1 (/ contrast 2)
   :wx2 wx :wy2 wy :wt2 (- wt)
   :stimulus-phase2 stimulus-phase
   :contrast2 (/ contrast 2)
   :-> ->))
       
(defun grating-pair-linear-response
    (base-func freq-func filter-phase
     &key (length *length*)
     (wx1 *base-wx*) (wy1 0.0) (wt1 *base-wt*)
     (stimulus-phase1 0.0) (contrast1 *base-contrast*)
     (wx2 *base-wx*) (wy2 0.0) (wt2 *base-wt*)
     (stimulus-phase2 0.0) (contrast2 *base-contrast*)
     ->)
  (with-local-viewables
      ((resp1 (drifting-grating-linear-response
	       base-func freq-func filter-phase
	       :length length
	       :wx wx1 :wy wy1 :wt wt1
	       :stimulus-phase stimulus-phase1 :contrast contrast1))
       (resp2 (drifting-grating-linear-response
	       base-func freq-func filter-phase
	       :length length
	       :wx wx2 :wy wy2 :wt wt2
	       :stimulus-phase stimulus-phase2 :contrast contrast2)))
    (add resp1 resp2 :-> ->)))

#|
;;; Test linear-response functions

(progn
  (setq base-func (raised-cos-fn :ctr-freq *base-wx*))
  ;;(setq freq-func 'Ixxx-freq)
  (setq freq-func 'Ixxx+Ixxt-freq)
  (setq *freq-resp-func* 'frequency-response)
  )

(drifting-grating-linear-response base-func freq-func 0.0 :length 1)
(counterphase-grating-linear-response base-func freq-func 0.0 :length 1)

;;; Constant response for gratings, freq doubled for counterphase.
(+. (square (drifting-grating-linear-response base-func freq-func 0.0 :length 1))
    (square (drifting-grating-linear-response base-func freq-func (/ pi 2.0) :length 1)))
(+. (square (counterphase-grating-linear-response base-func freq-func 0.0 :length 1))
    (square (counterphase-grating-linear-response base-func freq-func (/ pi 2.0) :length 1)))

;;; With abs-value, get modulated response.

;;; For grating, phase does not effect amplitude of modulation.  
(+. (abs-value (drifting-grating-linear-response base-func freq-func 0.0 :length 1))
    (abs-value (drifting-grating-linear-response base-func freq-func (/ pi 2.0) :length 1)))
(+. (abs-value (drifting-grating-linear-response base-func freq-func 0.0 :length 1
						 :stimulus-phase (/ pi 4.0)))
    (abs-value (drifting-grating-linear-response base-func freq-func (/ pi 2.0) :length 1
						 :stimulus-phase (/ pi 4.0))))

;;; For counterphase, phase does effect amplitude of modulation (but not much).
(+. (abs-value (counterphase-grating-linear-response base-func freq-func 0.0 :length 1))
    (abs-value (counterphase-grating-linear-response base-func freq-func (/ pi 2.0) :length 1)))
(+. (abs-value (counterphase-grating-linear-response base-func freq-func 0.0 :length 1
						     :stimulus-phase (/ pi 8.0)))
    (abs-value (counterphase-grating-linear-response base-func freq-func (/ pi 2.0) :length 1
						     :stimulus-phase (/ pi 8.0))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tiling

(defun test-spatial-tiling
    (filter-list
     &key (wt 0.0)
     (freq-resp-function *freq-resp-func*)
     (nonlinearity *dendrite-nonlinearity*)
     (min (/ *base-wx* 4.0)) (max (* *base-wx* 4.0))
     (df-size 10))
  (make-log-discrete-function
   #'(lambda (wx)
       (loop with contrast = 1.0
	     for filter in filter-list
	     for base-func = (first filter)
	     for freq-func = (second filter)
	     for scale-factor = (fourth filter)
	     sum
	     (abs (* scale-factor
		     (funcall nonlinearity
			      (funcall freq-resp-function freq-func base-func
				       contrast wx 0.0 wt))))))
   min max :base 2.0 :size df-size))

(defun test-temporal-tiling
    (filter-list
     &key (wx *base-wx*)
     (freq-resp-function *freq-resp-func*)
     (nonlinearity *dendrite-nonlinearity*)
     (min 0.0) (max (* 4 *base-wt*))
     (df-size 10))
  (make-discrete-function
   #'(lambda (wt)
       (loop with contrast = 1.0
	     for filter in filter-list
	     for base-func = (first filter)
	     for freq-func = (second filter)
	     for scale-factor = (fourth filter)
	     sum
	     (abs (* scale-factor
		     (funcall nonlinearity
			      (funcall freq-resp-function freq-func base-func
				       contrast wx 0.0 wt))))))
   min max :size df-size))

(defun test-2d-spatial-tiling
    (filter-list
     &key (wt 0.0)
     (freq-resp-function *freq-resp-func*)
     (nonlinearity *dendrite-nonlinearity*)
     (dims '(32 32)) (range '(-4.0 4.0)))
  (make-synthetic-image
   dims
   #'(lambda (wy wx)
       (loop with contrast = 1.0
	     for filter in filter-list
	     for base-func = (first filter)
	     for freq-func = (second filter)
	     for scale-factor = (fourth filter)
	     sum
	     (abs (* scale-factor
		     (funcall nonlinearity
			      (funcall freq-resp-function freq-func base-func
				       contrast wx wy wt))))))
   :x-range range :y-range range))

#|
;;; Test tiling

(progn
  (setq *base-wt* 4.0)
  (setq *filter-list* (make-filter-list-xt))
  nil)

(display
 (make-viewable-sequence
  (list (test-spatial-tiling *filter-list* :wt 0.0 :df-size 10)
	(test-spatial-tiling *filter-list* :wt (/ *base-wt* 2) :df-size 10)
	(test-spatial-tiling *filter-list* :wt *base-wt* :df-size 10)
	(test-spatial-tiling *filter-list* :wt (* *base-wt* 2) :df-size 10)))
 'overlay
  :y-range '(0.0 4.5) :y-tick-step 1.0)

(display
 (make-viewable-sequence
  (list (test-temporal-tiling *filter-list* :wx *base-wx* :df-size 20)
	(test-temporal-tiling *filter-list* :wx (* *base-wx* 2) :df-size 20)
	(test-temporal-tiling *filter-list* :wx (* *base-wx* (sqrt 2)) :df-size 20)
	(test-temporal-tiling *filter-list* :wx (/ *base-wx* 2) :df-size 20)
	(test-temporal-tiling *filter-list* :wx (/ *base-wx* (sqrt 2)) :df-size 20)))
 'overlay
 :y-range '(0.0 4.4) :y-tick-step 1.0)

;;; Retest tiling using actual filters

;;; Either use 'frequency-response2 with 'identity (linear outputs tile)
;;; OR use 'frequency-response with 'signed-square (sqr'd linear outputs tile)
(progn
  ;;(setq *dendrite-nonlinearity* 'identity)
  ;;(setq *freq-resp-func* 'frequency-response2)
  (setq *dendrite-nonlinearity* 'signed-square)
  (setq *freq-resp-func* 'frequency-response)
  )

(setq dendrite-responses (compute-dendrite-responses
			  *filter-list* :length 1
			  :stimulus-func 'drifting-grating-linear-response
			  :stimulus-args (list :contrast 1.0
					       ;; :wx *base-wx* :wt 0.0
					       ;; :wx *base-wx* :wt (/ *base-wt* 4)
					       :wx *base-wx* :wt (/ *base-wt* 2)
					       ;; :wx *base-wx* :wt *base-wt*
					       ;; :wx (* *base-wx* 2) :wt 0.0
					       ;; :wx (* *base-wx* 2) :wt (/ *base-wt* 2)
					       ;; :wx (/ *base-wx* 2) :wt 0.0
					       ;; :wx (/ *base-wx* 2) :wt (/ *base-wt* 2)
					       )))

;;; check that the responses sum to 1
(progn
  (setq dend-rect (halfwave-rectify dendrite-responses))
  (setq X-sum (similar (first (obv::image-list dend-rect))))
  (loop for X in (obv::image-list dend-rect) do
	(add X X-sum :-> X-sum))
  (display X-sum)
  (mean X-sum))

;;; spatial tiling of full (all orientations) filter set
(setq *filter-list* (make-filter-list))
(test-2d-spatial-tiling *filter-list* :wt 0.0 :range '(-4.0 4.0) :dims '(32 32))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Nonlinearities

(defmethod sub-threshold ((vbl viewable) &key (threshold *over-rectify-threshold*))
  (sub vbl threshold))

(defmethod signed-square ((vbl viewable))
  (mul vbl (abs-value vbl)))

(defmethod signed-square ((x number))
  (* x (abs x)))

(defmethod half-square ((vbl viewable) &key &allow-other-keys)
  (with-local-viewables
      ((clip-vbl (point-maximum vbl 0.0)))
    (square clip-vbl)))

(defmethod half-square ((x number) &key &allow-other-keys)
  (sqr (max x 0.0)))

(defmethod halfwave-rectify ((vbl viewable))
  (point-maximum vbl 0.0))

(defmethod halfwave-rectify ((x number))
  (max x 0.0))

(defmethod over-rectify ((vbl viewable) &key (threshold *over-rectify-threshold*))
  (with-local-viewables
      ((thresh-vbl (sub vbl threshold)))
    (point-maximum thresh-vbl 0.0)))

(defmethod over-rectify ((x number) &key (threshold *over-rectify-threshold*))
  (max (- x threshold) 0.0))

(defmethod clip-rectify ((vbl viewable) &key (clip-max *clip-max*))
  (clip vbl 0.0 clip-max))

(defmethod clip-rectify ((x number) &key (clip-max *clip-max*))
  (clip x 0.0 clip-max))

;;; *** not debugged
(defmethod soft-clip-rectify ((vbl viewable) &key (clip-max *clip-max*))
  (point-operation vbl #'(lambda (x)
			   (setq x (max x 0.0))
			   (setq x (/ x clip-max))
			   (* clip-max (if (> x 1.0) (1+ (log x 10.0)) x)))))

;;; *** not debugged
(defmethod soft-clip-rectify ((x number) &key (clip-max *clip-max*))
  (setq x (max x 0.0))
  (setq x (/ x clip-max))
  (* clip-max (if (> x 1.0) (1+ (log x 10.0)) x)))

(defmethod hyperbolic-ratio ((x number) &key (Rmax 1.0) (sigma 0.0) (n 1.0))
  (* Rmax (/ (expt x n) (+ (expt x n) (expt sigma n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun response-amplitude-and-phase
    (response frequency &key (sample-rate *sample-rate*))
  (with-local-viewables
      ((fft-resp (fft response))
       (magnitude (magnitude fft-resp))
       (phase (complex-phase fft-resp)))
    (let* ((size (x-dim response))
	   (sqrt-size (sqrt size))
	   (pixel (floor (* size frequency) sample-rate))
	   (response-mag (/ (iref magnitude 0 pixel) sqrt-size))
	   (response-phase (iref phase 0 pixel)))
      ;;(print-db response-mag response-phase)
      (values response-mag response-phase))))

(defun compute-energies (simple-responses)
  (with-local-viewables ((tmp (similar (frame 0 simple-responses))))
    (make-image-sequence
     (loop for i from 0 below (sequence-length simple-responses) by 4
	   collect
	   (add (frame i simple-responses)
		(add (frame (+ 1 i) simple-responses)
		     (add (frame (+ 2 i) simple-responses)
			  (frame (+ 3 i) simple-responses) :-> tmp)
		     :-> tmp))))))

(defun compute-responses
    (filter-list
     &key (length *length*) (skip *skip*)
     (stimulus-func 'drifting-grating-linear-response)
     (stimulus-args (list :wx *base-wx* :wy 0.0 :wt *base-wt*
			  :stimulus-phase 0.0 :contrast *base-contrast*))
     (dendrite-nonlinearity *dendrite-nonlinearity*)
     (sigma *sigma*) (alpha *alpha*) (Rmax *Rmax*)
     (output-nonlinearity *output-nonlinearity*))
  (with-local-viewables
      ((dendrite-responses (compute-dendrite-responses
			    filter-list
			    :length length
			    :stimulus-func stimulus-func
			    :stimulus-args stimulus-args
			    :dendrite-nonlinearity dendrite-nonlinearity))
       (normalized-responses (normalize-the-responses
			      dendrite-responses
			      :sigma sigma :alpha alpha :Rmax Rmax
			      :output-nonlinearity output-nonlinearity)))
    (let* ((skip-samples (* *sample-rate* skip))
	   (size-samples (* *sample-rate* length))
	   (cropped-samples (- size-samples skip-samples)))
      (crop normalized-responses :x skip-samples :x-size cropped-samples))))

(defun compute-responses-no-norm
    (filter-list
     &key (length *length*) (skip *skip*)
     (stimulus-func 'drifting-grating-linear-response)
     (stimulus-args (list :wx *base-wx* :wy 0.0 :wt *base-wt*
			  :stimulus-phase 0.0 :contrast *base-contrast*))
     (dendrite-nonlinearity *dendrite-nonlinearity*)
     (output-nonlinearity *output-nonlinearity*)
     &allow-other-keys)
  (with-local-viewables
      ((dendrite-responses (compute-dendrite-responses
			    filter-list
			    :length length
			    :stimulus-func stimulus-func
			    :stimulus-args stimulus-args
			    :dendrite-nonlinearity dendrite-nonlinearity))
       (output-responses (funcall output-nonlinearity dendrite-responses)))
    (let* ((skip-samples (* *sample-rate* skip))
	   (size-samples (* *sample-rate* length))
	   (cropped-samples (- size-samples skip-samples)))
      (crop output-responses :x skip-samples :x-size cropped-samples))))

(defun compute-dendrite-responses
    (filter-list
     &key (length *length*)
     (stimulus-func 'drifting-grating-linear-response)
     (stimulus-args (list :wx *base-wx* :wy 0.0 :wt *base-wt*
			  :stimulus-phase 0.0 :contrast *base-contrast*))
     (dendrite-nonlinearity *dendrite-nonlinearity*))
  (let
      ((dendrite-responses
	(make-image-sequence
	 (loop for filter in filter-list
	       for base-func = (first filter)
	       for freq-func = (second filter)
	       for filter-phase = (third filter)
	       for scale-factor = (fourth filter)
	       collect
	       (with-local-viewables
		   ((linear-response (apply stimulus-func
					    base-func freq-func filter-phase
					    :length length stimulus-args))
		    (dendrite-response (funcall dendrite-nonlinearity linear-response)))
		 (mul scale-factor dendrite-response))))))
    dendrite-responses))

(defun normalize-the-responses (unnormalized-responses
				&key (sigma *sigma*)
				(alpha *alpha*)
				(Rmax *Rmax*)				
				(initial-G *initial-feedback*)
				(initial-R *initial-response*)
				(output-nonlinearity *output-nonlinearity*)
				-> f->)
  (with-result ((normalized-responses ->) unnormalized-responses)
    (with-result ((feedback-signal f->) (frame 0 unnormalized-responses))
      (zero! normalized-responses)
      (zero! feedback-signal)
      (add initial-G feedback-signal :-> feedback-signal)
      (add initial-G normalized-responses :-> normalized-responses)
      (loop with G-data = (obv::data feedback-signal)
	    with X-data-list = (mapcar 'obv::data (obv::image-list unnormalized-responses))
	    with R-data-list = (mapcar 'obv::data (obv::image-list normalized-responses))
	    for time from 0 below (x-dim unnormalized-responses)
	    for G1 = (if (zerop time) initial-G (aref G-data (- time 1)))
	    for R1-sum = (loop for R-data in R-data-list
			       sum (if (zerop time) initial-R (aref R-data (- time 1))))
	    for X1-sum = (if (zerop time) 0.0 (loop for X-data in X-data-list sum (aref X-data (- time 1))))
	    do
	    ;;(format t "t=~a  X=~a  G=~a  R=~a~%" (- time 1) X1-sum G1 R1-sum)
	    (setf (aref G-data time) (+ (* (- 1 alpha) G1) (* alpha R1-sum)))
	    (loop for R-data in R-data-list
		  for X-data in X-data-list
		  for X = (aref X-data time)
		  do
		  (setf (aref R-data time)
			(funcall output-nonlinearity
				 (* (/ X (sqr sigma)) (max 0.0 (- Rmax (aref G-data time))))))
		  ))
      ;;(display feedback-signal)
      ;;(display (frame 0 normalized-responses))
      ;;(print-db (iref feedback-signal (1- (x-dim feedback-signal))))
      (values normalized-responses feedback-signal))))

(defun test-feedback (X &key (size 10)
			(sigma *sigma*)
			(alpha *alpha*)
			(Rmax *Rmax*)				
			(initial-G *initial-feedback*)
			(initial-R *initial-response*))
  (format t "hyperbolic-ratio=~a~%" (hyperbolic-ratio x :Rmax Rmax :sigma sigma :n 1))
  (with-result ((response nil) (list :class 'one-d-image :dimensions size))
    (with-result ((feedback nil) (list :class 'one-d-image :dimensions size))
      (loop for time from 0 below size
	    with R-data = (obv::data response)
	    with G-data = (obv::data feedback)
	    for R1 = (if (zerop time) initial-R (aref R-data (1- time)))
	    for G1 = (if (zerop time) initial-G (aref G-data (1- time)))
	    do
	    (setf (aref G-data time) (+ (* (- 1.0 alpha) G1) (* alpha R1)))
	    (setf (aref R-data time) (* (/ X sigma) (max 0.0 (- Rmax (aref G-data time)))))
	    (format t "t=~a  X=~a  G=~a  R=~a~%" time X (aref G-data time) (aref R-data time))
	    )
      (values response feedback))))

#|
;;; for test-feedback
(multiple-value-setq (response feedback)
  (test-feedback 1.0 :size 5
		 :sigma 1.0 :alpha 0.5 :Rmax 1.0
		 :initial-G 0.0 :initial-R 0.0))
|#

#|
;;; Test compute-dendrite-responses and normalize-the-responses.
;;; Make sure feedback converges.

;;; converges for alpha < (/ (* 2 *sigma*) (+ *sigma* sum-X))
;;; tau =  (/ *sigma* (* *alpha* (+ *sigma* sum-X)))
;;; for very small sum-X, tau -> (/ 1 *alpha*)

;;; For sum of sqr'd linear outputs.
(progn  
  (setq *dendrite-nonlinearity* 'half-square)
  (setq *freq-resp-func* 'frequency-response)
  (setq *output-nonlinearity* 'halfwave-rectify)
  (setq *Rmax* 1.0)
  (setq *sigma* 0.1)
  (setq *alpha* (sqr *sigma*))
  (setq *initial-feedback* 0.0)
  (setq *initial-response* 0.0))

;;; For sum of linear outputs.
(progn  
  (setq *dendrite-nonlinearity* 'halfwave-rectify)
  (setq *freq-resp-func* 'frequency-response2)
  (setq *output-nonlinearity* 'halfwave-rectify)
  (setq *Rmax* 1.0)
  (setq *sigma* 0.1)
  (setq *alpha* (sqr *sigma*))
  (setq *initial-feedback* 0.0)
  (setq *initial-response* 0.0))

;;; compute time constants (in secs)
(/ (/ *sigma* (* *alpha* (+ *sigma* 1.0))) *sample-rate*)
(/ (/ *sigma* (* *alpha* (+ *sigma* 0.1))) *sample-rate*)
(/ (/ *sigma* (* *alpha* (+ *sigma* 0.01))) *sample-rate*)

;;; drifting gratings
(setq dendrite-responses (compute-dendrite-responses
			  *filter-list*
			  :length 2
			  :stimulus-func 'drifting-grating-linear-response
			  :stimulus-args (list :wx *base-wx* :wt (/ *base-wt* 2) :contrast 1.0)))

;;; counterphase gratings
(setq dendrite-responses (compute-dendrite-responses
			  *filter-list*
			  :length 2
			  :stimulus-func 'counterphase-grating-linear-response
			  :stimulus-args (list :wx *base-wx* :wt (/ *base-wt* 2) :contrast 0.5)))

(progn
  (multiple-value-setq (responses feedback)
    (normalize-the-responses dendrite-responses))
  feedback)

(progn
  (setq band 0)
  (display (frame responses band) 'graph :y-range '(0.0 1.0) :y-tick-step 0.2)
  (loop for i from 1 to 3 do
	(overlay-display (frame responses (+ band (* 3 i))))))

;;; compute-time-constant for various inputs

;;; for sigma=0.1, 100-fold change in sum-x gives a 10-fold change in time constant
(let ((sigma 0.1) (alpha 0.1)
      (sum-x 1.0)
      ;;(sum-x 0.01)
      )
  (/ sigma (* alpha (+ sigma sum-x))))

;;; for sigma=0.01, 10-fold change in sum-x gives a 10-fold change in time-constant
(let ((sigma 0.01) (alpha 0.01)
      (sum-x 1.0)
      ;;(sum-x 0.1)
      )
  (/ sigma (* alpha (+ sigma sum-x))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Contrast Response

(defun response-vs-log-contrast
    (filter-list
     &key (length *length*) (skip *skip*)
     (compute-response-fn 'compute-responses)
     (stimulus-func 'drifting-grating-linear-response)
     (stimulus-args (list :wx *base-wx* :wt *base-wt*
			  :stimulus-phase 0.0))
     (fundamental-wt *base-wt*)
     (min 0.01) (max 1.0) (base 10.0) (size 9))
  (make-log-discrete-function
   #'(lambda (contrast)
       (obv::status-message "contrast=~a" contrast)
       (with-local-viewables
	   ((responses (funcall compute-response-fn
				filter-list :length length :skip skip
				:stimulus-func stimulus-func
				:stimulus-args (append (list :contrast contrast) stimulus-args))))
	 (response-amplitude-and-phase (frame 0 responses) (abs fundamental-wt))))
   min max :base base :size size))

#|
(make-log-discrete-function
 '(lambda (x) (/ (sqr x) (+ *sigma* (sqr x))))
 0.01 1.0 :base 10.0)

(make-log-discrete-function
 '(lambda (x) (sqr x))
 0.01 1.0 :base 10.0)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defun normalize-the-responses (unnormalized-responses
				&key (sigma *sigma*)
				(alpha *alpha*)
				(Rmax *Rmax*)				
				(initial-G *initial-feedback*)
				(initial-R *initial-response*)
				(output-nonlinearity *output-nonlinearity*)
				-> f->)
  (with-result ((normalized-responses ->) unnormalized-responses)
    (with-result ((feedback-signal f->) (frame 0 unnormalized-responses))
      (zero! normalized-responses)
      (zero! feedback-signal)
      (add initial-G feedback-signal :-> feedback-signal)
      (add initial-G normalized-responses :-> normalized-responses)
      (loop with G-data = (obv::data feedback-signal)
	    with X-data-list = (mapcar 'obv::data (obv::image-list unnormalized-responses))
	    with R-data-list = (mapcar 'obv::data (obv::image-list normalized-responses))
	    for time from 0 below (x-dim unnormalized-responses)
	    for G1 = (if (zerop time) initial-G (aref G-data (- time 1)))
	    for R1-sum = (loop for R-data in R-data-list
			       sum (if (zerop time) initial-R (aref R-data (- time 1))))
	    for X1-sum = (if (zerop time) 0.0 (loop for X-data in X-data-list sum (aref X-data (- time 1))))
	    do
	    ;;(format t "t=~a  X=~a  G=~a  R=~a~%" (- time 1) X1-sum G1 R1-sum)
	    (setf (aref G-data time) (/ (sqr sigma) (max 0.0 (- Rmax R1-sum))))
	    (loop for R-data in R-data-list
		  for X-data in X-data-list
		  for X = (aref X-data time)
		  for R1 = (aref R-data time)
		  do
		  (setf (aref R-data time)
			(funcall output-nonlinearity
				 (/ (+ X (* alpha R1)) (+ alpha (aref G-data time)))))))
      ;;(display feedback-signal)
      ;;(display (frame 0 normalized-responses))
      ;;(obv::print-db (iref feedback-signal (1- (x-dim feedback-signal))))
      (values normalized-responses feedback-signal))))

(defun test-feedback (X &key (size 10)
			(sigma *sigma*)
			(alpha *alpha*)
			(Rmax *Rmax*)				
			(initial-G *initial-feedback*)
			(initial-R *initial-response*))
  (format t "hyperbolic-ratio=~a~%" (hyperbolic-ratio x :Rmax Rmax :sigma sigma :n 1))
  (with-result ((response nil) (list :class 'one-d-image :dimensions size))
    (with-result ((feedback nil) (list :class 'one-d-image :dimensions size))
      (loop for time from 0 below size
	    with R-data = (obv::data response)
	    with G-data = (obv::data feedback)
	    for R1 = (if (zerop time) initial-R (aref R-data (- time 1)))
	    for G1 = (if (zerop time) initial-G (aref G-data (- time 1)))
	    do
	    (setf (aref G-data time) (/ sigma (max 0.0 (- Rmax  R1))))
	    (setf (aref R-data time) (/ (+ X (* alpha R1))
					(+ alpha (aref G-data time))))
	    ;;(format t "t=~a  X=~a  G=~a  R=~a~%" time X (aref G-data time) (aref R-data time))
	    )
      (values response feedback))))
|#

#|
(setq *alpha* 1.0)

(let* ((x 0.01) (sigma 0.01) (Rmax 1.0)
       (alpha 30.0))
  (setq response (test-feedback x :sigma sigma :Rmax Rmax
				:alpha alpha :initial-G 0.0 :initial-R 0.0
				:size 10000)))


;;; drifting
(display (setq simple-responses-100
	       (compute-responses
		*filter-list* :length 1 :skip 0
		:stimulus-func 'drifting-grating-linear-response
		:stimulus-args (list :wx (sqrt 2) :wy 0.0 :wt 4.0 :contrast 1.0)))
	 'flipbook :y-range '(0.0 0.5))
(display (setq energies-100 (compute-energies simple-responses-100))
	 'flipbook :y-range '(0.0 0.5))
(display (setq simple-responses-10
	       (compute-responses
		*filter-list* :length 1 :skip 0
		:stimulus-func 'drifting-grating-linear-response
		:stimulus-args (list :wx (sqrt 2) :wy 0.0 :wt 4.0 :contrast 0.1)))
	 'flipbook :y-range '(0.0 0.2))
(display (setq energies-10 (compute-energies simple-responses-10))
	 'flipbook :y-range '(0.0 0.2))

;;; counterphase
(display (setq simple-cp-responses-100
	       (compute-responses
		*filter-list* :length 1 :skip 0
		:stimulus-func 'counterphase-grating-linear-response
		:stimulus-args (list :wx (sqrt 2) :wy 0.0 :wt 4.0 :contrast 1.0)))
	 'flipbook :y-range '(0.0 0.5))
(display (setq energies-cp-100 (compute-energies simple-cp-responses-100))
	 'flipbook :y-range '(0.0 0.5))

;;; contrast-response
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
|#
