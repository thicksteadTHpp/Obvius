;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: flow.lisp
;;;  Author: Simoncelli
;;;  Description: Code for computing optical flow.
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Relavent top-level functions are compute-multi-scale-flow,
;;;; compute-flow, compute-flow-and-covariance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parameters:

;;; 3 frame filters (from deriv-filters.lisp) 9/92
;(defvar *t-kernel* '(0.2331712245941162 0.5336575508117676 0.2331712245941162))
;(defvar *td-kernel* '(-0.4593011736869812 0.0 0.4593011736869812))

;;; 5-tap spatial kernels (from deriv-filters.lisp) 9/92
;(defvar *s-kernel* '(3.342603892E-2 0.24112506 0.4508978 0.24112506 3.342603892E-2))
;(defvar *sd-kernel* '(-9.452048E-2 -0.30649704 0.0 0.30649704 9.452048E-2))

;;; 12/92 filters:
(defvar *t-kernel* '(0.230366 0.539269 0.230366))
(defvar *td-kernel* '(-0.441419 0.00000 0.441419))
(defvar *s-kernel* '(4.504187e-2 0.243908 0.422100 0.243908 4.504187e-2))
(defvar *sd-kernel* '(-0.108144 -0.269869 0.00000 0.269869 0.108144))
(defvar *s2-kernel* '(3.342604e-2 0.241125 0.450898 0.241125 3.342604e-2))
(defvar *s2d-kernel* '(-9.186104e-2 -0.307610 0.00000 0.307610 9.186104e-2))
(defvar *s2dd-kernel* '(0.202183 9.181186e-2 -0.587989 9.181186e-2 0.202183))

(defvar *deriv-edges* :repeat)  ; :dont-compute if covariance prop. with old warper

;;; Multi-scale parameters
(defvar *interp-kernel* '(0.707107 1.4142135 0.707107)) ;sum of alternate taps is r2
(defvar *interp-edges* :reflect1)

;;; **** SHould design this to 1) prevent aliasing, 2) attenuate
;;; frequencies near pi/2 that are handled poorly by the deriv filters...
(defvar *pyr-kernel* gauss-5)
(defvar *pyr-edges* :reflect1)

(defvar *process-sigma* 0.1)

;; Display only:
(defvar *flow-scale* 3.5)

(dolist (p '(*process-sigma* *t-kernel* *td-kernel* *s-kernel* *sd-kernel*
	     *s2-kernel* *s2d-kernel* *s2dd-kernel*
	     *deriv-edges* *interp-kernel* *interp-edges*
	     *pyr-kernel* *pyr-edges*))
  (pushnew p *flow-parameters*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copied from (and supersedes) integer-warping.lisp:
;;; Does NOT propagate covariances.
(defun compute-multi-scale-flow (pyr &key (interp-kernel *interp-kernel*)
				     (display-frame nil)
				     (flow-scale *flow-scale*))
  (let* ((*auto-destroy-orphans* t))	;make sure sub-viewables are destroyed
    (declare (special *auto-destroy-orphans*))
    (loop for lev = (- (height pyr) 1) then (- lev 1)
	  for zoomed-flow = nil
	  then (let ((zflow (gauss-in flow :kernel interp-kernel
				      :edge-handler *interp-edges*
				      :-> (make-viewable-sequence
					   (loop for i from 0 below (length. flow) collect
						 (make-image-pair (dimensions (access pyr lev))))))))
		 (destroy flow)
		 (if *integer-warp* (round. zflow :-> zflow) zflow))
	  for flow = (prog1
			 (compute-flow (access pyr lev) :prior-flow zoomed-flow)
		       (when zoomed-flow (destroy zoomed-flow)))
	  until (= lev 0)
	  do (when display-frame
	       (display (frame display-frame flow :->
			       (format nil "frame~A-lev~A" display-frame lev))
			'vector-field
			:skip (max 1 (floor (x-dim flow) 12))
			:scale (/ flow-scale (expt 2 lev))))
	  finally (return flow))))

;;; Similar to version in separable-flow.lisp, but uses shear-warping.
;;; Prior flow frames must correspond to output flow frames.
;;; OTHER KEYS: :blur-kernel :prior-offset :sigma-1 :sigma-2
(defun compute-flow
    (seq &rest keys &key prior-flow prior-vy prior-vxy prior-vx
	 (t-kernel *t-kernel*) (td-kernel *td-kernel*)
	 (s-kernel *s-kernel*) (sd-kernel *sd-kernel*)
	 (start-frame (floor (length t-kernel) 2))
	 (end-frame (- (sequence-length seq) (ceiling (length t-kernel) 2) -1))
	 (blur-kernel *blur-kernel*) prior-offset sigma-1 sigma-2 ->)
  (when (and prior-vy prior-vxy prior-vx)
    (unless (and (image-sequence-p prior-vy) (image-sequence-p prior-vx) 
		 (image-sequence-p prior-vxy)
		 (= (length. prior-vxy) (length. prior-vy)
		    (length. prior-vx) (length. prior-flow)))
      (error "Bad prior covariances")))
  (with-result ((result ->) (list  :class 'viewable-sequence
				   :length (- end-frame start-frame)
				   :sub-viewable-spec
				   (list :class 'image-pair :dimensions (dimensions seq)
					 :display-type 'vector-field)))
    (with-local-viewables ((dxdy (similar (frame seq 0)))
			   (dxdt (similar (frame seq 0)))
			   (dydt (similar (frame seq 0)))
			   (Vx (similar dxdy)) (Vy (similar dxdy)) (Vxy (similar dxdy))
			   (arglist (sub-plist keys :blur-kernel
					       :sigma-1 :sigma-2)))
      (loop-applying-separable-filters (seq index :warper prior-flow
					    :start start-frame :end end-frame)
	  ((derivs1 (list t-kernel td-kernel) (list s-kernel sd-kernel)))
	for res-pr in (viewable-list result)
	for i-prior from 0
	for arglist2 = (if (and prior-vy prior-vx prior-vxy)
			   (append (list :prior-vy (frame i-prior prior-vy)
					 :prior-vxy (frame i-prior prior-vxy)
					 :prior-vx (frame i-prior prior-vx))
				   arglist)
			   (append (sub-plist keys  :prior-offset) arglist))
	do
	(destructuring-bind (dt dy dx) (image-list derivs1)
	  (status-message "Computing energies & flow, frame ~A" index)
	  (mul dx dy :-> dxdy)
	  (mul dx dt :-> dxdt)
	  (mul dy dt :-> dydt)
	  (square dx :-> dx)
	  (square dy :-> dy)
	  (apply 'compute-flow-dist-from-separable-energies
		 dy dxdy dx dydt dxdt
		 :-y> (y-component res-pr) :-x> (x-component res-pr)
		 :-vy> Vy  :-vxy> Vxy :-vx> Vx arglist2)
	  (when prior-flow
	    ;;(compose-warps (frame i-prior prior-flow) res-pr :-> res-pr)
	    (add (frame i-prior prior-flow) res-pr :-> res-pr)
	    ))))
    result))

;;; Return covariances along with means
(defun compute-flow-and-covariance
    (seq &rest keys &key prior-flow prior-vy prior-vxy prior-vx
	 (t-kernel *t-kernel*) (td-kernel *td-kernel*)
	 (s-kernel *s-kernel*) (sd-kernel *sd-kernel*)
	 (start-frame (floor (length t-kernel) 2))
	 (end-frame (- (sequence-length seq) (ceiling (length t-kernel) 2) -1))
	 (blur-kernel *blur-kernel*) prior-offset sigma-1 sigma-2 -> -vx> -vxy> -vy>)
  (when (and prior-vy prior-vxy prior-vx)
    (unless (and (image-sequence-p prior-vy) (image-sequence-p prior-vx) 
		 (image-sequence-p prior-vxy)
		 (= (length. prior-vxy) (length. prior-vy)
		    (length. prior-vx) (length. prior-flow)))
      (error "Bad prior covariances")))
  (with-result ((result ->) (list  :class 'viewable-sequence
				   :length (- end-frame start-frame)
				   :sub-viewable-spec
				   (list :class 'image-pair :dimensions (dimensions seq)
					 :display-type 'vector-field)))
    (let ((seq-spec (list :class 'image-sequence :length (- end-frame start-frame)
			  :dimensions (dimensions seq))))
    (with-result ((vx-seq -vx>) seq-spec)
    (with-result ((vxy-seq -vxy>) seq-spec)
    (with-result ((vy-seq -vy>) seq-spec)
      (with-local-viewables ((dxdy (similar (frame seq 0)))
			     (dxdt (similar (frame seq 0)))
			     (dydt (similar (frame seq 0)))
			     (arglist (sub-plist keys :blur-kernel :sigma-1 :sigma-2)))
	(loop-applying-separable-filters (seq index :warper prior-flow
					      :start start-frame :end end-frame)
	    ((derivs1 (list t-kernel td-kernel) (list s-kernel sd-kernel)))
	  for res-pr in (viewable-list result)
	  for Vx in (image-list vx-seq)
	  for Vxy in (image-list vxy-seq)
	  for Vy in (image-list vy-seq)
	  for i-prior from 0
	  for arglist2 = (if (and prior-vy prior-vx prior-vxy)
			     (append (list :prior-vy (frame i-prior prior-vy)
					   :prior-vxy (frame i-prior prior-vxy)
					   :prior-vx (frame i-prior prior-vx))
				     arglist)
			     (append (sub-plist keys :prior-offset) arglist))
	  do
	  (destructuring-bind (dt dy dx) (image-list derivs1)
	    (status-message "Computing energies & flow, frame ~A" index)
	    (mul dx dy :-> dxdy)
	    (mul dx dt :-> dxdt)
	    (mul dy dt :-> dydt)
	    (square dx :-> dx)
	    (square dy :-> dy) 
	    (apply 'compute-flow-dist-from-separable-energies
		   dy dxdy dx dydt dxdt
		   :-y> (y-component res-pr) :-x> (x-component res-pr)
		   :-vy> Vy  :-vxy> Vxy :-vx> Vx arglist2)
	    (when prior-flow
	      ;;(compose-warps (frame i-prior prior-flow) res-pr :-> res-pr)
	      (add (frame i-prior prior-flow) res-pr :-> res-pr)
	      ))))
      (values result vy-seq vxy-seq vx-seq)))))))

;;; Use mixed first and second order derivs
;;; NOTE: does NOT propagate covariance information
(defun compute-multi-scale-flow1+2 (pyr &key (interp-kernel *interp-kernel*)
				     (display-frame nil)
				     (flow-scale *flow-scale*))
  (let* ((*auto-destroy-orphans* t))	;make sure sub-viewables are destroyed
    (declare (special *auto-destroy-orphans*))
    (loop for lev = (- (height pyr) 1) then (- lev 1)
	  for zoomed-flow = nil
	  then (let ((zflow (gauss-in flow :kernel interp-kernel
				      :edge-handler *interp-edges*
				      :-> (make-viewable-sequence
					   (loop for i from 0 below (length. flow) collect
						 (make-image-pair (dimensions (access pyr lev))))))))
		 (destroy flow)
		 (if *integer-warp* (round. zflow :-> zflow) zflow))
	  for flow = (prog1
			 (compute-flow1+2 (access pyr lev) :prior-flow zoomed-flow)
		       (when zoomed-flow (destroy zoomed-flow)))
	  until (= lev 0)
	  do (when display-frame
	       (display (frame display-frame flow :->
			       (format nil "frame~A-lev~A" display-frame lev))
			'vector-field
			:skip (max 1 (floor (x-dim flow) 12))
			:scale (/ flow-scale (expt 2 lev))))
	  finally (return flow))))

;;; Version using both 1st and 2nd derivs: Cheapened noise model here,
;;; assuming lots of independence between spatially adjacent points,
;;; since the full version is v. slow.
(defun compute-flow1+2
    (seq &rest keys &key prior-flow prior-vy prior-vxy prior-vx
	 (t-kernel *t-kernel*) (td-kernel *td-kernel*)
	 (s-kernel *s-kernel*) (sd-kernel *sd-kernel*)
	 (s2-kernel *s2-kernel*) (s2d-kernel *s2d-kernel*) (s2dd-kernel *s2dd-kernel*)
	 (start-frame (floor (length t-kernel) 2))
	 (end-frame (- (sequence-length seq) (ceiling (length t-kernel) 2) -1))
	 (blur-kernel *blur-kernel*) prior-offset sigma-1 sigma-2 ->)
  (when (and prior-vy prior-vxy prior-vx)
    (unless (and (image-sequence-p prior-vy) (image-sequence-p prior-vx) 
		 (image-sequence-p prior-vxy)
		 (= (length. prior-vxy) (length. prior-vy)
		    (length. prior-vx) (length. prior-flow)))
      (error "Bad prior covariances")))
  (with-result ((result ->) (list  :class 'viewable-sequence
				   :length (- end-frame start-frame)
				   :sub-viewable-spec
				   (list :class 'image-pair :dimensions (dimensions seq)
					 :display-type 'vector-field)))
    (with-local-viewables ((Mxx (similar (frame seq 0))) ;energies
			   (Mxy (similar Mxx)) (Myy (similar Mxx))
			   (byt (similar Mxx)) (bxt (similar Mxx))
			   (Vx (similar Mxx)) (Vy (similar Mxx)) (Vxy (similar Mxx))
			   temp
			   (arglist (sub-plist keys :blur-kernel
					       :prior-offset :sigma-1 :sigma-2)))
      (loop-applying-separable-filters (seq index :warper prior-flow
					    :start start-frame :end end-frame)
	  ((derivs1 (list t-kernel td-kernel) (list s-kernel sd-kernel))
	   (derivs2 (list t-kernel td-kernel) (list s2-kernel s2d-kernel s2dd-kernel)))
	for res-pr in (viewable-list result)
	for i-prior from 0
	for arglist2 = (if (and prior-vy prior-vx prior-vxy)
			   (append (list :prior-vy (frame i-prior prior-vy)
					 :prior-vxy (frame i-prior prior-vxy)
					 :prior-vx (frame i-prior prior-vx))
				   arglist)
			   arglist)
	do
	(destructuring-bind (dyt dxt dyy dxy dxx) (image-list derivs2)
	  (destructuring-bind (dt dy dx) (image-list derivs1)
	    (status-message "Computing energies & flow, frame ~A" index)
	    (mul dxx dxt :-> bxt) (mul dxy dxt :-> byt)
	    (setq  temp Mxx)
	    (add bxt (mul dxy dyt :-> temp) :-> bxt)
	    (add bxt (mul dx dt :-> temp) :-> bxt)
	    (add byt (mul dyy dyt :-> temp) :-> byt)
	    (add byt (mul dy dt :-> temp) :-> byt)
	    (mul dxy (add dxx dyy :-> temp) :-> Mxy)
	    (add Mxy (mul dx dy :-> temp) :-> Mxy)
	    (square dxy :-> dxy) (square dyy :-> dyy) (square dxx :-> dxx)
	    (square dy :-> dy) (square dx :-> dx)
	    (add dy (add dyy dxy :-> Myy) :-> Myy)
	    (add dx (add dxx dxy :-> Mxx) :-> Mxx)
	    (apply 'compute-flow-dist-from-separable-energies
		   Myy Mxy Mxx byt bxt
		   :-y> (y-component res-pr) :-x> (x-component res-pr)
		   :-vy> Vy  :-vxy> Vxy :-vx> Vx arglist2)
	    (when prior-flow
	      ;;(compose-warps (frame i-prior prior-flow) res-pr :-> res-pr)
	      (add (frame i-prior prior-flow) res-pr :-> res-pr)
	      ))))
      result)))

;;; Same as compute-flow1+2, but it returns the variance information.
(defun compute-flow-and-covariance1+2
    (seq &rest keys &key prior-flow prior-vy prior-vxy prior-vx
	 (t-kernel *t-kernel*) (td-kernel *td-kernel*)
	 (s-kernel *s-kernel*) (sd-kernel *sd-kernel*)
	 (s2-kernel *s2-kernel*) (s2d-kernel *s2d-kernel*) (s2dd-kernel *s2dd-kernel*)
	 (start-frame (floor (length t-kernel) 2))
	 (end-frame (- (sequence-length seq) (ceiling (length t-kernel) 2) -1))
	 (blur-kernel *blur-kernel*) prior-offset sigma-1 sigma-2 -> -vx> -vxy> -vy>)
  (when (and prior-vy prior-vxy prior-vx)
    (unless (and (image-sequence-p prior-vy) (image-sequence-p prior-vx) 
		 (image-sequence-p prior-vxy)
		 (= (length. prior-vxy) (length. prior-vy)
		    (length. prior-vx) (length. prior-flow)))
      (error "Bad prior covariances")))
  (with-result ((result ->) (list  :class 'viewable-sequence
				   :length (- end-frame start-frame)
				   :sub-viewable-spec
				   (list :class 'image-pair :dimensions (dimensions seq)
					 :display-type 'vector-field)))
    (let ((seq-spec (list :class 'image-sequence :length (- end-frame start-frame)
			  :dimensions (dimensions seq))))
    (with-result ((vx-seq -vx>) seq-spec)
    (with-result ((vxy-seq -vxy>) seq-spec)
    (with-result ((vy-seq -vy>) seq-spec)
      (with-local-viewables ((Mxx (similar (frame seq 0)))
			     (Mxy (similar Mxx)) (Myy (similar Mxx))
			     (byt (similar Mxx)) (bxt (similar Mxx))
			     temp
			     (arglist (sub-plist keys :blur-kernel
						 :prior-offset :sigma-1 :sigma-2)))
	(loop-applying-separable-filters (seq index :warper prior-flow
					      :start start-frame :end end-frame)
	    ((derivs1 (list t-kernel td-kernel) (list s-kernel sd-kernel))
	     (derivs2 (list t-kernel td-kernel) (list s2-kernel s2d-kernel s2dd-kernel)))
	  for res-pr in (viewable-list result)
	  for Vx in (image-list vx-seq)
	  for Vxy in (image-list vxy-seq)
	  for Vy in (image-list vy-seq)
	  for i-prior from 0
	  for arglist2 = (if (and prior-vy prior-vx prior-vxy)
			     (append (list :prior-vy (frame i-prior prior-vy)
					   :prior-vxy (frame i-prior prior-vxy)
					   :prior-vx (frame i-prior prior-vx))
				     arglist)
			     arglist)
	  do
	  (destructuring-bind (dyt dxt dyy dxy dxx) (image-list derivs2)
	    (destructuring-bind (dt dy dx) (image-list derivs1)
	      (status-message "Computing energies & flow, frame ~A" index)
	      (mul dxx dxt :-> bxt) (mul dxy dxt :-> byt)
	      (setq  temp Mxx)
	      (add bxt (mul dxy dyt :-> temp) :-> bxt)
	      (add bxt (mul dx dt :-> temp) :-> bxt)
	      (add byt (mul dyy dyt :-> temp) :-> byt)
	      (add byt (mul dy dt :-> temp) :-> byt)
	      (mul dxy (add dxx dyy :-> temp) :-> Mxy)
	      (add Mxy (mul dx dy :-> temp) :-> Mxy)
	      (square dxy :-> dxy) (square dyy :-> dyy) (square dxx :-> dxx)
	      (square dy :-> dy) (square dx :-> dx)
	      (add dy (add dyy dxy :-> Myy) :-> Myy)
	      (add dx (add dxx dxy :-> Mxx) :-> Mxx)
	      (apply 'compute-flow-dist-from-separable-energies
		     Myy Mxy Mxx byt bxt
		     :-y> (y-component res-pr) :-x> (x-component res-pr)
		     :-vy> Vy  :-vxy> Vxy :-vx> Vx arglist2)
	      (when prior-flow
		;;(compose-warps (frame i-prior prior-flow) res-pr :-> res-pr)
		(add (frame i-prior prior-flow) res-pr :-> res-pr)
		))))
	(values result vy-seq vxy-seq vx-seq))))))))

;;;; Multi-scale, with Kalman-like covariance propagation
;;; NOTE: Seems to have a bug:
(defun compute-multi-scale-cov-flow (pyr &key (interp-kernel *interp-kernel*)
					 (initial-deriv-edges :dont-compute)
					 (process-sigma *process-sigma*)
					 (dsp-frame nil)
					 (flow-scale *flow-scale*))
  (loop with var-kernel = (mapcar #'(lambda (x) (* x (sqrt 2))) interp-kernel)
	with flow with vx with vy with vxy
	for lev from (- (height pyr) 1) downto 0
	for deriv-edges = initial-deriv-edges then *deriv-edges*
	for zoomed-flow = nil
	then (prog1 (gauss-in flow :kernel interp-kernel :edge-handler *interp-edges*
			      :-> (make-viewable-sequence 
				   (loop for i from 0 below (length. flow) collect
					 (make-image-pair (dimensions (access pyr lev))))))
	       (destroy flow))
	for zoomed-vx = nil
	then (prog1 (gauss-in vx :kernel var-kernel :edge-handler *interp-edges*
			      :-> (make-image-sequence (dimensions (access pyr lev))
						       :length (length. vx)))
	       (destroy vx))
	for zoomed-vy = nil
	then (prog1 (gauss-in vy :kernel var-kernel :edge-handler *interp-edges*
			      :-> (make-image-sequence (dimensions (access pyr lev))
						       :length (length. vy)))
	       (destroy vy))
	for zoomed-vxy = nil
	then (prog1 (gauss-in vxy :kernel var-kernel :edge-handler *interp-edges*
			      :-> (make-image-sequence (dimensions (access pyr lev))
						       :length (length. vxy)))
	       (destroy vxy))
	do
	(when zoomed-vx (add zoomed-vx process-sigma :-> zoomed-vx))
	(when zoomed-vy (add zoomed-vy process-sigma :-> zoomed-vx))
	(multiple-value-setq (flow vy vxy vx)
	  (let ((*deriv-edges* deriv-edges))
	    (declare (special *deriv-edges*))
	    (compute-flow-and-covariance (access pyr lev)
					 :sigma-2 *sigma-2* :sigma-1 *sigma-1*
					 :prior-offset *prior-offset*
					 :prior-flow zoomed-flow :prior-vx zoomed-vx
					 :prior-vy zoomed-vy :prior-vxy zoomed-vxy)))
	(when zoomed-flow (destroy zoomed-flow))
	(when zoomed-vx (destroy zoomed-vx))
	(when zoomed-vy (destroy zoomed-vy))
	(when zoomed-vxy (destroy zoomed-vxy))
	(when (and dsp-frame (> lev 0))
	  (display (frame dsp-frame vy :-> (format nil "vy-lev~A" lev)))
	  (display (frame dsp-frame vxy :-> (format nil "vxy-lev~A" lev)))
	  (display (frame dsp-frame vx :-> (format nil "vx-lev~A" lev)))
	  (display (frame dsp-frame flow :-> (format nil "flow-lev~A" lev))
		   'vector-field
		   :skip (max 1 (floor (x-dim flow) 12))
		   :scale (/ flow-scale (expt 2 lev))))
	finally (return (values flow vy vxy vx))))

;;;; Multi-scale, with Kalman-like covariance propagation
;;; NOTE: Seems to have a bug:
(defun compute-multi-scale-cov-flow (pyr &key (interp-kernel *interp-kernel*)
					 (initial-deriv-edges :dont-compute)
					 (process-sigma *process-sigma*)
					 (dsp-frame nil)
					 (flow-scale *flow-scale*))
  (loop with var-kernel = (mapcar #'(lambda (x) (* x (sqrt 2))) interp-kernel)
	with flow with vx with vy with vxy
	for lev from (- (height pyr) 1) downto 0
	for deriv-edges = initial-deriv-edges then *deriv-edges*
	for zoomed-flow = nil
	then (prog1 (gauss-in flow :kernel interp-kernel :edge-handler *interp-edges*
			      :-> (make-viewable-sequence 
				   (loop for i from 0 below (length. flow) collect
					 (make-image-pair (dimensions (access pyr lev))))))
	       (destroy flow))
	for zoomed-vx = nil
	then (prog1 (gauss-in vx :kernel var-kernel :edge-handler *interp-edges*
			      :-> (make-image-sequence (dimensions (access pyr lev))
						       :length (length. vx)))
	       (destroy vx))
	for zoomed-vy = nil
	then (prog1 (gauss-in vy :kernel var-kernel :edge-handler *interp-edges*
			      :-> (make-image-sequence (dimensions (access pyr lev))
						       :length (length. vy)))
	       (destroy vy))
	for zoomed-vxy = nil
	then (prog1 (gauss-in vxy :kernel var-kernel :edge-handler *interp-edges*
			      :-> (make-image-sequence (dimensions (access pyr lev))
						       :length (length. vxy)))
	       (destroy vxy))
	do
	(when zoomed-vx (add zoomed-vx process-sigma :-> zoomed-vx))
	(when zoomed-vy (add zoomed-vy process-sigma :-> zoomed-vx))
	(multiple-value-setq (flow vy vxy vx)
	  (let ((*deriv-edges* deriv-edges))
	    (declare (special *deriv-edges*))
	    (compute-flow-and-covariance1+2 (access pyr lev)
					    :sigma-2 *sigma-2* :sigma-1 *sigma-1*
					    :prior-offset *prior-offset*
					    :prior-flow zoomed-flow :prior-vx zoomed-vx
					    :prior-vy zoomed-vy :prior-vxy zoomed-vxy)))
	(when zoomed-flow (destroy zoomed-flow))
	(when zoomed-vx (destroy zoomed-vx))
	(when zoomed-vy (destroy zoomed-vy))
	(when zoomed-vxy (destroy zoomed-vxy))
	(when (and dsp-frame (> lev 0))
	  (display (frame dsp-frame vy :-> (format nil "vy-lev~A" lev)))
	  (display (frame dsp-frame vxy :-> (format nil "vxy-lev~A" lev)))
	  (display (frame dsp-frame vx :-> (format nil "vx-lev~A" lev)))
	  (display (frame dsp-frame flow :-> (format nil "flow-lev~A" lev))
		   'vector-field
		   :skip (max 1 (floor (x-dim flow) 12))
		   :scale (/ flow-scale (expt 2 lev))))
	finally (return (values flow vy vxy vx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|  ;;; TEST single-scale flow computation:
(compile-load "/u/eero/lisp/motion2/flow-utilities")
(compile-load "/u/eero/lisp/motion2/loop-derivs")

;;; Moving fractal pattern:
(setq *seq*
      (with-local-viewables ((im (make-fractal '(32 32) :fractal-dimension 3.0))) ;1/r
	(make-image-sequence
	 (loop for i from 0 below 5 collect (circular-shift im :x-shift i)))))
(setq actual-flow
      (let ((pr (make-image-pair (dimensions *seq*))))
	(fill! (x-component pr) 1.0)
	pr))

;;; Moving sinusoidal plaid
(setq *seq*
      (make-image-sequence
       (loop for i from 0 below 5
	     for s1 = (make-sin-grating '(32 32) :period 6
					:orientation (* 54 (/ pi 180))
					:phase (* -1.63 i (/ 2-pi 6)))
	     for s2 = (make-sin-grating '(32 32) :period 6
					:orientation (* -27 (/ pi 180))
					:phase (* -1.02 i (/ 2-pi 6)))
	     collect (add s1 s2 ) into res
	     do (destroy s1) (destroy s2)
	     finally (return res))))

;;; First deriv flow
(setq flow1
      (make-viewable-sequence
       (with-local-viewables ((dxdy (similar (frame *seq* 0)))
			      (dxdt (similar (frame *seq* 0)))
			      (dydt (similar (frame *seq* 0))))
	 (loop-applying-separable-filters (*seq* my-i)
	     ((derivs1 (list *t-kernel* *td-kernel*) (list *s-kernel* *sd-kernel*)))
	   collect
	   (destructuring-bind (dt dy dx) (image-list derivs1)
	     (mul dx dy :-> dxdy)
	     (mul dx dt :-> dxdt)
	     (mul dy dt :-> dydt)
	     (square dx :-> dx)
	     (square dy :-> dy)
	     ;; compute flow
	     (multiple-value-bind (vy vx)
		 (compute-flow-dist-from-separable-energies dy dxdy dx dydt dxdt)
	       (make-image-pair (list vy vx))))))))
(v-error-summary (frame 0 flow1) actual-flow)
;;; using function:
(setq flow1 (compute-flow *seq*))

;;; Compare to old flow code:
(setq old-flow (compute-flow *seq*))
(loop for i from 1 below 4
      do (pdb (mean-square-error (frame old-flow i) (frame flow1 (- i 1)))))

;;; 2nd deriv flow:
;;; Get filters gauss dgauss d2gauss from deriv-filters.lisp:
(setq flow2
      (make-viewable-sequence
       (with-local-viewables ((Mxy (similar (frame *seq* 0)))
			      (byt (similar (frame *seq* 0)))
			      (bxt (similar (frame *seq* 0)))
			      temp)
	 (loop-applying-separable-filters (*seq* my-i)
	     ((derivs2 (list gauss dgauss) (list gauss dgauss d2gauss)))
	   collect
	   (destructuring-bind (dyt dxt dyy dxy dxx) (image-list derivs2)
	     (mul dxx dxt :-> bxt)
	     (mul dxy dxt :-> byt)
	     (setq  temp Mxy)
	     (add bxt (mul dxy dyt :-> temp) :-> bxt)
	     (add byt (mul dyy dyt :-> temp) :-> byt)
	     (mul dxy (add dxx dyy :-> Mxy) :-> Mxy)
	     (square dxy :-> dxy)
	     (add (square dyy :-> dyy) dxy :-> dyy)
	     (add (square dxx :-> dxx) dxy :-> dxx)
	     (multiple-value-bind (vy vx)
		 (compute-flow-dist-from-separable-energies dyy Mxy dxx byt bxt)
	       (make-image-pair (list vy vx))))))))
(v-error-summary (frame 0 flow2) actual-flow)

;;; 1st hilbert xform: horrible!
(setq flow1+h
      (make-viewable-sequence
       (with-local-viewables ((Mxx (similar (frame *seq* 0)))
			      (Mxy (similar (frame *seq* 0)))
			      (Myy (similar (frame *seq* 0)))
			      (byt (similar (frame *seq* 0)))
			      (bxt (similar (frame *seq* 0)))
			      temp)
	 (loop-applying-separable-filters (*seq* my-i)
	     ((derivs1 (list gauss dgauss) (list gauss dgauss))
	      (hilbs1 (list gauss hdgauss) (list gauss hdgauss) ))
	   collect
	   (destructuring-bind (dt dy dx) (image-list derivs1)
	     (destructuring-bind (ht hy hx) (image-list hilbs1)
	       (setq temp Mxx)
	       (mul dx dt :-> bxt)
	       (add bxt (mul hx ht :-> temp) :-> bxt)
	       (mul dy dt :-> byt)
	       (add byt (mul hy ht :-> temp) :-> byt)
	       (mul dx dy :-> Mxy)
	       (add Mxy (mul hx hy :-> temp) :-> Mxy)
	       (add (square dx :-> dx) (square hx :-> hx) :-> Mxx)
	       (add (square dy :-> dy) (square hy :-> hy) :-> Myy)
	       (multiple-value-bind (vy vx)
		   (compute-flow-dist-from-separable-energies Myy Mxy Mxx byt bxt
							      :blur-kernel '(1.0))
		 (make-image-pair (list vy vx)))))))))
(v-error-summary (frame 0 flow1+h) actual-flow)

;;; 1st and 2nd combined:
(setq flow1+2
      (make-viewable-sequence
       (with-local-viewables ((Mxx (similar (frame *seq* 0)))
			      (Mxy (similar (frame *seq* 0)))
			      (Myy (similar (frame *seq* 0)))
			      (byt (similar (frame *seq* 0)))
			      (bxt (similar (frame *seq* 0)))
			      temp)
	 (loop-applying-separable-filters (*seq* my-i)
	     ((derivs1 (list *t-kernel* *td-kernel*) (list *s-kernel* *sd-kernel*))
	      (derivs2 (list *t-kernel* *td-kernel*) (list *s2-kernel* *s2d-kernel* *s2dd-kernel*)))
	   collect
	   (destructuring-bind (dyt dxt dyy dxy dxx) (image-list derivs2)
	     (destructuring-bind (dt dy dx) (image-list derivs1)
	       (mul dxx dxt :-> bxt)
	       (mul dxy dxt :-> byt)
	       (setq  temp Mxx)
	       (add bxt (mul dxy dyt :-> temp) :-> bxt)
	       (add bxt (mul dx dt :-> temp) :-> bxt)
	       (add byt (mul dyy dyt :-> temp) :-> byt)
	       (add byt (mul dy dt :-> temp) :-> byt)

	       (mul dxy (add dxx dyy :-> temp) :-> Mxy)
	       (add Mxy (mul dx dy :-> temp) :-> Mxy)

	       (square dxy :-> dxy)
	       (square dyy :-> dyy)
	       (square dxx :-> dxx)
	       (square dy :-> dy)
	       (square dx :-> dx)
	       (add dy (add dyy dxy :-> Myy) :-> Myy)
	       (add dx (add dxx dxy :-> Mxx) :-> Mxx)
	       (multiple-value-bind (vy vx)
		   (compute-flow-dist-from-separable-energies Myy Mxy Mxx byt bxt)
		 (make-image-pair (list vy vx)))))))))
(v-error-summary (frame 1 flow1+2) actual-flow)

;;; This should be the same:
(compute-flow *seq* )

(compute-flow *seq*
	      :prior-flow (make-viewable-sequence (list-of-length 3 actual-flow)))

;;; Analytic angular flow
(setq aflow
      (make-viewable-sequence
       (with-local-viewables ((Mxx (similar (frame *seq* 0)))
			      (Mxy (similar Mxx))
			      (Myy (similar Mxx))
			      (bx (similar Mxx))
			      (by (similar Mxx))
			      (ctheta (similar Mxx))
			      (stheta (similar Mxx))
			      (temp-x (similar Mxx))
			      (temp-xy (similar Mxx))
			      (temp-y (similar Mxx))
			      (norm (similar Mxx))
			      (ds (similar Mxx))
			      (filt (make-separable-filter *blur-kernel* *blur-kernel*
							   :edge-handler *blur-edges*)))
	 (loop-applying-separable-filters (*seq* my-i)
	     ((derivs1 (list *t-kernel* *td-kernel*) (list *s-kernel* *sd-kernel*)))
	   collect
	   (destructuring-bind (dt dy dx) (image-list derivs1)
	     ;; Compute blurred energies
	     (apply-filter filt (square dx :-> temp-x) :-> Mxx)
	     (apply-filter filt (mul dx dy :-> temp-xy) :-> Mxy)
	     (apply-filter filt (square dy :-> temp-y) :-> Myy)
	     (apply-filter filt (mul dx dt :-> temp-x) :-> bx)
	     (apply-filter filt (mul dy dt :-> temp-y) :-> by)
	     (incf. Mxx *prior-offset*)
	     (incf. Myy *prior-offset*)
	     
	     ;; compute theta (orientation of velocity vector)
	     (sub (square Mxy  :-> temp-xy) (mul Mxx Myy :-> temp-x) :-> norm) ;-det(M)

	     (div (sub (mul Mxx by :-> temp-x) (mul Mxy bx :-> temp-y) :-> stheta) norm :-> stheta)
	     (div (sub (mul Myy bx :-> temp-x) (mul Mxy by :-> temp-y) :-> ctheta) norm :-> ctheta)
	     (sqrt. (add (sqr. ctheta :-> temp-x) (sqr. stheta :-> temp-y) :-> norm) :-> norm)
	     (div ctheta norm :-> ctheta)
	     (div stheta norm :-> stheta)

	      ;; compute derivative in theta direction, ds
	      (add (mul dx ctheta :-> temp-x) (mul dy stheta :-> temp-y) :-> ds)

	      ;; compute optimal angular velocity
	      (sub (square ds :-> temp-x) (square dt :-> temp-y) :-> temp-x)
	      (mul (mul ds dt :-> temp-xy) -2 :-> temp-xy)

	      (apply-filter filt temp-xy :-> bx) ;blur(ds.dt)
	      (apply-filter filt temp-x :-> Mxx) ;blur(ds^2 - dt^2)

	      (div bx
		   (add Mxx
			(sqrt. (add (sqr. bx :-> temp-x) (sqr. Mxx :-> temp-y) :-> temp-y) :-> temp-y)
			:-> temp-y)
		   :-> temp-x)
	      (make-image-pair (list (mul temp-x stheta) (mul temp-x ctheta))))))))
(v-error-summary (frame 0 aflow) actual-flow)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
