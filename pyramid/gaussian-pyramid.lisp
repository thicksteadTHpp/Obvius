;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: gaussian-pyramid.lisp
;;;  Author: David Heeger
;;;  Description: gaussian and laplacian pyramids
;;;  Creation Date: revised summer '90
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'obvius)

(export '(make-gaussian-pyramid gaussian-pyramid-p
	  make-laplacian-pyramid laplacian-pyramid-p
	  build access collapse))

(obv-require "pyramid")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; gaussian pyramid class

(defmacro gaussian-pyramid-p (obj)
  `(typep ,obj 'gaussian-pyramid))

(defmacro laplacian-pyramid-p (obj)
  `(typep ,obj 'laplacian-pyramid))

(defmethod print-object ((pyr gaussian-pyramid) stream)
  (format stream "#<~A " (object-class-name pyr))  
  (format stream " ~S>" (name pyr)))

;;; *** Shouldn't we force the step-vector of a passed-in filter to be '(2 2)???

;;; For gaussian-pyramid, low-band is the the top level.  It only uses
;;; forward-low-filter.  All other filters are nil.
(defun make-gaussian-pyramid (image &rest initargs
				    &key level name display-type
				    (filter (make-separable-filter 
					     gauss-5 gauss-5
					     :step-vector '(2 2)
					     :edge-handler nil))
				    ->)
  (declare (ignore name display-type))
  (when -> (setf (getf initargs :name) ->))
  (remf initargs :level)
  (with-result ((result nil)
		`(:class gaussian-pyramid
		  :low-band ,image :original ,image
		  :forward-low-filter ,filter
		  ,@initargs)
		'apply 'make-gaussian-pyramid image initargs)
    (when level (build result level))
    result))

;;; Laplacian pyramid uses forward-low-filter and inverse-low-filter
(defun make-laplacian-pyramid (image &rest initargs &key level name display-type
				     (forward-filter (make-separable-filter 
						      gauss-5 gauss-5
						      :step-vector '(2 2)
						      :edge-handler nil))
				     (inverse-filter forward-filter)
				     ->)
  (declare (ignore name display-type))
  (when -> (setf (getf initargs :name) ->))
  (remf initargs :level)
  (with-result ((result nil)
		`(:class laplacian-pyramid
		  :low-band ,image :original ,image
		  :forward-low-filter ,forward-filter
		  :inverse-low-filter ,inverse-filter
		  ,@initargs)
		'apply 'make-laplacian-pyramid image initargs)
    (when level (build result level))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Build and access methods (must set not current when modify the
;;; pyramid structure).

(defmethod build-level ((pyr gaussian-pyramid) low)
  (let ((new-low (apply-filter (forward-low-filter pyr) low)))
    (values new-low low)))

(defmethod build-level ((pyr laplacian-pyramid) low)
  (let* ((new-low (apply-filter (forward-low-filter pyr) low))
	 (new-band (similar low)))
    (expand-filter (inverse-low-filter pyr) new-low :-> new-band)
    (mul new-band (* (x-step (inverse-low-filter pyr))
		     (y-step (inverse-low-filter pyr)))
	 :-> new-band)
    (sub low new-band :-> new-band)
    (values new-low new-band)))

(defmethod collapse ((pyr gaussian-pyramid))
  (warn "Doesn't make sense to collapse a gaussian pyramid, returning base image")
  (access pyr 0))

(defmethod collapse ((pyr laplacian-pyramid))
  (loop with start-lev = (1- (length (levels pyr)))
	for lev from start-lev downto 0
	for low = (collapsible-low-band pyr)
	then res
	for res = (similar (access pyr lev))
	do
	(expand-filter (inverse-low-filter pyr) low :-> res)
	(mul res (* (x-step (inverse-low-filter pyr))
		    (y-step (inverse-low-filter pyr)))
	     :-> res)
	(destroy low)
	(add (access pyr lev) res :-> res)
	finally (return res)))

#|
;;; Old version, decided it was too hairy with all the keywords.  DH, 3/94.
(defmethod collapse ((pyr laplacian-pyramid)
		     &key (include-low t) (levels t) (bands t) (to-level 0))
  (cond ((eq levels t)
	 (setq levels (loop for i from 0 below (length (levels pyr)) collect i)))
	((numberp levels) (setq levels (list levels)))
	((not (and (listp levels) (every #'integerp levels)))
	 (error ":levels parameter should be a number, a list of numbers, ~
                      or t (indicates ALL levels).")))
  (unless (eq bands t)
    (warn "Bands keyword ignored for laplacian pyramids"))
  (setq bands (list 0))

  (loop with max-lev = (1- (length (levels pyr)))
	with start-lev = (if include-low max-lev (apply #'max levels))
	for lev from start-lev downto to-level
	for low = (if include-low
		      (collapsible-low-band pyr)
		      (similar (access pyr (1+ start-lev))))
	then res
	for res = (similar (access pyr lev))
	do
	(expand-filter (inverse-low-filter pyr) low :-> res)
	(mul res (* (x-step (inverse-low-filter pyr))
		    (y-step (inverse-low-filter pyr)))
	     :-> res)
	(destroy low)
	(when (member lev levels)
	  (add (access pyr lev) res :-> res))
	finally (return res)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Gaussian and laplacian pyramid display.  This should eventually be
;;; rewritten to use pasteups.

;;; Allow vertical or horizontal pyramid display.  Default is horizontal.
;;; *** Should also do square!
(defmethod pasteup-dimensions ((pyr gaussian-pyramid) &key pasteup-format border)
  (let ((x-dim (x-dim (original pyr)))
	(y-dim (y-dim (original pyr))))
    (if (or (null pasteup-format) (eq pasteup-format :horizontal))
	(list y-dim (+ (* 2 x-dim) (* (1- (length (levels pyr))) border)))
	(list (+ (* 2 y-dim) (* (1- (length (levels pyr))) border)) x-dim))))

(defmethod pasteup-format-to-layout ((pyr gaussian-pyramid) format)
  (when (eq format :square)
    (warn "Pasteup format must be horizontal or vertical for pyramids")))

;;; *** Behavior when independent-parameters is non-nil is wrong.  It
;;; should be the same as for grays.  How can we abstract this?
(defmethod compute-picture ((pic pasteup) (pyr gaussian-pyramid))
  (setf (system-dependent-frob pic)
	(make-bltable (screen-of (pane-of pic)) (dimensions pic)
		      :bltable (system-dependent-frob pic)))		      
  (clear (system-dependent-frob pic) :color (background (pane-of pic)))
  (loop with f-sum = (volume (forward-low-filter pyr))
	with format = (if (not (eq (pasteup-format pic) :vertical)) :horizontal :vertical)
	with x-offset = 0
	with y-offset = 0
	for i from 0 
	for im in (levels pyr)
	for factor = (expt f-sum i)
	for ped = (if (independent-parameters pic) (minimum im) (* (pedestal pic) factor))
	for scale = (if (independent-parameters pic) (range im) (* (scale pic) factor))
	do
	(draw-float-array (system-dependent-frob pic) (data im)
			  ped scale (zoom pic) y-offset x-offset)
	(if (eq format :horizontal)
	    (setq x-offset (+ x-offset (border pic) (x-dim im)))
	    (setq y-offset (+ y-offset (border pic) (y-dim im))))))

(defmethod compute-picture ((pic pasteup) (pyr laplacian-pyramid))
  (setf (system-dependent-frob pic)
	(make-bltable (screen-of (pane-of pic)) (dimensions pic)
		      :bltable (system-dependent-frob pic)))
  (clear (system-dependent-frob pic) :color (background (pane-of pic)))
  (loop with f-sum = (volume (forward-low-filter pyr))
	with format = (if (not (eq (pasteup-format pic) :vertical)) :horizontal :vertical)
	with x-offset = 0
	with y-offset = 0
	for i from 0 
	for im in (levels pyr)
	for factor = (expt f-sum i)
	for ped = (if (independent-parameters pic) (minimum im) (/ (pedestal pic) factor))
	for scale = (if (independent-parameters pic) (range im) (* (scale pic) factor))
	do
	(draw-float-array (system-dependent-frob pic) (data im)
			  ped scale (zoom pic) y-offset x-offset)
	(if (eq format :horizontal)
	    (setq x-offset (+ x-offset (border pic) (x-dim im)))
	    (setq y-offset (+ y-offset (border pic) (y-dim im))))))



