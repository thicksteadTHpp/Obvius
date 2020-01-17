;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: pasteup.lisp
;;;  Author: Heeger/Simoncelli
;;;  Description: Pasteup (side-by-side display) of grayscale images
;;;  Creation Date: ?
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

;;; Pasteup provides a static display type of a compound image viewable.  It 
;;; is basically a gray which is computed by pasting gray pictures of the
;;; subimages into a base picture.

;;; *** THis should be extended to correctly do the job for pyramids
;;; as well (along with any other (future) compound image viewable, as
;;; with flipbooks!  It should call a method which determines the
;;; position of each sub-image (handed back as a tree) and then it
;;; should just call mapcar-tree to do the blt-ing!  Furthermore, we
;;; need to think about how to do auto-scaling: should we compute min
;;; and max over all subimages and use this, or just scale according
;;; to the range of a single subimage (as it is now)?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod settable-parameters ((class-name (eql 'pasteup)))
  (append '(pasteup-format border independent-parameters) (call-next-method)))

;;; Inherits static-arrays-of method from grays.
;;; Inherits set-not-current from grays.

(defmethod title-bar-string ((pic pasteup))
  (if (independent-parameters pic)
      (format nil "~s independently-scaled" (name (viewable pic)))
      (format nil "(~S - ~,2,-2G) / ~,3,-2G"
	      (name (viewable pic))
	      (pedestal pic) (scale pic))))

;;; Inherits x-dim and y-dim from grays.

;;; *** Should write this, although it's a bit hairy.
(defmethod position-message ((pic pasteup) (vbl viewable) pane y x)
  (declare (ignore pane y x))
  nil)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset-picture-defaults ((pic pasteup) (vbl viewable) &rest initargs
				   &key
				   (zoom nil zoom-supplied-p)
				   (pane-of (slot-value pic 'pane-of))
				   (dimensions (slot-value pic 'dimensions))
				   (independent-parameters
				    nil
				    independent-parameters-supplied-p)
				   (border (slot-value pic 'border) border-supplied-p)
				   (pasteup-format (slot-value pic 'pasteup-format)
						   pasteup-format-supplied-p))
  (when (or independent-parameters-supplied-p border-supplied-p)
    (setf (getf initargs :current) nil))
  (when pasteup-format-supplied-p
    (setf (getf initargs :current) nil))
  (setf (getf initargs :layout) (pasteup-format-to-layout vbl pasteup-format))
  (when (or pasteup-format-supplied-p border-supplied-p (null dimensions))
    (setq dimensions (setf (getf initargs :dimensions)
			   (pasteup-dimensions vbl :pasteup-format pasteup-format :border border))))
  (when zoom-supplied-p
    (setq zoom
	  (cond ((eq zoom :auto)
		 (apply 'min (mapcar #'(lambda (pane-dim pic-dim) (/ pane-dim pic-dim))
				     (dimensions pane-of) dimensions)))
		((num-list-2-p zoom)
		 (apply 'min (mapcar #'(lambda (zoom-dim pic-dim) (/ zoom-dim pic-dim))
				     zoom dimensions)))
		((not (numberp zoom)) 1)
		(t zoom)))
    (setf (getf initargs :zoom)
	  (if (> zoom 1) (round zoom) (/ (round (/ zoom))))))
  (apply #'call-next-method pic vbl initargs))

(defmethod reset-picture-defaults ((pic pasteup) (seq image-sequence) &rest initargs
				   &key (pasteup-format (slot-value pic 'pasteup-format)))
  (unless pasteup-format
    (setf (getf initargs :pasteup-format)
	  (cond ((= (row-dim seq) 1) :horizontal)
		((= (col-dim seq) 1) :vertical)
		(t (error "Image-sequence must be 1xN or Nx1")))))
  (apply #'call-next-method pic seq initargs))

(defmethod pasteup-dimensions ((mat image-matrix) &key pasteup-format border)
  (let ((layout (pasteup-format-to-layout mat pasteup-format)))
    (list (* (+ border (y-dim mat))
	     (list-y-dim layout))
	  (* (+ border (x-dim mat))
	     (list-x-dim layout)))))

;;; ignores the pasteup-format slot for image-matrices
(defmethod pasteup-format-to-layout ((mat image-matrix) format)
  (declare (ignore format))
  (list (row-dim mat) (col-dim mat)))

(defmethod pasteup-format-to-layout ((im image-pair) format)
  (declare (ignore format))
  '(1 2))

(defmethod pasteup-format-to-layout ((seq image-sequence) format)
  (case format
    (:vertical (list (sequence-length seq) 1))
    (:horizontal (list 1 (sequence-length seq)))
    (:square (list (ceiling (sqrt (sequence-length seq)))
		   (ceiling (sequence-length seq)
			    (ceiling (sqrt (sequence-length seq))))))))

(defmethod compute-picture ((pic pasteup) (mat image-matrix))
  (setf (system-dependent-frob pic)
	(make-bltable (screen-of (pane-of pic)) (dimensions pic)
		      :bltable (system-dependent-frob pic)))
  (clear (system-dependent-frob pic) :color (background (pane-of pic)))
  (loop for y from 0 below (list-y-dim (layout pic)) do
	(loop for x from 0 below (list-x-dim (layout pic))
	      for im = (aref (data mat) y x)
	      while (image-p im)
	      for ped  = (if (independent-parameters pic) (minimum im) (pedestal pic))
	      for scale = (if (independent-parameters pic) (range im) (scale pic))
	      for y-offset = (* y (+ (border pic) (y-dim im)))
	      for x-offset = (* x (+ (border pic) (x-dim im)))
	      do
	      (status-message "Rescaling: (~d, ~d)" y x)
	      (draw-float-array (system-dependent-frob pic) (data im)
				ped scale (zoom pic) y-offset x-offset))))

(defmethod compute-picture ((pic pasteup) (seq image-sequence))
  (setf (system-dependent-frob pic)
	(make-bltable (screen-of (pane-of pic)) (dimensions pic)
		      :bltable (system-dependent-frob pic)))
  (clear (system-dependent-frob pic) :color (background (pane-of pic)))
  (loop for y from 0 below (list-y-dim (layout pic)) do
	(loop for x from 0 below (list-x-dim (layout pic))
	      for im = (nth (+ x (* y (list-x-dim (layout pic)))) (image-list seq))
	      while im
	      for ped  = (if (independent-parameters pic) (minimum im) (pedestal pic))
	      for scale = (if (independent-parameters pic) (range im) (scale pic))
	      for y-offset = (* y (+ (border pic) (y-dim seq)))
	      for x-offset = (* x (+ (border pic) (x-dim seq)))
	      do
	      (status-message "Rescaling: (~d, ~d)" y x)
	      (draw-float-array (system-dependent-frob pic) (data im)
				ped scale (zoom pic) y-offset x-offset))))



;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
