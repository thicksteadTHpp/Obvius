;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: gray.lisp
;;;  Author: Simoncelli/Sokolov
;;;  Description: Grayscale picture definition and utilities.
;;;  Creation Date: 6/10/88
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

(export '(reduce-contrast boost-contrast))

;;; The gray picture class is the simplest.  It is intended to display images,
;;; or other floating-point array objects. All grays use the default colormap
;;; of the screen they are on (which may be gamma corrected).  NOTE: These
;;; colormaps not necessarily 8 bits deep!  (Should probably be called
;;; monochrome or something).

(defmethod dimensions ((pic gray))
  (with-slots (zoom viewable) pic
    (mapcar #'(lambda (d) (* zoom d)) (dimensions viewable))))

(defmethod settable-parameters ((class-name (eql 'gray)))    
  (append '(pedestal scale)  (call-next-method)))

;;; Since this is called when the underlying viewable has been destructively 
;;; modified, it assumes that the former scaling parameters are no longer valid.
(defmethod set-not-current ((pic gray))
  (with-slots (reinit-args) pic
    (setf (getf reinit-args :scale) (get-default 'gray 'scale))
    (setf (getf reinit-args :pedestal) (get-default 'gray 'pedestal))
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Interactive methods:

(defmethod title-bar-string ((pic gray))
  (format nil "(~S - ~,2,-2G) / ~,3,-2G" 
	  (name (viewable pic)) (pedestal pic) (scale pic)))

;;; ** Should get rid of pane arg here:
(defmethod position-message ((pic gray) (im image) pane pane-y pane-x)
  (declare (ignore pane))
  (multiple-value-bind (y x)
      (pane-coord-to-viewable-coord pic pane-y pane-x)
    (setf y (floor y) x (floor x))
    (if (array-in-bounds-p (data im) y x)
	(status-message "(~d, ~d): ~g" y x (iref im y x))
	(status-message "(~d, ~d): out of bounds" y x))))

(defmethod position-message ((pic gray) (filt filter) pane pane-y pane-x)
  (declare (ignore pane))
  (multiple-value-bind (y x)
      (pane-coord-to-viewable-coord pic pane-y pane-x)
    (setf y (floor y) x (floor x))
    (if (array-in-bounds-p (kernel filt) y x)
	(status-message "(~d, ~d): ~g" y x (aref (kernel filt) y x))
	(status-message "(~d, ~d): out of bounds" y x))))

(defmethod drag-picture ((pic gray) dy dx)
  (with-slots (y-offset x-offset pane-of zoom) pic
    (unless (and dy dx)
      (setq dy (- y-offset)  dx (- x-offset)))
    (let* ((old-y0 (+ (floor (- (y-dim pane-of) (y-dim pic)) 2) y-offset))
	   (old-x0 (+ (floor (- (x-dim pane-of) (x-dim pic)) 2) x-offset))
	   (old-y1 (+ old-y0 (y-dim pic)))
	   (old-x1 (+ old-x0 (x-dim pic))))
      (mapc #'(lambda (args) (when args (apply 'clear pane-of args)))
	    (rectangles-to-clear old-y0 old-x0 old-y1 old-x1 dy dx)))
    (setf y-offset (+ y-offset dy)
	  x-offset (+ x-offset dx))
    (render pane-of (system-dependent-frob pic) y-offset x-offset zoom)))

;;; Rectangle of stuff exposed by drag that should be cleared.  These are
;;; overlapping, but code is simpler this way!
(defun rectangles-to-clear (old-y0 old-x0 old-y1 old-x1 dy dx)
  (let* ((top (if (< dy 0)
		  (prog1 (+ old-y1 dy) (setq dy (- dy)))
		  old-y0))
	 (left (if (< dx 0)
		   (prog1 (+ old-x1 dx) (setq dx (- dx)))
		   old-x0))
	 (y-rect `(:y0 ,top :x0 ,old-x0 :y1 ,(+ top dy) :x1 ,old-x1))
	 (x-rect `(:y0 ,old-y0 :x0 ,left :y1 ,old-y1 :x1 ,(+ left dx))))
    `(,(unless (= dy 0) y-rect)
      ,(unless (= dx 0) x-rect))))

#|
;;; *** Wasteful to clear the whole pane here...
(defmethod drag-picture ((pic gray) dy dx)
  (with-slots (y-offset x-offset pane-of zoom) pic
    (if (and dy dx)
	(setf y-offset (+ y-offset dy)  x-offset (+ x-offset dx))
	(setf y-offset 0  x-offset 0))
    (clear pane-of)
    (render pane-of (system-dependent-frob pic) y-offset x-offset zoom)))
|#

;;; Boost contrast of picture on top of current pane using SETP.  Keep
;;; the value which maps to the middle intensity (0.5) the same.
;;; Eventually, we'll use slider widgets to do this!
(defun boost-contrast (&optional (factor 2.0)) 
  (let* ((old-scale (getp scale))
	 (new-scale  (/ old-scale factor))
	 (old-pedestal (getp pedestal)))
    (setp scale new-scale
	  pedestal (+ old-pedestal (* 0.5 (- old-scale new-scale))))))

(defun reduce-contrast (&optional (factor 2.0))
  (let* ((old-scale (getp scale))
	 (new-scale  (* old-scale factor))
	 (old-pedestal (getp pedestal)))
    (setp scale new-scale
	  pedestal (+ old-pedestal (* 0.5 (- old-scale new-scale))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Methods for GRAY pictures and IMAGE viewables

;;; This method creates (or re-initializes) the frob, and then
;;; computes its data.  It captures placement, scaling etc.  It is
;;; used in flipbooks as a generic way to compute the data of the
;;; frob.  
;;; NOTE: foreground and background used for dither.
;;; [THO] added picture key arg in call to make -bltable
(defmethod compute-picture ((pic gray) (im image))
  (with-slots (system-dependent-frob pane-of) pic
    (setf system-dependent-frob		;remake the bltable!
	  (make-bltable (screen-of pane-of) (dimensions im)
			:bltable system-dependent-frob
			:depth (if (typep pic 'dither)
				   1
				   (depth (screen-of pane-of)))
			:picture pic
			:foreground :white
			:background :black))
    (clear system-dependent-frob :color (background pane-of))
    (draw-float-array system-dependent-frob (data im)
		      (pedestal pic) (scale pic) (zoom pic)
		      0 0)		;x and y offsets
    ))

(defmethod compute-picture ((pic gray) (filt filter))
  (with-slots (system-dependent-frob pane-of) pic
    (setf system-dependent-frob		;remake the bltable!
	  (make-bltable (screen-of pane-of) (dimensions filt)
			:bltable system-dependent-frob
			:depth (if (typep pic 'dither)
				   1
				   (depth (screen-of pane-of)))
			:picture pic
			:foreground :white
			:background :black))
    (clear system-dependent-frob :color (background pane-of))
    (draw-float-array system-dependent-frob (kernel filt)
		      (pedestal pic) (scale pic) (zoom pic)
		      0 0)		;x and y offsets
    ))

;;; This method captures the default parameter behavior for grays.  It
;;; sets slots automatically if necessary (e.g. pedestal slot is set
;;; to minimum of image if it is not a number).  This may need to be
;;; redefined for some viewable types, but it works for sequences.
;;; Zoom can be a number (zoom-factor), t (zoom to size of pane), a
;;; pair of numbers (zoom to those dimensions), or anything else (zoom
;;; to "natural" size of viewable).  Note: altering zoom does not
;;; force recomputation of frob (i.e. current is not set to nil)
(defmethod reset-picture-defaults ((pic gray) (vbl viewable) &rest initargs
				   &key
				   (pane-of (slot-value pic 'pane-of))
				   (current (slot-value pic 'pane-of))
				   (zoom nil zoom-supplied-p)
				   (scale nil scale-supplied-p)
				   (pedestal nil pedestal-supplied-p))
  (when scale-supplied-p
    (cond ((eq scale :auto) (setf (getf initargs :scale) (range vbl)))
	  ((not (numberp scale)) (setf (getf initargs :scale) 1)))
    (when (current-p pic) (setf (getf initargs :current) nil))) ;for set-not-current
  (when pedestal-supplied-p
    (cond ((eq pedestal :auto) (setf (getf initargs :pedestal) (minimum vbl)))
	  ((not (numberp pedestal)) (setf (getf initargs :pedestal) 0)))
    (when (current-p pic) (setf (getf initargs :current) nil)))
  (when zoom-supplied-p
    (setq zoom
	  (cond ((numberp zoom) zoom)
		((eq zoom :auto)
		 (apply 'min (mapcar #'(lambda (pane-dim pic-dim) (/ pane-dim pic-dim))
				     (dimensions pane-of) (dimensions vbl))))
		((num-list-2-p zoom)
		 (apply 'min (mapcar #'(lambda (zoom-dim pic-dim) (/ zoom-dim pic-dim))
				     zoom (dimensions vbl))))
		(t 1)))
    (setf (getf initargs :zoom) (if (> zoom 1) (round zoom) (/ (round (/ zoom))))))
  (apply #'call-next-method pic vbl initargs))

;;; Use auto-zoom for filters:
(defmethod reset-picture-defaults ((pic gray) (filt filter) &rest initargs
				   &key (zoom nil zoom-supplied-p)
				   &allow-other-keys)
  (unless zoom-supplied-p
    (setf (getf initargs :zoom) :auto))
  (apply #'call-next-method pic filt initargs))

;;; Standard system-independent draw-float-array method.  This draws
;;; the float-array into the bltable, dithering or quantizing as
;;; necessary.  Assumes that there are 256 gray levels when the frob
;;; depth is 8.  This version used by postscript-screens in
;;; hardcopy.lisp.  See x-blt.lisp for example of system-dependent
;;; version, for which there may be less than 256 grays available in
;;; the X colormap. Zoom is ignored here: zooming happens in render!
(defmethod draw-float-array ((bltable frob) float-array
			     pedestal scale zoom
			     y-offset x-offset)
  (declare (ignore zoom))
  (with-slots (data) bltable
    (unwind-protect
	 (cond ((= (depth bltable) 1)	;bitmap
		(internal-dither-into-1bit float-array 
					   (x-dim float-array) (y-dim float-array)
					   data (* (x-dim data) (depth data))
					   (float pedestal) (float (/-0 2.0 scale 1.0))
					   (round x-offset) (round y-offset)))
	       ((= (depth bltable) 8)	;8bit pixmap
		(internal-f-into-8bit float-array
				      (x-dim float-array) (y-dim float-array) 
				      data (x-dim data)
				      (float pedestal) (float (/-0 256.0 scale 1.0))
				      (round x-offset) (round y-offset)
				      0 255))		;min and max 8bit values
	       (t (error "Can't handle Screen/Bltable depths of ~A" (depth bltable)))))
    bltable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BITMAP pictures.

(defmethod settable-parameters ((class-name (eql 'bitmap)))    
  (append '(foreground background)  (call-next-method)))

(defmethod dimensions ((pic bitmap))
  (with-slots (zoom viewable) pic
    (mapcar #'(lambda (d) (* zoom d)) (dimensions viewable))))

#|
;;; *** this is not really correct, but it is probably OK
(defmethod dimensions ((bitmap bitmap))
  (if (system-dependent-frob bitmap)
      (dimensions (system-dependent-frob bitmap))
      (list 0 0)))
|#

(defmethod compute-picture ((pic bitmap) (im bit-image))
  (setf (system-dependent-frob pic)
	(make-bltable (screen-of (pane-of pic)) (dimensions im)
		      :bltable (system-dependent-frob pic)
		      :depth 1
		      :foreground (foreground pic)
		      :background (background pic)))
  (clear (system-dependent-frob pic))
  (draw-bit-array (system-dependent-frob pic) (data im) (zoom pic)))

(defmethod reinitialize-instance ((pic bitmap) &rest initargs)
  (when (and (system-dependent-frob pic)
	     (or (getf initargs :foreground) (getf Initargs :background)))
    (apply #'reinitialize-instance (system-dependent-frob pic)
	   (sub-plist initargs :foreground :background)))
  (call-next-method))

;;; Default draw-bit-array just copies the data.  This is what
;;; X-bltables do (see method in x-blt.lisp).  Postscript-bltables
;;; call pack-bit-array (see method in hardcopy.lisp).  NOTE: Can't
;;; just use copy, since arrays may be different sizes!  *** rewrite
;;; internal1-paste in C!
(defmethod draw-bit-array ((frob t) bit-array zoom)
  (declare (ignore zoom))
  (internal1-paste bit-array (data frob) 0 0 (y-dim bit-array) (x-dim bit-array) 0 0))

(defmethod drag-picture ((pic bitmap) dy dx)
  (with-slots (y-offset x-offset zoom pane-of) pic
    (unless (and dy dx)
      (setq dy (- y-offset)  dx (- x-offset)))
    (let* ((old-y0 (+ (floor (- (y-dim pane-of) (y-dim pic)) 2) y-offset))
	   (old-x0 (+ (floor (- (x-dim pane-of) (x-dim pic)) 2) x-offset))
	   (old-y1 (+ old-y0 (y-dim pic)))
	   (old-x1 (+ old-x0 (x-dim pic))))
      (mapc #'(lambda (args) (when args (apply 'clear pane-of args)))
	    (rectangles-to-clear old-y0 old-x0 old-y1 old-x1 dy dx)))
    (setf y-offset (+ y-offset dy) x-offset (+ x-offset dx))
    (render pane-of (system-dependent-frob pic) y-offset x-offset zoom)))

#|
(defmethod drag-picture ((pic bitmap) dy dx)
  (with-slots (y-offset x-offset zoom pane-of) pic
    (if (and dy dx)
	(setf y-offset (+ y-offset dy)  x-offset (+ x-offset dx))
	(setf y-offset 0                x-offset 0))
    (clear pane-of)
    (render pane-of (system-dependent-frob pic) y-offset x-offset zoom)))
|#

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
