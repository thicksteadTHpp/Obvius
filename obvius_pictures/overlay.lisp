;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: overlay.lisp
;;;  Author: Heeger with "help" from Chichilnisky
;;;  Description: methods for overlaying pictures
;;;  Creation Date: 3/90
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;; Test Examples:

;;; discrete functions
(display (make-viewable-sequence
	  (list (setq df0 (make-discrete-function #'(lambda (x) x) 0.0 1.0 :size 20))
		(setq df1 (make-discrete-function #'(lambda (x) (sqr x)) 0.0 1.0 :size 20))
		(setq df2 (make-discrete-function #'(lambda (x) (sqrt x)) 0.0 1.0 :size 20))))
	 'overlay
	 :plot-symbol :square :color :green :fill-symbol-p t :axis-color :pink :zoom 10)
(setp :current-picture 1 :plot-symbol :circle :color :red :fill-symbol-p nil)

;;; one-d-images
(display (make-image-sequence
	  (list (setq im0 (make-ramp '(1 64) :slope 1/64))
		(setq im1 (make-sin-grating '(1 64)))))
	 'overlay)
(setp :current-picture 1 :y-range '(-1.0 1.0))
(setf (iref im0 0 0) 2.0)

;;; bit-image on image 
(load-image "/home/heeger/images/einstein")
(setq edges (zero-crossings (-. (blur einstein :level 1) (blur einstein :level 3))))
(display (make-viewable-sequence (list einstein edges)) 'overlay)
(setp :current-picture 1 :foreground :red)
(setp :current-picture 0 :scale 400.0)

;;; bit-image on bit-image
(load-image "/home/heeger/images/reagan")
(setq edges1 (zero-crossings (-. (blur reagan :level 1) (blur reagan :level 3))))
(display (make-viewable-sequence (list edges edges1)) 'overlay)
(setp :current-picture 1 :foreground :red)
(setp :current-picture 0 :foreground :green)

;;; scatter plots
(progn
  (setq contrasts (make-image (make-matrix
			       (list .05 .075 .10625 .1375 .175 .2125 .25 .35 .5))))
  (setq pref-responses (make-image (make-matrix
				    (mapcar #'(lambda (x) (/ x 3.0))
					    (list 1.5 6 11 12 17 26 28 31 39)))))
  (setq null-responses (make-image (make-matrix
				    (mapcar #'(lambda (x) (/ x 3.0))
					    (list .1 .5 1 1.5 3.8 4 7.5 9 11)))))
  (display
   (make-viewable-sequence (list (make-image-pair (list pref-responses contrasts))
				 (make-image-pair (list null-responses contrasts))))
   'overlay
   :sub-display-types '(scatter-plot scatter-plot)
   :graph-type :line :plot-symbol :circle
   :zoom 400 :aspect-ratio 0.5 :y-tick-step 1.0 :x-tick-step 0.1))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod static-arrays-of ((pic overlay))
  (mapcan #'static-arrays-of (picture-list pic)))

;;; Calls destroy on each member of the picture-list.
;;; Would be nice to do call-next-method and avoid code duplication, but then
;;; the static arrays will be destroyed twice.
(defmethod destroy ((pic overlay) &key &allow-other-keys)
  (with-slots (viewable) pic
    (setf (pictures-of viewable)
	  (delete pic (pictures-of viewable))) 
    (mapc #'(lambda (p) (destroy p :silent t)) (picture-list pic))
    (remove-picture pic)
    (when (and *auto-destroy-orphans* (orphaned-viewable-p viewable))
      (destroy viewable))))

(defmethod set-not-current ((pic overlay))
  (loop for sub-pic in (picture-list pic) do (set-not-current sub-pic))
  (reinitialize-instance pic)
  (call-next-method))

(defmethod zoom-picture ((overlay overlay) factor y x)
  (declare (ignore y x))
  (reinitialize-instance overlay :zoom (* factor (slot-value overlay 'zoom)))
  (draw-pane (pane-of overlay)))

;;; *** 
(defmethod dimensions ((pic overlay))
  (error "Dimensions not defined for overlay"))

(defmethod title-bar-string ((pic overlay))
  (format nil "~s:~s" (name (viewable pic)) (current-picture pic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Related to mouse:

(defmethod position-message ((pic overlay) (vbl viewable) pane y x)
  (declare (ignore pane y x))
  (status-message "Position message broken on overlays"))

#|
;;; *** Would like to do this, but we'd have to do compute-picture on
;;; all of the sub-pictures in overlay-drawables and overlay-bltables.
;;; But that would take twice as long to display.
(defmethod position-message ((pic overlay) (vbl viewable) pane y x)
  (position-message (nth (current-picture pic) (picture-list pic))
		    (nth (current-picture pic) (viewable-list vbl))
		    pane y x))
|#

;;; Updates x-offset and y-offset in the overlay itself,
;;; updates these slots for each of the sub-pictures (necessary for
;;; position-message), and then redraws the pane.
(defmethod drag-picture ((pic overlay) dy dx)
  (with-slots (y-offset x-offset pane-of picture-list) pic
    (if (and dy dx)
	(setf y-offset (+ y-offset dy)  x-offset (+ x-offset dx))
	(setf y-offset 0  x-offset 0))
    (dolist (sub-pic picture-list)
      (setf (slot-value sub-pic 'y-offset) y-offset)
      (setf (slot-value sub-pic 'x-offset) x-offset))
    (draw-pane pane-of :clear t)))

;;; Redraws the pane to update the title bar.  If there is a picture
;;; parameters dialog, need to update it so widgets point at the new
;;; sub-picture (this is done by an around method in x-control-panel).
(defmethod single-step ((pic overlay) &optional (step-size 1))
  (with-slots (current-picture picture-list pane-of) pic
    (let* ((old-subpic-num current-picture)
	   (new-subpic-num (mod (+ old-subpic-num step-size) (length picture-list)))
	   (old-subpic (nth old-subpic-num picture-list))
	   (new-subpic (nth new-subpic-num picture-list)))
      (setf (current-picture pic) new-subpic-num)
      (update-subpic-dialog pic old-subpic new-subpic))
    (draw-pane pane-of :clear t)))

;;; Make sure that panes of sub-pictures are the same as the pane-of
;;; the overlay -- this is necessary for position-messages, etc.
(defmethod (setf pane-of) :around (pane (pic overlay))
  (call-next-method)
  (dolist (sub-pic (slot-value pic 'picture-list))
    (setf (slot-value sub-pic 'pane-of) pane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Picture Param Dialogs:

;;; *** There's a hairy kludge in this.  Normally, slot-value-dialog
;;; widgets all point to slots in the same object.  Here, some of the
;;; widgets refer to (and point to) the overlay and some to the
;;; sub-picture.  Picture-slot-update-function looks at the car
;;; of the widgets-list to find the picture.  So, the first field in
;;; the dialog must be a slot in the overlay.  Yuch!

(defmethod settable-parameters ((class-name (eql 'overlay)))
  (append '(current-picture) (call-next-method)))

;;; Slightly different for overlays (like flipbooks) to include
;;; sub-picture parameters.
(defmethod make-slot-value-dialog
    ((pic overlay)
     &key
     (slot-names (visible-slot-names (class-of pic)))
     (label (format nil "slot value dialog"))
     (update-function 'standard-update-function)
     (update-arguments nil)
     (exit-function 'destroying-exit-function))
  (setq slot-names (remove-duplicates
		    (apply 'append slot-names (mapcar 'settable-parameters
						      (picture-list pic)))
		    :from-end t))
  (call-next-method pic
		    :slot-names slot-names
		    :label label
		    :update-function update-function
		    :update-arguments update-arguments
		    :exit-function exit-function)
  (let ((current-subpic (nth (current-picture pic) (picture-list pic))))
    (update-subpic-dialog pic current-subpic current-subpic)))

;;; Slightly different for overlays (like flipbooks) to make it
;;; possible to set slots of the sub-pictures.
(defmethod make-slot-value-widget ((pic overlay) slot-name &rest initargs)
  (let* ((slot (find-slot (class-of pic) slot-name t))
	 (type (if slot (CLOS::slot-definition-type slot) t)))
    ;; if slot is not in the overlay, then look for it in a sub-pic
    (unless slot
      (setq pic (find-if #'(lambda (sub-pic)
			     (find-slot (class-of sub-pic) slot-name t))
			 (picture-list pic)))
      (when pic
	(setq slot (find-slot (class-of pic) slot-name t))
	(setq type (if slot (CLOS::slot-definition-type slot) t))))
    (unless slot
      (error "~A is not a valid slot-name for ~A" slot-name pic))
    (multiple-value-bind (widget-subtype specialized-initargs)
	(compute-item-type-and-initargs type)
      (let* ((widget-class (intern (concatenate 'string "SLOT-"
						(symbol-name widget-subtype)) 'obvius)))
	(unless (find-class widget-class nil)	;no error
	  (warn "There is no class named: ~A" widget-class)
	  (setq widget-class 'slot-text-field))
	(apply 'make-instance widget-class
	       :object pic :slot-name slot-name
	       (append initargs specialized-initargs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Enforced common params:

;;; List of parameters that must be the same for each sub-picture.
;;; For all picture types, should always include: zoom, x-offset, and
;;; y-offset.
(defmethod common-parameters ((class-name t))
  (list :zoom :x-offset :y-offset))

(defmethod common-parameters ((class-name (eql 'graph)))
  (append (list :axis-color :y-range :x-range
		:y-axis-type :y-axis :x-axis :y-tick-step :x-tick-step
		:y-label :x-label :aspect-ratio :y-max-label-length :x-max-label-length
		:y-tick-format-string :x-tick-format-string :y-tick-length :x-tick-length
		:y-tick-gap :x-tick-gap)
	  (call-next-method)))

(defmethod common-parameters ((class-name (eql 'polar-plot)))
  (append (list :maximum)
	  (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; All relevant slots of sub-picture (depending on type of
;;; sub-picture) are recomputed.  Zoom, x-offset, y-offset must be
;;; same in all sub-pictures.
(defmethod reset-picture-defaults ((pic overlay) (vbl viewable-sequence)
				   &rest initargs &key
				   (pane-of (slot-value pic 'pane-of))
				   (current (slot-value pic 'current))
				   (current-picture (slot-value pic 'current-picture))
				   (picture-list (slot-value pic 'picture-list))
				   (sub-display-types (slot-value pic 'sub-display-types))
				   &allow-other-keys)
  (setf (getf initargs :sub-display-types)
	(setq sub-display-types
	      (or sub-display-types
		  (loop for sub-vbl in (viewable-list vbl)
			collect
			(eval (get-default (class-name (class-of sub-vbl)) :display-type))))))

  ;; Args which don't belong to overlay
  (let* ((common-parameters (remove-duplicates
			     (apply 'append (mapcar 'common-parameters sub-display-types))))
	 (sub-initargs (copy-list initargs))
	 common-initargs noncommon-initargs)
    (remf sub-initargs :sub-display-types)
    (remf sub-initargs :picture-list)
    (remf sub-initargs :current-picture)
    (setq common-initargs (apply 'sub-plist sub-initargs common-parameters))
    (setq noncommon-initargs (plist-difference sub-initargs common-initargs))
    (setq initargs (plist-difference initargs sub-initargs))

    (when (null picture-list)
      ;; Create picture-list
      (setq picture-list
	    (setf (getf initargs :picture-list) 
		  (setf (slot-value pic 'picture-list)
			(loop for sub-vbl in (viewable-list vbl)
			      for sub-display-type in sub-display-types
			      collect (apply 'make-instance sub-display-type
					     :viewable sub-vbl :pane-of pane-of
					     sub-initargs)))))

      ;; Make sure common-initargs are set the same in all sub-pics.
      (setq current-picture (setf (getf initargs :current-picture) 0))
      (reset-sub-picture-defaults (nth current-picture picture-list)
				  (nth current-picture (viewable-list vbl))
				  pic vbl)
      (let* ((all-sub-initargs (loop with current-sub-pic = (nth current-picture picture-list)
				     with class = (class-of current-sub-pic)
				     for slot in (CLOS::class-slots class)
				     for slot-key = (CLOS::slot-definition-initargs slot)
				     for slot-name = (CLOS::slot-definition-name slot)
				     append (list (car slot-key)
						  (slot-value current-sub-pic slot-name))))
	     (all-common-initargs (apply 'sub-plist all-sub-initargs common-parameters)))
	(loop for subl = common-initargs then (cddr subl) until (null subl)
	      for key = (car subl)
	      do (setf (getf all-common-initargs key) (getf common-initargs key)))
	(setq common-initargs all-common-initargs)
	(setq noncommon-initargs nil)))

    ;; reset common-initargs
    (loop for sub-pic in (picture-list pic)
	  for sub-vbl in (viewable-list vbl)
	  do
	  ;;(apply #'reinitialize-instance sub-pic common-initargs)
	  (apply #'reset-sub-picture-defaults sub-pic sub-vbl pic vbl
		 common-initargs))

    ;; set zoom and offset slots in overlay, using values from sub-pic
    (let ((sub-pic (nth current-picture picture-list)))
      (setf (getf initargs :zoom) (slot-value sub-pic 'zoom))
      (setf (getf initargs :x-offset) (slot-value sub-pic 'x-offset))
      (setf (getf initargs :y-offset) (slot-value sub-pic 'y-offset))
      )
    
    ;; reset noncommon-initargs
    (when noncommon-initargs
      (let ((sub-pic (nth current-picture picture-list)))
	(apply #'reinitialize-instance sub-pic
	       noncommon-initargs)))

    (apply #'call-next-method pic vbl
	   :current nil :system-dependent-frob (system-dependent-frob pic)
	   initargs)))

;;; This allows computation of parameters that require looking at all sub pics.
(defmethod reset-sub-picture-defaults ((sub-pic picture) (sub-vbl viewable)
				       pic vbl &rest initargs)
  (declare (ignore pic vbl))
  (apply #'reinitialize-instance sub-pic initargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Used for overlaying drawables.
(def-simple-class overlay-frob ()
  ((picture :initform nil)
   ))

(defmethod render ((pane pane) (frob overlay-frob) y-offset x-offset zoom)
  (loop for sub-pic in (picture-list (picture frob))
	do
	(render pane (system-dependent-frob sub-pic) y-offset x-offset zoom)))

(defmethod reset-sub-picture-defaults ((sub-pic graph) (sub-vbl viewable)
				       pic vbl
				       &rest initargs
				       &key
				       (y-range nil y-range-supplied-p)
				       y-tick-step
				       &allow-other-keys)
  (unless y-range-supplied-p
    (setq y-range
	  (setf (getf initargs :y-range)
		(list (apply 'min (mapcar #'minimum (viewable-list vbl)))
		      (apply 'max (mapcar #'maximum (viewable-list vbl)))))))
  (unless (numberp y-tick-step)
    (setq y-tick-step (setf (getf initargs :y-tick-step)
	  (compute-tick-step y-range))))
  (apply #'call-next-method sub-pic sub-vbl pic vbl initargs))

(defmethod reset-sub-picture-defaults ((sub-pic polar-plot) (sub-vbl viewable)
				       pic vbl
				       &rest initargs
				       &key
				       (maximum nil maximum-supplied-p)
				       (zoom nil zoom-supplied-p))
  (unless maximum-supplied-p
    (setq maximum
	  (setf (getf initargs :maximum) (apply 'max (mapcar #'maximum (viewable-list vbl))))))
  (unless zoom-supplied-p (setq zoom (setf (getf initargs :zoom) :auto)))
  (when (eq zoom :auto)
    (setq zoom (setf (getf initargs :zoom) (dimensions (pane-of pic)))))
  (when (num-list-2-p zoom)
    (setq zoom (setf (getf initargs :zoom) (/ (apply 'min (getf initargs :zoom))
					      (getf initargs :maximum) 2))))
  (apply #'call-next-method sub-pic sub-vbl pic vbl initargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Overlay methods switch off first vbl in vbl-sequence, used to
;;; figure out what type of pictures to make (e.g., bltables vs
;;; drawables.  Also, for error checking.

;;; Compute-picture is really just a dummy that calls overlay.

;;; Overlay calls either overlay-bltables or overlay-drawables, which
;;; in turn loops through all of the sub-vbls and calls
;;; overlay-picture.  Overlay-picture switches on both the sub-vbl and
;;; the sub-pic.

(defmethod compute-picture ((pic overlay) (vbl viewable-sequence))
  (overlay (car (viewable-list vbl)) vbl pic))

(defmethod overlay ((sub-vbl viewable) vbl pic)
  (declare (ignore pic))
  (error "Can't display ~a as an overlay" vbl))

(defmethod overlay ((df discrete-function) vbl pic)
  (unless (every #'(lambda (x) (discrete-function-p x)) (viewable-list vbl))
    (error "Overlays on a discrete-function must all be discrete-functions"))
  (apply 'check-size df (viewable-list vbl))
  (overlay-drawables pic vbl))

(defmethod overlay ((im one-d-image) vbl pic)
  (unless (every #'(lambda (x) (one-d-image-p x)) (viewable-list vbl))
    (error "Overlays on a one-d-image must all be one-d-images"))
  (apply 'check-size im (viewable-list vbl))
  (overlay-drawables pic vbl))

(defmethod overlay ((im image) vbl pic)
  (unless (every #'(lambda (x) (bit-image-p x)) (cdr (viewable-list vbl)))
    (error "Overlays on an image must all be bit-images"))
  (apply 'check-size im (viewable-list vbl))
  (overlay-bltables pic vbl))

(defmethod overlay ((im bit-image) vbl pic)
  (unless (every #'(lambda (x) (bit-image-p x)) (viewable-list vbl))
    (error "Overlays on an bit-image must all be bit-images"))
  (apply 'check-size im (viewable-list vbl))
  (overlay-bltables pic vbl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun overlay-drawables (pic vbl)
  (setf (system-dependent-frob pic) (make-instance 'overlay-frob :picture pic))
  (loop for sub-pic in (picture-list pic)
	for sub-vbl in (viewable-list vbl)
	for i from 0
	do
	(when (not (current sub-pic))
	  (status-message "computing picture, frame ~d ..." i)
	  (without-status-messages (compute-picture sub-pic sub-vbl))
	  (setf (current sub-pic) (current sub-vbl)))
	finally (status-message "")))

#|
;;; *** I think these are no longer used.  -DH  8/26/93
(defmethod overlay-picture ((sub-vbl discrete-function) (sub-pic graph) vbl pic)
  (declare (ignore vbl))
  (with-slots (graph-type y-range x-range y-axis x-axis y-tick-step x-tick-step 
	       y-tick-length x-tick-length y-tick-gap x-tick-gap 
	       y-tick-format-string x-tick-format-string y-label x-label
	       color axis-color line-width
	       plot-symbol fill-symbol-p symbol-size
	       x-offset y-offset) sub-pic
    (multiple-value-bind (graph->frob-y graph->frob-x data->frob-y data->frob-x)
	(compute-graph-transforms sub-pic)
      (draw-graph (system-dependent-frob pic) (data sub-vbl)
		  graph-type graph->frob-y graph->frob-x data->frob-y data->frob-x
		  y-range x-range y-axis x-axis y-tick-step x-tick-step 
		  y-tick-length x-tick-length y-tick-gap x-tick-gap 
		  y-tick-format-string x-tick-format-string y-label x-label
		  :color color :axis-color axis-color :line-width line-width
		  :plot-symbol plot-symbol :fill-symbol-p fill-symbol-p
		  :symbol-size symbol-size))))

(defmethod overlay-picture ((sub-vbl one-d-image) (sub-pic graph) vbl pic)
  (declare (ignore vbl))
  (with-slots (graph-type y-range x-range y-axis x-axis y-tick-step x-tick-step 
	       y-tick-length x-tick-length y-tick-gap x-tick-gap 
	       y-tick-format-string x-tick-format-string y-label x-label
	       color axis-color line-width
	       plot-symbol fill-symbol-p symbol-size
	       x-offset y-offset) sub-pic
    (multiple-value-bind (graph->frob-y graph->frob-x data->frob-y data->frob-x)
	(compute-graph-transforms sub-pic)
      (draw-graph (system-dependent-frob pic) (data sub-vbl)
		  graph-type graph->frob-y graph->frob-x data->frob-y data->frob-x
		  y-range x-range y-axis x-axis y-tick-step x-tick-step 
		  y-tick-length x-tick-length y-tick-gap x-tick-gap 
		  y-tick-format-string x-tick-format-string y-label x-label
		  :color color :axis-color axis-color :line-width line-width
		  :plot-symbol plot-symbol :fill-symbol-p fill-symbol-p
		  :symbol-size symbol-size))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Overlays of scatter plots

(defmethod reset-sub-picture-defaults ((sub-pic scatter-plot) (sub-vbl image-pair)
				       pic vbl
				       &rest initargs
				       &key
				       (y-range nil y-range-supplied-p)
				       y-tick-step
				       &allow-other-keys)
  (unless y-range-supplied-p
    (setq y-range
	  (setf (getf initargs :y-range)
		(list (apply 'min (mapcar #'(lambda (v) (minimum (y-component v)))
					  (viewable-list vbl)))
		      (apply 'max (mapcar #'(lambda (v) (maximum (y-component v)))
					  (viewable-list vbl)))))))
  (unless (numberp y-tick-step)
    (setq y-tick-step (setf (getf initargs :y-tick-step)
			    (compute-tick-step y-range))))
  (apply #'call-next-method sub-pic sub-vbl pic vbl initargs)
  )

(defmethod common-parameters ((class-name (eql 'scatter-plot)))
  (append (list :axis-color :y-range :x-range
		:y-axis-type :y-axis :x-axis :y-tick-step :x-tick-step
		:y-label :x-label :aspect-ratio :y-max-label-length :x-max-label-length
		:y-tick-format-string :x-tick-format-string :y-tick-length :x-tick-length
		:y-tick-gap :x-tick-gap)
	  (call-next-method)))

(defmethod overlay ((im image-pair) vbl pic)
  (unless (every #'(lambda (x) (image-pair-p x)) (viewable-list vbl))
    (error "Overlays on an image-pair must all be image-pairs"))
  (apply 'check-size im (viewable-list vbl))
  (overlay-drawables pic vbl))

(defmethod overlay-picture ((sub-vbl image-pair) (sub-pic scatter-plot) vbl pic)
  (declare (ignore vbl))
  (with-slots (system-dependent-frob pane-of bounding-region graph-type
	       y-range x-range y-axis x-axis y-tick-step x-tick-step
	       y-tick-length x-tick-length y-tick-gap x-tick-gap
	       y-tick-format-string x-tick-format-string
	       y-label x-label axis-color color line-width
	       plot-symbol fill-symbol-p symbol-size) sub-pic
    (multiple-value-bind (graph->frob-y graph->frob-x data->frob-y data->frob-x)
	(compute-graph-transforms sub-pic)
      (draw-graph (system-dependent-frob pic)
		  (list (data (y-component sub-vbl))
			(data (x-component sub-vbl)))
		  graph-type graph->frob-y graph->frob-x data->frob-y data->frob-x
		  y-range x-range y-axis x-axis y-tick-step x-tick-step 
		  y-tick-length x-tick-length y-tick-gap x-tick-gap 
		  y-tick-format-string x-tick-format-string y-label x-label
		  :color color :axis-color axis-color :line-width line-width
		  :plot-symbol plot-symbol :fill-symbol-p fill-symbol-p
		  :symbol-size symbol-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun overlay-bltables (pic vbl)
  (with-slots (system-dependent-frob pane-of) pic
    (setf system-dependent-frob		;remake the bltable!
	  (make-bltable (screen-of pane-of) (dimensions (car (viewable-list vbl)))
			:bltable system-dependent-frob
			:depth (depth (screen-of pane-of))))
    (clear system-dependent-frob :color (background pane-of))
    (loop for sub-pic in (picture-list pic)
	  for sub-vbl in (viewable-list vbl)
	  for i from 0
	  do
	  (status-message "computing overlay picture, frame ~d ..." i)
	  ;; *** this would fix position-message
	  ;; (compute-picture sub-pic sub-vbl)
	  (overlay-picture sub-vbl sub-pic vbl pic)
	  finally (status-message ""))))

(defmethod overlay-picture ((sub-vbl image) (sub-pic gray) vbl pic)
  (declare (ignore vbl))
  (draw-float-array (system-dependent-frob pic) (data sub-vbl)
		    (pedestal sub-pic) (scale sub-pic) (zoom sub-pic)
		    0 0))

(defmethod overlay-picture ((sub-vbl bit-image) (sub-pic bitmap) vbl pic)
  (declare (ignore vbl))
  (overlay-bitmap (screen-of (pane-of pic)) pic sub-pic sub-vbl))

;;; *** This should be written in C for float arrays.  
(defun overlay-bitmap-data (bit-array byte-array val)
  (declare (type (array (unsigned-byte 8) (* *)) byte-array))
  (declare (type (array bit (* *)) bit-array))
  (dotimes (j (y-dim bit-array))
    (declare (fixnum j))
    (dotimes (i (x-dim bit-array))
      (declare (fixnum i))
      (unless (zerop (aref bit-array j i))
	(setf (aref byte-array j i) val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This makes hardcopy work on overlays.

(defmethod copy-object ((pic overlay) &rest initargs)
  (let ((new-sub-pics (loop for sub-pic in (picture-list pic)
			    collect
			    (apply 'copy-object sub-pic
				   :pane-of (pane-of sub-pic)
				   :system-dependent-frob nil
				   :current nil
				   :index (incf *picture-index*)
				   initargs))))
    (apply #'call-next-method pic :picture-list new-sub-pics initargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Local Variables:
;;; buffer-read-only: t 
;;; End:

