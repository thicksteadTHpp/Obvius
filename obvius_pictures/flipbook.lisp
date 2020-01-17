;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: flipbook.lisp
;;;  Author: Heeger, Simoncelli
;;;  Description: Flip book pictures
;;;  Creation Date: 
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

;;;; **** Should eventually provide mechanism to make some sub-picture
;;;; parameters independent and others dependent (e.g., independent
;;;; zoom, but same gray scale and pedestal).

(defmethod settable-parameters ((class-name (eql 'flipbook)))
  (append '(sub-display-type independent-parameters
	    frame-delay seq-delay back-and-forth)
	  (call-next-method)))

(defmethod static-arrays-of ((pic flipbook))
  (mapcan #'static-arrays-of (picture-list pic)))

;;; Calls destroy on each member of the picture-list.
;;; Would be nice to do call-next-method and avoid code duplication, but then
;;; the static arrays will be destroyed twice.
(defmethod destroy ((pic flipbook) &key &allow-other-keys)
  (with-slots (viewable) pic
    (setf (pictures-of viewable)
	  (delete pic (pictures-of viewable))) 
    (mapc #'(lambda (p) (destroy p :silent t)) (picture-list pic))
    (remove-picture pic)
    (when (and *auto-destroy-orphans* (orphaned-viewable-p viewable))
      (destroy viewable))))

(defmethod set-not-current ((pic flipbook))
  (loop for sub-pic in (picture-list pic) do
	(set-not-current sub-pic))
  (call-next-method))

;;; *** this is not really correct, but it is probably OK. -DH
(defmethod dimensions ((pic flipbook))
  (dimensions (car (picture-list pic))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod title-bar-string ((pic flipbook))
  (concatenate 'string
	       (format nil "~S #~d: " (name (viewable pic)) (current-picture pic))
	       (title-bar-string (nth (current-picture pic) (picture-list pic)))))

(defmethod position-message ((pic flipbook) (seq viewable-sequence) pane y x)
  (position-message (nth (current-picture pic) (picture-list pic))
		    (nth (current-picture pic) (viewable-list seq))
		    pane y x))

;;; Updates x-offset and y-offset in the flipbook itself, updates
;;; these slots for each of the sub-pictures (necessary for
;;; position-message), and then presents the sub-picture.  Could do
;;; this with reinitialize-instance, too.
(defmethod drag-picture ((pic flipbook) dy dx)
  (with-slots (y-offset x-offset zoom pane-of picture-list
			system-dependent-frob current-picture) pic
    (if (and dy dx)
	(setf y-offset (+ y-offset dy)  x-offset (+ x-offset dx))
	(setf y-offset 0  x-offset 0))
    (dolist (sub-pic picture-list)
      (setf (slot-value sub-pic 'y-offset) y-offset)
      (setf (slot-value sub-pic 'x-offset) x-offset))
    (clear pane-of)
    (render pane-of system-dependent-frob y-offset x-offset zoom)))

;;; Flipping through sub-pictures.  Redraws the pane to update the
;;; title bar.  If there is a picture parameters dialog, need to
;;; update it so widgets point at the new sub-picture (this is done by
;;; update-subpic-dialog).
(defmethod single-step ((pic flipbook) &optional (step-size 1))
  (with-slots (current-picture picture-list pane-of) pic
    (let* ((old-subpic-num current-picture)
	   (new-subpic-num (mod (+ old-subpic-num step-size) (length picture-list)))
	   (old-subpic (nth old-subpic-num picture-list))
	   (new-subpic (nth new-subpic-num picture-list)))
      (setf (current-picture pic) new-subpic-num)
      (update-subpic-dialog pic old-subpic new-subpic))
    (draw-pane pane-of :clear t)))

;;; Display flipbook as a movie.  Repeats should be a number or t
;;; (repeat until mouse click).
(defun display-seq (flipbook &optional (repeat t))
  (when (null repeat) (setq repeat 1))
  (let* ((pane (pane-of flipbook))
	 (frobs (mapcar 'system-dependent-frob (picture-list flipbook)))
	 (test-fn
	  (if (numberp repeat)
	      #'(lambda (i) (and (< i repeat)
				 (slot-value flipbook 'displaying-p)))
	      #'(lambda (i)
		  (declare (ignore i))
		  (slot-value flipbook 'displaying-p)))))
    (when (back-and-forth flipbook)
      (setq frobs (append (cdr frobs) (cdr (reverse frobs)))))
    (set-pane-title-bar pane "showing movie ...")
    (setf (displaying-p flipbook) t)
    (unwind-protect
	 (fast-display-seq
	  (car frobs) pane frobs
	  (x-offset flipbook) (y-offset flipbook) (zoom flipbook)
	  (frame-delay flipbook) (seq-delay flipbook) test-fn)
      (draw-pane pane :clear t))))

;;; Default method just calls render.  Specialized methods can do
;;; something low-level based on type of system-dependent-frob.
(defmethod fast-display-seq ((frob t) pane frobs
			     x-offset y-offset zoom
			     frame-delay seq-delay test-fn)
  (loop for count from 0 
	while (funcall test-fn count) do
	(loop for frob in frobs
	      while (funcall test-fn count) do
	      (clear pane)
	      (render pane frob y-offset x-offset zoom)
	      (when (> frame-delay 0) (frame-sleep frame-delay)))
	(when (> seq-delay 0) (frame-sleep seq-delay))))

;;; Use lexical closure so that we don't have to cons the array every
;;; time.   *** This is SUN  dependent and probably doesn't belong here!
(let ((timevalstruct (make-array 2 :element-type '(unsigned-byte 32))))
  (defun frame-sleep (frames)
    (declare (fixnum frames))
    (setf (aref timevalstruct 0) (floor frames 60)
	  (aref timevalstruct 1) (mod (the fixnum (* frames 16667)) 1000000))
    (select-fds 0 0 0 0 timevalstruct)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 8/18/92: Changed to check if sub-pics are current-p.instead of (not current)
(defmethod compute-picture ((pic flipbook) (seq viewable-sequence))
  (loop for sub-pic in (picture-list pic)
	for sub-vbl in (viewable-list seq)
	for i from 0
	do
	(when (not (current-p sub-pic))
	  (status-message "computing picture, frame ~d ..." i)
	  (without-status-messages (compute-picture sub-pic sub-vbl))
	  (setf (current sub-pic) (current sub-vbl)))
	finally (status-message ""))
  (setf (system-dependent-frob pic)
	(system-dependent-frob (nth (current-picture pic) (picture-list pic)))))

;;;; UGLY ISSUES:
;; 1) changing current-picture must cause change in system-dependent-frob
;; 2) changing zoom, x-offset, y-offset, pane-of must change all sub-pics.

;;; Make sure that panes of sub-pictures are the same as the pane-of
;;; the flipbook -- this is necessary for position-messages, etc.
(defmethod (setf pane-of) :around (pane (pic flipbook))
  (call-next-method)
  (dolist (sub-pic (slot-value pic 'picture-list))
    (setf (slot-value sub-pic 'pane-of) pane)))

;;; Make sure frob is consistent with setting of current picture.
;;; Used in single-stepping.
(defmethod (setf current-picture) :around (pic-num (pic flipbook))
  (call-next-method)
  (with-slots (system-dependent-frob picture-list) pic
    (setf system-dependent-frob
	  (slot-value (nth pic-num picture-list) 'system-dependent-frob))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; *** There's a hairy kludge in this.  Normally, slot-value-dialog
;;; widgets all point to slots in the same object.  Here, some of the
;;; widgets refer to (and point to) the overlay and some to the
;;; sub-picture.  Picture-slot-update-function looks at the car
;;; of the widgets-list to find the picture.  So, the first field in
;;; the dialog must be a slot in the overlay.  Yuch!

;;; Slightly different for flipbooks to include sub-picture parameters
(defmethod make-slot-value-dialog
    ((pic flipbook)
     &key
     (slot-names (visible-slot-names (class-of pic)))
     (label (format nil "slot value dialog"))
     (update-function 'standard-update-function)
     (update-arguments nil)
     (exit-function 'destroying-exit-function))
  (let ((sub-pic (nth (current-picture pic) (picture-list pic))))
    (setq slot-names (union slot-names (settable-parameters sub-pic) :preserve-order t))
    (call-next-method pic
		      :slot-names slot-names
		      :label label
		      :update-function update-function
		      :update-arguments update-arguments
		      :exit-function exit-function)))

;;; Slightly different for flipbooks to make it possible to set slots
;;; of the sub-pictures.
(defmethod make-slot-value-widget ((pic flipbook) slot-name &rest initargs)
  (let* ((slot (find-slot (class-of pic) slot-name t))
	 (type (if slot (CLOS::slot-definition-type slot) t))
	 (sub-pic (nth (current-picture pic) (picture-list pic)))
	 (sub-slot (find-slot (class-of sub-pic) slot-name t))
	 (sub-type (if sub-slot (CLOS::slot-definition-type sub-slot) t)))
    (cond (slot t)
	  (sub-slot
	   (setq slot sub-slot)
	   (setq type sub-type)
	   (setq pic sub-pic))
	  (t (error "~A is not a valid slot-name for ~A" slot-name pic)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Called by reinitialize-instance when making a new picture, or
;;; modifying slots of existing one.  If user sets
;;; :independent-parameters to t, all relevant slots (depending on
;;; type of sub-picture) will be set to be the same as the CURRENT
;;; sub-picture.  If user sets :independent-parameters to nil, all
;;; relevant slots (depending on type of sub-picture) will be
;;; recomputed.  Zoom, x-offset, y-offset must be same in all
;;; sub-pictures.
(defmethod reset-picture-defaults ((pic flipbook) (seq viewable-sequence)
				   &rest initargs &key
				   (pane-of (slot-value pic 'pane-of))
				   (current (slot-value pic 'current))
				   (sub-display-type (slot-value pic 'sub-display-type)
						     sub-display-type-supplied-p)
				   (current-picture (slot-value pic 'current-picture))
				   (picture-list (slot-value pic 'picture-list))
				   (independent-parameters
				    (slot-value pic 'independent-parameters)
				    independent-parameters-supplied-p)
				   &allow-other-keys)
  ;; separate the initarg list:
  (let* ((flipbook-keys (loop with class = (find-class 'flipbook)
			      for slot in (CLOS::class-slots class)
			      for keys = (CLOS::slot-definition-initargs slot)
			      append keys))
	 (flipbook-initargs (apply 'sub-plist initargs flipbook-keys))
	 (sub-initargs (plist-difference initargs flipbook-initargs)) ;the rest
	 (common-parameters (sub-plist initargs :zoom :x-offset :y-offset))
	 (sub-pic (nth current-picture picture-list)))
    ;; Make sure sub-display-type is set
    (when (eq sub-display-type :auto)
      (setq sub-display-type (setf (getf initargs :sub-display-type)
				   (or (display-type (car (viewable-list seq)))
				       (get-default (class-of (car (viewable-list seq))) 'display-type)))))
    (unless sub-display-type (error "Sub-display-type cannot be NIL"))
    (when (eq sub-display-type 'flipbook) (error "Can't display a flipbook of flipbooks"))
    (if (or sub-display-type-supplied-p (null picture-list))
	(progn				;if new sub-display-type, make sub-pictures.
	  (when picture-list  (dolist (sub-pic picture-list) (destroy sub-pic)))
	  (setq picture-list
		(setf (getf initargs :picture-list) ;create picture-list
		      (setf (slot-value pic 'picture-list)
			    (loop for vbl in (viewable-list seq)
				  collect (apply #'make-instance sub-display-type
						 :viewable vbl :pane-of pane-of
						 (append common-parameters sub-initargs))))))
	  (setf sub-pic (car picture-list)
		(getf initargs :current-picture) 0
		;;must be sure these are set in both the flipbook and the sub-pics
		common-parameters (list :zoom (zoom sub-pic)
					:x-offset (x-offset sub-pic)
					:y-offset (y-offset sub-pic)))
	  (if independent-parameters	
	      ;; Force common-parameters to be the same
	      (apply #'reset-compound-picture-defaults
		     sub-pic (viewable sub-pic) pic seq common-parameters)
	      ;; Force recomputation of ALL relavent parameters from defaults
	      (apply #'reset-compound-picture-defaults
		     sub-pic (viewable sub-pic) pic seq
		     :all :defaults (append common-parameters sub-initargs))))
	(progn				;Not making new sub-pictures.
	  ;; If setting to be non-independent, force common slots to match current sub-pic
	  (when (and independent-parameters-supplied-p (null independent-parameters))
	    (setq sub-initargs (append '(:all :current) sub-initargs)))
	  (if independent-parameters
	      (progn (when common-parameters
		       (apply #'reset-compound-picture-defaults
			      sub-pic (viewable sub-pic) pic seq common-parameters)
		       ;; replace common parameters by values computed for sub-pic
		       (setf common-parameters (list :zoom (zoom sub-pic)
						     :x-offset (x-offset sub-pic)
						     :y-offset (y-offset sub-pic))))
		     (when sub-initargs
		       (apply #'reinitialize-instance sub-pic sub-initargs)))
	      (when (or common-parameters sub-initargs)
		(apply #'reset-compound-picture-defaults sub-pic (viewable sub-pic)
		       pic seq (append common-parameters sub-initargs))))))
    (apply #'call-next-method pic seq
	   ;;:current nil			;*** 8/18/92
	   :system-dependent-frob (system-dependent-frob sub-pic)
	   (append common-parameters initargs))))

;;; Called to set the common slots of the sub-pictures.  Keyword :all
;;; indicates that ALL relevant parameters should be reset.  If its
;;; value is :defaults, then they should be set from defaults for the
;;; sub-pic class.  Otherwise, they should be set from the slots of
;;; the current sub-picture.
(defmethod reset-compound-picture-defaults ((sub-pic picture) (sub-vbl viewable)
					    flipbook seq &rest initargs)
  (declare (ignore seq))
  (remf initargs :all)
  (when initargs
    (dolist (sub-pic (picture-list flipbook))
      (apply #'reinitialize-instance sub-pic initargs))))

(defmethod reset-compound-picture-defaults ((sub-pic gray) (sub-vbl viewable)
					    flipbook seq &rest initargs
					    &key all &allow-other-keys)
  (when all				;force scale and pedestal to be recomputed
    (setf (getf initargs :scale)
	  (getf initargs :scale (if (eq all :defaults)
				    (get-default (clos::class-of sub-pic) :scale)
				    (slot-value sub-pic 'scale))))
    (setf (getf initargs :pedestal)
	  (getf initargs :pedestal (if (eq all :defaults)
				       (get-default (clos::class-of sub-pic) :pedestal)
				       (slot-value sub-pic 'pedestal)))))
  (when (eq (getf initargs :scale) :auto)
    (setf (getf initargs :scale) (range seq)))
  (when (eq (getf initargs :pedestal) :auto)
    (setf (getf initargs :pedestal) (minimum seq)))
  (apply #'call-next-method sub-pic sub-vbl flipbook seq initargs))

(defmethod reset-compound-picture-defaults ((sub-pic vector-field) (sub-vbl viewable)
					    flipbook seq &rest initargs
					    &key all &allow-other-keys)
  (when all				;force skip, scale, base-position recomputation
    (setf (getf initargs :skip)
	  (getf initargs :skip (if (eq all :defaults)
				   (get-default (clos::class-of sub-pic) :skip)
				   (slot-value sub-pic 'skip))))
    (setf (getf initargs :scale)
	  (getf initargs :scale (if (eq all :defaults)
				    (get-default (class-of sub-pic) :scale)
				    (slot-value sub-pic 'scale))))
    (setf (getf initargs :base-position)
	  (getf initargs :base-position (if (eq all :defaults)
					    (get-default (class-of sub-pic) :base-position)
					    (slot-value sub-pic 'base-position)))))
  (when (eq (getf initargs :scale) :auto)
    (setf (getf initargs :scale) (max (abs (maximum seq)) (abs (minimum seq)))))
  (apply #'call-next-method sub-pic sub-vbl flipbook seq initargs))


;;; Reset-compound-picture-defaults on graphs, so that defaults work
;;; correctly for flipbook, with independent-parameters nil, and
;;; sub-display-type 'graph.  *** This is not correct, but its closer
;;; than it used to be. -DH 6-12-92
(defmethod reset-compound-picture-defaults ((sub-pic graph) (sub-vbl viewable)
					    flipbook seq &rest initargs)
  ;; :All indicates that all parameters should be reset to those of the sub-pic.
  (when (getf initargs :all)
    (setf (getf initargs :zoom)
	  (getf initargs :zoom (zoom sub-pic)))
    (setf (getf initargs :aspect-ratio)
	  (getf initargs :aspect-ratio (aspect-ratio sub-pic)))
    (setf (getf initargs :x-range)
	  (getf initargs :x-range (x-range sub-pic)))
    (setf (getf initargs :y-range)
	  (getf initargs :y-range (list (minimum seq) (maximum seq))))
    (setf (getf initargs :x-axis)
	  (getf initargs :x-axis :auto))
    (setf (getf initargs :y-axis)
	  (getf initargs :y-axis :auto))
    (setf (getf initargs :x-tick-step)
	  (getf initargs :x-tick-step :auto))
    (setf (getf initargs :y-tick-step)
	  (getf initargs :y-tick-step :auto)))
  (let ((x-range (or (getf initargs :x-range) (x-range sub-pic)))
	(y-range (or (getf initargs :y-range) (y-range sub-pic))))
    (unless (numberp (getf initargs :aspect-ratio))
      (setf (getf initargs :aspect-ratio) (/-0 (y-dim (pane-of flipbook))
					       (x-dim (pane-of flipbook)))))
    (unless (numberp (getf initargs :x-axis))
      (setf (getf initargs :x-axis) (car y-range)))
    (unless (numberp (getf initargs :y-axis))
      (setf (getf initargs :y-axis) (car x-range)))
    (unless (numberp (getf initargs :x-tick-step))
      (setf (getf initargs :x-tick-step) (compute-tick-step x-range)))
    (unless (numberp (getf initargs :y-tick-step))
      (setf (getf initargs :y-tick-step) (compute-tick-step y-range))))
  (apply #'call-next-method sub-pic sub-vbl flipbook seq initargs))

#|
;;; *** Write this
(defmethod reset-compound-picture-defaults ((sub-pic surface-plot) (sub-vbl viewable)
					    flipbook seq &rest initargs))
|#


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
