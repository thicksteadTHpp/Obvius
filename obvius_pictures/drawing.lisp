;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: drawing.lisp
;;;  Author: Simoncelli
;;;  Description: Drawing pictures (vector-fields, graphs, and surface-plots).
;;;  Creation Date: 3/91
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

(export '(make-scatter))

;;; TO BE DONE: - Provide generic graphics-context: system-independent
;;; colors/fonts/line-widths etc.

;;; GRAPHS:
;; - hairy dependencies in reset-picture-defaults!
;; - how can user specify a given grph/pixel relationship (e.g. to compare to graphs).
;; - must abstract out the text sizing stuff (make it work for hardcopy.

;;; SCATTER PLOTS:
;; - Document
;; - Menu-interface

;;; POLAR PLOTS:
;; - Document
;; - Menu-interface
;; - axis labels.

;;; VECTOR FIELDS:
;; - arrow-head-length and base-position are currently ignored.
;; - :density in dialog box.

;;; SURFACE-PLOTS:
;; - Add focal length initarg (partially done).  Allow it to be set in dialog box.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; DRAWING pictures

;;;; A DRAWING is a picture which is constructed primarily by drawing
;;;; lines.  The intermediate representation (frob) is some
;;;; representation that can quickly and easily be redrawn (such as a
;;;; list of endpoint coordinates of lines to be drawn).

(defmacro drawing-p (obj)
  `(typep ,obj 'drawing))

(defmethod settable-parameters ((class-name (eql 'drawing)))
  (append '(color) (call-next-method)))

(defmethod reset-picture-defaults ((pic drawing) (vbl viewable) &rest initargs)
  (when initargs (setf (getf initargs :current) nil))
  (apply #'call-next-method pic vbl initargs))

(defmethod drag-picture ((pic drawing) dy dx)
  (with-slots (y-offset x-offset pane-of zoom) pic
    (if (and dy dx)
	(setf y-offset (+ y-offset dy)  x-offset (+ x-offset dx))
	(setf y-offset 0  x-offset 0))
    (clear pane-of)
    (render pane-of (system-dependent-frob pic) y-offset x-offset zoom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; VECTOR-FIELD pictures on IMAGE-PAIR viewables

#| Test:
(setq foo (make-image-pair (list (make-ramp '(64 64))
				 (make-ramp '(64 64) :orientation (/ pi -2)))
			   :display-type 'vector-field))
|#

;;; *** :density should be in here, as soon as dialog permit it.
(defmethod settable-parameters ((class-name (eql 'vector-field)))
  (append '(skip scale line-width arrow-heads)
	  (call-next-method)))

#|  TOO much magic:
(defmethod set-not-current ((vf vector-field))
  (setf (scale vf) nil)
  (call-next-method))
|#

(defmethod title-bar-string ((pic vector-field))
  (format nil "~S / ~,3,-2G" (name (viewable pic)) (scale pic)))

(defmethod position-message ((pic vector-field) (im image-pair) pane pane-y pane-x)
  (declare (ignore pane))
  (multiple-value-bind (y x)
      (pane-coord-to-viewable-coord pic pane-y pane-x)
    (if (array-in-bounds-p (data (x-component im)) y x)
	(status-message "(~D, ~D):  ~G ~G" y x
			(iref (y-component im) y x) (iref (x-component im) y x))
	(status-message "(~D, ~D): out of bounds" y x))))

(defmethod frob-coord-to-viewable-coord ((pic vector-field) frob-y frob-x)
  (with-slots (skip zoom) pic
    (values (* skip (floor frob-y (* zoom skip)))
	    (* skip (floor frob-x (* zoom skip))))))

(defmethod compute-picture ((vf vector-field) (ip image-pair))
  (with-slots (system-dependent-frob zoom skip scale line-width
	       color  base-position arrow-heads arrow-head-length) vf
    (let ((new-dims (dimensions vf)))
      (setf system-dependent-frob
	    (make-vf-frob (screen-of (pane-of vf)) new-dims
			  :frob system-dependent-frob
			  :vf vf))
      (draw-vector-field system-dependent-frob
			 (data (y-component ip)) (data (x-component ip))
			 skip zoom scale
			 line-width color base-position arrow-heads arrow-head-length
			 (round (* zoom skip 0.5))
			 (round (* zoom skip 0.5))))))

;;; This just holds some auxilliary info for the vector-field which
;;; allows it to be efficiently drawn directly to the pane.
(def-simple-class vf-frob (frob)
  (y0 x0 y1 x1 vf dimensions))

;;; Dummy frob holds data needed to draw the vf to the pane.
(defmethod make-vf-frob ((screen screen) dims &rest initargs &key frob &allow-other-keys)
  (remf initargs :frob)
  (setf (getf initargs :dimensions) dims)
  (cond ((typep frob 'vf-frob)
	 (apply 'reinitialize-instance frob initargs))
	(t
	 (destroy frob)
	 (apply 'make-instance 'vf-frob initargs))))

(defmethod draw-lines ((frob vf-frob) y0 x0 y1 x1 &rest args)
  (declare (ignore args))
  (setf (slot-value frob 'y0) y0
	(slot-value frob 'x0) x0
	(slot-value frob 'y1) y1
	(slot-value frob 'x1) x1))

(defmethod render ((pane pane) (frob vf-frob) y-offset x-offset zoom)
  (declare (ignore zoom))
  (with-slots (y0 x0 y1 x1 vf pane->frob-y pane->frob-x) frob
    (let ((corner-y (+ y-offset (floor (- (y-dim pane) (y-dim frob)) 2)))
	  (corner-x (+ x-offset (floor (- (x-dim pane) (x-dim frob)) 2))))
      (setf (slot-value pane->frob-y 'offset) (- corner-y))
      (setf (slot-value pane->frob-x 'offset) (- corner-x))
      (draw-lines pane y0 x0 y1 x1 :x-offset corner-x :y-offset corner-y
		  :line-width (line-width vf)
		  :foreground (color vf)))))

;;; Compute default bitmap dimensions, skip, scale and zoom
;;; parameters if they are not already set.  
(defmethod reset-picture-defaults ((vf vector-field) (ip image-pair) &rest initargs
				   &key
				   (pane-of (slot-value vf 'pane-of))
				   (zoom (slot-value vf 'zoom) zoom-supplied-p)
				   (skip (slot-value vf 'skip) skip-supplied-p)
				   (scale (slot-value vf 'scale) scale-supplied-p)
				   (density (get-default 'vector-field :density)
					    density-supplied-p))
  (remf initargs :density)
  (let* ((ip-dims (dimensions ip))
	 ;;first get approximate vf size
	 (vf-dims (cond ((numberp zoom) (mapcar #'(lambda (d) (round (* zoom d))) ip-dims))
			((eq zoom :auto) (dimensions pane-of))
			((consp zoom) zoom)
			(t ip-dims))))	;natural size is size of image-pair...
    (when (or zoom-supplied-p scale-supplied-p skip-supplied-p density-supplied-p)
      (cond ((or (and density-supplied-p (not skip-supplied-p))
		 (eq skip :auto))
	     (setq skip (setf (getf initargs :skip)
		   ;;number of vectors prop. to sqrt vf-size
		   (apply #'max 1 (mapcar #'(lambda (ip-dim vf-dim)
					      (ceiling ip-dim (* (sqrt vf-dim) density)))
					  ip-dims vf-dims)))))
	    (skip-supplied-p (setq skip (setf (getf initargs :skip) (round skip)))))
      (cond ((numberp zoom) zoom)	;ok
	    ((or (eq zoom :auto) (num-list-2-p zoom))
	     (let* ((samples (mapcar #'(lambda (dim) (ceiling dim skip))
				     ip-dims))
		    (box-sz (apply 'min (mapcar #'(lambda (dim num) (floor dim num))
						vf-dims samples))))
	       (setq vf-dims (mapcar #'(lambda (num) (* num box-sz)) samples))
	       (setq zoom (setf (getf initargs :zoom) (/ box-sz skip)))))
	    (t (setq zoom (setf (getf initargs :zoom) 1))))
      (unless (numberp scale)
	(setq scale (setf (getf initargs :scale)
	      (loop with data1 = (data (first-image ip))
		    with data2 = (data (second-image ip))
		    for j from 0 below (y-dim ip) by skip
		    maximize
		    (loop for i from 0 below (x-dim ip) by skip
			  maximize
			  (max (abs (aref data1 j i))  (abs (aref data2 j i)))))))))
    (setf (getf initargs :dimensions) vf-dims)
    (apply #'call-next-method vf ip initargs)))

;;; Plot a vector field, using draw-lines commands to the frob.
;;; arrow-heads is either nil (don't draw them) or a number which is used
;;; as a threshold.  base-position determines where the coordinate origin of the
;;; vector lies.  0 => foot, 1 => head.
;;; *** base-position,  arrow-head-length currently ignored...
(defmethod draw-vector-field
    ((frob t) y-array x-array skip zoom scale line-width color
     base-position arrow-heads arrow-head-length y-offset x-offset)
  (declare (ignore base-position arrow-head-length))
  (let* ((num-lines (* (if arrow-heads 3 1)
		       (ceiling (y-dim x-array) skip)
		       (ceiling (x-dim x-array) skip)))
	 (x0 (make-array num-lines :element-type 'fixnum :initial-element 0))
	 (x1 (make-array num-lines :element-type 'fixnum :initial-element 0))
	 (y0 (make-array num-lines :element-type 'fixnum :initial-element 0))
	 (y1 (make-array num-lines :element-type 'fixnum :initial-element 0))
	 (base-skip (round (* zoom skip)))
	 (count 0))
    (declare (type (array fixnum (*)) x0 x1 y0 y1))
    (setq scale (/-0 (* zoom skip) scale 0.0))
    (if (null arrow-heads)
	(loop initially (setq scale (/ scale 2.0))
	      for y from 0 below (y-dim x-array) by skip
	      for y-base from y-offset by base-skip
	      do
	      (loop for x from 0 below (x-dim x-array) by skip
		    for x-base from x-offset by base-skip
		    for xcomp = (aref x-array y x)
		    for ycomp = (aref y-array y x)
		    for xvec = (round (* xcomp scale))
		    for yvec = (round (* ycomp scale))
		    do
		    (setf (aref x0 count) (- x-base xvec) (aref y0 count) (- y-base yvec)
			  (aref x1 count) (+ x-base xvec) (aref y1 count) (+ y-base yvec))
		    (incf count)))
	(loop with arrow-norm = (/ arrow-heads (sqrt 2))
	      for y from 0 below (y-dim x-array) by skip
	      for y-base from y-offset by base-skip
	      do
	      (loop for x from 0 below (x-dim x-array) by skip
		    for x-base from x-offset by base-skip
		    for xcomp = (aref x-array y x)
		    for ycomp = (aref y-array y x)
		    for xvec = (round (* xcomp scale))
		    for yvec = (round (* ycomp scale))
		    for xdraw1 = (+ x-base xvec)
		    for ydraw1 = (+ y-base yvec)
		    for mag = (sqrt (+ (* xvec xvec) (* yvec yvec)))
		    for xnorm = (round (* arrow-norm (/-0 xvec mag 0.0)))
		    for ynorm = (round (* arrow-norm (/-0 yvec mag 0.0)))
		    do
		    (setf (aref x0 count) x-base (aref y0 count) y-base
			  (aref x1 count) xdraw1 (aref y1 count) ydraw1)
		    (incf count)
		    (when (> mag arrow-norm)
		      (setf (aref x0 count) xdraw1 (aref y0 count) ydraw1
			    (aref x1 count) (- xdraw1 xnorm (- ynorm))
			    (aref y1 count) (- ydraw1 xnorm ynorm)))
		    (incf count)
		    (when (> mag arrow-norm)
		      (setf (aref x0 count) xdraw1 (aref y0 count) ydraw1
			    (aref x1 count) (- xdraw1 xnorm ynorm)
			    (aref y1 count) (- ydraw1 (- xnorm) ynorm)))
		    (incf count))))
    (draw-lines frob y0 x0 y1 x1 :line-width line-width :foreground color)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; GRAPH pictures (holds parameters, captures complex defaulting behavior)

(defmethod settable-parameters ((class-name (eql 'graph)))
  (append '(graph-type y-axis-type y-range x-range
	    line-width y-axis x-axis y-tick-step x-tick-step
	    aspect-ratio axis-color
	    plot-symbol fill-symbol-p symbol-size)
	  (call-next-method)))

(defmethod set-not-current ((pic graph))
  (with-slots (reinit-args) pic
    (setf (getf reinit-args :y-range) (get-default 'graph 'y-range))
    (call-next-method)))

(defmethod zoom-picture ((graph graph) factor y x)
  (declare (ignore y x))
  (reinitialize-instance graph :zoom (* factor (slot-value graph 'zoom)))
  (draw-pane (pane-of graph)))

(defmethod position-message ((pic graph) vbl pane pane-y pane-x)
  (declare (ignore vbl pane))
  (multiple-value-bind (y x)
      (pane-coord-to-viewable-coord pic pane-y pane-x)
    (status-message "(~D, ~D)" y x)))

(defmethod frob-coord-to-viewable-coord ((pic graph) frob-y frob-x)
  (with-slots (frob->vbl-y frob->vbl-x) pic
    (values (transform-point frob->vbl-y frob-y)
	    (transform-point frob->vbl-x frob-x))))

;;; Be sure to set up all of the slots that have default-initargs of
;;; :auto here.  Assumes :x-range and :y-range have been set.
(defmethod reset-picture-defaults ((graph graph) (vbl viewable) &rest initargs
				   &key
				   (pane-of (slot-value graph 'pane-of))
				   (zoom nil zoom-supplied-p)
				   (aspect-ratio (slot-value graph 'aspect-ratio)
						 aspect-ratio-supplied-p)
				   (x-axis nil x-axis-supplied-p)
				   (y-axis nil y-axis-supplied-p)
				   (x-range (slot-value graph 'x-range) x-range-supplied-p)
				   (y-range (slot-value graph 'y-range) y-range-supplied-p)
				   (x-tick-step (slot-value graph 'x-tick-step))
				   (y-tick-step (slot-value graph 'y-tick-step))
				   x-tick-format-string y-tick-format-string
				   (graph->data-x (slot-value graph 'graph->data-x))
				   (graph->data-y (slot-value graph 'graph->data-y))
				   (y-max-label-length (slot-value graph 'y-max-label-length))
				   (x-max-label-length (slot-value graph 'x-max-label-length))
                                   )
  ;; Make sure transforms are filled in (may have been done by more specialized methods).
  (unless (typep graph->data-x 'transform)
    (setq graph->data-x (setf (slot-value graph 'graph->data-x)
	  (make-transform :coerce #'round))))
  (unless (typep graph->data-y 'transform)
    (setq graph->data-y (setf (slot-value graph 'graph->data-y)
	  (make-transform))))
  (when zoom-supplied-p
    (setq zoom
	  (cond ((numberp zoom) zoom)
		((eq zoom :auto)	;*** Magic number 50 for "typical" x-axis labels
		 (/ (- (x-dim pane-of) 50)
		    (transformed-distance graph->data-x x-range)))
		((num-list-2-p zoom)
		 (unless aspect-ratio-supplied-p
		   (setf (getf initargs :aspect-ratio) (apply '/-0 zoom)))
		 (/ (* 0.9 (cadr zoom)) (total-size vbl)))
		(t (floor (* (x-dim pane-of)) (x-dim vbl)))))
    (setf (getf initargs :zoom) (if (> zoom 1) (floor zoom) (/ (ceiling (/ zoom))))))
  (unless (numberp aspect-ratio)	; :auto will match pane's dims
    (setf (getf initargs :aspect-ratio)
	  (/-0 (y-dim pane-of) (x-dim pane-of))))
  (when (or (and x-axis-supplied-p x-axis (not (numberp x-axis)))
	    (and (not x-axis-supplied-p) y-range-supplied-p))
    (setf (getf initargs :x-axis) (car y-range)))
  (when (or (and y-axis-supplied-p y-axis (not (numberp y-axis)))
	    (and (not y-axis-supplied-p) x-range-supplied-p))
    (setf (getf initargs :y-axis) (car x-range)))
  (when (and x-axis-supplied-p x-axis (not y-range-supplied-p))
    (setq y-range (setf (getf initargs :y-range)
	  (list (min x-axis (car y-range)) (max x-axis (cadr y-range))))))
  (when (and y-axis-supplied-p y-axis (not x-range-supplied-p))
    (setq x-range (setf (getf initargs :x-range)
	  (list (min y-axis (car x-range)) (max y-axis (cadr x-range))))))
  ;; Set up tick steps and format strings.
  (unless (numberp x-tick-step)
    (setq x-tick-step (setf (getf initargs :x-tick-step)
	  (compute-tick-step x-range))))
  (unless (numberp y-tick-step)
    (setq y-tick-step (setf (getf initargs :y-tick-step)
	  (compute-tick-step y-range))))
  (let ((left-tick (* x-tick-step (ceiling (car x-range) x-tick-step)))
	(right-tick (* x-tick-step (floor (cadr x-range) x-tick-step)))
	(bottom-tick (* y-tick-step (ceiling (car y-range) y-tick-step)))
	(top-tick (* y-tick-step (floor (cadr y-range) y-tick-step))))
    (unless y-tick-format-string
      (setf (getf initargs :y-tick-format-string)
	    (compute-tick-format-string bottom-tick top-tick y-tick-step y-max-label-length)))
    (unless x-tick-format-string
      (setf (getf initargs :x-tick-format-string)
	    (compute-tick-format-string left-tick right-tick x-tick-step x-max-label-length))))
  (when initargs (setf (getf initargs :current) nil)) ;re-compute picture
  (apply #'call-next-method graph vbl initargs))

(defmethod reset-picture-defaults ((graph graph) (im image) &rest initargs
				   &key
				   (y-range (slot-value graph 'y-range))
				   (x-range (slot-value graph 'x-range)))
  (unless (num-list-2-p y-range)
    (setf (getf initargs :y-range) (list (minimum im) (maximum im))))
  (unless (num-list-2-p x-range)
    (setf (getf initargs :x-range) (list 0 (1- (total-size (data im))))))
  (apply #'call-next-method graph im initargs))

(defmethod reset-picture-defaults ((graph graph) (filt filter) &rest initargs
				   &key
				   (y-range (slot-value graph 'y-range))
				   (x-range (slot-value graph 'x-range))
				   (x-axis nil x-axis-supplied-p )
				   (graph-type nil graph-type-supplied-p))
  (unless graph-type-supplied-p
    (setf (getf initargs :graph-type) :bar))
  (unless (and x-axis-supplied-p (numberp x-axis))
    (setf (getf initargs :x-axis) (setq x-axis 0.0))) ;defaults to 0.0
  (unless (num-list-2-p y-range)
    (setf (getf initargs :y-range) (list (min (minimum filt) x-axis)
					 (max (maximum filt) x-axis))))
  (unless (num-list-2-p x-range)
    (setf (getf initargs :x-range) (list -1/6 (+ (total-size filt) -1 1/6))))
  (apply #'call-next-method graph filt initargs))

(defmethod reset-picture-defaults ((graph graph) (df discrete-function) &rest initargs
				   &key
				   (y-range (slot-value graph 'y-range))
				   (x-range (slot-value graph 'x-range))
				   (graph->data-x (slot-value graph 'graph->data-x)
						  graph->data-x-supplied-p))
  (with-slots (origin increment) df
    (unless (num-list-2-p y-range)
      (setf (getf initargs :y-range) (list (minimum df) (maximum df))))
    (unless (num-list-2-p x-range)
      (setf (getf initargs :x-range)
	    (list origin (+ origin (* increment (1- (total-size df)))))))
    (unless graph->data-x-supplied-p	;allow user to specify alternative
      (setf (getf initargs :graph->data-x)
	    (make-inverse-transform :scale increment :offset origin :coerce #'round)))
    (apply #'call-next-method graph df initargs)))

(defmethod reset-picture-defaults ((graph graph) (hist histogram) &rest initargs
				   &key
				   (x-range (slot-value graph 'x-range))
				   (graph-type nil graph-type-supplied-p))
  (unless graph-type-supplied-p (setf (getf initargs :graph-type) :bar))
  (unless (num-list-2-p x-range)
    (setf (getf initargs :x-range)
	  (list (origin hist) (+ (origin hist) (* (increment hist) (total-size hist))))))
  (setq initargs (apply #'call-next-method graph hist initargs))
  initargs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compute-picture ((graph graph) (im image))
  (compute-picture graph (data im)))

(defmethod compute-picture ((graph graph) (df discrete-function))
  (compute-picture graph (data df)))

(defmethod compute-picture ((graph graph) (filt filter))
  (compute-picture graph (kernel filt)))

;;; Called to compute the system-dependent intermediate picture
;;; representation.  Creates a dummy frob to hold some info necessary
;;; to draw directly to the pane.  Assumes the slots of the graph are
;;; completely set up.
(defmethod compute-picture ((graph graph) (data array))
  (with-slots (system-dependent-frob pane-of bounding-region graph-type
	       y-range x-range y-axis x-axis y-tick-step x-tick-step
	       y-tick-length x-tick-length y-tick-gap x-tick-gap
	       y-tick-format-string x-tick-format-string
	       y-label x-label axis-color color line-width
	       plot-symbol fill-symbol-p symbol-size) graph
    (multiple-value-bind (graph->frob-y graph->frob-x data->frob-y data->frob-x dims)
	(compute-graph-transforms graph)
      (setf system-dependent-frob
	    (make-graph-frob
	     (screen-of pane-of) dims
	     :frob system-dependent-frob
	     :graph graph :data data
	     :graph->frob-y graph->frob-y :graph->frob-x graph->frob-x
	     :data->frob-y data->frob-y :data->frob-x data->frob-x
	     )))))

;;; Generic frob holds some auxilliary info for the graph which allows
;;; it to be efficiently drawn directly to the pane.
(def-simple-class graph-frob (frob)
  (data graph dimensions
   graph->frob-y graph->frob-x data->frob-y data->frob-x))

;;; Dummy frob holds transforms needed to draw the graph to the pane
;;; and the data array.
(defmethod make-graph-frob ((screen screen) dims &rest initargs &key frob &allow-other-keys)
  (remf initargs :frob)
  (setf (getf initargs :dimensions) dims)
  (cond ((typep frob 'graph-frob)
	 (apply 'reinitialize-instance frob initargs))
	(t
	 (destroy frob)
	 (apply 'make-instance 'graph-frob initargs))))

(defmethod render ((pane pane) (frob graph-frob) y-offset x-offset zoom)
  (declare (ignore zoom))
  (with-slots (data graph pane->frob-y pane->frob-x
		    graph->frob-y graph->frob-x data->frob-y data->frob-x) frob
    (with-slots (graph-type y-range x-range y-axis x-axis y-tick-step x-tick-step
			 y-tick-length x-tick-length y-tick-gap x-tick-gap
			 y-tick-format-string x-tick-format-string y-label x-label
			 color axis-color line-width
			 plot-symbol fill-symbol-p symbol-size) graph
      (let ((corner-y (+ y-offset (floor (- (y-dim pane) (y-dim frob)) 2)))
	    (corner-x (+ x-offset (floor (- (x-dim pane) (x-dim frob)) 2))))
	(setf (slot-value pane->frob-y 'offset) (- corner-y))
	(setf (slot-value pane->frob-x 'offset) (- corner-x))
	(translate-transform! graph->frob-y corner-y)
	(translate-transform! graph->frob-x corner-x)
	(translate-transform! data->frob-y corner-y)
	(translate-transform! data->frob-x corner-x)
	(unwind-protect
	     (progn
	       (draw-graph pane data graph-type
			       graph->frob-y graph->frob-x data->frob-y data->frob-x
			       y-range x-range y-axis x-axis y-tick-step x-tick-step
			       y-tick-length x-tick-length y-tick-gap x-tick-gap
			       y-tick-format-string x-tick-format-string y-label x-label
			       :color color :axis-color axis-color :line-width line-width
			       :plot-symbol plot-symbol :fill-symbol-p fill-symbol-p
			       :symbol-size symbol-size))
	  (translate-transform! graph->frob-y (- corner-y))
	  (translate-transform! graph->frob-x (- corner-x))
	  (translate-transform! data->frob-y (- corner-y))
	  (translate-transform! data->frob-x (- corner-x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SURFACE-PLOT pictures.

;; *** :focal-length should be in the slot dialog box,
;;; but this currently does not work
(defmethod settable-parameters ((class-name (eql 'surface-plot)))    
  (append '(r theta phi projection x-step y-step z-size
	    box-p box-color line-width)
	  (call-next-method)))

(defmethod title-bar-string ((pic surface-plot))
  (format nil "Plot of ~S" (name (viewable pic))))

(defmethod focal-length ((pic surface-plot))
  (with-slots (r zoom) pic
    (* r zoom)))

;;; Reset parameters on a surface plot
;;; Note that r, zoom, and focal-length are redundant
(defmethod reset-picture-defaults ((pic surface-plot) (im image) &rest initargs
				   &key
				   (pane-of (slot-value pic 'pane-of))
				   (zoom nil zoom-supplied-p)
				   (theta nil theta-supplied-p)
				   (phi nil phi-supplied-p)
				   (x-step nil x-step-supplied-p)
				   (y-step nil y-step-supplied-p)
				   (z-size (slot-value pic 'z-size) z-size-supplied-p)
				   ;;(focal-length nil fl-supplied-p)
				   (r nil r-supplied-p))
  (when zoom-supplied-p
    (setf (getf initargs :zoom)
	  (cond ((and (numberp zoom) (> zoom 0.0))  zoom)
		((eq zoom :auto)	;auto-zoom to size of pane
		 (loop for pane-dim in (dimensions pane-of)
		       for pic-dim in (dimensions im)
		       minimize (/ pane-dim pic-dim (sqrt 2))))
		((num-list-2-p zoom)
		 (apply 'min (mapcar #'(lambda (zoom-dim pic-dim) (/ zoom-dim pic-dim))
				     zoom (dimensions im))))
		(t 1))))
  (when (and z-size-supplied-p (not (numberp z-size)))
    (setq z-size (setf (getf initargs :z-size) 0.5)))
  (when r-supplied-p
    (let ((corner-dist (sqrt (+ (sqr (/ (1- (x-dim im)) 2))
				(sqr (/ (1- (y-dim im)) 2))
				(sqr (* z-size (avg (dimensions im))))))))
      (setf (getf initargs :r)
	    (if (numberp r)
		(max r corner-dist)	;Camera can't be inside the graph "box".
		(* 2.0 corner-dist)))))
#|;; focal length stuff:
  (setq zoom (getf initargs :zoom (slot-value pic 'zoom)))
  (setq r (getf initargs :r (slot-value pic 'r)))
  (setf (getf initargs :focal-length)
	(cond ((and fl-supplied-p (numberp focal-length) (> focal-length 0.0)
		    (not (and zoom-supplied-p r-supplied-p)))
	       (cond (zoom-supplied-p
		      (setf (getf initargs :r) (/ focal-length zoom)))
		     (r-supplied-p
		      (setf (getf initargs :zoom) (/ focal-length r)))
		     (t
		      (setf (getf initargs :zoom) (/ focal-length r))))
	       focal-length)
	      (t (* r zoom))))
|#
  (when (and theta-supplied-p (not (numberp theta)))
    (setf (getf initargs :theta) (get-default 'surface-plot 'theta)))
  (when (and phi-supplied-p (not (numberp phi)))
    (setf (getf initargs :phi) (get-default 'surface-plot 'phi)))
  (when x-step-supplied-p
    (setf (getf initargs :x-step)
	  (if (and (numberp x-step) (> x-step 0))
	      (round x-step)
	      (max 1 (round (/ (x-dim im) 20))))))
  (when y-step-supplied-p
    (setf (getf initargs :y-step)
	  (if (and (numberp y-step) (> y-step 0))
	      (round y-step)
	      (max 1 (round (/ (y-dim im) 20))))))
  (apply #'call-next-method pic im initargs))


(defmethod compute-picture ((pic surface-plot) (im image))
  (with-slots (current r theta projection phi x-step y-step z-size
		       box-color box-p x-offset y-offset zoom
		       line-width color
		       system-dependent-frob) pic
    (let* ((x-dim (ceiling (x-dim im) x-step))
	   (pane (pane-of pic))
	   (y-dim (ceiling (y-dim im) y-step))
	   (corners-x (make-array 8 :element-type 'single-float))
	   (corners-y (make-array 8 :element-type 'single-float))
	   (x-array (make-array (list y-dim x-dim) :element-type 'single-float))
	   (y-array (make-array (list y-dim x-dim) :element-type 'single-float))
	   (z-org (mean im))
	   (z-scale (/-0 (* z-size (avg (dimensions im))) (range im)))
	   (z-min (minimum im))
	   (z-max (maximum im))
	   (bottom (make-array '(2 2) :element-type 'single-float
			       :initial-element z-min))
	   (top    (make-array '(2 2) :element-type 'single-float
			       :initial-element z-max))
	   (box-xform (compute-transform-matrix r theta phi (* r zoom)
						(* (1- x-dim) x-step) 0.5
						(* (1- y-dim) y-step) 0.5
						z-scale z-org))
	   bound-x bound-y xform
	   min-x min-y)

      ;; do the box corners
      (transform-nodes top 2 2 corners-y corners-x 1 1 box-xform
		       :projection projection)
      (transform-nodes bottom 2 2
		       (make-array 4 :element-type 'single-float
				   :displaced-to corners-y
				   :displaced-index-offset 4)
		       (make-array 4 :element-type 'single-float
				   :displaced-to corners-x
				   :displaced-index-offset 4)
		       1 1 box-xform :projection projection)
      (setq min-x (floor (minimum corners-x))
	    min-y (floor (minimum corners-y)))
      (setq bound-x (- (floor (maximum corners-x)) min-x -1)
	    bound-y (- (floor (maximum corners-y)) min-y -1))
      (dotimes (i 8) (decf (aref corners-y i) min-y))
      (dotimes (i 8) (decf (aref corners-x i) min-x))
      ;; initial transform matrix
      (setq xform (compute-transform-matrix r theta phi
					    (* r zoom)
					    1
					    (/ (1- (x-dim im)) 2)
					    -1
					    (/ (1- (y-dim im)) 2)
					    z-scale z-org))
      ;; do a 2-d post-translation, this is transform-dependent
      (setq xform
	    (cond ((eq projection :perspective)
		   (matrix-mul
		    (array-from-list `((1.0 0.0 ,(float (- min-x)))
				       (0.0 1.0 ,(float (- min-y)))
				       (0.0 0.0 1.0))
				     :element-type 'single-float)
		    xform))
		  ((eq projection :orthographic)
		   (add
		    (array-from-list `((0.0 0.0 0.0 ,(float (- min-x)))
				       (0.0 0.0 0.0 ,(float (- min-y)))
				       (0.0 0.0 0.0 0.0))
				     :element-type 'single-float)
		    xform))))
      (setf system-dependent-frob
	    (make-sp-frob (screen-of pane) (list bound-x bound-y)
			  :frob system-dependent-frob))
      ;; calculate and draw the surface
      (transform-nodes (data im) (y-dim im) (x-dim im)
		       y-array x-array y-step x-step xform
		       :projection projection)
      (draw-surface system-dependent-frob
		    y-array x-array
		    color line-width
		    (mod theta 2-pi))
      (when box-p			; draw the box
	(let ((box-y0 (make-array 12 :element-type 'fixnum))
	      (box-x0 (make-array 12 :element-type 'fixnum))
	      (box-y1 (make-array 12 :element-type 'fixnum))
	      (box-x1 (make-array 12 :element-type 'fixnum)))
	  (loop for from-index in '(0 1 3 2  0 1 2 3  4 5 7 6)
		for to-index in   '(1 3 2 0  4 5 6 7  5 7 6 4)
		for i from 0
		do (setf (aref box-y0 i) (floor (aref corners-y from-index))
			 (aref box-x0 i) (floor (aref corners-x from-index))
			 (aref box-y1 i) (floor (aref corners-y to-index))
			 (aref box-x1 i) (floor (aref corners-x to-index))))
	  (draw-lines system-dependent-frob box-y0 box-x0 box-y1 box-x1
		      :foreground box-color))))))

(defun avg (list) (/ (apply #'+ list) (list-length list)))

;;; sp-frob, conses up multiple line drawing commands
(defmethod make-sp-frob ((screen t) dims &rest initargs
			 &key frob &allow-other-keys)
  (remf initargs :frob)
  (setf (getf initargs :dimensions) dims)
  (cond ((typep frob 'sp-frob)
	 (setf (y0-list frob) nil
	       (x0-list frob) nil
	       (y1-list frob) nil
	       (x1-list frob) nil
	       (foreground-list frob) nil
	       (line-width-list frob) nil)
	 (apply 'reinitialize-instance frob initargs))
	(t
	 (destroy frob)
	 (apply 'make-instance 'sp-frob initargs))))

;;; Dummy frob caches the lines to be drawn
(def-simple-class sp-frob (frob)
  ((y0-list :initform nil)
   (x0-list :initform nil)
   (y1-list :initform nil)
   (x1-list :initform nil)
   (foreground-list :initform nil)
   (line-width-list :initform nil)
   dimensions))

(defmethod draw-lines ((frob sp-frob) y0 x0 y1 x1
		       &key
		       foreground line-width)
  (with-slots (y0-list x0-list y1-list x1-list
	       foreground-list line-width-list) frob
    (setf y0-list (append y0-list (list y0))
	  x0-list (append x0-list (list x0))
	  y1-list (append y1-list (list y1))
	  x1-list (append x1-list (list x1))
	  foreground-list (append foreground-list (list foreground))
	  line-width-list (append line-width-list (list line-width)))))
			 
;;; render the frob
(defmethod render ((pane pane) (frob sp-frob) y-offset x-offset zoom)
  (declare (ignore zoom))
  (with-slots (y0-list x0-list y1-list x1-list
	       foreground-list line-width-list
	       pane->frob-y pane->frob-x) frob
    (let ((corner-y (+ y-offset (floor (- (y-dim pane) (y-dim frob)) 2)))
	  (corner-x (+ x-offset (floor (- (x-dim pane) (x-dim frob)) 2))))
      (setf (slot-value pane->frob-y 'offset) (- corner-y)
	    (slot-value pane->frob-x 'offset) (- corner-x))
      (loop for y0 in y0-list
	    for x0 in x0-list
	    for y1 in y1-list
	    for x1 in x1-list
	    for foreground in foreground-list
	    for line-width in line-width-list
	    do 
	    (draw-lines pane y0 x0 y1 x1 :x-offset corner-x :y-offset corner-y
			:line-width line-width :foreground foreground)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Polar Plot Pictures

(defmethod settable-parameters ((class-name (eql 'polar-plot)))    
  (append '(maximum line-width plot-symbol fill-symbol-p symbol-size)  (call-next-method)))

(defmethod title-bar-string ((pic polar-plot))
  (format nil "~S" (name (viewable pic))))

(defmethod position-message ((pic polar-plot) (vbl t)
			     pane pane-y pane-x)
  (declare (ignore pane))
  (multiple-value-bind (y x)
      (pane-coord-to-viewable-coord pic pane-y pane-x)
    (status-message "(~D, ~D)" y x)))

(defmethod frob-coord-to-viewable-coord ((pic polar-plot) frob-y frob-x)
  (let* ((dimensions (list-of-length 2 (floor (* 2 (zoom pic) (maximum pic)))))
	 (center-x (floor (cadr dimensions) 2))
	 (center-y (floor (car dimensions) 2))
	 (x (/ (- frob-x center-x) (zoom pic)))
	 (y (/ (- frob-y center-y) (zoom pic)))
	 (mag (sqrt (+ (sqr x) (sqr y))))
	 (angle (if (and (zerop x) (zerop y))
		    0.0
		    (atan y x))))
    (values angle mag)))

(defmethod compute-picture ((pic polar-plot) (df discrete-function))
  (let* ((new-dims (list-of-length 2 (floor (* 2 (zoom pic) (maximum pic))))))
    (setf (system-dependent-frob pic)
	  (make-polar-plot-frob (screen-of (pane-of pic)) new-dims
				:frob (system-dependent-frob pic)
				:polar-plot pic))))

(defmethod compute-picture ((pic polar-plot) (pair viewable-sequence))
  (unless (= (length (viewable-list pair)) 2)
    (error "Can't polar plot: ~A is not a viewable pair" pair))
  (unless (or (and (discrete-function-p (first (viewable-list pair)))
		   (discrete-function-p (second (viewable-list pair))))
	      (and (one-d-image-p (first (viewable-list pair)))
		   (one-d-image-p (second (viewable-list pair)))))
    (error "Can't polar plot: ~A must be one-d-images or discrete-functions" pair))
  (check-size (first (viewable-list pair)) (second (viewable-list pair)))
  (let* ((new-dims (list-of-length 2 (floor (* 2 (zoom pic) (maximum pic))))))
    (setf (system-dependent-frob pic)
	  (make-polar-plot-frob (screen-of (pane-of pic)) new-dims
				:frob (system-dependent-frob pic)
				:polar-plot pic))))

(defmethod reset-picture-defaults
    ((pic polar-plot) (vbl viewable)
     &rest initargs)
  (when (eq (getf initargs :zoom) :auto)
    (setf (getf initargs :zoom) (dimensions (pane-of pic))))
  (when (num-list-2-p (getf initargs :zoom))
    (setf (getf initargs :zoom) (/ (apply 'min (getf initargs :zoom))
				   (getf initargs :maximum) 2)))
  (apply #'call-next-method pic vbl initargs))

(defmethod reset-picture-defaults
    ((pic polar-plot) (df discrete-function)
     &rest initargs)
  (when (eq (getf initargs :maximum) :auto)
    (setf (getf initargs :maximum) (maximum df)))
  (apply #'call-next-method pic df initargs))

(defmethod reset-picture-defaults
    ((pic polar-plot) (pair viewable-sequence)
     &rest initargs)
  (when (eq (getf initargs :maximum) :auto)
    (setf (getf initargs :maximum) (maximum (first (viewable-list pair)))))
  (apply #'call-next-method pic pair initargs))

(defmethod draw-polar-plot ((frob t) data origin increment
			    scale maximum line-width color
			    plot-symbol fill-symbol-p symbol-size
			    yoff xoff)
  (let* ((size (total-size data))
	 (initial-x (floor (+ (* scale (aref data 0) (cos origin)) xoff)))
	 (initial-y (floor (+ (* scale (aref data 0) (sin origin)) yoff)))
	 (x0 (make-array size :element-type 'fixnum :initial-element initial-x))
	 (x1 (make-array size :element-type 'fixnum :initial-element initial-x))
	 (y0 (make-array size :element-type 'fixnum :initial-element initial-y))
	 (y1 (make-array size :element-type 'fixnum :initial-element initial-y)))
    (declare (type (array fixnum (*)) x0 x1 y0 y1))
    (loop for i from 1 below size
	  for previous-angle = (+ (- origin increment) (* i increment))
	  for current-angle = (+ origin (* i increment))
	  for previous-mag = (aref data (mod (1- i) size))
	  for current-mag = (aref data i)
	  do
	  (setf (aref x0 i) (floor (+ (* scale previous-mag (cos previous-angle)) xoff)))
	  (setf (aref y0 i) (floor (+ (* scale previous-mag (sin previous-angle)) yoff)))
	  (setf (aref x1 i) (floor (+ (* scale current-mag (cos current-angle)) xoff)))
	  (setf (aref y1 i) (floor (+ (* scale current-mag (sin current-angle)) yoff)))
	  ;;(print-db previous-angle previous-mag current-angle current-mag)
	  ;;(print-db (aref x0 i) (aref y0 i) (aref x1 i) (aref y1 i))
	  )
    (draw-polar-plot-axes frob maximum scale xoff yoff line-width color)
    (draw-symbols frob y1 x1 :foreground color :line-width line-width
		  :plot-symbol plot-symbol :fill-symbol-p fill-symbol-p :symbol-size symbol-size)
    (draw-lines frob y0 x0 y1 x1 :line-width line-width :foreground color)))

(defmethod draw-polar-plot-pair ((frob t) angle-data mag-data
				 scale maximum line-width color
				 plot-symbol fill-symbol-p symbol-size
				 yoff xoff)
  (let* ((size (total-size angle-data))
	 (initial-x (floor (+ (* scale (aref mag-data 0) (cos (aref angle-data 0))) xoff)))
	 (initial-y (floor (+ (* scale (aref mag-data 0) (sin (aref angle-data 0))) yoff)))
	 (x0 (make-array size :element-type 'fixnum :initial-element initial-x))
	 (x1 (make-array size :element-type 'fixnum :initial-element initial-x))
	 (y0 (make-array size :element-type 'fixnum :initial-element initial-y))
	 (y1 (make-array size :element-type 'fixnum :initial-element initial-y)))
    (declare (type (array fixnum (*)) x0 x1 y0 y1))
    (loop for i from 1 below size
	  for previous-angle = (aref angle-data (1- i))
	  for current-angle = (aref angle-data i)
	  for previous-mag = (aref mag-data (1- i))
	  for current-mag = (aref mag-data i)
	  do
	  (setf (aref x0 i) (floor (+ (* scale previous-mag (cos previous-angle)) xoff)))
	  (setf (aref y0 i) (floor (+ (* scale previous-mag (sin previous-angle)) yoff)))
	  (setf (aref x1 i) (floor (+ (* scale current-mag (cos current-angle)) xoff)))
	  (setf (aref y1 i) (floor (+ (* scale current-mag (sin current-angle)) yoff)))
	  ;;(print-db previous-angle previous-mag current-angle current-mag)
	  ;;(print-db (aref x0 i) (aref y0 i) (aref x1 i) (aref y1 i))
	  )
    (draw-polar-plot-axes frob maximum scale xoff yoff line-width color)
    (draw-symbols frob y0 x0 :foreground color :line-width line-width
		  :plot-symbol plot-symbol :fill-symbol-p fill-symbol-p :symbol-size symbol-size)
    (draw-lines frob y0 x0 y1 x1 :line-width line-width :foreground color)))

(defun draw-polar-plot-axes (frob max-mag scale xoff yoff line-width color)
  (let ((hor-from-y (floor yoff)) (hor-from-x (floor (+ (* scale (- max-mag)) xoff)))
	(hor-to-y (floor yoff)) (hor-to-x (floor (+ (* scale max-mag) xoff)))
	(ver-from-y (floor (+ (* scale (- max-mag)) yoff))) (ver-from-x (floor xoff))
	(ver-to-y (floor (+ (* scale max-mag) yoff))) (ver-to-x (floor xoff)))
    (draw-line frob hor-from-y hor-from-x hor-to-y hor-to-x
	       :line-width line-width :foreground color)
    (draw-line frob ver-from-y ver-from-x ver-to-y ver-to-x
	       :line-width line-width :foreground color)))

(def-simple-class polar-plot-frob (frob)
  (polar-plot dimensions))

(defmethod make-polar-plot-frob
    ((screen screen) dims &rest initargs &key frob &allow-other-keys)
  (remf initargs :frob)
  (setf (getf initargs :dimensions) dims)
  (cond ((typep frob 'polar-plot-frob)
	 (apply 'reinitialize-instance frob initargs))
	(t
	 (destroy frob)
	 (apply 'make-instance 'polar-plot-frob initargs))))

(defmethod render ((pane pane) (frob polar-plot-frob) y-offset x-offset zoom)
  (let* ((pic (polar-plot frob))
	 (vbl (viewable pic))
	 (pane (pane-of pic))
	 (pane-dims (dimensions pane))
	 (center-x (floor (cadr pane-dims) 2))
	 (center-y (floor (car pane-dims) 2)))
    (if (discrete-function-p vbl)
	(draw-polar-plot pane
			 (data vbl) (origin vbl) (increment vbl)
			 zoom (maximum pic) (line-width pic) (color pic)
			 (plot-symbol pic) (fill-symbol-p pic) (symbol-size pic)
			 (+ y-offset center-y) (+ x-offset center-x))
	(draw-polar-plot-pair pane
			      (data (second (viewable-list vbl)))
			      (data (first (viewable-list vbl)))
			      zoom (maximum pic) (line-width pic) (color pic)
			      (plot-symbol pic) (fill-symbol-p pic) (symbol-size pic)
			      (+ y-offset center-y) (+ x-offset center-x)))))
#|
;;; examples:

(display (make-discrete-function '(lambda (x) 1.0) 0.0 2-pi :size 11)
	 'polar-plot)

(display (make-discrete-function '(lambda (x) x) 0.0 2-pi :size 11)
	 'polar-plot)

(display (make-discrete-function '(lambda (x) (cos (* 2.0 x))) 0.0 2-pi :size 63)
	 'polar-plot)

(display (make-viewable-sequence
	  (list (make-discrete-function '(lambda (x) (* 100.0 (cos x))) 0.0 2-pi :size 63)
		(make-discrete-function '(lambda (x) (* 2.0 x)) 0.0 2-pi :size 63)))
	 'polar-plot)

(display (make-viewable-sequence
	  (list (make-discrete-function '(lambda (x) (cos x)) 0.0 (* 4 pi) :size 63)
		(make-discrete-function '(lambda (x) (/ x 2.0)) 0.0 (* 4 pi) :size 63)))
	 'polar-plot)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Scatter Plot Pictures (graph of image-pair)

(defmethod reset-picture-defaults ((graph scatter-plot) (im image-pair)
				   &rest initargs
				   &key
				   (pane-of (slot-value graph 'pane-of))
				   (zoom nil zoom-supplied-p)
				   (graph->data-x (slot-value graph 'graph->data-x))
				   (graph->data-y (slot-value graph 'graph->data-y))
				   (y-range (slot-value graph 'y-range))
				   (x-range (slot-value graph 'x-range)))
  ;; Make sure transforms are filled in (may have been done by more specialized methods).
  (unless (typep graph->data-x 'transform)
    (setq graph->data-x (setf (slot-value graph 'graph->data-x)
			      (make-transform))))
  (unless (typep graph->data-y 'transform)
    (setq graph->data-y (setf (slot-value graph 'graph->data-y)
			      (make-transform))))
  (unless (num-list-2-p y-range)
    (setq y-range (setf (getf initargs :y-range) (list (minimum (y-component im))
						       (maximum (y-component im))))))
  (unless (num-list-2-p x-range)
    (setq x-range (setf (getf initargs :x-range) (list (minimum (x-component im))
						       (maximum (x-component im))))))
  (when zoom-supplied-p
    (setq zoom
	  (cond ((numberp zoom) zoom)
		((eq zoom :auto)	;*** Magic number 50 for "typical" x-axis labels
		 (/ (- (x-dim pane-of) 50)
		    (transformed-distance graph->data-x x-range)))
		((num-list-2-p zoom)
		 (setf (getf initargs :aspect-ratio) (apply '/-0 zoom))
		 (/ (* 0.9 (cadr zoom)) (range (x-component im))))
		(t (floor (* (x-dim pane-of)) (range (x-component im))))))
    (setf (getf initargs :zoom) (if (> zoom 1) (floor zoom) (/ (ceiling (/ zoom))))))
  (apply #'call-next-method graph im initargs))

(defmethod compute-picture ((graph scatter-plot) (im image-pair))
  (with-slots (system-dependent-frob pane-of bounding-region graph-type
	       y-range x-range y-axis x-axis y-tick-step x-tick-step
	       y-tick-length x-tick-length y-tick-gap x-tick-gap
	       y-tick-format-string x-tick-format-string
	       y-label x-label axis-color color line-width
	       plot-symbol fill-symbol-p symbol-size) graph
    (multiple-value-bind (graph->frob-y graph->frob-x data->frob-y data->frob-x dims)
	(compute-graph-transforms graph)
      (setf system-dependent-frob
	    (make-graph-frob
	     (screen-of pane-of) dims
	     :frob system-dependent-frob
	     :graph graph
	     :data (list (data (y-component im))
			 (data (x-component im)))
	     :graph->frob-y graph->frob-y :graph->frob-x graph->frob-x
	     :data->frob-y data->frob-y :data->frob-x data->frob-x
	     )))))

#|
(defmethod scatter-plot ((y array) (x array)
			 &rest initargs &key x-range y-range (scale t) ->)
  (check-size x y)
  (remf initargs :x-range)
  (remf initargs :y-range)
  (remf initargs :->)
  (apply 'scatter-plot (image-from-array (vectorize y)) (image-from-array (vectorize x))
	 :x-range  x-range :y-range y-range :scale scale :-> -> initargs))

(defmethod scatter-plot ((y image) (x image)
			 &rest initargs &key x-range y-range (scale t) ((:-> result)))
  (remf initargs :x-range)
  (remf initargs :y-range)
  (remf initargs :->)
  (let* ((x-min (minimum x))
	 (x-max (maximum x))
	 (x-extra (* 0.05 (- x-max x-min)))
	 (y-min (minimum y))
	 (y-max (maximum y))
	 (y-extra (* 0.05 (- y-max y-min)))
	 range)
    (unless x-range (setq x-range (list (- x-min x-extra) (+ x-max x-extra))))
    (unless y-range (setq y-range (list (- y-min y-extra) (+ y-max y-extra))))
    (unless scale (setq range (list (min (- x-min x-extra)
					 (- y-min y-extra))
				    (max (+ x-max x-extra)
					 (+ y-max y-extra)))))
    (let ((ip (make-image-pair (list y x) :name result :display-type 'scatter-plot)))
      (apply 'display ip (display-type ip) :x-range (or range x-range)
	     :y-range (or range y-range) initargs)
      ip)))
|#

(defmethod scatter-plot ((y array) (x array) &rest args)
  (warn "scatter-plot is out of date, use make-scatter instead")
  (make-scatter y x))

(defmethod scatter-plot ((y image) (x image) &rest args)
  (warn "scatter-plot is out of date, use make-scatter instead")
  (make-scatter (data y) (data x)))

#|
(display (make-image-pair (list (make-synthetic-image '(1 20) '(lambda (y x) (* 2.0 x)))
				(make-synthetic-image '(1 20) '(lambda (y x) x))))
	 'scatter-plot :zoom 40 :graph-type :point :line-width 1)

(display (make-image-pair (list (make-synthetic-image '(1 20) '(lambda (y x) (random 1.0)))
				(make-synthetic-image '(1 20) '(lambda (y x) x))))
	 'scatter-plot :zoom 80 :graph-type :point :line-width 1)

(scatter-plot (make-matrix 1 2 3 4 5 6 7 8 9) (make-matrix 2 4 6 8 10 12 14 16 18))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Simple viewable that displays itself as a scatter plot without the
;;; overhead of images and image-pairs.

(def-simple-class scatter-frob (viewable)
  ((y-data :initform nil)
   (x-data :initform nil))
  (:default-initargs :display-type 'scatter-plot))

(defmacro scatter-frob-p (obj)
  `(typep ,obj 'scatter-frob))

(defmethod make-scatter ((y-data list) (x-data list) &rest initargs &key name ->)
  (declare (ignore name))
  (when -> (setf (getf initargs :name) ->))
  (apply #'make-instance
	 'scatter-frob
	 :y-data (make-matrix y-data)
	 :x-data (make-matrix x-data)
	 initargs))

(defmethod make-scatter ((y-data array) (x-data array) &rest initargs &key name ->)
  (declare (ignore name))
  (when -> (setf (getf initargs :name) ->))
  (apply #'make-instance
	 'scatter-frob
	 :y-data y-data
	 :x-data x-data
	 initargs))

(defmethod make-scatter ((y image) (x image) &rest args)
  (apply #'make-scatter (data y) (data x) args))

(defmethod data ((sc scatter-frob))
  (y-data sc))

(defmethod reset-picture-defaults ((graph scatter-plot) (sc scatter-frob)
				   &rest initargs
				   &key
				   (pane-of (slot-value graph 'pane-of))
				   (zoom nil zoom-supplied-p)
				   (graph->data-x (slot-value graph 'graph->data-x))
				   (graph->data-y (slot-value graph 'graph->data-y))
				   (y-range (slot-value graph 'y-range))
				   (x-range (slot-value graph 'x-range)))
  ;; Make sure transforms are filled in (may have been done by more specialized methods).
  (unless (typep graph->data-x 'transform)
    (setq graph->data-x (setf (slot-value graph 'graph->data-x)
			      (make-transform))))
  (unless (typep graph->data-y 'transform)
    (setq graph->data-y (setf (slot-value graph 'graph->data-y)
			      (make-transform))))

  (unless (num-list-2-p y-range)
    (let ((y-extra (* (range (y-data sc)) 0.025)))
      (setq y-range (setf (getf initargs :y-range)
			  (list (- (minimum (y-data sc)) y-extra)
				(+ (maximum (y-data sc)) y-extra))))))
  (unless (num-list-2-p x-range)
    (let ((x-extra (* (range (x-data sc)) 0.025)))
      (setq x-range (setf (getf initargs :x-range)
			  (list (- (minimum (x-data sc)) x-extra)
				(+ (maximum (x-data sc)) x-extra))))))

  (when zoom-supplied-p
    (setq zoom
	  (cond ((numberp zoom) zoom)
		((eq zoom :auto)	;*** Magic number 50 for "typical" x-axis labels
		 (/ (- (x-dim pane-of) 50)
		    (transformed-distance graph->data-x x-range)))
		((num-list-2-p zoom)
		 (setf (getf initargs :aspect-ratio) (apply '/-0 zoom))
		 (/ (* 0.9 (cadr zoom)) (range (x-data sc))))
		(t (floor (* (x-dim pane-of)) (range (x-data sc))))))
    (setf (getf initargs :zoom) (if (> zoom 1) (floor zoom) (/ (ceiling (/ zoom))))))
    (apply #'call-next-method graph sc initargs))

(defmethod compute-picture ((graph scatter-plot) (sc scatter-frob))
  (with-slots (system-dependent-frob pane-of bounding-region graph-type
	       y-range x-range y-axis x-axis y-tick-step x-tick-step
	       y-tick-length x-tick-length y-tick-gap x-tick-gap
	       y-tick-format-string x-tick-format-string
	       y-label x-label axis-color color line-width
	       plot-symbol fill-symbol-p symbol-size) graph
    (multiple-value-bind (graph->frob-y graph->frob-x data->frob-y data->frob-x dims)
	(compute-graph-transforms graph)
      (setf system-dependent-frob
	    (make-graph-frob
	     (screen-of pane-of) dims
	     :frob system-dependent-frob
	     :graph graph
	     :data (list (y-data sc) (x-data sc))
	     :graph->frob-y graph->frob-y :graph->frob-x graph->frob-x
	     :data->frob-y data->frob-y :data->frob-x data->frob-x
	     )))))

(defmethod overlay ((sc scatter-frob) vbl pic)
  (unless (every #'(lambda (x) (scatter-frob-p x)) (viewable-list vbl))
    (error "Overlays on a scatter frob must all be scatter-frobs"))
  (apply 'check-size sc (viewable-list vbl))
  (overlay-drawables pic vbl))


#|
;; Test code
(make-scatter
 '(9.6 12.5 46.8 127.3 154.9 174.2 154.9 127.3 46.8 12.5 9.6)
 '(-180 -90 -60 -30 -15 0 15 30 60 90 180))

(display
 (make-viewable-sequence
  (list (make-scatter
	 '(9.6 12.5 46.8 127.3 154.9 174.2 154.9 127.3 46.8 12.5 9.6)
	 '(-180 -90 -60 -30 -15 0 15 30 60 90 180))
	(make-scatter
	 '(71.9 80.2 91.6 143.6 175.6 175.6 143.6 91.6 80.2 71.9)
	 '(-180 -90 -60 -30 -15 15 30 60 90 180))))
 'overlay
 :sub-display-types '(scatter-plot scatter-plot)
 :graph-type :line
 :x-range '(-200 200) :x-tick-step 90
 :y-range '(0 180) :y-tick-step 30
 :x-axis 0)
|#


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
