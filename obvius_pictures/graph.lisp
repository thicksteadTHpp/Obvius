;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: graph.lisp
;;;  Author: Simoncelli
;;;  Description: General graph-drawing routines.  Fairly independent of obvius
;;;  Creation Date: 3/91
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '())

;;;; Draw graph  (this stuff not dependent on obvius).

;;; **** This still needs a little work to get rid of LispView
;;; dependencies.  Also, some hairy stuff has to be fixed (e.g.:
;;; switching to log plot when there are negative values).

;;; Generic method used by postscript panes.  Redefined for
;;; X-drawables and X-panes in x-draw.lisp.  *** Many args ignored
(defmethod draw-graph ((drawable t) data graph-type
		       graph->frob-y graph->frob-x data->frob-y data->frob-x
		       y-range x-range y-axis x-axis
		       y-tick-step x-tick-step y-tick-length x-tick-length
		       y-tick-gap x-tick-gap y-tick-format-string x-tick-format-string
		       y-label x-label
		       &key
		       (font (font drawable))
		       color axis-color line-width
		       plot-symbol fill-symbol-p symbol-size
		       x-offset y-offset)
  (declare (ignore x-offset y-offset line-width axis-color color y-label x-label))
  (when x-axis
    (plot-x-axis drawable graph->frob-x (transform-point graph->frob-y x-axis)
		 x-range x-tick-step x-tick-length x-tick-gap
		 x-tick-format-string font))
  (when y-axis
    (plot-y-axis drawable graph->frob-y (transform-point graph->frob-x y-axis)
		 y-range y-tick-step y-tick-length y-tick-gap
		 y-tick-format-string font))
  (let ((x-axis-pos (transform-point graph->frob-y (or x-axis (apply 'clip 0 y-range)))))
    (plot-data graph-type data drawable
	       data->frob-y data->frob-x x-axis-pos
	       plot-symbol fill-symbol-p symbol-size)))

;;; range and tick-step are in graph coordinates.  axis-pos and
;;; tick-length are in frob coords.  Plots an axis line, with tick
;;; marks hanging from it and a label centered under each tick.  Labels that would
;;; overlap other labels are skipped over, although the tick is still plotted.
(defun plot-x-axis (frob graph->frob axis-pos range tick-step
			  tick-length tick-gap tick-format-string font)
  (draw-line frob
	     axis-pos (transform-point graph->frob (car range))
	     axis-pos (transform-point graph->frob (cadr range)))
  (loop with tick-endpt = (+ axis-pos tick-length)
	with first-tick = (* tick-step (ceiling (car range) tick-step))
	with last-tick = (cadr range)
	with font-height = (font-height font)
	with font-bottom = (+ tick-endpt font-height tick-gap)
	with last-labeled-tick-pos = nil
	for tick = first-tick then (+ tick tick-step) until (> tick last-tick)
	for tick-pos = (transform-point graph->frob tick)
	for tick-string = (format-tick nil tick-format-string tick)
	for string-width = (string-width font tick-string)
	do
	(draw-line frob axis-pos tick-pos tick-endpt tick-pos)
	(when (or (null last-labeled-tick-pos)
		  (> (abs (- tick-pos last-labeled-tick-pos)) string-width))
	  (setq last-labeled-tick-pos (+ tick-pos tick-gap))
	  (draw-text frob
		     font-bottom
		     (- tick-pos (ceiling string-width 2))
		     tick-string))))

(defun plot-y-axis (frob graph->frob axis-pos range tick-step
			  tick-length tick-gap tick-format-string font)
  (draw-line frob
	     (transform-point graph->frob (car range)) axis-pos
	     (transform-point graph->frob (cadr range)) axis-pos)
  (loop with tick-endpt = (- axis-pos tick-length)
	with tick-right = (- tick-endpt tick-gap)
	with first-tick = (* tick-step (ceiling (car range) tick-step))
	with last-tick = (cadr range)
	with font-height = (font-height font)
	with font-height/2 = (floor font-height 2)
	with last-labeled-tick-pos = nil
	for tick = first-tick then (+ tick tick-step) until (> tick last-tick)
	for tick-pos = (transform-point graph->frob tick)
	for tick-string = (format-tick nil tick-format-string tick)
	do
	(draw-line frob tick-pos axis-pos tick-pos tick-endpt)
	(when (or (null last-labeled-tick-pos)
		  (> (abs (- tick-pos last-labeled-tick-pos)) font-height))
	  (setq last-labeled-tick-pos (- tick-pos tick-gap))
	  (draw-text frob
		     (+ tick-pos font-height/2)
		     ;; add some leading here:
		     (- tick-right (string-width font tick-string))
		     tick-string))))

(defun draw-symbols (frob yarr xarr
		     &key line-width foreground
		     plot-symbol fill-symbol-p symbol-size)
  (cond ((null plot-symbol)
	 t)
	((eq plot-symbol :circle)
	 (draw-circles frob yarr xarr :fill-p fill-symbol-p :radius symbol-size
		       :line-width line-width :foreground foreground))
	((eq plot-symbol :square)
	 (draw-squares frob yarr xarr :fill-p fill-symbol-p :size symbol-size
		       :line-width line-width :foreground foreground))
	(t (error "~a is not at supported plot symbol" plot-symbol))))

;;; transforms are from data coords to frob coords
;;; [THO] 2020 changed so that the displaced arrays have the same type
;;;       as their base-arrays
(defmethod plot-data ((graph-type (eql :line)) (data array)
		      frob y-transform x-transform x-axis-pos
		      plot-symbol fill-symbol-p symbol-size)
  (declare (ignore x-axis-pos))
  (let* ((data-vector (vectorize data))
	 (dim-1 (1- (length data-vector)))
	 (x-vect (transform-range x-transform 0 dim-1))
	 (y-vect (transform-vector y-transform data-vector)))
    (draw-lines frob
		(displace-to y-vect dim-1)
		(displace-to x-vect dim-1 )
		(displace-to y-vect dim-1  :displaced-index-offset 1)
		(displace-to x-vect dim-1  :displaced-index-offset 1))
		;; (make-array dim-1 :displaced-to y-vect :element-type 'fixnum)
		;; (make-array dim-1 :displaced-to x-vect :element-type 'fixnum)
		;; (make-array dim-1 :displaced-to y-vect :displaced-index-offset 1
		;; 	    :element-type 'fixnum)
		;; (make-array dim-1 :displaced-to x-vect :displaced-index-offset 1
		;; 	    :element-type 'fixnum))
    (draw-symbols frob y-vect x-vect
		  :plot-symbol plot-symbol :fill-symbol-p fill-symbol-p
		  :symbol-size symbol-size)))

(defmethod plot-data ((graph-type (eql :bar)) (data array)
		      frob y-transform x-transform x-axis-pos
		      plot-symbol fill-symbol-p symbol-size)
  (declare (ignore plot-symbol symbol-size))
  (let* ((data-vector (vectorize data))
	 (dim (length data-vector))
	 (x0-vect (transform-range x-transform 0 (1- dim)))
	 (x1-vect (copy x0-vect))
	 (y0-vect (make-array dim :element-type 'fixnum
				  :initial-element x-axis-pos))
	 (y1-vect (transform-vector y-transform data-vector)))
    (sub x0-vect (ceiling (slot-value x-transform 'scale) 2) :-> x0-vect)
    (add x1-vect (floor (slot-value x-transform 'scale) 2) :-> x1-vect)
    (draw-rects frob y0-vect x0-vect y1-vect x1-vect :fill-p fill-symbol-p)
    ))

(defmethod plot-data ((graph-type (eql :point)) (data array)
		      frob y-transform x-transform x-axis-pos
		      plot-symbol fill-symbol-p symbol-size)
  (declare (ignore x-axis-pos))
  (let* ((data-vector (vectorize data))
	 (dim (length data-vector))
	 (x-vect (transform-range x-transform 0 (1- dim)))
	 (y-vect (transform-vector y-transform data-vector)))
    (draw-symbols frob y-vect x-vect
		  :plot-symbol plot-symbol :fill-symbol-p fill-symbol-p
		  :symbol-size symbol-size)
    ))

;;; data is list of arrays (y-data x-data)
;;; [THO] 2020 changed so that the displaced arrays have the same type
;;;       as their base-arrays
(defmethod plot-data ((graph-type (eql :line)) (data cons)
		      frob y-transform x-transform x-axis-pos
		      plot-symbol fill-symbol-p symbol-size)
  (declare (ignore x-axis-pos))
  (let* ((y-data-vector (vectorize (first data)))
	 (x-data-vector (vectorize (second data)))
	 (dim-1 (1- (length x-data-vector)))
	 (x-vect (transform-vector x-transform x-data-vector))
	 (y-vect (transform-vector y-transform y-data-vector)))
    (draw-lines frob
		(displace-to y-vect dim-1)
		(displace-to x-vect dim-1)
		(displace-to y-vect dim-1 :displaced-index-offset 1)
		(displace-to x-vect dim-1 :displaced-index-offset 1))
		;;before 2020
		;; (make-array dim-1 :displaced-to y-vect :element-type 'fixnum)
		;; (make-array dim-1 :displaced-to x-vect :element-type 'fixnum)
		;; (make-array dim-1 :displaced-to y-vect :displaced-index-offset 1
		;; 	    :element-type 'fixnum)
		;; (make-array dim-1 :displaced-to x-vect :displaced-index-offset 1
		;; 	    :element-type 'fixnum))
    (draw-symbols frob y-vect x-vect
		  :plot-symbol plot-symbol :fill-symbol-p fill-symbol-p
		  :symbol-size symbol-size)))

;;; data is list of arrays (y-data x-data)
(defmethod plot-data ((graph-type (eql :point)) (data cons)
		      frob y-transform x-transform x-axis-pos
		      plot-symbol fill-symbol-p symbol-size)
  (declare (ignore x-axis-pos))
  (let* ((y-data-vector (vectorize (first data)))
	 (x-data-vector (vectorize (second data)))
	 (x-vect (transform-vector x-transform x-data-vector))
	 (y-vect (transform-vector y-transform y-data-vector)))
    (draw-symbols frob y-vect x-vect
		  :plot-symbol plot-symbol :fill-symbol-p fill-symbol-p
		  :symbol-size symbol-size)
    ))

(defmethod plot-data ((graph-type (eql :bar)) (data cons)
		      frob y-transform x-transform x-axis-pos
		      plot-symbol fill-symbol-p symbol-size)
  (warn "Can't plot scatter plot as bar graph, using points instead")
  (plot-data :point data frob y-transform x-transform x-axis-pos
	     plot-symbol fill-symbol-p symbol-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; There are 3 coordinate systems: graph coordinates (as labelled by
;;; the ticks on the axes), data coords (corresponding to the actual
;;; values in the data array and their indices), and frob coordinates
;;; (pixels, or some other device coordinate system).  The zoom and
;;; aspect-ratio parameters control the transformation from data
;;; coords to frob coords.  The scale and offset parameters control
;;; the transformation from data coords to graph coords.

;;; Returns, as multiple values, the relevant coordinate-transforms:
;;; data->frob-y data->frob-x graph->frob-y graph->frob-y, and the total
;;; dimensions of the drawing area.
(defun compute-graph-transforms (graph)
  (with-slots (y-axis-type pane-of graph->data-y graph->data-x
			   y-range x-range zoom aspect-ratio
			   y-tick-step x-tick-step y-tick-gap x-tick-gap
			   y-tick-format-string x-tick-format-string
			   y-tick-length x-tick-length y-axis x-axis
			   frob->vbl-y frob->vbl-x) graph
    ;; First, compute xforms, assuming upper-left graph corner is (0,0).
    (let* ((data->frob-x (make-transform :scale zoom :offset (- (car x-range))
					 :coerce #'floor))
	   data->frob-y graph->frob-y graph->frob-x frob-y-range)
      (setq graph->frob-x (concat-transforms graph->data-x data->frob-x))
      (setq frob-y-range (* aspect-ratio (transformed-distance graph->frob-x x-range)))
      (setq data->frob-y
	    (case y-axis-type
	      (:log (make-transform :range1 y-range :range2 (list frob-y-range 0)
				    :func #'log-0 :ifunc #'exp :coerce #'floor))
	      (t (make-transform :range1 y-range :range2 (list frob-y-range 0)
				 :coerce #'floor))))
      (setq graph->frob-y (concat-transforms graph->data-y data->frob-y))
      ;; Now compute size of various elements
      (let* ((font (font pane-of))
	     (font-height (font-height font))
	     (left-tick (* x-tick-step (ceiling (car x-range) x-tick-step)))
	     (right-tick (* x-tick-step (floor (cadr x-range) x-tick-step)))
	     (bottom-tick (* y-tick-step (ceiling (car y-range) y-tick-step)))
	     (top-tick (* y-tick-step (floor (cadr y-range) y-tick-step)))
	     y-tick-label-width x-tick-label-width left right top bottom)
	;; *** Broken:
	(setq y-tick-label-width
	      (max (string-width font (format-tick nil y-tick-format-string top-tick))
		   (string-width font (format-tick nil y-tick-format-string bottom-tick))))
	(setq x-tick-label-width
	      (max (string-width font (format-tick nil x-tick-format-string right-tick))
		   (string-width font (format-tick nil x-tick-format-string left-tick))))
	(setq left (apply #'min
			  (transform-point graph->frob-x (car x-range))
			  (collect (and x-axis (- (transform-point graph->frob-x left-tick)
						  (ceiling x-tick-label-width 2)))
				   (and y-axis
					(- (transform-point graph->frob-x y-axis)
					   y-tick-label-width y-tick-length y-tick-gap)))))
	(setq right (apply #'max
			   (transform-point graph->frob-x (cadr x-range))
			   (and x-axis (list (+ (transform-point graph->frob-x right-tick)
						(floor x-tick-label-width 2))))))
	(setq bottom (apply #'max
			    (transform-point graph->frob-y (car y-range))
			    (collect (and y-axis (+ (transform-point graph->frob-y bottom-tick)
						    (floor font-height 2)))
				     (and x-axis (+ (transform-point graph->frob-y x-axis)
						    x-tick-length x-tick-gap font-height)))))
	(setq top (apply #'min
			 (transform-point graph->frob-y (cadr y-range))
			 (and y-axis (list (- (transform-point graph->frob-y top-tick)
					      (ceiling font-height 2))))))
	(translate-transform! graph->frob-x (- left))
	(translate-transform! data->frob-x (- left))
	(translate-transform! graph->frob-y (- top))
	(translate-transform! data->frob-y (- top))
	(setf frob->vbl-y (invert-transform graph->frob-y))
	(setf frob->vbl-x (invert-transform graph->frob-x))
	(values graph->frob-y graph->frob-x data->frob-y data->frob-x
		(list (- bottom top) (- right left)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Other stuff:

;;; Compute a tick-step that will give at least min-ticks ticks, but
;;; no more than max-ticks.  Range is a list of length 2 giving the
;;; min and max values to be plotted.
(defun compute-tick-step (range &key (min-ticks 4))
  (let* ((dist (abs (apply #'- range)))
	 (exp (1- (floor (log-0 dist 10))))
	 (valid-multiples '(1 2 5)))	;valid multiples of pwrs of 10
    ;; start with small tick-step, working up until
    (loop with max-step = (/ dist min-ticks)
	  with step-multiples = valid-multiples
	  with base-step = (expt 10 exp) ; pwr of 10
	  for prev-step = (* base-step (car step-multiples)) then step
	  for step = (* base-step (car step-multiples))
	  until (> step max-step)
	  do
	  (setq step-multiples (cdr step-multiples))
	  ;; try next power of 10:
	  (unless step-multiples
	    (setq step-multiples valid-multiples)
	    (setq base-step (* base-step 10)))
	  finally (return (if (integerp prev-step) prev-step (float prev-step))))))

;;; Return a format string that will do a reasonable job of formatting
;;; ticks between first-tick and last-tick.  If you want float ticks,
;;; you must pass float values for the first tick values.  Number of
;;; sig digits is taken from this.  We replace "E" with a less
;;; obtrusive "e".
(defun compute-tick-format-string (first-tick last-tick tick-step &optional (max-length 7))
  (let* ((plain-string (format nil "~A" tick-step))
	 (exp-string (format nil "~~~D,,,,'*,,'eE" max-length))
	 (dec-pos (position #\. plain-string))
	 (exp-p (find #\E plain-string)))
    (if exp-p
	exp-string
	(let* ((fixed-format (if dec-pos ;float tick-step
				 (format nil "~~,~DF" (- (length plain-string) dec-pos 1))
				 "~D"))
	       (first-string (format-tick nil fixed-format first-tick))
	       (last-string (format-tick nil fixed-format last-tick)))
	  (if (and (<= (length first-string) max-length)
		   (<= (length last-string) max-length))
	      fixed-format
	      exp-string)))))

;;; This allows modification of the standard format behavior.  Here,
;;; we remove leading whitespace which would mis-align x-axis labels.
;;; If the format string uses the ~F option, this function may also
;;; want to trim the "."  from the right side of the string.  The
;;; format-string is actually a list, containing a format string and a
;;; number: the number of digits after the decimal point.
(defun format-tick (stream tick-format-string tick)
  (string-left-trim " " (format stream tick-format-string tick)))

#| test:
(let* ((num 0.0001)
       (len 6)
       (format-string (obv::compute-tick-format-string num num num len)))
  (values (obv::format-tick nil format-string num) format-string))
|#

#|
(new-pane :width 300 :height 200)

(setq data (make-array 128 :element-type 'single-float))
(dotimes (i (length data))
  (setf (aref data i) (1+ (random 9.0))))

(setq graph
      (make-instance 'obv::graph
		     :y-range '(1 10) :x-range '(-1 1)
		     :graph->data-x (obv::make-transform :scale 64 :offset 64)
		     :y-axis -1 :x-axis 1
		     :y-tick-step 1 :x-tick-step 0.5
		     :graph-type :bar
		     :zoom 2 :aspect-ratio 0.65
		     :y-axis-type :linear
		     :line-width 1
		     :color :white
		     :x-tick-length 3 :y-tick-length 3
		     :pane-of obv::*current-pane*
		     ))

(multiple-value-bind (graph->frob-y graph->frob-x data->frob-y data->frob-x dims)
    (obv::compute-graph-transforms graph)
  (with-slots (obv::graph-type obv::y-range obv::x-range obv::y-axis obv::x-axis
	       obv::y-tick-step obv::x-tick-step obv::y-tick-length
	       obv::y-tick-length obv::x-tick-length
	       obv::y-tick-gap obv::x-tick-gap
	       obv::y-tick-format-string obv::x-tick-format-string
	       obv::y-label obv::x-label) graph
    (obv::draw-graph obv::*current-pane* data
		     obv::graph-type
		     graph->frob-y graph->frob-x data->frob-y data->frob-x
		     obv::y-range obv::x-range obv::y-axis obv::x-axis
		     obv::y-tick-step obv::x-tick-step obv::y-tick-length obv::x-tick-length
		     obv::y-tick-gap obv::x-tick-gap
		     obv::y-tick-format-string obv::x-tick-format-string
		     obv::y-label obv::x-label
		     :color :white 
		     :line-width 1)
    (values graph->frob-y graph->frob-x dims)))

|#

;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
