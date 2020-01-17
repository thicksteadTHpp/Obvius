;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-draw.lisp
;;;  Author: Patrick C. Teo
;;;  Description: GL rendering library
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)



(defvar *v2i-array-0*
  (LCL:with-static-area (make-array '(2) :element-type '(signed-byte 32) :initial-element 0)))
(defvar *v2i-array-1*
  (LCL:with-static-area (make-array '(2) :element-type '(signed-byte 32) :initial-element 0)))

(defun get-v2i-array-0 (x y)
  (setf (aref *v2i-array-0* 0) (floor x)
	(aref *v2i-array-0* 1) (floor y))
  *v2i-array-0*)

(defun get-v2i-array-1 (x y)
  (setf (aref *v2i-array-1* 0) (floor x)
	(aref *v2i-array-1* 1) (floor y))
  *v2i-array-1*)


;;; Returns the current font id.  The default GL font id is zero.
;;; We are currently ignoring the font-id, we have to fix this!!
(defmethod font ((pane GL-pane))
  (GL:with-GL-lock
    (GL:winset (wid pane))
    (GL:getfont)))

(defun font-height (font-id)
  (declare (ignore font-id))
  (GL:with-GL-lock (GL:getheight)))

(defun string-width (font-id string)
  (declare (ignore font-id))
  (GL:with-GL-lock (GL:strwidth string)))

(defmethod draw-text ((pane GL-pane) y x string &rest keys)
  (declare (ignore keys))
  (let ((x-int (floor x)) (y-int (floor y)))
    (GL:with-GL-lock
      (GL:cmov2i x-int y-int)
      (GL:charstr string))))



;;; Change *not* to use default foreground color
;;; set only when supplied -- otherwise use current color assignment!!
(defmethod draw-line ((pane GL-pane) from-y from-x to-y to-x
		      &rest keys
		      &key foreground line-width line-style)

  (declare (ignore keys line-style))
  (when foreground (set-window-color pane foreground))

  (if (and (= from-y to-y) (= from-x to-x))
      ;;;
      ;;; if it's a point, we have to draw a point instead
      ;;; otherwise, GL will freak out and give a div by zero erro.
      ;;;
      (let ((point-vertex (get-v2i-array-0 from-x from-y)))
	(GL:with-GL-lock
	  (when line-width (GL:pntsize (ceiling line-width)))
	  (GL:bgnpoint)
	  (GL:v2i point-vertex)
	  (GL:endpoint)))
      (let ((from-vertex (get-v2i-array-0 from-x from-y))
	    (to-vertex (get-v2i-array-1 to-x to-y)))
	(GL:with-GL-lock
	  (when line-width (GL:linewidth (ceiling line-width)))
	  (GL:bgnline)
	  (GL:v2i from-vertex)
	  (GL:v2i to-vertex)
	  (GL:endline)))))

(defmethod draw-lines ((pane GL-pane) y0 x0 y1 x1
		       &key foreground line-width line-style x-offset y-offset)
  (declare (ignore line-style))
  (declare (fixnum x-offset y-offset))
  (declare (type (vector (signed-byte 32)) y0 x0 y1 x1))  

  (let ((nlines (length y0)))
    (unless y-offset (setq y-offset 0))
    (unless x-offset (setq x-offset 0))

    (when foreground (set-window-color pane foreground))

    (GL:with-GL-lock
      (when line-width (GL:linewidth (ceiling line-width)))
      (internal-draw-lines y0 x0 y1 x1 y-offset x-offset nlines))))



(defmethod draw-rect ((pane GL-pane) y0 x0 y1 x1
		      &rest keys
		      &key foreground (fill-p nil) line-width line-style)
  (declare (ignore keys line-style))
  (let ((x0-int (floor x0)) (y0-int (floor y0))
	(x1-int (floor x1)) (y1-int (floor y1)))
    (when foreground (set-window-color pane foreground))
    (GL:with-GL-lock
      (when line-width (GL:linewidth (ceiling line-width)))
      (if fill-p
	  (GL:rectfi x0-int y0-int x1-int y1-int)
	  (GL:recti x0-int y0-int x1-int y1-int)))))

(defmethod draw-rects ((pane GL-pane) y0 x0 y1 x1
		       &key (fill-p nil) foreground line-width line-style)
  (declare (ignore line-style))
  (declare (type (vector (signed-byte 32)) y0 x0 y1 x1))

  (let ((nrects (length y0))
	(filled (if fill-p 1 0)))
    (when foreground (set-window-color pane foreground))
    (GL:with-GL-lock
      (when line-width (GL:linewidth (ceiling line-width)))
      (internal-draw-rects y0 x0 y1 x1 filled nrects))))



;;; draws a square given its center coordinate and its size (half its length)
(defmethod draw-square ((pane GL-pane) y0 x0 size
			&rest keys
			&key foreground (fill-p nil) line-width line-style)
  (declare (ignore keys line-style))
  (draw-rect pane (- y0 size) (- x0 size) (+ y0 size) (+ x0 size)
	     :foreground foreground
	     :fill-p fill-p
	     :line-width line-width
	     :line-style line-style))

;;; the size parameter shouldn't be a key!!
(defmethod draw-squares ((pane GL-pane) y0 x0
			 &key foreground (fill-p nil) line-width line-style size)
  (dotimes (i (length y0))
    (draw-square pane (aref y0 i) (aref x0 i) size
		 :foreground foreground
		 :fill-p fill-p
		 :line-width line-width
		 :line-style line-style)))
  
  
(defmethod draw-circle ((pane GL-pane) y-center x-center radius
			&rest keys
			&key foreground (fill-p nil) line-width line-style)
  (declare (ignore line-width line-style keys))
  (let ((x-center-int (floor x-center))
	(y-center-int (floor y-center))
	(radius-int (floor radius)))
    (when foreground (set-window-color pane foreground))
    (GL:with-GL-lock
      (if fill-p
	  (GL:circfi x-center-int y-center-int radius-int)
	  (GL:circi x-center-int y-center-int radius-int)))))

;;; radius shouldn't be a key!!
(defmethod draw-circles ((pane GL-pane) y-center x-center
			 &key foreground line-width line-style fill-p radius)
  (declare (ignore line-width line-style))
  (declare (fixnum radius))
  (declare (type (vector (signed-byte 32)) y-center x-center))

  (let ((ncircles (length y-center))
	(filled (if fill-p 1 0)))
    (when foreground (set-window-color pane foreground))
    (GL:with-GL-lock
	(internal-draw-circles y-center x-center radius
			       filled
			       ncircles))))
  


;;;
;;; GL specific draw-graph routine
;;;
(defmethod draw-graph ((drawable GL-pane)
		       data graph-type
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
  (declare (ignore y-offset x-offset y-label x-label))
  (let ((x-axis-pos (transform-point graph->frob-y (or x-axis (apply 'clip 0 y-range)))))
    
    (when axis-color (set-window-color drawable axis-color))
    (when x-axis
      (plot-x-axis drawable graph->frob-x (transform-point graph->frob-y x-axis)
		   x-range x-tick-step x-tick-length x-tick-gap
		   x-tick-format-string font))
    (when y-axis
      (plot-y-axis drawable graph->frob-y (transform-point graph->frob-x y-axis)
		   y-range y-tick-step y-tick-length y-tick-gap
		   y-tick-format-string font))

    (when color (set-window-color drawable color))
    (when line-width (GL:with-GL-lock (GL:pntsize (ceiling line-width))))

    ;;;
    ;;; Draw only a portion of the graph by clipping.
    ;;;
    (set-2d-viewport drawable
		     :top (transform-point graph->frob-y (cadr y-range))
		     :left (transform-point graph->frob-x (car x-range))
		     :bottom (transform-point graph->frob-y (car y-range))
		     :right (transform-point graph->frob-x (cadr x-range)))
    (plot-data graph-type data drawable
	       data->frob-y data->frob-x x-axis-pos
	       plot-symbol fill-symbol-p symbol-size)

    ;;;
    ;;; Restore entire 2D-viewport
    ;;;
    (set-2d-viewport drawable)))
