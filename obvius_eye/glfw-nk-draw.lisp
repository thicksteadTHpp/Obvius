;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  glfw-nk-draw.lisp
;;;  Author: Patrick C. Teo, [THO]
;;;  Description: nk drawing routines
;;;  Creation Date: 1993, 2020
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)


(defstruct (nk-canvas (:conc-name canvas-))
  context
  painter
  item-spacing
  panel-padding
  window-background
  drawing-color
  total-space
  rect)


;; returns the font-helper in dispatcher
;; which should hold a pointer to the default font
;; if not changed otherwise
(defmethod font ((pane nk-pane))
  (when (is-active-p (screen-of pane))
    (slot-value (screen-of pane) 'font-helper)))
    ;; (claw:c-val ((ctx (:struct (%nk:context)) (nk::nk-renderer-handle (slot-value (screen-of pane) 'nk-renderer))))
    ;;   (ctx :style :font &))))

;;todo get the font height from nk
;; font arg should be a font-helper class
(defmethod font-height ((font font-helper))
  (with-slots (font) font
    (claw:c-val ((font (:struct (%nk:font)) font))
      (font :handle :height))))
  

;;todo let nk calculate the string width
;; font should be a font-helper object
(defmethod string-width ((font font-helper) string)
  (font-helper-font-text-width font string))

(defmacro nk-drawing ((rgb-foreground-var background-var rect-var painter-var canvas) &body body)
  (assert (symbolp rgb-foreground-var))
  (assert (symbolp background-var))
  (assert (symbolp rect-var))
  (assert (symbolp painter-var))
  `(claw:c-val ((,rgb-foreground-var (:struct (%nk:color)) (canvas-drawing-color ,canvas))
		(,background-var (:struct (%nk:style-item)) (canvas-window-background ,canvas))
		(,rect-var (:struct (%nk:rect)) (canvas-rect ,canvas))
		(,painter-var (:struct (%nk:command-buffer)) (canvas-painter ,canvas)))
     ,@body))

(defmacro f> (num)
  `(float ,num 1f0))


(defmethod draw-text ((pane nk-pane) y x string &rest keys)
  (let ((font-handle (font-helper-font-handle (font pane)))
	(canvas (slot-value pane 'canvas)))
    (nk-drawing (foreground background rect painter canvas)
      (%nk:draw-text painter (%nk:recti rect (floor x) (floor y) 150 20) string (length string) font-handle foreground background))))
  ;;(vom:info "[draw-text] ~s at x: ~d y: d" string x y))
  ;; (declare (ignore keys))
  ;; (let ((x-int (floor x)) (y-int (4floor y)))
  ;;   (GL:with-GL-lock
  ;;     (GL:cmov2i x-int y-int)
  ;;     (GL:charstr string))))


;;; Change *not* to use default foreground color
;;; set only when supplied -- otherwise use current color assignment!!
(defmethod draw-line ((pane nk-pane) from-y from-x to-y to-x
		      &rest keys
		      &key foreground line-width line-style)
  
  (declare (ignore keys line-style))
  (let ((canvas (slot-value pane 'canvas))
	(line-width (float (if line-width line-width 1f0) 1f0)))
    (nk-drawing (foreground background rect painter canvas)
      (%nk:stroke-line painter (f> from-x) (f> from-y) (f> to-x) (f> to-y) line-width foreground))))   
 


  
  ;;(vom:info "[draw-line] from (x= ~d, y= ~d) to (x= ~d, y= ~d)" from-x from-y to-x to-y))
  ;; (when foreground (set-window-color pane foreground))

  ;; (if (and (= from-y to-y) (= from-x to-x))
  ;;     ;;;
  ;;     ;;; if it's a point, we have to draw a point instead
  ;;     ;;; otherwise, GL will freak out and give a div by zero erro.
  ;;     ;;;
  ;;     (let ((point-vertex (get-v2i-array-0 from-x from-y)))
  ;; 	(GL:with-GL-lock
  ;; 	  (when line-width (GL:pntsize (ceiling line-width)))
  ;; 	  (GL:bgnpoint)
  ;; 	  (GL:v2i point-vertex)
  ;; 	  (GL:endpoint)))
  ;;     (let ((from-vertex (get-v2i-array-0 from-x from-y))
  ;; 	    (to-vertex (get-v2i-array-1 to-x to-y)))
  ;; 	(GL:with-GL-lock
  ;; 	  (when line-width (GL:linewidth (ceiling line-width)))
  ;; 	  (GL:bgnline)
  ;; 	  (GL:v2i from-vertex)
  ;; 	  (GL:v2i to-vertex)
  ;; 	  (GL:endline)))))

(defmethod draw-lines ((pane nk-pane) y0 x0 y1 x1
		       &key foreground line-width line-style (x-offset 0) (y-offset 0))
  (declare (fixnum x-offset y-offset))
;; (declare (type (vector fixnum) y0 x0 y1 x1))  
  ;; (vom:info "[draw-lines]"))
  (let ((nlines (length y0)))
    (unless y-offset (setq y-offset 0))
    (unless x-offset (setq x-offset 0))
    (dotimes (i nlines)
      (draw-line pane
		 (+ (aref y0 i) y-offset) 
		 (+ (aref x0 i) x-offset)
		 (+ (aref y1 i) y-offset)
		 (+ (aref x1 i) x-offset)
		 :line-style line-style
		 :foreground foreground
		 :line-width line-width))))

  ;;   (when foreground (set-window-color pane foreground))

  ;;   (GL:with-GL-lock
  ;;     (when line-width (GL:linewidth (ceiling line-width)))
  ;;     (internal-draw-lines y0 x0 y1 x1 y-offset x-offset nlines))))



(defmethod draw-rect ((pane nk-pane) y0 x0 y1 x1
		      &rest keys
		      &key foreground (fill-p nil) line-width line-style)
  (declare (ignore keys line-style))
  (let ((x0 (f> x0)) (y0 (f> y0))
	(x1 (f> x1)) (y1 (f> y1)))
    (when foreground (set-window-color pane foreground))
    (let ((canvas (slot-value pane 'canvas))
	  (line-width (float (if line-width line-width 1f0) 1f0)))
      (nk-drawing (foreground background rect painter canvas)
	(if fill-p
	    (%nk:fill-rect painter (%nk:rect rect x0 y0 (- x1 x0) (- y1 y0)) 1f0 foreground)
	    (%nk:stroke-rect painter (%nk:rect rect x0 y0 (- x1 x0) (- y1 y0)) 1f0 line-width foreground))))))
  
    ;; (vom:info "[draw-rect] from (x= ~d, y= ~d) to (x= ~d, y= ~d)" x0 y0 x1 y1)))
    ;; (GL:with-GL-lock
    ;;   (when line-width (GL:linewidth (ceiling line-width)))
    ;;   (if fill-p
    ;; 	  (GL:rectfi x0-int y0-int x1-int y1-int)
    ;; 	  (GL:recti x0-int y0-int x1-int y1-int)))))

(defmethod draw-rects ((pane nk-pane) y0 x0 y1 x1
		       &key (fill-p nil) foreground line-width line-style)
  (declare (ignore line-style))
  (declare (type (simple-vector (signed-byte 32)) y0 x0 y1 x1))
  ;;  (vom:info "[draw-rects]"))
  (dotimes (i (reduce (lambda (a b) (min a b)) (mapcar #'length (list y0 x0 y1 x1))))
    (draw-rect pane
	       (svref y0 i)
	       (svref x0 i)
	       (svref y1 i)
	       (svref x1 i)
	       :line-style line-style
	       :fill-p fill-p
	       :foreground foreground
	       :line-width line-width)))

  ;; (let ((nrects (length y0))
  ;; 	(filled (if fill-p 1 0)))
  ;;   (when foreground (set-window-color pane foreground))
  ;;   (GL:with-GL-lock
  ;;     (when line-width (GL:linewidth (ceiling line-width)))
  ;;     (internal-draw-rects y0 x0 y1 x1 filled nrects))))



;;; draws a square given its center coordinate and its size (half its length)
(defmethod draw-square ((pane nk-pane) y0 x0 size
			&rest keys
			&key foreground (fill-p nil) line-width line-style)
  (declare (ignore keys line-style))
  (draw-rect pane (- y0 size) (- x0 size) (+ y0 size) (+ x0 size)
	     :foreground foreground
	     :fill-p fill-p
	     :line-width line-width
	     :line-style line-style))

;;; the size parameter shouldn't be a key!!
(defmethod draw-squares ((pane nk-pane) y0 x0
			 &key foreground (fill-p nil) line-width line-style size)
  (dotimes (i (length y0))
    (draw-square pane (aref y0 i) (aref x0 i) size
		 :foreground foreground
		 :fill-p fill-p
		 :line-width line-width
		 :line-style line-style)))
  
  
(defmethod draw-circle ((pane nk-pane) y-center x-center radius
			&rest keys
			&key foreground (fill-p nil) line-width line-style)
  (declare (ignore line-style keys))
  ;;  (vom:info "[draw-circle] center (x= ~d, y= ~d) radius ~d" x-center y-center radius)
  
  (let ((x-center (f> x-center))
	(y-center (f> y-center))
	(radius (f> radius))
	(width  (f> (if line-width line-width 1)))
	(canvas (slot-value pane 'canvas)))
    (nk-drawing (foreground background rect painter canvas)
      (if fill-p
	  (%nk:fill-circle painter (%nk:rect x-center y-center radius radius) foreground)
	  (%nk:stroke-circle painter (%nk:rect x-center y-center radius radius) width foreground)))))
    
    
    ;; (when foreground (set-window-color pane foreground))
    ;; (GL:with-GL-lock
    ;;   (if fill-p
    ;; 	  (GL:circfi x-center-int y-center-int radius-int)
    ;; 	  (GL:circi x-center-int y-center-int radius-int)))))

;;; radius shouldn't be a key!!
(defmethod draw-circles ((pane nk-pane) y-center x-center
			 &key foreground line-width line-style fill-p radius)
  (declare (ignore line-width line-style))
  (declare (fixnum radius))
  (declare (type (simple-vector (signed-byte 32)) y-center x-center))
  ;;  (vom:info "[draw-circles]"))
  (dotimes (n (min (length y-center) (length x-center)))
    (draw-circle pane (svref y-center n) (svref x-center n)
		 radius
		 :line-style line-style
		 :line-width line-width
		 :fill-p fill-p
		 :foreground foreground)))
  ;; (let ((ncircles (length y-center))
  ;; 	(filled (if fill-p 1 0)))
  ;;   (when foreground (set-window-color pane foreground))
  ;;   (GL:with-GL-lock
  ;; 	(internal-draw-circles y-center x-center radius
  ;; 			       filled
  ;; 			       ncircles))))
  


;;;
;;; GL specific draw-graph routine
;;;
(defmethod draw-graph ((drawable nk-pane)
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
  (declare (ignore y-offset x-offset y-label x-label)
	   (optimize (debug 3)))
  ;;(vom:info "[draw-graph]")
  (let ((x-axis-pos (transform-point graph->frob-y (or x-axis (apply 'clip 0 y-range)))))
    
    ;;(when axis-color (set-window-color drawable axis-color))
    (when x-axis
      (plot-x-axis drawable graph->frob-x (transform-point graph->frob-y x-axis)
		   x-range x-tick-step x-tick-length x-tick-gap
		   x-tick-format-string font))
    (when y-axis
      (plot-y-axis drawable graph->frob-y (transform-point graph->frob-x y-axis)
		   y-range y-tick-step y-tick-length y-tick-gap
		   y-tick-format-string font))

    ;;(when color (set-window-color drawable color))
    ;;(when line-width (GL:with-GL-lock (GL:pntsize (ceiling line-width))))

    ;;;
    ;;; Draw only a portion of the graph by clipping.
    ;;;
;;    (break)
    ;; (set-2d-viewport drawable
    ;; 		     :top (transform-point graph->frob-y (cadr y-range))
    ;; 		     :left (transform-point graph->frob-x (car x-range))
    ;; 		     :bottom (transform-point graph->frob-y (car y-range))
    ;; 		     :right (transform-point graph->frob-x (cadr x-range)))
    (plot-data graph-type data drawable
	       data->frob-y data->frob-x x-axis-pos
	       plot-symbol fill-symbol-p symbol-size)))

    ;;;
    ;;; Restore entire 2D-viewport
    ;;;
;;;    (set-2d-viewport drawable)))
