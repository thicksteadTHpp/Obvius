;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-8-bit.lisp
;;;  Author: Patrick C. Teo
;;;  Description: 8-bit screen specific stuff
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GL 8-bit Screen
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; "8bit-GL-screen" is the GL-screen implementation for an
;;; Indigo machine with an 8 bit screen.  All GL-screens
;;; have Gray/Color LUT's to allow gamma ramps.  For 8bit
;;; screens, the LUT values contain colormap indices (for which
;;; a colormap has to be maintained as well).  For 24bit
;;; screens, however, the LUT values contain the actual color values
;;; allowed (e.g. for gray it would be 0-255, for color it would be
;;; 0-255 for R/G/B separately).

(def-simple-class 8bit-GL-screen (GL-screen 8bit-GL-colormap) ()
  (:default-initargs
      :foreground :white                ;; colormap index for white color
      :background :blue                 ;; colormap index for black color
      :depth 8                          ;; depth of the screen
      :gray-depth 8                     ;; gray-depth of the screen
      :gray-shades 128                  ;; number of gray levels
      :gray-gamma 0.9                   ;; gamma ramp
      :rgb-bits '(2 2 2)                ;; number of bits of red/green/blue to be dithered to
      :rgb-gamma '(1.0 1.0 1.0)         ;; gamma ramp for each color channel
      :pseudo-colors 7                  ;; number of colors reserved for pseudo-color pictures
      :pseudo-color-function #'pseudo-color-ramp))  ;; default pseudo-color generation function


(defmethod settable-parameters ((class-name (eql '8bit-GL-screen)))
  (call-next-method))

(defmethod activate-screen :after ((screen 8bit-GL-screen))
  (unless (activated-p screen)
    (initialize-colormap screen)
    (set-gray-lut screen)
    (set-pseudo-color-lut screen)       ;; allocate pseudo-color first
    (set-color-lut screen)
    (setf (activated-p screen) t)))

;;; deallocate gray-lut and color-lut
(defmethod destroy :after ((screen 8bit-GL-screen) &key &allow-other-keys)
  (with-slots (gray-lut color-lut pseudo-color-lut) screen
    (destroy-gray-lut screen)
    (destroy-color-lut screen)
    (destroy-pseudo-color-lut screen)))


;;;===================================================================================
;;;
;;; Convert color description to GL colormap index
;;;
;;; (compute-GL-color) specializes to the current screen so as to determine
;;; a meaningful color representation (colormap index for 8bit and color value for 24bit
;;; screens respectively).
;;;
;;; Warning:: No error checking being done!
;;;
;;;===================================================================================

(defparameter *color-symbol-to-GL-color-map-index*
  `((:black   . ,GL:black)
    (:red     . ,GL:red)
    (:green   . ,GL:green)
    (:yellow  . ,GL:yellow)
    (:blue    . ,GL:blue)
    (:magenta . ,GL:magenta)
    (:cyan    . ,GL:cyan)
    (:white   . ,GL:white)))

(defmethod compute-GL-color ((screen 8bit-GL-screen) (color symbol))
  (let ((result (assoc color *color-symbol-to-GL-color-map-index*)))
    (if (null result) GL:white (cdr result))))

(defmethod compute-GL-color ((screen 8bit-GL-screen) (RGB-list cons))
  (make-color-if-necessary screen
			   :red (first RGB-list) :green (second RGB-list) :blue (third RGB-list)))

(defmethod compute-GL-color ((screen 8bit-GL-screen) (cindex number))
  cindex)




;;;===================================================================================
;;;
;;; 8-bit Gray Lookup Table
;;;
;;; While both 8bit and 24bit GL screens have Gray LUT's.  Each maintain their
;;; own LUT's differently.  Because colormaps are required for the 8bit GL screens,
;;; maintaining a Gray LUT requires allocating/deallocating colormap indices.  As
;;; a result, the LUT values are actually colormap indices.
;;;
;;;===================================================================================

;;;
;;; (setf gray-shades) methods to adjust the gray-scale lut when the gray-levels are changed.
;;;
(defmethod (setf gray-shades) :around (levels (screen 8bit-GL-screen))
  (cond ((< levels 2) (error "Too few gray shades."))
	((> levels (gray-depth screen)) (error "Too many gray shades."))
	(t (set-gray-lut screen :gray-shades levels))))

;;;
;;; (setf gray-gamma) method to adjust the gray-scale lut when the gray-gamma is changed.
;;;
(defmethod (setf gray-gamma) :around (new-gamma (screen 8bit-GL-screen))
  (set-gray-lut screen :gray-gamma new-gamma))


;;;
;;; Deallocate and destroy gray lut
;;;
(defmethod destroy-gray-lut ((screen 8bit-GL-screen))
  (with-slots (gray-lut) screen
    (when gray-lut
      (loop for i from 0 below (total-size gray-lut) do
	    (free-colormap-cell screen (aref gray-lut i) :warn nil))
      ;(free-array gray-lut)
      (setf gray-lut nil))))

;;;
;;; This is the main routine.  This creates a new gray lut by first deallocating
;;; the old gray lut and then reallocating color cells for each gray-level desired.
;;;
(defmethod set-gray-lut ((screen 8bit-GL-screen)
			 &key
			 (gray-shades (gray-shades screen))
			 (gray-gamma (gray-gamma screen)))

  (format t ";;; (Re-)allocating ~d grayscales...~%" gray-shades)
  
  ;; if old lut exists deallocate non-reserved colors
  (destroy-gray-lut screen)

  ;; reallocating gray-scales
  (let ((new-lut (make-array gray-shades :element-type '(unsigned-byte 8)))
	(cmap-overflow nil))
    (loop for i from 0 below gray-shades
	  for intensity = (expt (/ i (1- gray-shades)) gray-gamma)
	  for color = (make-color-if-necessary screen :red intensity :green intensity :blue intensity)
	  until (setq cmap-overflow (and (null color) (> i 2) i))  ;; we won't bother if there are too few colors
	  do
	  (setf (aref new-lut i) color))
    
    ;;; check if we had an overflow
    (if cmap-overflow
	(progn
	  (warn "Failure to allocate ~d gray shades!" gray-shades)
	  ;; now set it with the number of colors that we succeeded with this time
	  (destroy-gray-lut screen)
	  (set-gray-lut screen
			:gray-shades cmap-overflow
			:gray-gamma gray-gamma))

	(setf (slot-value screen 'gray-shades) gray-shades  ;; to by-pass the (setf gray-shades) method!!
	      (slot-value screen 'gray-gamma) gray-gamma
	      (gray-lut screen) new-lut)))
  
  (set-not-current screen))

;;;
;;; Simple debugging routine to print the Gray LUT
;;;
(defmethod print-gray-lut ((screen 8bit-GL-screen))
  (with-slots (gray-shades gray-lut) screen
    (loop for i from 0 below gray-shades do
	  (let* ((cmindex (aref gray-lut i))
		 (cmap-cell (find-colormap-cell screen cmindex))
		 (R-value (red cmap-cell))
		 (G-value (green cmap-cell))
		 (B-value (blue cmap-cell)))
	    (format t "Gray level = ~d  => color index = ~d (R=~d,G=~d,B=~d)~%"
		    i cmindex R-value G-value B-value)))
    (format t "~%Number of entries = ~d~%" gray-shades)))






;;;===================================================================================
;;;
;;; 8-bit Color Lookup Table
;;; 
;;; Similar to the 8-bit Gray LUT, 8bit-GL-screens have to maintain the LUT and
;;; actually allocate/deallocate colormap indices.   Currently, gamma ramps have not
;;; been implemented.
;;;
;;;===================================================================================

;;;
;;; (setf rgb-bits) methods adusts the color-LUT when the RGB bit allocation are changed.
;;;
(defmethod (setf rgb-bits) ((RGB-list cons) (screen 8bit-GL-screen))
  (unless (and (plusp (first RGB-list)) (plusp (second RGB-list)) (plusp (third RGB-list)))
    (error "Must be at least 1 bit for each R, G and B"))
  (when (> (apply '+ RGB-list) 7)
    (error "Cannot allocate more than 7 bits of color."))
  (set-color-lut screen :rgb-bits RGB-list))


;;;
;;; (setf rgb-gamma) sets the gamma exponent for each color channel.
;;;
(defmethod (setf rgb-gamma) ((RGB-gamma cons) (screen 8bit-GL-screen))
  (set-color-lut screen :rgb-gamma RGB-gamma))


;;;
;;; Deallocate and destroy color lut
;;;
(defmethod destroy-color-lut ((screen 8bit-GL-screen))
  (with-slots (color-lut) screen
    (when color-lut
      (loop for i from 0 below (total-size color-lut) do
	    (free-colormap-cell screen (aref color-lut i) :warn nil))
      ;(free-array color-lut)
      (setf color-lut nil))))
  

;;;
;;; This is the main routine for Color LUT's.  It deallocate the old LUT and reallocates
;;; a new LUT based on the RGB bit allocation.
;;;
(defmethod set-color-lut ((screen 8bit-GL-screen)
			  &key
			  (rgb-bits (rgb-bits screen))
			  (rgb-gamma (rgb-gamma screen)))

  (with-slots (color-lut) screen
    (let* ((red-levels (expt 2 (first rgb-bits)))
	   (green-levels (expt 2 (second rgb-bits)))
	   (blue-levels (expt 2 (third rgb-bits)))
	   (red-gamma (first rgb-gamma))
	   (green-gamma (second rgb-gamma))
	   (blue-gamma (third rgb-gamma))
	   (cmap-overflow nil)
	   (new-lut (make-array (* red-levels green-levels blue-levels) :element-type '(unsigned-byte 8))))
      
      (format t ";;; (Re-)allocating ~d by ~d by ~d...~%" red-levels green-levels blue-levels)
      
      ;; if old lut exists deallocate non-reserved colors
      (destroy-color-lut screen)
      
      ;; allocate new color lut
      (loop for i from 0 below (total-size new-lut)
	    for rval = (float (expt (/ (mod i red-levels) (1- red-levels)) red-gamma))
	    for gval = (float (expt (/ (mod (floor i red-levels) green-levels) (1- green-levels)) green-gamma))
	    for bval = (float (expt (/ (mod (floor i (* red-levels green-levels)) blue-levels) (1- blue-levels))
				    blue-gamma))
	    for color = (make-color-if-necessary screen :red rval :green gval :blue bval)
	    until (setq cmap-overflow (and (null color) (> i 2) i))
	    do
	    (setf (aref new-lut i) color))
      
      ;; check if we had an overflow
      (if cmap-overflow
	  (progn
	    (destroy-color-lut screen)
	    (error "Failure to allocate colors, only ~d colors available, please adjust rgb-bits." cmap-overflow))
	  (setf color-lut new-lut
		(slot-value screen 'rgb-bits) rgb-bits)))
    
    (set-not-current screen)))
	  



;;;===================================================================================
;;;
;;; 8-bit Pseudo Color Lut
;;; 
;;;===================================================================================

;;;
;;; (setf pseudo-colors) sets the number of pseudo colors to be allocated.
;;;
(defmethod (setf pseudo-colors) (num (screen 8bit-GL-screen))
  (set-pseudo-color-lut screen :pseudo-colors num))

;;;
;;; (setf pseudo-color-function) changes the pseudo color function
;;; which returns a list (R-val G-val B-val) given a pseudo-color index.
;;;
(defmethod (setf pseudo-color-function) (function (screen 8bit-Gl-screen))
  (unless (fboundp function)
    (error "~a must be a symbol with a function binding" function))
  (setf function (symbol-function function))
  (set-pseudo-color-lut screen :pseudo-color-function function))

;;;
;;; Deallocate and destroy pseudo color lut.
;;;
(defmethod destroy-pseudo-color-lut ((screen 8bit-GL-screen))
  (with-slots (pseudo-color-lut) screen
    (when pseudo-color-lut
      (loop for i from 0 below (total-size pseudo-color-lut) do
	    (free-colormap-cell screen (aref pseudo-color-lut i) :warn nil))
      ;(free-array pseudo-color-lut)
      (setf pseudo-color-lut nil))))


;;;
;;; Allocate a new pseudo-color-lut.
;;;
(defmethod set-pseudo-color-lut ((screen 8bit-GL-screen)
				 &key
				 (pseudo-colors (pseudo-colors screen))
				 (pseudo-color-function (pseudo-color-function screen)))
  (with-slots (pseudo-color-lut) screen
    (let* ((new-lut (make-array pseudo-colors :element-type '(unsigned-byte 8)))
	   (cmap-overflow nil))

      (format t ";;; (Re-)allocating ~a pseudo colors...~%" pseudo-colors)

      ;; If old lut exists, de-allocate non-reserved colors
      (destroy-pseudo-color-lut screen)

      ;; allocate new pseudo color lut
      (loop for i from 0 below (total-size new-lut)
	    for rgb-vals = (funcall pseudo-color-function i pseudo-colors)
	    for rval = (first rgb-vals)
	    for gval = (second rgb-vals)
	    for bval = (third rgb-vals)
	    for color = (make-color-if-necessary screen :red rval :green gval :blue bval)
	    until (setq cmap-overflow (and (null color) i))
	    do
	    (setf (aref new-lut i) color))

      ;; check if we had an overflow
      (if cmap-overflow
	  (progn
	    (destroy-pseudo-color-lut screen)
	    (error "Failure to allocate colors, only ~d colors available, please adjust psudo-colors." cmap-overflow))
	  (setf pseudo-color-lut new-lut
		(slot-value screen 'pseudo-colors) pseudo-colors
		(slot-value screen 'pseudo-color-function) pseudo-color-function)))

    (set-not-current screen)))
  

;;;
;;; Example pseudo-color-function:
;;;   Given an index and a size returns a list '(R-val G-val B-val) where
;;;   each component is an intensity value between 0.0 and 1.0.
;;;
(defun pseudo-color-ramp (i size)
  (let* ((rval (/ i (- size 1)))
	 (gval (- 1 rval))
	 (bval (* 1/2 (- 1 (abs (- (* 2 rval) 1))))))
    (list rval gval bval)))



;;;===================================================================================
;;;
;;; 8-bit True Color Pane Dependent Lut
;;; 
;;;===================================================================================

;;;
;;; Fill picture dependent color-table with true color indices
;;;
(defmethod fill-color-table ((screen 8bit-GL-screen) pic
			     r-bytes g-bytes b-bytes)
  (declare (type (array (unsigned-byte 8) (* *)) r-bytes g-bytes b-bytes))
  (with-slots (color-table) pic
    (let ((cmap-overflow nil) color)
      (loop for y from 0 below (y-dim r-bytes)
	    until cmap-overflow
	    do
	    (loop for x from 0 below (x-dim r-bytes)
		  for rval = (aref r-bytes y x)
		  for gval = (aref g-bytes y x)
		  for bval = (aref b-bytes y x)
		  for hash-index = (+ (* 65536 rval) (* 256 gval) bval)
		  for hash-value = (gethash hash-index color-table)
		  until cmap-overflow
		  do
		  (unless hash-value
		    (setq color (make-color-if-necessary screen
							 :red (float (/ rval 256))
							 :green (float (/ gval 256))
							 :blue (float (/ bval 256))))

		    (if color
			(setf (gethash hash-index color-table) color)
			(progn
			  (warn "Failed to allocate requested colors")
			  (setq cmap-overflow t))))
		  )))
    (set-not-current screen)
    color-table))


;;;
;;; Destroys the picture dependent color-table
;;;
(defmethod destroy-color-table ((screen 8bit-GL-screen) pic)
  (with-slots (color-table) pic
    (maphash
     #'(lambda (key value)
	 (declare (ignore key))
	 (free-colormap-cell screen value :warn nil))
     color-table)))




;;;;===================================================================================
;;;;
;;;; 8-bit Blting
;;;;
;;;;===================================================================================

;;;
;;; Computes a bltable that is handled by this screen depth
;;;
(defmethod make-bltable ((screen 8bit-GL-screen) base-dimensions
			 &rest initargs
			 &key
			 (depth (depth screen))
			 (foreground (foreground screen))
			 (background (background screen))
			 bltable)
  (remf initargs :bltable)
  (unless (and (typep bltable '8bit-GL-bltable)
	       (eq screen (screen-of bltable))
	       (= (depth bltable) depth)
	       (equal (base-dimensions bltable) base-dimensions))

    (let ((bltable-class (cond ((= depth 8) '8bit-GL-bltable)
			       ((= depth 1) '1bit-GL-bltable)
			       (t (error "Can't handle depth ~d" depth)))))
      (when bltable (destroy bltable))
      (setq bltable
	    (make-instance bltable-class
			   :screen-of screen
			   :foreground foreground
			   :background background
			   :base-dimensions base-dimensions
			   :dimensions base-dimensions))))
  (setf (image bltable) nil)
  bltable)

;;;
;;; Computes 8bit-GL-image of 1bit-GL-bltable
;;; (Handles zooming)
;;;
(defmethod compute-bltable-GL-image ((window 8bit-GL-window) (bltable 1bit-GL-bltable) zoom)
  (with-slots (data screen-of foreground background depth image dimensions base-dimensions) bltable
    (setf dimensions (list (ceiling (* (car base-dimensions) zoom))
			   (ceiling (* (cadr base-dimensions) zoom))))
    (when (null image)
      (setf image (similar data
			   :element-type '(unsigned-byte 16)
			   :dimensions dimensions :initial-element 0)))
    (cond ((= zoom 1)
	   (internal-paste-1bit-to-16bit data (x-dim data) (y-dim data)
					 image (x-dim image)
					 0 0
					 (second dimensions) (first dimensions)
					 background foreground))
	  ((> zoom 1)
	   (internal-supersample-1bit-to-16bit data (x-dim data) (y-dim data)
					       image (x-dim image)
					       0 0          ; dest starting corner
					       (second dimensions) (first dimensions)
					       (round zoom)
					       background foreground))
	  (t (internal-subsample-1bit-to-16bit data (x-dim data) (y-dim data)
					       image (second dimensions)
					       (round (/ 1 zoom))
					       background foreground)))))


;;;
;;; Computes 8bit-GL-image of 8bit-GL-bltable
;;; (Handles zooming)
;;;
(defmethod compute-bltable-GL-image ((window 8bit-GL-window) (bltable 8bit-GL-bltable) zoom)
  (with-slots (data screen-of depth image dimensions base-dimensions) bltable
    (setf dimensions (list (ceiling (* (car base-dimensions) zoom))
			   (ceiling (* (cadr base-dimensions) zoom))))
    (when (null image)
      (setf image (similar data
			   :element-type '(unsigned-byte 16)
			   :dimensions dimensions :initial-element 0)))
    (cond ((= zoom 1)
	   (internal-paste-8bit-to-16bit data (x-dim data) (y-dim data)
					 0 0
					 image (x-dim image)
					 0 0
					 (second dimensions) (first dimensions)))
	  ((> zoom 1)
	   (internal-supersample-8bit-to-16bit data (x-dim data) (y-dim data)
					       0 0          ; source starting corner
					       image (x-dim image)
					       0 0          ; dest starting corner
					       (second dimensions) (first dimensions)
					       (round zoom)))
	  (t (internal-subsample-8bit-to-16bit data (x-dim data) (y-dim data)
					       image (second dimensions)
					       (round (/ 1 zoom)))))))

;;;
;;; Generic render routine for 8bit GL screens
;;;
(defmethod render ((window 8bit-GL-window) (bltable GL-bltable) y-offset x-offset zoom)
  (with-slots (image pane->frob-y pane->frob-x dimensions base-dimensions) bltable
    (when (and image (/= (cadr dimensions) (ceiling (* (cadr base-dimensions) zoom))))
      (free-array image)
      (setf image nil))
    (compute-bltable-GL-image window bltable zoom)
    (let ((row (+ (floor (- (y-dim window) (y-dim bltable)) 2) y-offset))
	  (row-adjusted (- (floor (- (y-dim window) (y-dim bltable)) 2) y-offset))
	  (col (+ (floor (- (x-dim window) (x-dim bltable)) 2) x-offset)))
      (setf (slot-value pane->frob-y 'offset) (/ (- row) zoom)
	    (slot-value pane->frob-x 'offset) (/ (- col) zoom)
	    (slot-value pane->frob-y 'scale) (/ zoom)
	    (slot-value pane->frob-x 'scale) (/ zoom))
      (GL:with-GL-lock
	(GL:winset (wid window))
	(GL:rectwrite col row-adjusted
		      (1- (+ col (x-dim image)))
		      (1- (+ row-adjusted (y-dim image)))
		      image)))))





