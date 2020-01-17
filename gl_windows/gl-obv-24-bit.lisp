;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-24-bit.lisp
;;;  Author: Patrick C. Teo
;;;  Description: 24-bit screen specific stuff
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GL 24-bit Screen
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class 24bit-GL-screen (GL-screen) ()
  (:default-initargs
      :foreground :white                ;; colormap index for white color
      :background :blue                 ;; colormap index for black color
      :depth 24                         ;; depth of the screen
      :gray-depth 8                     ;; gray level depth of the screen
      :gray-shades 256                  ;; number of gray levels
      :gray-gamma 0.9                   ;; gamma ramp for gray
      :rgb-bits '(8 8 8)                ;; number of bits of red/green/blue
      :rgb-gamma '(1.0 1.0 1.0)         ;; gamma ramp for each color channel
      :pseudo-colors 7                  ;; number of colors reserved for pseudo-color pictures
      :pseudo-color-function #'pseudo-color-ramp))  ;; default pseudo-color generation function


(defmethod settable-parameters ((class-name (eql '24bit-GL-screen)))
  (call-next-method))

(defmethod activate-screen :after ((screen 24bit-GL-screen))
  (unless (activated-p screen)
    (set-gray-lut screen)
    (set-pseudo-color-lut screen)
    (set-color-lut screen)
    (setf (activated-p screen) t)))

(defmethod destroy :after ((screen 24bit-GL-screen) &key &allow-other-keys)
  (with-slots (gray-lut) screen
    (destroy-gray-lut screen)
    (destroy-color-lut screen)
    (destroy-pseudo-color-lut screen)))



;;;===================================================================================
;;;
;;; Convert color description to GL colormap index
;;;
;;; Warning:: No error checking being done!
;;;
;;;===================================================================================


(defparameter *color-symbol-to-GL-color-RGBA*
  `((:black   . #x00000000)
    (:red     . #x000000ff)
    (:green   . #x0000ff00)
    (:yellow  . #x0000ffff)
    (:blue    . #x00ff0000)
    (:magenta . #x00ff00ff)
    (:cyan    . #x0000ffff)
    (:white   . #x00ffffff)))

(defmethod compute-GL-color ((screen 24bit-GL-screen) (color symbol))
  (let ((result (assoc color *color-symbol-to-GL-color-RGBA*)))
    (if (null result) #x00ffffff (cdr result))))


(defmethod compute-GL-color ((screen 24bit-GL-screen) (RGB-list cons))
  (+ (first RGB-list)
     (* (second RGB-list) 256)
     (* (third RGB-list) 65536)))


(defmethod compute-GL-color ((screen 24bit-GL-screen) (cpack number))
  cpack)



;;;;===================================================================================
;;;;
;;;; 8-bit Gray Lookup Table
;;;; Fake Gray LUT as we have all the gray shades possible.  LUT used
;;;; primarily as a LUT for the gamma ramp.
;;;;
;;;;===================================================================================


;;; (setf gray-shades) methods to adjust the gray-scale lut
;;; when the gray-levels are changed.
;;; not too much error checking here.  we just check that there
;;; are at least 2 levels and at most the size of the colormap
(defmethod (setf gray-shades) :around (levels (screen 24bit-GL-screen))
  (cond ((< levels 2) (error "Too few gray shades."))
	((> levels (expt 2 (gray-depth screen))) (error "Too many gray shades."))
	(t (set-gray-lut screen :gray-shades levels))))

;;; (setf gray-gamma) method to adjust the gray-scale lut
;;; error checking of new-gamma is done in the class definition!!
(defmethod (setf gray-gamma) :around (new-gamma (screen 24bit-GL-screen))
  (set-gray-lut screen :gray-gamma new-gamma))

;;;
;;; Deallocate and destroy gray lut
;;;
(defmethod destroy-gray-lut ((screen 24bit-GL-screen))
  (with-slots (gray-lut) screen
    (when gray-lut
      ;(free-array gray-lut)
      (setf gray-lut nil))))
  

;;; allocate the gray-scale->cmindex lookup table
(defmethod set-gray-lut ((screen 24bit-GL-screen)
			 &key
			 (gray-shades (gray-shades screen))
			 (gray-gamma (gray-gamma screen)))

  (format t ";;; (Re-)allocating gray LUT (size ~d)...~%" gray-shades)
  
  ;; if old-lut exists deallocate non-reserved colors
  (destroy-gray-lut screen)

  ;; reallocating gray-scales
  (let ((new-lut (make-array gray-shades :element-type '(unsigned-byte 8))))
    (loop for i from 0 below gray-shades
	  for intensity = (expt (/ i (1- gray-shades)) gray-gamma)
	  do
	  (setf (aref new-lut i) (floor (* intensity 255))))

    (setf (slot-value screen 'gray-shades) gray-shades
	  (slot-value screen 'gray-gamma) gray-gamma
	  (gray-lut screen) new-lut))

  (set-not-current screen))





;;;;===================================================================================
;;;;
;;;; 8-bit Color Lookup Table
;;;;
;;;;===================================================================================

;;;
;;; (setf rgb-bits) sets the number of bits allocated to each color channel.
;;;
(defmethod (setf rgb-bits) ((RGB-list cons) (screen 24bit-GL-screen))
  (unless (and (plusp (first RGB-list)) (plusp (second RGB-list)) (plusp (third RGB-list)))
    (error "Must be at least 1 bit for each R, G and B"))
  (set-color-lut screen :rgb-bits RGB-list))


;;;
;;; (setf rgb-gamma) sets the gamma exponent for each color channel.
;;;
(defmethod (setf rgb-gamma) ((RGB-gamma cons) (screen 24bit-GL-screen))
  (set-color-lut screen :rgb-gamma RGB-gamma))


;;;
;;; Deallocate and destroy each R/G/B color lut
;;;
(defmethod destroy-color-lut ((screen 24bit-GL-screen))
  (with-slots (color-lut) screen
    (when color-lut
      ;(loop for lut in color-lut do (when lut (free-array lut)))
      (setf color-lut nil))))

;;;
;;; We do not need a color LUT when we have true color but we maintain
;;; three separate LUT's to allow the user to vary the number of colors
;;; and to allow ramping on each channel separately.
;;;
(defmethod set-color-lut ((screen 24bit-GL-screen)
			  &key
			  (rgb-bits (rgb-bits screen))
			  (rgb-gamma (rgb-gamma screen)))
  
  (format t ";;; (Re-)allocating color LUT (size R=~d,G=~d,B=~d)...~%"
	  (first rgb-bits) (second rgb-bits) (third rgb-bits))
  
  (destroy-color-lut screen)
  
  (with-slots (color-lut) screen
    (setf color-lut
	  (loop for bits in rgb-bits
		for gamma in rgb-gamma
		for levels = (expt 2 bits)
		for lut = (make-array levels :element-type '(unsigned-byte 8))
		collect
		(loop for i from 0 below levels
		      for intensity = (expt (/ i (1- levels)) gamma)
		      finally (return lut)
		      do
		      (setf (aref lut i) (floor (* intensity 255)))))
    
	  (slot-value screen 'rgb-bits) rgb-bits
	  (slot-value screen 'rgb-gamma) rgb-gamma))
  
  (set-not-current screen))





;;;===================================================================================
;;;
;;; Pseudo Color Lut
;;; 
;;;===================================================================================

;;;
;;; (setf pseudo-colors) sets the number of pseudo colors to be allocated.
;;;
(defmethod (setf pseudo-colors) (num (screen 24bit-GL-screen))
  (set-pseudo-color-lut screen :pseudo-colors num))

;;;
;;; (setf pseudo-color-function) changes the pseudo color function
;;; which returns a list (R-val G-val B-val) given a pseudo-color index.
;;;
(defmethod (setf pseudo-color-function) (function (screen 24bit-GL-screen))
  (unless (fboundp function)
    (error "~a must be a symbol with a function binding" function))
  (setf function (symbol-function function))
  (set-pseudo-color-lut screen :pseudo-color-function function))


;;;
;;; Deallocate and destroy each R/G/B pseudo color lut.
;;;
(defmethod destroy-pseudo-color-lut ((screen 24bit-GL-screen))
  (with-slots (pseudo-color-lut) screen
    (when pseudo-color-lut
      ;(loop for lut in pseudo-color-lut do (when lut (free-array lut)))
      (setf pseudo-color-lut nil))))


;;;
;;; Allocate a new pseudo-color-lut.
;;;   We maintain separate R/G/B luts which are identically
;;;   indexed by the pseudo-color value.
;;;
(defmethod set-pseudo-color-lut ((screen 24bit-GL-screen)
				 &key
				 (pseudo-colors (pseudo-colors screen))
				 (pseudo-color-function (pseudo-color-function screen)))
  (with-slots (pseudo-color-lut) screen
    (let* ((RGB-lut (loop for i from 0 below 3
			  collect (make-array pseudo-colors :element-type '(unsigned-byte 8)))))

      (format t ";;; (Re-)allocating pseudo color LUT (size ~d)...~%" pseudo-colors)

      ;; If old lut exists, de-allocate non-reserved colors
      (destroy-pseudo-color-lut screen)

      ;; allocate new pseudo color lut
      (loop for i from 0 below pseudo-colors
	    for rgb-intensities = (funcall pseudo-color-function i pseudo-colors)
	    do
	    (loop for lut in RGB-lut
		  for intensity in rgb-intensities
		  do
		  (setf (aref lut i) (floor (* intensity 255)))))

      ;; check if we had an overflow
      (setf pseudo-color-lut RGB-lut
	    (slot-value screen 'pseudo-colors) pseudo-colors
	    (slot-value screen 'pseudo-color-function) pseudo-color-function))

    (set-not-current screen)))


;;;===================================================================================
;;;
;;; 24-bit True Color Pane Dependent Lut
;;; 
;;;===================================================================================

;;;
;;; No color table needs to be maintained as we have true
;;; color.
;;;
(defmethod fill-color-table ((screen 24bit-GL-screen) pic
			     r-bytes g-bytes b-bytes)
  (declare (ignore pic r-bytes g-bytes b-bytes)))

(defmethod destroy-color-table ((screen 24bit-GL-screen) pic)
  (declare (ignore pic)))




;;;;===================================================================================
;;;;
;;;; 24-bit Blting
;;;;
;;;;===================================================================================

;;;
;;; Computes a bltable that is handled by this screen depth
;;;
(defmethod make-bltable ((screen 24bit-GL-screen) base-dimensions
			 &rest initargs
			 &key
			 (depth (depth screen))
			 (foreground (foreground screen))
			 (background (background screen))			 
			 bltable)
  (remf initargs :bltable)
  (unless (and (typep bltable 'GL-bltable)
	       (eq screen (screen-of bltable))
	       (= (depth bltable) depth)
	       (equal (base-dimensions bltable) base-dimensions))
    (let ((bltable-class (cond ((= depth 24) '24bit-GL-bltable)
			       ((= depth 8) '8bit-GL-bltable)
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
;;; Computes 24bit-GL-image of 1bit-GL-bltable
;;; (Handles zooming)
;;;
(defmethod compute-bltable-GL-image ((window 24bit-GL-window) (bltable 1bit-GL-bltable) zoom)
  (with-slots (data screen-of foreground background depth image dimensions base-dimensions) bltable
    (setf dimensions (list (ceiling (* (car base-dimensions) zoom))
			   (ceiling (* (cadr base-dimensions) zoom))))
    (when (null image)
      (setf image (similar data
			   :element-type '(unsigned-byte 32)
			   :dimensions dimensions :initial-element 0)))
    (cond ((= zoom 1)
	   (internal-paste-1bit-to-24bit data (x-dim data) (y-dim data)
					 image (x-dim image)
					 0 0
					 (second dimensions) (first dimensions)
					 background foreground))
	  ((> zoom 1)
	   (internal-supersample-1bit-to-24bit data (x-dim data) (y-dim data)
					       image (x-dim image)
					       0 0          ; dest starting corner
					       (second dimensions) (first dimensions)
					       (round zoom)
					       background foreground))
	  (t (internal-subsample-1bit-to-24bit data (x-dim data) (y-dim data)
					       image (second dimensions)
					       (round (/ 1 zoom))
					       background foreground)))))



;;;
;;; Computes 24bit-GL-image of 8bit-GL-bltable
;;; (Handles zooming)
;;;
(defmethod compute-bltable-GL-image ((window 24bit-GL-window) (bltable 8bit-GL-bltable) zoom)
  (warn "8bit-GL-bltable not entirely compatible on 24bit-GL-window")
  (with-slots (data screen-of depth image dimensions base-dimensions) bltable
    (setf dimensions (list (ceiling (* (car base-dimensions) zoom))
			   (ceiling (* (cadr base-dimensions) zoom))))
    (when (null image)
      (setf image (similar data
			   :element-type '(unsigned-byte 32)
			   :dimensions dimensions :initial-element 0)))
    (cond ((= zoom 1)
	   (internal-paste-8bit-to-24bit data (x-dim data) (y-dim data)
					 0 0
					 image (x-dim image)
					 0 0
					 (second dimensions) (first dimensions)))
	  ((> zoom 1)
	   (internal-supersample-8bit-to-24bit data (x-dim data) (y-dim data)
					       0 0          ; source starting corner
					       image (x-dim image)
					       0 0          ; dest starting corner
					       (second dimensions) (first dimensions)
					       (round zoom)))
	  (t (internal-subsample-8bit-to-24bit data (x-dim data) (y-dim data)
					       image (second dimensions)
					       (round (/ 1 zoom)))))))




;;;
;;; Computes 24bit-GL-image of 24bit-GL-bltable
;;; (Handles zooming)
;;;
(defmethod compute-bltable-GL-image ((window 24bit-GL-window) (bltable 24bit-GL-bltable) zoom)
  (with-slots (data screen-of depth image dimensions base-dimensions) bltable
    (setf dimensions (list (ceiling (* (car base-dimensions) zoom))
			   (ceiling (* (cadr base-dimensions) zoom))))
    (when (null image)
      (setf image (similar data
			   :element-type '(unsigned-byte 32)
			   :dimensions dimensions :initial-element 0)))
    (cond ((= zoom 1)
	   (internal-paste-24bit-to-24bit data (x-dim data) (y-dim data)
					  0 0
					  image (x-dim image)
					  0 0
					  (second dimensions) (first dimensions)))
	  ((> zoom 1)
	   (internal-supersample-24bit-to-24bit data (x-dim data) (y-dim data)
					       0 0          ; source starting corner
					       image (x-dim image)
					       0 0          ; dest starting corner
					       (second dimensions) (first dimensions)
					       (round zoom)))
	  (t (internal-subsample-24bit-to-24bit data (x-dim data) (y-dim data)
						image (second dimensions)
						(round (/ 1 zoom)))))))




;;;
;;; Generic render routine for 24bit GL screens
;;;
(defmethod render ((window 24bit-GL-window) (bltable GL-bltable) y-offset x-offset zoom)
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
	(GL:lrectwrite col row-adjusted
		      (1- (+ col (x-dim image)))
		      (1- (+ row-adjusted (y-dim image)))
		      image)))))






#|
;;; OLD

;;; called by both initialize-instance and reinitialize-instance
(defmethod shared-initialize :after ((screen 24bit-GL-screen) slot-names
				     &rest initargs
				     &key foreground background)
  (declare (ignore slot-names initargs))
  (when foreground
    (setf (foreground screen) foreground))
  (when background
    (setf (background screen) background))
  screen)

    

(defmethod (setf foreground) (color-desc (screen 24bit-GL-screen))
  (setf (slot-value screen 'foreground)
	(compute-GL-color screen color-desc)))
  
(defmethod (setf background) (color-desc (screen 24bit-GL-screen))
  (setf (slot-value screen 'background)
	(compute-GL-color screen color-desc)))

(defmethod settable-parameters ((class-name (eql '24bit-GL-screen)))
  (append '(rgb-bits gray-shades gray-gamma gray-dither) (call-next-method)))

|#
