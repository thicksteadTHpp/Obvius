;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-blt.lisp
;;;  Author: Patrick C. Teo
;;;  Description: GL blting routines
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)


;;;
;;; GL images (pixmaps)
;;;
;;; Images are raw 16bit- or 32bit- data which are accepted by
;;; the SGI graphics library routines for blting.
;;;
(def-simple-class GL-image ()
  (width
   height
  (data   :initform nil)))

(def-simple-class 8bit-GL-image (GL-image) ())
(def-simple-class 24bit-GL-image (GL-image) ())

(defmethod dimensions ((image GL-image))
  (list (height image) (width image)))

(defmethod x-dim ((image GL-image)) (width image))
(defmethod y-dim ((image GL-image)) (height image))


;;;
;;; GL Bltables
;;;
;;; Bltables are somewhat midway between the actual float data
;;; and the images (above).  For example, a bitmap will be a 1bit-bltable;
;;; when it is displayed, it is converted into a GL-image (a 16bit- GL-image
;;; or a 32bit- GL-image depending on the screen depth).  Similarly,
;;; a 8bit gray image is blted into an 8bit-bltable; when it is displayed,
;;; it is also converted to a corresponding GL-image.
;;;
(def-simple-class GL-bltable (frob)
  (screen-of					;; screen of bltable
   depth                                        ;; depth of bltable
   foreground 
   background 
   dimensions					;; zoomed dimensions
   base-dimensions				;; unzoomed dimensions
   (data          :initform nil)		;; raw data of image
   (frob-x-offset :initform 0)
   (frob-y-offset :initform 0)
   image)					;; GL pixmap used to display 
  (:default-initargs
      :pane->frob->y (make-transform :coerce #'floor)
      :pane->frob->x (make-transform :coerce #'floor)))

;;;
;;; Specialized for each type of screen
;;;

(defgeneric make-bltable (GL-screen base-dimensions &rest initargs &key bltable))

(defmethod static-arrays-of ((bltable GL-bltable)) (list (data bltable) (image bltable)))

(defmethod destroy :after ((bltable GL-bltable) &key &allow-other-keys)
  (when (data bltable) (free-array (data bltable)))
  (when (image bltable) (free-array (image bltable))))

;;; called by both initialize-instance and reinitialize-instance
(defmethod shared-initialize :after ((bltable GL-bltable) slot-names
				     &rest initargs
				     &key foreground background)
  (declare (ignore slot-names initargs))
  (when foreground
    (setf (foreground bltable) foreground))
  (when background
    (setf (background bltable) background))
  bltable)

;;;
(defmethod (setf foreground) (color-desc (bltable GL-bltable))
  (setf (slot-value bltable 'foreground)
	(compute-GL-color (screen-of bltable) color-desc)))
  
(defmethod (setf background) (color-desc (bltable GL-bltable))
  (setf (slot-value bltable 'background)
	(compute-GL-color (screen-of bltable) color-desc)))





;;;
;;; 1bit/8bit/24bit-GL Bltables
;;;
(def-simple-class 1bit-GL-bltable (GL-bltable) ()
  (:default-initargs :depth 1))

(def-simple-class 8bit-GL-bltable (GL-bltable) ()
  (:default-initargs :depth 8))

(def-simple-class 24bit-GL-bltable (GL-bltable) ()
  (:default-initargs :depth 24))



(defmethod initialize-instance :after ((bltable 1bit-GL-bltable) &rest initargs)
   (declare (ignore initargs))	   
   (with-slots (data base-dimensions) bltable
     (setf data (allocate-array base-dimensions :element-type 'bit :initial-element 0))))

(defmethod initialize-instance :after ((bltable 8bit-GL-bltable) &rest initargs)
   (declare (ignore initargs))	   	   
   (with-slots (data base-dimensions) bltable
     (setf data (allocate-array base-dimensions :element-type '(unsigned-byte 8) :initial-element 0))))

(defmethod initialize-instance :after ((bltable 24bit-GL-bltable) &rest initargs)
   (declare (ignore initargs))
   (with-slots (data base-dimensions) bltable
     (setf data (allocate-array base-dimensions :element-type '(unsigned-byte 32) :initial-element 0))))



;;;
;;; Clear GL bltables
;;;

(defmethod clear ((bltable 1bit-GL-bltable) &key color)
  (declare (ignore color))
  (fill! (data bltable) 0))

(defmethod clear ((bltable 8bit-GL-bltable) &key
		  (color (background bltable)))
  (fill! (data bltable) (compute-GL-color (screen-of bltable) color)))

(defmethod clear ((bltable 24bit-GL-bltable) &key
		  (color (background bltable)))
  (fill! (data bltable) (compute-GL-color (screen-of bltable) color)))



;;;
;;; Draw float arrays onto GL bltables
;;;

(defmethod draw-float-array ((bltable 1bit-GL-bltable) float-array
			     pedestal scale zoom
			     y-offset x-offset)
  (declare (ignore zoom))
  (with-slots (data screen-of depth) bltable
    (setq scale (/-0 1.0 scale 1.0))
    (internal-dither-into-1bit float-array (x-dim float-array) (y-dim float-array)
			       data (x-dim data)
			       (float pedestal) (float (* scale 2.0))
			       (round x-offset) (round y-offset)))
  bltable)

(defmethod draw-float-array ((bltable 8bit-GL-bltable) float-array
			     pedestal scale zoom
			     y-offset x-offset)
  (declare (ignore zoom))
  (with-slots (data screen-of depth) bltable
    (let* ((lut (gray-lut screen-of))
	   (lut-size (length lut)))

      (when (null lut)
	(error "Unable to draw float arrays -- graymap not allocated"))
      
      (setq scale (/-0 1.0 scale 1.0))
      (internal-f-into-8bit-lut float-array (x-dim float-array) (y-dim float-array)
				data (x-dim data)
				(float pedestal) (float (* scale lut-size))
				(round x-offset) (round y-offset)
				lut lut-size)))
  bltable)

(defmethod draw-float-array ((bltable 24bit-GL-bltable) float-array
			     pedestal scale zoom
			     y-offset x-offset)
  (declare (ignore zoom))
  (with-slots (data screen-of depth) bltable
    (let* ((lut (gray-lut screen-of))
	   (lut-size (length lut)))
      
      (when (null lut)
	(error "Unable to draw float arrays -- graymap not allocated"))

      (setq scale (/-0 1.0 scale 1.0))
      (internal-gray-f-into-24bit-lut float-array (x-dim float-array) (y-dim float-array)
				      data (x-dim data)
				      (float pedestal) (float (* scale lut-size))
				      (round x-offset) (round y-offset)
				      lut lut-size)))
  bltable)

    



;;;
;;; Renders RGB-color-float-arrays onto an 8bit-GL-bltable
;;; (Dithering involved)
;;;
(defmethod draw-color-float-arrays ((bltable 8bit-GL-bltable)
				    R-float-array G-float-array B-float-array
				    pedestal scale zoom
				    x-offset y-offset)
  (declare (ignore zoom))
  (with-slots (screen-of data) bltable
    (with-slots (color-lut rgb-bits) screen-of

      (when (null color-lut)
	(error "Unable to draw color float arrays -- colormap not allocated"))

      (let* ((red-levels (expt 2 (first (rgb-bits screen-of))))
	     (green-levels (expt 2 (second (rgb-bits screen-of))))
	     (blue-levels (expt 2 (third (rgb-bits screen-of))))
	     (tmp-r-lut (make-array red-levels :element-type '(unsigned-byte 8)))
	     (tmp-g-lut (make-array green-levels :element-type '(unsigned-byte 8)))	   
	     (tmp-b-lut (make-array blue-levels :element-type '(unsigned-byte 8))))
	
	(setq scale (/-0 1.0 scale 1.0))
	
        ;;; Initialize temporary LUTS --
        ;;; Green and blue LUTS are shifted accordingly. 
	(loop for i from 0 below red-levels do (setf (aref tmp-r-lut i) i))
	(loop for i from 0 below green-levels do (setf (aref tmp-g-lut i) (* red-levels i)))
	(loop for i from 0 below blue-levels do (setf (aref tmp-b-lut i) (* red-levels green-levels i)))
	
        ;;;
        ;;; Dither colors into quantized color values.
        ;;;
	(with-status-message "Dithering colors"
	  (with-static-arrays ((tmp-data (similar data))
			       (tmp-8bit-array
				(allocate-array (dimensions data) :element-type '(unsigned-byte 8))))

	    (zero! tmp-data)

	    (loop for float-array in `(,r-float-array ,g-float-array ,b-float-array)
		  for lut in `(,tmp-r-lut ,tmp-g-lut ,tmp-b-lut)
		  do
		  (zero! tmp-8bit-array)

		  ;; same pedestal and scale for RGB??
		  ;; dither each channel separately??
		  (internal-dither-into-8bit-lut float-array (x-dim float-array) (y-dim float-array)
						 tmp-8bit-array (x-dim tmp-8bit-array)
						 (float pedestal) (float (* scale (length lut)))
						 (round x-offset) (round y-offset)
						 lut (total-size lut))
		  (add tmp-8bit-array tmp-data :-> tmp-data))
	
            ;;;
            ;;; Convert quantized color values to GL color map indices.
            ;;;
	    (with-status-message "Converting to color map indices"
	      (loop for j from 0 below (y-dim data) do
		    (loop for i from 0 below (x-dim data) do
			  (setf (aref data j i)
				(aref color-lut (aref tmp-data j i)))))))))))
  bltable)
  

;;;
;;; Renders RGB-color-float-arrays onto a 24bit-GL-bltable
;;; (True color -- no dithering involved although we might want to
;;;  put in a key to force dithering)
;;;
(defmethod draw-color-float-arrays ((bltable 24bit-GL-bltable)
				    R-float-array G-float-array B-float-array
				    pedestal scale zoom
				    x-offset y-offset)
  (declare (ignore zoom))
  (with-slots (screen-of data) bltable
    (with-slots (color-lut) screen-of

      (when (null color-lut)
	(error "Unable to draw color float arrays -- colormap not allocated"))

	(setq scale (/-0 1.0 scale 1.0))

        ;;;
        ;;; Make 24bit image from the three color ramps
        ;;;
	(let ((R-lut (first color-lut))
	      (G-lut (second color-lut))
	      (B-lut (third color-lut)))

	  (internal-color-f-into-24bit-lut R-float-array G-float-array B-float-array
					   (x-dim R-float-array) (y-dim R-float-array)
					   data (x-dim data)
					   (float pedestal) (float (* scale (total-size R-lut)))
					   (round x-offset) (round y-offset)
					   R-lut G-lut B-lut (total-size R-lut)))))
  bltable)






;;;
;;; Renders a float array using the pseudo-color map.
;;;
(defmethod draw-pseudo-color-float-array ((bltable 8bit-GL-bltable)
					  float-array
					  pedestal scale zoom
					  y-offset x-offset)
  (declare (ignore zoom))
  (with-slots (screen-of data) bltable
    (let* ((lut (pseudo-color-lut screen-of))
	   (lut-size (total-size lut)))
      
      (when (null lut)
	(error "Unable to draw pseudo color arrays -- pseudo-color-map not allocated"))

      (setq scale (/-0 1.0 scale 1.0))
      (internal-f-into-8bit-lut float-array (x-dim float-array) (y-dim float-array) 
				data (x-dim data)
				(float pedestal) (float (* scale lut-size))
				(round x-offset) (round y-offset)
				lut lut-size)))
  bltable)


(defmethod draw-pseudo-color-float-array ((bltable 24bit-GL-bltable)
					  float-array
					  pedestal scale zoom
					  y-offset x-offset)
  (declare (ignore zoom))
  (with-slots (screen-of data) bltable
    (let* ((RGB-lut (pseudo-color-lut screen-of)))
      
      (when (null RGB-lut)
	(error "Unable to draw pseudo color arrays -- pseudo-color-map not allocated"))

      (setq scale (/-0 1.0 scale 1.0))
      (let ((R-lut (first RGB-lut))
	    (G-lut (second RGB-lut))
	    (B-lut (third RGB-lut)))
	
	(internal-color-f-into-24bit-lut float-array float-array float-array
					 (x-dim float-array) (y-dim float-array) 
					 data (x-dim data)
					 (float pedestal) (float (* scale (total-size R-lut)))
					 (round x-offset) (round y-offset)
					 R-lut G-lut B-lut (total-size R-lut)))))
  bltable)







;;; 
;;; Renders true color R/G/B values into an 8bit-GL-bltable
;;; 
(defmethod draw-true-color-array ((bltable 8bit-GL-bltable)
				  r-bytes g-bytes b-bytes color-table
				  zoom x-offset y-offset)
  (declare (ignore zoom x-offset y-offset))
  (declare (type (array (unsigned-byte 8) (* *)) r-bytes g-bytes b-bytes))
  
  (let* ((default-color (compute-GL-color (screen-of bltable) :black)))
    
    (loop for y from 0 below (y-dim r-bytes)
	with bltable-data = (data bltable)
	do
	(loop for x from 0 below (x-dim r-bytes)
	      for rval = (aref r-bytes y x)
	      for gval = (aref g-bytes y x)
	      for bval = (aref b-bytes y x)
	      for hash-index = (+ (* 65536 rval) (* 256 gval) bval)
	      for hash-value = (gethash hash-index color-table)
	      do
	      (if hash-value
		  (setf (aref bltable-data y x) hash-value)
		  (setf (aref bltable-data y x) default-color)))))
  bltable)


;;; 
;;; Renders true color R/G/B values into a 24bit-GL-bltable
;;; 
(defmethod draw-true-color-array ((bltable 24bit-GL-bltable)
				  r-bytes g-bytes b-bytes color-table
				  zoom x-offset y-offset)
  (declare (ignore zoom x-offset y-offset color-table))
  (declare (type (array (unsigned-byte 8) (* *)) r-bytes g-bytes b-bytes))
  (with-slots (data) bltable
    (internal-color-into-24bit r-bytes g-bytes b-bytes (x-dim r-bytes)
			       data (x-dim data) (y-dim data)
			       (round x-offset) (round y-offset)))
  bltable)




;;;
;;; Overlays
;;;
(defmethod overlay-bitmap ((screen GL-screen) pic sub-pic sub-vbl)
  (let ((GL-cmindex (compute-GL-color screen (foreground sub-pic))))
    (overlay-bitmap-data (data sub-vbl) (data (system-dependent-frob pic)) GL-cmindex)))
