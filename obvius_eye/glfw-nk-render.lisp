;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  glfw-nk-render.lisp
;;;  Author: Patrick C. Teo ,[THO]
;;;  Description: GL render routines
;;;  Creation Date: 1993 2019 
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; render functions
;;; function which take a bltable from obvius
;;; and transform the array from the data slot
;;; into a texture for using with openGL and NK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw float arrays onto GL bltables
;;;
;;;[THO] 2019 we use as intemediate format an 8bit array
;;; if gray than only one channel per bit
;;; if color three channles per bit = 24 bit = RGB
;;; as of 2019 no support for alpha channels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :obv)

;;same args as the external c function
;; (im (:array :single-float)) (im-x-size :fixnum) (im-y-size :fixnum) 
;;     (a (:array :unsigned-8bit)) (a-x-size :fixnum) (pedestal :double-float)
;;     (scale :double-float) (x-offset :fixnum) (y-offset :fixnum)
;;     (lut (:array :unsigned-8bit)) (lut-size :fixnum))
;; without offset
(defun f-into-8bit-lut (im  im-x-size  im-y-size  
			a  a-x-size  pedestal scale x-offset y-offset lut  lut-size)
  (declare (type (array single-float) im)
	   (type (simple-array (unsigned-byte 8)) a lut)
	   (type (unsigned-byte 32) im-x-size im-y-size a-x-size x-offset y-offset lut-size)
	   (type double-float pedestal scale)
	   (optimize (speed 3) (safety 0) (debug 0))
	   (ignore im-x-size im-y-size x-offset y-offset))
  
  (let ((int-ped (coerce (floor (* pedestal scale)) '(signed-byte 32)))
	(tmp 0)
	(ceiling (row-major-aref lut (1- lut-size))))
    (declare (type (signed-byte 32) tmp int-ped)
	     (type (unsigned-byte 8) ceiling))
    (flet ((float->8bit-lut (val)
	     (setf tmp (- (the (signed-byte 32) (floor (the double-float (+ (the double-float (* (the double-float (float val 1.0d0)) (the double-float scale))) (the double-float 0.5d0)))))
			  (the (signed-byte 32) int-ped)))
	     (the (unsigned-byte 8) (row-major-aref lut (if (> tmp 0)
							    (if (< tmp ceiling)
								tmp
								ceiling)
							    0)))))

      (declare (inline float->8bit-lut))
      (loop for index of-type (unsigned-byte 32) below a-x-size do
	(symbol-macrolet ((grey8 (row-major-aref a index))
			  (grey-float (row-major-aref im index)))
	  (setf grey8 (float->8bit-lut grey-float)))))))


(defun f-into-8bit-lut* (im  im-x-size  im-y-size  
			a  a-x-size  pedestal scale x-offset y-offset lut  lut-size)
  (declare (type (array single-float) im)
	   (type (simple-array (unsigned-byte 8)) a lut)
	   (type (unsigned-byte 32) im-x-size im-y-size a-x-size x-offset y-offset lut-size)
	   (type double-float pedestal scale)
	   (optimize (speed 3) (safety 0) (debug 0))
	   (ignore im-x-size im-y-size x-offset y-offset))
  
  (let ((int-ped (coerce (floor (* pedestal scale)) '(signed-byte 32)))
	(tmp 0)
	(ceiling (row-major-aref lut (1- lut-size))))
    (declare (type (signed-byte 32) tmp int-ped)
	     (type (unsigned-byte 8) ceiling))
    (flet ((float->8bit-lut (val)
	     (setf tmp (- (floor (+ (* val scale) 0.5))
			  int-ped))
	     (the (unsigned-byte 8) (row-major-aref lut (if (> tmp 0)
							    (if (< tmp ceiling)
								tmp
								ceiling)
							    0)))))

      (declare (inline float->8bit-lut))
      (loop for index of-type (unsigned-byte 32) from 0 below (* im-x-size im-y-size) do
	(symbol-macrolet ((grey8 (row-major-aref a index))
			  (grey-float (row-major-aref im index)))
	  (setf grey8 (floor grey-float)))))))


(defun generic-f-into-8bit-lut (im  im-x-size  im-y-size  
				a  a-x-size  pedestal scale x-offset y-offset lut  lut-size)
  (let ((int-ped (coerce (floor (* pedestal scale)) '(signed-byte 32)))
	(tmp 0)
	(ceiling (row-major-aref lut (1- lut-size))))
    (flet ((float->8bit-lut (val)
	     (setf tmp (- (floor (+ (* val scale) 0.5))
			  int-ped))
	     (row-major-aref lut (if (> tmp 0)
				     (if (< tmp ceiling)
					 tmp
					 ceiling)
				     0))))
      (declare (inline float->8bit-lut))
      (loop for index of-type (unsigned-byte 32) from 0 below (* im-x-size im-y-size) do
	(symbol-macrolet ((grey8 (row-major-aref a index))
			  (grey-float (row-major-aref im index)))
	  (setf grey8 (float->8bit-lut grey-float)))))))

(defun generic-f-into-8bit (im  im-x-size  im-y-size  
				a  a-x-size  pedestal scale x-offset y-offset floor  ceiling)
  (let ((int-ped (floor (* pedestal scale)))
	(tmp 0))
    (flet ((float->8bit (val)
	     (setf tmp (- (floor (+ (* val scale) 0.5))
			  int-ped))
	     (if (> tmp floor)
		 (if (< tmp ceiling)
		     tmp
		     ceiling)
		 floor)))
      (declare (inline float->8bit))
      (loop for index of-type (unsigned-byte 32) from 0 below (* im-x-size im-y-size) do
	(symbol-macrolet ((grey8 (row-major-aref a index))
			  (grey-float (row-major-aref im index)))
	  (setf grey8 (float->8bit grey-float)))))))



;; (setf tmp (- (the (signed-byte 32) (floor (the double-float (+ (the double-float (* (the double-float (float val 1.0d0)) (the double-float scale))) (the double-float 0.5d0)))))
;; 			  (the (signed-byte 32) int-ped)))
;; 	     (the (unsigned-byte 8) (row-major-aref lut (if (> tmp 0)
;; 							    (if (< tmp ceiling)
;; 								tmp
;; 								ceiling)
;; 							    0)))))



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
  (vom:info "[draw-float-array] 8bit-bltable and float array:")
  (with-slots (data screen-of depth) bltable
    (let* ((lut (gray-lut screen-of))
	   (lut-size (length lut)))

      (when (null lut)
	(error "Unable to draw float arrays -- graymap not allocated"))
      
      (setq scale (/-0 1.0 scale 1.0))
      (generic-f-into-8bit-lut float-array (x-dim float-array) (y-dim float-array)
			      data (x-dim data)
			      (float pedestal) (float (* scale lut-size))
			      (round x-offset) (round y-offset)
			      lut lut-size)))

  (vom:info "draw-float-array] 8bit-bltable ... done")
      ;; (f-into-8bit-lut* float-array (x-dim float-array) (y-dim float-array)
      ;; 		       data (x-dim data)
      ;; 		       (float pedestal) (float (* scale lut-size))
      ;; 		       (round x-offset) (round y-offset)
      ;; 		       lut lut-size)))
      ;;))
  bltable)


;;;;[tho] before 2019-12-23
  ;;     (internal-f-into-8bit-lut float-array (x-dim float-array) (y-dim float-array)
  ;; 				data (x-dim data)
  ;; 				(float pedestal) (float (* scale lut-size))
  ;; 				(round x-offset) (round y-offset)
  ;; 				lut lut-size)))
  ;; bltable)

(defmethod draw-float-array ((bltable 24bit-GL-bltable) float-array
			     pedestal scale zoom
			     y-offset x-offset)
  (declare (ignore zoom))
  (vom:info "[draw-float-array] 24bit-bltable and float array:")
  (with-slots (data screen-of depth) bltable
    (let* ((lut (gray-lut screen-of))
	   (lut-size (length lut)))
      
      (when (null lut)
	(error "Unable to draw float arrays -- graymap not allocated"))

      (setq scale (/-0 1.0 scale 1.0))
      (generic-f-into-8bit-lut float-array (x-dim float-array) (y-dim float-array)
			       data (x-dim data)
			       (float pedestal 10.0d0) 
			       (float (* scale lut-size) 1.0d0)
			       (round x-offset) (round y-offset)
			       lut lut-size)))
  bltable)

    
;; before [2019-12-20]
      ;; (internal-gray-f-into-24bit-lut float-array (x-dim float-array) (y-dim float-array)
      ;; 				      data (x-dim data)
      ;; 				      (float pedestal 10.0d0)
      ;; 				      (float (* scale lut-size) 1.0d0)
      ;; 				      (round x-offset) (round y-offset)
      ;; 				      lut lut-size)))







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
(defmethod overlay-bitmap ((screen GLFW-nk-screen) pic sub-pic sub-vbl)
  (let ((GL-cmindex (compute-GL-color screen (foreground sub-pic))))
    (overlay-bitmap-data (data sub-vbl) (data (system-dependent-frob pic)) GL-cmindex)))



