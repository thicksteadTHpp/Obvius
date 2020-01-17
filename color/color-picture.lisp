;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: color-picture.lisp
;;;  Author: heeger
;;;  Description: color pictures
;;;  Creation Date: 9/91, 6/93
;;; ----------------------------------------------------------------
;;; This file is part of the Object-Oriented Picture System (OBVIUS),
;;; (C) Vision Science Group,  Media Laboratory,  
;;; Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Color picture objects display methods for dithering color images.

(defmethod settable-parameters ((class-name (eql 'color-picture)))    
  (append '(pedestal scale)  (call-next-method)))

(defmethod set-not-current ((pic color-picture))
  (call-next-method)
  (reinitialize-instance pic
			 :scale (get-default 'color-picture 'scale)
			 :pedestal (get-default 'color-picture 'pedestal)))

(defmethod title-bar-string ((pic color-picture))
  (format nil "(~S - ~,2,-2G) / ~,3,-2G" 
	  (name (viewable pic)) (pedestal pic) (scale pic)))

(defmethod position-message ((pic color-picture) (im color-image)
			     pane pane-y pane-x)
  (declare (ignore pane))
  (multiple-value-bind (y x)
      (pane-coord-to-viewable-coord pic pane-y pane-x)
    (setf y (floor y) x (floor x))
    (let* ((vbl-list (viewable-list im))
	   (data1 (data (first vbl-list)))
	   (data2 (data (second vbl-list)))
	   (data3 (data (third vbl-list))))
      (if (array-in-bounds-p data1 y x)
	  (status-message "(~d, ~d): (~d,~d,~d)"
			  y x (aref data1 y x) (aref data2 y x) (aref data3 y x))
	  (status-message "(~d, ~d): out of bounds" y x)))))


(defmethod drag-picture ((pic color-picture) dy dx)
  (with-slots (y-offset x-offset pane-of zoom) pic
    (if (and dy dx)
	(setf y-offset (+ y-offset dy)  x-offset (+ x-offset dx))
	(setf y-offset 0  x-offset 0))
    (clear pane-of)
    (render pane-of (system-dependent-frob pic) y-offset x-offset zoom)))

(defmethod compute-picture ((pic color-picture) (im color-image))
  (with-slots (system-dependent-frob pane-of) pic
    (setf system-dependent-frob		;remake the bltable!
	  (make-bltable (screen-of pane-of) (dimensions im)
			:bltable system-dependent-frob
			:depth (depth (screen-of pane-of))))
    (clear system-dependent-frob :color (background pane-of))
    (let* ((vbl-list (viewable-list im))
	   (data1 (data (first vbl-list)))
	   (data2 (data (second vbl-list)))
	   (data3 (data (third vbl-list))))
      (draw-color-float-arrays system-dependent-frob data1 data2 data3
			       (pedestal pic) (scale pic) (zoom pic)
			       0 0))	;x and y offsets
    ))

(defmethod reset-picture-defaults ((pic color-picture) (vbl viewable) &rest initargs
				   &key
				   (pane-of (slot-value pic 'pane-of))
				   (current (slot-value pic 'pane-of))
				   (zoom nil zoom-supplied-p)
				   (scale nil scale-supplied-p)
				   (pedestal nil pedestal-supplied-p))
  (when scale-supplied-p
    (cond ((eq scale :auto) (setf (getf initargs :scale) (range vbl)))
	  ((not (numberp scale)) (setf (getf initargs :scale) 1)))
    (when (current-p pic) (setf (getf initargs :current) nil))) ;for set-not-current
  (when pedestal-supplied-p
    (cond ((eq pedestal :auto) (setf (getf initargs :pedestal) (minimum vbl)))
	  ((not (numberp pedestal)) (setf (getf initargs :pedestal) 0)))
    (when (current-p pic) (setf (getf initargs :current) nil)))
  (when zoom-supplied-p
    (setq zoom
	  (cond ((numberp zoom) zoom)
		((eq zoom :auto)
		 (apply 'min (mapcar #'(lambda (pane-dim pic-dim) (/ pane-dim pic-dim))
				     (dimensions pane-of) (dimensions vbl))))
		((num-list-2-p zoom)
		 (apply 'min (mapcar #'(lambda (zoom-dim pic-dim) (/ zoom-dim pic-dim))
				     zoom (dimensions vbl))))
		(t 1)))
    (setf (getf initargs :zoom) (if (> zoom 1) (round zoom) (/ (round (/ zoom))))))
  (apply #'call-next-method pic vbl initargs))

;;; Standard system-independent version.  Not implemented yet.  See
;;; lv-blt.lisp for example.
(defmethod draw-color-float-arrays
    ((bltable frob)
     r-float-array g-float-array b-float-array
     pedestal scale zoom
     x-offset y-offset)
  (error "Color pictures not implemented yet"))

#|
(load-image "images/clown")
(display clown 'color-picture)

(setq little-clown (gauss-out clown))
(setq bltable (make-bltable (current-screen) (dimensions little-clown)))

(setq bltable (make-bltable (current-screen) (dimensions little-clown) :bltable bltable))
(draw-color-float-arrays bltable
			 (data (frame 0 little-clown))
			 (data (frame 1 little-clown))
			 (data (frame 2 little-clown))
			 0.0 256.0 1 0 0)
(render *current-pane* bltable 0 0 1)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pseudo-color pictures:

(defmethod compute-picture ((pic pseudo-color) (im image))
  (with-slots (system-dependent-frob pane-of) pic
    (unless (> (depth (screen-of pane-of)) 1)
      (error "Can't render pseudo-color picture to a 1 bit screen"))
    (setf system-dependent-frob		;remake the bltable!
	  (make-bltable (screen-of pane-of) (dimensions im)
			:bltable system-dependent-frob
			:depth (depth (screen-of pane-of))
			:foreground :white
			:background :black))
    (clear system-dependent-frob :color (background pane-of))
    (draw-pseudo-color-float-array system-dependent-frob (data im)
				   (pedestal pic) (scale pic) (zoom pic)
				   0 0)	;x and y offsets
    ))


(defmethod draw-pseudo-color-float-array ((bltable frob) float-array
					  pedestal scale zoom
					  y-offset x-offset)
  (declare (ignore float-array pedestal scale zoom y-offset x-offset))
  (error "Generic Pseudo Color not implemented."))


;;; Example pseudo-color-functions:
(defun pseudo-color-ramp (i size)
  (let* ((rval (/ i (- size 1)))
	 (gval (- 1 rval))
	 (bval (* 1/2 (- 1 (abs (- (* 2 rval) 1)))))
	 )
    ;;(print-db rval gval bval)
    (list rval gval bval)))

(defun daves-colors (i size)
  (let ((bvals '(1 .9 .8 .7 .6 1 .9 .8 .7 .6
		 1 .9 .8 .7 .6 .6 .7 .8 .9 1
		 0 0 0 0 0 0 0 0 0 0
		 0 0 0 0 0))
	(gvals '(0 0 0 0 0 .5 .45 .4 .35
		 .3 1 .9 .8 .7 .6 .6 .7 .8 .9
		 1 .6 .7 .8 .9 1 .3 .35 .4 .45
		 .50 0 0 0 0 0))
	(rvals '(0 0 0 0 0 0 0 0 0 0
		 0 0 0 0 0 .6 .7 .8 .9 1
		 .6 .7 .8 .9 1 .6 .7 .8 .9 1
		 .6 .7 .8 .9 1)))
    (if (<= 0 i 34)
	(list (elt rvals i) (elt bvals i) (elt bvals i))
	(list 0 0 0))))


#|
(setq surface
      (make-synthetic-image
       '(65 65)
       #'(lambda (y x)
	   (* y (sin (* 2-pi x))
	      (exp (- (+ (/ (sqr x) (sqr 1/2)) (/ (sqr y) (sqr 1/2)))))))))
(display surface 'pseudo-color)

(setf (obv::gray-shades (current-screen)) 8)
(setf (obv::rgb-bits (current-screen)) '(1 1 1))

(setf (obv::pseudo-color-function (current-screen)) 'obv::pseudo-color-ramp)
(setf (obv::pseudo-colors (current-screen)) 7)

(setf (obv::pseudo-color-function (current-screen)) 'obv::daves-colors)
(setf (obv::pseudo-colors (current-screen)) 35)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; True-color pictures:

(defmethod compute-picture ((pic true-color) (im color-image))
  (with-slots (system-dependent-frob pedestal scale color-table) pic
    (let* ((pane (pane-of pic))
	   (screen (screen-of pane))
	   (im-list (viewable-list im)))
      (setf system-dependent-frob	;remake the bltable!
	    (make-bltable screen (dimensions im)
			  :bltable system-dependent-frob
			  :depth (depth screen)))
      (clear system-dependent-frob :color (background pane))
      (when color-table (destroy-color-table screen pic))
      (setf color-table (make-hash-table :size 64 :rehash-size 64))
      (when (or (< (minimum im) pedestal)
		(> (maximum im) (+ pedestal scale)))
	(warn "Clipping color values between ~a and ~a" pedestal (+ pedestal scale)))
      (with-static-arrays
	  ((r-bytes (allocate-array (dimensions im) :element-type '(unsigned-byte 8)))
	   (g-bytes (allocate-array (dimensions im) :element-type '(unsigned-byte 8)))
	   (b-bytes (allocate-array (dimensions im) :element-type '(unsigned-byte 8))))
	(convert-to-8bit (first im-list) r-bytes
			 pedestal (float (/-0 255 scale 1)))
	(convert-to-8bit (second im-list) g-bytes
			 pedestal (float (/-0 255 scale 1)))
	(convert-to-8bit (third im-list) b-bytes
			 pedestal (float (/-0 255 scale 1)))
	(fill-color-table screen pic r-bytes g-bytes b-bytes)
	(draw-true-color-array system-dependent-frob r-bytes g-bytes b-bytes
			       color-table
			       (zoom pic)
			       0 0)	;x and y offsets
	))))

(defmethod destroy ((pic true-color) &key &allow-other-keys)
  (destroy-color-table (screen-of (pane-of pic)) pic)
  (call-next-method))

;;; Standard system-independent version.  Not implemented yet.  See
;;; lv-blt.lisp for system-dependent example.
(defmethod draw-true-color-array
    ((bltable frob)
     r-bytes g-bytes b-bytes color-table
     zoom x-offset y-offset)
  (error "True color pictures not implemented yet"))

#|
;;; free up lots of color map entries:
(progn
  (setf (obv::gray-shades (current-screen)) 8)
  (setf (obv::rgb-bits (current-screen)) '(1 1 1))
  (setf (obv::pseudo-color-function (current-screen)) 'obv::pseudo-color-ramp)
  (setf (obv::pseudo-colors (current-screen)) 3))

(obv-require :conversion)
(obv-require :matrix)

(progn
  (lcl:cd (merge-pathnames "tutorials/color" obv::*obvius-directory-path*))
  (setf macbeth (read-cap-matrix "macbeth"))
  (setf daylight-65 (vectorize (read-cap-matrix "daylight-65")))
  (setf spectral-signals (matrix-mul macbeth (make-diagonal-matrix daylight-65)))
  (setf cones (read-cap-matrix "cones"))
  (setf cone-signals (matrix-mul-transpose cones spectral-signals))
  (setf phosphors (read-cap-matrix "phosphors"))
  (setf monitor->cones (matrix-mul-transpose phosphors cones))
  (setf cones->monitor (matrix-inverse monitor->cones))
  (setf monitor-signals (matrix-transpose-mul cone-signals cones->monitor))
  (setf gamma 0.6)
  (power monitor-signals gamma :-> monitor-signals))

(setq cim (make-color-image '(4 6) :length 3))
(loop for y from 0 below 4 do
      (loop for x from 0 below 6
	    for i = (+ x (* 6 y))
	    do
	    (loop for band from 0 below 3
		  do
		  (setf (iref (aref (matrix cim) 0 band) y x)
			(aref monitor-signals i band)))))
(display cim 'true-color :zoom :auto)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
