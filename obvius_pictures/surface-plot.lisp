;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: surface-plot.lisp
;;;  Author: Ciamac Moallemi <ciamac@media.mit.edu>
;;;  Description: Surface-plotting code
;;;  Creation Date: Tue Jun  2 16:59:33 1992
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1992, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '())

;;; **** Still has discretization bugs for hardcopy!

;;; subsample and transform 3-d points into 2-d points using
;;; either perspective or orthographic projection
(defun transform-nodes (data y-dim x-dim y-res x-res
			     y-step x-step matrix
			     &key (projection :perspective))
  (cond ((eq projection :perspective)
	 (internal-perspective-projection data y-dim x-dim y-res x-res
					 y-step x-step matrix))
	((eq projection :orthographic)
	 (internal-orthographic-projection data y-dim x-dim y-res x-res
					 y-step x-step matrix))
	(t (error "Unknown 3-D transform type"))))

;;; Return a 3x4 floating-point perspective transform matrix.  Camera
;;; will be moved in polar direction (r,theta,phi) from origin and will be
;;; looking back at the origin.
(defun compute-transform-matrix (dist theta phi fl
				      x-scale x-origin
				      y-scale y-origin
				      z-scale z-origin)
  (let* ((sin-theta (float (sin theta)))
	 (cos-theta (float (cos theta)))
	 (sin-phi (float (sin phi)))
	 (cos-phi (float (cos phi)))
	 (rescale (array-from-list
		   `((,(float x-scale) 0.0 0.0 ,(- (float (* x-scale x-origin))))
		     (0.0 ,(float y-scale) 0.0 ,(- (float (* y-scale y-origin))))
		     (0.0 0.0 ,(float z-scale) ,(- (float (* z-scale z-origin))))
		     (0.0 0.0 0.0 1.0))
		   :element-type 'single-float))
	 (move (array-from-list
		`((,cos-theta ,sin-theta 0.0 0.0)
		  (,(- (* sin-theta sin-phi)) ,(* cos-theta sin-phi) ,cos-phi 0.0)
		  (,(- (* sin-theta cos-phi)) ,(* cos-theta cos-phi) ,(- sin-phi)
		   ,(float dist))
		  (0.0 0.0 0.0 1.0))
		:element-type 'single-float))
	 (focal-length (array-from-list `((1.0  0.0  0.0  0.0)
					  (0.0 -1.0  0.0  0.0)
					  (0.0  0.0  ,(float (/ fl)) 0.0))
					:element-type 'single-float)))
    (matrix-mul focal-length (matrix-mul move rescale))))


;;; Rotate an array such that increasing first in x then in y takes
;;; one from front to back, according to theta
;;; Note that this rotation cannot always necessarily be done in place
(defun rotate-front-to-back (array theta)
  (let* ((pi/4 (/ pi 4))
	 (pi/2 (/ pi 2))
	 (7pi/4 (* 7 pi/4))
	 (3pi/4 (* 3 pi/4))
	 (5pi/4 (* 5 pi/4))
	 (3pi/2 (* 3 pi/2))
	 (y-dim (y-dim array))
	 (x-dim (x-dim array))
	 new-array)
    (cond ((< theta pi/4) 
	   (internal-flip-xy array array y-dim x-dim)
	   array)
          ((and (>= theta pi/4) (<= theta pi/2))
	   (setq new-array (make-array (list x-dim y-dim) 
				       :element-type 'single-float))
	   (internal-transpose array new-array y-dim x-dim)
	   (internal-flip-xy new-array new-array x-dim y-dim)
	   new-array)
	  ((and (> theta pi/2) (<= theta 3pi/4))
	   (setq new-array (make-array (list x-dim y-dim) 
				       :element-type 'single-float))
	   (internal-transpose array new-array y-dim x-dim)
	   (internal-flip-y array new-array x-dim y-dim)
	   new-array)
	  ((and (> theta 3pi/4) (< theta pi))
	   (internal-flip-x array array y-dim x-dim)
	   array)
	  ((and (>= theta pi) (< theta 5pi/4))
	   array)
	  ((and (>= theta 5pi/4) (<= theta 3pi/2))
	   (setq new-array (make-array (list x-dim y-dim) 
				       :element-type 'single-float))
	   (internal-transpose array new-array y-dim x-dim)
	   new-array)
	  ((and (> theta 3pi/2) (<= theta 7pi/4))
	   (setq new-array (make-array (list x-dim y-dim) 
				       :element-type 'single-float))
	   (internal-transpose array new-array y-dim x-dim)
	   (internal-flip-x array new-array x-dim y-dim)
	   new-array)
	  ((> theta 7pi/4)
	   (internal-flip-y array array y-dim x-dim)
	   array))))

;;; Remove hidden lines, and draw the surface onto the frob
(defun draw-surface (frob y-array x-array foreground line-width theta)
  ;; note that the size of y-{min,max}-buf must be 3 times that of
  ;; the x-dim of the frob, each is actually 3 buffers
  (let* ((new-y (rotate-front-to-back y-array theta))
	 (new-x (rotate-front-to-back x-array theta))
	 (y-min-buf (make-array (* 3 (x-dim frob)) :element-type 'fixnum))
	 (y-max-buf (make-array (* 3 (x-dim frob)) :element-type 'fixnum))
	 ;; upper bound on number of lines to be drawn
	 (max-lines (* 4 (y-dim new-y) (x-dim new-y)))
	 (y0-lines (make-array max-lines :element-type 'fixnum))
	 (x0-lines (make-array max-lines :element-type 'fixnum))	   
	 (y1-lines (make-array max-lines :element-type 'fixnum))
	 (x1-lines (make-array max-lines :element-type 'fixnum))
	 num-lines)
      (setq num-lines
	    (internal-compute-surface new-y new-x
				      (y-dim new-y) (x-dim new-y)
				      y-min-buf y-max-buf
				      (x-dim frob)
				      y0-lines x0-lines
				      y1-lines x1-lines
				      max-lines))
      ;; Not the most efficient use of memory, but copying would be
      ;; slower
      (draw-lines frob
		  (make-array num-lines
			      :element-type 'fixnum
			      :displaced-to y0-lines)
		  (make-array num-lines
			      :element-type 'fixnum
			      :displaced-to x0-lines)
		  (make-array num-lines
			      :element-type 'fixnum
			      :displaced-to y1-lines)
		  (make-array num-lines
			      :element-type 'fixnum
			      :displaced-to x1-lines)
		  :foreground foreground
		  :line-width line-width)))
      
    



