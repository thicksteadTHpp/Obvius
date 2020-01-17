;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: hardcopy.lisp
;;;  Author: Heeger
;;;  Description: postscript printing code.
;;;  Creation Date: Fall '88
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '(*default-printer* hardcopy))



#|
BUGS:
1) shouldn't we use the x-offset, y-offset and zoom slots of the picture?
   (could get rid of y-scale x-scale y-location x-location).  Maybe just add
   x-margin/y-margin parameters...
2) ignores drawing parameters like line-width.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; postscript screens and panes: *** Should these inherit from the X versions?

(def-simple-class postscript-screen (screen)
  ())

;;; *** Magic numbers.  Eventually, we need to generalize the
;;; postscript font stuff.
(def-simple-class postscript-font ()
  ((font-height :initform 14)
   (character-width :initform 10)))

(def-simple-class postscript-pane (pane)
  (path
   ps-stream
   (print-it :initform nil)
   (printer :initform *default-printer*)
   dimensions
   font
   (invert-flag :initform nil)
   title
   (y-scale :initform 1.0)
   (x-scale :initform 1.0)
   (y-location :initform 0)
   (x-location :initform 0)))

(defvar *postscript-screen* (make-instance 'postscript-screen))
(defvar *postscript-font* (make-instance 'postscript-font))
(defvar *postscript-pane* (make-instance 'postscript-pane
					 :screen-of *postscript-screen*
					 :font *postscript-font*))

(defparameter *postscript-paper-size* :a4) ;;possible values are :a4 :a5 :legal :letter

;;A4 =  210 mm x  297 mm =  595 pt x  842 pt
;; A5 =  148 mm x  210 mm =  420 pt x  595 pt
;;A Letter size page measures 215.9 × 279.4 millimeters or 8.50 × 11.00 inches. In PostScript, its dimensions are 612 × 792 points.
;;A Legal size page measures 216 × 356 millimeters or 8.5 × 14.00 inches. In PostScript, its dimensions are 612 × 1008 points.
(let ((paper-sizes '(:a4 (595 842) :a5 (420 595) :legal (612 1008) :letter (612 792))))
  (defun postscript-paper-size (&optional (paper :a4))
    (getf paper-sizes paper (getf paper-sizes :a4))))


(defmethod set-pane-title-bar ((pane postscript-pane) title)
  (when (eq (title pane) t)
    (setf (title pane) title)))

(defmethod OBVIUS::BACKGROUND ((pane obvius::postscript-pane)) nil)

(defmethod clear ((pane postscript-pane) &key &allow-other-keys) nil)
(defmethod depth ((screen postscript-screen)) 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; [THO] changed the papersize  
;;; Default method for all picture types just puts a copy of the
;;; picture onto the postscript pane.  This method is modelled after
;;; the display function on viewables.  If the keyword parameter :path
;;; is specified, the viewable will be saved as a postscript file with
;;; that pathname.  If :path is not specified, a postscript file will
;;; be sent to the printer specified by the :printer argument (it is
;;; first saved in a temporary file in the directory
;;; *temp-ps-directory*).  :x-location and :y-location specify the
;;; position of the upper left hand corner of the picture on the page.
;;; :x-scale and :y-scale specify magnification factors.  Sizes
;;; measured in postscript points.  Can pass an existing picture for
;;; display-type.
(defmethod hardcopy ((vbl viewable)
		     &optional (display-type t)
		     &rest keys
		     &key 
		     (pane *postscript-pane*)
		     (path nil)
		     (printer *default-printer*)
		     (y-location 0) (x-location 0)
		     (y-scale 1.0)
		     (x-scale 1.0)
		     (invert-flag nil)
		     (suppress-text path) ;if path provided, don't include text
		     (title t title-supplied-p)	;t => use standard pane title
		     make-new
		     &allow-other-keys)
  ;; remove keys which shouldn't be passed to make-instance
  (remf keys :pane) (remf keys :make-new) (remf keys :path) (remf keys :printer)
  (remf keys :y-location) (remf keys :x-location) (remf keys :y-scale) (remf keys :x-scale)
  (remf keys :invert-flag) (remf keys :suppress-text) (remf keys :title)
  (setf (print-it pane) (not path))
  (setf (path pane)
	(or (namestring (parse-namestring path))
	    (format nil "~a~a" *temp-ps-directory* (gensym "obvius-psfile-"))))
  (copy-file (format nil "~Aobv-ps.pro" *obvius-directory-path*) (path pane))
  (setf (printer pane) printer)
  (if (and suppress-text (not title-supplied-p))
      (setf (title pane) nil)
      (setf (title pane) title))
  (setf (invert-flag pane) invert-flag)
  (setf (y-location pane) y-location)
  (setf (x-location pane) x-location)
  (setf (y-scale pane) y-scale)
  (setf (x-scale pane) x-scale)
  (setf (dimensions pane) (postscript-paper-size *postscript-paper-size*)) ;; legacy impl:'(660 500)) ;*** size of 8.5x11 paper in postscript pts.
  (let ((pic (if (typep display-type 'picture)
		 display-type		;mouse binding passes in existing pic.
		 (prog1 (find-picture vbl display-type)
		   (when (eq display-type t) (setq display-type (display-type vbl)))))))
    (when display-type			;if this is nil, don't do anything.
      ;; Make a new copy of the picture
      (setq pic
	    (if (or (null pic) make-new)
		(apply 'make-instance display-type :viewable vbl :pane-of pane keys)
		(apply 'copy-object pic
		       :pane-of pane
		       :system-dependent-frob nil
		       :current nil
		       :index (incf *picture-index*)
		       keys)))
      ;; No longer need these lines since we pass these args directly
      ;; to copy-object.  -DH 9/93
      ;;(setf (system-dependent-frob pic) nil)
      ;;(setf (current pic) nil)
      (unwind-protect 
	   (push-picture pic pane)
	(destroy pic)))			;clean up, destroy the postscript picture
    t))

;;; Draw-pane for postscript-panes will send the postscript data to
;;; the file.  This is a little bit gross.  For drawings, the picture
;;; should be drawn in compute-picture (but it might be drawn in
;;; render).  We also can't determine the type of frob (bltable or
;;; whatever) ahead of time.  We write the footer here, after the
;;; title bar has been set by the standard draw-pane method.  Must
;;; unwind-protect it because it may be reponsible for closing the
;;; stream!
(defmethod draw-pane :around ((pane postscript-pane) &key (clear nil))
  (declare (ignore clear))
  (let ((pic (car (picture-stack pane))))
    (when pic
      (set-pane-title-bar pane (title-bar-string pic)) ; do this before present
      (call-next-method pane :clear nil)
      (write-postscript-foot pane)
      (when (print-it pane) (ship-to-printer (path pane) (printer pane)))
      )))

;;; Error catch
(defmethod present :around ((pic flipbook) (vbl viewable) (pane postscript-pane))
  (error "Can't hardcopy flipbooks.  Try display-type pasteup or overlay instead!"))

;;; Write postscript header here for drawings.  For bltables, the
;;; header is done from render.
(defmethod present :around ((pic drawing) (vbl viewable) (pane postscript-pane))
   (write-drawable-postscript-head pane)
   (call-next-method))

;;; *** This won't work for overlaying drawing on top of bltable.
(defmethod present :around ((pic overlay) (vbl viewable) (pane postscript-pane))
   (when (drawing-p (car (picture-list pic)))
     (write-drawable-postscript-head pane))
   (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; POSTSCRIPT-BLTABLE

;;; Postscript-bltable will contain an 8bit data array which will be
;;; hex-converted to postscript by the render method.
(def-simple-class postscript-bltable (frob)
  (screen-of 
   data
   depth))

(defmethod obvius::clear ((bltable obvius::postscript-bltable) &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  nil)

(defmethod make-bltable ((screen postscript-screen) dimensions
			 &key (depth 8) bltable &allow-other-keys)
  (let* ((bltable-dimensions (bltable-dimensions screen dimensions depth))
	 (byte-dimensions (cond ((= depth 8)
				  bltable-dimensions)
				 (( = depth 1)
				  (list (car bltable-dimensions)
					(/ (cadr bltable-dimensions) 8)))
				 (t (error "Bad depth argument: ~A" depth)))))
    (unless (and (typep bltable 'postscript-bltable)
		 (eq screen (screen-of bltable))
		 (eq depth (depth bltable))
		 (equal byte-dimensions (dimensions (data bltable))))
      (when bltable (destroy bltable))
      (setq bltable
	    (make-instance 'postscript-bltable
			   :screen-of screen
			   :depth depth
			   :data (allocate-array byte-dimensions
						 :element-type '(unsigned-byte 8)
						 :initial-element 255))))
    bltable))

(defmethod bltable-dimensions ((screen postscript-screen) dims depth)
  (cond ((= depth 1)
	 (list (* 8 (ceiling (car dims) 8))
	       (* 32 (ceiling (cadr dims) 32))))
	((= depth 8)
	 (list (car dims)
	       (* 4 (ceiling (cadr dims) 4))))))

(defmethod dimensions ((bltable postscript-bltable))
  (let ((byte-dimensions (dimensions (data bltable))))
    (if (= (depth bltable) 8)
	byte-dimensions
	(list (car byte-dimensions) (* 8 (cadr byte-dimensions))))))

(defmethod static-arrays-of ((bltable postscript-bltable))
  (when  (allocated-array-p (data bltable))
    (list (data bltable))))

(defmethod destroy ((bltable postscript-bltable) &key &allow-other-keys)
  (with-slots (data) bltable
    (and data
	 (allocated-array-p data)
	 (free-array data))))

;;; ***IGNORES Y-OFFSET X-OFFSET and ZOOM.
(defmethod render ((pane postscript-pane) (bltable postscript-bltable)
		   y-offset x-offset zoom)
  (declare (ignore x-offset y-offset zoom))
  (write-bltable-postscript-head pane bltable)
  (with-status-message "Saving postscript data"
    (write-postscript-data (path pane) (data bltable))))

;;; Unlike standard method, must pack the bits for postscript.  See
;;; other methods in gray.lisp and x-blt.lisp.  *** Should write
;;; internal1-paste in C and then use it to do this in one step!
(defmethod draw-float-array ((bltable postscript-bltable) float-array
			     pedestal scale zoom
			     y-offset x-offset)
  (declare (ignore zoom))
  (with-slots (data) bltable
    (unwind-protect
	 (cond ((= (depth bltable) 1)	;bitmap
		(with-static-arrays ((new-bit-array (allocate-array (dimensions bltable)
								    :element-type 'bit
								    :initial-element 1)))
		  (internal-dither-into-1bit float-array 
					     (x-dim float-array) (y-dim float-array)
					     new-bit-array (x-dim new-bit-array)
					     (float pedestal) (float (/-0 2.0 scale 1.0))
					     (round x-offset) (round y-offset))
		  (pack-bit-array new-bit-array data)))
	       ((= (depth bltable) 8)	;8bit pixmap
		(generic-f-into-8bit float-array
				      (x-dim float-array) (y-dim float-array) 
				      data (x-dim data)
				      (float pedestal) (float (* (/-0 1.0 scale 1.0) 256))
				      (round x-offset) (round y-offset)
				      0 255))		;min and max 8bit values
		;; (internal-f-into-8bit float-array
		;; 		      (x-dim float-array) (y-dim float-array) 
		;; 		      data (x-dim data)
		;; 		      (float pedestal) (float (* (/-0 1.0 scale 1.0) 256))
		;; 		      (round x-offset) (round y-offset)
		;; 		      0 255))		;min and max 8bit values
	       (t (error "Can't handle Screen/Bltable depths of ~A" (depth bltable)))))
    bltable))

;;; Unlike standard method, must pack the bits for postscript.  See
;;; other methods in gray.lisp and x-blt.lisp.  *** Should write
;;; internal1-paste in C and then use it to do this in one step!
(defmethod draw-bit-array ((bltable postscript-bltable) bit-array zoom)
  (declare (ignore zoom))
  (with-static-arrays ((new-bit-array (allocate-array (dimensions bltable)
						      :element-type 'bit
						      :initial-element 1)))
    (internal1-paste bit-array new-bit-array 0 0 (y-dim bit-array) (x-dim bit-array) 0 0)
    (pack-bit-array new-bit-array (data bltable))))

;;; For Lucid, data is an array of bytes, individual bits accessed using LDB
;;; *** Is this faster with arithmetic shifting instead of ldb?
;;; *** Should write this mess in C!
(defun pack-bit-array (bit-array byte-array)
  (let* ((bit-image-data (parent-array bit-array))
	 (bitmap-data (parent-array byte-array))
	 (bit-offset (displaced-start bit-array))
	 (byte-offset (displaced-start byte-array))
	 (rem16 (rem (x-dim bit-array) 16)))
    (declare (type (simple-array * (*)) bit-image-data bitmap-data)
	     (fixnum bit-offset byte-offset rem16))
    (loop for row from 0 below (y-dim bit-array) do
	  (loop for col from 0 below
		(* 2 (the fixnum (floor (x-dim bit-array) 16))) do 
		(loop for bit from 0 below 8 do
		      (setf (ldb (byte 1 (- 7 bit)) (aref bitmap-data byte-offset))
			    (aref bit-image-data bit-offset))
		      (incf bit-offset))
		(incf byte-offset))
	  ;; Now do last two bytes in row (may not be filled).
	  (when (not (zerop rem16))
	    (loop for bit from 0 below (min rem16 8)
		  do (setf (ldb (byte 1 (- 7 bit)) (aref bitmap-data byte-offset))
			   (aref bit-image-data bit-offset))
		  (setq bit-offset (1+ bit-offset)))
	    (setq byte-offset (1+ byte-offset))
	    (loop for bit from 0 below (- rem16 8)
		  do (setf (ldb (byte 1 (- 7 bit)) (aref bitmap-data byte-offset))
			   (aref bit-image-data bit-offset))
		  (setq bit-offset (1+ bit-offset)))
	    (setq byte-offset (1+ byte-offset))))))

;;; Overlay stuff
;;; Color must be either :black or :white
(defmethod overlay-bitmap ((screen postscript-screen) pic sub-pic sub-vbl)
  (let ((color-pixel (if (eq (foreground sub-pic) :black) 0 255)))
    (overlay-bitmap-data (data sub-vbl) (data (system-dependent-frob pic)) color-pixel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Drawing methods

(defmethod string-width ((font postscript-font) string)
  (* (character-width font) (length string)))

(defmethod draw-text ((pane postscript-pane) y x string &key &allow-other-keys)
  (format (ps-stream pane) "(~a) ~a ~a drawtext~%" string x y))

(defmethod draw-line ((pane postscript-pane) from-y from-x to-y to-x &key &allow-other-keys)
  (format (ps-stream pane) "~a ~a ~a ~a drawline~%" to-x to-y from-x from-y))

(defmethod draw-lines ((pane postscript-pane) y0 x0 y1 x1 &key &allow-other-keys)
  (loop for i from 0 below (length y0)
	do
	(format (ps-stream pane) "~a ~a ~a ~a drawline~%" 
		(aref x0 i) (aref y0 i) (aref x1 i) (aref y1 i))))

(defmethod draw-rect ((pane postscript-pane) y0 x0 y1 x1 &key &allow-other-keys)
  (format (ps-stream pane) "~a ~a moveto~%" x0 y0)
  (format (ps-stream pane) "~a ~a lineto~%" x1 y0)
  (format (ps-stream pane) "~a ~a lineto~%" x1 y1)
  (format (ps-stream pane) "~a ~a lineto~%" x0 y1)
  (format (ps-stream pane) "closepath fill~%"))

(defmethod draw-circles ((pane postscript-pane) yarr xarr
			 &key fill-p radius &allow-other-keys)
  (format (ps-stream pane) "newpath~%")
  (loop for i from 0 below (length yarr)
	do
	(format (ps-stream pane) "~a ~a ~a 0 360 arc" 	
		(aref xarr i) (aref yarr i) radius)
	(if fill-p
	    (format (ps-stream pane) " fill~%")
	    (format (ps-stream pane) " stroke~%"))
	))

(defmethod draw-squares ((pane postscript-pane) yarr xarr
			 &key fill-p size &allow-other-keys)
  (format (ps-stream pane) "newpath~%")
  (loop for i from 0 below (length yarr)
	for x = (- (aref xarr i) size)
	for y = (- (aref yarr i) size)
	do
	(format (ps-stream pane) "~a ~a moveto~%" x y)
	(format (ps-stream pane) "~a ~a lineto~%" (+ x (* 2 size)) y)
	(format (ps-stream pane) "~a ~a lineto~%" (+ x (* 2 size)) (+ y (* 2 size)))
	(format (ps-stream pane) "~a ~a lineto~%" x (+ y (* 2 size)))
	(if fill-p
	    (format (ps-stream pane) "closepath fill~%")
	    (format (ps-stream pane) "closepath stroke~%"))
	))

(defmethod draw-rects ((pane postscript-pane) y0-vect x0-vect y1-vect x1-vect
		       &key
		       fill-p &allow-other-keys)
  (declare (type (array fixnum (*)) y0-vect x0-vect y1-vect x1-vect))
  (format (ps-stream pane) "newpath~%")
  (loop for i from 0 below (length y0-vect)
	for x0 = (aref x0-vect i)
	for y0 = (aref y0-vect i)
	for x1 = (aref x1-vect i)
	for y1 = (aref y1-vect i)
	do
	(format (ps-stream pane) "~a ~a moveto~%" x0 y0)
	(format (ps-stream pane) "~a ~a lineto~%" x1 y0)
	(format (ps-stream pane) "~a ~a lineto~%" x1 y1)
	(format (ps-stream pane) "~a ~a lineto~%" x0 y1)
	(if fill-p
	    (format (ps-stream pane) "closepath fill~%")
	    (format (ps-stream pane) "closepath stroke~%"))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; utilities needed for writing postscript files

;;; converts an 8bit array to hex chars and appends chars to the file
(defun write-postscript-data (path 8bit-array)
  (vom:info "[write-postscript-data] to path: ~a " path)
  (with-static-arrays ((line-buffer (allocate-array (+ 1 (* 2 (array-dimension 8bit-array 0)))
						    :element-type '(unsigned-byte 8))))
    (internal-chartohex 
     (uiop:native-namestring path) 8bit-array 
     (array-dimension 8bit-array 0) 
     (array-dimension 8bit-array 1)
     line-buffer)))

;;; writes obvius-postscript header info to stream
(defun write-bltable-postscript-head (pane bltable &key (left-margin 54))
  (let* ((path (path pane))
	 (depth (depth bltable))	;*** Might be wrong for 1bit bltables.
	 (dims (dimensions bltable)) ;;(bltable-dimensions (screen-of pane) dimensions depth)
	 (ydim (first dims))
	 (xdim (second dims))
	 (y-location (y-location pane))
	 (x-location (x-location pane))
	 (y-scale (y-scale pane))
	 (x-scale (x-scale pane))
	 (invert-flag (invert-flag pane)))
    (with-status-message "Copying header"
      (with-open-file (stream (pathname path)
			      :direction :output
			      :if-exists :append
			      :element-type :default)
	;; added %%BoundingBox llx lly urx ury
	(format stream "%%BoundingBox: ~a ~a ~a ~a ~%" 
		(+ x-location left-margin) 
		(- 720 y-location (* y-scale ydim))
		;; 0 720 translate 1 -1 scale is hard coded in obv-ps.pro
		(+ x-location left-margin (* x-scale xdim))
		(- 720 y-location))
	(format stream "gsave~%")
	(format stream "~a ~a translate~%" 
		(+ x-location left-margin)
		(+ (* y-scale ydim) y-location))
	(format stream "~a ~a scale~%" 
		(* x-scale xdim) (* y-scale ydim))
	(when invert-flag
	  (format stream "invert~%"))
	(format stream "~a ~a ~a doimage~%" xdim ydim depth)))))

(defun write-drawable-postscript-head (pane &key (left-margin 54))
  (let* ((path (path pane))
	 (ydim (y-dim pane))
	 (xdim (x-dim pane))
	 (y-location (y-location pane))
	 (x-location (x-location pane))
	 (y-scale (y-scale pane))
	 (x-scale (x-scale pane))
	 (invert-flag (invert-flag pane)))
    (with-status-message "Copying header"
      (with-open-file (stream (pathname path)
			      :direction :output
			      :if-exists :append
			      :element-type :default)
	;; added %%BoundingBox llx lly urx ury
	(format stream "%%BoundingBox: ~a ~a ~a ~a ~%" 
		(+ x-location left-margin) 
		(- 720 y-location (* y-scale ydim))
		;; 0 720 translate 1 -1 scale is hard coded in obv-ps.pro
		(+ x-location left-margin (* x-scale xdim))
		(- 720 y-location))
	(format stream "gsave~%")
	(format stream "~a ~a translate~%" 
		(+ x-location left-margin)
		y-location)
	(format stream "~a ~a scale~%" x-scale y-scale)
	(when invert-flag
	  (format stream "invert~%")))
      ;; Re-open the stream for the drawing commands:
      (when (ps-stream pane) (close (ps-stream pane)))
      (setf (ps-stream pane) (open (pathname (path pane))
				:direction :output
				:if-exists :append
				:element-type :default)))))

;;; writes obvius postscript foot info to stream
(defun write-postscript-foot (pane &key (left-margin 54) (bottom 600))
  (when (ps-stream pane) (close (ps-stream pane))) ;Do this first, in case of errors.
  (let* ((path (path pane))
	 (title (if (stringp (title pane)) (title pane) "")))
    (with-open-file (stream (pathname path)
			    :direction :output
			    :if-exists :append
			    :element-type :default)
      (format stream "~%grestore~%")
      (format stream "~a ~a moveto (~a ) nameshow~%"
	      left-margin bottom title)
      (format stream "showpage~%")))
  ;; clean up
  (setf (ps-stream pane) nil)
  (setf (path pane) nil))

#|
(obv-compile-load "hardcopy")

(hardcopy einstein 'gray :path "~/images/testps/gray.ps")

(obv-require :gaussian-pyramid)
(setq pasteup (make-laplacian-pyramid (gauss-out einstein) :level 3))
(hardcopy pasteup 'pasteup :path "~/images/testps/pasteup.ps")

(setq bitmap (zero-crossings (-. (blur einstein)
				 (blur (blur einstein)))))
(hardcopy bitmap 'bitmap :invert-flag t :path "~/images/testps/bitmap.ps")

(display einstein 'surface-plot :x-step 7 :y-step 7)
(hardcopy einstein 'surface-plot :path "~/images/testps/surface.ps")

(display (setq vf (make-image-pair
		   (list (make-ramp '(32 32))
			 (make-ramp '(32 32) :orientation (/ pi 2.0)))))
	 'vector-field :scale 20.0)
(hardcopy vf 'vector-field :path "~/images/testps/vf.ps")

(display (setq graph (make-discrete-function #'(lambda (x) (sqr (sqr x)))
					     0.0 1.0 :size 20))
	 'graph :plot-symbol :circle)
(hardcopy graph 'graph :path "~/images/testps/graph.ps")

(display (setq polar-plot (make-discrete-function #'(lambda (x) (cos (* 2.0 x))) 
						  0.0 2-pi :size 31))
	 'polar-plot :plot-symbol :circle)
(hardcopy polar-plot 'polar-plot :path "~/images/testps/polar-plot.ps")

(display (setq scatter-plot (make-image-pair
			     (list (+. (make-ramp 16) (make-gaussian-noise 16))
				   (make-ramp 16))))
	 'scatter-plot :plot-symbol :circle)
(hardcopy scatter-plot 'scatter-plot :path "~/images/testps/scatter-plot.ps")

(obv-require :contour-plot)
(display (setq contour (make-synthetic-image
			'(65 65)
			#'(lambda (y x)
			    (* y (sin (* 2-pi x))
			       (exp (- (+ (/ (sqr x) (sqr 1/2)) (/ (sqr y) (sqr 1/2)))))))))
	 'contour-plot :z-min -0.2 :z-max 0.2 :num-levels 14 :skip 3 :zoom 4)
(hardcopy contour 'contour-plot :path "~/images/testps/contour.ps")

(progn
  (setq df0 (make-discrete-function #'(lambda (x) x) 0.0 1.0 :size 20))
  (setq df1 (make-discrete-function #'(lambda (x) (sqr x)) 0.0 1.0 :size 20))
  (setq df2 (make-discrete-function #'(lambda (x) (sqr (sqr x))) 0.0 1.0 :size 20))
  (display (setq ov-graph (make-viewable-sequence (list df0 df1 df2)))
	   'overlay :plot-symbol :circle))
(hardcopy ov-graph t :path "~/images/testps/ov-graph.ps")

(progn
  (setq polar1 (make-discrete-function #'(lambda (x) (cos (* 2.0 x))) 
				       0.0 2-pi :size 31))
  (setq polar2 (make-discrete-function #'(lambda (x) 0.25)
				       0.0 2-pi :size 31))
  (display (setq ov-polar (make-viewable-sequence (list polar2 polar1)))
	   'overlay :sub-display-types '(polar-plot polar-plot))
  (setp :current-picture 1 :plot-symbol :circle))
(setp :zoom 100)
(hardcopy ov-polar 'overlay :path "~/images/testps/ov-polar.ps")

(progn
  (load-image "~/images/einstein")
  (setq edges (zero-crossings (-. (blur einstein) (blur (blur einstein)))))
  (display (setq ov-bitmap (make-viewable-sequence (list einstein edges))) 'overlay))
(hardcopy ov-bitmap t :path "~/images/testps/ov-bitmap.ps")
|#



;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
