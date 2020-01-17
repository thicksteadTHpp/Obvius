;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: picture-classes.lisp
;;;  Author: Heeger with "help" from Chichilnisky
;;;  Description: 
;;;  Creation Date: 1/94
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '(color-picture pseudo-color true-color contour-plot
          drawing vector-field graph surface-plot polar-plot scatter-plot skip
          flipbook
          gray dither bitmap 
          overlay pasteup
          ))

;;; This system-independent color is a symbol or a triplet of numbers
;;; between 0 and 1 for (rgb).
(def-simple-class drawing (picture)
  ((color :initform :white :type (or keyword list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class overlay (picture)
  ((picture-list :initform nil)
   (sub-display-types :initform nil)
   (current-picture :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skip determines the subsampling factor.  If not a number, the
;;; sampling will be chosen automatically to give a reasonable vector
;;; density for the given vector-field size.  The
;;; density initarg controls this density.  Zoom
;;; determines the overall size of the vector-field picture, relative
;;; to the size of the underlying viewable.  If nil, then the vector
;;; field will be the size of the underlying viewable (i.e. they would
;;; line up correctly if the vector field were superposed on the
;;; image).  If t, the zoom will be determined to make the overall
;;; vector field be the size of the pane.  If a dimension list, this
;;; will be the size of the vector field.  Scale determines the length
;;; of the vectors.  The vector magnitude will be DIVIDED (as for
;;; gray) by the scale and the resulting number will specify the
;;; length of the vector, in units corresponding to the distance
;;; between the vector bases! -EPS

;;; Density parameter controls the way default sampling is chosen for
;;; vector fields.  The number of vectors in a row or column varies
;;; roughly as the square root of the x- or y- dimensions of the
;;; vector field, where density is the proportionality factor.

;;; *** Could use a base-point slot (value between 0 and 1) to determine
;;; where the base point falls in the vector (at the head, the tail,
;;; or in between...).
;;; *** Also need arrow-head-length slot

(def-simple-class vector-field (drawing)
  ((skip :type (or (eql :auto) (integer 1 *)))
   dimensions
   (scale :type (or (eql :auto) number))
   (line-width :initform 1 :type (integer 1 10))
   (base-position :initform 0 :type (float 0.0 1.0))
   (arrow-heads :initform 4 :type (or number (member nil))) ;threshold
   (arrow-head-length :initform 5 :type integer))
  (:default-initargs :density 0.9 :zoom :auto	;by default, zoom to pane size.
		     :skip :auto :scale :auto))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; zoom and aspect-ratio define the  data->frob coord xform.
(def-simple-class graph (drawing)
  ((y-range :type (or cons (eql :auto))
	    :documentation "Y range of plotted data, in graph (axes) coordinates.")
   (x-range :type (or cons (eql :auto)) 
	    :documentation "X range of plotted data, in graph (axes) coordinates.")
   (graph-type :initform :line :type (member :line :bar :point))
   (y-axis-type :initform :linear :type (member :linear :log))
   (y-axis :type (or null number (eql :auto))
	   :documentation "If number, Y-coordinate of the Y-axis.
If nil, no Y-axis is plotted.")
   (x-axis :type (or null number (eql :auto))
	   :documentation "If number, Y-coordinate of the X-axis.
If nil, no X-axis is plotted.")
   (y-tick-step :type (or null number (eql :auto))
	    :documentation "If length2 list, Y-axis tick start and step.  \
If nil, no tick marks.")
   (x-tick-step :type (or null number (eql :auto))
	    :documentation "If length2 list, X-axis tick start and step.  \
If nil, no tick marks.")
   (y-label :type string :initform "" :documentation "String to label the X-axis.")
   (x-label :type string :initform "" :documentation "String to label the X-axis.")
   (aspect-ratio :type (or number (eql :auto)))
   (line-width :initform 1 :type (integer 1 10))
   (axis-color :initform :white
	       :type (or keyword list)
	       :documentation "Color of graph axes.")
   (plot-symbol :initform nil :type (member :circle :square nil))
   (fill-symbol-p :initform t :type (member t nil))
   (symbol-size :initform 3 :type (integer 1 10))
   (y-max-label-length :initform 7 :type (integer 0 10))
   (x-max-label-length :initform 7 :type (integer 0 10))
   y-tick-format-string				      ;set from y-max-label-length
   x-tick-format-string				      ;set from x-max-label-length
   (y-tick-length :initform 3 :type (integer 0 10))
   (x-tick-length :initform 3 :type (integer 0 10))
   (y-tick-gap :allocation :class :initform 2)	      ;gap between tick end and text.
   (x-tick-gap :allocation :class :initform 1)
   ;; transforms from graph coordinates to data vector coordinates.
   (graph->data-y :type transform)
   (graph->data-x :type transform)
   ;; These are used to echo mouse position, etc.
   frob->vbl-y				      ;coord transforms, set up
   frob->vbl-x)				      ;by compute-graph-transforms
  (:default-initargs
      :zoom :auto :aspect-ratio 0.8
      :y-range :auto :x-range :auto :y-axis :auto :x-axis :auto
      :y-tick-step :auto :x-tick-step :auto))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The camera is always pointing at the center pixel of the image
;;; (i.e. at origin) and its position is specified by 3 parameters: r,
;;; theta and phi where: theta = angle anti-clockwise from negative
;;; y-axis, phi = angle above the x-y plane.  The zoom parameter
;;; essentially adjusts the focal length of the camera.  X-step and
;;; y-step specify the sampling rate for the plot in x and y direction
;;; respectively.  Z-Size specifies the vertical distance from the
;;; highest to the lowest point, relative to the mean of the x- and y-
;;; array dimensions.
(def-simple-class surface-plot (drawing)
  ((r  :type (or number (eql :auto)))
   (theta :initform (/ pi 12) :type number)
   (phi :initform (/ pi 4) :type (float 0.0 1.57))
   (projection :initform :perspective :type (member :perspective :orthographic))
   (box-p :initform t :type (member t nil))
   (box-color :initform :yellow :type (or keyword list))
   (x-step :type (or (integer 1 *) (eql :auto)))
   (y-step :type (or (integer 1 *) (eql :auto)))
   (z-size :initform 0.5)
   (line-width :initform 1 :type (integer 1 10)))
  (:default-initargs :r :auto :zoom :auto
		     :x-step :auto :y-step :auto))

;;; Polar plots are defined for discrete-functions, in which the
;;; origin and increment are interpreted in radians.
;;; Polar plot are also defined for viewable-sequences.  Actually, the
;;; sequence must be a pair of viewables (either an image-pair or a
;;; pair of discrete-functions).  Like polar-images, the first
;;; viewable of the pair is magnitude (amplitude), and the second is
;;; angle (in radians).
(def-simple-class polar-plot (drawing)
  ((maximum :type (or number (eql :auto))
	    :documentation "Value that corresponds to max of the polar plot.")
   (line-width :initform 1 :type (integer 1 10))
   (plot-symbol :initform nil :type (member :circle :square nil))
   (fill-symbol-p :initform t :type (member t nil))
   (symbol-size :initform 3 :type (integer 1 10)))
  (:default-initargs :maximum :auto :zoom :auto :line-width 1))

(def-simple-class scatter-plot (graph)
  ()
  (:default-initargs :plot-symbol :circle :graph-type :point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Blting classes

(def-simple-class gray (picture)
  ((pedestal :type (or number (eql :auto))
	     :documentation "Value added to floating point viewable values to map them
to the gray colormap")
   (scale :type (or number (eql :auto))
	  :documentation "Value used as a divisor for floating point viewable values to map them
to the gray colormap.  The gray colormap range is considered to be [0,1]."))
  (:default-initargs :pedestal :auto :scale :auto))

;;; Allows the user to force a one-bit (dithered) picture.
(def-simple-class dither (gray)  ())

;;; BITMAP pictures. A bitmap is a one-bit deep picture, intended for displaying
;;; one-bit viewables.
(def-simple-class bitmap (picture)
  ((foreground :initform :white :type (or keyword list))
   (background :initform :black :type (or keyword list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defaults for zoom, x-offset, y-offset come from the subpicture (see
;;; reset-picture-defaults).
(def-simple-class flipbook (picture)
  ((picture-list :initform nil)
   (current-picture :initform 0)
   (displaying-p :initform nil)
   (frame-delay :initform 4 :type integer
		:documentation
		"Time delay (in 1/60 seconds) between frames when showing sequence")
   (back-and-forth :initform nil :type (member t nil))
   (seq-delay :initform 0 :type integer
	      :documentation
	      "Time delay (in 1/60 seconds) inserted after each run of the sequence")
   (sub-display-type :type symbol)
   (independent-parameters :initform nil :type (member t nil)))
  (:default-initargs :sub-display-type :auto))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class pasteup (gray)
  (dimensions
   (independent-parameters :initform nil :type (member t nil))
   (pasteup-format :initform nil :type (member nil :horizontal :vertical :square))
   (layout :initform nil)
   (border :initform 1 :type (integer 0 20))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class color-picture (picture)
  ((pedestal :type (or number (eql :auto))
	     :documentation "Value added to floating point viewable values to map them
to [0,1]")
   (scale :type (or number (eql :auto))
	  :documentation "Value used as a divisor for floating point viewable values to map them
to [0,1]."))
  (:default-initargs :pedestal :auto :scale :auto))

(def-simple-class pseudo-color (gray)
  ())

;;; Each true-color picture contains a color-table that is a hash
;;; table keyed by: (+ (* 65536 rval) (* 256 gval) bval).  It holds
;;; the pixel (cmap index) for that exact rgb value.

(def-simple-class true-color (color-picture)
  (color-table)
  (:default-initargs :pedestal 0 :scale :auto))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class contour-plot (drawing)
  ((dimensions)
   (z-min :initform :auto :type (or (eql :auto) flonum))
   (z-max :initform :auto :type (or (eql :auto) flonum))
   (num-levels :initform 10 :type (or (eql :auto) (integer 1 *)))
   (line-width :initform 1 :type (integer 1 10))
   (skip :type (or (integer 1 *) (eql :auto)))
   (x-offset :initform 0 :type fixnum)
   (y-offset :initform 0 :type fixnum)
   (i-pts  :initform (make-fill-vector 3 0))
   (i-pt-0 :initform (make-raw-3d-point))
   (i-pt-1 :initform (make-raw-3d-point))
   (i-pt-2 :initform (make-raw-3d-point))
   (curves)
   (segment :initform (let ((segment (make-raw-segment)))
			(setf (segment-p0 segment) (make-raw-3d-point))
			(setf (segment-p1 segment) (make-raw-3d-point))
			segment))
   (grid)
   )
  (:default-initargs :density 1.5 :zoom 1	;by default, zoom same size as image
		     :num-levels :auto :skip :auto
                     :z-min :auto :z-max :auto
		     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
