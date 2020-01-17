;;;  Author: Heeger/Chichilnisky
;;;  Description: Class definition for images, etc.
;;;  Creation Date: 1/94
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)

(export '(viewable-matrix image-matrix
          viewable-sequence image-sequence 
          image one-d-image slice data bit-image color-image 
          discrete-function histogram complex-discrete-function 
          log-discrete-function periodic-discrete-function
          filter  kernel  start-vector  step-vector  edge-handler
          separable-filter  filter-1  filter-2
	  hex-filter  hex-start
          pyramid qmf-pyramid
          forward-filters inverse-filters forward-low-filter 
          inverse-low-filter original low-band levels
	  gaussian-pyramid laplacian-pyramid
          image-pair complex-image polar-image 
          one-d-image-pair one-d-complex-image
          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-simple-class viewable-matrix (viewable)
  ((data :initform nil)))

(def-simple-class image-matrix (viewable-matrix)
  ()
  (:default-initargs :display-type 'pasteup))

(def-simple-class viewable-sequence (viewable-matrix)
  ()
  (:default-initargs :display-type 'flipbook))

(defclass image-sequence (viewable-sequence image-matrix)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A discrete-function is a vector, together with an origin and
;;; increment values which specify the mapping of the domain (x-axis)
;;; of the function.  Values are interpolated using the interpolator.
(def-simple-class discrete-function (viewable)
  (data
   (origin :initform nil)
   (increment :initform nil)
   (interpolator :initform 'linear-interpolate)
   (endpoint :initform t))
  (:default-initargs :size 256 :display-type 'graph))

(def-simple-class histogram (discrete-function)
  (image))

(def-simple-class complex-discrete-function (viewable-sequence)
  ()
  (:default-initargs :length 2 :display-type 'flipbook))

(def-simple-class periodic-discrete-function (discrete-function)
  ()
  (:default-initargs :endpoint nil :interpolator `periodic-linear-interpolate))

(def-simple-class log-discrete-function (discrete-function)
  ((base :initform 10.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Definition of the image object.  Data is a 2D array of floats.  Note that
;;; images are explicitly 2D.
(def-simple-class image (viewable)
  (data)
 (:default-initargs :display-type 'gray))

;;; Subclass one-d-image is identical to image except its default
;;; display type is a GRAPH.  When an image is created, the
;;; initialize-instance method checks to see whether one of the
;;; dimensions is unity, and if so a one-d-image is created.
(def-simple-class one-d-image (image) 
  ()
 (:default-initargs :display-type 'graph))

;;; A slice is a one-d-image that was taken from an image.
;;; *** DO we really need these slots in slices?  If we never make use
;;; of the parent slot, we could just create a 1-d-image with the name
;;; slot set to a string which gives the slice position and parent
;;; image name.
(def-simple-class slice (one-d-image)
  (x y parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class bit-image (image)
  ()
  (:default-initargs :display-type 'bitmap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; *** Multiple inheritance: does order matter? ***
(def-simple-class image-pair (image-sequence)
  ()
  (:default-initargs :length 2 :display-type 'pasteup))

;;; Accessors defined so that these slots may be set using setf.
(def-simple-class complex-image (image-pair) 
  ())

;;; First im is the mag, other im is the complex-phase
(def-simple-class polar-image (image-pair) 
  ())

;;; one-d-image-pairs must be displayed as flipbooks for now, until
;;; pasteups are generalized to deal with them.
(def-simple-class one-d-image-pair (image-pair)
  ()
  (:default-initargs :display-type 'flipbook))

;;; *** Is the method inheritance correct?  Want to inherit
;;; default-display-type from one-d-image-pair, and want to inherit
;;; special complex-image functions from complex-images.
(def-simple-class one-d-complex-image (one-d-image-pair complex-image)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filter classes

;;; Kernel is an array.  Edge-handler is a keyword symbol for a
;;; function that will be used to compute a new filter array at the
;;; edges of the image.  Choices are nil (wrap), :reflect1, :reflect2,
;;; :zero, :repeat, :ereflect.  See the file c-source/edges.c for
;;; details.
(def-simple-class filter (viewable)
  (kernel
   (start-vector :initform nil)
   (step-vector :initform nil)
   (edge-handler :initform nil))
  (:default-initargs :display-type :auto))

;;; First filter is in y direction for images, z direction for sequences.
(def-simple-class separable-filter (filter)
  (filter-1
   filter-2))

(def-simple-class hex-filter (filter)
  ((hex-start :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class color-image (image-sequence)
  ()
  (:default-initargs :display-type 'pasteup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pyramids --- self-similar sub-band image transforms including
;;; QMF pyramids and steerable pyramids.
;;; Pyramid class contains forward-filters and inverse-filters, each of 
;;; which is a list of filters or nil.  The length of the forward-filters 
;;; list is the branching factor of the image tree

;;; **** original slot is so that collapse will know how big to make
;;; the result.  We should instead use a slot like "original-plist"
;;; (fill by calling method original-plist), that is appropriate to
;;; pass to set-result/with-result.

(def-simple-class pyramid (viewable)
  ((forward-filters :initform nil)
   (forward-low-filter :initform nil)
   (inverse-filters :initform nil)
   (inverse-low-filter :initform nil)
   (original :initform nil)
   (low-band :initform nil)
   (levels :initform nil))		;list of bandpass viewable-sequences
  (:default-initargs :display-type nil))

(def-simple-class qmf-pyramid (pyramid)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-simple-class gaussian-pyramid (pyramid)
  ()
  (:default-initargs :display-type 'pasteup))

(def-simple-class laplacian-pyramid (gaussian-pyramid)
  ())

