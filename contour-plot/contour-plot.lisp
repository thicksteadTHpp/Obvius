;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: contour.lisp
;;;  Author: Jonathan Bachrach
;;;  Description: 
;;;  Creation Date: Spring '92
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

#|
The parameters are:

Name            Default Description             :auto
----------------------------------------------------------------------------------------------
num-levels      6       number of slices
z-min           :auto   minimum z slice         (+ z-min (/ (- z-max z-min) (* 2 num-levels)))
z-max           :auto   maximum z slice         (- z-max (/ (- z-max z-min) (* 2 num-levels)))
density         1.5     same as vector field
skip            :auto   subsampling step size   same as vector-field
line-width      1       standard usage
zoom            1       standard usage          plot fills picture
x-offset        0       standard usage
y-offset        0       standard usage

The code works by constructing 3D triangles from sampling the image in
a grid (scan-grid).  It actually creates four triangles for each
rectangle in the grid.  Then for each z-value, it constructs a curve
containing segments each of which represents the intersection of the
constant z-value plane with a particular triangle.  So in the end, the
contour contains a list of curves.  To plot these curves, it merely
has to zip through the segments.  But for other reasons, the code
creates fixnum vectors for each of these curves and uses them to display.

The code maintains global pool's of objects (3d-point's, segments, and
triangles) and it increases the size of these pool's on demand.
Therefore, the first usage will be the slowest and until there are
enough of these objects around, it will take some time to construct
them.  After that, this program should run within a factor of two as
fast as C code.

I chose the density and num-levels to make the contour-plot come up
fast, but you might have to tune these parameters for your more common
usage and machine.
|#

;;;; *** Should modify this  to call draw-lines.  It is currently very SLOW.
;;;; *** Pools could be replaced by lucid resources.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(export '(*x-min*
	  *x-max*
	  *y-min*
	  *y-max*
	  *z-min*
	  *z-max*
	  small-flonum
	  smaller-flonum
	  large-flonum
	  ))
|#

(deftype flonum () 'single-float)
(defconstant impossible-flonum 1.919234765111915s49)
#+mit-loop
(pushnew 'flonum loop::loop-floating-point-types)
(defsubst flonum (x)
  (declare (type fixnum x))
  (coerce x 'flonum))
(deftype flonum-vector () '(simple-array single-float (*)))
(eval-when (compile load eval)
  (defsubst make-flonum-vector (size &optional (initial-element 0.0))
    (make-array size :element-type 'single-float :initial-element initial-element)))
(defconstant *flonum-vector-nil* (make-flonum-vector 0))
(defconstant fv-nil (make-flonum-vector 0))

(deftype flonum-matrix () '(simple-array single-float (* *)))
(eval-when (compile load eval)
  (defsubst make-flonum-matrix (x-size y-size &optional (initial-element 0.0))
    (make-array (list x-size y-size) :element-type 'single-float
		:initial-element initial-element)))
(defconstant *flonum-matrix-nil* (make-flonum-matrix 0 0))
(defconstant fm-nil (make-flonum-matrix 0 0))

(defconstant small-flonum   1.0e-10)
(defconstant smaller-flonum 1.0e-20)
(defconstant large-flonum   1.0e+10)

(defmacro print-standard-instance ((object stream) &body body)
  `(let ((.stream. ,stream)
         (.object. ,object))
    (format .stream. "#<")
    (format .stream. "~:(~S~)" (class-name (class-of .object.)))
    ,@(when body `((format .stream. " ")))
    ,@body
    (format .stream. " ")
    #+Genera (format .stream. "~X" (si:%pointer .object.))
    #+Lucid  (format .stream. "~X" (sys:%pointer .object.))
    #+Excl   (format .stream. "~X" (excl::pointer-to-fixnum .object.))
    #+:coral (format .stream. "~X" (ccl::%ptr-to-int .object.))
    #+kcl    (format .stream. "~X" (si:address .object.))
    (format .stream. ">")
    (values)))

;;; FILL-VECTOR

#|
(export '(fill-vector
	  fill-vector-data
	  fill-vector-index
	  fill-vector-init-size
	  fill-vector-grow-size
	  index
	  add-one fast-add-one
	  sub-one fast-sub-one
	  make-fill-vector
	  ;; iterate
	  iterate-range
	  fvref
	  sv-nil
	  ))
|#

(defconstant sv-nil (make-array 0))

(defclass fill-vector ()
  ((data      :initform sv-nil    :accessor fill-vector-data      :type simple-vector)
   (init-size :initarg :init-size :accessor fill-vector-init-size :type fixnum)
   (grow-size :initarg :grow-size :accessor fill-vector-grow-size :type fixnum)
   (index     :initform 0         :accessor fill-vector-index     :type fixnum)
   )
  (:default-initargs
   :init-size 100
   :grow-size 100)
  )

(defun make-fill-vector-data (size) (make-array size))

(defun make-fill-vector (init-size grow-size)
  (make-instance 'fill-vector :init-size init-size :grow-size grow-size))

(defmethod initialize-instance :after ((fill-vector fill-vector) &key init-size)
  (setf (fill-vector-data fill-vector) (make-fill-vector-data init-size))
  (values))

(defmethod size ((fill-vector fill-vector))
  (length (fill-vector-data fill-vector)))

(defmethod index ((fill-vector fill-vector))
  (fill-vector-index fill-vector))

(defsubst fvref (fill-vector index)
  (declare (type fixnum index))
  (svref (fill-vector-data fill-vector) index))

(defmethod add-one ((fill-vector fill-vector) element)
  (declare (type-reduce number fixnum))
  (let ((data (fill-vector-data fill-vector)))
    (when (= (length data) (fill-vector-index fill-vector))
      (setq data (make-fill-vector-data (+ (length data) (fill-vector-grow-size fill-vector))))
      (replace data (fill-vector-data fill-vector))
      (setf (fill-vector-data fill-vector) data))
    (setf (svref data (fill-vector-index fill-vector)) element)
    (incf (fill-vector-index fill-vector)))
  element)

(defsubst fast-add-one (fill-vector element)
  (declare (type-reduce number fixnum))
  (setf (svref (fill-vector-data fill-vector) (fill-vector-index fill-vector)) element)
  (incf (fill-vector-index fill-vector))
  element)

(defmethod sub-one ((fill-vector fill-vector))
  (declare (type-reduce number fixnum))
  (when (= (fill-vector-index fill-vector) 0)
    (error "SUB ON EMPTY FILL-VECTOR ~S" fill-vector))
  (decf (fill-vector-index fill-vector))
  (svref (fill-vector-data fill-vector) (fill-vector-index fill-vector)))

(defsubst fast-sub-one (fill-vector)
  (declare (type-reduce number fixnum))
  (decf (fill-vector-index fill-vector))
  (svref (fill-vector-data fill-vector) (fill-vector-index fill-vector)))

(defmethod iterate ((fill-vector fill-vector) fn)
  (loop with data = (fill-vector-data fill-vector)
	for i from 0 below (fill-vector-index fill-vector)
	for element = (svref data i)
	do (funcall fn element))
  (values))

(defmethod iterate-range ((fill-vector fill-vector) start end fn)
  (loop with data = (fill-vector-data fill-vector)
	for i from start below end
	for element = (svref data i)
	do (funcall fn element))
  (values))

(defmethod omit ((fill-vector fill-vector) eat-me)
  (declare (type-reduce number fixnum))
  (loop with data = (fill-vector-data fill-vector)
	for i from 0 below (fill-vector-index fill-vector)
	for element = (svref data i)
	do (when (eq element eat-me)
	     (loop for j from i below (1- (length data))
		   do (setf (svref data j) (svref data (1+ j))))
	     (decf (fill-vector-index fill-vector))
	     (return eat-me))
	finally (return nil)))

(defmethod print-object ((fill-vector fill-vector) stream)
  (print-standard-instance (fill-vector stream)
    (format stream ":INDEX ~D :SIZE ~D"
	    (fill-vector-index fill-vector)
	    (array-dimension (fill-vector-data fill-vector) 0)))
  fill-vector)

;;; POOL

#|
(export '(pool
	  pool-vector-init-size
	  pool-vector-grow-size
	  pool-actives
	  pool-inactives
	  make-pool
	  allocate
	  deallocate
	  clear
	  ;; iterate
	  iterate-range
	  ))
|#

(defparameter pool-vector-init-size 1000)
(defparameter pool-vector-grow-size 1000)
(proclaim '(type fixnum pool-vector-init-size pool-vector-grow-size))

(defclass pool ()
  ((constructor :initarg :constructor :accessor pool-constructor)
   (actives     :initform sv-nil      :accessor pool-actives)
   (inactives   :initform sv-nil      :accessor pool-inactives)
   )
  (:default-initargs
   :constructor #'(lambda () nil)
   ))

(defun make-pool (constructor)
  (make-instance 'pool :constructor constructor))

(defmethod initialize-instance :after ((pool pool) &key)
  (setf (pool-actives pool)   (make-fill-vector pool-vector-init-size pool-vector-grow-size))
  (setf (pool-inactives pool) (make-fill-vector pool-vector-init-size pool-vector-grow-size))
  (values))

(defmethod allocate ((pool pool))
  (let ((new (if (> (index (pool-inactives pool)) 0)
		 (sub-one (pool-inactives pool))
		 (funcall (pool-constructor pool)))))
    (add-one (pool-actives pool) new)
    new))

(defmethod deallocate ((pool pool) element)
  (omit (pool-actives pool) element)
  (add-one  (pool-inactives pool) element)
  element)

(defmethod clear ((pool pool) &key &allow-other-keys)
  (loop while (> (index (pool-actives pool)) 0)
	do (add-one (pool-inactives pool) (sub-one (pool-actives pool))))
  (values))

(defmethod iterate ((pool pool) fn)
  (iterate (pool-actives pool) fn)
  (values))

(defmethod iterate-range ((pool pool) start end fn)
  (iterate-range (pool-actives pool) start end fn)
  (values))

(defmethod print-object ((pool pool) stream)
  (print-standard-instance (pool stream)
    (format stream ":ACTIVES ~D :INACTIVES ~D"
	  (index (pool-actives pool))
	  (index (pool-inactives pool))))
  pool)

;;; 3D-POINT

#|
(export '(3d-point
	  3d-point-x
	  3d-point-y
	  3d-point-z
	  make-3d-point
	  make-raw-3d-point
	  zero-3d-point
	  make-3d-point*
	  3d-distance
	  3d-distance^2
	  matching-segment
;	  *points*
	  ))
|#
	  
(defclass 3d-point ()
  ((coordinates :initform (make-flonum-vector 3) :accessor 3d-point-coordinates :type flonum-vector)))

(defsubst 3d-point-x (point)
  (the flonum (aref (3d-point-coordinates point) 0)))
(defsubst set-3d-point-x (value point)
  (declare (type flonum value))
  (the flonum (setf (aref (3d-point-coordinates point) 0) value)))
(defsetf 3d-point-x (point) (value) `(set-3d-point-x ,value ,point))
(defsubst 3d-point-y (point)
  (the flonum (aref (3d-point-coordinates point) 1)))
(defsubst set-3d-point-y (value point)
  (declare (type flonum value))
  (the flonum (setf (aref (3d-point-coordinates point) 1) value)))
(defsetf 3d-point-y (point) (value) `(set-3d-point-y ,value ,point))
(defsubst 3d-point-z (point)
  (the flonum (aref (3d-point-coordinates point) 2)))
(defsubst set-3d-point-z (value point)
  (declare (type flonum value))
  (the flonum (setf (aref (3d-point-coordinates point) 2) value)))
(defsetf 3d-point-z (point) (value) `(set-3d-point-z ,value ,point))

;; (defsubst (setf 3d-point-x) (value point)
;;   (declare (type flonum value))
;;   (the flonum (setf (aref (3d-point-coordinates point) 0) value)))

(defun make-raw-3d-point ()
  (make-instance '3d-point))

(defvar *points* (make-pool 'make-raw-3d-point))

(defsubst make-3d-point (x y z)
  (declare (type-reduce number flonum)
	   (type flonum x y z))
  (let ((p (allocate *points*)))
    (setf (3d-point-x p) x)
    (setf (3d-point-y p) y)
    (setf (3d-point-z p) z)
    p))

(defvar zero-3d-point (make-raw-3d-point))

(defsubst make-3d-point* (p x y z)
  (declare (type-reduce number flonum)
	   (type flonum x y z))
  (setf (3d-point-x p) x)
  (setf (3d-point-y p) y)
  (setf (3d-point-z p) z)
  p)  

(defmethod supplant ((dst 3d-point) src)
  (setf (3d-point-x dst) (3d-point-x src))
  (setf (3d-point-y dst) (3d-point-y src))
  (setf (3d-point-z dst) (3d-point-z src))
  dst)

(defmethod duplicate ((point 3d-point))
  (make-3d-point (3d-point-x point) (3d-point-y point) (3d-point-z point)))

(defsubst 3d-distance (p0 p1)
  (declare (type-reduce number flonum))
  (sqrt (+ (sqr (- (3d-point-x p1) (3d-point-x p0)))
           (sqr (- (3d-point-y p1) (3d-point-y p0)))
           (sqr (- (3d-point-z p1) (3d-point-z p0))))))

(defsubst 3d-distance^2 (p0 p1)
  (declare (type-reduce number flonum))
  (+ (sqr (- (3d-point-x p1) (3d-point-x p0)))
     (sqr (- (3d-point-y p1) (3d-point-y p0)))
     (sqr (- (3d-point-z p1) (3d-point-z p0)))))

(defmethod print-object ((3d-point 3d-point) stream)
  (print-standard-instance (3d-point stream)
    (format stream ":X ~6,3F :Y ~6,3F :Z ~6,3F"
	  (3d-point-x 3d-point)
	  (3d-point-y 3d-point)
	  (3d-point-z 3d-point)))
  3d-point)


;;; SEGMENT

#|
(export '(segment
	  segment-p0
	  segment-p1
	  make-segment
	  make-raw-segment
	  make-segment*
	  make-segment-fast*
	  intersect-plane
	  long?
	  plot
;	  *segments*
	  ))
|#

(defclass segment ()
  ((p0 :initarg :p0 :accessor segment-p0)
   (p1 :initarg :p1 :accessor segment-p1))
  (:default-initargs
   :p0 nil
   :p1 nil))

(defun make-raw-segment ()
  (make-instance 'segment))

(defvar *segments* (make-pool 'make-raw-segment))

(defun make-segment (p0 p1)
  (let ((segment (allocate *segments*)))
    (setf (segment-p0 segment) p0)
    (setf (segment-p1 segment) p1)
    segment))

(defmethod make-segment* ((segment segment) p0 p1)
  (supplant (segment-p0 segment) p0)
  (supplant (segment-p1 segment) p1)
  segment)

(defmethod supplant ((dst segment) src)
  (supplant (segment-p0 dst) (segment-p0 src))
  (supplant (segment-p1 dst) (segment-p1 src))
  dst)

(defmethod duplicate ((segment segment))
  (make-segment (duplicate (segment-p0 segment)) (duplicate (segment-p1 segment))))


(defsubst intersect-plane (segment z i-pt)
  (declare (type flonum z)
	   (type-reduce number flonum))
  (let ((p0x (3d-point-x (segment-p0 segment)))
	(p0y (3d-point-y (segment-p0 segment)))
	(p0z (3d-point-z (segment-p0 segment)))
	(p1x (3d-point-x (segment-p1 segment)))
	(p1y (3d-point-y (segment-p1 segment)))
	(p1z (3d-point-z (segment-p1 segment)))
	(lambda 0.0))
    (declare (type flonum p0x p0y p0z p1x p1y p1z lambda))
    (unless (or (and (> p0z z) (> p1z z))
		(and (< p0z z) (< p1z z))
		(< (abs (- p0z p1z)) small-flonum))
      (setq lambda (/ (- z p0z) (- p1z p0z)))
      (setf (3d-point-x i-pt) (+ p0x (* (- p1x p0x) lambda)))
      (setf (3d-point-y i-pt) (+ p0y (* (- p1y p0y) lambda)))
      (setf (3d-point-z i-pt) z)
      t)))
	     
(defsubst long? (segment)
  (declare (type-reduce number flonum))
  (> (3d-distance^2 (segment-p1 segment) (segment-p0 segment)) smaller-flonum))

(defmethod plot ((segment segment) (frob t) line-width &optional (zoom 1))
  (with-slots (p0 p1) segment
    (let ((x0 (round (* zoom (3d-point-x p0))))
	  (y0 (round (* zoom (3d-point-y p0))))
	  (x1 (round (* zoom (3d-point-x p1))))
	  (y1 (round (* zoom (3d-point-y p1)))))
      (draw-line frob x0 y0 x1 y1 :line-width line-width)))
  (values))

(defmethod print-object ((segment segment) stream)
  (print-standard-instance (segment stream)
    (format stream ":P0 ~S :P1 ~S"
	    (segment-p0 segment)
	    (segment-p1 segment)))
  segment)

;;; TRIANGLE

#|
(export '(triangle
	  triangle-p0
	  triangle-p1
	  triangle-p2
	  make-triangle
	  make-raw-triangle
	  make-triangle*
	  intersect-plane?
	  insert-intersections
;	  *triangles*
	  ))
|#

(defclass triangle ()
  ((p0 :initarg :p0 :accessor triangle-p0)
   (p1 :initarg :p1 :accessor triangle-p1)
   (p2 :initarg :p2 :accessor triangle-p2))
  (:default-initargs
   :p0 nil
   :p1 nil
   :p2 nil))

(defun make-raw-triangle ()
  (make-instance 'triangle))

(defvar *triangles* (make-pool 'make-raw-triangle))

(defun make-triangle (p0 p1 p2)
  (let ((triangle (allocate *triangles*)))
    (setf (triangle-p0 triangle) p0)
    (setf (triangle-p1 triangle) p1)
    (setf (triangle-p2 triangle) p2)
    triangle))

(defmethod make-triangle* ((triangle triangle) p0 p1 p2)
  (supplant (triangle-p0 triangle) p0)
  (supplant (triangle-p1 triangle) p1)
  (supplant (triangle-p2 triangle) p2)
  triangle)

(defmethod supplant ((dst triangle) src)
  (supplant (triangle-p0 dst) (triangle-p0 src))
  (supplant (triangle-p1 dst) (triangle-p1 src))
  (supplant (triangle-p2 dst) (triangle-p2 src))
  dst)

(defmethod duplicate ((triangle triangle))
  (make-triangle (duplicate (triangle-p0 triangle))
		 (duplicate (triangle-p1 triangle))
		 (duplicate (triangle-p2 triangle))))


(defsubst intersect-plane? (triangle z)
  (declare (type flonum z)
	   (type-reduce number flonum))
  (let ((z0 (3d-point-z (triangle-p0 triangle)))
	(z1 (3d-point-z (triangle-p1 triangle)))
	(z2 (3d-point-z (triangle-p2 triangle))))
    (declare (type flonum z0 z1 z2))
    (not (or (and (> z0 z) (> z1 z) (> z2 z))
	     (and (< z0 z) (< z1 z) (< z2 z))))))
	
(defsubst insert-intersections (triangle segment i-pts i-pt-0 i-pt-1 i-pt-2 z)
  (declare (type flonum z)
	   (type-reduce number flonum))
  (when (intersect-plane? triangle z)
    (setf (fill-vector-index i-pts) 0)
    (when (intersect-plane (make-segment* segment (triangle-p0 triangle) (triangle-p1 triangle)) z i-pt-0)
      (fast-add-one i-pts i-pt-0))
    (when (intersect-plane (make-segment* segment (triangle-p1 triangle) (triangle-p2 triangle)) z i-pt-1)
      (fast-add-one i-pts i-pt-1))
    (when (intersect-plane (make-segment* segment (triangle-p2 triangle) (triangle-p0 triangle)) z i-pt-2)
      (fast-add-one i-pts i-pt-2))
    (case (fill-vector-index i-pts)
      (3 (cond ((long? (make-segment* segment (fvref i-pts 0) (fvref i-pts 1)))
		(duplicate segment))
	       ((long? (make-segment* segment (fvref i-pts 1) (fvref i-pts 2)))
		(duplicate segment))
	       ((long? (make-segment* segment (fvref i-pts 2) (fvref i-pts 0)))
		(duplicate segment))
	       (t (format t "~%WARNING: ZERO SEGMENT LENGTH :IP0 ~S :IP1 ~S :IP2 ~S :TP0 ~S :TP1 ~S :TP2 ~S"
			  (fvref i-pts 0) (fvref i-pts 1) (fvref i-pts 2)
			  (triangle-p0 triangle) (triangle-p1 triangle) (triangle-p2 triangle)
			  ))))
      (2 (when (long? (make-segment* segment (fvref i-pts 0) (fvref i-pts 1)))
	   (duplicate segment)))
      (otherwise
       t
       ;;(format t "~%WARNING ~D IS A NONSTANDARD NUMBER OF INTERSECTIONS" (index i-pts))
       )))
  (values))
	       
(defmethod plot ((triangle triangle) (frob t) line-width &optional (zoom 1))
  (declare (ignore zoom))
  (with-slots (p0 p1 p2) triangle
    (draw-line frob
	       (round (* zoom (3d-point-x p0))) (round (* zoom (3d-point-y p0)))
	       (round (* zoom (3d-point-x p1))) (round (* zoom (3d-point-y p1)))
	       :line-width line-width)
    (draw-line frob
	       (round (* zoom (3d-point-x p1))) (round (* zoom (3d-point-y p1)))
	       (round (* zoom (3d-point-x p2))) (round (* zoom (3d-point-y p2)))
	       :line-width line-width)
    (draw-line frob
	       (round (* zoom (3d-point-x p2))) (round (* zoom (3d-point-y p2)))
	       (round (* zoom (3d-point-x p0))) (round (* zoom (3d-point-y p0)))
	       :line-width line-width)
    )
  (values))

(defmethod print-object ((triangle triangle) stream)
  (print-standard-instance (triangle stream)
    (format stream ":P0 ~S :P1 ~S :P2 ~S"
	    (triangle-p0 triangle)
	    (triangle-p1 triangle)
	    (triangle-p2 triangle)))
  triangle)

;;; CURVE
#|
(export '(curve
	  curve-z
	  curve-segments
	  curve-segment-start
	  curve-segment-end
	  make-curve
	  plot
	  ))
|#

(defclass curve ()
  ((z             :initarg :z             :accessor curve-z             :type flonum)
   (segments      :initarg :segments      :accessor curve-segments)
   (segment-start :initarg :segment-start :accessor curve-segment-start :type fixnum)
   (segment-end   :initarg :segment-end   :accessor curve-segment-end   :type fixnum)
   )
  (:default-initargs
   :z 0.0))

(defun make-curve (z segment-start segment-end)
  (make-instance 'curve :z z :segments *segments* :segment-start segment-start :segment-end segment-end))

(defmethod plot ((curve curve) (frob t) line-width &optional (zoom 1))
  (iterate-range (curve-segments curve) (curve-segment-start curve) (curve-segment-end curve)
		 #'(lambda (segment) (plot segment frob line-width zoom)))
  (values))

(defmethod print-object ((curve curve) stream)
  (print-standard-instance (curve stream)
    (format stream ":Z ~D" (curve-z curve)))
  curve)

;;; CONTOUR


;;; *** :density should be in here, as soon as dialog permit it.
;;; JB figure out density
(defmethod settable-parameters ((class-name (eql 'contour-plot)))
  (append '(z-min z-max num-levels skip line-width)
	  (call-next-method)))

#|  TOO much magic:
(defmethod set-not-current ((vf vector-field))
  (setf (scale vf) nil)
  (call-next-method))
|#

(defmethod title-bar-string ((pic contour-plot))
  (format nil "Contour ~S, ~D Levels" (name (viewable pic)) (num-levels pic)))

(defmethod position-message ((pic contour-plot) (im image) pane pane-y pane-x)
  (declare (ignore pane))
  (multiple-value-bind (y x)
      (pane-coord-to-viewable-coord pic pane-y pane-x)
    (if (array-in-bounds-p (data im) y x)
	(status-message "(~D, ~D):  ~G" y x (iref im y x))
	(status-message "(~D, ~D): out of bounds" y x))))

(defmethod frob-coord-to-viewable-coord ((pic contour-plot) frob-y frob-x)
  (with-slots (skip zoom) pic
    (values (* skip (floor frob-y (* zoom skip)))
	    (* skip (floor frob-x (* zoom skip))))))

;;; Compute default bitmap dimensions, skip, and zoom
;;; parameters if they are not already set.  
(defmethod reset-picture-defaults
    ((contour contour-plot) (im image) &rest initargs
     &key
     (pane-of (slot-value contour 'pane-of))
     (skip (slot-value contour 'skip) skip-supplied-p)
     (num-levels (slot-value contour 'num-levels) num-levels-supplied-p)
     (zoom (slot-value contour 'zoom) zoom-supplied-p)
     (density (get-default 'contour-plot :density) density-supplied-p))
  (remf initargs :density)
  (let* ((im-dims (dimensions im))
	 ;;first get approximate contour size
	 (contour-dims (cond ((numberp zoom) (mapcar #'(lambda (d) (round (* zoom d))) im-dims))
			     ((eq zoom :auto) (dimensions pane-of))
			     ((consp zoom) zoom)
			     (t im-dims))))	;natural size is size of image...
    (when (or zoom-supplied-p skip-supplied-p density-supplied-p num-levels-supplied-p)
      ;; If skip is supplied, then use it.  Otherwise choose skip based on
      ;; density (which is always supplied via default initarg).
      (cond ((and skip-supplied-p (numberp skip))
	     (setq skip (setf (getf initargs :skip) (round skip))))
	    (t
	     (setq skip (setf (getf initargs :skip)
			      ;;number of vectors prop. to sqrt vf-size
			      (apply #'max 1 (mapcar #'(lambda (idim)
							 (round (* (sqrt idim) density)))
						     im-dims))))))
      (unless (integerp num-levels)
	;; JB SHOULD BE A DEFAULT-NUM-LEVELS PARAM OR SQRT OF CONTOUR-DIM
	(setq num-levels (setf (getf initargs :num-levels) 6))) 
      (cond ((numberp zoom) zoom)	;ok
	    ((or (eq zoom :auto) (num-list-2-p zoom))
	     (let* ((num-boxes (mapcar #'(lambda (dim) (ceiling dim skip)) im-dims))
		    (box-size (apply 'min (mapcar #'(lambda (dim num) (floor dim num))
						  contour-dims num-boxes))))
	       (setq contour-dims (mapcar #'(lambda (num) (* num box-size)) num-boxes))
	       (setq zoom (setf (getf initargs :zoom) (/ box-size skip)))))
	    (t (setq zoom (setf (getf initargs :zoom) 1)))))
    (setf (getf initargs :dimensions) contour-dims)
    ;; (print initargs)
    (apply #'call-next-method contour im initargs)))

(defmethod compute-picture ((contour contour-plot) (im image))
  (with-slots (system-dependent-frob 
	       zoom skip line-width color
	       x-offset y-offset num-levels
	       z-min z-max grid curves) contour
    (let* ((new-dims (dimensions contour))
	   (im-dims (dimensions im))
	   (num-samples (list (1+ (floor (first im-dims) skip)) (1+ (floor (second im-dims) skip)))))
      (setf system-dependent-frob
	    (make-contour-frob (screen-of (pane-of contour)) new-dims
			       :contour contour
			       :frob system-dependent-frob))
      (when (or (null grid)
		(not (loop for grid-dim in num-samples
			   for old-grid-dim in (dimensions grid)
			   always (= grid-dim old-grid-dim))))
	(setf grid (make-array num-samples)))
      (setf curves nil)
      (clear *points*)
      (clear *segments*)
      (clear *triangles*)
      (scan-grid contour im)
      (loop with gap single-float = (/ (- z-max z-min) (1- (flonum num-levels)))
	    with tol = (expt 10 (floor (log gap 10)))
	    for i from 0 to num-levels
	    for z = (tolerance (+ (* i gap) z-min) tol)
	    for start-segment fixnum = (index (pool-actives *segments*))
	    do
	    ;;(format t "~%CURVE ~D" z)
	    (find-contour contour z)
	    (push (make-curve z start-segment (index (pool-actives *segments*)))
		  curves))
      (plot contour system-dependent-frob line-width zoom)
      )))

(defsubst fx1+ (i)
  (declare (type-reduce number fixnum)
           (type fixnum i))
  (the fixnum (1+ i)))

(defsubst fx1- (i)
  (declare (type-reduce number fixnum)
           (type fixnum i))
  (the fixnum (1- i)))

(defmethod scan-grid ((contour contour-plot) (image image))
  (declare (type-reduce number flonum))
  (let* ((data (data image))
	 (image-x-size (array-dimension data 0))
	 (image-y-size (array-dimension data 1))
	 (grid (grid contour))
	 (grid-x-size (array-dimension grid 0))
	 (grid-y-size (array-dimension grid 1))
	 (x-skip (/ (1- (flonum image-x-size)) (1- (flonum grid-x-size))))
	 (y-skip (/ (1- (flonum image-y-size)) (1- (flonum grid-y-size))))
	 (z-min large-flonum)
	 (z-max (- large-flonum))
	 )
    (declare (type flonum z-min z-max x-skip y-skip)
	     (type fixnum image-x-size image-y-size grid-x-size grid-y-size)
	     (type flonum-matrix data))
    (loop for grid-i fixnum from 0 below grid-x-size 
	  for x single-float from 0.0 by x-skip
	  for image-i fixnum = (round x)
	  do (loop for grid-j fixnum from 0 below grid-y-size
		   for y single-float from 0.0 by y-skip
		   for image-j fixnum = (round y)
		   ;; for z single-float = (aref (the flonum-matrix data) image-i image-j)
		   for z single-float = (iref image image-i image-j)
		   do (cond ((= z impossible-flonum)
			     (setf (aref grid grid-i grid-j) nil))
			    (t
			     (when (< z z-min) (setq z-min z))
			     (when (> z z-max) (setq z-max z))
			     (setf (aref grid grid-i grid-j) (make-3d-point x y z))
			     ))))
    (when (eq (z-min contour) :auto)
      (let* ((gap (/ (- z-max z-min) (flonum (num-levels contour))))
             (gap/2 (/ gap 2.0)))
        (setf (z-min contour) (+ z-min gap/2))
        (setf (z-max contour) (- z-max gap/2))))
    (loop with mid-point = nil
	  for i fixnum from 0 below (fx1- grid-x-size)
	  do (loop for j fixnum from 0 below (fx1- grid-y-size)
		   for p00 = (aref grid        i        j)
		   for p01 = (aref grid        i (fx1+ j))
		   for p10 = (aref grid (fx1+ i)        j)
		   for p11 = (aref grid (fx1+ i) (fx1+ j))
		   do (when (and p00 p01 p10 p11)
			(setq mid-point (make-3d-point (* 0.5  (+ (3d-point-x p00) (3d-point-x p10)))
						       (* 0.5  (+ (3d-point-y p00) (3d-point-y p01)))
						       (* 0.25 (+ (3d-point-z p00) (3d-point-z p01)
								  (3d-point-z p10) (3d-point-z p11)))))
			(make-triangle p00 p10 mid-point)
			(make-triangle p10 p11 mid-point)
			(make-triangle p11 p01 mid-point)
			(make-triangle p01 p00 mid-point))))
    )
  (values))

(defmethod find-contour ((contour contour-plot) z)
  (declare (type-reduce number flonum)
	   (type flonum z))
  (with-slots (segment i-pts i-pt-0 i-pt-1 i-pt-2) contour
    (iterate *triangles* #'(lambda (triangle) (insert-intersections triangle segment i-pts i-pt-0 i-pt-1 i-pt-2 z))))
  (values))

(defmethod plot ((contour contour-plot) (frob t) line-width &optional (zoom 1))
  ;; (iterate *triangles* #'(lambda (triangle) (plot triangle frob line-width zoom)))
  (loop for curve in (curves contour)
	do (iterate-range (curve-segments curve) (curve-segment-start curve) (curve-segment-end curve)
			  #'(lambda (segment)
			      (draw-line frob
					 (round (* zoom (3d-point-x (segment-p0 segment))))
					 (round (* zoom (3d-point-y (segment-p0 segment))))
					 (round (* zoom (3d-point-x (segment-p1 segment))))
					 (round (* zoom (3d-point-y (segment-p1 segment))))
					 :line-width line-width))))
  (values))


(def-simple-class curve-frob ()
  ((z  :initarg :z)
   (x0 :initarg :x0)
   (y0 :initarg :y0)
   (x1 :initarg :x1)
   (y1 :initarg :y1)))

(defun make-curve-frob (z num-lines)
  (make-instance 'curve-frob
		 :z z
		 :x0 (make-array num-lines :element-type 'fixnum :initial-element 0)
		 :x1 (make-array num-lines :element-type 'fixnum :initial-element 0)
		 :y0 (make-array num-lines :element-type 'fixnum :initial-element 0)
		 :y1 (make-array num-lines :element-type 'fixnum :initial-element 0)))

(defmethod render ((pane pane) (frob curve-frob) y-offset x-offset zoom)
  (declare (ignore zoom))
  (with-slots (x0 y0 x1 y1) frob
    (draw-lines pane x0 y0 x1 y1 :x-offset x-offset :y-offset y-offset))
  (values))


;;;; CONTOUR FROB

;;; This just holds some auxilliary info for the contour which allows it to be
;;; efficiently drawn directly to the pane.
(def-simple-class contour-frob (frob)
  (contour curve-frobs dimensions))

(defmethod plot ((contour contour-plot) (frob contour-frob) line-width &optional (zoom 1))
  (declare (ignore line-width))
  (loop initially (setf (curve-frobs frob) nil)
	for i = 0
	for curve in (curves contour)
	for curve-frob = (make-curve-frob (curve-z curve) (- (curve-segment-end curve) (curve-segment-start curve)))
	for x0 = (x0 curve-frob)
	for y0 = (y0 curve-frob)
	for x1 = (x1 curve-frob)
	for y1 = (y1 curve-frob)
	do (iterate-range (curve-segments curve) (curve-segment-start curve) (curve-segment-end curve)
			  #'(lambda (segment)
			      (setf (aref x0 i) (round (* zoom (3d-point-x (segment-p0 segment)))))
			      (setf (aref y0 i) (round (* zoom (3d-point-y (segment-p0 segment)))))
			      (setf (aref x1 i) (round (* zoom (3d-point-x (segment-p1 segment)))))
			      (setf (aref y1 i) (round (* zoom (3d-point-y (segment-p1 segment)))))
			      (incf i)
			      ))
	   (push curve-frob (curve-frobs frob)))
  (values))

#|
;;; Old version did not handle color:

(defmethod render ((pane pane) (frob contour-frob) y-offset x-offset zoom)
  (with-slots (contour curve-frobs pane->frob-y pane->frob-x) frob
    (let ((corner-y (+ y-offset (floor (- (y-dim pane) (y-dim frob)) 2)))
	  (corner-x (+ x-offset (floor (- (x-dim pane) (x-dim frob)) 2))))
      (setf (slot-value pane->frob-y 'offset) (- corner-y))
      (setf (slot-value pane->frob-x 'offset) (- corner-x))
      (loop for curve-frob in curve-frobs
	    do
	    (render pane curve-frob corner-y corner-x zoom)))))
|#

(defmethod render ((pane pane) (frob contour-frob) y-offset x-offset zoom)
  (declare (ignore zoom))
  (with-slots (contour curve-frobs pane->frob-y pane->frob-x) frob
    (let ((corner-y (+ y-offset (floor (- (y-dim pane) (y-dim frob)) 2)))
	  (corner-x (+ x-offset (floor (- (x-dim pane) (x-dim frob)) 2))))
      (setf (slot-value pane->frob-y 'offset) (- corner-y))
      (setf (slot-value pane->frob-x 'offset) (- corner-x))
      (loop for curve-frob in curve-frobs
	    for x0 = (x0 curve-frob)
	    for y0 = (y0 curve-frob)
	    for x1 = (x1 curve-frob)
	    for y1 = (y1 curve-frob)
	    do
	    (draw-lines pane x0  y0 x1 y1 :x-offset corner-x :y-offset corner-y
			:foreground (color contour))
	    ))))


;;;;

;;; Dummy frob holds data needed to draw the contour to the pane.
(defmethod make-contour-frob ((screen t) dims &rest initargs &key frob &allow-other-keys)
  (remf initargs :frob)
  (setf (getf initargs :dimensions) dims)
  (cond ((typep frob 'contour-frob)
	 (apply 'reinitialize-instance frob initargs))
	(t
	 (destroy frob)
	 (apply 'make-instance 'contour-frob initargs))))

#|
(display (make-synthetic-image
	  '(64 64)
	  #'(lambda (y x)
	      (exp (- (+ (/ (sqr x) (sqr 1/2)) (/ (sqr y) (sqr 1/2)))))))
	 'contour-plot)

(display (make-synthetic-image
		'(64 64)
		#'(lambda (y x)
		    (exp (- (+ (/ (sqr x) (sqr 1/2)) (/ (sqr y) (sqr 2)))))))
	 'contour-plot)

(display (make-synthetic-image
	  '(64 64)
	  #'(lambda (y x)
	      (* (sin (* 2-pi x))
		 (exp (- (+ (/ (sqr x) (sqr 1/2)) (/ (sqr y) (sqr 3/4))))))))
	 'contour-plot)

(display (make-synthetic-image
	  '(65 65)
	  #'(lambda (y x)
	      (* y (sin (* 2-pi x))
		 (exp (- (+ (/ (sqr x) (sqr 1/2)) (/ (sqr y) (sqr 1/2))))))))
	 'contour-plot :z-min -0.2 :z-max 0.2 :num-levels 15 :skip 3 :zoom 4)
|#

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:

