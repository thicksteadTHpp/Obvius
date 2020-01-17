;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: coord-xforms.lisp
;;;  Author: Simoncelli
;;;  Description: Generalized, invertible, 1D coordinate transformation objects.
;;;  Creation Date: 3/91
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '())

;;;; *** Should probably re-implement these as structures for efficiency

;;; y = scale * x + offset.
;;; Coerce is used to coerce the type of the result value.  It can
;;; also be nil.
;;; [THO] changed 2020 type is as T as standard
;;; otherwise icoerce wont work and arrays cannot be of element-type nil
(defclass transform ()
  ((offset :initarg :offset :initform 0)
   (scale :initarg :scale :initform 1)
   (coerce :initarg :coerce :initform nil)
   ;;type ;old form befor 2020
   (type :initform T)))

;;; y = scale * monotonic-invertible-func( pre-scale * x + pre-offset) + offset
(defclass general-transform (transform)
  ((pre-scale :initarg :pre-scale :initform 1)
   (pre-offset :initarg :pre-offset :initform 0)
   (func :initarg :func :initform 'identity)
   (ifunc :initarg :ifunc :initform 'identity)))

;;; Handles both types of xform, depending on whether func and ifunc are passed.
(defun make-transform (&rest initargs)
  (apply #'make-instance 'general-transform initargs))

(defun make-inverse-transform (&key scale offset coerce)
  (make-instance 'transform
		 :scale (/-0 1 scale 1)
		 :offset (- (/-0 offset scale 0))
		 :coerce coerce))

(defmethod initialize-instance ((xform transform)
				&rest initargs
				&key range1 range2 (scale 1) (offset 0))
  (cond ((and range1 range2)
	 (setq scale (/-0 (apply #'- range2) (apply #'- range1) 1))
	 (setf offset (- (car range2) (* scale (car range1)))))
	((zerop scale)
	 (setq scale 1)
	 (warn "zero :scale parameter encountered: setting scale to 1.")))
  ;; Force next method to use new scale offset.
  (apply #'call-next-method xform :scale scale :offset offset initargs))

(defmethod initialize-instance ((xform general-transform)
				&rest initargs
				&key range1 range2
				(pre-scale 1) (pre-offset 0)
				(func 'identity) (ifunc 'identity))
  ;; Alter range1 according to the nonlinearity.
  (when (and range1 range2 func ifunc)
    (setq range1
	  (mapcar #'(lambda (x) (funcall func (+ (* x pre-scale) pre-offset)))
		  range1))
    (remf initargs :scale) (remf initargs :offset)
    (setf (getf initargs :range1) range1))
  (when (or (and (symbolp func) (eq func 'identity))
	    (and (functionp func) (eq func #'identity)))
    (change-class xform 'transform))
  (apply #'call-next-method xform initargs))

;;; Set up the type slot
(defmethod initialize-instance :around ((xform transform) &rest initars)
   (call-next-method)
   (setf (slot-value xform 'type)
	 (type-from-coerce-function (slot-value xform 'coerce))))


;;; [THO] 2020-01 this sets the type of transforms sometimes to bit
;;; at least in sbcl which breaks any code following
;;; workaround: just distinguish between fixun and float
;;; [MAYBE]: add more cases
;;; Function returns the type of thing produced by this coerce function. 
;;; This is gross, but I'm not sure how else to do it.

(defun type-from-coerce-function (coerce)
  (if coerce
      (let* ((result (funcall coerce 1f0))
	     (result-type (type-of result)))
	(cond ((subtypep result-type 'fixnum) 'fixnum)
	      ((subtypep result-type 'single-float) 'single-float)
	      ((subtypep result-type 'double-float) 'double-float)
	      (T T)))
      T))

;; legacy implementation
  ;; (if coerce
  ;;     (type-of (funcall coerce 1.1))
  ;;     t))

(defmethod print-object ((xform transform) stream)
  (format stream "#<~A :scale ~A :offset ~A>" (class-name (class-of xform))
	  (slot-value xform 'scale) (slot-value xform 'offset)))

(defmethod print-object ((xform general-transform) stream)
  (if (symbolp (slot-value xform 'func))
      (format stream "#<~A :scale ~A :offset ~A :func ~A>" (class-name (class-of xform))
	      (slot-value xform 'scale) (slot-value xform 'offset)
	      (slot-value xform 'func))
      (call-next-method)))

(defmethod invert-transform (xform &key coerce)
  (with-slots (scale offset) xform
    (make-instance 'transform
		   :scale (/-0 1 scale 1)
		   :offset (- (/-0 offset scale 0))
		   :coerce coerce)))
  
(defmethod invert-transform ((xform general-transform) &key coerce)
  (with-slots (scale offset pre-scale pre-offset ifunc func) xform
    (make-instance 'general-transform
		   :scale (/-0 1.0 pre-scale 1)
		   :offset (- (/-0 pre-offset pre-scale 0))
		   :pre-scale (/-0 1.0 scale 1)
		   :pre-offset (- (/-0 offset scale 0))
		   :func ifunc
		   :ifunc func
		   :coerce coerce)))

;;; new-xform(x) = xform2(xform1(x)).
(defmethod concat-transforms ((xform1 transform) (xform2 transform) &key ->)
  (let* ((scale (* (slot-value xform2 'scale) (slot-value xform1 'scale)))
	 (offset (+ (* (slot-value xform2 'scale) (slot-value xform1 'offset))
		    (slot-value xform2 'offset)))
	 (coerce (slot-value xform2 'coerce))
	 (xform ->))
    (cond (xform
	   (change-class xform 'transform)
	   (setf (slot-value xform 'scale) scale)
	   (setf (slot-value xform 'offset) offset)
	   (setf (slot-value xform 'coerce) coerce)
	   xform)
	  (t (make-instance 'transform :scale scale :offset offset
			    :coerce coerce)))))

(defmethod concat-transforms ((xform1 transform) (xform2 general-transform) &key ->)
  (let* ((scale (slot-value xform2 'scale))
	 (offset (slot-value xform2 'offset))
	 (pre-scale (* (slot-value xform2 'pre-scale) (slot-value xform1 'scale)))
	 (pre-offset (+ (* (slot-value xform2 'pre-scale) (slot-value xform1 'offset))
			(slot-value xform2 'pre-offset)))
	 (func (slot-value xform2 'func))
	 (ifunc (slot-value xform2 'ifunc))
	 (coerce (slot-value xform2 'coerce))
	 (xform ->))
    (cond (xform
	   (change-class xform 'general-transform)
	   (setf (slot-value xform 'scale) scale)
	   (setf (slot-value xform 'offset) offset)
	   (setf (slot-value xform 'pre-scale) pre-scale)
	   (setf (slot-value xform 'pre-offset) pre-offset)
	   (setf (slot-value xform 'func) func)
	   (setf (slot-value xform 'ifunc) ifunc)
	   (setf (slot-value xform 'coerce) coerce)
	   xform)
	  (t (make-instance 'general-transform
			    :scale scale :offset offset :pre-scale pre-scale
			    :pre-offset pre-offset :func func :ifunc ifunc
			    :coerce coerce)))))

(defmethod concat-transforms ((xform1 general-transform) (xform2 transform) &key ->)
  (let* ((scale (* (slot-value xform2 'scale) (slot-value xform1 'scale)))
	 (offset (+ (* (slot-value xform2 'scale) (slot-value xform1 'offset))
		    (slot-value xform2 'offset)))
	 (pre-scale (slot-value xform1 'pre-scale))
	 (pre-offset (slot-value xform1 'pre-offset))
	 (func (slot-value xform1 'func))
	 (ifunc (slot-value xform1 'ifunc))
	 (coerce (slot-value xform2 'coerce))
	 (xform ->))
    (cond (xform
	   (change-class xform 'general-transform)
	   (setf (slot-value xform 'scale) scale)
	   (setf (slot-value xform 'offset) offset)
	   (setf (slot-value xform 'pre-scale) pre-scale)
	   (setf (slot-value xform 'pre-offset) pre-offset)
	   (setf (slot-value xform 'func) func)
	   (setf (slot-value xform 'ifunc) ifunc)
	   (setf (slot-value xform 'coerce) coerce)
	   xform)
	  (t (make-instance 'general-transform
			    :scale scale :offset offset :pre-scale pre-scale
			    :pre-offset pre-offset :func func :ifunc ifunc
			    :coerce coerce)))))

;;; Translate the output range of the xform.
(defun translate-transform! (xform shift)
  (incf (slot-value xform 'offset) shift)
  xform)

#| *** to be written
(defmethod concat-transforms ((xform1 general-transform) (xform2 general-transform)
			      &key ->)
  (error "Not implemented."))

(defun dilate-transform! (xform dilation ctr)
  )
|#

;;; transform an integer range [ first, last ], inclusive.
(defmethod transform-range ((xform transform) first last)
  (with-slots (scale offset coerce type) xform
    (let* ((size (1+ (- last first)))
	   (res (make-array size :element-type type)))
      (if coerce
	  (dotimes (i size)
	    (setf (aref res i) (funcall coerce (+ (* i scale) offset))))
	  (dotimes (i size)
	    (setf (aref res i) (+ (* i scale) offset))))
      res)))

(defmethod transform-range ((xform general-transform) first last)
  (with-slots (scale offset pre-scale pre-offset func coerce type) xform
    (let* ((size (1+ (- last first)))
	   (res (make-array size :element-type type)))
      (if coerce
	  (dotimes (i size)
	    (setf (aref res i)
		  (funcall coerce (+ (* (funcall func (+ (* pre-scale i) pre-offset))
					scale) offset))))
	  (dotimes (i size)
	    (setf (aref res i)
		  (+ (* (funcall func (+ (* pre-scale i) pre-offset)) scale) offset))))
      res)))

(defmethod transform-vector ((xform transform) vector)
  (with-slots (scale offset coerce type) xform
    (let* ((size (length vector))
	   (res (make-array size :element-type type)))
      (if coerce
	  (dotimes (i size)
	    (setf (aref res i) (funcall coerce (+ (* (aref vector i) scale) offset))))
	  (dotimes (i size)
	    (setf (aref res i) (+ (* (aref vector i) scale) offset))))
      res)))

(defmethod transform-vector ((xform general-transform) vector)
  (with-slots (scale offset pre-scale pre-offset func coerce type) xform
    (let* ((size (length vector))
	   (res (make-array size :element-type type)))
      (if coerce
	  (dotimes (i size)
	    (setf (aref res i)
		  (funcall coerce
			   (+ (* (funcall func
					  (+ (* pre-scale (aref vector i)) pre-offset))
				 scale) offset))))
	  (dotimes (i size)
	    (setf (aref res i)
		  (+ (* (funcall func (+ (* pre-scale (aref vector i)) pre-offset))
			scale) offset))))
      res)))

(defmethod transform-point ((xform transform) point)
  (with-slots (scale offset coerce) xform
    (if coerce
	(funcall coerce (+ (* point scale) offset))
	(+ (* point scale) offset))))

(defmethod transform-point ((xform general-transform) point)
  (with-slots (scale offset pre-scale pre-offset func coerce) xform
    (if coerce
	(funcall coerce (+ (* (funcall func (+ (* pre-scale point) pre-offset))
			      scale) offset))
	(+ (* (funcall func (+ (* pre-scale point) pre-offset)) scale) offset))))

;;; *** Doesn't coerce!
(defmethod inverse-transform-point ((xform transform) point)
  (with-slots (scale offset) xform
    (/-0 (- point offset) scale 0)))

;;; | xform( (cadr range) ) - xform( (car range) ) |
(defun transformed-distance (xform range)
  (abs (apply #'- (mapcar #'(lambda (p) (transform-point xform p)) range))))

;;; val should be a fixnum
(defun increment-integer-vector! (vect val)
  (declare (type (simple-array fixnum (*)) vect) (type fixnum val))
  (dotimes (i (length vect)) (incf (aref vect i) val))
  vect)


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
