;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: image.lisp
;;;  Author: Eero Simoncelli
;;;  Description: Class definition, accessors, basic utilities for images.
;;;  Creation Date: Spring, 1988
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)
(export '(*auto-bind-loaded-images*
	  make-image make-slice dimensions rank
	  image-p  one-d-image-p x-dim y-dim total-size 
	  print-values iref image-from-array))

;;; --------------------- GLOBAL PARAMETERS ----------------------

(defvar *auto-bind-loaded-images* t
  "If t, automatically binds images loaded from files to symbols matching 
their filenames.")
(eval-when (load eval) (setf (get '*auto-bind-loaded-images* :type) '(member t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro one-d-image-p (obj)
  `(typep ,obj 'one-d-image))

(defmacro image-p (obj)
  `(typep ,obj 'image))

(defmethod dimensions ((im image))
  (dimensions (data im)))

#|
(defmethod x-dim ((im image))
  (list-x-dim (dimensions im)))

(defmethod y-dim ((im image))
  (list-y-dim (dimensions im)))
|#

(defmethod element-type ((im image))
  'single-float)

(defmethod total-size ((im image))
  (apply #'* (dimensions im)))

(defmethod rank ((im image)) 2)
(defmethod rank ((im one-d-image)) 1)

(defmethod iref ((image image) &rest args)
  (apply 'aref (data image) args))

#+Lucid
(defmethod (setf iref) ((val number) (image image) &rest args)
  (apply #'lucid-runtime-support::set-aref (float val) (data image) args)
  (set-not-current image)
  val)

#+MCL
(defmethod (setf iref) ((val number) (image image) &rest args)
  (setf (apply #'aref (data image) args) (float val))
  (set-not-current image)
  val)

(defmethod static-arrays-of ((img image))
  (list (data img)))

;;; Print method for image objects -- needs to be modified to allow 
;;; automatic calls to print-values
(defmethod print-object ((im image) stream)
  (format stream "#<~A " (object-class-name im))
  (format stream "~S" (when (slot-boundp im 'name) (name im)))
  (format stream " ~A>" (dimensions im)))

;;; The method CHECK-SIZE checks that a list of images are the same size.
;;; Returns the model image if they are, signals an error otherwise.
(defmethod check-size ((im image) &rest im-list)
  (cond ((null im-list) im)
	((not (equal (dimensions im) (dimensions (car im-list))))
	 (error "~As have different dimensions." (object-class-name im)))
	(t (apply 'check-size im-list))))

;;; This basically does a make-instance.  It is here to let user get
;;; the initialization arglist, and for consistency with other image
;;; operations.
#|
(defun make-image (dims &rest initargs &key data display-type name ->)
  (declare (ignore data display-type name))
  (when -> (setf (getf initargs :name) ->))
  (with-result ((result nil)
		`(:class image :dimensions ,dims ,@initargs)
		'make-image dims)
    result))
|#
;; Made methods to lose the old image-from-array stuff.
(defmethod make-image ((dims list) &rest initargs &key data display-type name ->)
  (declare (ignore data display-type name))
  (when -> (setf (getf initargs :name) ->))
  (with-result ((result nil)
		`(:class image :dimensions ,dims ,@initargs)
		'make-image dims)
    result))

;; swiped from image-from-array
(defmethod make-image ((arr array) &key ->)
  (with-result ((result ->) (list :class 'image :dimensions (dimensions arr))
		'make-image arr)
    (copy arr :-> (data result))
    result))

(defmethod make-image ((dim number) &rest initargs &key data display-type name ->)
  (declare (ignore data display-type name))
  (when -> (setf (getf initargs :name) ->))
  (with-result ((result nil)
		`(:class image :dimensions ,dim ,@initargs)
		'make-image dim)
    result))

;;; ---------------------- SET-RESULT -------------------------

;;; The SET-RESULT method is used by all functions that return images
;;; as results.

(defmethod set-result ((name t) (model image))
  (check-type name viewable-name)
  (make-instance (class-of model)
		 :data (allocate-array (dimensions model) :initial-element 0.0)
		 :name name
		 :display-type (display-type model)))

;;; model-plist for image must include dimensions
(defmethod set-result ((res image) (model-plist list))
  (let ((model-dims (getf model-plist :dimensions)))
    (cond ((not (typep res (getf model-plist :class)))
	   (error "Result ~a is incompatible with argument type ~a"
		  res (getf model-plist :class)))
	  ((not (equal (or (and (listp model-dims) model-dims) (list model-dims)) (dimensions res)))
	   (error "Result size ~a and argument size ~a are incompatible"
		  (dimensions res) (getf model-plist :dimensions)))
	  (t res))))

#| OLD VERSION
(defmethod initialize-instance :around
     ((im image) &rest initargs &key data dimensions &allow-other-keys)
  (cond (data
	 (when dimensions
	   (unless (equal (dimensions data) dimensions)
	     (error "Dimensions ~a incomptible with data ~a" dimensions data))))
	(dimensions
	 (setq data (setf (getf initargs :data)
			  (allocate-array dimensions :element-type (element-type im)))))
	(t (error "Must pass either :data or :dimensions when creating an 'image")))
  (remf initargs :dimensions)
  (when (and (image-p im)		;don't do this for subclasses!
	     (or (= (x-dim data) 1) (= (y-dim data) 1)))
    (change-class im 'one-d-image)
    (loop for default in (clos:class-default-initargs (find-class 'one-d-image))
	  do (setf (getf initargs (nth 0 default)) (funcall (nth 1 default)))))
  (apply #'call-next-method im initargs))
|#

;;; *** Should be using reinitialize-instance for 1-d-images.
;;; New version as of 1-16-90 uses vectors for one-d-images
(defmethod initialize-instance :around
     ((im image) &rest initargs &key data dimensions &allow-other-keys)
  (when (numberp dimensions) (setq dimensions (setf (getf initargs :dimensions)
						    (list dimensions))))
  (cond (data
	 (when dimensions
	   (unless (equal (dimensions data) dimensions)
	     (error "Dimensions ~a incomptible with data ~a" dimensions data))))
	(dimensions
	 (setq data (setf (getf initargs :data)
			  (allocate-array dimensions :element-type (element-type im)
					  :initial-element (coerce 0 (element-type im))))))
	(t (error "Must pass either :data or :dimensions when creating an 'image")))
  (remf initargs :dimensions)
  (when (and (image-p im)		;don't do this for subclasses!
	     (or (vectorp data) (= (x-dim data) 1) (= (y-dim data) 1)))
    (change-class im 'one-d-image)
    ;;; *** THIS is bad: throws away the initargs that are passed!!!
    #+MCL
    (loop for default in (CLOS::class-default-initargs (find-class 'one-d-image))
	  do (setf (getf initargs (nth 0 default)) 
                   (get-default 'one-d-image (nth 0 default))))
    #+Lucid
    (loop for default in (CLOS::class-default-initargs (find-class 'one-d-image))
	  do (setf (getf initargs (nth 0 default)) (funcall (nth 1 default))))
    )
  (apply #'call-next-method im initargs)
  )

;;; *** version 1.2 -> 2.0 transition function:
(defmacro with-result-image (&rest stuff)
  (declare (ignore stuff))
  (error "This macro is not provided in v2.0.  Use with-result instead!"))

;;; ---------------------- SLICES -------------------------

(defmethod make-slice ((v viewable) &rest junk)
  (declare (ignore junk))
  (error "Make-slice is not defined for ~as" (object-class-name v)))

(defmethod make-slice ((im image) &rest initargs
		       &key x-coord y-coord
		       (x x-coord) (y y-coord)
		       name display-type ->
		       &allow-other-keys)
  (declare (ignore display-type name ))
  (unless (xor x y) 
    (error "Make-slice takes one and only one keyword parameter."))
  (when x (setq x (clip x 0 (1- (x-dim im)))))
  (when y (setq y (clip y 0 (1- (y-dim im)))))
  (let* ((dims (if y (x-dim im) (y-dim im))))
    (with-result ((slice ->)
		  (append (list :class 'slice 
				:parent im
				:dimensions dims
				:name (format nil "~A-slice of ~A: ~A=~A"
					      (if y "X" "Y")
					      (name im) 
					      (if x "X" "Y")
					      (or x y)))
			  initargs)
		  'apply 'make-slice im initargs)
      (let ((i-array (data im)))
	(with-displaced-vectors ((s-array (data slice)))
	  (if x
	      (dotimes (i dims)		;*** inefficient!!
		(setf (aref s-array i) (aref i-array i x)))
	      (dotimes (i dims)
		(setf (aref s-array i) (aref i-array y i))))))
      slice)))

;;; ---------------------- MISC -------------------------

;;; Works for all array types and for vectors
(defun image-from-array (arr &key ->)
  (with-result ((result ->)
		(list :class 'image
		      :dimensions (dimensions arr))
		'image-from-array arr)
    (copy arr :-> (data result))
    result))

  
;;; Prints out pixel values of the image, sampling as necessary to print no more than
;;; *max-print-vals* lines with *max-print-vals* values per line.  The global
;;; parameters *x-print-range* and *y-print-range* should contain lists of two numbers 
;;; between 0.0 and 1.0.
(defmethod print-values ((im image) &key
			 (y (* (car *y-print-range*) (y-dim im)))
			 (x (* (car *x-print-range*) (x-dim im)))
			 (y-size (- (* (cadr *y-print-range*) (y-dim im)) y))
			 (x-size (- (* (cadr *x-print-range*) (x-dim im)) x)))
  (print-values (data im) :y y :x x :y-size y-size :x-size x-size))
 
;;; For back-compatibility:
(defmacro with-local-images (image-list &body body)
  (declare (ignore image-list body))
  (error "WITH-LOCAL-IMAGES is no longer defined.  Use WITH-LOCAL-VIEWABLES instead."))

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
