;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: filter.lisp
;;;  Author: David Heeger / Eero Simoncelli
;;;  Description: filter objects
;;;  Creation Date: summer '88
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)

(export '(qmf-5 qmf-7 qmf-9 qmf-13 gauss-5
	  x-dim y-dim dimensions rank volume sum-of 
	  filter-p separable-filter-p hex-filter-p
	  normalize! image-from-filter
	  copy mul add negate sub shift-by-pi hex-modulate impulse-response
	  make-filter make-separable-filter 
	  apply-filter expand-filter single-frame-correlate
	  gauss gauss-in gauss-out blur
	  interpolate-hex-image))

;;; Filters should be generated only by making calls to MAKE-FILTER 
;;; or to MAKE-SEPARABLE-FILTER

;;; Some standard filters:
(defvar qmf-5 '(-0.076103 0.3535534 0.8593118 0.3535534 -0.076103))

(defvar qmf-7
  '(-0.0076129 -0.073710695 0.3622055 0.8524323 0.3622055 -0.073710695 -0.0076129))

(defvar qmf-9
  '(0.02807382 -0.060944743 -0.073386624 0.41472545 0.7973934 
    0.41472545 -0.073386624 -0.060944743 0.02807382))

(defvar qmf-13
  '(-0.014556438 0.021651438 0.039045125 -0.09800052 -0.057827797 0.42995453
    0.7737113 0.42995453 -0.057827797 -0.09800052 0.039045125 0.021651438 -0.014556438))

(defvar gauss-5 '(0.0625 0.25 0.375 0.25 0.0625))


;;; This list must match the list in the C file edges.c
(defvar *filter-edge-handlers*
  '(:dont-compute :zero :repeat :reflect1 :reflect2 :extend :ereflect :treflect))

(defmacro filter-p (obj)
  `(typep ,obj 'filter))

(defmacro separable-filter-p (obj)
  `(typep ,obj 'separable-filter))

(defmacro hex-filter-p (obj)
  `(typep ,obj 'hex-filter))

(defmethod data ((filter filter))
  (kernel filter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FILTER METHODS.  Use of with-slots macro makes these more efficient.

(defmethod print-object ((filter filter) stream)
  (format stream "#<~A ~S ~A>"
	  (object-class-name filter) (name filter) (dimensions filter)))

;;; *** These two "accessors" are for backward compatibility.
(defmethod y-filter ((filter separable-filter))
  (with-slots (filter-1) filter
    filter-1))

(defmethod x-filter ((filter separable-filter))
  (with-slots (filter-2) filter
    filter-2))

(defmethod z-start ((filter filter))
  (with-slots (start-vector) filter
    (list-z-dim start-vector 0)))

(defmethod y-start ((filter filter))
  (with-slots (start-vector) filter
    (list-y-dim start-vector 0)))

(defmethod x-start ((filter filter))
  (with-slots (start-vector) filter
    (list-x-dim start-vector 0)))

(defmethod z-step ((filter filter))
  (with-slots (step-vector) filter
    (list-z-dim step-vector 1)))

(defmethod y-step ((filter filter))
  (with-slots (step-vector) filter
    (list-y-dim step-vector 1)))

(defmethod x-step ((filter filter))
  (with-slots (step-vector) filter
    (list-x-dim step-vector 1)))

#|
(defmethod z-dim ((filter filter))
  (list-z-dim (dimensions filter) 1))

(defmethod y-dim ((filter filter))
  (list-y-dim (dimensions filter) 1))

(defmethod x-dim ((filter filter))
  (list-x-dim (dimensions filter) 1))
|#

(defmethod total-size ((filter filter))
  (with-slots (kernel) filter
    (array-total-size kernel)))

(defmethod dimensions ((filter filter))
  (with-slots (kernel) filter
    (array-dimensions kernel)))

(defmethod rank ((filter filter))
  (with-slots (kernel) filter
    (array-rank kernel)))

;;; *** Bad name!  For back-compatibility.
(defmethod volume ((filter filter))
  (with-slots (kernel) filter
    (sum-of kernel)))

(defmethod sum-of ((filter filter))
  (with-slots (kernel) filter
    (sum-of kernel)))

(defmethod mean ((filter filter) &key ignore-zeros)
  (with-slots (kernel) filter
    (mean kernel :ignore-zeros ignore-zeros)))

;;; The function CHECK-SIZE checks that a list of filters are the same size.
;;; Returns the last filter if they are, signals an error otherwise.
(defmethod check-size ((filter filter) &rest filt-list)
  (cond ((null filt-list) filter)
	((not (equal (dimensions filter) (dimensions (car filt-list))))
	 (error "~As have different dimensions." (object-class-name filter)))
	(t (apply 'check-size filt-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MAKE-FILTER and SET-RESULT methods for FILTERS

(defmethod set-result ((name t) (model filter))
  (check-type name viewable-name)
  (make-instance (class-of model)
		 :name name
		 :kernel (similar (kernel model))
		 :edge-handler (edge-handler model)
		 :start-vector (start-vector model)
		 :step-vector (step-vector model)
		 :display-type (display-type model)))

;;; Set-result case 3 (user passes a result viewable, model is a
;;; plist) is not defined here.  It is inherited from the default
;;; method in viewable.lisp.  We don't bother with any additional
;;; error checking in this case since we don't want to destructively
;;; modify any slots other than the kernel slot.

;;; When this method gets called, the step-vector and start-vector slots might be empty.
(defmethod initialize-instance :after ((filt filter) &rest keys)
  (unless (and (kernel filt) (arrayp (kernel filt)))
    (error "filter kernel-slot must be an array"))
  (when (eq (getf keys :display-type) :auto)
    (setf (slot-value filt 'display-type)
	  (case (rank filt)
	    (1 'graph)
	    (2 'gray)
	    (t nil))))
  filt)

;;; **** version 1.2 -> 2.0 transition function:
(defmacro with-result-filter (&rest stuff)
  (declare (ignore stuff))
  (error "This macro is not provided in v2.0.  Use with-result instead!"))

;;; **** version 1.2 -> 2.0 transition function:
(defmacro with-result-separable-filter (&rest stuff)
  (declare (ignore stuff))
  (error "This macro is not provided in v2.0.  Use with-result instead!"))

;;; MAKE-FILTER.   
;;; kernel is an array or a nested set of lists (as in the
;;; :initial-contents arg to make-array).  Also can be an existing filter (useful when
;;; making separable filters.  in either case the kernel
;;; is copied into a new array Edge-handler choices are nil (wrap),
;;; :reflect1, :reflect2, :zero, :repeat, :ereflect.
;;; *** Shouldn't the kernel array stuff happen in the initialize-instance method?
(defun make-filter (kernel &rest initargs
			   &key start-vector step-vector edge-handler name ->)
  (declare (ignore start-vector step-vector edge-handler name))
  (when -> (setf (getf initargs :name) ->))
  (let* ((new-kernel-array 
	  (cond ((listp kernel) (array-from-list kernel :element-type 'single-float))
		((arrayp kernel) (copy kernel :-> (make-array (dimensions kernel)
							      :element-type 'single-float)))
		((filter-p kernel) (copy (kernel kernel)))
		(t (error "MAKE-FILTER accepts only lists, arrays, or filters~%")))))
    (with-result ((result nil)
		  `(:class filter :kernel ,new-kernel-array ,@initargs)
		  'apply 'make-filter (list-from-array new-kernel-array) initargs)
      result)))

;;; *** Write This ***
(defun make-hex-filter (kernel
		    &key 
		    ->
		    display-type
		    edge-handler
		    start-vector
		    step-vector)
  (declare (ignore kernel -> display-type edge-handler start-vector step-vector))
  (error "Not ported yet."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MAKE-SEPARABLE-FILTER and SET-RESULT methods for SEPARABLE-FILTERS

;;; this method on separable-filters inherits from set-result method on filter

(defmethod set-result ((name t) (model separable-filter))
  (check-type name viewable-name)
  (make-instance (class-of model)
		 :name name
		 :kernel (similar (kernel model))
		 :edge-handler (edge-handler model)
		 :start-vector (start-vector model)
		 :step-vector (step-vector model)
		 :display-type (display-type model)
		 :filter-1 (with-result ((res nil) (filter-1 model)) res)
		 :filter-2 (with-result ((res nil) (filter-2 model)) res)))

;;; When this method gets called, the step-vector and start-vector
;;; slots might be empty.  The kernel slot must be a 2d array.  The
;;; filter-2 and filter-1 slots must be filters.
(defmethod initialize-instance :after ((filt separable-filter) &key &allow-other-keys)
  (cond ((not (and (filter-2 filt) (filter-p (filter-2 filt))))
	 (error "filter-2 must be a filter"))
	((not (and (filter-1 filt) (filter-p (filter-1 filt))))
	 (error "filter-1 must be a filter")))
  filt)

;;; MAKE-SEPARABLE-FILTER.  kernel-1 and kernel-2 are either filters,
;;; lists, or vectors.  If they are filters then the kernel arrays are
;;; copied into new arrays.  If they are lists or vectors then we make
;;; them into filters with make-filter.  :edge-handler keyword can be
;;; used to override the edge handlers of the separate filters.
;;; Otherwise, each one uses its own edge-handler.  
;;; **** This function breaks for kernels with dimensions of 1 in any
;;; direction.  e.g. What if kernel-1 is already a y-filter!  Then
;;; this will create a 3d filter.
(defun make-separable-filter (kernel-1 kernel-2 &rest initargs
			      &key 
			      (display-type nil display-specified-p)
			      (edge-handler nil edge-handler-specified-p)
			      (start-vector nil start-vector-specified-p)
			      (step-vector nil step-vector-specified-p)
			      name ->)
  (declare (ignore name))
  (when -> (setf (getf initargs :name) ->))
  (let (filter-1 filter-2 kernel)
    (cond ((filter-p kernel-1)
	   (setq filter-1 (copy kernel-1))
	   (if edge-handler-specified-p
	       (setf (edge-handler filter-1) edge-handler)
	       (setq edge-handler (edge-handler filter-1)))
	   (if display-specified-p
	       (setf (display-type filter-1) display-type)
	       (setq display-type (display-type filter-1)))
	   (setf (start-vector filter-1)
		 (sublist-of-length (rank filter-1)
				    (if start-vector-specified-p
					start-vector
					(start-vector filter-1))
				    0))
	   (setf (step-vector filter-1)
		 (sublist-of-length (rank filter-1)
				    (if step-vector-specified-p
					step-vector
					(step-vector filter-1))
				    1)))
	  (t (setq filter-1
		   (apply 'make-filter kernel-1
			  :start-vector
			  (sublist-of-length (rank kernel-1) start-vector 0)
			  :step-vector
			  (sublist-of-length (rank kernel-1) step-vector 1)
			  initargs))))
    (cond ((filter-p kernel-2)
	   (setq filter-2 (copy kernel-2))
	   (if edge-handler-specified-p
	       (setf (edge-handler filter-2) edge-handler)
	       (setq edge-handler (edge-handler filter-2)))
	   (if display-specified-p
	       (setf (display-type filter-2) display-type)
	       (setq display-type (display-type filter-2)))
	   (setf (start-vector filter-2)
		 (sublist-of-length (rank filter-2)
				    (if start-vector-specified-p
					(nthcdr (rank filter-1) start-vector)
					(start-vector filter-2))
				    0))
	   (setf (step-vector filter-2)
		 (sublist-of-length (rank filter-2)
				    (if step-vector-specified-p
					(nthcdr (rank filter-1) step-vector)
					(step-vector filter-2))
				    1)))
	  (t (setq filter-2
		   (apply 'make-filter kernel-2
			  :start-vector
			  (sublist-of-length
			   (rank kernel-2) (nthcdr (rank filter-1) start-vector) 0)
			  :step-vector
			  (sublist-of-length
			   (rank kernel-1) (nthcdr (rank filter-1) step-vector) 1)
			  initargs))))
    (setq start-vector (append (start-vector filter-1) (start-vector filter-2)))
    (setq step-vector (append (step-vector filter-1) (step-vector filter-2)))
    (setq kernel (array-cross-product (kernel filter-1) (kernel filter-2)))
    (with-result ((result nil)
		  `(:class separable-filter
		    :kernel ,kernel :filter-2 ,filter-2 :filter-1 ,filter-1
		    :start-vector ,start-vector :step-vector ,step-vector
		    ,@initargs)
		  'apply 'make-separable-filter 
		  (list-from-array (kernel filter-1))
		  (list-from-array (kernel filter-2))
		  :start-vector (cons 'list start-vector)
		  :step-vector (cons 'list step-vector)
		  initargs)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; filter operations

;;; *** For now, to look at filters, convert them to images...
(defmethod image-from-filter ((filt filter))
  (let ((rank (rank filt)))
    (cond ((<= rank 2)
	   (image-from-array (kernel filt)))
	  ((= rank 3)
	   (make-image-sequence
	    (loop with kernel = (kernel filt)
		  with xy-size = (apply #'* (cdr (dimensions filt))) 
		  for i from 0 below (car (dimensions kernel))
		  for xy-array = (make-array (cdr (dimensions filt))
					     :element-type (array-element-type kernel)
					     :displaced-to kernel
					     :displaced-index-offset (* xy-size i))
		  collect (image-from-array xy-array))))
	  (t (error "Can't make image from filter ~A" filt)))))

;;; *** These should probably take a result arg (in which case get rid
;;; of ! on their names).  They should also take a norm arg (defaults
;;; to 1.0).
(defmethod normalize! ((filt filter))
  (with-displaced-vectors ((kernel (kernel filt)))
    (normalize kernel :-> kernel))
  filt)

(defmethod normalize! ((filt separable-filter))
  (set-not-current filt)
  (normalize! (filter-1 filt))
  (normalize! (filter-2 filt))
  (array-cross-product (kernel (filter-1 filt)) (kernel (filter-2 filt))
		       :-> (kernel filt))
  filt)

(defun impulse-response (filter dims)
  (with-local-viewables ((imp (make-impulse dims)))
    (apply-filter filter imp)))

(defmethod shift-by-pi ((filt filter) &key ->)
  (when (and (/= (x-dim filt) 1)
	     (/= (y-dim filt) 1))
    (error "Shift-by-pi method is only defined on one-dimensional filters"))
  (with-result ((result ->) filt 'shift-by-pi filt)
    (copy filt :-> result)
    (with-displaced-vectors ((filt-array (kernel filt))
			     (res-array (kernel result)))
      (loop with start = (if (oddp (floor (total-size filt) 2)) 0 1)
	    for i from start below (total-size filt) by 2 do
	    (setf (aref res-array i) (- (aref filt-array i)))))
    result))

;;; This should actually multiply by a cosine grating...
(defmethod hex-modulate ((filt hex-filter) direction dist &key (-> (copy filt)))
  (with-result ((result ->) filt 'hex-modulate filt)
    (let ((arr (kernel result))
	  (yctr (/ (1- (y-dim filt)) 2))
	  (xctr (/ (1- (x-dim filt)) 2))
	  (ydir (car direction))
	  (xdir (cadr direction)))
      (loop for y from 0 below (y-dim filt) do
	    (loop for x from 0 below (x-dim filt) do 
		  (when (= (/ dist 2) 
			   (mod (+ (* ydir (- y yctr)) (* xdir (- x xctr))) dist))
		    (setf (aref arr y x) (- (aref arr y x)))))))
    result))

;;; NOTE: start-vector, step-vector, and edge-handler are copied by set-result.
(defmethod copy ((filt filter) &key ->)
  (with-result ((result ->) filt 'copy filt)
    (copy (kernel filt) :-> (kernel result))
    result))

(defmethod copy ((filt separable-filter) &key ->)
  (with-result ((result ->) filt 'copy filt)
    (with-slots (kernel filter-2 filter-1) filt
      (copy kernel :-> (kernel result))
      (copy filter-2 :-> (filter-2 result))
      (copy filter-1 :-> (filter-1 result)))
    result))

;;; Need these defined so it doesn't call method in generic-ops.lisp
(defmethod mul ((constant number) (filt filter) &key ->)
  (mul filt constant :-> ->))

;;; In filter.lisp
;;; Need these defined so it doesn't call method in generic-ops.lisp
(defmethod add ((constant number) (filt filter) &key ->)
  (mul filt constant :-> ->))

(defmethod mul ((filt filter) (constant number) &key ->)
  (with-result ((result ->) filt 'mul filt constant)
    (with-slots (kernel) filt
      (mul kernel constant :-> (kernel result)))
    result))

(defmethod mul ((filter separable-filter) (constant number) &key ->)
  (with-result ((result ->) filter 'mul filter constant)
    (let ((sqrt-constant (sqrt (abs constant))))
      (with-slots (kernel filter-2 filter-1) filter
        (mul kernel constant :-> (kernel result))
        (mul (kernel filter-2) (* sqrt-constant (signum constant))
			    :-> (kernel (filter-2 result)))
        (mul (kernel filter-1) sqrt-constant 
			    :-> (kernel (filter-1 result)))))
    result))

(defmethod add ((filter filter) (constant number) &key ->)
  (with-result ((result ->) filter 'add filter constant)
    (with-slots (kernel) filter
      (add kernel constant :-> (kernel result)))
    result))

(defmethod add ((filter separable-filter) (constant number) &key ->)
  (with-result ((result ->) filter 'add filter constant)
    (let ((sqrt-constant (sqrt constant)))
      (with-slots (kernel filter-2 filter-1) filter
        (add kernel constant :-> (kernel result))
        (add (kernel filter-2) sqrt-constant 
			    :-> (kernel (filter-2 result)))
        (add (kernel filter-1) sqrt-constant 
			    :-> (kernel (filter-1 result)))))
    result))

;;; Adding any two filters (separable or nonseparable) results in a
;;; nonseparable filter.
(defmethod add ((filter1 filter) (filter2 filter) &key ->)
  (check-size filter1 filter2)
  (with-result ((result ->)
		`(:class filter
		  :kernel ,(similar (kernel filter1))
		  :edge-handler ,(edge-handler filter1)
		  :start-vector ,(start-vector filter1)
		  :step-vector ,(step-vector filter1)
		  :display-type ,(display-type filter1))
		'add filter1 filter2)
    (add (kernel filter1) (kernel filter2) :-> (kernel result))
    result))

;;; Subtracting any two filters (separable or nonseparable) results in a
;;; nonseparable filter
(defmethod sub ((filter1 filter) (filter2 filter) &key ->)
  (check-size filter1 filter2)
  (with-result ((result ->)
		`(:class filter
		  :kernel ,(similar (kernel filter1))
		  :edge-handler ,(edge-handler filter1)
		  :start-vector ,(start-vector filter1)
		  :step-vector ,(step-vector filter1)
		  :display-type ,(display-type filter1))
		'sub filter1 filter2)
    (sub (kernel filter1) (kernel filter2) :-> (kernel result))
    result))

;;; Negating any filter (separable or nonseparable) results in a 
;;; nonseparable filter
(defmethod negate ((filter filter) &key ->)
  (mul filter -1 :-> ->))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; APPLY methods (correlate and downsample).

;;; **** These need to be made smarter: they should do fft's when the
;;; filter kernels are large!

;;; Rotate filter kernel for application to a higher dimension.  This
;;; just displaces an array to the original kernel and extends the
;;; start- and step- vectors to account for the new dimensions.  It is
;;; called within apply-filter.  The direction should be an integer
;;; indicating how meny dimensions to shift over.  For example, to
;;; apply a rank one filter in the y-direction, use direction = 1.
;;; *** Doesn't do the right thing for separable filters.
(defmethod directionalize ((filter filter) direction)
  (if (> direction 0)
      (let ((kernel (kernel filter)))
	(make-instance (class-of filter)
		       :kernel (make-array (append (dimensions kernel)
						   (list-of-length direction 1))
					   :element-type (array-element-type kernel)
					   :displaced-to kernel)
		       :start-vector (append (start-vector filter)
					     (list-of-length direction 0))
		       :step-vector (append (step-vector filter)
					    (list-of-length direction 1))
		       :edge-handler (edge-handler filter)))
      (copy filter)))

(defun subsampled-dimensions (orig-dimensions start step)
  (nreverse
   (loop with rdims = (reverse orig-dimensions)
	 for rstart = (reverse start) then (cdr rstart)
	 for rstep = (reverse step) then (cdr rstep)
	 for dim in rdims
	 collect (max 1 (ceiling (- dim (or (car rstart) 0)) (or (car rstep) 1))))))

;;; :direction keyword indicates the filter orientation: 0=x, 1=y, etc.
(defmethod apply-filter ((filter filter) (im image) &key -> (direction 0))
;;  (declare (optimize (debug 3)))
  (with-local-viewables ((filt (directionalize filter direction))
			 (edge-handler (edge-handler filter)))
    (when (> (rank filt) (rank im))
      (error "Rank of ~A is too large to be applied to ~A in direction ~A"
	     filter im direction))
    (when (or (> (y-dim filt) (y-dim im)) (> (x-dim filt) (x-dim im)))
      (error "Filter dimensions ~A are greater than image dimensions ~A."
	     (dimensions filt) (dimensions im)))
    (when (or (> (y-start filt) (y-dim im)) (> (x-start filt) (x-dim im)))
      (error "Filter start ~A is greater than image dimensions ~A."
	     (start-vector filt) (dimensions im)))
    (when (and (edge-handler filt)
	       (not (member (edge-handler filt) *filter-edge-handlers*)))
      (error "Bad edge handler: ~A.  ~%Legitimate choices are: ~A"
	     (edge-handler filt) *filter-edge-handlers*))
;;    (break)
    (with-result ((result ->)
		  (list :class (class-of im)
			:dimensions (subsampled-dimensions
				     (dimensions im) (start-vector filt)
				     (step-vector filt))
			:display-type (display-type im))
		    'apply-filter filter im :direction direction)
      (when (eq im result)
	(error "Sorry, destructive modification impossible for convolutions."))
      (if edge-handler
	  (if (hex-filter-p filt)
	      (internal-hex-filter (data im) (x-dim im) (y-dim im)
				   (kernel filt) (similar (kernel filt))
				   (x-dim filt) (y-dim filt) (x-start filt) (x-step filt)
				   (y-start filt) (y-step filt) (data result)
				   (string-downcase (symbol-name edge-handler))
				   (1+ (hex-start filt)))
	      (internal-filter (data im) (x-dim im) (y-dim im)
			       (kernel filt) (similar (kernel filt))
			       (x-dim filt) (y-dim filt) (x-start filt) (x-step filt)
			       (y-start filt) (y-step filt) (data result)
			       (string-downcase (symbol-name edge-handler))))
	  (if (hex-filter-p filt)
	      (internal-wrap-filter (data im) (x-dim im) (y-dim im)
				    (kernel filt)
				    (x-dim filt) (y-dim filt) (x-start filt) (x-step filt)
				    (y-start filt) (y-step filt) 
				    (data result) (1+ (hex-start filt)))
	      (internal-wrap-filter (data im) (x-dim im) (y-dim im)
				    (kernel filt)
				    (x-dim filt) (y-dim filt) (x-start filt) (x-step filt)
				    (y-start filt) (y-step filt)
				    (data result) 0)))
      result)))

(defmethod apply-filter ((filter separable-filter) (im image) &key (direction 0) ->)
;;  (declare (optimize (debug 3)))
  (when (> (+ direction (rank filter)) (rank im))
    (error "Rank of ~A is too large to be applied to ~A in direction ~A"
	   filter im direction))
  (with-slots (filter-2 filter-1) filter
    (with-result ((result  ->)
		  (list :class (class-of im)
			:dimensions (subsampled-dimensions (dimensions im)
				     (start-vector filter) (step-vector filter)))
		  'apply-filter filter im :direction direction)
       (with-local-viewables ((tmp (apply-filter filter-2 im :direction direction)))
	 (apply-filter filter-1 tmp :direction (+ direction (rank filter-2))
		       :-> result)))))

;;; For separable filter and one-d-image, only apply the appropriate sub-filter.
(defmethod apply-filter ((filter separable-filter) (im one-d-image) &key -> (direction 0)) 
  (with-slots (filter-2 filter-1) filter
    (let* ((filt  (if (= (x-dim im) 1) filter-1 filter-2)))
      (when (> (+ direction (rank filt)) (rank im))
	(error "Rank of ~A is too large to be applied to ~A in direction ~A"
	       filt im direction))
      (with-result ((result ->)
		    (list :class (class-of im)
			  :dimensions 
			  (subsampled-dimensions (dimensions im)
						 (start-vector filt) (step-vector filt)))
		    'apply-filter filter im :direction direction)
	(apply-filter filt im :-> result)))))

;;; Reversed args!  *** Should get rid of this ...
(defmethod apply-filter ((thing t) (filter filter) &rest keys)
  (apply 'apply-filter filter thing keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EXPAND methods (upsample and convolve).  These add expanded image
;;; to the result image.

;;; Check for image-p result arg at beginning is there because result
;;; size is non-unique.
(defmethod expand-filter ((filter filter) (im image) &key (zero t) (direction 0) ((:-> res))
			  &allow-other-keys)
  (with-local-viewables ((filt (directionalize filter direction))
			 (edge-handler (edge-handler filter)))
    (if (not (image-p res))
	(setq res (make-image (list (* (y-dim im) (y-step filt)) (* (x-dim im) (x-step filt)))
			      :display-type (display-type im) :name res))
	(when (not (equal (subsampled-dimensions
			   (dimensions res) (start-vector filt) (step-vector filt))
			  (dimensions im)))
	  (error "Result argument size is incompatible.")))
    (when (> (rank filt) (rank im))
      (error "Rank of ~A is too large to be applied to ~A in direction ~A"
	     filter im direction))
    (when (or (> (y-dim filt) (y-dim res))
	      (> (x-dim filt) (x-dim res)))
      (error "Filter dimensions ~A are greater than image dimensions ~A." 
	     (dimensions filt) (dimensions res)))
    (when (or (> (y-start filt) (y-dim res)) (> (x-start filt) (x-dim res)))
      (error "Filter start ~A is greater than image dimensions ~A."
	     (start-vector filt) (dimensions res)))
    (when (and (edge-handler filt)
	       (not (member (edge-handler filter) *filter-edge-handlers*)))
      (error "Bad edge handler: ~A.  ~%Legitimate choices are: ~A"
	     (edge-handler filter) *filter-edge-handlers*))
    (with-result ((result res) res 'expand-filter filter im :zero zero :direction direction)
      (when (eq im result)
	(error "Sorry, destructive modification impossible for convolutions."))
      (when zero (zero! result))
      (if edge-handler
	  (if (hex-filter-p filt)
	      (internal-hex-expand (data im) 
				   (kernel filt) (similar (kernel filt))
				   (x-dim filt) (y-dim filt)
				   (x-start filt) (x-step filt) 
				   (y-start filt) (y-step filt) 
				   (data result) (x-dim result) (y-dim result)
				   (string-downcase (symbol-name edge-handler))
				   (1+ (hex-start filt)))
	      (internal-expand (data im) 
			       (kernel filt) (similar (kernel filt))
			       (x-dim filt) (y-dim filt)
			       (x-start filt) (x-step filt) 
			       (y-start filt) (y-step filt) 
			       (data result) (x-dim result) (y-dim result)
			       (string-downcase (symbol-name edge-handler))))
	  (if (hex-filter-p filt)
	      (internal-wrap-expand (data im) (kernel filt)
				(x-dim filt) (y-dim filt)
				(x-start filt) (x-step filt) 
				(y-start filt) (y-step filt) 
				(data result) (x-dim result) (y-dim result)
				(1+ (hex-start filt)))
	      (internal-wrap-expand (data im) (kernel filt)
				(x-dim filt) (y-dim filt)
				(x-start filt) (x-step filt) 
				(y-start filt) (y-step filt) 
				(data result) (x-dim result) (y-dim result)
				0)))
      result)))

(defmethod expand-filter ((filt separable-filter) (im image)
			  &key (zero t) (direction 0) ((:-> res)))  
  (if (not (image-p res))
      (setq res (make-image (list (* (y-dim im) (y-step filt)) (* (x-dim im) (x-step filt)))
			    :display-type (display-type im) :name res))
      (when (not (equal (subsampled-dimensions (dimensions res)
					       (start-vector filt) (step-vector filt))
			(dimensions im)))
	(error "Result argument size is incompatible.")))
  (when (> (+ direction (rank filt)) (rank im))
    (error "Rank of ~A is too large to be applied to ~A in direction ~A"
	   filt im direction))
  (with-slots (filter-2 filter-1) filt
    (with-result ((result res) res 'expand-filter filt im :direction direction :zero zero)
      (with-local-viewables ((tmp (make-image (list (y-dim im) (x-dim result)))))
	(expand-filter filter-1 (expand-filter filter-2 im :direction direction :-> tmp) 
		       :zero zero :direction (+ direction (rank filter-2)) :-> result))
      result)))

(defmethod expand-filter ((filter separable-filter) (im one-d-image)
			  &key (zero t) (direction 0) ((:-> res)))
  (with-slots (filter-2 filter-1) filter
    (let* ((filt  (if (= (x-dim im) 1) filter-1 filter-2)))
      (when (> (+ direction (rank filter)) (rank im))
	(error "Rank of ~A is too large to be applied to ~A in direction ~A"
	       filter im direction))
      (with-result ((result res) res 'expand-filter filter im)
	(expand-filter filt im :zero zero :direction direction :-> result)
	result))))

(defmethod expand-filter ((image image) filter &key (zero t) (direction 0) ((:-> res)))
  (expand-filter filter image :zero zero :direction direction :-> res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EXAMPLES

#|
(make-filter '(1.0 2.0 1.0) 
	     :-> 'f121
	     :start-vector '(0 0)
	     :step-vector '(1 1)
	     :edge-handler :reflect1)

(make-separable-filter f121 f121 
		       :-> 'sf121)

(make-separable-filter '(1.0 2.0 1.0)
		       '(1.0 2.0 1.0)
		       :-> 'sf121
		       :edge-handler :reflect1
		       :start-vector '(0 0)
		       :step-vector '(2 2))

(load-image "/usr/local/images/einstein")

(apply-filter sf121 einstein :-> 'small-einstein)
|#

#|
(setq f121 (make-filter '(0.25 0.5 0.25))
      f110 (make-filter '(0.5 0.5 0.0))
      f011 (make-filter '(0.0 0.5 0.5))
      f01-1 (make-filter '(0.0 0.5 -0.5))
      f1-10 (make-filter '(0.5 -0.5 0.0))
      f-12-1 (make-filter '(-0.25 0.5 -0.25))
      f10-1 (make-filter '(0.5 0.0 -0.5))
      f101 (make-filter '(0.5 0.0 0.5))
      f14641 (make-filter '(0.0625 0.25 0.375 0.25 0.0625))
      fx14641  (make-filter '(0.125 0.5 0.75 0.5 0.125)))

(setq gauss (make-separable-filter '(0.0625 0.25 0.375 0.25 0.0625)
				   '(0.0625 0.25 0.375 0.25 0.0625)
				   :step-vector '(2 2)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Filtering on image-sequences

;;; *** ISSUES: how differently should the time domain be treated?
;;; Should we allow keywords to override start-, step-, and end-
;;; frames?  How do we make the default direction of application for a
;;; 1-D filter be Z? Z-edge handling?
#|  No longer needed: generic one on (thing filter) covers this
(defmethod apply-filter ((seq image-sequence) (filter filter)
			 &key -> 
			 (start-frame 0)
			 (end-frame (sequence-length seq))
			 (direction (if (= (rank filter) 1) 2 0)))
  (apply-filter filter seq :start-frame start-frame :end-frame end-frame
		:direction direction :-> ->))
|#

(defmethod apply-filter ((filter filter) (seq image-sequence)
			 &key -> 
			 (start-frame 0)
			 (end-frame (sequence-length seq))
			 (direction (if (= (rank filter) 1) 2 0)))
  (with-local-viewables ((filt (directionalize filter direction)))
    (when (> (rank filt) (rank seq))
      (error "Rank of ~A is too large to be applied to ~A in direction ~A"
	     filter seq direction))
    (with-result ((result ->)
		  (list :class (class-of seq)
			:dimensions (subsampled-dimensions
				     (cons (- end-frame start-frame) (dimensions seq))
				     (start-vector filt) (step-vector filt)))
		  'apply-filter filter seq
		  :start-frame start-frame :end-frame end-frame)
      (if (< (rank filt) 3)		;a spatial filter
	  (loop for res-frame from 0 below (z-dim result)
		for frame from (+ (z-start filter) start-frame) by (z-step filter)
		for arg-im = (frame frame seq)
		for res-im = (frame res-frame result)
		do (apply-filter filter arg-im :-> res-im :direction direction
				 :start-frame start-frame :end-frame end-frame))
	  (loop for res-frame from 0 below (z-dim result)
		for ctr-frame from (+ (z-start filt) start-frame) by (z-step filt)
		do
		(when (> (z-dim result) 1)
		  (status-message "Apply-filter: frame ~A" res-frame))
		(single-frame-correlate filt seq 
					:center-frame ctr-frame
					:-> (frame res-frame result))
		finally (status-message "")))
      result)))

(defmethod apply-filter ((filter separable-filter) (seq image-sequence)
			 &key -> (direction 0)
			 (start-frame 0)
			 (end-frame (sequence-length seq)))
  (when (> (+ direction (rank filter)) (rank seq))
    (error "Rank of ~A is too large to be applied to ~A in direction ~A"
	   filter seq direction))
  (with-slots (filter-2 filter-1) filter
    (with-result ((result  ->)
		  (list :class (class-of seq)
			:dimensions (subsampled-dimensions
				     (cons (- end-frame start-frame) (dimensions seq))
				     (start-vector filter) (step-vector filter)))
		  'apply-filter filter seq :start-frame start-frame
		  :end-frame end-frame :direction direction)
      (cond ((< (+ direction (rank filter)) 3)
	     (loop for res-frame from 0 below (z-dim result)
		   for frame from (+ (obv::z-start filter) start-frame) by (obv::z-step filter)
		   for arg-im = (frame frame seq)
		   for res-im = (frame res-frame result)
		   do (apply-filter filter arg-im :-> res-im :direction direction
				    :start-frame start-frame :end-frame end-frame)))
	    (t
	     (with-local-viewables
		 ((dfilt (obv::directionalize filter direction)) ;just to get z-dim
		  (tmp (apply-filter filter-1 seq :direction (+ direction (rank filter-2))
				     :start-frame start-frame :end-frame end-frame)))
	       (loop for frame from 0 below (z-dim result)
		     do
		     (apply-filter filter-2 (frame frame tmp)
				   :direction direction
				   :-> (frame frame result))))))
      result)))

;;; Kludge: Just compute one z slice of a 3d circular convolution.  
;;; *** Currently ignores z-edge-handler (always does :repeat)!
;;; *** DOesn't know about separable filters. 
(defun single-frame-correlate (filt seq &key -> (center-frame (floor (z-dim filt) 2)))
  (with-result ((result ->) (list :class 'image
				  :dimensions (subsampled-dimensions
					       (dimensions seq) (start-vector filt) (step-vector filt)))
		'single-frame-correlate filt seq :center-frame center-frame)
    (zero! result)
    (with-local-viewables ((temp-im (make-image (dimensions result)))
			   (kernel (kernel filt))
			   (xy-dims (list (y-dim kernel) (x-dim kernel)))
			   (z-filt-dim (z-dim kernel)))
      (if (= (y-dim kernel) (x-dim kernel) 1) ;temporal filter
	  (loop with z-vect = (vectorize kernel)
		for z from 0 below z-filt-dim
		for frame-num from (- center-frame (floor z-filt-dim 2))
		for frame = (frame seq (clip frame-num 0 (1- (sequence-length seq))))
		do 
		(add (mul frame (aref z-vect z) :-> temp-im) result :-> result))
	  (loop with xy-filt = (make-filter
				(make-array xy-dims
					    :element-type (array-element-type kernel)
					    :displaced-to kernel)
				:start-vector (start-vector filt)
				:step-vector (step-vector filt))
		with xy-filt-size = (total-size xy-filt)
		for z from 0 below z-filt-dim
		for frame-num from (- center-frame (floor z-filt-dim 2))
		for frame = (frame seq (clip frame-num 0 (1- (sequence-length seq))))
		do 
		(setf (kernel xy-filt)
		      (make-array xy-dims
				  :element-type (array-element-type kernel)
				  :displaced-to kernel
				  :displaced-index-offset (* xy-filt-size z)))
		;; ***Wasteful call to apply-filter here.  Should call C code directly.
		(add (apply-filter xy-filt frame :-> temp-im)
		     result :-> result))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Interpolation, subsampling, and blurring methods

;;; *** Fix the default edge-handler here. 
(defun interpolate-hex-image (im &key (filter (make-filter '((0.0  0.25 0.0)
							     (0.25 1.0  0.25)
							     (0.0  0.25 0.0))))
				 ->)
  (with-result ((result ->) im 'interpolate-hex-image im filter)
    (apply-filter filter im :-> result)))

;;; 5x5 separable gaussian convolution with optional subsampling
;;; kernel can be 1xN or Nx1 array or vector or list
;;; calls APPLY-FILTER
;;; Note: this works for one-d-images as well
;;; *** These do funny things with odd images.... -EPS
;;; *** Why not just have a :filter keyword?  -EPS
(defmethod gauss-out ((image image) 
		      &key ->
		      (resample 2)
		      (edge-handler :reflect1)
		      (kernel gauss-5))
  "  (gauss-out im &key :-> :resample :kernel)  
  Gaussian convolution and resample."
  (let ((filter (make-separable-filter kernel kernel 
				       :step-vector (list resample resample)
				       :edge-handler edge-handler)))

    (with-result ((result ->)
		  (list :class (class-of image)
			:dimensions 
			(subsampled-dimensions (dimensions image)
					       (start-vector filter) (step-vector filter)))
		  'gauss-out image 
		  :kernel (list-from-array (kernel (filter-2 filter)))
		  :resample resample
		  :edge-handler edge-handler)
      (apply-filter filter image :-> result))))

;;; 5x5 separable gaussian convolution with optional supersampling
;;; calls EXPAND-FILTER.  Don't use with-result here since you'll have
;;; to do the big nasty cond like in expand-filter!
(defmethod gauss-in ((image image) 
		     &key ->
		     (resample 2)
		     (edge-handler :reflect1)
		     (kernel (mapcar #'(lambda (x) (* x 2)) gauss-5)))
  "  (gauss-in im &key :-> :resample :kernel)  
  Gaussian convolution and resample."
  (let ((filter (make-separable-filter kernel kernel 
				       :step-vector (list resample resample)
				       :edge-handler edge-handler)))
    (expand-filter filter image :-> ->)))

;;; Recursive method for doing Gaussian blurring.  WARNING: Lots of aliasing in this
;;; procedure!
(defmethod blur ((im image)
		 &key ->
		 (level 1)
		 (kernel (mapcar #'(lambda (x) (* x (sqrt 2))) gauss-5))
		 (edge-handler :reflect1))
  (with-result ((result ->) im 'blur im :level level :kernel kernel :edge-handler edge-handler)
    (if (<= level 0)
	(if (eq im result) result (copy im :-> result))	;no blurring
	(with-local-viewables
	    ((next-lev (gauss-out im :kernel kernel :edge-handler edge-handler)))
	  (blur next-lev
		:level (1- level)
		:edge-handler edge-handler 
		:kernel kernel 
		:-> next-lev)
	  (gauss-in next-lev
		    :edge-handler edge-handler 
		    :kernel kernel
		    :-> result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; apply-filter and expand-filter for viewable-matrices

(defmethod apply-filter ((filter filter) (mat viewable-matrix)
			 &key -> (direction 0))
  (unary-viewable-matrix-op 'apply-filter mat -> filter
			    :direction direction))

(defmethod expand-filter ((filter filter) (mat viewable-matrix)
			  &key -> (zero t) (direction 0))
  (unary-viewable-matrix-op 'expand-filter mat -> filter
			    :direction direction
			    :zero zero))



;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
