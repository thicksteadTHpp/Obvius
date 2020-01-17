;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: pyramid.lisp
;;;  Author: David Heeger
;;;  Description: general pyramids and filter pyramids
;;;  Creation Date: summer '88
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '(pyramid-p qmf-pyramid-p
	  make-qmf-pyramid make-separable-qmf-pyramid
	  build collapse access access-band
	  ;;access-diag access-hor access-vert
	  ;;make-hex-qmf-pyramid access-rdiag access-ldiag
	  height
	  ))

;;;--------------------- Pyramid Classes -------------------------

(defmethod inferiors-of ((pyr pyramid))
  (append (list (original pyr) (low-band pyr)) (levels pyr)))

(defmacro pyramid-p (obj)
  `(typep ,obj 'pyramid))

(defmacro qmf-pyramid-p (obj)
  `(typep ,obj 'qmf-pyramid))

#|
;;; Same as generic method on viewables.
(defmethod notify-of-inferior-destruction ((pyr pyramid) inf-vbl)
  (cerror "Destroy both ~A and ~A"
	  "Trying to destroy ~A, which is contained in ~A."
	  inf-vbl pyr)
  (destroy pyr))
|#

;;; Low filter must be first one on the filters list.
(defun make-qmf-pyramid (image &rest initargs
			       &key level name display-type filters ->)
  (when -> (setf (getf initargs :name) ->))
  (remf initargs :level)
  (remf initargs :filters)
  (with-result ((result nil)
		`(:class qmf-pyramid
		  :low-band ,image :original ,image
		  :forward-filters ,(cdr filters) :inverse-filters ,(cdr filters)
		  :forward-low-filter ,(car filters) :inverse-low-filter ,(car filters)
		  ,@initargs)
		'apply 'make-qmf-pyramid image initargs)
    (when level (build result level))
    result))

(defmethod height ((pyr pyramid))
  (length (levels pyr)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; UTILITIES

;;; The routine SET-RESULT is used by all functions which return 
;;; pyramids as results.  It is slightly different from SET-RESULT for images
;;;  1) If the result parameter is nil, create a new, blank pyramid of the
;;;     same type.
;;;  2) If the result parameter is a pyramid, check the tree structure.
;;;  3) If the result is a string or symbol, create a new pyramid with this name.
;;;  4) If the result is a symbol, create blank pyramid with this name, and 
;;;     bind the symbol to it, warning the user if the symbol is already bound.
;;;  The parameter pyr must be a pyramid.

(defmethod set-result ((name t) (model pyramid))
  (check-type name viewable-name)
  (make-instance (class-of model)
		 :name name
		 :display-type (display-type model)
		 :forward-filters (forward-filters model)
		 :forward-low-filter (forward-low-filter model)
		 :inverse-filters (inverse-filters model)
		 :inverse-low-filter (inverse-low-filter model)
		 :original (original model)
		 :low-band (similar (low-band model))
		 :levels (mapcar #'similar (levels model))))

;;; *** This should do some more error checking ***
(defmethod set-result ((res pyramid) (model-plist list))
  (unless (typep res (getf model-plist :class))
    (error "Result ~a is incompatible with argument type ~a"
	   res (getf model-plist :class)))
  res)

(defmethod initialize-instance :after
     ((pyr pyramid) &key &allow-other-keys)
  (when (low-band pyr) (pushnew pyr (superiors-of (low-band pyr))))
  (dolist (level (levels pyr)) (pushnew pyr (superiors-of level))))

;;; Check that the tree structures are the same -- leave the checking of the
;;; individual image dimensions to the functions which operate on the
;;; images themselves.
(defmethod check-size ((pyr pyramid) &rest pyr-list)
  (cond ((null pyr-list) pyr)
	((/= (length (levels pyr)) (length (levels (car pyr-list))))
	 (error "Pyramid tree structures are different"))
	(t (apply 'check-size (car pyr-list) (cdr pyr-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ACCESS, BUILD and COLLAPSE routines.

(defmethod access ((pyr pyramid) level &key ((:-> res)))
  (when (<= (length (levels pyr)) level)
    (error "Pyramid contains only ~A levels.  Call BUILD to add more."
	   (length (levels pyr))))
  (let ((level (nth level (levels pyr))))
    (cond ((null res) level)
	  ((viewable-p res) (copy level :-> res))
	  (t (set-name level res) level))))


;;; Return a particular band out of a level.  level parameter can be a
;;; number (just a single level), a list of two numbers (range of
;;; levels), or t (all levels).
;;; *** Needs a -> arg, handled as in access.
(defmethod access-band ((pyr pyramid) &key level band)
  (cond ((eq level t) (setq level (list 0 (1- (length (levels pyr))))))
	((numberp level) (setq level (list level level)))
	((not (and (listp level) (= (length level) 2) (every #'numberp level)))
	 (error ":level parameter should be a number, a list of two numbers, ~
                 or t (indicates ALL levels).")))
  (unless (<= 0 (car level) (cadr level) (1- (length (levels pyr))))
    (error "Legal level numbers must be between 0 and ~A"
	   (1- (length (levels pyr)))))
  (if (apply #'= level)
      (frame band (nth (car level) (levels pyr)))
      (make-viewable-sequence
       (loop for lev from (car level) to (cadr level)
	     collect (frame band (nth lev (levels pyr)))))))

;;; Build pyramid up to given level. *** Is this general enough?
(defmethod build ((pyr pyramid) level)
  (when (<= (length (levels pyr)) level) 
    (set-not-current pyr)
    (loop with new-low
	  with new-level
	  for i from (length (levels pyr)) below level
	  for low = (low-band pyr) then new-low
	  do				;build next level, install in pyramid
	  (multiple-value-setq (new-low new-level)
	    (build-level pyr low))
	  (setf (levels pyr) (append (levels pyr) (list new-level)))
	  (pushnew pyr (superiors-of new-level))
	  finally			;install final lowpass in pyramid
	  (when (low-band pyr) (delete pyr (superiors-of (low-band pyr))))
	  (when new-low
	    (setf (low-band pyr) new-low)
	    (pushnew pyr (superiors-of new-low)))))
  pyr)

;;; This method captures the construction algorithm of the pyramid.
;;; It builds a pyramid level from a (lowpass) image.  It returns a
;;; new lowpass (to be used as the basis of the next level) and a new
;;; level consisting of a sequence of bands.
(defmethod build-level ((pyr pyramid) low)
  (let ((new-low (apply-filter (forward-low-filter pyr) low))
	(bands (loop for f in (forward-filters pyr)
		     collect (apply-filter f low))))
    (values new-low
	    (if (image-p low)		;*** gross!
		(make-image-sequence bands :display-type 'pasteup)
		(make-viewable-sequence bands)))))

(defmethod collapse ((pyr pyramid))
  (loop with start-lev = (1- (length (levels pyr)))
	for lev from start-lev downto 0
	for low = (collapsible-low-band pyr)
	then res
	for res = (if (> lev 0)
		      (similar (access-band pyr :level (1- lev) :band 0))
		      (similar (original pyr)))
	do
	(expand-filter (inverse-low-filter pyr) low :-> res)
	(destroy low)
	(loop for f in (inverse-filters pyr)
	      for n from 0
	      for b = (access-band pyr :level lev :band n)
	      do 	
	      (expand-filter f b :-> res :zero nil))
	finally (return res)))

;;; see steer-pyramid.lisp for specialized version
(defmethod collapsible-low-band ((pyr pyramid))
  (copy (low-band pyr)))

#|
;;; Old version, decided it was too hairy with all the keywords.  DH, 3/94.
(defmethod collapse ((pyr pyramid)
		     &key (include-low t) (levels t) (bands t) (to-level 0))
  (cond ((eq levels t)
	 (setq levels (loop for i from 0 below (length (levels pyr)) collect i)))
	((numberp levels) (setq levels (list levels)))
	((not (and (listp levels) (every #'integerp levels)))
	 (error ":levels parameter should be a number, a list of numbers, ~
                      or t (indicates ALL levels).")))
  (cond ((eq bands t)
	 (setq bands (loop for i from 0 below (sequence-length (car (levels pyr))) collect i)))
	((numberp bands) (setq bands (list bands)))
	((not (and (listp bands) (every #'integerp bands)))
	 (error ":bands parameter should be a number, a list of numbers, ~
                      or t (indicates ALL bands).")))
  (loop with max-lev = (1- (length (levels pyr)))
	with start-lev = (if include-low max-lev (apply #'max levels))
	for lev from start-lev downto to-level
	for low = (if include-low
		      (collapsible-low-band pyr)
		      (similar (access-band pyr :level start-lev :band 0)))
	then res
	for res = (if (> lev 0)
		      (similar (access-band pyr :level (1- lev) :band 0))
		      (similar (original pyr)))
	do
	(expand-filter (inverse-low-filter pyr) low :-> res)
	(destroy low)
	(when (member lev levels)
	  (loop for n in bands
		for f = (nth n (inverse-filters pyr))
		for b = (access-band pyr :level lev :band n)
		do 	
		(expand-filter f b :-> res :zero nil)))
	finally (return res)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; macros for pyramid operations 

(defmacro unary-pyramid-op (function pyr res &rest args)
  (let ((result-pyr (gensym)) (lev (gensym)) (result-lev (gensym)))
    `(with-result ((,result-pyr ,res)
		   ,pyr
		   ,function ,pyr ,@args)
      (loop for ,lev in (levels ,pyr)
	    for ,result-lev in (levels ,result-pyr)
	    do
	    (funcall ,function ,lev ,@args :-> ,result-lev))
      ,result-pyr)))

;; like unary-pyramid-op, but reverses order of pyr and args
(defmacro unary-pyramid-reverse-op (function pyr res &rest args)
  (let ((result-pyr (gensym)) (lev (gensym)) (result-lev (gensym)))
    `(with-result ((,result-pyr ,res)
		   ,pyr
		   ,function ,pyr ,@args)
      (loop for ,lev in (levels ,pyr)
	    for ,result-lev in (levels ,result-pyr)
	    do
	    (funcall ,function ,@args ,lev :-> ,result-lev))
      ,result-pyr)))

(defmacro binary-pyramid-op (function pyr1 pyr2 res &rest args)
  (let ((result-pyr (gensym)) (lev1 (gensym)) (lev2 (gensym)) (result-lev (gensym)))
    `(with-result ((,result-pyr ,res)
		   (check-size ,pyr1 ,pyr2)
		   ,function ,pyr1 ,pyr2 ,@args)
      (loop for ,lev1 in (levels ,pyr1)
	    for ,lev2 in (levels ,pyr2)
	    for ,result-lev in (levels ,result-pyr)
	    do
	    (funcall ,function ,lev1 ,lev2 ,@args :-> ,result-lev))
      ,result-pyr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unary ops

(defmethod fill! ((pyr pyramid) val)
  (loop for lev in (levels pyr) do
	(fill! lev val))
  (fill! (low-band pyr) val)
  (set-not-current pyr)
  (set-history pyr 'fill! pyr val)
  pyr)

(defmethod copy ((pyr pyramid) &key ->)
  (unary-pyramid-op 'copy pyr ->))

(defmethod linear-xform ((pyr pyramid) scale offset &key ->)
  (unary-pyramid-op 'linear-xform pyr -> scale offset))

(defmethod negate ((pyr pyramid) &key ->)
  (unary-pyramid-op 'negate pyr ->)) 

(defmethod clip ((pyr pyramid) below above &key ->)
  (unary-pyramid-op 'clip pyr -> below above))

(defmethod abs-value ((pyr pyramid) &key ->)
  (unary-pyramid-op 'abs-value pyr ->))

(defmethod square ((pyr pyramid) &key ->)
  (unary-pyramid-op 'square pyr ->))

(defmethod square-root ((pyr pyramid) &key ->)
  (unary-pyramid-op 'square-root pyr ->))

(defmethod power ((pyr pyramid) (val number) &key ->)
  (unary-pyramid-op 'power pyr -> val))

(defmethod natural-logarithm ((pyr pyramid) &key (zero-val *div-by-zero-result*) ->)
  (unary-pyramid-op 'natural-logarithm pyr -> :zero-val zero-val))

(defmethod point-operation ((pyr pyramid) (func t) &key binsize ->) 
  (unary-pyramid-op 'point-operation pyr -> func :binsize binsize))

(defmethod periodic-point-operation ((pyr pyramid) (func t) period &key binsize ->)
  (unary-pyramid-op 'periodic-point-operation pyr -> func period :binsize binsize))

(defmethod round. ((pyr pyramid) &key (divisor 1.0) ->)
  (unary-pyramid-op 'round. pyr -> :divisor divisor))

(defmethod truncate. ((pyr pyramid) &key (divisor 1.0) ->)
  (unary-pyramid-op 'truncate. pyr -> :divisor divisor))

(defmethod floor. ((pyr pyramid) &key (divisor 1.0) ->)
  (unary-pyramid-op 'floor. pyr -> :divisor divisor))

(defmethod quantize ((pyr pyramid) &key ->
		     (binsize (/ (range pyr) (get-default 'discrete-function :size)))
		     (origin (mean pyr)))
  (unary-pyramid-op 'quantize pyr -> :origin origin :binsize binsize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; arithmetic ops: pyr,pyr

(defmethod add ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  (binary-pyramid-op 'add pyr1 pyr2 ->))

(defmethod sub ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  (binary-pyramid-op 'sub pyr1 pyr2 ->))

(defmethod mul ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  (binary-pyramid-op 'mul pyr1 pyr2 ->))

(defmethod div ((pyr1 pyramid) (pyr2 pyramid)
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (binary-pyramid-op 'div pyr1 pyr2 ->
		     :zero-val zero-val :suppress-warning suppress-warning))

(defmethod power ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  (binary-pyramid-op 'power pyr1 pyr2 ->))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; arithmetic ops: pyr,number

(defmethod add ((pyr pyramid) (const number) &key ->)
  (unary-pyramid-op 'add pyr -> const))

(defmethod add ((const number) (pyr pyramid) &key ->)
  (unary-pyramid-op 'add pyr -> const))

(defmethod sub ((pyr pyramid) (const number) &key ->)
  (unary-pyramid-op 'sub pyr -> const))

(defmethod sub ((const number) (pyr pyramid) &key ->)
  (unary-pyramid-reverse-op 'sub pyr -> const))

(defmethod mul ((pyr pyramid) (const number) &key ->)
  (unary-pyramid-op 'mul pyr -> const))

(defmethod mul ((const number) (pyr pyramid) &key ->)
  (unary-pyramid-op 'mul pyr -> const))

(defmethod div ((pyr pyramid) (const number)
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (unary-pyramid-op 'div pyr -> const
		    :zero-val zero-val :suppress-warning suppress-warning))

(defmethod div ((const number) (pyr pyramid)
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (unary-pyramid-reverse-op 'div pyr -> const
			    :zero-val zero-val :suppress-warning suppress-warning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geom ops (*** need crop, paste, side-by-side, subsample, downsample, upsample)

(defmethod circular-shift ((pyr pyramid) &key
			   (y-shift 0) (x-shift 0)
			   (y y-shift) (x x-shift) ->)
  (unary-pyramid-op 'circular-shift pyr -> :y y :x x
		    :y-shift y-shift :x-shift x-shift))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; comparison ops

(defmethod point-minimum ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  (binary-pyramid-op 'point-minimum pyr1 pyr2 ->))

(defmethod point-minimum ((pyr pyramid) (val number) &key ->)
  (unary-pyramid-op 'point-minimum pyr -> val))

(defmethod point-minimum ((val number) (pyr pyramid) &key ->)
  (unary-pyramid-op 'point-minimum pyr -> val))

(defmethod point-maximum ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  (binary-pyramid-op 'point-maximum pyr1 pyr2 ->))

(defmethod point-maximum ((pyr pyramid) (val number) &key ->)
  (unary-pyramid-op 'point-maximum pyr -> val))

(defmethod point-maximum ((val number) (pyr pyramid) &key ->)
  (unary-pyramid-op 'point-maximum pyr -> val))

(defmethod square-error ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  (binary-pyramid-op 'square-error pyr1 pyr2 ->))

(defmethod abs-error ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  (binary-pyramid-op 'abs-error pyr1 pyr2 ->))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; greater-than, etc.
;;; *** hairy to implement because result is made up of bit-images of
;;; different sizes.

#|
(defmethod equal-to ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  )

(defmethod equal-to ((pyr pyramid) (const number) &key ->)
  )

(defmethod equal-to ((const number) (pyr pyramid) &key ->)
  (equal-to pyr const :-> ->))

(defmethod greater-than ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  )

(defmethod greater-than ((pyr pyramid) (const number) &key ->)
  )

(defmethod greater-than ((const number) (pyr pyramid) &key ->)
  (less-than pyr const :-> ->))

(defmethod less-than ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  )

(defmethod less-than ((pyr pyramid) (const number) &key ->)
  )

(defmethod less-than ((const number) (pyr pyramid) &key ->)
  (greater-than pyr const :-> ->))

(defmethod greater-than-or-equal-to ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  )

(defmethod greater-than-or-equal-to ((pyr pyramid) (const number) &key ->)
  )

(defmethod greater-than-or-equal-to ((const number) (pyr pyramid) &key ->)
  (less-than-or-equal-to pyr const :-> ->))

(defmethod less-than-or-equal-to ((pyr1 pyramid) (pyr2 pyramid) &key ->)
  )

(defmethod less-than-or-equal-to ((pyr pyramid) (const number) &key ->)
  )

(defmethod less-than-or-equal-to ((const number) (pyr pyramid) &key ->)
  (greater-than-or-equal-to pyr const :-> ->))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ops that return scalars

;;; *** Need to write: minimum-location, maximum-location,
;;; mean-abs-error, max-abs-error

(defmethod minimum ((pyr pyramid))
  (loop for lev in (levels pyr)
	minimize (the single-float (minimum lev))))

(defmethod maximum ((pyr pyramid))
  (loop for lev in (levels pyr)
	maximize (the single-float (maximum lev))))

(defmethod mean ((pyr pyramid) &key ignore-zeros)
  (loop for lev in (levels pyr)
	summing (mean lev :ignore-zeros ignore-zeros) into total
	counting t into num
	finally (return (/ total num))))

;;; assumes that all of the sub-viewables are identical size
(defmethod mean-square-error ((pyr1 pyramid) (pyr2 pyramid))
  (loop for lev1 in (levels pyr1)
	for lev2 in (levels pyr2)
	for size = (apply #'* (dimensions lev1))
	summing (* size (mean-square-error lev1 lev2)) into total
	summing size into total-size
	finally (return (/ total total-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; filtering ops (*** need gauss-in, gauss-out)

;;; *** apply-filter and expand-filter busted when up/down-sampling

(defmethod apply-filter ((pyr pyramid) (filter filter)
			 &key -> (direction 0))
  (unary-pyramid-op 'apply-filter pyr -> filter
		    :direction direction))

(defmethod apply-filter ((filter filter) (pyr pyramid)
			 &key -> (direction 0))
  (unary-pyramid-op 'apply-filter pyr -> filter
		    :direction direction))

(defmethod expand-filter ((pyr pyramid) (filter filter)
			  &key -> (zero t) (direction 0))
  (unary-pyramid-op 'expand-filter pyr -> filter
		    :direction direction
		    :zero zero))

(defmethod expand-filter ((filter filter) (pyr pyramid)
			  &key -> (zero t) (direction 0))
  (unary-pyramid-op 'expand-filter pyr -> filter
		    :direction direction
		    :zero zero))

(defmethod blur ((pyr pyramid)
		 &key 
		 (level 1)
		 (kernel (mapcar #'(lambda (x) (* x (sqrt 2))) gauss-5))
		 (edge-handler :reflect1)
		 ->)
  (unary-pyramid-op 'blur pyr ->
		    :level level
		    :kernel kernel
		    :edge-handler edge-handler))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; QMF pyramids

;;; *** Make this a method on image and one-d-image ...
;;; lo-filt may be a list or a one-d-filter.
(defmethod make-separable-qmf-pyramid ((image image) &rest initargs &key
				       (lo-filt qmf-7)
				       (edge-handler (if (filter-p lo-filt)
							 (edge-handler lo-filt)
							 :reflect1))
				       &allow-other-keys)
  (remf initargs :lo-filt)
  (remf initargs :edge-handler)
  (setq lo-filt (if (filter-p lo-filt) lo-filt (make-filter lo-filt)))
  (let* ((hi-filt (shift-by-pi lo-filt))
	 (low-filt (make-separable-filter lo-filt lo-filt
					  :edge-handler edge-handler
					  :start-vector '(0 0)
					  :step-vector '(2 2)))
	 (diag-filt (make-separable-filter hi-filt hi-filt
					   :edge-handler edge-handler
					   :start-vector '(1 1)
					   :step-vector '(2 2)))
	 (hor-filt (make-separable-filter hi-filt lo-filt
					  :edge-handler edge-handler
					  :start-vector '(1 0)
					  :step-vector '(2 2)))
	 (vert-filt (make-separable-filter lo-filt hi-filt
					   :edge-handler edge-handler
					   :start-vector '(0 1)
					   :step-vector '(2 2)))
	 (filters (list low-filt hor-filt vert-filt diag-filt )))
    (apply 'make-qmf-pyramid image :filters filters initargs)))

(defmethod make-separable-qmf-pyramid ((image one-d-image) &rest initargs &key
				       (lo-filt qmf-7)
				       (edge-handler (if (filter-p lo-filt)
							 (edge-handler lo-filt)
							 :reflect1))
				       &allow-other-keys)
  (remf initargs :lo-filt)
  (remf initargs :edge-handler)
  (setq lo-filt (if (filter-p lo-filt) lo-filt (make-filter lo-filt)))
  (let* ((hi-filt (shift-by-pi lo-filt)))
    (setf (step-vector lo-filt) '(2)
	  (start-vector lo-filt) '(0)
	  (step-vector hi-filt) '(2)
	  (start-vector hi-filt) '(1))
    (apply 'make-qmf-pyramid image :filters (list lo-filt hi-filt) initargs)))

#| WRITE THESE:
(defmethod access-vert ((pyr qmf-pyramid) level &key ((:-> res)))
  (access-band pyr :level  :-> res))

(defmethod access-diag ((pyr separable-qmf-pyramid) level &key ((:-> res)))
  (access-by-recipe pyr (nconc (list-of-length (1- level) 0) (list 1)) :-> res))

(defmethod access-hor ((pyr separable-qmf-pyramid) level &key ((:-> res)))
  (access-by-recipe pyr (nconc (list-of-length (1- level) 0) (list 2)) :-> res))
|#

#|
;;; *** Put newer filter coeffs in here!
(defun default-hex-qmf-filter-coeffs ()
  (let (( a  0.6935061 )
	( b  0.2905438 )
	( c -0.0323747 )
	( e -0.0027388 )
	( d -0.0319443 )
	( f -0.0028679 )
	( z  0.0       ))
    (list  (list z z z e z f z f z e z z z )
	   (list z z f z c z d z c z f z z )
	   (list z f z d z b z b z d z f z )
	   (list e z c z b z a z b z c z e )
	   (list z f z d z b z b z d z f z )
	   (list z z f z c z d z c z f z z )
	   (list z z z e z f z f z e z z z ))))

(defun make-hex-qmf-pyramid (image &key 
				   (lo-filt (default-hex-qmf-filter-coeffs))
				   (level 0) 
				   name ->
				   (edge-handler (if (filter-p lo-filt)
						     (edge-handler lo-filt)
						     nil))
				   (display-type nil display-supplied-p))
  (when (one-d-image-p image) (error "Can not make hex pyramids on one-d-images"))
  (setq lo-filt (if (filter-p lo-filt) 
		    lo-filt
		    (make-hex-filter lo-filt)))
  (let* ((low (copy lo-filt))
	 (ldiag (hex-modulate (copy lo-filt) '(1 1) 4))
	 (rdiag (hex-modulate (copy lo-filt) '(1 -1) 4))
	 (vert  (hex-modulate (copy lo-filt) '(1 0) 2))
	 (forward-filters (list low ldiag rdiag vert))
	 (inverse-filters forward-filters))
    (setf (edge-handler low) edge-handler
	  (start-vector low) '(0 0)
	  (step-vector low) '(2 2)
	  (hex-start low) 0)
    (setf (edge-handler rdiag) edge-handler
	  (start-vector rdiag) '(0 0)
	  (step-vector rdiag) '(2 2)
	  (hex-start rdiag) 1)
    (setf (edge-handler ldiag) edge-handler
	  (start-vector ldiag) '(1 1)
	  (step-vector ldiag) '(2 2)
	  (hex-start ldiag) 0)
    (setf (edge-handler vert) edge-handler
	  (start-vector vert) '(1 1)
	  (step-vector vert) '(2 2)
	  (hex-start vert) 1)
    (with-result ((result ->)
		  `(:class hex-qmf-pyramid
		    ,@(when display-supplied-p (list :display-type display-type))
		    :forward-filters ,forward-filters
		    :inverse-filters ,inverse-filters
		    :image-tree ,(cons image (list nil nil nil nil)))
		  'make-hex-qmf-pyramid image lo-filt)
      (build result level)
      result)))

(defmethod access-ldiag ((pyr hex-qmf-pyramid) level &key ((:-> res)))
  (access-by-recipe pyr (nconc (list-of-length (1- level) 0) (list 1)) :-> res))

(defmethod access-rdiag ((pyr hex-qmf-pyramid) level &key ((:-> res)))
  (access-by-recipe pyr (nconc (list-of-length (1- level) 0) (list 2)) :-> res))
|#


#|
(make-separable-qmf-pyramid einstein 
			    '(-0.07610252183564592  0.3535533905932738  
			      0.8593118248578394 0.3535533905932738  
			      -0.07610252183564592) 
			    :-> 'pyr :level 1)
(build pyr 5)
(collapse pyr :-> 'reconstruct)
|#


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
