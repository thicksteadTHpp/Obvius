;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: image-sequence.lisp
;;;  Author: heeger
;;;  Description: (temporal) sequences of images
;;;  Creation Date: 1/94
;;; ----------------------------------------------------------------
;;; This file is part of the Object-Oriented Picture System (OBVIUS),
;;; (C) Vision Science Group,  Media Laboratory,  
;;; Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obv)
(export '(image-sequence make-image-sequence	  
          image-sequence-p  image-list
          make-synthetic-image-sequence make-iterated-image make-slice))

(defmethod image-list ((seq image-sequence))
  (listify (data seq)))

(defmethod (setf image-list) ((im-list list) (seq image-sequence))
  (let ((old-im-list (image-list seq)))
    (if im-list
	(progn
	  (unless (and (listp im-list)
		       (every #'(lambda (x) (image-p x)) im-list))
	    (error "Bad im-list ~a: must be a list of images" im-list))
	  (apply 'check-size im-list)
	  (setf (data seq) (make-array (list 1 (length im-list))
				       :initial-contents (list im-list)))
	  (loop for im in im-list do (pushnew seq (superiors-of im))))
	(setf (data seq) nil))
    (mapc #'(lambda (inf) (notify-of-superior-destruction inf seq)) old-im-list)
    (dolist (pic (pictures-of seq)) (set-not-current pic)))
  seq)

(defmethod print-object ((seq image-sequence) stream)
  (format stream "#<~A " (object-class-name seq))
  (format stream "~S " (name seq))
  (format stream "(~A" (sequence-length seq))
  (format stream " ~A ~A)>" (car (dimensions seq)) (cadr (dimensions seq))))

(defmacro image-sequence-p (obj)
  `(typep ,obj 'image-sequence))

(defmethod rank ((seq image-sequence)) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make-Image-Sequence

;;; Im-list can be a list of images or a dim-list or nil.  If im-list
;;; is nil, must pass either (1) :length and :sub-viewable-spec or (2)
;;; :length and :dimensions.  The dim-list can be (z y x) or (y x).
;;; In the latter case, the :length keyword must also be provided.  If
;;; im-list is 2 integers interpret it as :dimensions.  If im-list is
;;; 3 integers, interpret it as :dimensions=(cdr im-list) and
;;; :length=(car im-list).
(defun make-image-sequence (im-list &rest initargs
			    &key sub-viewable-spec dimensions length display-type name ->)
  (declare (ignore  display-type name))
  (when -> (setf (getf initargs :name) ->))
  (cond ((null im-list))
	((every #'(lambda (x) (image-p x)) im-list))
	((every #'integerp im-list)
	 (cond ((= (length im-list) 2)
		(setf (getf initargs :dimensions) (setq dimensions im-list)))
	       ((= (length im-list) 3)
		(setf (getf initargs :dimensions) (setq dimensions (cdr im-list)))
		(setf (getf initargs :length) (setq length (car im-list))))
	       (t (error "Bad imlist ~a: must be a list of images or a dimension list" im-list)))
	 (setq im-list nil))
	(t (error "Bad imlist ~a: must be nil, a list of images, or a dimension list" im-list)))
  (unless im-list
    (setf (getf initargs :sub-viewable-spec)
	  (cond (sub-viewable-spec sub-viewable-spec)
		(dimensions (list :class 'image
				  :dimensions dimensions))
		(t (error "Must specify either :dimensions or :sub-viewable-spec")))))
  (with-result ((result nil)
		`(:class image-sequence
		  :viewable-list ,im-list
		  ,@initargs)
		'apply 'make-image-sequence im-list initargs)
    result))

(defmethod initialize-instance ((seq image-sequence) &rest initargs
				&key image-list &allow-other-keys)
  (when image-list
    (setf (getf initargs :viewable-list) image-list)
    (remf initargs :image-list))
  (apply #'call-next-method seq initargs))

;;; *** version 1.2 -> 2.0 transition function:
(defmacro with-result-image-sequence (&rest stuff)
  (declare (ignore stuff))
  (error "This macro is not provided in v2.0.  Use with-result instead!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make a sequence of images with pixel values computed from funct.  Funct
;;; should take arguments z, y, x and return a float.  Dims must
;;; either be a list of three numbers (z y x), or a list of two
;;; numbers with the sequence length specified with the :length
;;; keyword.
(defun make-synthetic-image-sequence (dims func
					   &rest initargs
					   &key
					   length
					   display-type name ->
					   (x-range '(-1.0 1.0))
					   (y-range x-range)
					   (z-range y-range))
  (declare (ignore length display-type name))
  (remf initargs :x-range) (remf initargs :y-range) (remf initargs :z-range)
  (when (= (length dims) 3)
    (setf (getf initargs :length) (car dims))
    (setf (getf initargs :dimensions) (cdr dims)))
  (with-result ((result ->) `(:class image-sequence :dimensions ,dims ,@initargs)
		'apply 'make-synthetic-image-sequence dims func initargs)
    (loop with z-step = (/ (apply #'- z-range) (- (sequence-length result)))
	  for res-im in (image-list result)
	  for i from 0 
	  for z-val = (car z-range) then (+ z-val z-step)
	  do
	  (status-message  "Make-synthetic-image-sequence: frame ~A ..." i)
	  (make-synthetic-image (dimensions res-im)
				#'(lambda (y x) (funcall func z-val y x))
				:x-range x-range :y-range y-range
				:-> res-im)
	  finally (status-message ""))
    result))

;;; Make a sequence of images, starting with im, by iteratively
;;; operating with func (func must be a function that performs an
;;; operation on the argument image).
(defun make-iterated-image (im func length &rest initargs
			       &key name display-type ->)
  (declare (ignore name display-type))
  (with-result ((result ->) `(:class image-sequence
			      :dimensions ,(dimensions im)
			      :length ,length ,@initargs)
		'apply 'make-iterated-image im func length initargs)
    (loop for res-im in (image-list result)
	  for i from 0 
	  for prev-im = (copy im :-> res-im) then (funcall func prev-im :-> res-im)
	  do (status-message "Make-iterated-image: frame ~A" i)
	  finally (status-message ""))
    result))

#|
;;; test

(make-iterated-image
 al '(lambda (x &key ->) (circular-shift x :y 10 :x 0 :-> ->)) 10)

(make-synthetic-image-sequence '(50 50) '(lambda (z y x) (float (* z y x))) :length 10)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; *** This function is not debugged ***
(defmethod make-slice ((seq image-sequence)
		       &key x-coord y-coord z-coord (x x-coord) (y y-coord) (z z-coord) name ->
		       (display-type nil display-type-specified-p))
  (declare (ignore name))
  (let* ((coord-list (list z y x)) dims)
    (when (not (<= 1 (count-if-not 'null coord-list) 2))
      (error "Make-slice on sequences requires one or two coordinate keywords."))
    (setq dims (loop for dim in (cons (length. seq) (dimensions seq))
		     for slicep in coord-list
		     when (not slicep)
		     collect dim))
    ;; Special cases
    (cond ((= (length dims) 1)		; 1D slice, but dims must have length 2
	   (if y 
	       (setq dims (cons 1 dims)) ;an x image
	       (setq dims (append dims '(1))))) ;a y image
	  ((and x (not y) (not z)) ;a y-z slice: z --> x
	   (setq dims (nreverse dims))))
    (with-result ((result ->)
		  `(:class image :dimensions ,dims
		    ,@(when display-type-specified-p (list :display-type display-type)))
		  'make-slice seq :x x :y y :z z)
      (cond (z			;easy case: a frame or slice of a frame
	     (crop (frame seq z) :y (or y 0) :x (or x 0)
		   :y-dim (if y 1 (y-dim seq))
		   :x-dim (if x 1 (x-dim seq))
		   :-> result))
	    ((and y x)	;1D slice in z direction
	     (loop for im in (viewable-list seq)
		   for i from 0 
		   do
		   (setf (iref result 0 i) (iref im y x))))
	    (t				;x-z or y-z slice
	     (with-local-viewables
		 ((temp (make-image (list (if y 1 (y-dim seq))
					  (if x 1 (x-dim seq))))))
	       (loop for im in (obvius::viewable-list seq)
		     for z from 0
		     do
		     (crop im :y (or y 0) :x (or x 0)
			   :y-dim (if y 1 (y-dim seq))
			   :x-dim (if x 1 (x-dim seq))
			   :-> temp)
		     (paste temp result :y (if y z 0) :x (if x z 0)
			    :-> result)))))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Most viewable sequence operations inherit from viewable-matrix.

;;; *** These not tested yet ***

;;; *** Crop, paste, side-by-side defined here because they are not
;;; yet defined for image-matrices.  Once they are written for
;;; image-matrices, get rid of these definitions.

;;; simply pastes together the viewables in the 2 sequences.
(defmethod paste ((seq1 image-sequence) (seq2 image-sequence) &rest keys
		  &key (offset '(0 0))
		  (y-offset (car offset)) (x-offset (cadr offset))
		  (y y-offset) (x x-offset) ->)
  (remf keys :->)
  (with-result ((result ->)
		(list :class (class-of seq1)
		      :dimensions (dimensions seq2)
		      :length (sequence-length seq2)
		      :display-type (display-type seq1))
		'apply 'paste seq1 seq2 keys)
    (loop for vbl1 in (viewable-list seq1)
	  for vbl2 in (viewable-list seq2)
	  for res in (viewable-list result)
	  do
	  (paste vbl1 vbl2 :y y :x x :-> res))
    result))

(defmethod side-by-side ((seq1 image-sequence) (seq2 image-sequence)
			 &key (space 0) ->)
  (declare (fixnum space))
  (with-result ((result ->)
		(list :class (class-of seq1)
		      :dimensions (list (max (y-dim seq1) (y-dim seq2))
					(+ space (x-dim seq1) (x-dim seq2)))
		      :length (sequence-length seq1)
		      :display-type (display-type seq1))
		'side-by-side seq1 seq2 :space space)
    (paste seq1 result :y 0 :x 0 :-> result)
    (paste seq2 result :y 0 :x (+ space (x-dim seq1)) :-> result)
    result))

;;; simply crops in x and y.  Use sub-sequence to crop in z.
(defmethod crop ((seq image-sequence) &rest keys &key
		 (offset '(0 0)) 
		 (y (car offset)) (x (cadr offset))
		 (y-size (- (y-dim seq) y)) (x-size (- (x-dim seq) x))
		 (y-dim y-size) (x-dim x-size)
		 ->)
  (with-result ((result ->)
		(list :class (class-of seq)
		      :dimensions (list y-dim x-dim)
		      :length (sequence-length seq)
		      :display-type (display-type seq))
		'apply 'crop seq keys)
    (loop for vbl in (viewable-list seq)
	  for res in (viewable-list result)
	  do
	  (crop vbl :y y :x x :x-dim x-dim :y-dim y-dim :-> res))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;; test make-image-sequence
(setq al (gauss-out einstein))

(make-instance 'image-sequence :viewable-list (list al al))
(make-image-sequence (list al al) :name 'foo :display-type 'pasteup)
(make-image-pair (list al al) :name 'bar)
(make-image-sequence '(10 10) :length 3)
(make-image-sequence nil :dimensions '(10 10) :length 3)
(make-image-sequence nil :length 3 :sub-viewable-spec (list :class 'image
							    :dimensions '(10 10)))

;;; test viewable-sequences
(set-default 'flipbook :independent-parameters nil)
(set-default 'pasteup :independent-parameters nil)

(display (setq vf-seq (make-viewable-sequence
	 (loop for i from 1 to 5
	       collect (*. i (make-image-pair
			      (list (make-ramp '(32 32))
				    (make-ramp '(32 32) :orientation (/ pi 2.0))))))))
	 'flipbook :sub-display-type 'vector-field)

(setq al-seq (make-image-sequence
	      (loop for i from 0 below 10
		    collect (circular-shift al :y (* i 5) :x 0))))

(setq bit-seq (>. al-seq 100.0))
	 
(setq pyr-seq (make-viewable-sequence
	       (loop for i from 0 below 10
		     collect (make-gaussian-pyramid
			      (circular-shift al :y (* i 5) :x 0) :level 3))))

(setq 1d-seq (make-viewable-sequence
	      (loop for i from 0 below 10
		    for col from 50
		    collect (make-slice al :x col))))
|#


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
