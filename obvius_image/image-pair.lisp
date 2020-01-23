;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: image-pair.lisp
;;;  Author: David Heeger
;;;  Description: image pair viewables
;;;  Creation Date: summer '88, 
;;;  Modified: to be a subclass of image-sequence, Oct '89
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obv)

(export '(image-pair-p complex-image-p polar-image-p
	  make-image-pair make-complex-image make-polar-image
	  x-component y-component left-image right-image
	  first-image second-image real-part imaginary-part switch-components
	  magnitude complex-phase  square-magnitude complex-conjugate
	  complex-to-polar  polar-to-complex
	  mul div print-values))

;;; IMAGE PAIR viewable

(defmacro image-pair-p (obj)
  `(typep ,obj 'image-pair))

(defmacro complex-image-p (obj)
  `(typep ,obj 'complex-image))

(defmacro polar-image-p (obj)
  `(typep ,obj 'polar-image))

(defmacro one-d-image-pair-p (obj)
  `(typep ,obj 'one-d-image-pair))

(defmacro one-d-complex-image-p (obj)
  `(typep ,obj 'one-d-complex-image))

(defmethod print-object ((pair image-pair) stream)
  (format stream "#<~A " (object-class-name pair))  
  (format stream "~S" (name pair))
  (format stream " ~A>" (dimensions pair)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; image-pairs inherit x-dim, y-dim, dimensions, inferiors-of,
;;; notify-of-infer-destruction from sequences

;;; Should this return the size of both images?
(defmethod total-size ((image-pair image-pair))
  (apply #'* (dimensions image-pair)))

(defmethod y-component ((im image-pair) &key ((:-> res)))
  (frame im 0 :-> res))

(defmethod x-component ((im image-pair) &key ((:-> res)))
  (frame im 1 :-> res))

(defmethod left-image ((im image-pair) &key ((:-> res)))
  (frame im 0 :-> res))

(defmethod right-image ((im image-pair) &key ((:-> res)))
  (frame im 1 :-> res))

(defmethod first-image ((im image-pair) &key ((:-> res)))
  (frame im 0 :-> res))

(defmethod second-image ((im image-pair) &key ((:-> res)))
  (frame im 1 :-> res))

(defmethod real-part ((im complex-image) &key ((:-> result)))
  (frame im 1 :-> result))

(defmethod imaginary-part ((im complex-image) &key ((:-> result)))
  (frame im 0 :-> result))

(defmethod magnitude ((im polar-image) &key ((:-> result)))
  (frame im 0 :-> result))

(defmethod complex-phase ((im polar-image) &key ((:-> result)))
  (frame im 1 :-> result))

(defmethod switch-components ((pair image-pair))
  (let ((tmp (aref (data pair) 0 0)))
    (setf (aref (data pair) 0 0) (aref (data pair) 0 1))
    (setf (aref (data pair) 0 1) tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Image-pair viewable

(defun make-image-pair (imlist-or-dims &rest initargs &key display-type name ->)
  (declare (ignore display-type name))
  (when -> (setf (getf initargs :name) ->))
  (unless (and (listp imlist-or-dims) (= (length imlist-or-dims) 2))
    (error "argument must be a list of two images or two dimensions (Y X)."))
  (with-result ((result nil)
		`(:class image-pair
		  ,@(if (every #'integerp imlist-or-dims)
			(list :dimensions imlist-or-dims)
			(list :viewable-list imlist-or-dims))
		  ,@initargs)
		'apply 'make-image-pair imlist-or-dims initargs)
    result))

;;; Mostly relies on inherted initialize-instance (defined for
;;; viewable-sequence and viewable-matrix).  Handle :size differently
;;; here.
(defmethod initialize-instance ((im image-pair) &rest initargs
				&key viewable-list length
				&allow-other-keys)
  (when viewable-list
    (unless (or (= (length viewable-list) 2)
		(every #'(lambda (x) (typep x 'image)) viewable-list))
      (error "Image-list ~a must be a list of 2 images" viewable-list)))
  (if length
      (unless (= length 2) (error "Length ~a of image-pair must be 2" length))
      (setf (getf initargs :length) 2))
  (apply #'call-next-method im initargs))

;;; COMPLEX IMAGE viewable
;;; im-list can be either a list of 2 images or it is a dim list
(defun make-complex-image (imlist-or-dims &rest initargs &key display-type name ->)
  (declare (ignore display-type name))
  (when -> (setf (getf initargs :name) ->))
  (let ((result (apply 'make-image-pair imlist-or-dims initargs)))
    (change-class result 'complex-image)))

;;; POLAR IMAGE viewable
;;; im-list can be either a list of 2 images or it is a dim list
(defun make-polar-image (imlist-or-dims &rest initargs &key display-type name ->)
  (declare (ignore display-type name))
  (when -> (setf (getf initargs :name) ->))
  (let ((result (apply 'make-image-pair imlist-or-dims initargs)))
    (change-class result 'polar-image)))

;;; **** version 1.2 -> 2.0 transition function:
(defmacro with-result-image-pair (&rest stuff)
  (declare (ignore stuff))
  (error "This macro is not provided in v2.0.  Use with-result instead!"))

;;; **** version 1.2 -> 2.0 transition function:
(defmacro with-result-complex-image (&rest stuff)
  (declare (ignore stuff))
  (error "This macro is not provided in v2.0.  Use with-result instead!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Most image-pair operations inherited from image-sequences

(defmethod print-values ((pair image-pair) &rest args)
  (format t "~%** y-component **")
  (apply #'print-values (cons (y-component pair) args))
  (format t "** x-component **")
  (apply #'print-values (cons (x-component pair) args)))

(defmethod make-slice ((pair image-pair) &rest initargs
		       &key x-coord y-coord (x x-coord) (y y-coord) name display-type ->
		       &allow-other-keys)
  (declare (ignore name display-type ->))
  (remf initargs :x)
  (remf initargs :y)
  (unless (getf initargs :display-type) (setf (getf initargs :display-type) 'flipbook))
  (let* ((y-component-slice (make-slice (y-component pair) :x x :y y))
	 (x-component-slice (make-slice (x-component pair) :x x :y y))
	 (result (apply 'make-image-pair (list y-component-slice x-component-slice)
			initargs)))
    (cond ((complex-image-p pair) (change-class result 'one-d-complex-image))
	  ((image-pair-p pair) (change-class result 'one-d-image-pair))
	  (t (error "Can't make slices of ~as" (class-of pair))))
    result))

;;;; Special complex-image operations (the rest are inherited from image-sequence).

(defmethod mul ((complex-im complex-image) (complex-num complex)
		&key ->)
  (with-result ((result ->) complex-im 'mul complex-im complex-num)
    (let* ((r-im (real-part complex-im))
	   (i-im (imaginary-part complex-im))
	   (r-num (realpart complex-num))
	   (i-num (imagpart complex-num)))
      (with-local-viewables ((tmp1 (similar r-im))
			     (tmp2 (similar r-im)))
	(sub (mul r-im r-num :-> tmp1) (mul i-im i-num :-> tmp2)
	    :-> (real-part result))
	(add (mul r-im i-num :-> tmp1) (mul i-im r-num :-> tmp2)
	    :-> (imaginary-part result)))
      result)))

(defmethod mul ((complex-num complex) (complex-im complex-image)
		&key ->)
  (mul complex-im complex-num :-> ->))

;;; Mul and div must be different than for image-pairs.
(defmethod mul ((im1 complex-image) (im2 complex-image) &key ->)
  (with-result ((result ->) im1 'mul im1 im2)
    (with-local-viewables
     ((tmp1 (similar (real-part im1)))
      (tmp2 (similar (real-part im1))))
     (sub (mul (real-part im1) (real-part im2) :-> tmp1)
	  (mul (imaginary-part im1) (imaginary-part im2) :-> tmp2)
	  :-> (real-part result))
     (add (mul (real-part im1) (imaginary-part im2) :-> tmp1)
	  (mul (imaginary-part im1) (real-part im2) :-> tmp2)
	  :-> (imaginary-part result))
     result)))

#|
;;; old version 3/29/94
(defmethod mul ((im1 complex-image) (im2 complex-image) &key ->)
  (with-result ((result ->) im1 'mul im1 im2)
    (sub (mul (real-part im1) (real-part im2) :-> (real-part result))
	 (mul (imaginary-part im1) (imaginary-part im2) :-> (imaginary-part result))
	 :-> (real-part result))
    (add (mul (real-part im1) (imaginary-part im2) :-> (imaginary-part result))
	 (mul (imaginary-part im1) (real-part im2)) ;temp image created here !!
	 :-> (imaginary-part result))
    result))
|#

(defmethod mul ((im1 complex-image) (im2 image) &key ->)
  (with-result ((result ->) im1 'mul im1 im2)
    (mul (real-part im1) im2 :-> (real-part result))
    (mul (imaginary-part im1) im2 :-> (imaginary-part result))
    result))

(defmethod mul ((im2 image) (im1 complex-image) &key ->)
  (mul im1 im2 :-> ->))

(defmethod div ((im1 complex-image) (im2 complex-image)
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) im1 'div im1 im2)
    (mul im1 (complex-conjugate im2) :-> result)
    (div result (square-magnitude im2) :zero-val zero-val :suppress-warning suppress-warning
	 :-> result)))

(defmethod div ((im1 complex-image) (im2 image)
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) im1 'div im1 im2)
    (div (real-part im1) im2 :zero-val zero-val :suppress-warning suppress-warning
	 :-> (real-part result))
    (div (imaginary-part im1) im2 :zero-val zero-val :suppress-warning suppress-warning
	 :-> (imaginary-part result))
    result)) 

(defmethod magnitude ((pair image-pair) &key ->)
  (with-result ((result ->) 
		(list :class 'image :dimensions (dimensions pair))
		'magnitude pair)
    (array-magnitude (data (x-component pair)) (data (y-component pair)) :-> (data result))
    result))

;;; As in Common Lisp, make this compute the magnitude
(defmethod abs-value ((pair complex-image) &key ->)
  (with-result ((result ->)
		(list :class 'image :dimensions (dimensions pair))
		'magnitude pair)
    (array-magnitude (data (x-component pair)) (data (y-component pair)) :-> (data result))
    result))

(defmethod square-magnitude ((pair image-pair) &key ->)
  (with-result ((result ->)
		(list :class 'image :dimensions (dimensions pair))
		'square-magnitude pair)
    (array-square-magnitude (data (x-component pair)) (data (y-component pair)) :-> (data result))
    result))

(defmethod complex-phase ((pair image-pair) &key ->)
  (with-result ((result ->)
		(list :class 'image :dimensions (dimensions pair))
		'complex-phase pair)
    (array-complex-phase (data (x-component pair)) (data (y-component pair)) :-> (data result))
    result))

(defmethod complex-conjugate ((im complex-image) &key ->)
  (with-result ((result ->) im 'complex-conjugate im)
    (unless (eq (real-part im) (real-part result))
      (copy (real-part im) :-> (real-part result)))
    (negate (imaginary-part im) :-> (imaginary-part result))
    result))

(defmethod complex-to-polar ((im complex-image) &key ->)
  (with-result ((result ->)
		(list :class 'polar-image :dimensions (dimensions im))
		'complex-to-polar im)
    (magnitude im :-> (magnitude result))
    (complex-phase im :-> (complex-phase result))
    result))

(defmethod polar-to-complex ((im polar-image) &key ->)
  (with-result ((result ->)
		(list :class 'complex-image :dimensions (dimensions im))
		'complex-to-polar im)
    (let ((phase (complex-phase im))
	  (mag (magnitude im)))
      (periodic-point-operation phase #'cos 2-pi
				:binsize (/ 2-pi (get-default 'discrete-function :size))
				:-> (real-part result))
      (periodic-point-operation phase #'sin 2-pi
				:binsize (/ 2-pi (get-default 'discrete-function :size))
				:-> (imaginary-part result))
      (mul (real-part result) mag :-> (real-part result))
      (mul (imaginary-part result) mag :-> (imaginary-part result)))
    result))

;;; Arithmetic functions for polar images
;;; *** Most of these are not implemented yet

(defmethod add ((im1 polar-image) (im2 polar-image) &key ->)
  (declare (ignore ->))
  (error "Add not implemented for polar-images"))

(defmethod add ((im1 polar-image) (im2 image) &key ->)
  (declare (ignore ->))
  (error "Add not implemented for polar-images"))

(defmethod add ((im1 image) (im2 polar-image) &key ->)
  (declare (ignore ->))
  (error "Add not implemented for polar-images"))

(defmethod add ((im1 polar-image) (const number) &key ->)
  (declare (ignore ->))
  (error "Add not implemented for polar-images"))

(defmethod add ((const number) (im1 polar-image) &key ->)
  (declare (ignore ->))
  (error "Add not implemented for polar-images"))

(defmethod sub ((im1 polar-image) (im2 polar-image) &key ->)
  (declare (ignore ->))
  (error "Sub not implemented for polar-images"))

(defmethod sub ((im1 polar-image) (im2 image) &key ->)
  (declare (ignore ->))
  (error "Sub not implemented for polar-images"))

(defmethod sub ((im1 image) (im2 polar-image) &key ->)
  (declare (ignore ->))
  (error "Sub not implemented for polar-images"))

(defmethod sub ((im1 polar-image) (const number) &key ->)
  (declare (ignore ->))
  (error "Sub not implemented for polar-images"))

(defmethod sub ((const number) (im1 polar-image) &key ->)
  (declare (ignore ->))
  (error "Sub not implemented for polar-images"))

(defmethod mul ((im1 polar-image) (im2 polar-image) &key ->)
  (declare (ignore ->))
  (error "Mul not implemented for polar-images"))

(defmethod mul ((im1 polar-image) (im2 image) &key ->)
  (with-result ((result ->) im1 'mul im1 im2)
    (mul (magnitude im1) im2 :-> (magnitude result))
    (copy (complex-phase im1) :-> (complex-phase result))
    result))

(defmethod mul ((im2 image) (im1 polar-image) &key ->)
  (mul im1 im2 :-> ->))

(defmethod div ((im1 polar-image) (im2 polar-image) &key -> &allow-other-keys)
  (declare (ignore ->))
  (error "Div not implemented for polar-images"))

(defmethod div ((im1 polar-image) (im2 image)
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) im1 'div im1 im2)
    (div (magnitude im1) im2 :zero-val zero-val :suppress-warning suppress-warning
	 :-> (magnitude result))
    (copy (complex-phase im1) :-> (complex-phase result))
    result))


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
