;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: bit-image.lisp
;;;  Author: David Heeger
;;;  Description: one-bit image viewables
;;;  Creation Date: summer '88
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obv)

(export '(bit-image-p x-dim y-dim iref
	  make-bit-image
	  invert mul add or. and. not. if.
	  copy similar crop paste
	  less-than greater-than equal-to 
	  greater-than-or-equal-to less-than-or-equal-to
	  >. <. =. >=. <=.
	  zero-crossings
	  image-from-bit-image coerce-to-float))

(defmacro bit-image-p (obj)
  `(typep ,obj 'bit-image))

#+Lucid
(defmethod (setf iref) ((val number) (image bit-image) &rest args)
  (apply #'lucid-runtime-support::set-aref
	 (if (zerop val) 0 1)
	 (data image) args)
  (set-not-current image)
  val)

#|
(defmethod x-dim ((image bit-image))
  (list-x-dim (dimensions image)))

(defmethod y-dim ((image bit-image))
  (list-y-dim (dimensions image)))
|#

(defmethod element-type ((im bit-image))
  'bit)

(defun make-bit-image (dims &rest initargs &key data display-type name ->)
  (declare (ignore data display-type name))
  (when -> (setf (getf initargs :name) ->))
  (with-result ((result nil)
		`(:class bit-image :dimensions ,dims ,@initargs)
		'make-bit-image dims)
    result))

;;; NOTE: other set-result methods inherit from images. See image.lisp.
(defmethod set-result ((name t) (model bit-image))
  (check-type name viewable-name)
  (make-instance (class-of model)
		 :data (allocate-array (dimensions model) :element-type 'bit)
		 :name name
		 :display-type (display-type model)))

;;; **** version 1.2 -> 2.0 transition function:
(defmacro with-result-bit-image (&rest stuff)
  (declare (ignore stuff))
  (error "This macro is not provided in v2.0.  Use with-result instead!"))

(defmethod static-arrays-of ((img bit-image))
  (list (data img)))

;;; Print method for bit-image objects
(defmethod print-object ((im bit-image) stream)
  (format stream "#<~A " (object-class-name im))  
  (format stream "~S" (name im))
  (format stream " ~A>" (dimensions im)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; threshold operations  *** THESE SHOULD BE REWRITTEN IN C!!

;;; Define three macros for making methods to do comparison operations:
;;; (1) compare two images, (2) compare image with a number, and
;;; (3) compare a number with an image.  
;;; Result is always a bit-image.

;;; change if to cond, error if not either 'bit-image or 'image?
(defmacro def-compare-im-im (method-name func)
  `(defmethod ,method-name ((im1 image) (im2 image) 
			    &key ((:-> res)))
    (with-result ((result res) 
		  (list :class 'bit-image
			:dimensions (dimensions (check-size im1 im2)))
		  ',method-name im1 im2)
      (loop-over-image-pixels ((val1 im1) (val2 im2) (rval result bit))
	(if (,func val1 val2) (setf rval 1) (setf rval 0)))
      result)))

(defmacro def-compare-im-val (method-name func)
  `(defmethod ,method-name ((image image) (val number) 
			    &key ((:-> res)))
    (with-result ((result res)
		  (list :class 'bit-image
			:dimensions (dimensions image))
		  ',method-name image val)
      (loop-over-image-pixels ((ival image) (rval result bit))
	(if (,func ival val) (setf rval 1) (setf rval 0)))
      result)))

(defmacro def-compare-val-im (method-name func)
  `(defmethod ,method-name ((val number) (image image) 
			    &key ((:-> res)))
    (with-result ((result res)
		  (list :class 'bit-image
			:dimensions (dimensions image))
		  ',method-name val image)
      (loop-over-image-pixels ((ival image) (rval result bit))
	(if (,func val ival) (setf rval 1) (setf rval 0)))
      result)))

;;; (greater-than thing1 thing2 &key ->)
;;; where thing1 an thing2 can both be images or either one may be a number.
;;; methods automatically generated at compile time using def-compare-* macros.
(def-compare-im-im greater-than >)
(def-compare-im-val greater-than >)
(def-compare-val-im greater-than >)

;;; (less-than thing1 thing2 &key ->)
;;; where thing1 an thing2 can both be images or either one may be a number.
;;; methods automatically generated at compile time using def-compare-* macros.
(defmethod less-than ((im1 image) (im2 image) &key ->)
  (greater-than-or-equal-to im2 im1 :-> ->))
(defmethod less-than ((im1 image) (val number) &key ->)
  (greater-than-or-equal-to val im1 :-> ->))
(defmethod less-than ((val number) (im2 image) &key ->)
  (greater-than-or-equal-to im2 val :-> ->))

;;; (equal-to thing1 thing2 &key ->)
;;; where thing1 an thing2 can both be images or either one may be a number.
;;; methods automatically generated at compile time using def-compare-* macros.
(def-compare-im-im equal-to =)
(def-compare-im-val equal-to =)
(def-compare-val-im equal-to =)

;;; (greater-than-or-equal-to thing1 thing2 &key ->)
;;; where thing1 an thing2 can both be images or either one may be a number.
;;; methods automatically generated at compile time using def-compare-* macros.
(def-compare-im-im greater-than-or-equal-to >=)
(def-compare-im-val greater-than-or-equal-to >=)
(def-compare-val-im greater-than-or-equal-to >=)

;;; (less-than-or-equal-to thing1 thing2 &key ->)
;;; where thing1 an thing2 can both be images or either one may be a number.
;;; methods automatically generated at compile time using def-compare-* macros.
(defmethod less-than-or-equal-to ((im1 image) (im2 image) &key ->)
  (greater-than im2 im1 :-> ->))
(defmethod less-than-or-equal-to ((im1 image) (val number) &key ->)
  (greater-than val im1 :-> ->))
(defmethod less-than-or-equal-to ((val number) (im2 image) &key ->)
  (greater-than im2 val :-> ->))

(defmacro >. (thing1 thing2 &key ((:-> res)))
  `(greater-than ,thing1 ,thing2 :-> ,res))

(defmacro <. (thing1 thing2 &key ((:-> res)))
  `(less-than ,thing1 ,thing2 :-> ,res))

(defmacro =. (thing1 thing2 &key ((:-> res)))
  `(equal-to ,thing1 ,thing2 :-> ,res))

(defmacro >=. (thing1 thing2 &key ((:-> res)))
  `(greater-than-or-equal-to ,thing1 ,thing2 :-> ,res))

(defmacro <=. (thing1 thing2 &key ((:-> res)))
  `(less-than-or-equal-to ,thing1 ,thing2 :-> ,res))

;;; Compute a bit image containing the zero-crossings of "im".
;;; This one is reasonably fast on machines with fast floating point.
(defmethod zero-crossings ((im image) &key ->)
  (with-result ((result ->)
 		(list :class 'bit-image
		      :dimensions (dimensions im))
		'zero-crossings im)
    (zero! result)
    (with-local-viewables ((shift-x (circular-shift im :y 0 :x 1))
			   (shift-y (circular-shift im :y 1 :x 0))
			   (prod-x (mul im shift-x))
			   (prod-y (mul im shift-y)))
      (loop-over-image-pixels ((valx prod-x) (valy prod-y) (val-res result bit))
	(when (or (minusp valx) (minusp valy))
	  (setf val-res 1))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; bit-image operations and joint operations on bit and float images
;;; need to write crop, paste

(defmacro or. (im1 im2 &key ((:-> res)))
  `(add ,im1 ,im2 :-> ,res))

(defmacro and. (im1 im2 &key ((:-> res)))
  `(mul ,im1 ,im2 :-> ,res))

(defmacro not. (im &key ((:-> res)))
  `(invert ,im :-> ,res))

(defmacro if. (mask body1 body2 &key ->)
  (let ((m (gensym))  (im (gensym)))
    `(let* ((,m ,mask)
	    (,im (invert ,m)))
      (unwind-protect
	   (add (mul ,m ,body1) (mul ,im ,body2) :-> ,->)
	(destroy ,im)))))

(defmethod invert ((image bit-image) &key ->)
  (with-result ((result ->) image 'invert image)
    (bit-not (data image) (data result))
    result))

(defmethod mul ((im1 bit-image) (im2 bit-image) &key ->)
  (with-result ((result ->) im1 'mul im1 im2)
    (bit-and (data im1) (data im2) (data result))
    result))

(defmethod add ((im1 bit-image) (im2 bit-image) &key ->)
  (with-result ((result ->) im1 'add im1 im2)
    (bit-ior (data im1) (data im2) (data result))
    result))

(defmethod mul ((bimage bit-image) (image image) &key ->)
  (with-result ((result ->) image 'mul bimage image)
    (loop-over-image-pixels ((bval bimage bit) (ival image) (rval result))
      (setf rval (if (plusp bval) ival 0.0)))
    result))

(defmethod mul ((image image) (bimage bit-image) &key ((:-> res)))
  (mul bimage image :-> res))

(defmethod correlate ((im1 bit-image) (im2 bit-image) &key
		      (x 0) (y 0) (x-dim (x-dim im1)) (y-dim (y-dim im1)) ->)
  (check-size im1 im2)
  (with-result ((result ->) (list :class 'image :dimensions (list y-dim x-dim))
		'correlate im1 im2)
    (correlate (data im1) (data im2) :x x :y y :x-dim x-dim :y-dim y-dim :-> (data result))
    result))

#|
(display (setf foo (make-random-dots '(5 5))) t :zoom 20)
(display (correlate foo foo) 'surface-plot)
(display (correlate foo foo :x -2 :y -2 :x-dim 5 :y-dim 5) 'surface-plot)
|#

(defmethod coerce-to-float ((im bit-image) &key ->)
  (with-result ((result ->) (list :class 'image
				  :dimensions (dimensions im))
		'image-from-bit-image im)
    (coerce-to-float (data im) :-> (data result))
    result))

(defmethod image-from-bit-image ((im bit-image) &key ->)
  (coerce-to-float im :-> ->))


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:

