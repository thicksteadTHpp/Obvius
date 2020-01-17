;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: lisp-ops.lisp
;;;  Author: EJ Chichilnisky
;;;  Description: handle lists like other stuff (e.g., arrays) in Obvius
;;;  Creation Date: fall '91
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Methods to handle lists the way that OBVIUS handles other stuff,
;;; like arrays.  Operations (like add) that make new lists can use
;;; the :-> argument as usual.  Some operations (like maximum) only
;;; can handle lists of numbers.  Others (like sum-of) can handle
;;; lists of things.

(in-package obvius)
(export '(elements listify circular-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Conversions between lists and arrays

;;; Analogous to vectorize, take a nested list and string it out into a single list.
;;; Pronounced like, and related to, "mystify".
(defmethod listify ((tree list))
  (cond ((null tree) nil)
	((consp (car tree)) (append (listify (car tree)) (listify (cdr tree))))
	(t (cons (car tree) (listify (cdr tree))))))

(defmethod listify ((arr array))
  (coerce (vectorize arr) 'list))

(defmethod elements ((list list) &key &allow-other-keys)
  list)

(defmethod elements ((vector vector) &key ((:-> result) (make-list (length vector))))
  (dotimes (index (length vector))
    (setf (nth index result) (aref vector index)))
  result)

(defmethod elements ((array array) &key ((:-> result)))
  (unless result
    (setf result (make-list (y-dim array)))
    (dotimes (index (y-dim array))
      (setf (nth index result) (make-list (x-dim array)))))
  (unless (< (length (dimensions array)) 3)
    (error "This function only implemented for 1-d or 2-d arrays"))
  (dotimes (row (y-dim array))
    (dotimes (column (x-dim array))
      (setf (nth column (nth row result)) (aref array row column))))
  result)

;;; Like common lisp union but keeps them in order
;;(fmakunbound 'union)
(defun union (l1 l2 &key (test #'eql) test-not (key #'identity) preserve-order)
  (remove-duplicates (append l1 l2) :test test :test-not test-not :key key
		     :from-end preserve-order))

(defun circular-list (thing)
  (let ((res (list thing)))
    (setf (cdr res) res)
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fill up an array with other stuff.
(defmethod fill! ((vec vector) (list list))
  (unless (= (length list) (length vec))
    (error "Vector and list have different sizes"))
  (loop for index from 0 below (length vec)
	with elt-type = (array-element-type vec) do
	(setf (aref vec index) (coerce (nth index list) elt-type)))
  vec)

(defmethod fill! ((arr array) (list list))
  (apply 'check-size list)
  (unless (< (rank arr) 3)
    (error "Structured fill not implemented for this array of this dimension"))
  (cond ((numberp (first list))
	 (if (equal (dimensions list) (list (total-size arr)))
	     (fill! (vectorize arr) list)
	     (error "Array and list have imcompatible sizes")))
	((or (listp (first list)) (vectorp (first list)))
	 (loop for row in (rows arr)
	       for thing in list do
	       (fill! row thing)))
	(t (error "Array and list have imcompatible sizes")))
  arr)

(defmethod fill! ((list list) (arr array))
  (with-displaced-vectors ((vec arr))
    (dotimes (i (length vec))
      (setf (nth i list) (aref vec i)))
    list))

(defmethod fill! ((list cons) (item number))
  (dotimes (index (length list))
    (setf (nth index list) item))
  list)

(defmethod fill! ((list cons) (list2 cons))
  (dotimes (index (length list))
    (setf (nth index list) (nth index list2)))
  list)

(defmethod dimensions ((list cons))
  (cons (length list)
	(when (and (first list) (listp (first list)))
	  (dimensions (apply 'check-size list)))))

(defmethod rank ((list cons))
  (length (dimensions list)))

(defmethod total-size ((list cons))
  (apply '* (dimensions list)))

(defmethod check-size ((val number) &rest val-list)
  (if (notany 'null (mapcar 'numberp val-list))
      val
      (error "list ~a is not all numbers." val-list)))

(defmethod check-size ((list cons) &rest list-list)
  (cond ((null list-list) list)
	((not (equal (dimensions list) (dimensions (car list-list))))
	 (error "~As have different dimensions." (object-class-name list)))
	(t (apply 'check-size list-list))))

(defmethod y-dim ((list cons))
  (list-y-dim (dimensions list)))

(defmethod x-dim ((list cons))
  (list-x-dim (dimensions list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-destructive operations that only make sense for lists of numbers, and hence
;;; will only work with lists of numbers

(defmethod vector-length ((list cons))
  (vector-length (make-matrix list)))

(defmethod maximum ((list cons))
  (let ((max (first list))
	(pos 0))
    (loop for elt in (rest list)
	  for i from 1
	  do (when (> elt max)
	       (setq max elt
		     pos i)))
    (values max pos)))

(defmethod minimum ((list cons))
  (let ((min (first list))
	(pos 0))
    (loop for elt in (rest list)
	  for i from 1
	  do (when (< elt min)
	       (setq min elt
		     pos i)))
    (values min pos)))

(defmethod minimum-location ((vec cons))
  (multiple-value-bind (min-value min-location)
      (minimum vec)
    (declare (ignore min-value))
    min-location))

(defmethod maximum-location ((vec cons))
  (multiple-value-bind (max-value max-location)
      (maximum vec)
    (declare (ignore max-value))
    max-location))

(defmethod mean ((list cons) &key ignore-zeros)
  (when ignore-zeros
    (setq list (loop for item in list
		     unless (almost-equal item 0.0 :tolerance 0.0)
		     collect item)))
  (div (sum-of list) (length list)))

(defmethod variance ((list cons) &key ignore-zeros)
  (variance (make-matrix list) :ignore-zeros ignore-zeros))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Destructive operations that only make sense for lists of numbers.
(defmethod randomize ((list cons) (amount number) &key ((:-> result) (copy list)))
  (elements (randomize (make-matrix list) amount) :-> result))

(defmethod randomize ((list cons) (amount cons) &key ((:-> result) (copy list)))
  (elements (randomize (make-matrix list) (make-matrix amount)) :-> result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-destructive methods that require the relevant methods to be defined on elements of list
(defmethod almost-equal ((list-1 cons) (list-2 cons) &key (tolerance *tolerance*))
  (check-size list-1 list-2)
  (unless (loop for item-1 in list-1
		for item-2 in list-2
		unless (almost-equal item-1 item-2 :tolerance tolerance)
		return t)
    list-1))

(defmethod almost-equal ((list cons) (val t) &key (tolerance *tolerance*))
  (unless (loop for item in list
		unless (almost-equal item val :tolerance tolerance)
		return t)
    val))

(defmethod almost-equal ((val t) (list cons)  &key (tolerance *tolerance*))
  (unless (loop for item in list
		unless (almost-equal item val :tolerance tolerance)
		return t)
    val))

(defmethod scalar-multiple ((list-1 list) (list-2 list) &key (tolerance *tolerance*))
  (when (scalar-multiple (make-matrix list-1) (make-matrix list-2) :tolerance tolerance)
    list-1))

(defmethod sum-of ((list cons))
  (reduce #'add list))

(defmethod symmetric-p ((l cons) &rest args)
  (declare (ignore args))
  (every 'almost-equal l (reverse (nthcdr (floor (length l) 2) l))))

(defmethod anti-symmetric-p ((l cons) &rest args)
  (declare (ignore args))
  (every #'(lambda (a b) (almost-equal a (- b))) l (reverse (nthcdr (floor (length l) 2) l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on numbers that need to be defined so list methods will work
(defmethod square ((val number) &key ->)
  (declare (ignore ->))
  (sqr val))

(defmethod negate ((val number) &key ->)
  (declare (ignore ->))
  (- val))

(defmethod abs-value ((val number) &key ->)
  (declare (ignore ->))
  (abs val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Potentially destructive operations, that output into new lists
;;; Many of these will call methods on the indivudual elements of the lists
(defmethod similar ((list cons) &key (dimensions (length list))
		    ((:-> result) (make-list dimensions)))
  result)

(defmethod square-root ((number number) &key ->)
  (declare (ignore ->))
  (sqrt number))

(defmethod square-root ((list cons) &key ((:-> result) (copy list)))
  (loop for item in list
	for index from 0 do
	(setf (nth index result) (square-root item)))
  result)

(defmethod natural-logarithm ((number number) &key -> (zero-val *div-by-zero-result*))
  (declare (ignore ->))
  (if (zerop number) *div-by-zero-result* (log number)))

(defmethod natural-logarithm ((list cons) &key ((:-> result) (copy list))
			      (zero-val *div-by-zero-result*))
  (loop for item in list
	for index from 0 do
	(setf (nth index result) (natural-logarithm item :zero-val zero-val)))
  result)

(defmethod copy ((list cons) &key ((:-> result) (similar list)))
  (loop for item in list
	for index from 0 do
	(setf (nth index result) (if (or (symbolp item) (numberp item))
				     item
				     (copy item))))
  result)

(defmethod negate ((list cons) &key ((:-> result) (copy list)))
  (loop for item in list
	for index from 0 do
	(setf (nth index result) (negate item)))
  result)

(defmethod abs-value ((list cons) &key ((:-> result) (copy list)))
  (loop for item in list
	for index from 0 do
	(setf (nth index result) (abs-value item)))
  result)

(defmethod shuffle ((list list) &key ((:-> result)))
  (unless (eq list result)
    (setq result (copy-list list)))
  (loop with size = (length result)
	for i from 0 below size
	for j from size by -1
	for rand = (+ i (random j))
	for tmp = (elt result i)
	do
	(setf (elt result i) (elt result rand))
	(setf (elt result rand) tmp))
  result)

(defmethod normalize ((list cons) &key ((:-> result) (similar list)))
  (loop for index from 0 below (length list)
	with length = (vector-length list)
	do (setf (nth index result) (div (nth index list) length)))
  result)

(defmethod square ((list cons) &key ((:-> result) (similar list)))
  (loop for index from 0 below (length list)
	do (setf (nth index result) (square (nth index list))))
  result)

(defmethod add ((list cons) (val number) &key ((:-> result) (similar list)))
  (check-size list result)
  (dotimes (index (length result))
    (setf (nth index result) (add (nth index list) val)))
  result)

(defmethod add ((val number) (list cons) &key ((:-> result) (similar list)))
  (check-size list result)
  (dotimes (index (length result))
    (setf (nth index result) (add (nth index list) val)))
  result)

(defmethod add ((list1 cons) (list2 cons) &key ((:-> result) (similar list1)))
  (check-size list1 list2 result)
  (dotimes (index (length result))
    (setf (nth index result) (add (nth index list1) (nth index list2))))
  result)

(defmethod sub ((list cons) (val number) &key ((:-> result) (similar list)))
  (check-size list result)
  (dotimes (index (length result))
    (setf (nth index result) (sub (nth index list) val)))
  result)

(defmethod sub ((val number) (list cons) &key ((:-> result) (similar list)))
  (check-size list result)
  (dotimes (index (length result))
    (setf (nth index result) (sub val (nth index list))))
  result)

(defmethod sub ((list1 cons) (list2 cons) &key ((:-> result) (similar list1)))
  (check-size list1 list2 result)
  (dotimes (index (length result))
    (setf (nth index result) (sub (nth index list1) (nth index list2))))
  result)

(defmethod mul ((val number) (list cons) &key ((:-> result) (similar list)))
  (check-size list result)
  (dotimes (index (length result))
    (setf (nth index result) (mul (nth index list) val)))
  result)

(defmethod mul ((list cons) (val number) &key ((:-> result) (similar list)))
  (check-size list result)
  (dotimes (index (length result))
    (setf (nth index result) (mul (nth index list) val)))
  result)

(defmethod mul ((list1 cons) (list2 cons) &key ((:-> result) (similar list1)))
  (check-size list1 list2 result)
  (dotimes (index (length result))
    (setf (nth index result) (mul (nth index list1) (nth index list2))))
  result)

(defmethod div ((val number) (list cons) &key ((:-> result) (similar list))
		(zero-val *div-by-zero-result*) &allow-other-keys)
  (check-size list result)
  (dotimes (index (length result))
    (setf (nth index result) (div val (nth index list) :zero-val zero-val)))
  result)

(defmethod div ((list cons) (val number) &key ((:-> result) (similar list))
		(zero-val *div-by-zero-result*) &allow-other-keys)
  (check-size list result)
  (dotimes (index (length result))
    (setf (nth index result) (div (nth index list) val :zero-val zero-val)))
  result)

(defmethod div ((list1 cons) (list2 cons) &key ((:-> result) (similar list1))
		(zero-val *div-by-zero-result*)
		&allow-other-keys)
  (check-size list1 list2 result)
  (dotimes (index (length result))
    (setf (nth index result) (div (nth index list1) (nth index list2) :zero-val zero-val)))
  result)


;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
