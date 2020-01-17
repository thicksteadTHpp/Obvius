;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: generic-ops
;;;  Author: EJ Chichilnisky (blame him)
;;;  Description: generic versions of all ops, calling recursively on the data slots
;;;  Creation Date: 5/92
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  :obvius)
(export '())

;;; *** Isn't this over-generifying the methods??!!  Too much CLOS!

(defmethod data ((viewable viewable))
  (error (concatenate 'string "Sorry, there is no data slot defined on viewable type ~a. "
		      "This error probably occurred because you tried to perform some operation "
		      "that is not yet defined for viewables of type ~a.")
	 (class-of viewable) (class-of viewable)))

(defmethod point-operation ((viewable viewable) (func t) &key binsize ->)
  (with-result ((result ->) viewable 'point-operation viewable func :binsize binsize) 
    (point-operation (data viewable) func :binsize binsize :-> (data result))
    result))

(defmethod point-operation ((func function) thing &rest keys)
  (apply 'point-operation thing func keys))

(defmethod point-operation ((func discrete-function) thing &rest keys)
  (apply 'point-operation thing func keys))

(defmethod add ((viewable1 viewable) (viewable2 viewable) &key ->)
  (with-result ((result ->) (check-size viewable1 viewable2) 'add viewable1 viewable2)
    (add (data viewable1) (data viewable2) :-> (data result))
    result))

(defmethod sub ((viewable1 viewable) (viewable2 viewable) &key ->)
  (with-result ((result ->) (check-size viewable1 viewable2) 'sub viewable1 viewable2)
    (sub (data viewable1) (data viewable2) :-> (data result))
    result))

(defmethod mul ((viewable1 viewable) (viewable2 viewable) &key ->)
  (with-result ((result ->) (check-size viewable1 viewable2) 'mul viewable1 viewable2)
    (mul (data viewable1) (data viewable2) :-> (data result))
    result))

(defmethod div ((viewable1 viewable) (viewable2 viewable) &key ->
		(zero-val *div-by-zero-result*) suppress-warning)
  (with-result ((result ->) (check-size viewable1 viewable2) 'div viewable1 viewable2)
    (div (data viewable1) (data viewable2) :zero-val zero-val
	 :suppress-warning suppress-warning :-> (data result))
    result))

(defmethod normalize ((vbl viewable) &key ->)
  (with-result ((result ->) vbl 'normalize vbl)
    (normalize (data vbl) :-> (data result))
    result))

(defmethod add ((viewable viewable) (val number) &key ->)
  (with-result ((result ->) viewable 'add viewable val)
    (add (data viewable) val :-> (data result))
    result))

(defmethod add ((val number) (viewable viewable) &key ->)
  (with-result ((result ->) viewable 'add val viewable)
    (add (data viewable) val :-> (data result))
    result))

(defmethod sub ((val number) (viewable viewable) &key ->)
  (with-result ((result ->) viewable 'sub val viewable)
    (sub val (data viewable) :-> (data result))
    result))

(defmethod sub ((viewable viewable) (val number) &key ->)
  (with-result ((result ->) viewable 'sub viewable val)
    (add (data viewable) (- val) :-> (data result))
    result))

(defmethod mul ((viewable viewable) (val number) &key ->)
  (with-result ((result ->) viewable 'mul viewable val) 
    (mul (data viewable) val :-> (data result))
    result))

(defmethod mul ((val number) (viewable viewable) &key ->)
  (with-result ((result ->) viewable 'mul val viewable)
    (mul (data viewable) val :-> (data result))
    result))


(defmethod div ((val number) (viewable viewable) 
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) viewable 'div val viewable :zero-val zero-val)
    (div val (data viewable) :zero-val zero-val :suppress-warning suppress-warning
	 :-> (data result))
    result))

(defmethod div ((viewable viewable) (val number)
		&key (zero-val *div-by-zero-result*) suppress-warning ->)
  (with-result ((result ->) viewable 'div viewable val :zero-val zero-val)
    (div (data viewable) val :zero-val zero-val :suppress-warning suppress-warning
	 :-> (data result))
    result))


(defmethod maximum ((viewable viewable))
  (maximum (data viewable)))

(defmethod mean ((viewable viewable) &key ignore-zeros )
  (mean (data viewable) :ignore-zeros ignore-zeros))

(defmethod minimum ((viewable viewable))
  (minimum (data viewable)))

(defmethod gamma-correct ((viewable viewable) (gamma number)
			  &key (below (minimum viewable))
			  (above (maximum viewable))
			  (binsize (/ (- above below) (get-default 'discrete-function :size)))
			   ->)
  (with-result ((result ->) viewable 'gamma-correct gamma
		:below below :above above :binsize binsize)
    (gamma-correct (data viewable) gamma :below below :above above :binsize binsize
		   :-> (data result))
    result))

(defmethod clip ( (viewable viewable) (below number) (above number) &key ->)
  (with-result ((result ->) viewable 'clip viewable below above)
    (clip (data viewable) below above :-> (data result))
    result))

(defmethod abs-value ((viewable viewable)  &key ->)
  (with-result ((result ->) viewable 'abs-value viewable)
    (abs-value (data viewable) :-> (data result))
    result))

(defmethod fill! ((viewable viewable) (val number))
  (with-result ((res viewable) viewable 'fill! val)
    (fill! (data res) val)
    res))

(defmethod fill! ((viewable viewable) (list list))
  (with-result ((res viewable) viewable 'fill! list)
    (fill! (data res) list)
    res))

(defmethod fill! ((viewable viewable) (array array))
  (with-result ((res viewable) viewable 'fill! array)
    (fill! (data res) array)
    res))

(defmethod zero! ((viewable viewable))
  (fill! viewable 0.0))

(defmethod total-size ((viewable viewable))
  (total-size (data viewable)))

(defmethod dimensions ((viewable viewable))
  (dimensions (data viewable)))

(defmethod print-values ((viewable viewable)
			 &key
			 (y (* (car *y-print-range*) (y-dim viewable)))
			 (x (* (car *x-print-range*) (x-dim viewable)))
			 (y-size (- (* (cadr *y-print-range*) (y-dim viewable)) y))
			 (x-size (- (* (cadr *x-print-range*) (x-dim viewable)) x)))
  (print-values (data viewable) :y y :x x :y-size y-size :x-size x-size))

(defmethod negate ((viewable viewable) &key ->)
  (with-result ((result ->) viewable 'negate viewable)
    (negate (data viewable) :-> (data result))
    result))

(defmethod mean-square-error ((viewable1 viewable) (viewable2 viewable))
  (check-size viewable1 viewable2)
  (mean-square-error (data viewable1) (data viewable2)))

(defmethod square ((viewable viewable) &key ->)
  (with-result ((result ->) viewable 'square viewable)
    (square (data viewable) :-> (data result))
    result))

(defmethod square-root ((viewable viewable) &key ->)
  (with-result ((result ->) viewable 'square-root viewable)
    (square-root (data viewable) :-> (data result))
    result))

(defmethod half-square ((viewable viewable)  &key ->)
  (with-result ((result ->) viewable 'half-square viewable)
    (half-square (data viewable) :-> (data result))
    result))

(defmethod sum-of ((viewable viewable))
  (sum-of (data viewable)))

(defmethod elements ((viewable viewable) &key ((:-> result) (make-list (length (data viewable)))))
  (elements (data viewable) :-> result))

(defmethod dot-product ((viewable1 viewable) (viewable2 viewable) &key ->)
  (declare (ignore ->))
  (dot-product (data viewable1) (data viewable2)))

(defmethod vector-length ((viewable viewable))
  (vector-length (data viewable)))

(defmethod randomize ((viewable viewable) (number number) &key ->)
  (with-result ((result ->) viewable 'randomize viewable)
    (randomize (data viewable) number :-> (data result))
    result))

(defmethod randomize ((viewable1 viewable) (viewable2 viewable) &key ->)
  (with-result ((result ->) viewable1 'randomize viewable1)
    (randomize (data viewable1) (data viewable2) :-> (data result))
    result))

(defmethod almost-equal ((viewable viewable) (val number) &key (tolerance *tolerance*))
  (when (almost-equal (data viewable) val :tolerance tolerance)
    viewable))

(defmethod almost-equal ((val number) (viewable viewable) &key (tolerance *tolerance*))
  (when (almost-equal (data viewable) val :tolerance tolerance)
    val))

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
