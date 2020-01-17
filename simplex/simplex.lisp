;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: simplex.lisp
;;;  Author: Eero Simoncelli
;;;  Description: Multidimensional function minimization routine.
;;;  Creation Date:
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Routines to find the minimum of a multidimensional function, given
;;; a starting simplex (a set of N+1 vertices) in the N-dimensional
;;; space.  The routine should be run several times to insure proper
;;; convergence.  Adapted from "Numerical Recipes in C", pp. 307-309.

(in-package :obvius)
(export '(with-simplex run-simplex))

#|
;;; EXAMPLE of usage:

(with-simplex (simplex (make-array 3 :element-type 'single-float
				    :initial-contents '(0.0 0.0 0.0))
			1.0)
  (run-simplex simplex
	       #'(lambda (vec)
		   (+ (sqr (- (aref vec 0) 4.0))
		      (sqr (- (aref vec 1) 2.0))
		      (sqr (- (aref vec 2) 1.0))))
	       :print-frequency 20))
|#

;;; This macro creates an initial simplex given a starting point
;;; (vector) and a noise level, delta.  The initial simplex vertices
;;; consist of a copy of the original start-point vector, and that
;;; vector with noise added to each element.  Delta can be a list or a
;;; number.  It is used to determine the standard deviation of noise
;;; used to set up the initial simplex.
(defmacro with-simplex ((simplex-name start-point delta &optional (minval 1.0)) . body)
  (let ((start-vect (gensym))
	(i (gensym))
	(new-vect (gensym))
	(pos (gensym))
	(variance (gensym)))
    `(let* ((,start-vect 
	     (if (listp ,start-point)
		 (make-array (length ,start-point) 
			     :element-type 'single-float
			     :initial-contents ,start-point)
		 (copy ,start-point :-> 
		       (make-array (length ,start-point) 
				   :element-type 'single-float))))
	    (,simplex-name 
	     (cons ,start-vect
		   (loop for ,i from 0 below (length ,start-vect)
			 for ,new-vect = (copy ,start-vect)
			 do (loop for ,pos from 0 below (length ,new-vect)
				  for ,variance = (sqr (if (numberp ,delta)
							   (* ,delta 
							      (max (abs (aref ,new-vect ,pos))
								   ,minval))
							   (nth ,pos ,delta)))
				  do (incf (aref ,new-vect ,pos) (gaussian-noise 0.0 ,variance)))
			 collect ,new-vect))))
      ,@body)))

#|
;;; Old version creates simplex points by moving in direction of each
;;; of the standard basis vectors (instead of noise).
(defmacro with-simplex ((simplex-name start-point delta &optional (minval 1.0)) . body)
  (let ((start-vect (gensym)))
    `(let* ((,start-vect (if (listp ,start-point)
			     (make-array (length ,start-point) 
					 :element-type 'single-float
					 :initial-contents ,start-point)
			     (copy ,start-point :-> 
				   (make-array (length ,start-point) 
					       :element-type 'single-float))))
	    (,simplex-name (cons ,start-vect
				 (loop for i from 0 below (length ,start-vect)
				       for new-vect = (copy ,start-vect)
				       do (incf (aref new-vect i)
						(* ,delta 
						   (if (> (abs (aref new-vect i))
							  (abs ,minval))
						       (aref new-vect i)
						       ,minval)))
				       collect new-vect))))
      ,@body)))
|#

(defvar *alpha* -1.0)			
(defvar *beta* 0.5)			;see Numerical recipes in C.
(defvar *gamma* 2.0)

;;; Find minimum of the error func using downhill simplex method,
;;; starting with the given simplex.  Simplex is a list or vector of
;;; FLOAT vectors, The list length must be one more than the length of
;;; the individual vectors.  Each vector corresponds to a point in
;;; the parameter space (domain) of the function.  The minimal point
;;; (vector) is returned.  The func should return values greater than
;;; 0.0.

;;; print-frequency:
;;; print after this many iterations.  t => print contraction dots,
;;; and error at termination.  nil => don't print.

;;; tolerance:
;;; routine stops when diff between min and max function values on
;;; vertices reaches tolerance level, relative to the mean of their
;;; values.

(defun run-simplex (simplex func &key
			    (print-frequency 20)
			    (print-function 'standard-simplex-print-function)
			    (max-function-calls 2000)
			    (tolerance *tolerance*))
  (let* ((dim (length (elt simplex 0)))	;dimensionality of space
	 (y (make-array (1+ dim)	;function values at vertices
			:element-type 'single-float))	
	 (simplex-sum			;sum of vertices
	  (make-array dim :element-type 'single-float))
	 (num-calls 0)			;number of calls to func
	 (count 0)			;number of iterations
	 (temp-vertex (make-array dim :element-type 'single-float))
 	 temp-val new-val min-vtx max2-vtx max-vtx)
    (declare (special num-calls) 
	     (type (simple-array single-float (*)) y simplex-sum temp-vertex) 
	     (float new-val)
	     (fixnum min-vtx max-vtx max2-vtx count num-calls))
    (when (/= (length simplex) (1+ dim ))
      (error "Simplex must have one more vertex than dimensions of space."))
    (compute-function-values func simplex y)
    (compute-simplex-sum simplex simplex-sum)
    (do* ()
	 ((progn (multiple-value-setq (min-vtx max2-vtx max-vtx) (find-min-max y))
		 (or (< (compute-tolerance (aref y min-vtx) (aref y max-vtx)) 
			tolerance)
		     (> num-calls max-function-calls)))
	  (when (> num-calls max-function-calls)
	    (format t "~%Too many function calls in AMOEBA"))
	  (format t "~%")
	  (when print-frequency 
	    (format t "Final error: ~A~%" (funcall func (elt simplex min-vtx))))
	  (elt simplex min-vtx))	;return minimal vertex 
      (setq new-val (crawl (elt simplex max-vtx) temp-vertex
			   y simplex-sum func max-vtx *alpha*))
      (cond ((<= new-val (aref y min-vtx))
	     (setq new-val (crawl (elt simplex max-vtx) temp-vertex
				  y simplex-sum func max-vtx *gamma*)))
	    ((>= new-val (aref y max2-vtx))
	     (setq temp-val (aref y max-vtx))
	     (setq new-val  (crawl (elt simplex max-vtx) temp-vertex
				   y simplex-sum func max-vtx *beta*))
	     (when (>= new-val temp-val)
	       (when print-frequency (format t " .")) ;indicate contraction
	       (contract-simplex simplex min-vtx)
	       (compute-function-values func simplex y)
	       (compute-simplex-sum simplex simplex-sum)))
	    (t nil))
      (when (and (numberp print-frequency)
		 (= 0 (mod (incf count) print-frequency)))
	(funcall print-function (elt simplex min-vtx) (elt y min-vtx))))))

;;; for back compatibility
(defun amoeba (simplex func)
  (run-simplex simplex func))

(defun standard-simplex-print-function (vtx error)
  (format t "VALS: ")
  (write vtx :array t :pretty t)
  ;;(print-values vtx)
  (format t "~%ERROR: ~A~%" error))

;;; Contract the simplex toward the minimal vertex
(defun contract-simplex (simplex min-vtx) 
  (declare (special num-calls) (fixnum min-vtx))
  (let* ((min-vertex (elt simplex min-vtx))
	 (dim (length min-vertex)))
    (declare (type (simple-array single-float (*)) min-vertex) (fixnum dim))
    (loop for vertex in simplex do
	  (loop for i from 0 below dim do
		(setf (aref vertex i) 
		      (* 0.5 (+ (aref vertex i) (aref min-vertex i)))))))
  simplex)

;;; Reflect max-vertex of the simplex through the other vertices 
;;; according to factor.
(defun crawl (max-vertex temp-vertex y simplex-sum func max-vtx factor)
  (declare (special num-calls)
	   (type (simple-array single-float (*))
		 max-vertex temp-vertex simplex-sum y)
	   (fixnum max-vtx)
	   (float factor))
  (let* ((dim (length max-vertex))
	 (factor1 (/ (- 1.0 factor) dim))
	 (factor2 (- factor1 factor))
	 (temp-val 0.0))
    (declare (fixnum dim) (float factor1 factor2 temp-val)) 
    (loop for i from 0 below dim
	  do (setf (aref temp-vertex i) (- (* factor1 (aref simplex-sum i))
					   (* factor2 (aref max-vertex i)))))
    (setq temp-val (funcall func temp-vertex))
    (incf num-calls)
    (when (< temp-val (aref y max-vtx))
      (setf (aref y max-vtx) temp-val)
      (loop for i from 0 below dim 
	    for new-coord float = (aref temp-vertex i) do
	    (incf (aref simplex-sum i) (- new-coord (aref max-vertex i)))
	    (setf (aref max-vertex i) new-coord)))
    temp-val))

;;; Compute func at each of the vertices and store in vals
(defun compute-function-values (func simplex vals)
  (declare (special num-calls)
	   (type (simple-array single-float (*)) vals))
  (incf num-calls (length simplex))
  (loop for vertex in simplex 
	for v-num from 0
	do (setf (aref vals v-num) (funcall func vertex)))
  vals)

;;; Compute sum of simplex vertices and put into sum-vertex
(defun compute-simplex-sum (simplex sum-vertex)
  (declare (type (simple-array single-float (*)) sum-vertex))
  (let* ((dim (length sum-vertex)))
    (declare (fixnum dim))
    (loop for i from 0 below dim do
	  (setf (aref sum-vertex i) 0.0))
    (loop for vertex in simplex do
	  (loop for i from 0 below dim do
		(incf (aref sum-vertex i) (aref vertex i)))))
  sum-vertex)

;;; Normalized difference between min and max values.
(defun compute-tolerance (min max)
  (declare (float min max))
  (* 2.0 (/ (abs (- max min))
	    (+ (abs max) (abs min)))))

;;; Return positions of min, 2nd highest,  and max values in sequence
(defun find-min-max (vals)
  (declare (type (simple-array single-float (*)) vals))
  (let* ((min-pos 0) (max-pos 0) (max2-pos 0))
    (declare (fixnum min-pos max-pos max2-pos))
    (loop for i from 0 below (length vals)
	  for new-val = (aref vals i) do
	  (cond ((< new-val (aref vals min-pos))
		 (setq min-pos i))
		((> new-val (aref vals max-pos))
		 (setq max2-pos max-pos)
		 (setq max-pos i))
		((> new-val (aref vals max2-pos))
		 (setq max2-pos i))))
    (values min-pos max2-pos max-pos)))


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
