;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: 
;;;  Author:
;;;  Description:
;;;  Creation Date:
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '(normal cumulative-normal inverse-cumulative-normal
	  gaussian-noise normal-noise))


;;; Normal distributions and related friends. Thanks to D. Pelli.
(defun normal (x)
  (/ (exp (- (* 0.5 (sqr x)))) (sqrt 2-pi)))

;;; From Abramowitz and Stegun Eq. (26.2.17).
;;; Error |e|<7.5 10^-8
(defun cumulative-normal (x)
  (if (minusp x)
      (- 1.0 (cumulative-normal (- x)))
      (let* ((tmp (/ 1.0 (+ 1.0 (* 0.2316419 x))))
             (tmp1 tmp)
             (P (* tmp1 0.319381530)))
        (setq tmp1 (* tmp tmp1))
        (incf P (* tmp1 -0.356563782))
        (setq tmp1 (* tmp tmp1))
        (incf P (* tmp1 1.781477937))
        (setq tmp1 (* tmp tmp1))
        (incf P (* tmp1 -1.821255978))
        (setq tmp1 (* tmp tmp1))
        (incf P (* tmp1 1.330274429))
        (- 1.0 (* P (normal x))))))

;;; z-score, i.e. inverse of cumulative-normal(),
;;; based on Abramowitz and Stegun Eq. 26.2.23.
;;; Error |e| < 4.5 10^-4.
(defun inverse-cumulative-normal (p)
  (if (> p 0.5)
      (- (inverse-cumulative-normal (- 1.0 p)))
      (let* ((tmp (sqrt (* -2.0 (log p))))
             (tmp-sqr (sqr tmp)))
        (- (- tmp (/ (+ 2.515517 (* 0.802853 tmp) (* 0.010328 tmp-sqr))
                     (+ 1.0 (* 1.432788 tmp) (* 0.189269 tmp-sqr)
                        (* 0.001308 tmp tmp-sqr))))))))

#|
(make-discrete-function 'cumulative-normal -2.0 2.0)
(make-discrete-function 'inverse-cumulative-normal 0.001 0.999)
|#

(defun normal-noise ()
  (gaussian-noise 0.0 1.0))

(defmethod gaussian-noise ((mean number) (variance number) &key ->)
  (declare (ignore ->))
  (+ mean (* (inverse-cumulative-normal (random 1.0)) (sqrt variance))))

#|
;;; Test example:
(length (setf vec (make-array 1000 :element-type 'single-float)))
(dotimes (i (length vec))
  (setf (aref vec i) (gaussian-noise 0.0 1.0)))
(values (mean vec) (variance vec))
(make-histogram vec)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Multivariate gaussian noise: depends on SVD code.

;;; Generate gaussian noise given a covariance matrix.
;;; If Y is a (row) vector with entries distributed iid N(0,1),
;;; and if Z is a (row) vector, Z = Y A  (some matrix A)
;;; then the covariance matrix of Z is At*A.
;;; Hence, given a covariance matrix M for Z (desired covariance),
;;; we do this:
;;; (0) Decompose M = A At
;;; (1) Generate Y
;;; (2) Return Z = Y At
(defmethod gaussian-noise ((mean vector) (covariance array) &key ((:-> result) (similar mean)))
  (declare (type (array single-float) result)
           (type (vector single-float) mean)
           (type (array single-float (* *)) covariance))
  (let* ((temp-arr (similar result))
         (temp-vec (vectorize temp-arr))
         (transform (matrix-transpose (quadratic-decomposition covariance))))
    (declare-matrices () (temp-vec))
    (dotimes (index (length temp-vec))
      (declare (fixnum index))
      (setf (aref temp-vec index) (gaussian-noise 0.0 1.0)))
    (matrix-mul temp-arr transform :-> result)
    (add-rows result mean :-> result))
  result)

#|
;;; Will work one vector at a time or on rows.
(dimensions (setf noise (make-array '(500 2) :element-type 'single-float)))
(setf mean (make-matrix '(10.0 10.0)))
(setf transform (make-matrix '((20.0 10.0) (-3.0 15.0))))
(setf covariance (matrix-transpose-mul transform transform))

;;; Make one/many observations, check them against desired
(print-values (gaussian-noise mean covariance))
(dimensions (gaussian-noise mean covariance :-> noise))
(print-values (div (sub (covariance-rows noise) covariance) covariance))
(print-values (div (sub (mean-rows noise) mean) mean))

;;; Check the circularizing transform
(print-values (covariance-rows (matrix-mul noise (matrix-inverse transform))))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;; Old gaussian-noise code.  Commented out 5-26-92 by DH and EJ.

;;; A very good and easy way to generate a 2-D (x,y) gaussian zero-mean unit
;;; deviation random variable, is to generate two random numbers rand1, rand2
;;; between 0 and 1 then 
;;; 
;;; 	x = sqrt(-2.0 * log(rand1)) * cos(TWOPI*rand2);
;;; 	y = sqrt(-2.0 * log(rand1)) * sin(TWOPI*rand2);
;;; 
;;; For a 1-D number only take x or y.
;;; 
;;; Algorithm is referenced in "Numerical Methods" (Germund Dahlquist).  

(defun gaussian-noise (&key (variance 1.0) (iterations 6))
  (setq iterations (round iterations))
  (let ((sum (/ iterations -2.0))
	(stdev (sqrt (* (/ 12.0 iterations) variance)))) ;var of uniform is 1/12
    (declare (single-float sum stdev))
    (loop for i from 0 below iterations do (incf sum (random 1.0)))
    (* stdev sum)))

;;; Polar method for normal deviates
;;; Modified from Image-Calc (lisp-defs.lisp)
;;; Algorithm from Knuth Volume II
;;; returns two samples from normal distribution

(defun normal-random (&key (sigma 1.0) (mean 0.0))
  (loop for v1 = (- (random 2.0) 1.0)
	for v2 = (- (random 2.0) 1.0)
	for s = (+ (sqr v1) (sqr v2))
	until (< s 1)
	finally
	(let ((r (* sigma (sqrt (/ (* -2 (log s)) s)))))
	  (return (values (+ (* v1 r) mean) (+ (* v2 r) mean))))))

;;; Uses old iterative algorithm, with new arg-list. Not very
;;; different statistically.
(defmethod gaussian-noise ((mean number) (variance number) &key -> (iterations 6))
  (declare (ignore ->))
  (let ((sum (/ iterations -2.0))
	(stdev (sqrt (* (/ 12.0 iterations) variance)))) ;var of uniform is 1/12
    (declare (single-float sum stdev))
    (loop for i from 0 below iterations do (incf sum (random 1.0)))
    (+ mean (* stdev sum))))
|#




;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
