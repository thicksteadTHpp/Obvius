;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: statistics.lisp
;;;  Author: Chichilnisky
;;;  Description: Standard distributions and CDFs, noise generation,
;;;               functions for statistical psychophysics modeling.
;;;  Creation Date: 5/23/92
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :obvius)

(export '(check-probability check-integer weibull inverse-weibull
	  detection-weibull inverse-detection-weibull 2afc-weibull inverse-2afc-weibull
          detection-log-normal inverse-detection-log-normal
	  detection-normal inverse-detection-normal
          chi-square cumulative-chi-square cumulative-f cumulative-t
          poisson cumulative-poisson poisson-noise factorial factor
          cumulative-binomial binomial binomial-noise bernoulli-noise
	  binomial-coefficient log-binomial-coefficient
	  standard-deviation sample-variance sample-standard-deviation standard-error
	  poisson-randomize gaussian-randomize
	  logistic inverse-logistic confidence-interval mean-confidence-interval))

;;; Common error-checking for stats functions.
(defun check-probability (p)
  (unless (<= 0.0 p 1.0)
    (error "~a is not in the range [0,1]" p)))

(defun check-positive (p)
  (unless (plusp p)
    (error "Argument ~a must be positive" p)))

(defun check-nonnegative (p)
  (when (minusp p)
    (error "Argument ~a must be nonnegative" p)))

(defun check-integer (&rest numbers)
  (dolist (number numbers)
    (unless (= (ffloor number) number)
      (error "~a is not an integer" number))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common probability densities and cumulative densities

(defun chi-square (x df &key (sigma 1.0))
  (check-integer df)
  (let ((df/2 (/ df 2.0)))
    (if (plusp x)
	(* (exp (- (/ x (* 2.0 (sqr sigma)))))
	   (expt x (- df/2 1))
	   (/ (expt 2.0 df/2))
	   (/ (expt sigma df))
	   (/ (exp (gammln df/2))))
	0.0)))
#|
(make-discrete-function #'(lambda(x) (chi-square x 2)) 0.0 10.0 :size 128)
|#

(defun cumulative-chi-square (x df)
  (check-integer df)
  (check-positive df)
  (check-nonnegative x)
  (gammp (/ df 2.0) (/ x 2.0)))

#|
(cumulative-chi-square 10.0 8) ; 0.7349740266799927
(cumulative-chi-square 10.0 7) ; 0.8114265
(cumulative-chi-square 10.0 4) ; 0.95957
(cumulative-chi-square 10.0 11) ; 0.46961
(cumulative-chi-square 10.0 12) ; 0.384039
(cumulative-chi-square 6 2) ; 0.95
(cumulative-chi-square 11.07 5); 0.95
(cumulative-chi-square 18.31 10); 0.95
(cumulative-chi-square 1.15 5) ; 0.05
(cumulative-chi-square 3.94 10) ; 0.05
|#

(defun poisson (k lambda)
  (/ (* (expt lambda k) (exp (- lambda))) (factorial k)))

#|
(make-discrete-function #'(lambda(x) (poisson x 2)) 0.0 10.0 :size 11)
(distribution-mean *)

(make-discrete-function #'(lambda(x) (poisson x 10)) 0.0 50.0 :size 51)
(distribution-mean *)
|#

;; The numerical recipes version works up to 35, then overflows single-floats.
;; Lisp works beyond that, but is slow. So, we use a hybrid.
(defun factorial (x)
  (check-integer x)
  (check-nonnegative x)
  (cond ((zerop x) 1)
	((> x 34) (loop with val = 1
			for x from x downto 2
			do (setq val (* val x))
			finally (return val)))
	(t (round (exp (factln x))))))

#|
;; 2 6 24 120 big big big big
(mapcar #'factorial '(2 3 4 5 33 34 35 36))
|#

;;; Pr{observed < count}
(defun cumulative-poisson (count lambda)
  (check-integer count)
  (if (zerop count) 0.0
      (gammq (ffloor count) (float lambda))))

#|
;; Test poisson distribution and cumulative
(let ((lambda 100)
      (count 80))
  (print (loop for i from 0 below count
	       sum (poisson i lambda)))
  (print (cumulative-poisson count lambda))
  nil)
|#

(defun cumulative-f (x df-1 df-2)
  (check-integer df-1 df-2)
  (check-nonnegative x)
  (if (plusp x)
      (- 1.0 (betai (/ df-2 2.0) (/ df-1 2.0) 
                    (/ (float df-2) (+ df-2 (* df-1 x)))))
      0.0))
#|
(cumulative-f 55.83 4 1) ; 0.90
(cumulative-f 4.19 3 4) ; 0.90
(cumulative-f 9.28 3 3) ; 0.95
(cumulative-f 3.18 9 9) ; 0.95
|#

(defun cumulative-t (x df)
  (check-integer df)
  (let ((result (- 1.0 (/ (betai (/ df 2.0) 0.5 (/ df (+ df (sqr x)))) 2.0))))
    (if (plusp x) result (- 1.0 result))))

#|
(cumulative-t 6.314 1) ; 0.95
(cumulative-t 1.943 6) ; 0.95
(cumulative-t 1.796 11) ; 0.95
(cumulative-t 0.325 1) ; 0.6
(cumulative-t 0.265 6) ; 0.6
(cumulative-t 0.256 26) ; 0.6
|#

;;; Probability of k successes from a binomial draw with probability p and n trials
(defun binomial (k p n)
  (check-probability p)
  (* (expt p k) (expt (- 1.0 p) (- n k)) (binomial-coefficient n k)))

;; This is the probability of n or fewer successes (not probability of n or more).
;; This is consistent with cumulative distributions which rise up to 1,
;; rather than the NR terminology. 
(defun cumulative-binomial (count p trials)
  (check-integer trials count)
  (let ((limit (+ count 1)))
    (cond ((>= limit trials) 1.0)
	  ((minusp limit) 0.0)
	  ((- 1.0 (betai (float limit) (+ 1.0 (- trials limit)) (float p)))))))

;;; Number of distinct ways of choosing k binomial successes in n trials.
;;; Alternately, number of distinct binary strings of length n that contain k 1s.
;;; Same as the words "n choose k"
;;; Use the Numerical Recipes version for speed
(defun binomial-coefficient (n k)
  (check-integer n k)
  (round (bico (floor n) (floor k))))

(defun log-binomial-coefficient (n k)
  (check-integer n k)
  (- (factln n) (factln k) (factln (- n k))))

#|
(let ((n 5)
      (k 2))
  (print (binomial-coefficient n k))
  (print (/ (factorial n) (* (factorial k) (factorial (- n k)))))
  nil)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generation of noise (random draws) from various given distributions

(defun bernoulli-noise (p)
  (if (< (random 1.0) p) 1 0))

(defun binomial-noise (p &optional (trials 1))
  (check-integer trials)
  (loop for i from 0 below trials count (not (zerop (bernoulli-noise p)))))

;;; Two ways to compute Poisson noise:
;;; 1) Use a NR approximation to the CDF
;;; 2) Build up the CDF as you go along
;;; The former's run-time is too slow for low-mean Poisson.
;;; The latter's run-time climbs with mean of Poisson, so bad for high mean.
;;; So, use hybrid.
;;; Note: (exp) underflows around -700. This is a way to set the cutoff.
;;; *** If the probabilities were done with logs, thing might work better for high mean.
(defun poisson-noise (lambda)
  (if (< lambda 700)
      ;; Build up the CDF as you go
      (let ((random (random 1.0))
	    (flambda (float lambda))	; Save floating conversions
	    (probability (exp (- lambda))) ; Current probability
	    (cumulative (exp (- lambda)) ) ; Cumulative probability
	    )
	(declare (single-float random cumulative probability flambda))
	(when (zerop probability)
	  (error "Poisson exponent too large"))
	(loop for i from 1
	      when (> cumulative random)
	      return (1- i)
	      do
	      (setq probability (* probability (/ flambda (float i))))
	      (incf cumulative probability)))
      ;; Use NR approximation to CDF
      (loop for i from 0
	    with random = (random 1.0)
	    when (> (cumulative-poisson i lambda) random)
	    return (1- i))))

#|
;;; Test Poisson noise
(length (setf vec (make-array 1000 :element-type 'single-float)))
(progn
  (dotimes (i (length vec))
    (setf (aref vec i) (float (poisson-noise 6))))
  (values (mean vec) (variance vec)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Randomization methods based on noise generation
(defmethod gaussian-randomize ((arr array) (variance number) &key ((:-> result) (similar arr)))
  (unless (eq arr result)
    (copy arr :-> result))
  (with-displaced-vectors ((vec result))
    (dotimes (i (length vec))
      (declare (fixnum i))
      (incf (aref vec i) (gaussian-noise 0.0 variance)))
    result))

(defmethod gaussian-randomize ((arr array) (variance array) &key ((:-> result) (similar arr)))
  (unless (eq arr result)
    (copy arr :-> result))
  (with-displaced-vectors ((vec result)
			   (v-vec variance))
    (dotimes (i (length vec))
      (declare (fixnum i))
      (incf (aref vec i) (gaussian-noise 0.0 (abs (aref v-vec i)))))
    result))

(defmethod poisson-randomize ((arr array) (lambda array) &key ((:-> result) (similar arr)))
  (with-displaced-vectors ((r-vec result)
			   (a-vec arr)
			   (m-vec lambda))
    (dotimes (i (length r-vec))
      (declare (fixnum i))
      (setf (aref r-vec i)
	    (+ (poisson-noise (aref m-vec i))
	       (aref a-vec i))))
    result))

(defmethod poisson-randomize ((arr array) (lambda number) &key ((:-> result) (similar arr)))
  (with-displaced-vectors ((r-vec result)
			   (a-vec arr))
    (dotimes (i (length r-vec))
      (declare (fixnum i))
      (setf (aref r-vec i) (+ (poisson-noise lambda) (aref a-vec i))))
    result))

#|
(let ((array (make-array 1000 :element-type 'single-float)))
  (zero! array)
  (poisson-randomize array 10.0 :-> array)
  (format t "~a ~a" (mean array) (variance array)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statistics on sample data
(defun standard-deviation (thing)
  (sqrt (variance thing)))

(defun sample-variance (thing)
  (* (variance thing) (/ (total-size thing) (- (total-size thing) 1))))

(defun sample-standard-deviation (thing)
  (sqrt (sample-variance thing)))

(defun standard-error (thing)
  (/ (sample-standard-deviation thing) (sqrt (total-size thing))))

(defun confidence-interval (thing)
  (let ((mean (mean thing))
        (range (* 2.0 (sample-standard-deviation thing))))
    (list (- mean range) (+ mean range))))

(defun mean-confidence-interval (thing)
  (let ((mean (mean thing))
        (range (* 2.0 (standard-error thing))))
    (list (- mean range) (+ mean range))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functional forms common to psychophysical statistical models

(defun weibull (x &optional (alpha 1.0) (beta 1.0) (gamma 0.0))
  (- 1.0 (* (- 1.0 gamma) (exp (- (expt (/ x alpha) beta))))))

(defun inverse-weibull (w &optional (alpha 1.0) (beta 1.0) (gamma 0.0))
  (* alpha (expt (- (log (/ (- 1.0 w) (- 1.0 gamma)))) (/ 1.0 beta))))

(defun detection-weibull (x &optional (alpha 1.0) (beta 1.0))
  (- 1.0 (exp (- (expt (/ x alpha) beta)))))

(defun inverse-detection-weibull (w &optional (alpha 1.0) (beta 1.0))
  (* alpha (expt (- (log (- 1.0 w))) (/ 1.0 beta))))

(defun 2afc-weibull (x &optional  (alpha 1.0) (beta 1.0))
  (- 1.0 (* 0.5 (exp (- (expt (/ x alpha) beta))))))

(defun inverse-2afc-weibull (w &optional  (alpha 1.0) (beta 1.0))
  (* alpha (expt (- (log (/ (- 1.0 w) 0.5))) (/ 1.0 beta))))

;;; Cumulative normal on a log scale
(defun detection-log-normal (x &optional (half 1.0) (slope 1.0))
  (cumulative-normal (* slope (log (/ x half) 2.0))))

(defun inverse-detection-log-normal (x &optional (half 1.0) (slope 1.0))
  (* half (expt 2.0 (/ (inverse-cumulative-normal x) slope))))

(defun detection-normal (x &optional (mean 0.0) (sd 1.0))
  (cumulative-normal (/ (- x mean) sd)))

(defun inverse-detection-normal (x &optional (mean 0.0) (sd 1.0))
  (+ (* (inverse-cumulative-normal x) sd) mean))

(defmethod logistic ((x number) &key)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defmethod inverse-logistic ((x number) &key)
  (- (log (/ (- 1.0 x) x))))

(defmethod logistic ((arr array) &key ((:-> result) (similar arr)))
  (check-size arr result)
  (cond ((float-arrays-p arr result)
	 (internal-logistic arr result (total-size arr)))
	(t (with-displaced-vectors ((displaced-arr arr) 
				    (displaced-result result))
	     (dotimes (i (total-size arr))
	       (declare (fixnum i))
	       (setf (aref displaced-result i)
		     (/ 1.0 (+ 1.0 (exp (- (aref displaced-arr i))))))))))
  result)

(defmethod inverse-logistic ((arr array) &key ((:-> result) (similar arr)))
  (div 1.0 arr :zero-val most-positive-single-float :suppress-warning t :-> result)
  (sub result 1.0 :-> result)
  (natural-logarithm result :zero-val most-negative-single-float
		     :suppress-warning t :-> result)
  (negate result :-> result))

;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
