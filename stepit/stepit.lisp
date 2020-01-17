;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: stepit.lisp
;;;  Author: Chichilnisky
;;;  Description: Stepit interfce. Basic code is C code, ported from Fortran.
;;;  Creation Date: 12.23.91 brought in from working homebrew version.
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :obvius)
(export '(stepit-fit stepit-purge!))

;;; General Fitting Function Interface
;;; Here is the setup for fitting with stepit. After some experimentation,
;;; it seems to be the right general model for a fitting procedure. EJC 12.14.91.

;;; Stepit is called with the following arguments:
;;; 1. error-function - function to be called with parameters and other arguments (see below)
;;;    It is the value of this function that stepit attempts to minimize. 
;;; 2. initial-parameters - starting point of parameters (a single-float vector or array)
;;; 3. error-function-args - list of arguments that are passed (after the parameters)
;;;    to the error function at each iteration. These parameters are passed
;;;    to error-function as individual parameters, not as a list.
;;; keywords - control parametrs specific to stepit (as opposed to another fitting procedure)
;;; The main routines declares and accesses special global variables
;;; so that a running counts can be maintained.

;;; Example:
;;; (stepit-fit 'my-error-function (make-matrix 1.0 2.0) (list 100 200))
;;; then, at each iteration, my-error-function will be called these arguments:
;;; #(xx.xx xx.xx) 100 200
;;; where the first array contains the current parameters selected by stepit.
;;; See the test code for a simple example.

;;; Special notes:
;;; Foreign function interface is key. One foreign funciton declared.
;;; One foreign callable set up. One function pointer calculated.
;;; Note that the arrays here are passed to stepit() for repeated use in C function.
;;; Hence, all arrays given to stepit() must be static. Non-static arrays *will*
;;; cause screwups as Lisp moves them around.

;;; BUGS:
;;; During October, 1992, there were suspicious behaviors
;;; that suggested a memory overwrite. This code was inserted after the ffi
;;; call to catch problems; none were observed after some cleanup of ffi code.
;;; I have left this code around for possible future use,
;;; though I hope it won't be needed!
#|
(unless (and (almost-equal t-lower-bounds lower-bounds)
	     (almost-equal t-upper-bounds upper-bounds))
  (error "Memory overwrite in STEPIT: ~a != ~a or ~a != ~a"
	 t-lower-bounds lower-bounds t-upper-bounds upper-bounds))
|#

;;; Parameters that are accessed at each function call.
;;; These must be stacks so that stepit can be re-entrant.
(defvar *stepit-parameters-stack* nil)
(defvar *stepit-function-stack* nil)
(defvar *stepit-interval-stack* nil)
(defvar *stepit-counter-stack* nil)
(defvar *stepit-arguments-stack* nil)


(defun stepit-purge! ()
  (declare (special *stepit-parameters-stack* *stepit-interval-stack* *stepit-function-stack*
		    *stepit-counter-stack* *stepit-arguments-stack*))
  
  (setq *stepit-parameters-stack* nil)
  (setq *stepit-function-stack* nil)
  (setq *stepit-interval-stack* nil)
  (setq *stepit-counter-stack* nil)
  (setq *stepit-arguments-stack* nil)
  )

;;; Error function that gets called at each iteration from stepit.
(defun stepit-error-function ()  
  (declare (special *stepit-parameters-stack* *stepit-interval-stack* *stepit-function-stack*
		    *stepit-counter-stack* *stepit-arguments-stack*))
  (let ((error (apply (car *stepit-function-stack*)
		      (car *stepit-parameters-stack*)
		      (car *stepit-arguments-stack*)))
	(interval (car *stepit-interval-stack*))
	(counter (incf (nth 0 *stepit-counter-stack*))))
    (when (and interval (zerop (mod counter interval)))
      (status-message "~d: ~f" counter error))
    error))


;;; Main interface function.
(defun stepit-fit (error-function
		   initial-parameters 
		   error-function-args
		   &key
		   (lower-bounds (fill! (similar initial-parameters) -1e10))
		   (upper-bounds (fill! (similar initial-parameters) 1e10))
		   (smallest-steps (mul initial-parameters 1e-3))
		   (initial-steps (mul initial-parameters 0.1))
		   (max-function-calls most-positive-fixnum)
		   count randomize masks (mask masks)
		   ((:-> result) (similar initial-parameters)))

  (declare (special *stepit-parameters-stack* *stepit-interval-stack* *stepit-function-stack*
		    *stepit-counter-stack* *stepit-arguments-stack*))
  
  (let* ((dimension (total-size initial-parameters))
	 (foreign-function-pointer (foreign-function-pointer '_stepit_foreign_error_function))
	 (counter 0)
	 (mosq 5)			; Constant used by STEPIT
	 (ntrac -1)			; Constant used by STEPIT
	 error)
    
    (with-static-arrays ((parameters (allocate-array (dimensions initial-parameters)
						     :element-type 'single-float))
			 (err (similar parameters :dimensions (* dimension mosq)))
			 (t-lower-bounds (similar parameters))
			 (t-upper-bounds (similar parameters))
			 (t-smallest-steps (similar parameters))
			 (t-initial-steps (similar parameters))
			 (dx (similar parameters))
			 (xs (similar parameters))
			 (dlx (similar parameters))
			 (fosc (similar parameters :dimensions mosq))
			 (xosc (similar parameters :dimensions (* mosq dimension)))
			 (salvo (similar parameters))
			 (fstor (similar parameters))
			 (vec (similar parameters))
			 (mask-array (similar parameters :element-type '(signed-byte 32)))
			 (jflat (similar mask-array)))

      ;; Make keywords into static.
      (copy lower-bounds :-> t-lower-bounds)
      (copy upper-bounds :-> t-upper-bounds)
      (copy smallest-steps :-> t-smallest-steps)
      (copy initial-steps :-> t-initial-steps)
      
      ;; Start with initial parameters, optionally randomized
      (if randomize
	  (randomize initial-parameters (if (equal randomize t) initial-parameters randomize)
		     :-> parameters)
	  (copy initial-parameters :-> parameters))
      
      ;; Then fill the mask array according to keyword argument.
      ;; Any masked parameters that were randomized should be reset.
      (fill! mask-array 0)
      (when mask
	(with-displaced-vectors ((d-initial-parameters initial-parameters)
				 (d-parameters parameters)
				 (d-mask-array mask-array)
				 (d-mask mask))
	  (dotimes (i dimension)
	    (unless (zerop (aref d-mask i))
	      (setf (aref d-mask-array i) 1)
	      (setf (aref d-parameters i) (aref d-initial-parameters i))))))
      
      ;; Set up global data for error function access
      (push parameters *stepit-parameters-stack*)
      (push error-function *stepit-function-stack*)
      (push count *stepit-interval-stack*)
      (push counter *stepit-counter-stack*)
      (push error-function-args *stepit-arguments-stack*)
	
      ;; Call the C routine, passing it the address of the error function
      ;; and a bunch of arrays that it needs.
      (setq error (stepit_fit foreign-function-pointer parameters
			      t-lower-bounds t-upper-bounds
			      t-smallest-steps t-initial-steps
			      err dx xs dlx xosc salvo fstor vec fosc mask-array jflat
			      dimension max-function-calls ntrac mosq))
      (setq counter (pop *stepit-counter-stack*))
      (pop *stepit-parameters-stack*)
      (pop *stepit-function-stack*)
      (pop *stepit-interval-stack*)
      (pop *stepit-arguments-stack*)
      (copy parameters :-> result)
      (values result error counter))))
  
#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sample fitting code
(setf vector (make-matrix 1 3))

(stepit-fit #'vector-distance (make-matrix 10 10) (list vector))
(stepit-fit #'vector-distance (make-matrix 10 10) (list vector) :mask #*10)
(stepit-fit #'vector-distance (make-matrix 10 10) (list vector) :mask (make-matrix 1 0))
(stepit-fit #'vector-distance (make-matrix 10 10) (list vector) :mask #*00)
(stepit-fit #'vector-distance (make-matrix 10 10) (list vector) :count 2)
(stepit-fit #'vector-distance (make-matrix 10 10) (list vector) :smallest-steps
	    (make-matrix 0.1 0.1) :count 5)
(stepit-fit #'vector-distance (make-matrix 10 10) (list vector) :lower-bounds (make-matrix 4 4))
(stepit-fit #'vector-distance (make-matrix 10 10) (list vector) :upper-bounds (make-matrix 4 4))

;;; Test the error handling with a non-deterministic error-function,
;;; to see that the error-handling works OK.
(defun my-error-function (parameters vector)
  (random 1.0))
(stepit-fit 'my-error-function (make-matrix 10 10) (list vector))
|#


;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
