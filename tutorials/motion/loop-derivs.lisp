;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: loop-derivs.lisp
;;;  Author: Simoncelli
;;;  Description: Utility for computing flow fields.
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Nasty macro to loop over a sequence applying a set of
;;;; space-time separable filters.  This is done efficiently: only
;;;; applying the spatial filters to one frame on each iteration,
;;;; except on the first frame.  For straight convolutions, it could
;;;; be done faster by applying the temporal filter first.  But this
;;;; is indended to be used with prewarping!

;;; Tell emacs how to indent this macro:
(eval-when (load eval)
  (format t "[[EVAL-STREAM>>
           (put 'loop-applying-separable-filters 'common-lisp-indent-hook 2)
           <<EVAL-STREAM]]~%"))

;;; warper frames must corresond to convolution output frames.
(defmacro loop-applying-separable-filters
    ((sequence index &key start end warper) filter-clauses &body body)
  (let ((t-filt-lists '**t-filt-lists) (s-filt-lists '**s-filt-lists)
	(t-filts '**t-filts) (s-filts '**s-filts)
	(s-kernels '**s-kernels) (t-kernels '**t-kernels)
	(t-filt '**t-filt) (s-filt '**s-filt)
	(first-t-index '**first-t-index) (last-t-index '**last-t-index)
	(d '**d) (seq '**seq) (len '**len) (warp-seq '**warp-seq) (ignored '**ignored)
	(deriv-seq '**deriv-seq) (deriv-seq-list '**deriv-seq-list)
	(t-output '**t-output) (t-input '**t-input)
	(start-frame '**start-frame) (end-frame '**end-frame)
	(spatial-subseqs '**spatial-subseqs)
	(spatial-subseq-lists '**spatial-subseq-lists)
	deriv-seq-syms t-kernel-clauses s-kernel-clauses)
    ;; parse the clauses here:
    (loop for clause in filter-clauses
	  for sym = (car clause)
	  for t-kernels = (nth 1 clause)
	  for s-kernels = (nth 2 clause)
	  collect sym into syms
	  collect t-kernels into t-k-list
	  collect s-kernels into s-k-list
	  finally (setq deriv-seq-syms syms
			t-kernel-clauses t-k-list
			s-kernel-clauses s-k-list))
    `(let ((,seq ,sequence) (,warp-seq ,warper)
	   (,t-filt-lists (list ,@t-kernel-clauses))
	   (,s-filt-lists (list ,@s-kernel-clauses))
	   ,@deriv-seq-syms ,deriv-seq-list ,spatial-subseq-lists
	   ,len ,start-frame ,end-frame)
      ;; compute separable filter lists for each clause
      (loop with ,s-filts
	    with ,t-filts
	    for ,s-kernels on ,s-filt-lists
	    for ,t-kernels on ,t-filt-lists
	    do (multiple-value-setq (,t-filts ,s-filts)
		 (make-separable-combinations (car ,t-kernels) (car ,s-kernels)))
	    (setf (car ,s-kernels) ,s-filts)
	    (setf (car ,t-kernels) ,t-filts))
      (setq ,len (loop for ,t-filts in ,t-filt-lists maximize (total-size (car ,t-filts))))
      (setq ,start-frame (or ,start (floor ,len 2)))
      (setq ,end-frame (or ,end (- (length. ,seq) (ceiling ,len 2) -1)))
      (unless (or (null ,warp-seq)
		  (and (image-pair-sequence-p ,warp-seq)
		       (= (- ,end-frame ,start-frame) (length. ,warp-seq))))
	(error "Warp sequence ~A length is not equal to (- ~A ~A)" 
	       ,warp-seq ,end-frame ,start-frame))
      (setq ,spatial-subseq-lists	;initialize as a nested list of nils
       (loop for ,t-filts in ,t-filt-lists
	     collect
	     (loop for ,t-filt in ,t-filts
		   for ,len = (total-size ,t-filt)
		   collect nil)))
      (setq ,deriv-seq-list		;make derivative sequences
       (list ,@(loop for sym in deriv-seq-syms
		     for i from 0
		     collect `(setq ,sym (make-image-sequence (dimensions ,seq)
					  :length (length (nth ,i ,s-filt-lists)))))))
      (unwind-protect
	   (loop for ,index from ,start-frame below ,end-frame ;derivative frame
		 for ,ignored = 
		 (with-status-message (format nil "Computing derivs, frame ~A" ,index)
		   (without-status-messages
		    (loop for ,spatial-subseqs in ,spatial-subseq-lists ;each clause
			  for ,t-filts in ,t-filt-lists
			  for ,s-filts in ,s-filt-lists
			  for ,len = (total-size (car ,t-filts))
			  for ,first-t-index = (- ,index (floor ,len 2))
			  for ,last-t-index = (+ ,index (ceiling ,len 2) -1)
			  for ,deriv-seq in ,deriv-seq-list
			  do
			  (loop for ,t-filt in ,t-filts ;each derivative
				for ,s-filt in ,s-filts
				for ,t-input in ,spatial-subseqs
				for ,d from 0
				for ,t-output = (frame ,d ,deriv-seq)
				do
				(if (null ,t-input)
				    (setq ,t-input
					  (setf (nth ,d ,spatial-subseqs)
						(apply-filter ,s-filt ,seq
							      :start-frame ,first-t-index
							      :end-frame (1+ ,last-t-index))))
				    (progn ;only compute last frame
				      (apply-filter ,s-filt (frame ,seq ,last-t-index)
						    :-> (frame 0 ,t-input))
				      (rotate-seq! ,t-input)))
				(single-frame-tfilt-with-prewarping
				 ,t-filt ,t-input
				 (when ,warp-seq
				   (frame ,warp-seq (- ,index ,start-frame)))
				 :-> ,t-output)))))
		 ,@body)
	(apply 'obv::destroy-viewables
	       (append (listify ,spatial-subseq-lists) ,deriv-seq-list))))))

;;; Number of distinct terms in the polynomial (\sum_i x_i)^order,
;;; where the sum is over i from 1 to dimensionality.
(defun number-of-terms (order dimensionality)
  (unless (and (fixnump order) (>= order 0)
	       (fixnump dimensionality) (> dimensionality 0))
    (error "args must both be positive integers"))
  (cond ((or (= order 0) (= dimensionality 1)) 1)
	((= order 1) dimensionality)
	((= dimensionality 2) (1+ order))
	(t (loop for n from 0 to order
		 summing (number-of-terms n (- dimensionality 1))))))

;;; Return a list of t-filters and s-filters such that applying each
;;; pair of corresponding filter would produce the full set of Nth
;;; derivatives, from T^N to X^N.  kernels should be ordered from 0th
;;; deriv to Nth.
(defun make-separable-combinations (t-kernels s-kernels)
  (let ((t-filts (mapcar #'make-filter t-kernels))
	(s-filts (mapcar #'(lambda (k) (make-filter k :edge-handler *deriv-edges*))
			 s-kernels)))
    (loop with order = (- (length s-filts) 1)
	  for t-order from (1- (length t-filts)) downto 0
	  for s-order = (- order t-order)
	  for s-filts-of-order = (loop for x-order from 0 to s-order
				       for y-order = (- s-order x-order)
				       collect (make-separable-filter
						(nth y-order s-filts)
						(nth x-order s-filts)
						:edge-handler *deriv-edges*))
	  append s-filts-of-order into s-res-list
	  append (list-of-length (length s-filts-of-order) (nth t-order t-filts)) into t-res-list
	  finally (return (values t-res-list s-res-list)))))

#|
;;; TEST make-separable-combinations:
;;; substitude these for the filters
(append (list-of-length x-order :x) (list-of-length y-order :y))
(list-of-length t-order :t)

(apply 'mapcar #'append
       (multiple-value-list
	   (make-separable-combinations '((0) (1) (2)) '((3) (4) (5)))))
(apply 'mapcar #'append
       (multiple-value-list
	   (make-separable-combinations '((0) (1)) '((3) (4) (5)))))
|#

#| TEST DERIVATIVE COMPUTATIONS:
(compile-load "/u/eero/lisp/motion2/flow-utilities")

(setq *seq* (make-image-sequence
	   (loop for i from 0 below 5
		 collect (make-gaussian-noise '(16 16) ))))

;;;; TEST on first derivs (get filters from integer-warping.lisp):
(setq true-dt (apply-filter (make-separable-filter
			     *td-kernel*
			     (make-separable-filter *s-kernel* *s-kernel*
						    :edge-handler *deriv-edges*)) *seq*))
(setq true-dy (apply-filter (make-separable-filter
			    *t-kernel*
			    (make-separable-filter *sd-kernel* *s-kernel*
						   :edge-handler *deriv-edges*)) *seq*))
(setq true-dx (apply-filter (make-separable-filter
			     *t-kernel*
			     (make-separable-filter *s-kernel* *sd-kernel*
						    :edge-handler *deriv-edges*)) *seq*))
(setq dt (make-image-sequence (dimensions *seq*) :length (length. *seq*)))
(setq dy (make-image-sequence (dimensions *seq*) :length (length. *seq*)))
(setq dx (make-image-sequence (dimensions *seq*) :length (length. *seq*)))

(progn (zero! dt) (zero! dy) (zero! dx))
(loop-applying-separable-filters (*seq* my-i)
    ((derivs1 (list *t-kernel* *td-kernel*) (list *s-kernel* *sd-kernel*)))
  do
  (copy (frame 0 derivs1) :-> (frame my-i dt))
  (copy (frame 1 derivs1) :-> (frame my-i dy))
  (copy (frame 2 derivs1) :-> (frame my-i dx)))

(loop for i from 0 below (length. *seq*)
      do (pdb (mean-square-error (frame dt i) (frame true-dt i))
	      (mean-square-error (frame dy i) (frame true-dy i))
	      (mean-square-error (frame dx i) (frame true-dx i))))

;;; TEST 2nd derivs simultaneously:
;;; Get filters gauss dgauss d2gauss from deriv-filters.lisp:
(setq deriv2
      (loop-applying-separable-filters (*seq* my-i)
	  ((derivs1 (list *t-kernel* *td-kernel*) (list *s-kernel* *sd-kernel*))
	   (derivs2 (list gauss dgauss) (list gauss dgauss d2gauss)))
	finally (return (copy derivs2))))
(setq true-deriv2
      (make-image-sequence
       (multiple-value-bind (t-filts s-filts)
	   (make-separable-combinations (list gauss dgauss) (list gauss dgauss d2gauss))
	 (loop for s-filt in s-filts
	       for t-filt in t-filts
	       do
	       (loop for f in (list t-filt (filter-1 s-filt) (filter-2 s-filt))
		     do (pvect (kernel f) :digits 5)
		     finally (format t "~%"))
	       collect (frame 0 (apply-filter (make-separable-filter t-filt s-filt)
					      *seq* :start-frame 2 :end-frame 3))))))
(loop for i from 0 below 5
      do (pdb (mean-square-error (frame true-deriv2 i) (frame deriv2 i))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
