;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Psychophysics analysis code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

(export '(psychometric-model
	  data-file header-plist
	  levels trials fractions staircase-levels staircase-responses
	  repetition-limit parametric-function inverse-parametric-function
	  parameters lower-bounds upper-bounds
	  load-psychometric-data load-all-psychometric-data
	  number-of-levels remove-infrequent-levels pool
	  maximum-likelihood-fit maximum-likelihood-fit-error
	  log-likelihood log-likelihood-ratio degrees-of-freedom
	  likelihood-ratio-test
	  make-psychometric-plot))

(obv-require :stepit)
(obv-require :statistics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General structure for keeping track of psychometric data.
;;; Note that the user is responsible for keeping slots consistent.
;;; All items are numbers or arrays.

(def-simple-class psychometric-model ()
  (data-file
   header-plist				; plist header, read from the raw data file
   levels				; Independent variable values
   trials				; Number of trials at each level
   fractions                            ; Fraction correct at each level
   staircase-levels			; Intensity used for each successive trial
   staircase-responses			; Subject's response for each successive trial
   repetition-limit			; used for remove-infrequent-levels (nil or number)
   parametric-function			; example: 'detection-weibull
   inverse-parametric-function		; example: 'inverse-detection-weibull
   parameters				; list of parameters
   lower-bounds				; list of lower bounds or nil
   upper-bounds				; list of upper bounds or nil
   ))

(defun load-psychometric-data
    (data-file
     &key
     (level-key (error "must pass keyword specification for variable arg"))
     (response-key :response)
     search-plist
     repetition-limit
     parametric-function inverse-parametric-function parameters
     lower-bounds upper-bounds)
  (let (header trial-plists
        levels trials fractions
	staircase-responses staircase-levels)
    (with-open-file (stream data-file :direction :input)
      (setq header (read stream))
      (setq trial-plists (loop for trial-plist = (read stream nil nil)
			       while trial-plist
			       collect trial-plist)))
    (when search-plist
      (let ((search-keys (loop for l = search-plist
			       then (cddr l)
			       while l
			       collect (car l))))
	(setq trial-plists
	      (remove-if #'(lambda (trial-plist)
			     (not (every #'eq
					 (loop for key in search-keys
					       collect
					       (getf search-plist key))
					 (loop for key in search-keys
					       collect
					       (getf trial-plist key)))))
			 trial-plists))))
    (setq levels (make-matrix (sort
			       (remove-duplicates
				(mapcar #'(lambda (x) (getf x level-key)) trial-plists))
			       #'<)))
    (setq trials (similar levels))
    (setq fractions (similar levels))
    (loop for trial-plist in trial-plists
	  for level = (getf trial-plist level-key)
	  for position = (position level levels :test #'almost-equal)
	  do
	  (incf (aref trials position))
	  (when (getf trial-plist response-key)
	    (incf (aref fractions position))))
    (div fractions trials :-> fractions)
    (setq staircase-responses
	  (make-matrix (mapcar #'(lambda (x) (if (getf x response-key) 1 0)) trial-plists)))
    (setq staircase-levels
	  (make-matrix (mapcar #'(lambda (x) (getf x level-key)) trial-plists)))
    (make-instance 'psychometric-model
		   :data-file (file-namestring data-file)
		   :header-plist header
		   :levels levels
		   :trials trials
		   :fractions fractions
		   :staircase-levels staircase-levels
		   :staircase-responses staircase-responses
		   :repetition-limit repetition-limit
		   :parametric-function parametric-function
		   :inverse-parametric-function inverse-parametric-function
		   :parameters parameters
		   :lower-bounds lower-bounds
		   :upper-bounds upper-bounds)))

;;; Loads info from subject-analyzed and subject-done files.
;;; Constructs psychometric-models for all conditions.  If :reanalyze
;;; is non-nil, then it refits all the psychometric-models (using
;;; parametric-function and initial-parameters).  If :reanalyze nil
;;; then it fits only those psychometric-models that are not already
;;; in the analyzed file.
(defun load-all-psychometric-data
    (subject-name directory
     &key
     (level-key (error "must pass keyword specification for variable arg"))
     (response-key :response)
     search-plist
     reanalyze
     repetition-limit
     parametric-function inverse-parametric-function initial-parameters
     lower-bounds upper-bounds)
  (let ((done-file (merge-pathnames
		    directory (format nil "~a-done" subject-name)))
	(analyzed-file (merge-pathnames
			directory (format nil "~a-analyzed" subject-name)))
	(analyzed-bak (merge-pathnames
			directory (format nil "~a-analyzed-backup" subject-name)))
	done-plists analyzed-plists not-analyzed-plists
	psychometric-models)
    (when (and reanalyze (probe-file analyzed-file))
      (rename-file analyzed-file analyzed-bak))    
    (with-open-file (done-stream done-file
				 :direction :input :if-does-not-exist :create)
      (setq done-plists (loop for done-plist = (read done-stream nil nil)
			      while done-plist
			      collect done-plist)))
    (with-open-file (analyzed-stream analyzed-file
				     :direction :input :if-does-not-exist :create)
      (setq analyzed-plists (loop for analyzed-plist = (read analyzed-stream nil nil)
			      while analyzed-plist
			      collect analyzed-plist)))
    (loop with analyzed-data-files = (mapcar #'(lambda (x) (getf x :data-file))
					     analyzed-plists)
	  for done-plist in done-plists
	  do
	  (unless (member (getf done-plist :data-file) analyzed-data-files :test #'equal)
	    (setq not-analyzed-plists (cons done-plist not-analyzed-plists))))

    ;; Construct psychometric models for data that is already analyzed.
    (loop for analyzed-plist in analyzed-plists
	  for data-file = (merge-pathnames directory (getf analyzed-plist :data-file))
	  for psychometric-model =
	      (load-psychometric-data
	       data-file
	       :level-key level-key
	       :response-key response-key
	       :search-plist search-plist
	       :repetition-limit repetition-limit
	       :parametric-function (getf analyzed-plist :parametric-function)
	       :inverse-parametric-function (getf analyzed-plist :inverse-parametric-function)
	       :parameters (getf analyzed-plist :parameters)
	       :lower-bounds (getf analyzed-plist :lower-bounds)
	       :upper-bounds (getf analyzed-plist :upper-bounds))
	  do
	  (unless (and (eq (getf analyzed-plist :parametric-function)
			   parametric-function)
		       (eq (getf analyzed-plist :inverse-parametric-function)
			   inverse-parametric-function))
	    (warn "Data from ~a was analyzed using different fitting functions: ~a and ~a"
		  (getf analyzed-plist :data-file)
		  (getf analyzed-plist :parametric-function)
		  (getf analyzed-plist :inverse-parametric-function)))
	  (unless (eq repetition-limit (getf analyzed-plist :repetition-limit))
	    (warn "Data from ~a was analyzed using different repetition limit: ~a"
		  (getf analyzed-plist :data-file)
		  (getf analyzed-plist :repetition-limit)))
			   
	  (setq psychometric-models (cons psychometric-model psychometric-models)))
    
    ;; Construct psychometric models for data that is not yet analyzed.
    (loop for not-analyzed-plist in not-analyzed-plists
	  for data-file = (merge-pathnames directory (getf not-analyzed-plist :data-file))
	  for psychometric-model =
	      (load-psychometric-data
	       data-file
	       :level-key level-key
	       :response-key response-key
	       :search-plist search-plist
	       :repetition-limit repetition-limit
	       :parametric-function parametric-function
	       :inverse-parametric-function inverse-parametric-function
	       :parameters (copy-list initial-parameters)
	       :lower-bounds lower-bounds
	       :upper-bounds upper-bounds)
	  do
	  (status-message "Fitting data from ~a" (file-namestring data-file))
	  (when repetition-limit
	    (remove-infrequent-levels psychometric-model :repetition-limit repetition-limit))
	  (maximum-likelihood-fit psychometric-model)
	  (with-open-file (analyzed-stream analyzed-file
					   :direction :output :if-exists :append)
	    (format analyzed-stream "~s~%"
		    (append not-analyzed-plist
			    (list :repetition-limit repetition-limit
				  :parametric-function parametric-function
				  :inverse-parametric-function inverse-parametric-function
				  :parameters (parameters psychometric-model)
				  :lower-bounds (lower-bounds psychometric-model)
				  :upper-bounds (upper-bounds psychometric-model)))))
	  (setq psychometric-models (cons psychometric-model psychometric-models)))
    psychometric-models))


(defmethod number-of-levels ((psychometric-model psychometric-model))
  (length (levels psychometric-model)))

;; Remove all the trials that have *fewer* than the specified number
;; of trials.
;; *** Does destructive modification.
(defmethod remove-infrequent-levels ((psychometric-model psychometric-model)
				     &key (repetition-limit 5))
  (with-slots (levels trials fractions) psychometric-model
    (when (< (minimum trials) repetition-limit)
      ;; Order everything according to trials
      (multiple-sort trials levels #'>)
      (multiple-sort trials fractions #'>)
      (sort trials #'>)

      ;; Cut short all the vectors to lop off the undesirable trials
      (let ((length (or (position repetition-limit trials :test #'>)
			(number-of-levels psychometric-model))))
	(setf trials (vectorize trials :size length))
	(setf levels (vectorize levels :size length))
	(setf fractions (vectorize fractions :size length)))

      ;; Re-order the data according to the stimulus levels
      (multiple-sort levels trials #'<)
      (multiple-sort levels fractions #'<)
      (sort levels #'<))
    psychometric-model))

;; Glom together PD objects, to create a larger object with more
;; trials and levels.  Combine levels that are the same.
(defmethod pool ((psychometric-model psychometric-model) &rest pd-list)
  (let* ((list (cons psychometric-model pd-list))
	 (result (copy psychometric-model)))
    (when pd-list
      (let* ((trials (apply 'concatenate (type-of (trials psychometric-model))
			    (mapcar 'trials list)))
	     (levels (apply 'concatenate (type-of (levels psychometric-model))
			    (mapcar 'levels list)))
	     (fractions (apply 'concatenate (type-of (fractions psychometric-model))
			       (mapcar 'fractions list)))
	     (r-levels (sort (remove-duplicates levels) #'<))
	     (r-fractions (similar r-levels :initial-element 0.0))
	     (r-trials (similar r-levels :initial-element 0.0)))
	;; Use the original data to build a more concise form of data
	;; where trials that are the same get combined.
	(dotimes (i (length r-levels))
	  (dotimes (j (length levels))
	    (when (= (aref r-levels i) (aref levels j))
	      (incf (aref r-trials i) (aref trials j))
	      (incf (aref r-fractions i) (* (aref trials j) (aref fractions j))))))
	(div r-fractions r-trials :-> r-fractions)
	(with-slots (trials levels fractions
			    parametric-function
			    inverse-parametric-function) result
	  (setf trials r-trials)
	  (setf levels r-levels)
	  (setf fractions r-fractions)
	  (setf inverse-parametric-function (inverse-parametric-function psychometric-model))
	  (setf parametric-function (parametric-function psychometric-model)))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Doing maximum likelihood fitting of psychometric functions
;;; Note that the parameters slot will get filled at each iteration.

(defmethod maximum-likelihood-fit ((model psychometric-model)
				   &key
				   (lower-bounds (lower-bounds model))
				   (upper-bounds (upper-bounds model)))
  (with-slots (parameters parametric-function levels) model
    (cond  ((<= (length levels) 1)
	    (warn "Too few levels for fitting in ~a" model))
	   (t
	    (unless parameters
	      (setf parameters (make-list (- (length (arglist parametric-function)) 1)
					  :initial-element 1.0)))
	    (unless lower-bounds (setq lower-bounds (fill! (similar parameters) -1e8)))
	    (unless upper-bounds (setq upper-bounds (fill! (similar parameters) 1e8)))
	    
	    (stepit-fit #'maximum-likelihood-fit-error (make-matrix parameters) (list model)
			:lower-bounds (make-matrix lower-bounds)
			:upper-bounds (make-matrix upper-bounds)))))
  model)

;;; Error function for Maximum Likelihood fit of psychometric data
;;; Vector comes from fitting routine.
(defun maximum-likelihood-fit-error (vector psychometric-model)
  (with-slots (levels trials parameters fractions parametric-function) psychometric-model
    (dotimes (i (length parameters))
      (setf (nth i parameters) (aref vector i)))
    (- (loop for index from 0 below (number-of-levels psychometric-model)
             for ai = (aref levels index) ; variable
             for ni = (aref trials index) ; number-of-trials
             for xi = (aref fractions index) ; percent-correct
             for Prob-i = (clip (apply parametric-function ai parameters) 1e-10 0.999999999)
             sum (* ni (+ (* xi (log Prob-i)) (* (- 1.0 xi) (log (- 1.0 Prob-i)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Likelihood ratio calculations and tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod log-likelihood ((model psychometric-model)
			   &key parametric (binomial (not parametric)))
  (assert (xor parametric binomial))
  (with-slots (levels trials fractions parametric-function parameters) model
    (loop for index from 0 below (length fractions)
	  for ai = (aref levels index) ; variable
	  for ni = (round (aref trials index)) ; number-of-trials
	  for xi = (aref fractions index) ; percent-correct
	  for ki = (round (* ni xi))
	  for Prob-i = (clip (if parametric
				 (apply parametric-function ai parameters) 
				 xi)
			     1e-10 0.999999999)
	  for log-binomial =  (+ (* ki (log Prob-i))
				 (* (- ni ki) (log (- 1.0 Prob-i)))
				 (log-binomial-coefficient ni ki))
	  sum log-binomial)))

(defmethod log-likelihood-ratio ((model psychometric-model))
  (- (log-likelihood model :binomial t) (log-likelihood model :parametric t)))

(defmethod degrees-of-freedom ((model psychometric-model))
  (- (length (levels model)) (length (parameters model))))

(defmethod likelihood-ratio-test ((model psychometric-model))
  (- 1 (cumulative-chi-square (* 2.0 (log-likelihood-ratio model))
			      (degrees-of-freedom model))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-psychometric-plot ((model psychometric-model)
				   &key (log t) (base 10.0)
				   (name (data-file model)) ->)
  (when -> (setq name ->))
  (with-slots (parametric-function parameters) model
    (let* ((levels (if log
		       (logarithm (levels model) base)
		       (levels model)))
	   (fractions (fractions model))
	   (operation #'(lambda(x) (apply parametric-function x parameters)))
	   (predictions (point-operation (levels model) operation))
	   )
      (make-viewable-sequence
       (list (make-scatter fractions levels)
	     (make-scatter predictions levels))
       :name name
       :display-type 'overlay))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
