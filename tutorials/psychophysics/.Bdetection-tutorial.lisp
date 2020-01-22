
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spatial Pattern Detection Experiment: Code for generating
;;; synthetic psychometric data, by running a simulated staircase.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun groucho (&optional (trials 1))
  (dotimes (trial trials)
    (print (run-subject "groucho"
			:experiment 'masking-experiment
			:directory *path*
			:keywords '(:mean-intensity :mask-contrast)))))

(defun harpo (&optional (trials 1))
  (dotimes (trial trials)
    (print (run-subject "harpo"
			:experiment 'masking-experiment
			:directory *path*
			:keywords '(:mean-intensity :mask-contrast)))))

(defun masking-experiment
    (&key
     (mean-intensity 0.5)
     (mask-contrast 0.0)
     (number-of-staircases 2)
     (number-of-trials 48)
     (initial-step-size 2)
     (step-factor (expt 2.0 0.25))
     (data-file "tmp")
     comment)

  (let (fixed-args variable-args target-contrasts initial-level)
  
    ;; Decide which levels (e.g., target contrasts) to use.  This is hard
    ;; because it depends on the gamut of the monitor.
    (setq target-contrasts (get-contrasts :mean-intensity mean-intensity
					  :step-factor step-factor))

    ;; Choose initial-level (close to mask-contrast)
    (setq initial-level (loop for target-contrast in target-contrasts
			      for level from 0
			      until (> target-contrast mask-contrast)
			      finally return level))
  
    (message "Initialize")
    ;; Initialize display device, etc.

    (message "Frame buffer")
    ;; Make frame buffer image and write it to frame buffer.
    
    (message "Color map luts")
    ;; Create luts corresponding to each level (dont forget to gamma correct),
    ;; and either: (1) pass them along or (2) write them to files (and pass
    ;; along where to find them).

    (message "Write file header")
    (multiple-value-bind (second minute hour day month year) (get-decoded-time)
      (declare (ignore second minute hour))
      (with-open-file (stream data-file
			      :direction :output :if-exists :rename-and-delete)
	(format stream "~s~%" (list :mean-intensity mean-intensity
				    :mask-contrast mask-contrast
				    :day day
				    :month month
				    :year year
				    :comment comment))))

    (message "Staircase arguments")
    (setq fixed-args (make-list number-of-staircases
				:initial-element (list :mean-intensity mean-intensity
						       :mask-contrast mask-contrast
						       :data-file data-file)))
    (setq variable-args (make-list number-of-staircases
				   :initial-element
				   (loop for target-contrast in target-contrasts
					 collect (list :target-contrast target-contrast))))
	  
    (message "Staircase")
    (staircase 'masking-trial
	       :fixed-args fixed-args
	       :variable-args variable-args
	       :step-sizes (list-of-length number-of-staircases initial-step-size)
	       :level-indices (loop for staircase from 0 below number-of-staircases
				    collect (if (evenp staircase)
						(+ initial-level initial-step-size)
						(- initial-level initial-step-size)))
	       :number-of-trials number-of-trials
	       :number-of-correct-before-reversal 1
	       :number-of-incorrect-before-reversal 1)

    (message "Clean Up")
    ;; Destroy/delete color map luts, destroy/delete frame buffer image,
    ;; uninitialize display device, etc.

    data-file))

;;; *** not done
(defun masking-trial
    (&key mean-intensity mask-contrast target-contrast trial data-file)
  (declare (ignore mean-intensity))
  (let* ((response-to-mask (plusp (bernoulli-noise (apply parametric-function target-contrast
						 parameters)))))
    (when data-file
      (with-open-file (stream data-file
			      :direction :output
			      :if-exists :append
			      :if-does-not-exist :create)
	(format stream "~s~%" (list :target-contrast target-contrast
				    :response response))))
    response))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
