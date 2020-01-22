
(in-package 'user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is an example of how to set up a psychophysics experiment using
;;; Obvius.  The code in this file relies on a bunch of other stuff, highlights
;;; of which are the following functions: run-subject, staircase,
;;; load-all-psychometric-data, maximum-likelihood-fit, likelihood-ratio-test,
;;; check-in-gamut, and gamma-lut-correct.  These are all defined in:
;;; psychophysics.lisp, psychophysics-analysis.lisp, and gamma.lisp.

;;; This code makes use of and creates a bunch of data files.  All of these
;;; files have the subject's name in them.  You can set things up to run on a
;;; bunch of different subjects simultaneously.  This file is set up to run
;;; only on subject named "moe".  Take a look at the function (defun moe...) to
;;; see how to set up for another subject.

;;; The data files are:

;;; <subject>-todo contains a bunch of plists of stimulus conditions.  On each
;;; trial, the first plist in the file is removed from the file, and it is used
;;; to run the staircase.  These plists should be a complete specification of
;;; the stimulus condition.  This plist are carried along through all steps of
;;; data analysis.  The experimenter must create and edit this file, adding new
;;; stimulus conditions as desired.  This is the ONLY data file that you should
;;; edit.  The others are automatically generated.  They are text files so it
;;; is easy to look at them, but be very careful about editing them since you
;;; might inadvertently screw something up.

;;; <subject>-done contains a bunch of plists of the stimulus conditions that
;;; have already been done.  Each of these plists is the same as the one that
;;; was in <subject>-todo, with additional information added.  The main
;;; additional info is the filename for a data-file that contains the raw
;;; responses to each stimulus presentation through the staircase.

;;; Data-files have filenames of the form <subject>-<date>-<number>.  The first
;;; line of this file is a header, a plist that is the same as the one that was
;;; in the <subject>-todo file, with some additional information added (like
;;; the date that the data was generated).  After the header, there are a bunch
;;; of plists.  Each one specifies the particular stimulus that was presented
;;; on that trial of the staircase and the subject's response.

;;; <subject>-analyzed contains a bunch of plists that summarize all of the
;;; data-files.  For each stimulus condition, we fit the raw data with a
;;; parameter function (e.g., a cumulative normal).  The plists in the
;;; <subject>-analyzed file include infor about which parametric function was
;;; used and what the best fit parameters are.

;;; For each different experiment, you need to write a bunch of additional
;;; functions.  An example of how to do this are given at the bottom of this
;;; file.  The example is a "fake" contrast matching experiment.  Rather than
;;; getting actual subject's responses, it uses bernoulli noise to simulate a
;;; subject's responses.  For other (real) experiments you would have to
;;; replace the functions in this file with whatever you need for your
;;; experiment.  However, there are some conventions that you must adhere to.
;;; Best policy is to use this file as a model for an experiment that you are
;;; putting together.  Here's a list of the main functions defined below for
;;; the contrast matching experiment and the most important conventions (i.e.,
;;; what you must deal with when putting together a real experiment):

;;; moe: This is the top-level function that a naive subject can type to get
;;; things going.  It calls "run-subject".

;;; matching-experiment: It initializes the display, creates the frame buffer
;;; image, creates the color map lookup tables corresponding to each stimulus
;;; condition for that staircase.  Along the way, it must decide which
;;; contrasts to use for each level of the staircase.  These contrasts are
;;; calculated by "get-match-contrasts".  This function (or its replacement in
;;; a real experiment that you put together) must and write the header plist to
;;; the data file.

;;; matching-trial: This uses "bernoulli-noise" to simulate a subject's
;;; response.  In a real experiment, this would actually put up the stimuli and
;;; get a butten press.  Regardless, this function (or its replacement in a
;;; real experiment) must write the stimulus condition (the :matching-contrast
;;; in this example) and the response (t or nil) to the datafile.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;; Here, we simulate and analyze data for contrast matching experiment.
;;; Running through this stuff will give a feel for the top-level interface to
;;; an experient set up in Obvius.  First, you will generate some simulated
;;; data.  Then analyze it: fitting the psychometric functions with parametric
;;; form, and picking off the 50% (match) point.

;;; Set path (change this to be somplace in your home directory):
(setq *path* "/usr/src/obvius/tutorials/psychophysics/data/")

;;; Copy the following stuff from this directory to *path*: gamma-lut-data,
;;; moe-todo-orig, curly-todo-orig.  Then copy moe-todo-orig to moe-todo.  And
;;; copy curly-todo-orig to curly-todo.

;;; Compile and load necessary code:
(progn
  (obv-require :psychophysics)
  (obv-require :conversion)
  (obv-require :statistics)
  (compile-load (merge-pathnames "tutorials/psychophysics/psychophysics-tutorial"
				 obv::*obvius-directory-path*)))

;;; load gamma lut
(progn
  (setq gamma-lut-data (matrix-transpose (read-ascii-matrix (merge-pathnames *path* "gamma-lut-data"))))
  (setq rfloats (displaced-row 0 gamma-lut-data))
  (setq gfloats (displaced-row 1 gamma-lut-data))
  (setq bfloats (displaced-row 2 gamma-lut-data))
  (setq 8bits (make-array 256 :element-type '(unsigned-byte 8)
			  :initial-contents (loop for i from 0 below 256 collect i)))
  (setq *gamma-lut* (make-gamma-lut gfloats 8bits)))

;;; Runs 1 simulated staircase.  Stores info about the raw data in file called:
;;; "moe-done".
(moe)

;;; Loads raw data for subject "moe", constructs a psychometric-model object,
;;; and fits the data using a parametric function.  Stores info about the
;;; analyzed data in file called: "moe-analyzed".
(setq psychometric-models
      (load-all-psychometric-data "moe" *path*
				  :level-key :match-contrast
				  :parametric-function 'detection-log-normal
				  :inverse-parametric-function 'inverse-detection-log-normal
				  :initial-parameters '(1.0 1.0)
				  :lower-bounds '(1e-8 1e-8)
				  :upper-bounds '(1e8 1e8)))
(describe (car psychometric-models))

;;; Let's view the staircase:
(make-image (staircase-levels (car psychometric-models)))

;;; Runs 7 more simulated staircases for a total of 8 datasets (4 with
;;; :test-contrast 0.1 and 4 with :test-contrast 0.2)
(moe 7)

;;; Loads any previous analyzed data from "moe-analyzed", then loads the new
;;; raw data and fits it.  Appends info about the newly analyzed data to the
;;; end of the file: "moe-analyzed".
(setq psychometric-models
      (load-all-psychometric-data "moe" *path*
				  :level-key :match-contrast
				  :parametric-function 'detection-log-normal
				  :inverse-parametric-function 'inverse-detection-log-normal
				  :initial-parameters '(1.0 1.0)
				  :lower-bounds '(1e-8 1e-8)
				  :upper-bounds '(1e8 1e8)))

;;; Display the psychometric function (data) overlayed with the
;;; predictions of the best-fitting model.
(loop for model in psychometric-models
      do
      (display (make-psychometric-plot model :log t :base 10))
      (setp :current-picture 0
	    :graph-type :point
	    :plot-symbol :circle)
      (setp :current-picture 1
	    :graph-type :line
	    :plot-symbol nil)
      (setp :y-range '(0 1) :y-tick-step 0.2
	    :x-range '(-1.5 0.0) :x-tick-step 0.5
	    :zoom 150))

;;; Often, it is a good idea to fit the data, ignoring levels that had only a
;;; few trials.  For example, a button press error at a very high or very low
;;; contrast will badly mess up the fit, because responses at those levels
;;; should be 1 and 0 respectively.  Here, we reanalyze the data using a
;;; repetition-limit of 5.  When :reanalyze is non-nil, the <subject>-analyzed
;;; gets moved to <subject>-analyzed-backup and the fits are redone from the
;;; raw data.  When :repetition-limit is non-nil, the function
;;; "remove-infrequent-levels" is called before fitting.
(setq psychometric-models
      (load-all-psychometric-data "moe" *path*
				  :reanalyze t
				  :level-key :match-contrast
				  :repetition-limit 5
				  :parametric-function 'detection-log-normal
				  :inverse-parametric-function 'inverse-detection-log-normal
				  :initial-parameters '(1.0 1.0)
				  :lower-bounds '(1e-8 1e-8)
				  :upper-bounds '(1e8 1e8)))

;;; Now redisplay the data as above.

;;; We can easily collect information about the various conditions.  For
;;; example, here we make a list of the parameter fits for all the
;;; :test-contrast=0.1 conditions.
(mapcar #'(lambda (pm) (when (= (getf (header-plist pm) :test-contrast) 0.1)
			 (parameters pm)))
	psychometric-models)

;;; Here we read off the estimated 50% (match) point for each condition:
(mapcar #'(lambda (pm)
	    (apply (inverse-parametric-function pm) 0.5 (parameters pm)))
	psychometric-models)

;;; Here we make a scatter plot of match-contrast versus test-contrast
(let ((match-contrasts
       (mapcar #'(lambda (pm)
		   (apply (inverse-parametric-function pm) 0.5 (parameters pm)))
	       psychometric-models))
      (test-contrasts
       (mapcar #'(lambda (pm)
		   (getf (header-plist pm) :test-contrast))
	       psychometric-models)))
  (scatter-plot (make-matrix match-contrasts) (make-matrix test-contrasts)))
(setp :y-range '(0.05 0.25) :y-tick-step 0.05
      :x-range '(0.05 0.25) :x-tick-step 0.05
      :zoom 1000)

;;; Now run a bunch of simulated staircases on another subject:
(curly 8)
;;; Look at the results (as we did above for moe).
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;; Statistics: likelihood ratio test is used to test if the
;;; parametric-function is appropriate.

(setq model (car psychometric-models))
(progn
  (display (make-psychometric-plot model :log t :base 10))
  (setp :current-picture 0
	:graph-type :point
	:plot-symbol :circle)
  (setp :current-picture 1
	:graph-type :line
	:plot-symbol nil)
  (setp :y-range '(0 1) :y-tick-step 0.2
	:x-range '(-1.25 -0.75) :x-tick-step 0.5
	:zoom 400))

;;; We have two ways to think about the data in a psychometric function.  The
;;; first is to treat each data point as the result of a bernoulli random
;;; process with its own individual probability.  We'll call this the
;;; "independent bernoulli model".  According to this model, the probability of
;;; getting data exactly equal to the observed data is given by the binomial
;;; formula.  Of course, we don't know the actual underlying bernoulli
;;; probability so we use the observed frequency as an estimate.  For one data
;;; point, we calculate the probabilities this way:

(progn
  (setq p (aref (fractions model) 0))
  (setq n (round (aref (trials model) 0)))
  (setq k (round (* p n)))
  (binomial k p n))

;;; The probability of getting all the data taken together is the product of
;;; the individual probabilities:

(setq p1 (apply #'* (loop for i from 0 below (length (fractions model))
			  for x = (aref (fractions model) i)
			  for n = (round (aref (trials model) i))
			  for k = (round (* x n))
			  for p = x
			  collect (binomial k p n))))

;;; The second way to think about the data is to consider all of the data
;;; points at once, using the parametric function to give the estimate
;;; underlying bernoulli probabilities.  We'll call this the "parametric
;;; function model".  Here's how to calculate the probability of getting
;;; exactly the observed data according to this model:

(setq p2 (apply #'* (loop for i from 0 below (length (fractions model))
			  for x = (aref (fractions model) i)
			  for n = (round (aref (trials model) i))
			  for k = (round (* x n))
			  for contrast = (aref (levels model) i)
			  for p = (apply (parametric-function model)
					 contrast
					 (parameters model))
			  collect (binomial k p n))))

;;; The observed data is less likely using this second model because we are
;;; trying to explain the data with fewer parameters.  Is this difference
;;; significant?  To answer this question we can use a likelihood ratio test.
;;; It turns out that the log of the ratio of the likelihoods is distributed as
;;; a chi-square random variable, i.e., the following is a chi-square random
;;; variable.

(setq X^2 (* 2 (log (/ p1 p2))))

;;; So we compare the "parametric function model" to the "independent bernoulli
;;; model" by looking up the tail probability of the chi-square distribution:

(setq p-value (- 1 (cumulative-chi-square X^2 (degrees-of-freedom model))))

;;; If this p-value is relatively large (e.g., bigger than 0.05) then we
;;; conclude that the difference between the two models is not significant.
;;; All of the above is bundled up in the following function:

(likelihood-ratio-test model)

;;; Let's apply the likelihood-ratio-test to all of the data.  Most of the
;;; p-values should be large, but a few of them will be small (just by chance).

(setq p-values (sort (mapcar #'likelihood-ratio-test psychometric-models) #'>))

;;; But why is the log-likelihood-ratio a chi-square random variable?  Rather
;;; than prove this result, we will do a simulation to convince you that it is
;;; correct.

;;; Setup a dummy psychometric model object:
(setq actual-parameters '(0.1 3.0))
(setq model (make-instance 'psychometric-model
			   :data-file nil
			   :header-plist nil
			   :staircase-levels nil
			   :staircase-responses nil
			   :levels (make-matrix '(0.06 0.072 0.0864 0.10368 0.124416))
			   :trials (make-matrix '(10 10 10 10 10))
			   :fractions (make-matrix '(0 0 0 0 0))
			   :parametric-function #'detection-log-normal
			   :inverse-parametric-function #'inverse-detection-log-normal
			   :parameters (copy-list actual-parameters)
			   :upper-bounds '(1e8 1e8)
			   :lower-bounds '(1e-8 1e-8)))

;;; Do a bunch of simulations and plot the log-likelihood-ratios.
(setq ratios
      (make-image
       (make-matrix
	(loop for i from 0 below 100 do
	      (status-message "~a..." i)
	      (loop for j from 0 below (length (levels model))
		    for level = (aref (levels model) j)
		    for trials = (aref (trials model) j)
		    for p = (apply (parametric-function model)
				   level
				   actual-parameters)
		    do
		    (setf (aref (fractions model) j) (/ (binomial-noise p trials)
							trials)))
	      (maximum-likelihood-fit model)
	      collect (* 2 (log-likelihood-ratio model))))))

;;; Plot frequency-distribution of the simulated ratios and overlay the
;;; chi-square density.
(let ((histogram (make-histogram ratios :bincenter 0.0 :binsize 0.5 :range '(0.0 10.0)))
      (distribution (make-discrete-function
		     #'(lambda (x) (chi-square x (degrees-of-freedom model)))
		     0.0 10.5 :size 22)))
  (div histogram (integral histogram) :-> histogram)
  (display (make-viewable-sequence (list distribution histogram)) 'overlay)
  (setp :current-picture 1 :fill-symbol-p nil))

;;; Plot cumulative distributions, of the real data and of the chi-square
(progn
  (setq ratio-vector (data ratios))
  (sort ratio-vector #'<)
  (setq cumulative-histogram
	(div (make-discrete-function
	      #'(lambda (x) (loop for i from 0 below (total-size ratios)
				  while (< (aref ratio-vector i) x)
				  finally return i))
	      0.0 10.0 :size 20)
	     (total-size ratios)))
  (setq cumulative-distribution
	(make-discrete-function
	 #'(lambda (x) (cumulative-chi-square x (degrees-of-freedom model)))
	 0.0 10.0 :size 20))
  (display (make-viewable-sequence (list cumulative-distribution cumulative-histogram))
	   'overlay))

;;; What percent of tests fail at each significance level?
(setq significances (point-operation ratios
				     #'(lambda (x) (cumulative-chi-square
						    x (degrees-of-freedom model)))
				     :binsize nil))
(loop for i from 0 below (total-size significances)
      sum (if (> (iref significances i) 0.99) 1 0))
(loop for i from 0 below (total-size significances)
      sum (if (> (iref significances i) 0.95) 1 0))
(loop for i from 0 below (total-size significances)
      sum (if (> (iref significances i) 0.90) 1 0))
(loop for i from 0 below (total-size significances)
      sum (if (> (iref significances i) 0.85) 1 0))
(loop for i from 0 below (total-size significances)
      sum (if (> (iref significances i) 0.80) 1 0))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Contrast Matching Experiment: Code for generating synthetic psychometric
;;; data, by running a simulated staircase.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *path*)
(defvar *gamma-lut*)

;;; The argument :keywords is a list of keys to check for in the
;;; todo file.  Error is signaled if any of those are missing.
(defun moe (&optional (trials 1))
  (dotimes (trial trials)
    (print (run-subject "moe"
			:experiment 'matching-experiment
			:directory *path*
			:keywords '(:mean-intensity :test-contrast)))))

(defun curly (&optional (trials 1))
  (dotimes (trial trials)
    (print (run-subject "curly"
			:experiment 'matching-experiment
			:directory *path*
			:keywords '(:mean-intensity :test-contrast)))))


(defun matching-experiment
    (&key
     (mean-intensity 0.5)
     (test-contrast 0.1)
     (number-of-staircases 2)
     (number-of-trials 48)
     (initial-step-size 2)
     (step-factor (expt 2.0 0.25))
     (data-file "tmp")
     comment)

  (let (fixed-args variable-args match-contrasts initial-level)
  
    ;; Decide which levels (e.g., match contrasts) to use.  This is hard
    ;; because it depends on the gamut of the monitor.
    (setq match-contrasts (get-contrasts :mean-intensity mean-intensity
					 :step-factor step-factor))

    ;; Choose initial-level
    (setq initial-level (loop for match-contrast in match-contrasts
			      for level from 0
			      until (> match-contrast test-contrast)
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
				    :test-contrast test-contrast
				    :day day
				    :month month
				    :year year
				    :comment comment))))

    (message "Staircase arguments")
    (setq fixed-args (make-list number-of-staircases
				:initial-element (list :mean-intensity mean-intensity
						       :test-contrast test-contrast
						       :data-file data-file)))
    (setq variable-args (make-list number-of-staircases
				   :initial-element
				   (loop for match-contrast in match-contrasts
					 collect (list :match-contrast match-contrast))))
	  
    (message "Staircase")
    (staircase 'matching-trial
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
    ;; Destroy/delete color map luts, destroy/delete frame buffer
    ;; image, uninitialize display device, etc.

    data-file))

;;; contrast = (max-min)/2mean

;;; Starts with maximum-modulation.  Then divides that by step-factor to get
;;; successive contrasts until you run out of precision in the frame buffer
;;; (i.e., the actual frame buffer values end up all being equal to the mean).
(defun get-contrasts (&key (mean-intensity 0.5)
			   max-contrast
			   (step-factor (expt 2.0 0.25))
			   (gamma-lut *gamma-lut*))
  (check-in-gamut mean-intensity gamma-lut)
  (unless max-contrast
    (multiple-value-bind (domain min-intensity max-intensity) (domain gamma-lut)
      (declare (ignore domain))
      (setq max-contrast 
	    (/ (min (- mean-intensity min-intensity)
		    (- max-intensity mean-intensity))
	       mean-intensity))))
  ;;(print-db max-contrast)
  (reverse (loop with mean-gun = (gamma-lut-correct mean-intensity gamma-lut)
		 for level-index from 0
		 for match-contrast = (* max-contrast (expt step-factor (- level-index)))
		 for max-intensity = (+ mean-intensity (* match-contrast mean-intensity))
		 for min-intensity = (- mean-intensity (* match-contrast mean-intensity))
		 for max-gun = (gamma-lut-correct max-intensity gamma-lut)
		 for min-gun = (gamma-lut-correct min-intensity gamma-lut)
		 until (= mean-gun max-gun min-gun)
		 collect match-contrast)))

(defun matching-trial
    (&key mean-intensity test-contrast match-contrast trial data-file
	  (parametric-function #'detection-log-normal)
	  (slope 2)
	  (parameters (list test-contrast slope)))
  (declare (ignore mean-intensity))
  (let ((response (plusp (bernoulli-noise (apply parametric-function match-contrast
						 parameters)))))
    (when data-file
      (with-open-file (stream data-file
			      :direction :output
			      :if-exists :append
			      :if-does-not-exist :create)
	(format stream "~s~%" (list :match-contrast match-contrast
				    :response response))))
    response))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
