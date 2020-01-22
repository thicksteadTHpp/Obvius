
(in-package 'user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This tutorial takes is an example of how to set up and run a
;;; psychophysics experiment, and anaylze/fit psychophysical data
;;; using Obvius.  This example is a "fake" spatial pattern detection
;;; experiment.  Rather than getting actual subject's responses, it
;;; uses a model to simulate a subject's responses.

;;; In a typical spatial pattern detection experiment, the contrast of
;;; a visual stimulus (called the target) is adjusted until it is just
;;; barely detectable.  In some experiments (called masking
;;; experiments), the target is superimposed on a background (called
;;; the masker).  Again, the contrast of the target is adjusted (while
;;; the masker contrast is held fixed) until the target is just barely
;;; detectable.  Typically, a target is harder to detect (i.e., a
;;; higher contrast is required) in the presence of a high contrast
;;; masker.

;;; The example in this tutorial is a contrast increment threshold
;;; experiment.  The mask is a sinusoidal grating (of a particular
;;; spatial-frequency).  The target is an identical pattern (same
;;; spatial-frequency).  The experiment measures the threshold
;;; contrast for detecting the target, e.g., the contrast required for
;;; the target to be just barely noticeable.

;;; We use a two alternative forced-choice (2AFC) task and a double
;;; random staircase procedure.  On each trial, the subject sees two
;;; stimuli (mask alone, mask plus target) in random order.  The
;;; subject's task is to pick the the stimulus that contained the
;;; target.  Since, in this particular masking experiment, the masker
;;; and target are identical spatial patterns, the task is simply to
;;; pick the stimulus that has the higher contrast.

;;; The target contrast (or contrast increment) is adjusted from one
;;; trial to the next depending on the subject's responses.  If the
;;; subject's response is incorrect, then we need to make the task
;;; easier, and the staircase procedure increases the target contrast
;;; on the next trial.  If the subject's response is correct twice in
;;; a row, then the staircase procedure decreases the target contrast.
;;; In this way, the staircase homes in on the "threshold" contrast,
;;; the contrast the yields 75%*** correct performance.

;;; The code in this file relies on a bunch of other stuff, highlights
;;; of which are the following functions: run-subject, staircase,
;;; load-all-psychometric-data, maximum-likelihood-fit,
;;; likelihood-ratio-test, check-in-gamut, and gamma-lut-correct.
;;; These are all defined in: <obv>/psychophysics.lisp,
;;; <obv>/psychophysics-analysis.lisp, and <obv>/gamma.lisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA FILES:

;;; The Obvius psychophysics code makes use of and creates a bunch of
;;; data files.  All of these files have the subject's name in them.
;;; You can set things up to run on a bunch of different subjects
;;; simultaneously.  This file is set up to run only on subjects named
;;; groucho and harpo.  Take a look at the function (defun groucho...)
;;; to see how to set up for another subject.

;;; The data files are:

;;; <subject>-todo contains a bunch of plists of stimulus conditions.
;;; On each trial, the first plist in the file is removed from the
;;; file, and it is used to run the staircase.  These plists should be
;;; a complete specification of the stimulus condition.  This plist
;;; are carried along through all steps of data analysis.  The
;;; experimenter must create and edit this file, adding new stimulus
;;; conditions as desired.  This is the ONLY data file that you should
;;; edit.  The others are automatically generated.  They are text
;;; files so it is easy to look at them, but be very careful about
;;; editing them since you might inadvertently screw something up.

;;; <subject>-done contains a bunch of plists of the stimulus
;;; conditions that have already been done.  Each of these plists is
;;; the same as the one that was in <subject>-todo, with additional
;;; information added.  The main additional info is the filename for a
;;; data-file that contains the raw responses to each stimulus
;;; presentation through the staircase.

;;; Data-files have filenames of the form <subject>-<date>-<number>.
;;; The first line of this file is a header, a plist that is the same
;;; as the one that was in the <subject>-todo file, with some
;;; additional information added (like the date that the data was
;;; generated).  After the header, there are a bunch of plists.  Each
;;; one specifies the particular stimulus that was presented on that
;;; trial of the staircase and the subject's response.

;;; <subject>-analyzed contains a bunch of plists that summarize all
;;; of the data-files.  For each stimulus condition, we fit the raw
;;; data with a parameter function (e.g., a cumulative normal).  The
;;; plists in the <subject>-analyzed file include infor about which
;;; parametric function was used and what the best fit parameters are.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FUNCTIONS FOR RUNNING THE EXPERIMENT:

;;; For each different experiment, you need to write a bunch of
;;; additional functions.  Examples of these functions are given below
;;; the bottom of this file.  For other (real) experiments you would
;;; have to replace the functions in this file with whatever you need
;;; for your experiment.  However, there are some conventions that you
;;; must adhere to.  Best policy is to use this file as a model for an
;;; experiment that you are putting together.  Here's a list of the
;;; main functions defined below for the contrast matching experiment
;;; and the most important conventions (i.e., what you must deal with
;;; when putting together a real experiment):

;;; groucho: This is the top-level function that a naive subject can
;;; type to get things going.  It calls "run-subject".

;;; masking-experiment: It initializes the display, creates the frame
;;; buffer image, creates the color map lookup tables corresponding to
;;; each stimulus condition for that staircase.  Along the way, it
;;; must decide which contrasts to use for each level of the
;;; staircase.  These contrasts are calculated by "get-contrasts".
;;; The "Masking-experiment" function (or its replacement in a real
;;; experiment that you put together) writes the header plist to the
;;; data file.

;;; masking-trial: This uses a simple model to simulate a subject's
;;; response.  In a real experiment, this would actually put up the
;;; stimuli and get a button press.  Regardless, this function (or its
;;; replacement in a real experiment) must write the stimulus
;;; condition (the :target-contrast in this example) and the response
;;; (t or nil) to the datafile.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; COLLECTING DATA:

#|
;;; Here, we simulate and analyze data for contrast matching
;;; experiment.  Running through this stuff will give a feel for the
;;; top-level interface to an experient set up in Obvius.  First, you
;;; will generate some simulated data.  Then analyze it: fitting the
;;; psychometric functions with parametric form, and picking off the
;;; 50% (match) point.

;;; Set path (change this to be somplace in your home directory):
(setq *path* "/usr/src/obvius/tutorials/psychophysics/data/")

;;; Copy the following stuff from this directory to *path*:
;;; gamma-lut-data, groucho-todo-orig, harpo-todo-orig.  Then copy
;;; groucho-todo-orig to groucho-todo.  And copy harpo-todo-orig to
;;; harpo-todo.

;;; Compile and load necessary code:
(progn
  (obv-require :psychophysics)
  (obv-require :statistics)
  (obv-require :conversion)
  (compile-load (merge-pathnames "tutorials/psychophysics/detection-tutorial"
				 obv::*obvius-directory-path*)))


;;; Load the gamma correction lookup table:
(progn
  (setq gamma-lut-data (matrix-transpose (read-ascii-matrix (merge-pathnames *path* "gamma-lut-data"))))
  (setq rfloats (displaced-row 0 gamma-lut-data))
  (setq gfloats (displaced-row 1 gamma-lut-data))
  (setq bfloats (displaced-row 2 gamma-lut-data))
  (setq 8bits (make-array 256 :element-type '(unsigned-byte 8)
			  :initial-contents (loop for i from 0 below 256 collect i)))
  (setq *gamma-lut* (make-gamma-lut gfloats 8bits)))

;;; Runs 1 simulated staircase.  Stores info about the raw data in file called:
;;; "groucho-done".
(groucho)

;;; *** From here on needs to be updated...

;;; Loads raw data for subject "groucho", constructs a psychometric-model object,
;;; and fits the data using a parametric function.  Stores info about the
;;; analyzed data in file called: "groucho-analyzed".
(setq psychometric-models
      (load-all-psychometric-data "groucho" *path*
				  :level-key :target-contrast
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
(groucho 7)

;;; Loads any previous analyzed data from "groucho-analyzed", then loads the new
;;; raw data and fits it.  Appends info about the newly analyzed data to the
;;; end of the file: "groucho-analyzed".
(setq psychometric-models
      (load-all-psychometric-data "groucho" *path*
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
      (load-all-psychometric-data "groucho" *path*
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
(harpo 8)
;;; Look at the results (as we did above for groucho).
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STATISTICS ON THE PSYCHOMETRIC FUNCTIONS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; THE MODEL (SIMULATING DATA):

#|
;;; Contrast increment threshold.  Mask is a sinusoidal grating
;;; (particular spatial-frequency).  Increase the contrast of this
;;; grating until the difference is just barely noticeable.

;;; Data is simulated using a (simple-minded) one-channel model.  We
;;; assume that the subject is monitoring the response of a single
;;; noisy channel (e.g., a single neuron or a collection of neurons
;;; with identical response properties).  This channel behaves like a
;;; linear operator (dot product between stimulus and receptive field
;;; weighting function) followed by a nonlinear transducer.

;;; Then noise is added to its response...
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FITTING THE DATA:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spatial Pattern Detection Experiment: Code for generating
;;; synthetic psychometric data, by running a simulated staircase.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *path*)
(defvar *gamma-lut*)

;;; Keywords to test for in the todo-file.  Error is signaled if any
;;; of them are missing.
(defvar *todo-keys*)
(setq *todo-keys* (list :mean-intensity
			:spatial-frequency :mask-contrast 
			:step-factor :number-of-trials :number-of-staircases))

(defun groucho (&optional (trials 1))
  (dotimes (trial trials)
    (print (run-subject "groucho"
			:experiment 'masking-experiment
			:directory *path*
			:keywords *todo-keys*))))

(defun harpo (&optional (trials 1))
  (dotimes (trial trials)
    (print (run-subject "harpo"
			:experiment 'masking-experiment
			:directory *path*
			:keywords *todo-keys*))))

(defun masking-experiment
    (&key
     mean-intensity
     spatial-frequency
     mask-contrast
     step-factor
     number-of-trials
     number-of-staircases
     (initial-step-size 2)
     (data-file "tmp"))

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

    (message "Staircase arguments")
    (setq fixed-args (make-list number-of-staircases
				:initial-element (list :mean-intensity mean-intensity
						       :spatial-frequency spatial-frequency
						       :mask-contrast mask-contrast
						       :data-file data-file)))
    (setq variable-args (make-list number-of-staircases
				   :initial-element
				   (loop for target-contrast in target-contrasts
					 collect (list :target-contrast target-contrast))))
	  
    (message "Write file header")
    ;; Write fixed-args along with date and any other keywords in the
    ;; todo file/*todo-keys*.
    (multiple-value-bind (second minute hour day month year) (get-decoded-time)
      (declare (ignore second minute hour))
      (with-open-file (stream data-file
			      :direction :output :if-exists :rename-and-delete)
	(format stream "~s~%" (append fixed-args
				      (list :step-factor step-factor
					    :number-of-trials number-of-trials
					    :number-of-staircases number-of-staircases
					    :day day
					    :month month
					    :year year)))))

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
	       :number-of-correct-before-reversal 2
	       :number-of-incorrect-before-reversal 1)

    (message "Clean Up")
    ;; Destroy/delete color map luts, destroy/delete frame buffer image,
    ;; uninitialize display device, etc.

    data-file))

  
;;; Note: contrast = (max-min)/2mean

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



;;; Computes responses to stimuli, using linear operator followed by
;;; nonlinear transducer.  Then adds noise (zero mean, unit variance).
;;; Task is to detect the stimulus with higher contrast.  Decision
;;; rule here is simply to pick the stimulus that yields greater
;;; response.  Since noise has been added, the simulated subject will
;;; sometimes be wrong...
(defun masking-trial
    (&key
     mean-intensity spatial-frequency 
     target-contrast mask-contrast
     trial data-file)
  (declare (ignore mean-intensity))
  (let* ((response-to-mask-alone (compute-single-channel-response
				  mask-contrast
				  spatial-frequency))
	 (response-to-mask-plus-target (compute-single-channel-response
					(+ mask-contrast target-contrast)
					spatial-frequency))
	 (noisy-response-to-mask (+ response-to-mask-alone
				    (gaussian-noise 0.0 1.0)))
	 (noisy-response-to-mask-plus-target (+ response-to-mask-plus-target
						(gaussian-noise 0.0 1.0)))
	 (response (> noisy-response-to-mask-plus-target noisy-response-to-mask)))
    (when data-file
      (with-open-file (stream data-file
			      :direction :output
			      :if-exists :append
			      :if-does-not-exist :create)
	(format stream "~s~%" (list :target-contrast target-contrast
				    :response response))))
    response))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Single channel model.  Linear operator followed by nonlinear
;;; transducer:

(defun compute-single-channel-response
    (contrast
     spatial-frequency 
     &key p q)
  (let ((linear-response (* contrast (dog-sensitivity spatial-frequency))))
    (nonlinear-transducer linear-response)))

(defun dog-sensitivity (sf
			&key
			(sigma1 16)
			(sigma2 1)
			(relative-sensitivity 1/4))
  (- (exp (- (/ (sqr sf) (* 2 (sqr sigma1)))))
     (* relative-sensitivity
	(exp (- (/ (sqr sf) (* 2 (sqr sigma2))))))))
			
#|
(let ((sigma1 16)
      (sigma2 1)
      (relative-sensitivity 1/4))
  (make-log-discrete-function
   #'(lambda (x)
       (dog-sensitivity x
			:sigma1 sigma1 :sigma2 sigma2
			:relative-sensitivity relative-sensitivity))
   1/4 64 :base 2))
(setp :y-range '(0 1) :y-tick-step 0.25)	  
|#

(defun nonlinear-transducer (x &key (a1 45) (a2 0.0075) (p 2.4))
  (* a1 (/ (expt x p) (+ (sqr x) (sqr a2)))))

#|
;;; Like Fig 8b in Legge & Foley (1980), JOSA, 70:1458-1470:
(make-log-discrete-function
 #'(lambda (x) (log (nonlinear-transducer x :a1 45 :a2 0.0075) 10))
 0.0005 0.5 :base 10)
(setp :y-range '(-3 2)
      :y-tick-step 1
      :x-range '(-3.3 -0.3)
      :x-tick-step 1
      aspect-ratio 1.5)
|#

(defun find-threshold-error (vector mask-contrast sf)
  (let* ((target-contrast (aref vector 0))
	 (response-to-mask-alone (compute-single-channel-response
				  mask-contrast sf))
	 (response-to-mask-plus-target (compute-single-channel-response
					(+ mask-contrast target-contrast)
					spatial-frequency)))
    (sqr (- (- response-to-mask-plus-target response-to-mask-alone) 1))))

#|
;;; Find threshold for a given sf and mask-contrast:
(let ((mask-contrast 0.0)
      
(stepit-fit #'find-threshold-error
	    (make-matrix mask-contrast)
	    (list mask-contrast sf)
	    :lower-bounds (make-matrix (make-list 1e-6))
	    :upper-bounds (make-matrix (make-list 1)))

;;; Model TvC at 2 c/deg:
(make-discrete-function
 #'(lambda (mask-contrast-db)
     (let* ((sf 2)
	    (mask-contrast (expt 10 (/ contrast-db 20)))
	    (threshold-contrast (stepit-fit #'find-threshold-error
					    (make-matrix mask-contrast)
					    (list mask-contrast sf)
					    :lower-bounds (make-matrix (make-list 1e-6))
					    :upper-bounds (make-matrix (make-list 1)))))
|#
