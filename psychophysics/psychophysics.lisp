;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General code for running psychophysics experiments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

(export '(run-subject staircase
	  unique-file-name subject-unique-file-name
	  test-subject-todo-file
	  message voice-message))

#|
ToDo:

Maximum-likelihood-fit relies on stepit and has stepit hardcoded.
Maybe make the fitting function a keyword.

Dump stepit and use gradient descent code (once EJ has cleaned it up).

Fix to stepit. Have it check for a (signed-byte 32) heap.  If its not
there, allocate a small one (100 elements should be enough).

Maybe add :id to plists in done and analyzed files.  Currently, everything is
identified wrt to data-file.  For some exps (e.g., method of adjustment instead
of staircases) there is no need for a separate raw data file.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run a staircase procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generate stimuli and collect responses in a staircase
;;; Run-Trial-Function is a function that computes and displays a
;;; stimulus, and returns a response, t or nil. Run-Trial-Function
;;; function must deal with any file writing.  Fixed-Args is a list of
;;; p-lists, one p-list for each simultaneous staircase, e.g.,
;;;   '((:spatial-period 64 :eta '(0.5 0.5 0.5))
;;;     (:spatial-period 64 :eta '(0.5 0.0 0.0)))
;;; Variable-Args is a list of lists of p-lists like this:
;;;   '(((:scale 1.3) (:scale 1.9))
;;;     ((:scale 2.0) (:scale 2.8)))
;;; Each innermost p-list corresponds to arguments for a particular
;;; level in a particular staircase.  Level-indices specifies the
;;; starting levels for each staircase.  Step-sizes is a list of
;;; starting step sizes for each staircase.  At the first turnaround
;;; of each staircase, the step for that staircase gets set to 1.

(defun staircase (run-trial-function
                  &key
                  fixed-args
                  variable-args
                  suppress-warning
                  verbose
                  (level-indices (make-list (length fixed-args) :initial-element 0))
                  (step-sizes (make-list (length level-indices) :initial-element 1))
                  (number-of-correct-before-reversal 1)
                  (number-of-incorrect-before-reversal 1)
                  (number-of-trials 64))

  (unless (= (length fixed-args) (length level-indices)
	     (length variable-args) (length step-sizes))
    (error "~a, ~a, ~a, and ~a have different lengths"
	   fixed-args level-indices variable-args step-sizes))

  (let* ((number-of-staircases (length fixed-args))
         (number-of-correct-in-a-row (make-list number-of-staircases :initial-element 0))
         (initial-level-indices (copy-list level-indices))
         (response nil)
         (staircase-list (loop for i from 0 below (* number-of-staircases number-of-trials)
                               collect (mod i number-of-staircases))))

    (when verbose (format t ";;; Trials: "))
    (loop for which-staircase in (shuffle staircase-list)
          for trial from 0
          for level-index = (max 0 (min (nth which-staircase level-indices)
					(1- (length (nth which-staircase variable-args)))))
          for initial-level-index = (nth which-staircase initial-level-indices)
          for args = (append (nth which-staircase fixed-args)
                             (nth level-index (nth which-staircase variable-args)))
          do

          (when verbose (format t "~A " trial))
          (setf (getf args :trial) trial)
          (unless (or suppress-warning (not verbose))
            (when (or (zerop level-index)
		      (= level-index (1- (length (nth which-staircase variable-args)))))
              (warn "level ~a is extreme~%" level-index)))

          ;; run trial and get response
          (setq response (apply run-trial-function args))

          ;; at the first turnaround, if the step size is large, set it to 1.
	  (when (and (or (and response (> level-index initial-level-index))
			 (and (not response) (< level-index initial-level-index)))
		     (> (nth which-staircase step-sizes) 1))
	    (setf (nth which-staircase step-sizes) 1))
		   
	  ;; update number-of-correct-in-a-row
	  (cond ((plusp (nth which-staircase number-of-correct-in-a-row))
		 ;; previous correct, if correct again incf number-of-correct
		 (if response
		     (incf (nth which-staircase number-of-correct-in-a-row))
		     (setf (nth which-staircase number-of-correct-in-a-row) 0)))
		((minusp (nth which-staircase number-of-correct-in-a-row))
		 ;; previous incorrect, if incorrect again decf number-of-correct
		 (if response
		     (setf (nth which-staircase number-of-correct-in-a-row) 0)
		     (decf (nth which-staircase number-of-correct-in-a-row))))
		((zerop (nth which-staircase number-of-correct-in-a-row))
		 ;; previous reset
		 (if response
		     (incf (nth which-staircase number-of-correct-in-a-row))
		     (decf (nth which-staircase number-of-correct-in-a-row)))))
	  
	  ;; if incorrect n-times in a row, increase the level-index
	  (when (= (- (nth which-staircase number-of-correct-in-a-row))
		   number-of-incorrect-before-reversal)
	    (incf (nth which-staircase level-indices) (nth which-staircase step-sizes))
	    (setf (nth which-staircase number-of-correct-in-a-row) 0))
	  
	  ;; if correct n-times in a row, decrease the level-index
	  (when (= (nth which-staircase number-of-correct-in-a-row)
		   number-of-correct-before-reversal)
	    (decf (nth which-staircase level-indices) (nth which-staircase step-sizes))
	    (setf (nth which-staircase number-of-correct-in-a-row) 0))
	  
	  ))
  (when verbose (format t "~%"))
  level-indices)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choose a sensible, unique data file name for a subject in the directory specified
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There are two ways of doing this.
;; One file at a time is faster for directories that are very full
;; (for example, subject data directories)
(defun unique-file-name (path &key extension individual)
  (if individual
      (loop for number from 0
	    for file-name = (concatenate 'string (format nil "~A-~A" path number) extension)
	    while (probe-file file-name)
	    finally return file-name)
      (loop with basename = (file-namestring path)
	    with directory = (directory-namestring path)
	    with files = (mapcar 'file-namestring (directory directory))
	    for number from 0
	    for file-name = (concatenate 'string basename (format nil "-~A" number) extension)
	    while (find file-name files :test #'string=)
	    finally return (concatenate 'string directory file-name))))

(defun subject-unique-file-name (subject-name &optional (directory "/tmp"))
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (unique-file-name (merge-pathnames
		       directory
		       (format nil "~a-~a-~a-~a" subject-name month day year))
		      :individual t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test a subject's todo file for consistency
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Check that each todo plist in the subjects todo file has the
;;; requisite keywords.  You pass those keywords as a list.
(defun test-subject-todo-file (subject-name &key keywords (directory "/tmp"))
  (let ((todo-file (merge-pathnames
		      directory (format nil "~a-todo" subject-name))))
    (with-open-file (stream todo-file :direction :input :if-does-not-exist :error)
      (let ((pending-trials
             (loop for todo-plist = (read stream nil nil)
                   for i from 0
                   while todo-plist
                   do
                   (unless (listp todo-plist)
                     (error "Not a plist: entry ~a" i))
                   (loop for keyword in keywords do
                         (unless (getf todo-plist keyword)
                           (error "Missing keywords: entry ~a" i)))
                   count (not (getf todo-plist :file-name)))))
        pending-trials))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Automatically fire up an experiment, given the subjects name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *psychophysics-busted* nil)

(defun run-subject (subject-name &rest other-args &key
                                 (directory (error "Must specify directory"))
                                 (experiment (error "Must specify experiment"))
                                 keywords &allow-other-keys)

  ;; Set global so that if it busts, you have to do something manually
  ;; to fix things.  This global gets reset to nil at the end of this function.
  (when *psychophysics-busted*
    (error "The experiment is busted.  Get help!"))
  (setq *psychophysics-busted* t)

  (remf other-args :directory)
  (remf other-args :experiment)
  (remf other-args :keywords)

  (let* ((todo-file (merge-pathnames
		       directory (format nil "~a-todo" subject-name)))
	 (todo-bak (merge-pathnames
		      directory (format nil "~a-todo-backup" subject-name)))
         (done-file (merge-pathnames
		     directory (format nil "~a-done" subject-name)))
         (data-file (merge-pathnames
		     directory (subject-unique-file-name subject-name directory)))
	 todo-plist)

    ;; Make sure the todo is OK.
    (when keywords
      (test-subject-todo-file subject-name :keywords keywords :directory directory))

    ;; Make backup copy of todo file.
    (lcl::run-program "cp" :arguments (list (namestring todo-file) (namestring todo-bak)))

    ;; Read in the todo one by one.  Write them all out
    ;; except for the first one (that is the one we will use).
    (with-open-file (in-stream todo-bak
			       :direction :input
			       :if-does-not-exist :error)
      (with-open-file (out-stream todo-file
				  :direction :output
				  :if-exists :rename-and-delete)
	(setq todo-plist (read in-stream nil nil))
	(loop for tmp-todo-plist = (read in-stream nil nil)
              while tmp-todo-plist
              do
	      (format out-stream "~s~%" tmp-todo-plist))))
    
    (unless todo-plist (message "No more trials for ~a today."
				 (string-upcase subject-name)))
    (when todo-plist
      ;; Tack on a file-name.
      (setf (getf other-args :data-file) (namestring data-file))
      ;; Run an experiment with this todo, and update accordingly.
      (apply experiment (append todo-plist other-args))
      ;; Write plist to done file.
      (setf todo-plist (append todo-plist (list :data-file (file-namestring data-file))))
      (with-open-file (out-stream done-file
				  :direction :output
				  :if-does-not-exist :create
				  :if-exists :append)
        (format out-stream "~s~%" todo-plist)))

    (setq *psychophysics-busted* nil)
    todo-plist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Message and voice-message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun message (control-string &rest args)
  (apply 'format t (concatenate 'string ";;; " control-string "~%") args))

;;; Runs the appropriate file for a voice message, and directs output
;;; to /dev/audio.
(defun voice-message (message)
  (lcl::run-program "cp" :arguments (list message "/dev/audio")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
