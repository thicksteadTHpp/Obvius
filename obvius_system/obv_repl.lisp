;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: repl.lisp
;;;  Author: Simoncelli, [THO]
;;;  Description: Multi-processing, read-eval-print loop
;;;  Creation Date:
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adapted for use in SBCL by [THO] in 2019
;; there are some of the definitions from lucid-repl.lisp in here
;; 
(in-package :obv)

(export '(status-message  with-status-message without-status-messages))

;;;; Enqueue and Handle Mouse Events

;;; Certain simple mouse events (e.g.  cycle-pane, select-pane, etc)
;;; are handled asynchronously by the mouse process.  All others are
;;; pushed onto a queue for delayed evaluation in the top-level
;;; Read-Eval-Print-Loop (repl) process.  This allows us to avoid
;;; global data access collisions.

;;; Evaluate all queued (mouse) event forms.  This is called by repl.
;;; [THO] 2019 This does not only handle mouse events but all queued events
;;; this will be called when a viewable is printed to the top level
(defun handle-queued-events ()
  (loop for val = (pop-from-eval-queue)
	while val
	do (eval val)))

;;; Push-onto-eval-queue puts a form or function object on the list of
;;; things to be evaluated by the lisp process.  Pop-from-eval-queue
;;; gets the next thing off the queue.  Eval-queue-p is a predicate
;;; which returns t if there is anything on the queue (nil otherwise).
;;; We use a lexical closure to protect the queue, and we inhibit
;;; scheduling to avoid asynchronous queue modification collisions.
(let (the-eval-queue)
  (defun push-onto-eval-queue (form &key silent)
    (unless (or silent (top-level-p))
      (let ((*print-length* 2) (*print-level* 2))
	(status-message "Enqueuing form: ~A" form)))
    (with-scheduling-inhibited (push form the-eval-queue)))
  (defun eval-queue-length ()
    (length the-eval-queue))
  (defun pop-from-eval-queue ()
    (with-scheduling-inhibited 
	(let ((next-item (car (last the-eval-queue))))
	  (setq the-eval-queue (nbutlast the-eval-queue))
	  next-item)))
  (defun eval-queue-p ()  ;return t if anything on the queue, nil otherwise
    (if the-eval-queue t nil)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Top-level loop (handles standard-input stream and enqueued mouse events).

;;; *** *prompt* should be defined by lucid somewhere.  For now, we
;;; define it here.
(defvar *prompt* "> ")

(defvar *top-level-p* nil)

;;; Escape hatch for debugging repl!
(defvar *quit-repl* nil)

;;; This routine tells whether the lisp repl is waiting for input at
;;; the top level. May be system dependent.  *Top-level-p* gets set in
;;; repl.
(defun top-level-p ()
  (declare (special *top-level-p*))
  *top-level-p*)

;;; Main process Read-Eval-Print-Loop
;; (defun repl ()
;;   (declare (special lisp:- *top-level-p* *status-reporter*))
;;   (let (val *quit-repl*)  (declare (special *quit-repl*))
;;     (tagbody start-repl
;;        (print-top-level-values nil)	;output prompt and status-message.
;;        (unwind-protect 
;; 	    (loop			
;; 		(setq *top-level-p* t)
;; 	      (when *status-reporter* (status-message "Top Level"))
;; 	      (LCL:process-wait
;; 	       "Awaiting keyboard or mouse input"
;; 	       #'(lambda ()
;; 		   (or (eval-queue-p)
;; 		       (listen-ignoring-whitespace lisp:*standard-input*))))
;; 	      (setq *top-level-p* nil)
;; 	      (when *status-reporter* (status-message "computing ..."))
;; 	      (cond ((eval-queue-p)
;; 		     (handle-queued-mouse-events))
;; 		    ((listen-ignoring-whitespace lisp:*standard-input*)
;; 		     (setq lisp:- (read lisp:*standard-input*))
;; 		     (setq val (multiple-value-list (eval lisp:-)))
;; 		     (set-top-level/ val)
;; 		     (when val  (set-top-level* (car val)))
;; 		     (print-top-level-values val))))
;; 	 (if (or LCL:*quitting-lisp* *quit-repl*)
;; 	     (format t "[RIP]~%")
;; 	     (go start-repl))))))

;;; Print list of values (as multiple-value return) for top-level
;;; output, printing prompt at completion.  We capture this behavior
;;; here so that it can be called from the top level loop AND from the
;;; eval-queue.  If the values are viewables and
;;; *auto-display-viewables* is non-nil, then display them.
(defun print-top-level-values (values)
  (declare (special *auto-display-viewables*))
  (if values
      (loop for val in values
	    for i from (1- (list-length values)) by -1
	    do
	    (format *standard-output* "~S~%" val)
	    (when (and *auto-display-viewables* (viewable-p val))
	      (status-message "displaying ...")
	      (display val))
	    (when (= i 0) (format *standard-output* "~A" *prompt*)))
      (format *standard-output* "~%~A" *prompt*)))



;;; Whitespace is defined on page 336 of Steele (Common Lisp).
;; (defun listen-ignoring-whitespace (&optional (stream lisp:*standard-input*))
;;   "Read whitespace characters from stream until no more characters are
;; available OR a non-whitespace character is found.  Returns nil if only
;; characters are found, t otherwise."
;;   (loop for char = (read-char-no-hang stream nil nil) ;Return nil on eof
;; 	until (or (null char)		;no input available from stream
;; 		  (and (char/= char #\space)
;; 		       (char/= char #\newline)
;; 		       (char/= char #\return)
;; 		       (char/= char #\linefeed)
;; 		       (char/= char #\tab)
;; 		       (char/= char #\page)))
;; 	finally (return (when char (unread-char char stream) t))))

;;; Holds the value of *debugger-hook* when this file is loaded.
;; (defvar *original-debugger-hook*)
;; (eval-when (load)
;;   (when (not (boundp '*original-debugger-hook*))
;;     (setq *original-debugger-hook* *debugger-hook*)))

;; ;;; Indicate to user that top-level process is in the debugger.
;; (defvar *obvius-debugger-hook*
;;   #'(lambda ()
;;       (declare (special *status-reporter*))
;;       (when *status-reporter* (status-message " in debugger ..."))))

;; ;;; Reset the debugger hook to call the *obvius-debugger-hook* and
;; ;;; then the *original-debugger-hook*.
;; (setq *debugger-hook*
;;       #'(lambda (cond func)
;; 	  (declare (special *obvius-debugger-hook* *original-debugger-hook*))
;; 	  (funcall *obvius-debugger-hook*) ;no args
;; 	  (when *original-debugger-hook*
;; 	    (funcall *original-debugger-hook* cond func))))

;;; See pages 325-326 of Steele.  These variables must be bound by the
;;; toplevel loop.
;; (defun set-top-level+ (form)
;;   (declare (special lisp:+++ lisp:++ lisp:+))
;;   (setq lisp:+++ lisp:++  lisp:++ lisp:+  lisp:+ form))

;; (defun set-top-level* (val)
;;   (declare (special lisp:*** lisp:** lisp:*))
;;   (setq lisp:*** lisp:**  lisp:** lisp:*  lisp:* val))

;; (defun set-top-level/ (val-list)
;;   (declare (special lisp:/// lisp:// lisp:/))
;;   (setq lisp:/// lisp://  lisp:// lisp:/  lisp:/ val-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Status message stuff

;;; Global switch to turn off messages.
(defvar *status-messages* t
  "Switch to turn off the status-message line.  Use the macro without-status-messages
to temporarily turn it off.")
(eval-when (load eval) (setf (get '*status-messages* :type) '(member nil t)))

(defmacro without-status-messages (&rest body)
  `(let ((*status-messages* nil))
    (declare (special *status-messages*))
    ,@body))

;;; Print "<msg> ..." at start, and "<msg> ... done." at end
;;; msg can be a string, or a list or a string and format-args
;;; *** Conses more than it should.
;; (defmacro with-status-message (msg &rest body)
;;   (let ((str (gensym)))
;;     `(let* ((,str (concatenate 'string ,msg "... ")))
;;        (prog1
;; 	   (progn (status-message ,str) ,@body)
;; 	 (status-message (concatenate 'string ,str "done") )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;status message 
;;; adapted by [tho] 2016-08-18
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Print "<msg> ..." at start, and "<msg> ... done." at end
;;; msg can be a string, or a list or a string and format-args
;;; *** Conses more than it should.
(defmacro with-status-message (msg &rest body)
;;  (vom:info "-[STATuS MESSAGE]-")
  (let ((str (gensym)))
    `(let* ((,str (concatenate 'string "-[STATuS MESSAGE]- " ,msg "... ")))
       (prog1
	   (progn (vom:info "~a" ,str) ,@body)

	 (vom:info "~a" (concatenate 'string  ,str " ... done") )))))


;;; This will be reset to t by emacs if Lisp is running in a
;;; cl-shell-mode buffer.
;;(defvar user::*emacs-cl-shell* nil)

;;; These control I/O of messages, passive mouse documentation and
;;; user selection of viewables with the mouse. They can be lists
;;; (stacks), or just single objects.
(defvar *status-reporter* nil)		;typically a status message line
(defvar *mouse-doc-reporter* nil)	;also typically a status message line
(defvar *selection-receiver* nil)	;typically a dialog box

;;; If *obvius-status-window* is non-nil, send the string to the
;;; status line.  else if *emacs-cl-shell* is non-nil, send the string
;;; to emacs to be put in the minibuffer.  Otherwise, print it on the
;;; *trace-output* stream.
(defun status-message (string &rest format-args)
  (cond ((null *status-messages*) nil)
	(*status-reporter*
	 (report (if (consp *status-reporter*)
		     (car *status-reporter*)
		     *status-reporter*)
		 (apply #'format nil string format-args)))
	;; (user::*emacs-cl-shell*
	;;  (format t "[[MESSAGE-STREAM>>")
	;;  (force-output)
	;;  (apply #'format t string format-args)
	;;  (force-output)
	;;  (format t "<<MESSAGE-STREAM]]")
	;;  (force-output))
	(t
	 (format *trace-output* ";;; ")    ;Print as comment.
	 (format t "[[MESSAGE-STREAM>>")
	 (apply #'format *trace-output* string format-args)
	 (format t "<<MESSAGE-STREAM]]")
	 (fresh-line *trace-output*)
	 (force-output *trace-output*)))) ;*** is this needed?

;;; Thingy for outputing an expression which will be evaluated.  If
;;; *emacs-cl-shell* is non-nil, assumes that the lisp process is
;;; running inside of emacs, and that the hacks in the file
;;; cl-shell.el have been loaded to allow the process to insert text
;;; at the current location of the point (cursor) for evaluation.
;;; Otherwise, prints the sexpr in a comment.
;; (defun insert-sexpr-for-evaluation (sexpr)
;;   (cond (*selection-receiver*
;; 	 (insert-selection (if (consp *selection-receiver*)
;; 			       (car *selection-receiver*)
;; 			       *selection-receiver*)
;; 			   sexpr))
;; 	(user::*emacs-cl-shell*
;; 	 (format t "[[INPUT-STREAM>>")
;; 	 (force-output)
;; 	 (format t " ~S" sexpr)
;; 	 (force-output)
;; 	 (format t "<<INPUT-STREAM]]")		;closing delimiter.
;; 	 (force-output))
;; 	(t (format t ";;; ~A~%" sexpr))))

;; (defun display-mouse-documentation (pane left-doc middle-doc right-doc)
;;   (declare (ignore pane))
;;   (cond (*mouse-doc-reporter*
;; 	 (mouse-report (if (consp *mouse-doc-reporter*)
;; 			   (car *mouse-doc-reporter*)
;; 			   *mouse-doc-reporter*)
;; 		       left-doc middle-doc right-doc))
;; 	(user::*emacs-cl-shell*
;; 	 (format t "[[MESSAGE-STREAM>>")
;; 	 (force-output)
;; 	 (format t "MOUSE:  ~66,1,4,<~A~;~A~;~A~>" left-doc middle-doc right-doc)
;; 	 (force-output)
;; 	 (format t "<<MESSAGE-STREAM]]")
;; 	 (force-output))
;; 	(t nil)))


