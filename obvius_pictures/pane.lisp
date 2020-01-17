;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  pane.lisp
;;;  Author: Simoncelli/Heeger
;;;  Description: Machine independent pane system.
;;;  Creation Date:  Spring, 1988
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file defines (system-independent) screens and panes.  A
;;; screen is an abstraction of a display device, which contains a
;;; bunch of panes.  A pane is a generic window-system object:
;;; basically just a stack of pictures.  The global variable
;;; *current-pane* maintains a pointer to the currently selected pane,
;;; which is used for auto-display of viewables.

(in-package :obvius)
(export '(new-pane refresh next-pane cycle-pane
	  pop-picture current-viewable get-viewable
	  current-screen *current-pane* *auto-update-pictures* @ @@ @@@))

(defvar *screen-list* nil)
(defvar *current-pane* nil)
(defvar *empty-pane-string* "empty pane"
  "This string is written in the title bar of an empty pane.")

(defvar *auto-update-pictures* nil
  "If non-nil, calling display on a viewable with an existing picture
will cause that picture to be recomputed if it is not current.  If nil
(the default), the user must explicitly refresh the pane to update an
out-of-date picture.")
(eval-when (load eval) (setf (get '*auto-update-pictures* :type) '(member t nil)))

;;; System-independent screen object.  This will be a supeclass for
;;; the system-dependent screen (e.g. see x-window.lisp).  NOTE:
;;; System-dependent code should provide a function called
;;; MAKE-SCREEN.  IT MUST PUT THE SCREEN ON THE *SCREEN-LIST* (it
;;; would be nice to do that here, but we don't want
;;; postscript-screen's on the list).
;;; Screens must have a depth method.
(def-simple-class screen ()
  ((pane-list :initform nil)))

;;; See comment above!
;(defmethod initialize-instance :around ((screen screen) &rest args)
;  (call-next-method)
;  (setq *screen-list* (nconc *screen-list* (list screen)))
;  screen)

(def-simple-class pane ()
  (screen-of
   (picture-stack :initform nil)
   (locked :initform nil)))			;multi-processing lock.

;;; Keeps track of transformation due to x-offset, y-offset, zoom.
;;; Frobs must have a dimensions method, which should return the size
;;; of a bounding box containing the picture.
(def-simple-class frob ()
  ((pane->frob-y :initform (make-transform))
   (pane->frob-x :initform (make-transform))))

(defmethod initialize-instance ((the-pane pane) &rest initargs)
  (let ((screen (getf initargs :screen-of)))
    (unless screen (error "Must provide a :screen-of initarg"))
    (call-next-method)			;standard slot setting
    (push the-pane (pane-list screen))))

(defmacro pane-p (p)
  `(typep ,p 'pane))

;;; This calls the system-dependent function make-pane to create a new
;;; pane (window).  It selects the new pane.  Initialize-instance
;;; takes care of setting up the double pointers between the pane and
;;; screen.
(defun new-pane (&rest args
		       &key
		       (screen (current-screen))
		       left bottom right top width height border-width)
  (declare (ignore left bottom right top width height border-width))
  (if screen
      (remf args :screen)
      (error "Cannot create a pane -- no current screen!"))
  (vom:info "[new-pane] screen: ~a args: ~a" screen args) 
  (let ((the-pane (apply 'make-pane screen :screen-of screen args)))
    (set-selected-pane the-pane)
    the-pane))

;;; This version just sets the global *current-pane*.
;;; System-dependent versions should highlight the window in some way
;;; (e.g. bold title text, or different colored border).
(defmethod set-selected-pane ((pane pane))
  (setq *current-pane* pane))

;;; *** This should ask X which screen the mouse is on.
(defun current-screen ()
  (or (and *current-pane* (screen-of *current-pane*))
      (and *screen-list* (car *screen-list*))
      (make-screen)))

;;; Convert a point in pane coordinates back to something relavent to
;;; the picture.  In general, the frob knows how to convert from pane
;;; to frob coords, and the picture knows how to convert from frob
;;; back to viewable.  *** Is this flexible enough?  Or should we
;;; break it into two methods?
(defun pane-coord-to-viewable-coord (pic y x)
  (multiple-value-bind (frob-y frob-x)
      (pane-coord-to-frob-coord (pane-of pic) (system-dependent-frob pic) y x)
    (frob-coord-to-viewable-coord pic frob-y frob-x)))

;;; Default method just uses transform stored in frob.  Render methods
;;; should set this to be accurate!
(defmethod pane-coord-to-frob-coord ((pane pane) (frob frob) y x)
  (with-slots (pane->frob-y pane->frob-x) frob
    (values (transform-point pane->frob-y y)
	    (transform-point pane->frob-x x))))

;;; Default method does nothing:
(defmethod frob-coord-to-viewable-coord ((pic picture) frob-y frob-x)
  (values frob-y frob-x))

;;; Call this before calling the window-system-dependent version
;; [THO] 2020-01 changed to work with error conditions
(defmethod destroy :around ((pane pane) &key &allow-other-keys)
  (if  (picture-stack pane)
       (restart-case 
	   (error 'picture-stack-not-empty :text ;;"Destroy all of these pictures: ~A"
		  "You are destroying a pane that contains pictures")
	 ;;(picture-stack pane))
	 (destroy-pane-restart () (call-next-method)))
       (call-next-method)))

;;; Destroy all pictures on the stack, remove the pane from the list
;;; of panes, and reset the current pane if necessary.  If you destroy
;;; the only pane, this will insist on making a new one!
(defmethod destroy ((pane pane) &key &allow-other-keys)
  (with-locked-pane pane
    (loop until (null (picture-stack pane))
	  do (pop-picture pane))	;destroy all pictures on the stack, 
    ;;(mapc #'destroy (picture-stack pane))  ;call pop-picture - maybe system-dependent!
    (setf (pane-list (screen-of pane))
	  (delete pane (pane-list (screen-of pane))))
    (when (eq pane *current-pane*)
      (if (pane-list (screen-of pane))
	  (set-selected-pane (car (pane-list (screen-of pane))))
	  (set-selected-pane (new-pane))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STACK MANIPULATION

;;; This pushes a NEW picture onto a pane.  It should be called for
;;; all new pictures since pictures are not intended to exist off of
;;; pane stacks.  It puts the picture on the pane, binds the pane slot
;;; of the picture and redraws the pane.
(defun push-picture (pic &optional (pane *current-pane*))
  (with-locked-pane pane
    (setf (pane-of pic) pane
	  (picture-stack pane) (cons pic (picture-stack pane)))
    (draw-pane pane)))

;;; This moves a picture to the top of the specified pane, and then
;;; refreshes that pane.  It is called from display and from user
;;; mouse click "Move to here".  It first calls remove-picture to
;;; remove the picture from its previous pane.
(defun move-picture (pic &optional (pane *current-pane*))
  (let ((old-screen (screen-of (pane-of pic)))
	(new-screen (screen-of pane)))
    (remove-picture pic)
    (when (not (eq old-screen new-screen))
      (setq pic (convert-picture pic old-screen new-screen)))
    (push-picture pic pane)))

;;; This can be made smarter for certain screen combinations:
;;; *** Should convert-picture be dependent on screens or panes?
(defmethod convert-picture ((pic picture) (old-screen screen) (new-screen screen))
  (setf (system-dependent-frob pic) nil	;throw away old one
	(current pic) nil)		;ensure that present recomputes!
  pic)

;;; Removes picture from pane stack, without destroying it.  This is
;;; essentially the inverse of push-picture, except it can remove
;;; pictures from anywhere in the stack.  It is called by the destroy
;;; method for pictures, and is used to move a picture from one pane
;;; to another.
(defun remove-picture (pic)
  (let ((pane (pane-of pic)))
    (with-locked-pane pane
      (setf (pane-of pic) nil)		;as a precaution...
      (cond ((eq pic (car (picture-stack pane)))
	     (setf (picture-stack pane) (cdr (picture-stack pane)))
	     (draw-pane pane))
	    (t (setf (picture-stack pane) (delete pic (picture-stack pane)))))))
  pic)

;;; Destroys the top picture on the pane stack.  Destroy calls
;;; remove-picture, which takes care of redrawing the pane.  This is
;;; the only place where pictures are destroyed.
(defun pop-picture (&optional (pane *current-pane*))
  (with-slots (picture-stack) pane
    (with-locked-pane pane
      (when picture-stack		;if pane stack is not empty
	(destroy (car picture-stack))))))

;;; Cycle the pane stack and refresh the display.  
(defun cycle-pane (&optional (pane *current-pane*) (num 1))
  (with-slots (picture-stack) pane
    (with-locked-pane pane
      (let ((len (list-length picture-stack)))
	(when (> len 1)
	  (let* ((mod-num (mod num len))
		 (rest-len (- len mod-num)))
	    (setf picture-stack (nconc (nthcdr mod-num picture-stack)
				       (nbutlast picture-stack rest-len)))
	    (draw-pane pane)))))))

;;; Select and return the next pane in the pane-list of the current-screen.
(defun next-pane ()
  "Set next pane in list of panes to be the selected pane."
  (let* ((panes (pane-list (screen-of *current-pane*)))
	 (cpane (position *current-pane* panes))
	 (npane (nth (mod (1+ cpane) (length panes)) panes)))
    (set-selected-pane npane)
    npane))

;;; Top level macro to return viewable shown at top of current pane
(defmacro current-viewable ()
  `(when (picture-stack *current-pane*)
    (viewable (car (picture-stack *current-pane*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DISPLAY functions

;;; A top-level entry point which ALWAYS forces recomputation of the
;;; picture frob.
(defun refresh (&optional (pane *current-pane*))
  (let ((pic (car (picture-stack pane))))
    (when pic (setf (current pic) nil))
    (draw-pane pane)))

;;; Bring pane display up to date by displaying the picture on the top
;;; of its stack.  This takes care of the standard stuff associated
;;; with displaying the pane (ie clearing the pane, and setting the
;;; title bar).  Also, deals with empty panes.  The current slot of
;;; the picture determines whether the intermediate representation is
;;; to be recomputed.  If the slot is nil, then present recomputes the
;;; frob.  If the slot is a number, then it doesn't.  The refresh
;;; function provides an interface to draw-pane that always recomputes
;;; the frob.
(defmethod draw-pane ((pane pane) &key (clear t))
  (with-locked-pane pane
    (let ((pic (car (picture-stack pane))))
      (cond (pic
	     #| only do this on calls to display. (not cycle, move etc)
	     (when (and *auto-update-pictures* (not (current-p pic)))
	       (setf (current pic) nil))
             |#
	     (when clear
	       (clear pane)
	       (set-pane-title-bar pane "displaying..."))
	     (catch-errors
	      (progn
		(present pic (viewable pic) pane)
		(set-pane-title-bar 
		 pane (if (current-p pic)
			  (title-bar-string pic)
			  (format nil "**-~S" (name (viewable pic))))))
	      (warn "Destroying bad picture ~A." pic)
	      (catch-errors (pop-picture pane))))
	    (t
	     (clear pane)
	     (set-pane-title-bar pane *empty-pane-string*))))
    t))

;;; Displays the picture containing this viewable (redundant to pass
;;; both picture and viewable, but it's nice to allow specialization
;;; for different combinations).  In general, it will ignore the pane
;;; argument, which is there to allow system-dependent specialization
;;; to take advantage of specialized hardware.  This method should
;;; take care of re-computing the picture (if necessary), setting the
;;; current flag, and actually drawing the picture into the pane.
(defmethod present ((pic picture) (vbl viewable) (pane pane))
  (with-slots (current system-dependent-frob y-offset x-offset zoom) pic
    ;; If current slot is nil, then recompute the frob.
    (when (not current)
      (with-status-message "Computing picture"
	(compute-picture pic vbl))
      (setf current (current vbl)))
    ;; Display the picture in the pane.
    ;;(break)
    (render pane system-dependent-frob y-offset x-offset zoom)))

(defmethod compute-picture ((pic picture) (vbl viewable))
  (error "Unsupported picture/viewable combination: ~S ~S~%"
	 (object-class-name pic) (object-class-name vbl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Interactive manipulation of viewables

(defvar @ nil)     ; Last image clicked upon...
(defvar @@ nil)    ; Second to last image clicked upon
(defvar @@@ nil)   ; And a third, just in case...

;;; This returns an expression which evaluates to the viewable in the
;;; picture on the top of the pane stack.  For environments where
;;; expressions cannot be inserted for evaluation (ie. standard shells
;;; as opposed to Emacs shells), this binds the selected viewable to
;;; the global variable "@".
(defun get-viewable (&optional (pane *current-pane*))
  (declare (special @ @@ @@@))
  (let ((vbl (and (picture-stack pane)
		  (viewable (car (picture-stack pane))))))
    (when vbl
      (shiftf  @@@ @@ @ vbl)
      (if (named-viewable-p vbl)
	  (name vbl)
	  `(pid ,(index (car (picture-stack pane))))))))

;;; Returns the viewable associated with the picture with the given
;;; index number.  Signals an error if the picture is not found.
(defun pid (pic-index)
  (dolist (screen *screen-list*)
    (dolist (pane (pane-list screen))
      (dolist (pic (picture-stack pane))
	(when (eq (index pic) pic-index)
	  (return-from pid (viewable pic))))))
  (error "Picture with index ~A no longer exists!" pic-index))


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
