;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: picture.lisp
;;;  Author: Simoncelli/Heeger
;;;  Description:  Definition of the class PICTURE.
;;;  Creation Date: Spring, 1988
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '(setp getp))

(defvar *picture-index* 0)

;;; *** Rewrite this comment.  Pictures exist only in panes.  Each
;;; type of picture corresponds to a way of looking at viewables.
;;; Examples are gray (8bit grayscale), 1d graph, surface plot,
;;; dither, vector-field, etc....  Most picture types will maintain a
;;; system-dependent intermediate representation (eg grays keep a
;;; bitmap) which can be displayed more efficiently than starting from
;;; the viewable.  The current slot is a version number for the system
;;; dependent intermediate representation.  The index slot contains an
;;; identification number which is incremented every time a new
;;; picture is displayed in the pane, and each picture has the
;;; corresponding number associated with it, for convenient reference.
;;; The following methods must be provided for every picture subclass
;;; (see the documentation section "Adding a New Picture Subclass"):
;;; static-arrays-of set-not-current convert-picture set-parameter
;;; title-bar-string position-message drag-picture (?)  zoom-picture
;;; present

;;; The system-dependent-frob is a window-system dependent
;;; intermediate representation which depends on the type of screen.
;;; For gray pictures on X-screens, it is an offscreen pixmap.  For
;;; graphs on X-screens, it is an offscreen bitmap.  For grays on
;;; postscript screens, it is an offscreen array.  x-offset and
;;; y-offset keep track of the position of the picture within the
;;; pane.  
(def-simple-class picture ()
  (viewable
   system-dependent-frob
   pane-of
   (current :initform nil)
   (reinit-args :initform nil)
   (index :initform (incf *picture-index*))
   (zoom :initform 1 :type (or number (eql :auto) cons)
	 :documentation "Enlargement factor, relative to the `natural' size of the
picture.  This can be set to :auto (zoom to size of pane), or a pair of numbers
 (zoom to those dimensions, in pixels)")
   (x-offset :initform 0 :type integer)
   (y-offset :initform 0 :type integer)))

(defmethod settable-parameters ((class-name (eql 'picture)))
  (append '(zoom x-offset y-offset) (call-next-method)))

(defmethod print-object ((pic picture) stream)
  (let* ((vbl (when (slot-boundp pic 'viewable) (slot-value pic 'viewable)))
	 (index (when (slot-boundp pic 'index) (slot-value pic 'index)))
	 (vbl-name (and vbl (name vbl))))
    (if vbl-name
	(format stream "#<~A of ~S>" (object-class-name pic) vbl-name)
	(format stream "#<~A #~D>" (object-class-name pic) index))))

;;; Subclasses of picture should return a list of all static arrays used in  
;;; the picture and its system-dependent-frob.
(defmethod static-arrays-of ((pic picture))
  (static-arrays-of (system-dependent-frob pic)))

(defun current-p (pic)
  (eq (current pic) (current (slot-value pic 'viewable)))) ;use eq - slot might be nil

(defmethod dimensions ((pic picture))
  (dimensions (slot-value pic 'system-dependent-frob)))

;;; This method is called when the underlying viewable is
;;; destructively modified.  It should reset the relevant picture
;;; parameters, and then it should call-next-method.  See
;;; set-not-current on grays for an example (in the file gray.lisp).
;;; The default method updates the title bar if the picture is on top
;;; of the stack.
(defmethod set-not-current ((pic picture))
  (when (eq pic (car (picture-stack (pane-of pic))))
    (set-pane-title-bar (pane-of pic) 
			(format nil "**-~S" (name (viewable pic)))))
   nil)

;;; *** Should set reinit-args here, instead of calling
;;; reinitialize-instance, but doing this breaks overlays.
(defmethod initialize-instance :around ((pic picture)
					&rest initargs
					&key viewable pane-of
					&allow-other-keys)
  (unless viewable (error "Must provide a :viewable argument when creating a picture"))
  (unless pane-of  (error "Must provide a :pane-of argument when creating a picture"))
  (call-next-method pic :viewable viewable :pane-of pane-of) ;fill initform slots
  ;; *** SHould this be here:
  (remf initargs 'clos::initargs-validated)
  (remf initargs :viewable) (remf initargs :pane-of)
  ;;(setf (reinit-args pic) initargs)	;these to be reinitialized at display time
  (apply #'reinitialize-instance pic initargs)
  )

;;; If there are reinit-args, call reinitialize-instance on them
(defmethod compute-picture :around (pic vbl)
  (declare (ignore vbl))
  (with-slots (reinit-args) pic
    (when reinit-args
      (apply #'reinitialize-instance pic reinit-args)
      (setf reinit-args nil))
    (call-next-method)))

;;; Modification of the standard CLOS reinitialize-instance method.
;;; Calls reset-picture-defaults to modify the initarg list, then
;;; calls the usual method.
(defmethod reinitialize-instance ((pic picture) &rest initargs
				  &key
				  (viewable (slot-value pic 'viewable))
				  &allow-other-keys)
  (setq initargs (apply #'reset-picture-defaults pic viewable initargs))
  (apply #'call-next-method pic initargs)) ;fill slots

;;; reset-picture-defaults is basically an extended shared-initalize
;;; method that dispatches on two args!  reset-picture-defaults should
;;; compute the parameters that need to be computed automatically,
;;; altering their values in the initargs list It should also take
;;; care of setting other parameters that need to be modified because
;;; they are dependent on the initargs passed.  It should return the
;;; modified initargs list.
(defmethod reset-picture-defaults ((pic picture) (vbl viewable) &rest initargs
				   &key
				   (zoom nil zoom-supplied-p)
				   (x-offset nil x-offset-supplied-p)
				   (y-offset nil y-offset-supplied-p)
                                   &allow-other-keys)
  (when x-offset-supplied-p
    (setf (getf initargs :x-offset) (if (numberp x-offset) (round x-offset) 0)))
  (when y-offset-supplied-p
    (setf (getf initargs :y-offset) (if (numberp y-offset) (round y-offset) 0)))
  (when (and zoom-supplied-p  (or (not (numberp zoom)) (< zoom 0.0)))
    (setf (getf initargs :zoom) 1))
  initargs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Interactive methods:

;;; Must have the pane locked for drag-picture
(defmethod drag-picture :around ((pic picture) dy dx)
   (with-locked-pane (pane-of pic)
     (call-next-method)))

;;; Called by mouse-click.  Redraw pane, with picture offset by dr
;;; rows and dc columns from its current position.  This will work for
;;; all pictures, but it will be slow.  Redefined by sub-picture types
;;; to simply render rather than calling draw-pane.
(defmethod drag-picture ((pic picture) dy dx)
  (with-slots (y-offset x-offset pane-of) pic
    (if (and dy dx)
	(setf y-offset (+ y-offset dy)  x-offset (+ x-offset dx))
	(setf y-offset 0                x-offset 0))
    (draw-pane pane-of)))

;;; Zooms centering on the mouse position if the whole picture doesn't
;;; fit in the pane.  Otherwise, tries to zoom centered on same
;;; position.  A bit ugly.
(defmethod zoom-picture ((pic picture) factor y x)
  (with-slots (zoom pane-of y-offset x-offset system-dependent-frob) pic
    (let* ((zoomed-y-dim (* (y-dim system-dependent-frob) factor))
	   (zoomed-x-dim (* (x-dim system-dependent-frob) factor))
	   (y-overhang (floor (- zoomed-y-dim (y-dim pane-of)) 2))
	   (x-overhang (floor (- zoomed-x-dim (x-dim pane-of)) 2))
	   (new-x-offset x-offset)
	   (new-y-offset y-offset))
      (when (> y-overhang 0)  (decf new-y-offset (- y (/ (y-dim pane-of) 2))))
      (when (> x-overhang 0)  (decf new-x-offset (- x (/ (x-dim pane-of) 2))))
      (setq new-y-offset
	    (clip (* new-y-offset factor) (- (abs y-overhang)) (abs y-overhang))
	    new-x-offset
	    (clip (* new-x-offset factor) (- (abs x-overhang)) (abs x-overhang)))
      (reinitialize-instance pic
			     :zoom (* zoom factor)
			     :x-offset new-x-offset
			     :y-offset new-y-offset)
      (draw-pane pane-of))))

;;; Default is a nop.  Called by mouse-click.
;;; *** Should this return a string? (it often calls status-message).
(defmethod position-message ((pic picture) (vbl viewable) pane y x)
  (declare (ignore pane x y))
  nil)

(defmethod title-bar-string ((pic t))
  (format nil "~s" (name (viewable pic))))

;;; Destroys the picture (does not destroy the viewable) by freeing
;;; its system dependent arrays and removing it from the stack of the
;;; pane it is on.  This is called by the destroy method on a
;;; viewable.
;;; *** This destroys the static-arrays, then they get destroyed
;;; again by the call to destroy the system-dependent-frob.  This is
;;; done just in case the picture has some static-arrays that are not
;;; in the frob.  The right thing to do is to have destroy methods on
;;; each picture type that do the right thing for that picture type.
(defmethod destroy ((pic picture) &key silent &allow-other-keys)
  (let ((viewable (slot-value pic 'viewable)))
    (setf (pictures-of viewable)
	  (delete pic (pictures-of viewable)))
    (remove-picture pic)
    (mapc #'free-array (static-arrays-of pic))
    (destroy (system-dependent-frob pic))
    (if *auto-destroy-orphans*
	(push-onto-eval-queue		;Do this only at top level!
	 `(when (orphaned-viewable-p ,viewable) (destroy ,viewable))
	 :silent t)
	(when (not silent)
	  (push-onto-eval-queue		;Do this only at top level!
	   `(when (orphaned-viewable-p ,viewable)
	     (warn "Creating orphan: popping last picture of ~A~%" ,viewable)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype color () '(or keyword list))

;;; SETP sets picture parameters using a syntax similar to setq.
;;; GETP returns the value of the slot named by its argument in the current picture.

;;; Set parameters of the picture on the top of the current pane.
;;; This macro is usually called by the user from top level.  It's a
;;; bit messy because it has to work from any package...  NOTE: Don't
;;; call set-not-current here because that would indicate that the
;;; VIEWABLE had changed.
(defmacro setp (&rest args)
  `(let* ((pane obvius::*current-pane*)
	  (pic (car (obvius::picture-stack pane))))
    (when pic
      (reinitialize-instance
       pic
       :current nil
       ,@(loop for params = args then (cddr params)
	       until (null (cdr params))
	       for slot = (car params)
	       for val = (cadr params)
	       nconc (list (intern (symbol-name slot) :keyword)
			   val)))
      (refresh pane)
      (values))))

(defmacro getp (&optional slot-name)
  (append `(let ((pic (car (picture-stack *current-pane*)))))
	  (if slot-name
	      `((slot-value pic (quote ,(find-symbol (string slot-name) 'obvius))))
	      `((pretty-print-slots pic)))))

;;; Prints names and values for all slots if no arguments are supplied.
(defmacro pretty-print-slots (obj)
  `(format t "Slot Name~30tSlot Value~%~60,,,'-<~>~%~{~a~30t~a~%~}~%"
	   (apply #'append 
		  (mapcar #'(lambda (slot) (list (CLOS::slot-definition-name slot) 
						 (slot-value ,obj (CLOS::slot-definition-name slot))))
			  (CLOS::class-slots (class-of ,obj))))))


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
