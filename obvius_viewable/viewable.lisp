;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: viewable.lisp
;;;  Author:  Eero Simoncelli
;;;  Description:  Definition of the class VIEWABLE.
;;;  Creation Date: Spring 1988.
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)
(export '(*auto-display-viewables*  *auto-destroy-orphans*
	  *preserve-picture-pane*
	  viewable viewable-p viewable-name
	  display-type  history-sexp  history
	  inferiors-of  superiors-of
	  info-set info-get 
	  with-result  set-result similar
	  display 
	  set-history  set-not-current  sym-or-vbl
	  set  destroy  with-local-viewables))

(defvar *auto-display-viewables* nil
  "If t, viewables returned to the top-level listener are automatically displayed
on the currently selected pane.")
(eval-when (load eval) (setf (get '*auto-display-viewables* :type) '(member t nil)))

(defvar *auto-destroy-orphans* nil
   "Slightly risky parameter causes viewables to be destroyed when they are
orphaned. This can occur when their names are rebound, when their pictures are
popped, or when their superior viewables are destroyed.")
(eval-when (load eval) (setf (get '*auto-destroy-orphans* :type) '(member t nil)))

;;; Viewables are the primary objects in the OBVIUS system.  Each
;;; instance of viewable should have a display-type slot.  This slot
;;; may contain nil, indicating that the viewable is not to be
;;; auto-displayed.  Otherwise, it should contain a symbol
;;; corresponding to a picture class which is the default picture type
;;; for auto-display of the viewable.  Info-list is a property list
;;; which can be used for additional fields.  It is easily modified
;;; using info-get and info-set.  NOTE: It is erased whenever the
;;; image is destructively modified!  If the name slot is bound to a
;;; symbol, then we try to ensure that the symbol is bound to the
;;; viewable (i.e. bi-directional pointer).  The lisp function SET is
;;; shadowed by OBVIUS to maintain this.

;;; *** Rewrite this documentation!
;;; The following methods must be provided for all viewables (see documentation:
;;; "Adding New Viewables"):
;;;    make-<vbl>        - the constructor for the viewable.  Fills the slots.
;;;    static-arrays-of  - returns list of static arrays allocated in make-<vbl>.
;;;    set-not-current   - called when the viewable is destructively modified.
;;;    inferiors-of      - returns a list of the inferior viewables (compound vbls only).
;;;    notify-of-inferior-destruction - Called when an inferior is destroyed.
;;;    print-object      - called by top-level read-eval-print loop.
;;;    present          (a joint method on pictures and viewables)

(def-simple-class viewable ()
  ((name :initform nil)
   (display-type :initform nil :type symbol) ;default display-type
   (history :initform nil)
   (info-list :initform nil)		;plist of temporary information
   (current :initform 1)		;a version number
   (pictures-of :initform nil)		;list of pictures of this viewable
   (superiors-of :initform nil)))	;list of viewables containing

(defmethod settable-parameters ((class-name (eql 'viewable)))
  (append '(display-type) (call-next-method)))

;;; This method should return a list containing all of the static
;;; arrays which were allocated when the viewable was created.  It
;;; should be rewritten for each viewable subclass.  It will be used
;;; to free the static arrays when the viewable is destroyed.
(defmethod static-arrays-of ((thing t))
  nil)

;;; All compound viewables must define this method to return a list of
;;; their sub-viewables.  When a compound viewable is destroyed, it
;;; must be removed from the superiors-of list of its sub-viewables.
(defmethod inferiors-of ((vbl viewable))
  nil)

(defun viewable-p (obj)
  (typep obj 'viewable))

;;; This method is called whenever a viewable is destructively
;;; modified.  Broadcast to all pictures containing the viewable and
;;; also to all viewables containing it.
(defmethod set-not-current ((vbl viewable))
  (incf (slot-value vbl 'current))
  (setf (slot-value vbl 'info-list) nil)
  (mapc #'set-not-current (slot-value vbl 'pictures-of))
  (mapc #'set-not-current (slot-value vbl 'superiors-of))) ;*** many unnec. calls

;;; Get value of a property on the image info-list.  This just calls
;;; getf.
(defun info-get (viewable slot)
  (getf (slot-value viewable 'info-list) slot))

;;; Set value of a property on the image property list.  Return the value.
(defun info-set (viewable slot value)
  (setf (getf (slot-value viewable 'info-list) slot) value))

(defsetf info-get info-set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; HISTORY STUFF

;;; We keep a history for each viewable.  The history of a viewable
;;; contains the viewable, the name of the function which created the
;;; viewable, and a list of histories of the arguments to that
;;; function. This way, if a viewable is destructively modified, the
;;; history of viewables that were constructed from it will remain
;;; intact!  The user should be able to extract the S-expression which
;;; created the viewable (using history-sexp or constructor), or a
;;; condensed version of this with viewable names substituted where
;;; possible (using history).

;;; **** Should probably make these structures, for efficiency
(def-simple-class history ()
  (viewable
   creation-function			;the function which created the vbl
   creation-args))			;a list of vbl histories and other args

(defmacro history-p (obj)
  `(typep ,obj 'history))

;;; Normal print method prints history object in abbreviated form.
;;; Should probably print recursively according to the common-lisp
;;; variable *print-level*.
(defmethod print-object ((h history) stream)
  (let ((*print-pretty* t))
    (declare (special *print-pretty*))
    (loop initially (format stream "#H<~A" (slot-value h 'creation-function))
	  for arg in (cdr (history-sexp h :level 1))
	  do (format stream " ~S" arg)
	  finally (format stream ">"))))

;;; This puts together a constructor for the viewable, preferable
;;; nested :level deep.  I.E., clips branches of the history tree by
;;; using names of viewables if they exist.  Level can be an integer or a
;;; non-integer (all levels).
(defmethod history-sexp ((h history) &key (level 1))
  (with-slots (viewable creation-function creation-args) h
    (if (and (integerp level)
	     (<= level 0)
	     (named-viewable-p viewable)
	     (eq (history viewable) h))	;this won't be true if destructively modified
	(name viewable)
	(cons creation-function
	      (mapcar #'(lambda (arg)
			  (if (history-p arg)
			      (history-sexp arg
					    :level
					    (if (integerp level) (1- level) level))
			      (constructor arg)))
		      creation-args)))))

(defmethod history-sexp ((vbl viewable) &key (level 1))
  (history-sexp (slot-value vbl 'history) :level level))

;;; Return an S-expression which would construct the object.  See
;;; generic versions in misc.lisp.
(defmethod constructor ((vbl viewable))
  (history-sexp (slot-value vbl 'history) :level nil))

;;; SET-HISTORY destructively replaces elements of hist-list which are
;;; viewables by their histories, and sets the history of vbl to the
;;; modified hist-list.
;;; NOTE: loop is MUCH faster than using mapl.  **** SHOULD ALSO
;;; GET RID OF CONSING &REST arg!
(defun set-history (vbl &rest hist-list)
  (loop for l on hist-list
	for car = (car l)
	do (when (viewable-p car) (rplaca l (history car))))
  (setf (slot-value vbl 'history)
	(make-instance 'history 
		       :viewable vbl
		       :creation-function (car hist-list)
		       :creation-args (cdr hist-list))))


#| Use default method
(defmethod copy ((h history) &key ->)
  h)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; VIEWABLE NAMES -- we put in some fancy behavior to allow viewables
;;; to know when there is a global symbol which is bound to them.

;;; Legitimate types for filling the name slot of a viewable.
(deftype viewable-name () '(or string symbol null))

;;; The following three expressions rebind the Common Lisp function
;;; SET to behave correctly if the second arg is a viewable. 
;;; *** Should do this with defadvice, but be careful: could easily
;;; get into circular function calls!

(defvar *common-lisp-set-function*)

(eval-when (compile load)
  (when (not (boundp '*common-lisp-set-function*))
    (setq *common-lisp-set-function* #'cl:set)))

;;; Modified set function.  If val is a viewable, just call set-name.
;;; Otherwise, check to see if the symbol is currently bound to a
;;; viewable, and if so set the name of the viewable to nil.
(fmakunbound 'set)
(defun set (sym val)
  "Modified version of lisp:set that handles binding of symbols to viewables
and rebinding of symbols which are already bound to viewables."
  (let (vbl)
    (check-type sym symbol)
    (cond ((viewable-p val)  (set-name val sym)  val)
	  (t (when (and (boundp sym)
			(viewable-p (setq vbl (symbol-value sym)))
			(eq (name vbl) sym))
	       (set-name vbl nil)
	       (if *auto-destroy-orphans*
		   (push-onto-eval-queue ;Do this only at top level!
		    `(when (orphaned-viewable-p ,vbl) (destroy ,vbl))
		    :silent t)
		   (when (orphaned-viewable-p vbl)
		     (warn "Creating orphan: rebinding viewable symbol ~A~%" sym))))
	     (funcall *common-lisp-set-function* sym val)))))

#|  
;;; This is the right way to change the lisp:set function, but it is
;;; not generic Common Lisp, so we leave it here until advice is added
;;; to the standard!
(defadvice (lisp:set viewable-name-change) (sym val)
  (let (vbl)
    (check-type sym symbol)
    (cond ((viewable-p val)  (set-name val sym)  val)
	  (t (when (and (boundp sym)
			(viewable-p (setq vbl (symbol-value sym)))
			(eq (name vbl) sym))
	       (set-name vbl nil)
	       (if *auto-destroy-orphans*
		   (push-onto-eval-queue ;Do this only at top level!
		    `(when (orphaned-viewable-p ,vbl) (destroy ,vbl))
		    :silent t)
		   (when (orphaned-viewable-p vbl)
		     (warn "Creating orphan: rebinding viewable symbol ~A~%" sym))))
	     (advice-continue sym val)))))
|#

;;; Set the name of a viewable, binding it if it is a symbol.  If the
;;; viewable already had a symbol name, unbind it. 
(defun set-name (vbl new-name &aux old-vbl)
  (check-type new-name viewable-name)
  ;; If current name of vbl is a symbol bound to vbl, unbind the symbol
  (when (named-viewable-p vbl) (makunbound (name vbl)))
  (when (and (symbolp new-name) new-name (not (keywordp new-name)))
    ;; If new-name is symbol bound to another viewable, set its name to nil.
    (when (and (boundp new-name) (named-viewable-p (setq old-vbl (symbol-value new-name))))
      (set-name old-vbl nil)
      (if *auto-destroy-orphans*
	  (push-onto-eval-queue		;DO this at top level only!
	   `(when (orphaned-viewable-p ,old-vbl) (destroy ,old-vbl :silent t))
	   :silent t)
	  (when (orphaned-viewable-p old-vbl)
	    (warn "Creating orphan: rebinding viewable symbol ~A~%" new-name))))
    ;; If new-name is a non-nil symbol, set global symbol value to vbl
    (funcall *common-lisp-set-function* new-name vbl))    
  (setf (slot-value vbl 'name) new-name) ;set name slot of vbl
  (loop for pic in (pictures-of vbl)	 ;redisplay visible pictures
	for pane = (pane-of pic)
	do
	(when (eq pic (car (picture-stack pane))) ;is pic on top?
	  (draw-pane pane))))

(defsetf name set-name)

;;; Check consistency of viewable name: Returns true if arg is a
;;; viewable, the viewable name slot contains a symbol, and the symbol
;;; is bound to the viewable.
(defun named-viewable-p (vbl)
  (and (viewable-p vbl)
       (let ((the-name (name vbl)))
	 (and (symbolp the-name)
	      (boundp the-name)
	      (eq (symbol-value the-name) vbl)))))

;;; These viewables are protected from orphan destruction.  That is,
;;; these viewables are not to be considered as orphans by the system.
;;; So if *auto-destroy-orphans* is non-nil, these viewables will not
;;; be destroyed, ieven if they are orphaned.
(defvar *protected-viewables* nil)

;;; Assumes arg is a viewable.
(defun orphaned-viewable-p (vbl)
  (and (null (pictures-of vbl))
       (null (superiors-of vbl))
       (not (named-viewable-p vbl))
       (not (member vbl *protected-viewables*))))

;;; Very useful little function: allows you to reuse globals!
(defun sym-or-vbl (sym)
  (if (and (boundp sym) (viewable-p (symbol-value sym)))
      (symbol-value sym)
      sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DESTROY methods

(defmethod destroy ((thing t) &key &allow-other-keys)
  nil)

(defmethod destroy ((l list) &rest keyargs)
  (mapc #'(lambda (thing) (apply 'destroy thing keyargs))  l)
  nil)

;;; When a viewable is destroyed (ie its memory is returned to the
;;; system), it must notify its superiors (they may signal an error if
;;; this is not allowed), it must notify its inferiors (they must
;;; remove it from their superiors-of lists), and it must destroy its
;;; pictures.  
(defmethod destroy ((vbl viewable) &key silent suppress-error)
  (unless (eq (name vbl) :destroyed)
    (mapc #'(lambda (sup) (notify-of-inferior-destruction
			   sup vbl :suppress-error suppress-error))
	  (superiors-of vbl))
    (mapc #'(lambda (inf) (notify-of-superior-destruction
			   inf vbl :suppress-error suppress-error)) 
	  (inferiors-of vbl))
    (let ((*auto-destroy-orphans* nil))	;otherwise, re-destroys vbl.
      (declare (special *auto-destroy-orphans*))
      (dolist (pic (pictures-of vbl)) (destroy pic :silent t)))
    (destroy-viewable vbl :silent silent))
  nil)

#|
;;; old version 7/93 DH
(defmethod destroy ((vbl viewable) &key silent)
  (unless (eq (name vbl) :destroyed)
    (mapc #'(lambda (sup) (notify-of-inferior-destruction sup vbl))
	  (superiors-of vbl))
    (mapc #'(lambda (inf) (notify-of-superior-destruction inf vbl)) 
	  (inferiors-of vbl))
    (let ((*auto-destroy-orphans* nil))	;otherwise, re-destroys vbl.
      (declare (special *auto-destroy-orphans*))
      (dolist (pic (pictures-of vbl)) (destroy pic :silent t)))
    (destroy-viewable vbl :silent silent))
  nil)
|#

;;; This is called on the superiors of a viewable when that viewable
;;; is destroyed. This method may be provided for every viewable type
;;; which contains other viewables (ie - every compound viewable).
;;; Different types of superior viewable may choose whether or not to
;;; allow their inferiors to be destroyed. See, for example,
;;; image-pair and pyramid viewables.  Generic method does a
;;; continuable error.
(defmethod notify-of-inferior-destruction ((sup-vbl viewable) inf-vbl
					   &key suppress-error)
  (unless suppress-error
    (cerror "Destroy  both ~A and ~A."
	    "You are attempting to destroy ~A which is contained in ~A."
	    inf-vbl sup-vbl))
  (destroy sup-vbl :suppress-error suppress-error))

#|
;;; old version 7/93 DH
(defmethod notify-of-inferior-destruction ((sup-vbl viewable) inf-vbl)
  (declare (ignore inf-vbl))
  (cerror "Destroy  both ~A and ~A."
	  "You are attempting to destroy ~A which is contained in ~A."
	  inf-vbl sup-vbl)
  (destroy sup-vbl))
|#

;;; This is called on the inferiors of a viewable when that viewable
;;; is destroyed.  It removes the superior viewable from the
;;; superiors-of slot of its inferiors.  If the global parameter
;;; *auto-destroy-orphans* is non-nil, it destroys the inferior if it
;;; has been orphaned, (i.e. has no symbol name, has no pictures, and
;;; does not belong to any other compound viewables).  **** SERIOUS
;;; BUG: This should be done at top level to avoid destroying local
;;; variable inferiors which might be needed by a function, etc. but
;;; if we do that, then we won't free up the space when we need it
;;; (i.e. immediately!).  
;;; This method need not be rewritten for each subclass of viewable.
(defmethod notify-of-superior-destruction ((inf-vbl viewable) sup-vbl
					   &key suppress-error)
  (with-slots (superiors-of) inf-vbl
    (setf superiors-of (delete sup-vbl superiors-of))
    (when (and *auto-destroy-orphans* (orphaned-viewable-p inf-vbl))
      (destroy inf-vbl :silent t :suppress-error suppress-error))))

#|
;;; old version 7/93 DH
(defmethod notify-of-superior-destruction ((inf-vbl viewable) sup-vbl)
  (with-slots (superiors-of) inf-vbl
    (setf superiors-of (delete sup-vbl superiors-of))
    (when (and *auto-destroy-orphans* (orphaned-viewable-p inf-vbl))
      (destroy inf-vbl :silent t))))
|#

;;; Free the arrays of the viewable and if its name is a symbol which
;;; is bound to it, unbind it.
(defmethod destroy-viewable ((vbl viewable) &key silent)
  (dolist (arr (static-arrays-of vbl))
    (free-array arr :suppress-warning t))
  (unless silent 
    (status-message "~A named ~S destroyed." (object-class-name vbl) (name vbl)))
  (when (named-viewable-p vbl)
    (makunbound (name vbl)))
  (setf (slot-value vbl 'name) :destroyed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GENERIC VERSIONS OF WITH-RESULT AND SET-RESULT

;;; This macro is to be used for all functions (methods) which return
;;; viewables as results. It creates a result viewable and binds it to
;;; a local variable for use in the body.  The result is created based
;;; on the model and the result-argument passed by the user (see the
;;; set-result method).  The macro takes care of the system
;;; administrative stuff in a standard way so that people writing
;;; operations which return viewables don't have to know about these.
;;; The result is protected from the garbage-collector (ogc) and
;;; auto-destruction of orphans (see *auto-destroy-orphans*).
(defmacro with-result (((res-symbol res-arg) model . hist-list) . body)
  `(let* ((,res-symbol (set-result ,res-arg ,model)) ;create/check result
	  (*protected-viewables*  (cons ,res-symbol *protected-viewables*)))
    (declare (special *protected-viewables*)) ;dynamic shadowing
    (unwind-protect
	 (progn ,@body)			;execute the body
      (set-not-current ,res-symbol)	;mark the result viewable modified
      ,(when hist-list			;set the history AFTER executing body.
	 (if (equal (car hist-list) '(quote apply))
	     `(apply 'set-history ,res-symbol ,@(cdr hist-list))
	     `(set-history ,res-symbol ,@hist-list)))))) 

;;; In general, the set-result method must handle four cases:
;;; 1) user passes a result viewable, model is a viewable.
;;; 2) user passes name (nil/symbol/string), model is a viewable.
;;; 3) user passes a result viewable, model is a plist.
;;; 4) user passes name (nil/symbol/string), model is a plist.
;;; Must redefine Case 2 for most viewable types.  It is recommended
;;; that Case 3 be redefined for most viewble types to do proper error
;;; checking.  Cases 1 and 4 probably need not be redefined.

;;; *** NOTE: might want to check that types of model-specification and
;;; res are compatible (e.g., one of them inherits from the other)  This
;;; involves converting symbols to class objects (or vice versa) and calling
;;; subtypep...

;;; Case 1: This one is the same for most viewable types, no need to
;;; redefine it for most sub-viewables.
(defmethod set-result ((res viewable) (model viewable))
  (unless (subtypep (class-of res) (class-of model))
    (error "Result ~a is not a subtype of the model type ~a" res (class-of model)))
  (check-size model res)
  res)

;;; Case 2: This one must make a new viewable, modeled on the model
;;; viewable.  This may need to be redefined for some sub-viewables.
;;; It should copy relevant slots from the model into the result.
;;; Should always include (check-type name viewable-name) and a
;;; make-instance.  This case need not be defined if there is a
;;; sufficient initialize-instance method that sets up the slots with
;;; default values.  If you want the slot values to depend on the
;;; model's slot values, then you need to redefine this set-result
;;; method for the sub-viewable type.  An example of each are images
;;; and sequences.  Case 2 set-result method is redefined for images;
;;; it conses up a data array of the same dimensions as the model's
;;; data array.  Case 2 set-result method is not redefined for
;;; sequences; it is assumed that the calling function will set up the
;;; viewable-list.
(defmethod set-result ((name t) (model viewable))
  (check-type name viewable-name)
  (make-instance (class-of model)
		 :name name
		 :display-type (display-type model)))

;;; Case 3: This should be redefined for sub-viewables to do error
;;; checking.  It should make certain that the result passed by the
;;; user is consistent with the model-plist.  This default method
;;; checks only that the result class and model class are the same.
(defmethod set-result ((res viewable) (model-plist list))
  (unless (typep res (getf model-plist :class))
    (error "Result ~a is incompatible with argument type ~a"
	   res (getf model-plist :class)))
  res)

;;; Case 4: make a new instance of the viewable. plist must go first
;;; because it contains the class-name.  This one is inherited by all
;;; viewables, no need to redefine it.
(defmethod set-result ((name t) (model-plist list))
  (check-type name viewable-name)
  (unless (eq (car model-plist) :class)
    (error "Bad model plist: ~A.~%First keyword must be :CLASS."
	   model-plist))
  (apply 'make-instance (append (cdr model-plist) (list :name name))))

;;; In general, the check-size method should ensure that a list of
;;; viewables has compatible "dimensions" so that point operations
;;; performed on them will work.  The meaning of "dimensions" depends
;;; on the sub-class of viewable.  The method should return a viewable
;;; if the sizes are consistent, and signal an error otherwise.  See
;;; example in image.lisp.  If this has not been defined, assume no
;;; checking is necessary.
(defmethod check-size ((vbl viewable) &rest vbl-list)
  (declare (ignore vbl-list))
  vbl)

(defmethod initialize-instance :after ((vbl viewable) &key name &allow-other-keys)
  (check-type name viewable-name)	
  (when name (set-name vbl name)))
 
;;; SIMILAR makes a new (blank) viewable.
(defmethod similar ((vbl viewable) &key ->)
  (when (viewable-p ->)
    (error "Cannot pass an existing viewable as a result arg to SIMILAR."))
  (with-result ((result ->) vbl)  result))

;;; *** Implement this
(defmethod copy ((vbl viewable) &key ->)
  (declare (ignore ->))
  (error "Copy not yet implemented on viewables of type ~a"
	 (class-name (class-of vbl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DISPLAY 

;;; This is the method which prints a textual representation of the
;;; viewable.  *** Should print dimensions if the slot exists.
(defmethod print-object ((vbl viewable) stream)
  (format stream "#<~A ~S>" (object-class-name vbl) (name vbl)))

;;; There are two reasonable behaviors for when the user calls display
;;; and it comes back with an already-existing picture.  One is that
;;; the picture is always moved to the *current-pane*.  This lets the
;;; user know which pane is current, AND it guarantees that a
;;; subsequent setp will affect the right picture!  Alternatively, the
;;; user may have organized a set of pictures on particular panes and
;;; not wish them to jump around automatically.  We provide a variable
;;; to control this behavior.
(defvar *preserve-picture-pane* nil
  "If nil, calling display on a viewable with an existing picture will
cause the picture to jump to the current pane.  Otherwise, the picture will
remain on the same pane, but will be made visible (i.e., will be moved  to the
top of the stack).")
(eval-when (load eval) (setf (get '*preserve-picture-pane* :type) '(member t nil)))

;;; This generally gets called by the top-level read-eval-print loop
;;; (repl), although it is also the exported method for creating
;;; pictures of viewables.  If no picture of the right type exists, a
;;; new picture is created with all default parameter values, and
;;; displayed on the current pane.  If no pane exists (*current-pane*
;;; is nil), a new one is created.  The display-type argument defaults
;;; to t, meaning to use the display-type slot of the viewable.  If
;;; display-type is nil, the viewable is not displayed.  Otherwise,
;;; the value should be a symbol corresponding to a picture type.  All
;;; extra keyword args are passed along to the call to make-instance
;;; for the picture.  The keyword :make-new is used to control what
;;; happens if a picture of the right type already exists.  If it is
;;; non-nil then a new picture is made and displayed.  Otherwise, one
;;; of the existing pictures is displayed, and updated if it is
;;; out-of-date and *auto-update-pictures* is non-nil. The function
;;; find-picture determines which one of the existing pictures is
;;; displayed.
(defmethod display ((vbl viewable)
		    &optional (display-type t)
		    &rest keys &key
		    (pane (or *current-pane* (new-pane)))
		    make-new
		    &allow-other-keys)
  (declare (special *current-pane*))
  (remf keys :pane)
  (remf keys :make-new)
  (let ((pic-type (if (eq display-type t) (display-type vbl) display-type))
	(pic (find-picture vbl display-type)))
    (when pic-type			;if nil, don't display!
      (cond ((or make-new (null pic))
	     (set-selected-pane pane)
	     (setq pic (apply 'make-instance pic-type
			      :viewable vbl
			      :pane-of pane keys))
	     (push-picture pic pane)
	     (pushnew pic (pictures-of vbl))) ;add pic to pictures-of the vbl.
	    ((progn			;Picture not visible on pane
	       (when *preserve-picture-pane* (setq pane (pane-of pic)))
	       (when (and *auto-update-pictures* (not (current-p pic)))
		 (setf (current pic) nil))
	       (not (eq (car (picture-stack pane)) pic)))
	     (move-picture pic pane))
	    ((not (current-p pic))	;Picture needs to be updated: This may or may 
	     (draw-pane pane))		;not refresh, dep. on *auto-update-pictures*
	    (t nil))
      t)))

;;; Returns a list of all pictures of the appropriate type containing
;;; the viewable, or nil if not found.
(defun find-all-pictures (vbl &optional (display-type (display-type vbl)))
  (loop for pic in (pictures-of vbl)
	when (and (eq (viewable pic) vbl) ;double check pointer consistency
		  (typep pic display-type))
	collect pic))

;;; Returns a picture of the given type (or one inheriting from that
;;; type).  Gives priority to visible pictures, then pictures already
;;; on the current pane.  *** could also give preference to pictures
;;; that are exactly the requested type (instead of a subtype).
(defun find-picture (vbl &optional (display-type (display-type vbl)))
  (declare (special *current-pane*))
  (let ((pics (find-all-pictures vbl display-type)))
    (or (loop for pic in pics do	;visible?
	      (when (eq (car (picture-stack (pane-of pic))) pic) (return pic)))	      
	(loop for pic in (picture-stack *current-pane*) do   ;in current pane?
	      (when (member pic pics) (return pic)))
	(car pics))))			;most recently created

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; WITH-LOCAL-VIEWABLES

;;; This macro can be used in place of let* when the values being
;;; assigned to the local variables are viewables which should be
;;; destroyed upon leaving the macro.  It tries to do this
;;; intelligently, destroying the viewables in an order which will not
;;; cause errors (see destroy-viewables below).  Execute body, and at
;;; the end, destroy viewables bound in vbl-list.  The following
;;; exceptions are NOT destroyed: 1) any local viewable that is
;;; RETURNED from the body, 2) a local viewable that has a NON-LOCAL
;;; superior, 3) local viewables with global symbol names, and 4)
;;; local viewables displayed in pictures.  Returned inferiors are
;;; protected from auto-destruction of orphans.  NOTE: this is similar
;;; to the with-local-arrays macro.
(defmacro with-local-viewables (vbl-list &body body)
  (let* ((vars (loop for item in vbl-list
		     for var = (if (symbolp item) item (car item))
		     collect var))
	 (res (gensym)))
    `(let* (,@vars ,res)
      (unwind-protect
	   (progn ,@(loop for item in vbl-list
			  when (listp item) collect `(setq ,@item))
		  (setq ,res (multiple-value-list (progn ,@body)))
		  ;;(when (intersection (remove-if-not 'viewable-p ,res) (list ,@vars))
		  ;;  (error "Attempting to return a local viewable"))
		  (values-list ,res))
	(let ((*protected-viewables* (append ,res *protected-viewables*)))
	  (declare (special *protected-viewables*))
	  (destroy-viewables ,@vars))))))


#| TEST:
(let ((*auto-destroy-orphans* t)
      (im1  (make-image '(10 10) :name "im1"))
      (im2  (make-image '(10 10) :name "im2")))
  (declare (special *auto-destroy-orphans*))
  (with-local-viewables ((pair (make-image-pair (list im1 im2) :name "pair")))
    im1))				;should not be destroyed!

(ogc)	     ;should not have creatd garbage (im2 should be destroyed)
|#

;;; Attempts to (silently) destroy the viewables in an order that will
;;; not cause errors (ie. superiors before inferiors!).  Modified to
;;; add inferiors to list (this way, compound viewables are completely
;;; cleaned up).  To allow user to return inferiors, or put external
;;; viewables into a local compound viewable, we only destroy orphans.
;;; We sort the list so that superiors come before their inferiors.
;;; This prevents local inferiors from being preserved because of the
;;; existence of their superiors!  In summary, the following will NOT
;;; be destroyed. 1) viewables that have a superior not on the list,
;;; 2) viewables on the *protected-viewables* list, 3) viewables with
;;; global symbol names, and 4) viewables displayed in pictures.
(defun destroy-viewables (&rest vbls)
  
  (declare (special *protected-viewables*)
	   (optimize (debug 3)))
  (break)
  ;; Destructively delete all non-viewables and duplicates:
  (setq vbls (delete-duplicates (delete-if-not #'viewable-p vbls)))
  ;; Destructively add inferiors:
  (loop for sub-list = vbls then (cdr sub-list) until (null sub-list)
	for vbl = (car sub-list)
	do (dolist (inf (inferiors-of vbl))
	     (unless (member inf vbls)
	       (rplacd sub-list (cons inf (cdr sub-list))))))
  ;; Destructively re-order list so superiors come before their inferiors:
  (setq vbls (sort-by-superiors! vbls))
  ;; Silently destroy orphaned viewables in list:
  (dolist (vbl vbls)
    (when (orphaned-viewable-p vbl) (destroy vbl :silent t)))
  t)

;;; Destructively sort a viewable list, ordering superiors before
;;; their inferiors.
(defun sort-by-superiors! (vbls)
  (loop with nthcdr = nil
	with max-count = (* 2 (length vbls))
	with sub-list = vbls
	for count from 0
	until (or (null sub-list) (> count max-count))
	for vbl = (car sub-list)
	for sups = (superiors-of vbl)
	for last-sup-pos = (loop for sup in sups
				 maximize (or (position sup sub-list) 0))
	do (cond ((> last-sup-pos 0)	;if sup in list, put vbl after it.
		  (setq nthcdr (nthcdr last-sup-pos sub-list))
		  (setf (cdr nthcdr) (cons vbl (cdr nthcdr)))
		  (setf (car sub-list) (cadr sub-list))
		  (setf (cdr sub-list) (cddr sub-list)))
		 (t (setq sub-list (cdr sub-list)))))
  vbls)

#| 
;;; *** Should use something like this instead of sort, but watch for
;;; circular lists!
(defun sort-by-superiors (&rest vbls)
  (loop with vbl-tree = (loop for vbl in vbls collect (list vbls))
	for sub-tree = vbl-tree
	until (null (cdr sub-tree))
	for first-chain = (car sub-tree)
	do
	(loop for next-chain in (cdr sub-tree)
	      for pos = (position (car first-chain)
				  next-chain
				  #'key 'inferiors-of
				  #'test #'member)
	      until pos
	      finally (when pos
			;; insert in next-chain
			(setq pos (nthcdr pos next-chain))
			(rplacd pos (append first-chain (cdr pos)))
			;; delete from vbl-tree
			(rplaca sub-tree (cadr sub-tree))
			(rplacd sub-tree (cddr sub-tree))))
	finally
	(return (apply #'nconc vbl-tree))))
|#	
	


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
