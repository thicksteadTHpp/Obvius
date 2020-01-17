;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: clos-extensions.lisp
;;;  Author: Eero Simoncelli
;;;  Description: Extensions to CLOS
;;;  Creation Date: 2/91
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;[tho] 2019-11-06
;;replaced the occurences of CLos-package with closer-mop

;;[tho] 2016-08-11
;;changed the (nth 2 ...) lookups on lists of
;;slot initargs to (nth 1 ...) like in mcl
;;same in sbcl
(in-package obvius)
(export '(def-simple-class object-class-name
	  get-defaults set-default
	  sub-plist plist-difference))

;;; Does a defclass, automatically setting up an initform, initarg and
;;; accessor for each slot if the user does not provide them.  Each
;;; slot specification is either a slot symbol or a list (as for
;;; defclass).  Default initform is nil.  Default accessor is the slot
;;; name.  Default initarg is the slot name preceded by a colon.
(defmacro def-simple-class (class-name superclasses slots &rest options)
  (loop with default-flag = (gensym)
	for form in slots
	for name = (if (listp form) (car form) form)
	for keys = (when (listp form) (cdr form))
	collect `(,name
		  ,@keys
		  ,@(when (eq (getf keys :initform default-flag) default-flag)
		      `(:initform nil))
		  ,@(when (eq (getf keys :initarg default-flag) default-flag)
		      `(:initarg ,(read-from-string (format nil ":~A" name))))
		  ,@(when (eq (getf keys :accessor default-flag) default-flag)
		      `(:accessor ,name)))
	into slot-list
	finally
	(return `(defclass ,class-name ,superclasses ,slot-list ,@options))))

;;; Returns a symbol corresponding to the class name of thing.  Can be used as
;;; an argument to make-instance.
(defun object-class-name (thing)
  (class-name (class-of thing)))

;;; Iterative depth-first search returns list of all subclasses,
;;; including the class itself.
(defun find-all-subclasses (root-class)
  (let ((the-list (list root-class)))
    (loop for sub-list = the-list then (cdr sub-list)
	  until (null sub-list)
	  for node = (car sub-list)
	  do
	 (dolist (sub-node (closer-mop:class-direct-subclasses node))
	    (unless (member sub-node the-list)
	      (rplacd sub-list (cons sub-node (cdr sub-list))))))
    the-list))

;;; Return list of slots user can adjust.  The primary dispatching is
;;; done on the class name (symbol), so that this method may be called
;;; without referring to a particular instance (e.g. for setting
;;; default values for the slots of the class).  It would be nice to
;;; do these with class-allocated slots, but then it is difficult to
;;; deal with inheritance and there is no standard way to access the
;;; value without consing an instance...
(defmethod settable-parameters ((object t))
  (settable-parameters (object-class-name object)))

;;; For classes with method not defined: calls method recursively up
;;; the inheritance hierarchy.  Also provides the termination case: t.
;;; Methods on specific class names should append a list of
;;; appropriate slot-names to the result of (call-next-method).
(defmethod settable-parameters ((class-name symbol))
  (unless (eq class-name t)
    (settable-parameters
     (class-name (cadr (closer-mop:class-precedence-list (find-class class-name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;[tho] 2019 let closer-mop handle that
;;;;
;;;; This is some generic code for reading and writing the default
;;;; values for CLOS slots and initargs.  It is a bit dependent on
;;;; Lucid's implementation of CLOS, but will stabilize as the
;;;; meta-object protocol stabilizes.

;;; NOTE: this is dependent on Lucid's current CLOS implementation.  We assume:
;;; 1) existence of functions CLOS::class-default-initargs CLOS::class-slots
;;;    CLOS::class-direct-slots CLOS::slot-definition-name CLOS::slot-definition-initargs
;;;    CLOS::slot-definition-initform
;;; 2) default-initargs in the form '(<initarg-name> <initfunction> <initform>)
;;; 3) That we can call setf on the following:
;;;         (slot-value <a-slot> 'CLOS::initform)
;;;         (slot-value <a-slot> 'CLOS::initfunction)
;;;         (slot-value <a-class> 'CLOS::default-initargs)
;;; POSSIBLE BUG: Seems like (slot-value <a-class> 'CLOS::default-initargs)
;;; contains both the direct and the inherited initargs.  How can we avoid modifying
;;; the inherited ones in set-default-internal?

;;; Top-level function similar to describe, but lists default values
;;; for slots and initargs of class.  Made more hairy because it tries
;;; to preserve the slot order.  *** Could be made more efficient!
;;; *** hard to keep this consistent with get-default:  Maybe we
;;; should just call get-default on each slot-or-initarg?
(defun get-defaults (class &key (print t) slot-names-or-initargs)
  (when (symbolp class) (setq class (find-class class)))
  (let (default-list)
    (if slot-names-or-initargs
	(loop for name in slot-names-or-initargs
	      collect (get-default class name) into the-defaults
	      finally (setq default-list the-defaults))
	;; if not provided, cons up list of all slot-names-or-initargs of class
	(loop with all-default-initargs = (copy-list (closer-mop:class-default-initargs class))
	   with all-slots = (closer-mop:class-slots class)
	   for slot in all-slots
	   for initargs = (closer-mop:slot-definition-initargs slot)
	   for default-initargs =
	      (loop for initarg in initargs
		    for default-initarg = (find initarg all-default-initargs :key 'car)
		    when default-initarg collect default-initarg)
	      ;; If default-initargs for this slot, remove them from all-default-initargs
	      do (when default-initargs
		   (setq all-default-initargs
			 (nset-difference all-default-initargs default-initargs)))
	      collect (or (car (car default-initargs))
			  (car initargs)
			  (closer-mop:slot-definition-name slot))
	      into names
	      collect (or (nth 1 (car default-initargs))
			  (ignore-errors (closer-mop:slot-definition-initform slot)))
	      into defaults
	      finally (loop for initarg in all-default-initargs
			    do
			    (setq names (nconc names (list (car initarg))))
			    (setq defaults (nconc defaults (list (nth 1 initarg))))
			    finally (setq slot-names-or-initargs names
					  default-list defaults))))
    (cond (print (format t "~%Defaults for class ~S:~%" (closer-common-lisp:class-name class))
		 (loop for the-name in slot-names-or-initargs
		       for the-default in default-list
		       do (format t "  ~S ~25T~S~%" the-name the-default))
		 (values))
	  (t (loop for the-name in slot-names-or-initargs
		   for the-default in default-list
		   nconc (list the-name the-default))))))

;;; Top-level macro to allow altering the default form for a slot.  We
;;; use a macro so that the default form is not evaluated, as in the
;;; original (defclass) defaults.  If the class has a default-initarg
;;; with the given (keyword) name, alter it to use the new form.  If
;;; it has a direct slot with that name, set the initform of the slot.
;;; If it has an inherited slot with that name, add a default-initarg
;;; (i.e. we do not want to change the initform of the parent class!).
;;; Otherwise, signal an error.  *** The default will be evaluated in
;;; the global environment (not the lexical environment of the
;;; original defclass).
(defmacro set-default (class-name slot-name-or-initarg default-form &key silent)
  `(set-default-internal ,class-name ,slot-name-or-initarg ',default-form ,silent))

(defun set-default-internal (class-name slot-name-or-initarg new-default silent)
  (let* ((class (find-class class-name))
	 (default-initargs (slot-value class 'closer-mop:class-default-initargs)) ;;was clos::default-initargs
	 (the-slot (find-slot class slot-name-or-initarg t))
	 (initarg (find-default-initarg slot-name-or-initarg default-initargs the-slot))
	 (initfunction #'(lambda () (eval new-default))))
    (cond (initarg			;initarg?
	   (unless silent (format t ";;; Replacing default-initarg ~S~%" (car initarg)))
	   (setf (slot-value class 'closer-mop:class-default-initargs) ;;was default-initargs
		 (cons (list (car initarg) initfunction new-default)
		       (remove initarg default-initargs))))
	  ((find the-slot (closer-mop:class-direct-slots class)) ;direct slot?
	   (unless silent (format t ";;; Replacing initform of slot ~S~%" the-slot))
	   (setf (slot-value the-slot 'closer-mop:slot-definition-initform) new-default) ;;was initform
	   (setf (slot-value the-slot 'closer-mop:slot-definition-initfunction) initfunction))
	  (the-slot			;inherited slot?
	   (unless (setq initarg (car (closer-mop:slot-definition-initargs the-slot)))
	     (error "There are no initargs defined for slot ~A of class ~A"
		    slot-name-or-initarg class-name))
	   (unless silent (format t ";;; Adding default-initarg ~S~%" initarg))
	   (setf (slot-value class 'closer-mop:class-default-initargs)
		 (cons (list initarg initfunction new-default)
		       default-initargs)))
	  (t (error "~A is not a slot name or default-initarg of class ~A"
		    slot-name-or-initarg class-name)))
    new-default))

;;; Generic CLOS default reader.  If there is a default-initarg with
;;; the given name, return its value.  Else if there is a slot with
;;; the given name or initarg AND there is a default-initarg which
;;; applies to that slot, return its value.  Else if there is a slot
;;; with the given name or initarg, return the initform of the slot.
;;; Otherwise signal an error.
(defun get-default (class slot-name-or-initarg)
  (when (symbolp class) (setq class (find-class class)))
  (let* ((default-initargs (closer-mop:class-default-initargs class))
	 (the-slot (find-slot class slot-name-or-initarg t))
	 (initarg (find-default-initarg slot-name-or-initarg default-initargs the-slot)))
    (cond (initarg (nth 1 initarg))
	  (the-slot (closer-mop:slot-definition-initform the-slot))
	  (t (error "~A is a bad slot-name-or-initarg for class ~A"
		    slot-name-or-initarg class)))))

;;; Find and return the slot of the-class with the given slot-name or
;;; initarg.  If not found and no-errors is nil, signal an error.
;;; Otherwise return nil.
(defun find-slot (the-class slot-name-or-initarg &optional no-errors)
  (or (find slot-name-or-initarg
	    (closer-mop:class-slots the-class)
	    :key 'closer-mop:slot-definition-initargs
	    :test 'member)
      (find (symbol-name slot-name-or-initarg) ;check for slot-name, ignoring package!
	    (CLOS::class-slots the-class)
	    :key #'(lambda (slot) (symbol-name (closer-mop:slot-definition-name slot)))
	    :test 'equal)
      (unless no-errors
	(error "~A is a bad slot-name-or-initarg for class ~A"
	       slot-name-or-initarg the-class))))

;;; Return the default-initarg in the list of default-initargs
;;; corresponding to slot-name-or-initarg or the-slot.
(defun find-default-initarg (slot-name-or-initarg default-initargs &optional the-slot)
  (or (find slot-name-or-initarg default-initargs :key 'car)
      (find (intern (symbol-name slot-name-or-initarg) :keyword) default-initargs :key 'car)
      (and the-slot
	   (loop for arg in (closer-mop:slot-definition-initargs the-slot)
		 for default-initarg = (find arg default-initargs :key 'car)
		 when default-initarg return default-initarg))))

(defun find-slot-name-from-initarg (initarg class)
  (let ((slot (find initarg (CLOS::class-slots class)
		    :key 'closer-mop:slot-definition-initargs
		    :test 'member)))
    (when slot (CLOS::slot-definition-name slot))))

;;; If key is in the plist, return a list containing it and it's
;;; value.  Otherwise return nil.
(defun get-initarg (plist key &optional default-value)
  (let ((arg (getf plist key :NOT-PASSED)))
    (cond ((not (eq arg :NOT-PASSED))	;arg was passed
	   (list key arg))
	  (default-value (list key default-value))
	  (t nil))))

;;; return all keyword/arg pairs in plist1 that are NOT in plist2.
;;; Like set-difference, but operates on keyword/arg pairs
(defun plist-difference (plist1 plist2)
  (loop for subl = plist1 then (cddr subl) until (null subl)
	for key = (car subl)
	unless (getf plist2 key)
	append (list key (cadr subl))))

;;; Return a plist containing only the given keys
(defun sub-plist (plist &rest keys)
  (loop for subl = plist then (cddr subl) until (null subl)
	for key = (car subl)
	when (member key keys)
	append (list key (cadr subl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copy, similar on generic objects

;;; Define generic copy and similar functions. Useful for creating new
;;; objects.  copy is called recursively. Note that lists (as of
;;; 11.13.91) do not get copied recursively.  *** kludge, currently
;;; tries to copy if slot is anything but a number or symbol.  Should
;;; use find-method to look for a copy method instead.
(defmethod copy ((thing t) &key ((:-> result) (make-instance (class-of thing))))
  (loop for slot in (closer-mop:class-slots (class-of thing))
     for slot-name = (closer-mop:slot-definition-name slot)
	for slot-value = (slot-value thing slot-name)
	when slot-value
	do (cond ((or (symbolp slot-value) (numberp slot-value))
		  (setf (slot-value result slot-name) slot-value))
		 (t (setf (slot-value result slot-name) (copy slot-value)))))
  result)

(defmethod similar ((thing t) &rest initargs)
  (let ((result (apply 'make-instance (class-of thing) initargs)))
    (loop for slot in (closer-mop:class-slots (class-of thing))
	  for slot-name = (closer-mop:slot-definition-name slot)
	  for slot-value = (slot-value thing slot-name)
	  when slot-value
	  do
	  (cond ((or (symbolp slot-value) (numberp slot-value))
		 (setf (slot-value result slot-name) slot-value))
		(t (setf (slot-value result slot-name) (similar slot-value)))))
    result))

;;; *** this should be defined only on generic-objects.  Should it
;;; also call copy on the slot values?  Copy object, by passing
;;; slot-values as initarg keywords.  initargs arguments can be used
;;; to override the slot values.
(defmethod copy-object ((object t) &rest initargs)
  (loop with class = (class-of object)
	for slot in (closer-mop:class-slots class)
	for slot-name = (closer-mop:slot-definition-name slot)
	for slot-initarg = (car (closer-mop:slot-definition-initargs slot))
	when (and (not slot-initarg) (slot-boundp object slot-name))
	append (list slot-name (slot-value object slot-name)) into slot-values
	do (when (and slot-initarg
		      (slot-boundp object slot-name)
		      (eq (getf initargs slot-initarg :not-supplied) :not-supplied))
	     (setf (getf initargs slot-initarg) (slot-value object slot-name)))
	finally
	(let ((new-object (apply 'make-instance class initargs)))
	  (loop for name-and-val = slot-values then (cddr name-and-val)
		until (null name-and-val)
		do (setf (slot-value new-object (car name-and-val))
			 (cadr name-and-val)))
	  (return new-object))))

#| OLD version : calling empty make-instance is broken
;;; *** this should be defined only on generic-objects.  Should it also
;;; call copy on the slot values?
(defmethod copy-object ((object t))
  (let ((new-object (make-instance (class-of object))))
    (loop for slot in (CLOS::class-slots (class-of object))
	  for slot-name = (CLOS::slot-definition-name slot)
	  do (setf (slot-value new-object slot-name)
		   (slot-value object slot-name)))
    new-object))
|#

;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
