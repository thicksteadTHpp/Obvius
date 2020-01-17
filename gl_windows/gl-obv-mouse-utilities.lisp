;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-mouse-utilities.lisp
;;;  Author: Patrick C. Teo
;;;  Description: OBVIUS-GL mouse utilities
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)




(defun remove-interest (pane interest-class)
  (setf (interests pane)
	(remove-if #'(lambda (interest) (typep interest interest-class))
		   (interests pane))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Documented Mouse Interest
;;;
;;; Stolen largely from lv-mouse-utilities.  Displays the
;;; documentation of the left/middle/right button mouse
;;; event.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class documented-mouse-interest (GL-mouse-interest)
  ((doc-string :initform "Undocumented")))

(defun compute-mouse-documentation (pane)
  (let* ((modifiers (make-mouse-modifier-gesture))
	 (l-interest (match-mouse-event-spec `(,modifiers (:left :down)) pane))
	 (l-doc (if (typep l-interest 'documented-mouse-interest)
		    (doc-string l-interest)
		    "Unbound"))
	 (m-interest (match-mouse-event-spec `(,modifiers (:middle :down)) pane))
	 (m-doc (if (typep m-interest 'documented-mouse-interest)
		    (doc-string m-interest)
		    "Unbound"))
	 (r-interest (match-mouse-event-spec `(,modifiers (:right :down)) pane))
	 (r-doc (if (typep r-interest 'documented-mouse-interest)
		    (doc-string r-interest)
		    "Unbound")))
    (values l-doc m-doc r-doc)))

(def-simple-class mouse-entry-interest (GL-window-manager-interest) ()
  (:default-initargs :window-manager-interest-gesture :enter))

(def-simple-class mouse-exit-interest (GL-window-manager-interest) ()
  (:default-initargs :window-manager-interest-gesture :exit))	  

(def-simple-class redraw-interest (GL-window-manager-interest) ()
  (:default-initargs :window-manager-interest-gesture :redraw))

(def-simple-class quit-interest (GL-window-manager-interest) ()
  (:default-initargs :window-manager-interest-gesture :quit))

(def-simple-class close-interest (GL-window-manager-interest) ()
  (:default-initargs :window-manager-interest-gesture :close))

(def-simple-class modifier-change-interest (GL-keyboard-interest) ()
  (:default-initargs :keyboard-interest-gesture '(:modifier (or :up :down))))

(defmethod receive-event ((pane GL-pane) (interest mouse-entry-interest) event)
  (declare (ignore event))
  (multiple-value-bind (l-doc m-doc r-doc)
	(compute-mouse-documentation pane)
    (display-mouse-documentation pane l-doc m-doc r-doc)))

(defmethod receive-event ((pane GL-pane) (interest modifier-change-interest) event)
  (declare (ignore event))
  (multiple-value-bind (l-doc m-doc r-doc)
	(compute-mouse-documentation pane)
    (display-mouse-documentation pane l-doc m-doc r-doc)))

(defmethod receive-event ((pane GL-pane) (interest mouse-exit-interest) event)
  (declare (ignore event))
  (display-mouse-documentation pane "" "" ""))

(defmethod receive-event ((pane GL-pane) (interest redraw-interest) event)
  (declare (ignore event))
  (initialize-2d-viewport pane)
  (draw-pane pane :clear t))

(defmethod receive-event ((pane GL-pane) (interest quit-interest) event)
  (declare (ignore event))
  (destroy pane))



;;;
;;; Tests
;;;
#|
(in-package 'obv)

(def-simple-class left-interest (documented-mouse-interest) ()
  (:default-initargs :doc-string "left doc" :mouse-interest-gesture '((:meta :up) (:left :down))))

(def-simple-class middle-interest (documented-mouse-interest) ()
  (:default-initargs :doc-string "middle doc" :mouse-interest-gesture '((:meta :up) (:middle :down))))

(def-simple-class right-interest (documented-mouse-interest) ()
  (:default-initargs :doc-string "right doc" :mouse-interest-gesture '((:meta :up) (:right :down))))

(def-simple-class meta-left-interest (documented-mouse-interest) ()
  (:default-initargs :doc-string "meta-left doc" :mouse-interest-gesture '((:meta :down) (:left :down))))

(defmethod receive-event ((pane GL-pane) (interest left-interest) event)
  (format t "received a left mouse click~%"))

(defmethod receive-event ((pane GL-pane) (interest middle-interest) event)
  (format t "received a middle mouse click~%"))

(defmethod receive-event ((pane GL-pane) (interest right-interest) event)
  (format t "received a right mouse click~%"))

(defmethod receive-event ((pane GL-pane) (interest meta-left-interest) event)
  (format t "received a meta-left mouse click~%"))

;;; Make a "self-documenting" window
(setq my-pane (new-pane))
(setq interests
      (list (make-instance 'left-interest)
	    (make-instance 'middle-interest)
	    (make-instance 'right-interest)
	    (make-instance 'meta-left-interest)
	    (make-instance 'mouse-entry-interest)
	    (make-instance 'mouse-exit-interest)
	    (make-instance 'modifier-change-interest)))
(setf (interests my-pane) interests)
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dragging Interest
;;;
;;; This is implemented slightly differently from
;;; lv-mouse-utilities.
;;;
;;; Dragging interests are defined by a begin-drag,
;;; move-drag and end-drag interest for which the begin-drag
;;; interest has to have the move- and end- drag interests
;;; specified as one of its fields.  This class is ONLY
;;; an abstract superclass and should not be instantiated.
;;; This is because it has (receive-event) methods which
;;; are supposed to be invoked prior to the (receive-event)
;;; methods of its specialized class.  The begin-drag
;;; method saves away the windows current interest list
;;; and replaces it with the move- and end- interest while
;;; the end-drag interest restores the window's original
;;; interest list.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class begin-drag-interest (documented-mouse-interest)
  ((move-drag-interest :initform nil)
   (end-drag-interest :initform nil)))

(def-simple-class move-drag-interest (GL-mouse-interest) ())

(def-simple-class end-drag-interest (GL-mouse-interest)
  ((old-interests :initform nil)))


;;;
;;; We remove all the previous interests and store them in the end-
;;; interest instance and insert a move-drag-interest and a
;;; end-drag-interest.
;;;	
(defmethod receive-event ((pane GL-pane) (interest begin-drag-interest) (event GL-mouse-event))
  (with-slots (move-drag-interest end-drag-interest) interest
    (add-device (screen-of pane) GL:mousex)
    (add-device (screen-of pane) GL:mousey)
    (setf (old-interests end-drag-interest) (interests pane))
    (setf (interests pane) `(,end-drag-interest ,move-drag-interest))))


;;;
(defmethod receive-event ((pane GL-pane) (interest move-drag-interest) (event GL-mouse-event)))


;;;
;;; We reinstall the old interests that was cached away in end-drag-interest
;;; and unqueue the mousex and mousey devices.
;;;
(defmethod receive-event ((pane GL-pane) (interest end-drag-interest) (event GL-mouse-event))
  (remove-device (screen-of pane) GL:mousex)
  (remove-device (screen-of pane) GL:mousey)
  (setf (interests pane) (old-interests interest))
  (multiple-value-bind (l-doc m-doc r-doc)
      (compute-mouse-documentation pane)
    (display-mouse-documentation pane l-doc m-doc r-doc)))


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-simple-class move-new-drag-interest (GL-mouse-interest)
  ((drag-modifiers     :initform nil)
   (old-interests      :initform nil)
   (last-event-p       :initform nil)))

(def-simple-class end-new-drag-interest (GL-mouse-interest)
  ((old-interests      :initform nil)))

(def-simple-class begin-new-drag-interest (documented-mouse-interest)
  ((move-new-drag-interest :initform nil)
   (end-new-drag-interest  :initform nil)))

(defmethod receive-event ((pane GL-pane) (interest begin-new-drag-interest) (event GL-mouse-event))
  (with-slots (move-new-drag-interest end-new-drag-interest) interest
    (with-slots (last-event-p) move-new-drag-interest
      (add-device (screen-of pane) GL:mousex)
      (add-device (screen-of pane) GL:mousey)
      (setf last-event-p nil)
      (setf (old-interests move-new-drag-interest) (interests pane)
	    (old-interests end-new-drag-interest) (interests pane))
      (setf (interests pane) (list move-new-drag-interest end-new-drag-interest)))))

(defmethod receive-event ((pane GL-pane) (interest move-new-drag-interest) (event GL-mouse-event))
  (with-slots (old-interests drag-modifiers last-event-p) interest
    (with-slots (mouse-event-gesture) event
      (when (and (not last-event-p)
		 (setf last-event-p (eq (check-modifier drag-modifiers mouse-event-gesture) 'fail)))
	(remove-device (screen-of pane) GL:mousex)
	(remove-device (screen-of pane) GL:mousey)
	(setf (interests pane) old-interests)
	(multiple-value-bind (l-doc m-doc r-doc)
	  (compute-mouse-documentation pane)
	  (display-mouse-documentation pane l-doc m-doc r-doc))))))
	
(defmethod receive-event ((pane GL-pane) (interest end-new-drag-interest) (event GL-mouse-event))
  (with-slots (old-interests) interest
    (remove-device (screen-of pane) GL:mousex)
    (remove-device (screen-of pane) GL:mousey)
    (setf (interests pane) old-interests)
    (multiple-value-bind (l-doc m-doc r-doc)
      (compute-mouse-documentation pane)
      (display-mouse-documentation pane l-doc m-doc r-doc))))
	    




    
    

	
    



