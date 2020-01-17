;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-mouse.lisp
;;;  Author: Patrick C. Teo
;;;  Description: OBVIUS-GL mouse events
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)


(defparameter *left-mouse-action* '(:left :down))
(defparameter *left-mouse-end-action* '(:left :up))
(defparameter *middle-mouse-action* '(:middle :down))
(defparameter *middle-mouse-end-action* '(:middle :up))
(defparameter *right-mouse-action* '(:right :down))
(defparameter *right-mouse-end-action* '(:right :up))
(defparameter *mouse-move-action* '(or :move-x :move-y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Picture specific mouse interests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-simple-class picture-specific-mouse-interest (documented-mouse-interest) ())

(defun set-picture-specific-mouse-bindings (pane)
  (remove-interest pane (find-class 'picture-specific-mouse-interest))
  (setf (interests pane)
	(append (picture-specific-mouse-interests (car (picture-stack pane)))
		(interests pane))))

(defmethod picture-specific-mouse-interests ((pic t)) nil)
  
(defmacro when-pane-unlocked (pane . body)
  `(if (locked ,pane)
    (status-message "Locked pane: mouse event ignored.")
    (progn ,@body)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Raw mouse bindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *raw-mouse-modifier* '(and (:meta :up) (:shift :up) (:ctrl :up)))

(def-simple-class select-viewable-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Select Viewable"
      :mouse-interest-gesture `(,*raw-mouse-modifier* ,*left-mouse-action*)))

(def-simple-class refresh-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Refresh"
      :mouse-interest-gesture `(,*raw-mouse-modifier* ,*middle-mouse-action*)))

;;; select-pane-interests requires dragging!! (TO BE CHANGED!!)
(def-simple-class select-pane-move-drag-interest (move-new-drag-interest) ()
  (:default-initargs
      :drag-modifiers *right-mouse-action*
      :mouse-interest-gesture `(nil ,*mouse-move-action*)))

(def-simple-class select-pane-end-drag-interest (end-new-drag-interest) ()
  (:default-initargs
      :mouse-interest-gesture `(nil ,*right-mouse-end-action*)))

(def-simple-class select-pane-begin-drag-interest (begin-new-drag-interest) ()
  (:default-initargs
      :doc-string "Select Pane"
      :mouse-interest-gesture `(,*raw-mouse-modifier* ,*right-mouse-action*)
      :move-new-drag-interest (make-instance 'select-pane-move-drag-interest)
      :end-new-drag-interest (make-instance 'select-pane-end-drag-interest)))

;;; select-pane-begin-drag-interest
(defmethod receive-event :after ((pane GL-pane)
				 (interest select-pane-begin-drag-interest)
				 (event GL-mouse-event))
  (set-selected-pane pane)
  (when (picture-stack pane)
    (let ((pic (car (picture-stack pane))))
      (position-message pic (viewable pic) pane (mouse-event-y event) (mouse-event-x event)))))

;;; select-pane-move-drag-interest
(defmethod receive-event :after ((pane GL-pane)
				 (interest select-pane-move-drag-interest)
				 (event GL-mouse-event))
  (when (picture-stack pane)
    (let ((pic (car (picture-stack pane))))
      (position-message pic (viewable pic) pane (mouse-event-y event) (mouse-event-x event)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control mouse bindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *ctrl-mouse-modifier* '(and (:meta :up) (:shift :up) (:ctrl :down)))

(def-simple-class previous-picture-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Previous Picture"
      :mouse-interest-gesture `(,*ctrl-mouse-modifier* ,*left-mouse-action*)))

(def-simple-class next-picture-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Next Picture"
      :mouse-interest-gesture `(,*ctrl-mouse-modifier* ,*middle-mouse-action*)))

(def-simple-class move-to-here-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Move to Here"
      :mouse-interest-gesture `(,*ctrl-mouse-modifier* ,*right-mouse-action*)))


  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shift mouse bindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *shift-mouse-modifier* '(and (:meta :up) (:shift :down) (:ctrl :up)))

(def-simple-class zoom-in-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Zoom in"
      :mouse-interest-gesture `(,*shift-mouse-modifier* ,*left-mouse-action*)))

(def-simple-class zoom-out-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Zoom out"
      :mouse-interest-gesture `(,*shift-mouse-modifier* ,*right-mouse-action*)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Meta mouse bindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *meta-mouse-modifier* '(and (:meta :down) (:shift :up) (:ctrl :up)))

(def-simple-class describe-viewable-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Describe Viewable"
      :mouse-interest-gesture `(,*meta-mouse-modifier* ,*left-mouse-action*)))

(def-simple-class set-parameter-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Set Picture Parameters"
      :mouse-interest-gesture `(,*meta-mouse-modifier* ,*middle-mouse-action*)))

(def-simple-class picture-menu-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Picture Menu"
      :mouse-interest-gesture `(,*meta-mouse-modifier* ,*right-mouse-action*)))



      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control shift mouse bindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *ctrl-shift-mouse-modifier* '(and (:meta :up) (:shift :down) (:ctrl :down)))

(def-simple-class pop-picture-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Pop Picture"
      :mouse-interest-gesture `(,*ctrl-shift-mouse-modifier* ,*left-mouse-action*)))

(def-simple-class center-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Center"
      :mouse-interest-gesture `(,*ctrl-shift-mouse-modifier* ,*middle-mouse-action*)))

;;; New Drag Picture interests!!!
(def-simple-class new-drag-picture-move-drag-interest (move-new-drag-interest) ()
  (:default-initargs
      :drag-modifiers *right-mouse-action*
      :mouse-interest-gesture `(nil ,*mouse-move-action*)))

(def-simple-class new-drag-picture-end-drag-interest (end-new-drag-interest) ()
  (:default-initargs
      :mouse-interest-gesture `(nil ,*right-mouse-end-action*)))

(def-simple-class new-drag-picture-begin-drag-interest (begin-new-drag-interest) ()
  (:default-initargs
      :doc-string "Drag Picture"
      :mouse-interest-gesture `(,*ctrl-shift-mouse-modifier* ,*right-mouse-action*)    
      :move-new-drag-interest (make-instance 'new-drag-picture-move-drag-interest)
      :end-new-drag-interest (make-instance 'new-drag-picture-end-drag-interest)))

(let (old-x old-y)
  (defmethod receive-event :after ((pane GL-pane)
				   (interest new-drag-picture-begin-drag-interest)
				   (event GL-mouse-event))
    (with-slots (mouse-event-x mouse-event-y) event
      (set-cursor GL-target-cursor)
      (setf old-x mouse-event-x
	    old-y mouse-event-y)))

  (defun move-drag (pic end-y end-x)
    (unless (or (and (minusp old-x) (minusp old-y))
		(and (zerop (- end-x old-x)) (zerop (- end-y old-y))))
	(drag-picture pic (- end-y old-y) (- end-x old-x)))
      (setf old-x end-x old-y end-y))
    
  (defmethod receive-event :after ((pane GL-pane)
				   (interest new-drag-picture-move-drag-interest)
				   (event GL-mouse-event))
    (with-slots (mouse-event-x mouse-event-y) event
      (with-slots (last-event-p) interest
	(move-drag (car (picture-stack pane)) mouse-event-y mouse-event-x)
	(when last-event-p (set-cursor :default)))))

  (defmethod receive-event :after ((pane GL-pane)
				   (interest new-drag-picture-end-drag-interest)
				   (event GL-mouse-event))

    (with-slots (mouse-event-x mouse-event-y) event
      (move-drag (car (picture-stack pane)) mouse-event-y mouse-event-x)
      (set-cursor :default))))
	     





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control shift meta mouse bindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *ctrl-shift-meta-mouse-modifier* '(and (:meta :down) (:shift :down) (:ctrl :down)))

(def-simple-class destroy-viewable-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Destroy Viewable"
      :mouse-interest-gesture `(,*ctrl-shift-meta-mouse-modifier* ,*left-mouse-action*)))

(def-simple-class hardcopy-interest (documented-mouse-interest) ()
  (:default-initargs
      :doc-string "Hardcopy"
      :mouse-interest-gesture `(,*ctrl-shift-meta-mouse-modifier* ,*right-mouse-action*)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shift meta mouse bindings (picture specific)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *shift-meta-mouse-modifier* '(and (:meta :down) (:shift :down) (:ctrl :up)))

(def-simple-class x-slice-interest (picture-specific-mouse-interest) ()
  (:default-initargs
      :doc-string "X Slice"
      :mouse-interest-gesture `(,*shift-meta-mouse-modifier* ,*left-mouse-action*)))

(def-simple-class y-slice-interest (picture-specific-mouse-interest) ()
  (:default-initargs
      :doc-string "Y Slice"
      :mouse-interest-gesture `(,*shift-meta-mouse-modifier* ,*middle-mouse-action*)))

(def-simple-class crop-picture-move-drag-interest (move-new-drag-interest) ()
  (:default-initargs
    :drag-modifiers *right-mouse-action*
    :mouse-interest-gesture `(nil ,*mouse-move-action*)))

(def-simple-class crop-picture-end-drag-interest (end-new-drag-interest) ()
  (:default-initargs
      :mouse-interest-gesture `(nil ,*right-mouse-end-action*)))

(def-simple-class crop-picture-begin-drag-interest
    (picture-specific-mouse-interest begin-new-drag-interest) ()
  (:default-initargs
    :doc-string "Crop Picture"
    :mouse-interest-gesture `(,*shift-meta-mouse-modifier* ,*right-mouse-action*)
    :move-new-drag-interest (make-instance 'crop-picture-move-drag-interest)
    :end-new-drag-interest (make-instance 'crop-picture-end-drag-interest)))



(let ((left 0) (top 0) (right 0) (bottom 0) x0 y0 old-x old-y drawn-p active-p)
  
  (defmethod receive-event :after ((pane GL-pane)
				   (interest crop-picture-begin-drag-interest)
				   (event GL-mouse-event))
    (with-slots (mouse-event-x mouse-event-y) event
      (let* ((pic (car (picture-stack pane)))
	     (vbl (viewable pic)))
	(multiple-value-bind (y x)
	  (pane-coord-to-viewable-coord pic mouse-event-y mouse-event-x)
	  (setf top (clip y 0 (y-dim vbl))
		left (clip x 0 (x-dim vbl))
		x0 mouse-event-x y0 mouse-event-y
		old-x -1 old-y -1
		active-p t drawn-p nil)
	  (status-message "Cropping... start: (~d ~d)" top left)))))

  (defun end-crop (pane end-y end-x &key (last-event-p t))
    (let* ((pic (car (picture-stack pane)))
	   (vbl (viewable pic)))
      (unless (or (not active-p)
		  (and (minusp old-x) (minusp old-y))
		  (and (zerop (- end-x old-x)) (zerop (- end-y old-y))))

	(GL:with-GL-lock
	  (GL:drawmode GL:pupdraw)
	  (when drawn-p (GL:color 0) (draw-rect pane y0 x0 old-y old-x))
	  (GL:color 7) (draw-rect pane y0 x0 end-y end-x)
	  (GL:drawmode GL:normaldraw))
	
	(multiple-value-bind (y x)
	  (pane-coord-to-viewable-coord pic end-y end-x)
	  (setf bottom y right x)
	  (status-message "Cropping... start: (~d ~d) end: (~d ~d) size: (~d ~d)"
			  top left bottom right
			  (abs (- top bottom)) (abs (- left right))))
			  
	(setf drawn-p t))
    
      (setf old-x end-x old-y end-y)
    
      (when last-event-p

	(GL:with-GL-lock
	  (GL:drawmode GL:pupdraw)
	  (when drawn-p (GL:color 0) (draw-rect pane y0 x0 old-y old-x))
	  (GL:drawmode GL:normaldraw))
	
	(when (and (/= left right) (/= bottom top))
	  (push-onto-eval-queue
	   `(display (crop ,vbl
		      :y ,(floor (min top bottom))
		      :x ,(floor (min left right))
		      :y-dim ,(floor (abs (- top bottom)))
		      :x-dim ,(floor (abs (- left right)))))))
	(setf active-p nil))))
    
  (defmethod receive-event :after ((pane GL-pane)
				   (interest crop-picture-move-drag-interest)
				   (event GL-mouse-event))
    (with-slots (last-event-p) interest
      (with-slots (mouse-event-x mouse-event-y) event
	(end-crop pane mouse-event-y mouse-event-x :last-event-p last-event-p))))

  (defmethod receive-event :after ((pane GL-pane)
				   (interest crop-picture-end-drag-interest)
				   (event GL-mouse-event))
    (with-slots (mouse-event-x mouse-event-y) event
      (end-crop pane mouse-event-y mouse-event-x :last-event-p t))))

      
	
  






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control meta mouse bindings (picture specific)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *ctrl-meta-mouse-modifier* '(and (:meta :down) (:shift :up) (:ctrl :down)))

(def-simple-class boost-contrast-interest (picture-specific-mouse-interest) ()
  (:default-initargs
      :doc-string "Boost Contrast"
      :mouse-interest-gesture `(,*ctrl-meta-mouse-modifier* ,*left-mouse-action*)))

(def-simple-class reduce-contrast-interest (picture-specific-mouse-interest) ()
  (:default-initargs
      :doc-string "Reduce Contrast"
      :mouse-interest-gesture `(,*ctrl-meta-mouse-modifier* ,*middle-mouse-action*)))

(def-simple-class histogram-interest (picture-specific-mouse-interest) ()
  (:default-initargs
      :doc-string "Histogram"
      :mouse-interest-gesture `(,*ctrl-meta-mouse-modifier* ,*right-mouse-action*)))


(def-simple-class reverse-sequence-interest (picture-specific-mouse-interest) ()
  (:default-initargs
      :doc-string "Previous Frame"
      :mouse-interest-gesture `(,*ctrl-meta-mouse-modifier* ,*left-mouse-action*)))

(def-simple-class step-sequence-interest (picture-specific-mouse-interest) ()
  (:default-initargs
      :doc-string "Next Frame"
      :mouse-interest-gesture `(,*ctrl-meta-mouse-modifier* ,*middle-mouse-action*)))

(def-simple-class display-sequence-interest (picture-specific-mouse-interest) ()
  (:default-initargs
      :doc-string "Display Sequence"
      :mouse-interest-gesture `(,*ctrl-meta-mouse-modifier* ,*right-mouse-action*)))

(def-simple-class stop-display-sequence-interest (documented-mouse-interest)
  ((flipbook :initform nil)
   (old-interests :initform nil))
  (:default-initargs
      :doc-string "Stop Sequence"
      :mouse-interest-gesture '(() (or :left :middle :right))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Picture specific interests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *crop-interests*
  `(,(make-instance 'crop-picture-begin-drag-interest)))

(defparameter *gray-mouse-interests*
  `(,(make-instance 'x-slice-interest)
    ,(make-instance 'y-slice-interest)
    ,(make-instance 'boost-contrast-interest)
    ,(make-instance 'reduce-contrast-interest)
    ,(make-instance 'histogram-interest)))

(defparameter *image-mouse-interests*
  (append *gray-mouse-interests* *crop-interests*))

(defparameter *overlay-mouse-interests*
  `(,(make-instance 'reverse-sequence-interest)
    ,(make-instance 'step-sequence-interest)))

(defparameter *sequence-mouse-interests*
  `(,(make-instance 'x-slice-interest)
    ,(make-instance 'y-slice-interest)
    ,(make-instance 'reverse-sequence-interest)
    ,(make-instance 'step-sequence-interest)
    ,(make-instance 'display-sequence-interest)))

(defparameter *default-pane-interests*
  `(,(make-instance 'refresh-interest)
    ,(make-instance 'quit-interest)
    ,(make-instance 'mouse-exit-interest)
    ,(make-instance 'mouse-entry-interest)
    ,(make-instance 'modifier-change-interest)
    ,(make-instance 'select-viewable-interest)
    ,(make-instance 'redraw-interest)

    ,(make-instance 'select-pane-begin-drag-interest)

    ,(make-instance 'previous-picture-interest)
    ,(make-instance 'next-picture-interest)
    ,(make-instance 'move-to-here-interest)
    ,(make-instance 'zoom-in-interest)
    ,(make-instance 'zoom-out-interest)
    ,(make-instance 'pop-picture-interest)
    ,(make-instance 'center-interest)

    ,(make-instance 'new-drag-picture-begin-drag-interest)

    ,(make-instance 'destroy-viewable-interest)
    ,(make-instance 'describe-viewable-interest)
    ,(make-instance 'set-parameter-interest)
    ,(make-instance 'picture-menu-interest)
    ,(make-instance 'hardcopy-interest)))
    



(defmethod picture-specific-mouse-interests ((pic gray)) *image-mouse-interests*)

;;; pasteups can't do crop
(defmethod picture-specific-mouse-interests ((pic pasteup)) *gray-mouse-interests*)

(defmethod picture-specific-mouse-interests ((pic overlay)) *overlay-mouse-interests*)

(defmethod picture-specific-mouse-interests ((pic flipbook)) *sequence-mouse-interests*)



  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Event Handlers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; catch-all in case of errors!
(defmethod receive-event (pane interest event)
  (declare (ignore pane event))
  (warn "No receive-event method defined for ~A" interest))

;;; select-viewable-interest
(defmethod receive-event ((pane GL-pane) (interest select-viewable-interest) event)
  (declare (ignore event))
  (insert-sexpr-for-evaluation (get-viewable pane)))

;;; refresh-interest
(defmethod receive-event ((pane GL-pane) (interest refresh-interest) event)
  (declare (ignore event))
  (let ((pic (car (picture-stack pane))))
    (if (and pic (not (current-p pic)))
	(push-onto-eval-queue `(refresh ,pane))
	(when-pane-unlocked pane (refresh pane)))))

;;; previous-picture-interest
(defmethod receive-event ((pane GL-pane) (interest previous-picture-interest) event)
  (declare (ignore event))
  (when-pane-unlocked pane
     (set-selected-pane pane)
     (cycle-pane pane 1)))

;;; next-picture-interest
(defmethod receive-event ((pane GL-pane) (interest next-picture-interest) event)
  (declare (ignore event))
  (when-pane-unlocked pane
     (set-selected-pane pane)
     (cycle-pane pane -1)))

;;; move-to-here-interest
(defmethod receive-event ((pane GL-pane) (interest move-to-here-interest) event)
  (declare (ignore event))
  (when-pane-unlocked pane
     (when (and (picture-stack *current-pane*) (not (eq pane *current-pane*)))
       (move-picture (car (picture-stack *current-pane*)) pane))
     (set-selected-pane pane)))

;;; zoom-in-interest
(defmethod receive-event ((pane GL-pane) (interest zoom-in-interest) (event GL-mouse-event))
  (when-pane-unlocked pane
    (zoom-picture (car (picture-stack pane)) 2 (- (y-dim pane) (mouse-event-y event)) (mouse-event-x event))))

;;; zoom-out-interest
(defmethod receive-event ((pane GL-pane) (interest zoom-out-interest) (event GL-mouse-event))
  (when-pane-unlocked pane
    (zoom-picture (car (picture-stack pane)) 1/2 (- (y-dim pane) (mouse-event-y event)) (mouse-event-x event))))		      

;;; pop-picture-interest
(defmethod receive-event ((pane GL-pane) (interest pop-picture-interest) event)
  (declare (ignore event))
  (when-pane-unlocked pane (pop-picture pane)))

;;; center-interest
(defmethod receive-event ((pane GL-pane) (interest center-interest) event)
  (declare (ignore event))
  (when-pane-unlocked pane
     (set-selected-pane pane)
     (when (picture-stack pane)
       (clear pane)
       (drag-picture (car (picture-stack pane)) nil nil))))

;;; destroy-viewable-interest
(defmethod receive-event ((pane GL-pane) (interest destroy-viewable-interest) event)
  (declare (ignore event))
  (when (picture-stack pane)
    (let ((vbl (viewable (car (picture-stack pane)))))
      (push-onto-eval-queue `(destroy ,vbl))
      (when-pane-unlocked pane (pop-picture pane)))))

;;; hardcopy-interest
(defmethod receive-event ((pane GL-pane) (interest hardcopy-interest) event)
  (declare (ignore event))
  (set-selected-pane pane)
  (let ((pic (car (picture-stack pane))))
    (push-onto-eval-queue `(hardcopy (viewable ,pic) ,pic))))

;;; describe-viewable-interest
(defmethod receive-event ((pane GL-pane) (interest describe-viewable-interest) event)
  (declare (ignore event))
  (let ((pic (car (picture-stack pane))))
    (push-onto-eval-queue
     `(when ,pic
       (describe (viewable ,pic))
       (print-top-level-values nil)))))

;;; set-parameter-interest
(defmethod receive-event ((pane GL-pane) (interest set-parameter-interest) event)
  (declare (ignore event))
  (status-message "Set Picture Parameters not implemented in this version."))

;;; picture-menu-interest
(defmethod receive-event ((pane GL-pane) (interest picture-menu-interest) event)
  (declare (ignore event))
  (status-message "Picture Menu not implemented in this version."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gray Event Handlers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; x-slice-interest
(defmethod receive-event ((pane GL-pane) (interest x-slice-interest) (event GL-mouse-event))
  (when (picture-stack pane)
    (let* ((pic (car (picture-stack pane)))
	   (vbl (viewable pic)))
      (multiple-value-bind (y x)
	  (pane-coord-to-viewable-coord pic (mouse-event-y event) (mouse-event-x event))
	(declare (ignore x))
	(push-onto-eval-queue
	 `(display (make-slice ,vbl :y ,y) t :pane ,pane))))))

;;; y-slice-interest
(defmethod receive-event ((pane GL-pane) (interest y-slice-interest) (event GL-mouse-event))
  (when (picture-stack pane)
    (let* ((pic (car (picture-stack pane)))
	   (vbl (viewable pic)))
      (multiple-value-bind (y x)
	  (pane-coord-to-viewable-coord pic (mouse-event-y event) (mouse-event-x event))
	(declare (ignore y))
	(push-onto-eval-queue
	 `(display (make-slice ,vbl :x ,x) t :pane ,pane))))))

;;; boost-contrast-interest
;;; change contrast, keeping pixel value which maps to 0.5 (middle gray) fixed.
(defmethod receive-event ((pane GL-pane) (interest boost-contrast-interest) event)
  (declare (ignore event))
  (when-pane-unlocked pane
    (let* ((pic (car (picture-stack pane)))
	   (new-scale (/ (slot-value pic 'scale) 1.5)))
      (reinitialize-instance pic
			     :scale new-scale
			     :pedestal (+ (slot-value pic 'pedestal)
					  (/ (- (slot-value pic 'scale) new-scale) 2)))
      (draw-pane pane :clear nil))))

;;; reduce-contrast-interest
(defmethod receive-event ((pane GL-pane) (interest reduce-contrast-interest) event)
  (declare (ignore event))
  (when-pane-unlocked pane
    (let* ((pic (car (picture-stack pane)))
	   (new-scale (* (slot-value pic 'scale) 1.5)))
      (reinitialize-instance pic
			     :scale new-scale
			     :pedestal (+ (slot-value pic 'pedestal)
					  (/ (- (slot-value pic 'scale) new-scale) 2)))
      (draw-pane pane :clear nil))))

;;; histogram-interest
(defmethod receive-event ((pane GL-pane) (interest histogram-interest) event)
  (declare (ignore event))
  (when (picture-stack pane)
    (let ((vbl (viewable (car (picture-stack pane)))))
      (push-onto-eval-queue `(display (make-histogram ,vbl) t :pane ,pane)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Flipbook Event Handlers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; reverse-sequence-interest
(defmethod receive-event ((pane GL-pane) (interest reverse-sequence-interest) event)
  (declare (ignore event))
  (when-pane-unlocked pane
     (let ((first-picture (car (picture-stack pane))))
       (when (or (typep first-picture 'flipbook) (typep first-picture 'overlay))
	 (single-step first-picture -1)))))


;;; step-sequence-interest
(defmethod receive-event ((pane GL-pane) (interest step-sequence-interest) event)
  (declare (ignore event))
  (when-pane-unlocked pane
     (let ((first-picture (car (picture-stack pane))))
       (when (or (typep first-picture 'flipbook) (typep first-picture 'overlay))
	 (single-step first-picture)))))


;;; display-sequence-interest
(defmethod receive-event ((pane GL-pane) (interest display-sequence-interest) event)
  (declare (ignore event))
  (let ((first-picture (car (picture-stack pane))))
    (when (typep first-picture 'flipbook)
      (let ((stop-interest (make-instance 'stop-display-sequence-interest
					  :flipbook first-picture
					  :old-interests (interests pane))))
	(push-onto-eval-queue `(display-seq ,first-picture))
	(setf (interests pane) (list stop-interest))))))

;;; stop-display-sequence-interest
(defmethod receive-event ((pane GL-pane) (interest stop-display-sequence-interest) event)
  (declare (ignore event))
  (with-slots (flipbook old-interests) interest
    (setf (slot-value flipbook 'displaying-p) nil
	  (interests pane) old-interests)))
  
