;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-pane.lisp
;;;  Author: Patrick C. Teo
;;;  Description: Generic OBVIUS pane handling stuff
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GL Pane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-simple-class GL-pane (pane) ())

(defmethod set-pane-title-bar ((pane GL-pane) title)
  (setf (label pane) (string-right-trim "." title)))

;;; Lucid bug 7126
(defmethod (setf status) ((val symbol) (pane GL-pane)))
(defmethod (setf status) ((val (eql :destroyed)) (pane GL-pane))
  (destroy pane))

;;; Set up picture specific mouse bindings (see gl-mouse.lisp)
(defmethod draw-pane :after ((pane GL-pane) &rest keys)
  (declare (ignore keys))	  
  (unless (eq (status pane) :destroyed)
    (set-picture-specific-mouse-bindings pane)))

;;; Should take this opportunity to do something to indicate
;;; selected pane. 
(defmethod set-selected-pane ((pane GL-pane))
  (when (not (eq pane *current-pane*))
    (call-next-method)))

(defmethod destroy :after ((pane GL-pane) &key &allow-other-keys))
(defmethod clear ((pane GL-pane) &key
		  (y0 0) (x0 0)
		  (y1 (y-dim pane))
		  (x1 (x-dim pane))
		  (color (background pane)))
  (GL:with-GL-lock (GL:winset (wid pane)))
  (draw-rect pane y0 x0 y1 x1 :foreground color :fill-p t))

;;; What shall we do with (background pane)/(foreground pane) and setf's??



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GL 8-bit Pane
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class 8bit-GL-pane (GL-pane 8bit-GL-window) ()
  (:default-initargs
      :width 300
      :height 256
      :label "Obvius Pane"))

;;; REQUIRED: (make-pane)
(defmethod make-pane ((screen 8bit-GL-screen) &rest keys
		      &key left bottom right top width height border-width)
  (declare (ignore width height left bottom right top border-width))
  (let ((new-pane (apply 'make-instance '8bit-GL-pane :screen-of screen keys)))
    (setf (status new-pane) :realized)
    (clear new-pane)
    new-pane))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GL 24-bit Pane
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class 24bit-GL-pane (GL-pane 24bit-GL-window) ()
  (:default-initargs
      :width 300
      :height 256
      :label "Obvius Pane"))

;;; REQUIRED: (make-pane)
(defmethod make-pane ((screen 24bit-GL-screen) &rest keys
		      &key left bottom right top width height border-width)
  (declare (ignore width height left bottom right top border-width))
  (let ((new-pane (apply 'make-instance '24bit-GL-pane :screen-of screen keys)))
    (setf (status new-pane) :realized)
    (clear new-pane)
    new-pane))



#|

What's not done:
================

(1)  Iconize window
(2)  Resize window -- is this possible with GL?
(3)  Setting foreground/background
(4)  Destroy -- what do we have to free?

|#

