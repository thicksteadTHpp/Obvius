;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  glfw-nk-.lisp
;;;  Author: [THO]
;;;  Description: methods whicha are colled from nseide the render loop
;;;               to draw nk-windows and content
;;;  Creation Date: 2019
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

;;; displays the current picture which is on top of picture stack
;;; this function is called from inside the render-loop
;;[TODO] maybe locking
;; function which cycles through all panes (windows) of the current screen
;; and calls %display-pictures on them
(defun %display-panes (disp nk-context)
  (loop for pane in (pane-list disp)
	for picture = (first (picture-stack pane))
        if picture do (with-slots (system-dependent-frob y-offset x-offset zoom) picture
		       (render pane  system-dependent-frob y-offset x-offset zoom))
	  else       do (render pane nil T T 1)))
	

;;draws an empty window (pane) used when pane is cleared
;; or no picture is on the stack
(defun draw-empty-nk-win (pane &optional (ctx (get-context pane)))
  (let (;;(+height+ (y-dim pane))
	;;(+width+  (x-dim pane))
	(+rect+   (slot-value pane 'nk-rect))
	(+wid+    (slot-value pane 'wid))
	(+title+  (slot-value pane 'title)))
    (claw:c-val ((rect (:struct (%nk:rect)) +rect+))
      (let ((val (%nk:begin-titled ctx +wid+ +title+  rect 
			    (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
				    %nk:+window-minimizable+ %nk:+window-title+))))
	  (%nk:end ctx)))))
  

;;[TODo] check if pane  is nk-pane
;; (defmethod compute-picture :after (pic vbl)
;;   (setf (status (pane-of pic)) :realized))
				

;;around method to render
;; call render only from inside the render loop
;; otherwise no gl-context is available
(defmethod render :around ((pane nk-pane) frob y-offset x-offset zoom)
  (when *in-render-loop*
    (if  (eq (status pane) :cleared)
	 (if (null frob)
	     (draw-empty-nk-win pane)
	     (prog1 (draw-empty-nk-win pane)
	       (setf (status pane) :realized)))
	 (prog1 (call-next-method)
	   (setf (status pane) :realized)))))


(defmethod render ((pane nk-pane) (frob (eql nil)) y-offset x-offset zoom)
  (draw-empty-nk-win pane))
 
;;renders bltables (arrays of pixels)
(defmethod render :after ((pane nk-pane) (bltable gl-bltable) y-offset x-offset zoom)
    ;; draw empty win if pane is cleared
  ;; but dont' loose picture stack
  (claw:c-val ((ctx (:struct (%nk:context)) (get-context pane)))
    (let ((+rect+  (slot-value pane 'nk-rect))
	  (+title+ (slot-value pane 'title))
	  (+wid+   (slot-value pane 'wid))
	  (+height+ (float (first (base-dimensions bltable)) 1f0))
	  (+width+  (if (second (base-dimensions bltable)) (second (base-dimensions bltable)) 1))
	  (+image+ (slot-value bltable 'nk-image)))
      (unless (claw:wrapper-null-p +rect+) 
	(let ((val (%nk:begin-titled ctx +wid+ +title+ +rect+
				     (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
					     %nk:+window-minimizable+ %nk:+window-title+))))
	  (unless (= val 0)
	    (unless (claw:wrapper-null-p +image+)
	      (%nk:layout-row-static ctx +height+ +width+ 1)
	      (when (/= 0 (%nk:button-image ctx +image+))
		(vom:info "[image] button pressed pic-id: ~d" (index (first (picture-stack pane ))))
		(setf *current-pane* pane))))
	  (%nk:end ctx))))))


;; renders drawables
;; [THO] TODO check if we can use this for all drawables
;; we need to get a painter to the nk-window
(defmethod render :around ((pane nk-pane) (gf graph-frob) y-offset x-offset zoom)
  (claw:c-val ((ctx (:struct (%nk:context)) (get-context pane)))
    (let ((+rect+  (slot-value pane 'nk-rect))
	  (+title+ (slot-value pane 'title))
	  (+wid+   (slot-value pane 'wid))
	  (+height+ (padded-height pane))
	  (+width+  (padded-width pane)))
      (claw:c-with ((r (:struct (%nk:rect)) :calloc T)
		    (rgb (:struct (%nk:color)) :calloc nil)
		    (total-space (:struct (%nk:rect)))
		    (background (:struct (%nk:style-item)) :from (ctx :style :window :fixed-background))
		    (font (:struct (%nk:font)) :from (font-helper-font-handle (font-helper pane))))
	(%nk:recti +rect+ 50 50 +width+ +height+)
	;;(%nk:window-set-bounds ctx +title+ (%nk:recti +rect+ 0 0 +width+ +height+))
	
	    (let ((val (%nk:begin ctx +title+ (%nk:recti +rect+ 50 50 +width+ +height+) (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+ %nk:+window-minimizable+ %nk:+window-title+))))
	      (unless (= val 0)
		(%nk:window-get-content-region total-space ctx)
		(%nk:layout-row-dynamic ctx (total-space :h) 1)
		(%nk:widget total-space ctx)
		(claw:c-with ((painter (:struct (%nk:command-buffer)) :from (%nk:window-get-canvas ctx)))
		  (setf (slot-value pane 'canvas) (make-nk-canvas :context ctx
								  :painter painter
								  :drawing-color (%nk:rgb rgb 247 230 154)
								  :window-background background
								  :total-space total-space
								  :rect r))
		  ;; call render with frob and offset from total space
		  ;; this does not affect (call-next-method)
		  ;; (setf y-offset (total-space :y)
		  ;; 	x-offset (total-space :x))
		  (call-next-method pane gf (total-space :y) (+ 20 (total-space :x)) 1)))
	      (%nk:end ctx)
	      (setf (slot-value pane 'canvas) nil))))))




(defparameter +height+ 0)
(defparameter +width+ 0)
(defparameter +rect+ nil)
(defparameter +title+ *empty-pane-string*)
(defparameter +wid+ nil)

(defparameter +count-0+ 0)


       
;;use this or the graph will be very small
(defmethod compute-picture :before ((pic graph) vbl)
  (setf (zoom pic) 1.0))
