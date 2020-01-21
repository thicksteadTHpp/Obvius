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
;; (defun %display-pictures (pane nk-context)
;;   (let (pic bltable func)
;;   (and  (setf pic (first (picture-stack pane)))
;; 	(setf bltable (slot-value pic 'system-dependent-frob))
;; 	(setf func (slot-value bltable 'nk-func))
;; 	(funcall func nk-context))))

;; (defun %display-pictures (pane nk-context)
;;   (nk-display-picture (first (picture-stack pane)) nk-context))



;; (defgeneric nk-display-picture (picture context)
;;   (:method ((picture (eql nil)) ctx)
;;     ;; should not happen
;;     )
;;   (:method (picture ctx)
;;     ;; do nothing
;;     )
;;   (:method :around (picture ctx)
;;     ;; draw empty win if pane is cleared
;;     ;; but dont' loose picture stack
;;     (if (eq (status (pane-of picture)) :cleared)
;; 	(draw-empty-nk-win (pane-of picture) ctx)
;; 	(call-next-method)))
;;   (:method ((picture picture) ctx)
;;     (let ((bltable (when (slot-boundp picture 'system-dependent-frob)
;; 		     (slot-value picture 'system-dependent-frob)))
;; 	  (pane    (pane-of picture)))
;;       (when bltable  
;; 	(let ((+height+ (float (first (base-dimensions bltable)) 1f0))
;; 	      (+width+  (if (second (base-dimensions bltable)) (second (base-dimensions bltable)) 1))
;; 	      (+rect+  (slot-value bltable 'nk-rect))
;; 	      (+image+ (slot-value bltable 'nk-image))
;; 	      (+title+ (slot-value pane 'title))
;; 	      (+wid+   (slot-value pane 'wid)))
;; 	  (unless (or (claw:wrapper-null-p +image+)
;; 		      (claw:wrapper-null-p +rect+))
;; 	    (let ((val (%nk:begin-titled ctx +wid+ +title+ +rect+;;(simple-bounds +rect+ 50f0 50f0 +width+ +height+) 
;; 					 (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
;; 						 %nk:+window-minimizable+ %nk:+window-title+))))
;; 	      (unless (= val 0)
;; 		(%nk:layout-row-static ctx +height+ +width+ 1)
;; 		;;(%nk:image ctx +image+)
;; 		 (when (/= 0 (%nk:button-image ctx +image+))
;; 		   (vom:info "[image] button pressed pic-id: ~d" (index picture))
;; 		   (setf *current-pane* pane)))
;; 	      (%nk:end ctx))))))))

;;function which cycles through all panes (windows) of the current screen
;; and calls %display-pictures on them
(defun %display-panes (disp nk-context)
  (loop for pane in (pane-list disp)
	for picture = (first (picture-stack pane))
        if picture do (with-slots (system-dependent-frob y-offset x-offset zoom) picture
		       (render pane  system-dependent-frob y-offset x-offset zoom))
	  else       do (draw-empty-nk-win pane nk-context)))
	
	;; if (eq (status pane) :realized)
	;;   do (%display-pictures pane nk-context)
	;; if (eq (status pane) :cleared)
;;   do (draw-empty-nk-win pane nk-context)))

(defun draw-empty-nk-win (pane &optional (ctx (get-context pane)))
;;  (vom:info "[draw-empty-nk-win]")
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
(defmethod compute-picture :after (pic vbl)
  (setf (status (pane-of pic)) :realized))
				


(defmethod render :around ((pane nk-pane) frob y-offset x-offset zoom)
  (when *in-render-loop*
    (if  (eq (status pane) :cleared)
	 (draw-empty-nk-win pane)
	 (prog1 (call-next-method)
	   (setf (status pane) :realized)))))


(defmethod render ((pane nk-pane) (frob (eql nil)) y-offset x-offset zoom)
  (draw-empty-nk-win pane))
 

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
		(vom:info "[image] button pressed pic-id: ~d" (index picture))
		(setf *current-pane* pane))))
	  (%nk:end ctx))))))


;; (defmethod render :around ((pane nk-pane) (gf graph-frob) y-offset x-offset zoom)
;;   (claw:c-val ((ctx (:struct (%nk:context)) (get-context pane)))
;;     (let ((+rect+  (slot-value pane 'nk-rect))
;; 	  (+title+ (slot-value pane 'title))
;; 	  (+wid+   (slot-value pane 'wid))
;; 	  (+height+ (float (padded-height pane) 1f0))
;; 	  (+width+  (float (padded-width pane) 1f0)))
;;       (let ((val (%nk:begin-titled ctx +wid+ +title+ +rect+ (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
;; 								    %nk:+window-minimizable+ %nk:+window-title+))))
;; 	(unless (= val 0)
;; 	  (claw:c-with ((r (:struct (%nk:rect)) :calloc T)
;; 			(rgb (:struct (%nk:color)) :calloc nil)
;; 			(total-space (:struct (%nk:rect)))
;; 			 (background (:struct (%nk:style-item)) :from (ctx :style :window :fixed-background)))
;; 	    (%nk:window-get-content-region total-space ctx)
;; 	    (%nk:layout-row-dynamic ctx (total-space :h) 1)
;; 	    (%nk:widget total-space ctx)
;; 	    (claw:c-with ((painter (:struct (%nk:command-buffer)) :from (%nk:window-get-canvas ctx)))
;; 	      (setf (slot-value pane 'canvas) (make-nk-canvas :context ctx
;; 							      :painter painter
;; 							      :drawing-color (%nk:rgb rgb 247 230 154)
;; 							      :window-background background
;; 							      :total-space total-space
;; 							      :rect r))
;; 	      ;; call render with frob and offset from total space
;; 	      (render pane gf (total-space :y) (total-space :x) 1))))
;; 	(%nk:end ctx)
;; 	(setf (slot-value pane 'canvas) nil)))))


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
		  (setf y-offset (total-space :y)
			x-offset (total-space :x))
		  (call-next-method)))
		  ;;(%nk:fill-rect painter (%nk:recti +rect+ (floor (+ 15 (total-space :x))) (floor (+ 15 (total-space :y))) 210 210) 5.0 (%nk:rgb rgb 247 230 154)) 
		  ;;(%nk:fill-rect painter (%nk:recti r (+ 20 (floor (total-space :x))) (+ 20 (floor (total-space :y))) 200 200) 5.0 (%nk:rgb rgb 188 174 118))
		  ;;(%nk:draw-text painter (%nk:recti r (+ 30 (floor (total-space :x))) (+ 30 (floor (total-space :y))) 150 20) "Text to draw" 12 font (%nk:rgb rgb 188 174 118) (%nk:rgb rgb 0 0 0))))
	      (%nk:end ctx)
	      (setf (slot-value pane 'canvas) nil))))))



;; (defun nk-display-picture (picture ctx)
;;   (let ((frob (when (slot-boundp picture 'system-dependent-frob)
;; 		(slot-value picture 'system-dependent-frob))))
;;     (if (or (null frob)
;; 	    (eq (status (pane-of picture)) :cleared))
;; 	(draw-empty-nk-win (pane-of picture))
;; 	(nk-display-frob frob picture ctx)))) 

;;special vars
(defparameter +height+ 0)
(defparameter +width+ 0)
(defparameter +rect+ nil)
(defparameter +title+ *empty-pane-string*)
(defparameter +wid+ nil)

(defparameter +count-0+ 0)


;; (let ((toggles (make-hash-table))
;;       (toggle-id 0))
;;   (defun new-toggle ()
;;     (prog1 toggle-id
;;       (setf (gethash toggle-id toggles) nil)
;;       (incf toggle-id 1)))
;;   (defun toggle-value (id)
;;     (gethash id toggles))
;;   (defun toggle (id)
;;     (let ((val (gethash id toggles)))
;;       (setf (gethash id toggles) (not val))))
;;   (defun reset-toggles ()
;;     (clrhash toggles)))


(defmacro do-once (&body body)
  `(when (= 0 +count-0+)
     ,@body
     (incf +count-0+ 1)))



;; (defgeneric nk-display-frob (frob picture ctx)
;;   (:method ((frob (eql nil)) picture ctx)
;;     ;; do nothing
;;     )
;;   (:method (frob (picture picture) ctx)
;;   ;;dummy method
;;   )
;;   (:method :around (frob picture ctx)
;;     ;; draw empty win if pane is cleared
;;     ;; but dont' loose picture stack
;;     (let ((pane (pane-of picture)))
;;       (let ((+rect+  (slot-value pane 'nk-rect))
;; 	    (+title+ (slot-value pane 'title))
;; 	    (+wid+   (slot-value pane 'wid)))
;; 	(declare (special +rect+ +title+ +wid+))
;; 	(unless (claw:wrapper-null-p +rect+) 
;; 	  (let ((val (%nk:begin-titled ctx +wid+ +title+ +rect+
;; 				       (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
;; 					       %nk:+window-minimizable+ %nk:+window-title+))))
;; 	    (unless (= val 0)
;; 	      (call-next-method))
;; 	    (%nk:end ctx))))))
;;   (:method ((bltable gl-bltable) (picture picture) ctx)
;;     (let ((+height+ (float (first (base-dimensions bltable)) 1f0))
;; 	  (+width+  (if (second (base-dimensions bltable)) (second (base-dimensions bltable)) 1))
;; 	  (+image+ (slot-value bltable 'nk-image))
;; 	  (pane (pane-of picture)))
;;       (unless (claw:wrapper-null-p +image+)
;; 	(%nk:layout-row-static ctx +height+ +width+ 1)
;; 	(when (/= 0 (%nk:button-image ctx +image+))
;; 	  (vom:info "[image] button pressed pic-id: ~d" (index picture))
;; 	  (setf *current-pane* pane)))))
;;     ;;
	
	
  ;; (%nk:fill-rect painter (%nk:recti r (floor (total-space :x)) (floor (total-space :y)) 200 200) 5f0 (%nk:rgb rgb 247 230 154))))))

;;amybe we should specialise on drawing
;; (defmethod render :around ((pane nk-pane) (frob graph-frob) y-offset x-offset zoom)
;;   (in-gl-thread-of* (disp) (screen-of pane)
;;     (nk-display-frob frob (first (picture-stack pane)) (slot-value disp 'nk-context))))
;;     ;; (when (/= 0 (%nk:button-label ctx "graph button"))
;;     ;;   (vom:info "[image] There should be a graph "))))


;; (defmacro nk-bounds-rect (return-value x y w h)
;;  ` (macrolet ((f (val)
;; 	       `(float ,val 1f0)))
;;     (%nk:rect ,return-value (f ,x) (f ,y) (f ,w) (f ,h))))

;; (defmacro simple-bounds (return-value x y w h)
;;   `(nk-bounds-rect ,return-value ,x ,y (+ 30 ,w) (+ 54 ,h))) 


;; [tho]2020-01 not used anymore instead use nk-display-pictre
;;this is the system-dependent-frob a.k.a bltable
;; for the current pane
;; ctx is the current nk-context
(defun make-nk-image-func  (pane bltable)
  (let ((+height+ (float (first (base-dimensions bltable)) 1f0))
	(+width+  (if (second (base-dimensions bltable)) (second (base-dimensions bltable)) 1))
	(+rect+  (slot-value bltable 'nk-rect))
	(+image+ (slot-value bltable 'nk-image))
;;	(+this+ bltable)
	(+title+ (slot-value pane 'title))
	(+wid+   (slot-value pane 'wid)))
    (lambda (ctx)
      (let ((val (%nk:begin-titled ctx +wid+ +title+ +rect+ 
				   (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
					   %nk:+window-minimizable+ %nk:+window-title+))))
	(unless (= val 0)
	  (%nk:layout-row-static ctx +height+ +width+ 1)
	  (%nk:image ctx +image+)
	  (%nk:end ctx))))))



(defmacro nk-window-func ((ctx-var rect-var) &body body)
  (assert (symbolp ctx-var))
  (assert (symbolp rect-var))
  `(lambda (,ctx-var)
     (claw:c-with ((,rect-var (:struct (%nk:rect))))
       ,@body)
     (%nk:end ,ctx-var)))
       

