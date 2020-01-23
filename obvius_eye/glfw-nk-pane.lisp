;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-pane.lisp
;;;  Author: Patrick C. Teo [THO] adapted to nuklar and opengl glfw3
;;;  Description: Generic OBVIUS pane handling stuff
;;;  Creation Date: 1993 2019
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GL Pane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-simple-class nk-pane (pane)
  ((width  :type integer :initform 256)
   (height :type integer :initform 256)
   (x-pos  :type integer :initform 50)   ;;x position of upper left corner of window(pane)
   (y-pos  :type integer :initform 50)   ;;y position of upper left corner of window(pane)
   (width-padding :type integer :initform 30) ;; to make the window this amount larger than content
   (height-padding :type integer :initform 54) 
   (title :type string :initform *empty-pane-string*)
   (foreground :accessor foreground)
   (background :accessor background)
   (wid :accessor wid) 
   depth
   (nk-rect  :initform nil)  ;;holds width and height and pos of nk-window as foreign struct
   canvas                    ;;used for drawing
   gray-depth
   ;;maybe the next two slots ar enot needed anymore
   (interests :initform nil);; *default-pane-interests*)
   (devices   :initform nil);;*default-window-devices*)
   (status :accessor status :type (member :offline :cleared :realized :destroyed) :initform :offline)))

(defmethod set-pane-title-bar ((pane nk-pane) title)
  (setf (title pane) (string-right-trim "." title)))

(defmethod initialize-instance :after ((window nk-pane) &rest args &key)
  (declare (ignore args))
  (vom:info "[nk-pane] after init:")
  (setf (wid window) (make-window-id (title window)))
  (update-nk-rect window)
  (with-slots (wid width height foreground background title status devices) window

    (initialize-2d-viewport window)
    ;;; activate the screen if necessary.  this is done if this
    ;;; is the first window to be opened.
    (activate-screen (screen-of window))
    ;;; inherit the foreground, background, depth from the screen
    (unless (boundp 'foreground)
      (setf (slot-value window 'foreground) (foreground (screen-of window))))
    (unless (boundp 'background)
      (setf (slot-value window 'background) (background (screen-of window))))
    (unless (boundp 'depth)
      (setf (slot-value window 'depth) (depth (screen-of window))))
    (unless (boundp 'gray-depth)
      (setf (slot-value window 'gray-depth) (gray-depth (screen-of window))))
    (setf (slot-value window 'status) :realized)))


(defmethod destroy :after ((pane nk-pane) &key &allow-other-keys)
  (vom:info "[nk-pane] :after destroying nk-pane: ~a" pane)
  ;;(remove-from-render-loop pane)
  (with-slots (nk-rect) pane
    (and nk-rect
	 (claw:free nk-rect)))
  (setf (status pane) :destroyed))

(defmethod font-helper ((pane nk-pane))
  (font-helper (screen-of pane)))


(defmethod get-context ((window nk-pane))
  (slot-value (screen-of window) 'nk-context))

(defmethod x-dim ((window nk-pane))
  (width window))

(defmethod y-dim ((window nk-pane))
  (height window))

(defmethod dimensions ((window nk-pane))
  (list (height window) (width window)))

(defmethod update-nk-rect ((window nk-pane))
  (with-slots (nk-rect width height width-padding height-padding x-pos y-pos) window
    (when (or (null nk-rect)
	      (claw:wrapper-null-p nk-rect))
      (setf nk-rect (claw:calloc '(:struct (%nk:rect)))))
    
    (claw:c-val ((rect (:struct (%nk:rect)) nk-rect))
      (let ((w (floor (padded-width window)))
	    (h (floor (padded-height window))))
	(setf nk-rect (%nk:recti rect x-pos y-pos w h))))))

(defmethod padded-width ((window nk-pane))
  (with-slots (width width-padding) window
      (+ width-padding width)))


(defmethod padded-height ((window nk-pane))
  (with-slots (height height-padding) window
      (+ height-padding height)))

;;; patch because obvius assumes the origin is at the top right
;;; hand corner.  If the window is resized, we have to update this
;;; as well as the ortho2() specifications.
(defmethod set-2d-viewport ((window nk-pane) &key
			    (left 0) (right (1- (width window)))
						 (top 0) (bottom (1- (height window))))
  (vom:info "[set-2d-viewport] left: ~d right: ~d top: ~d bottom: ~d" left right top bottom)) 
  ;; (let ((bottom-adj (1- (- (height window) bottom)))
  ;; 	(top-adj (1- (- (height window) top)))
  ;; 	(4x4-identity-matrix
  ;; 	 (LCL:with-static-area (make-array '(4 4) :element-type 'single-float
  ;; 					   :initial-contents '((1.0 0.0 0.0 0.0)
  ;; 							       (0.0 1.0 0.0 0.0)
  ;; 							       (0.0 0.0 1.0 0.0)
  ;; 							       (0.0 0.0 0.0 1.0))))))
  ;;   (GL:with-GL-lock
  ;;     (GL:mmode GL:msingle)
  ;;     (GL:loadmatrix 4x4-identity-matrix)
  ;;     (GL:ortho2 (- left 0.5) (+ right 0.5) (- bottom-adj 0.5) (+ top-adj 0.5))
  ;;     (GL:viewport left right bottom-adj top-adj)
  ;;     (GL:translate 0.0 (1- (coerce (height window) 'double-float)) 0.0)
  ;;     (GL:scale 1.0 -1.0 1.0))))


(defmethod initialize-2d-viewport ((window nk-pane))
  (vom:info "[initialize-2d-viewport] of window: ~a" window)
  ;; (let ((f-width (LCL:with-static-area
  ;; 		     (make-array '(1) :element-type '(signed-byte 32) :initial-element 0)))
  ;; 	(f-height (LCL:with-static-area
  ;; 		      (make-array '(1) :element-type '(signed-byte 32) :initial-element 0))))
  ;;   (with-slots (wid width height) window
  ;;     (GL:with-GL-lock
  ;; 	(GL:winset wid)
  ;; 	(GL:getsize f-width f-height)
  ;; 	(setf width (aref f-width 0) height (aref f-height 0))
  	(set-2d-viewport window))

;;;
;;; GL window event handler --
;;; Basically updates the hot-device table and then proceeds
;;; with mapping events to registered interests
;;;
(defmethod receive-window-dispatch ((window nk-pane) device status)
  (vom:info "[receive-window-dispatch] window: ~a" window)
  (check-interests window device status))







;;; Should take this opportunity to do something to indicate
;;; selected pane. 
(defmethod set-selected-pane ((pane nk-pane))
  (when (not (eq pane *current-pane*))
    (call-next-method)))

;; (defmethod remove-from-render-loop ((pane nk-pane))
;;   (remove-from-nk-render-loop (screen-of pane) (wid pane)))



(defmethod clear ((pane nk-pane) &key
				   (y0 0) (x0 0)
				   (y1 (y-dim pane))
				   (x1 (x-dim pane))
				   (color (background pane)))
  (vom:info "[clear] nk-pane")
  (setf (status pane) :cleared
	(slot-value pane 'title) *empty-pane-string*))
  
  ;; (GL:with-GL-lock (GL:winset (wid pane)))
  ;; (draw-rect pane y0 x0 y1 x1 :foreground color :fill-p t))

;;; What shall we do with (background pane)/(foreground pane) and setf's??




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GL 8-bit Pane
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class 8bit-nk-pane (nk-pane) ()
  (:default-initargs
      :depth 8
      :title (str+ *empty-pane-string* " 8bit")))

;;REQUIRED: (make-pane)
(defmethod make-pane ((screen 8bit-glfw-nk-screen) &rest keys
		      &key left bottom right top width height border-width screen-of)
  (declare (ignore width height left bottom right top border-width))
  (let ((new-pane (apply 'make-instance '8bit-nk-pane  :screen-of screen keys)))
    ;;(setf (status new-pane) :offline)
    (clear new-pane)
    new-pane))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GL 24-bit Pane
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simple-class 24bit-nk-pane (nk-pane) ()
  (:default-initargs
      :depth 24
      :title (str+ *empty-pane-string* " 24bit")))

;;; REQUIRED: (make-pane)
(defmethod make-pane ((screen 24bit-glfw-nk-screen) &rest keys
		      &key left bottom right top width height border-width screen-of)
  (declare (ignore width height left bottom right top border-width))
  (let ((new-pane (apply 'make-instance '24bit-nk-pane :screen-of screen keys)))
    ;;(setf (status new-pane) :offline)
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

