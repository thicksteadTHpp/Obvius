;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-window.lisp
;;;  Author: Patrick C. Teo [THO]
;;;  Description: Generic window handling stuff
;;;  [THO] We dont't need this at all
;;;  [2019-12-19] will not be loaded anymore
;;;  Creation Date: 1993 2019
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

(let ((seed (random 1000)))
  (defun get-random-id ()
    (prog1 seed
      (setf seed (+ seed (random 10))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; we use nuklear imediate mode gui so window is named nk
;;; Do we need this class at all??
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-simple-class nk-window ()
  ((width  :type integer :initform 300)
   (height :type integer :initform 256)
   (title :type string :initform "Nuklear Window")
   foreground
   background
   (wid :accessor wid) 
   depth
   gray-depth
   (interests :initform nil);; *default-pane-interests*)
   (devices   :initform nil);;*default-window-devices*)
   (status :type (member :offline :realized :destroyed) :initform :offline)))

(defmethod initialize-instance :after ((win nk-window) &rest args &key)
  (declare (ignore args))
  (setf (wid win) (make-window-id (title win))))

;;; Lucid bug 7126
;;; We have to define a (setf status) with val unspecialized first.
;;;
;; (defmethod (setf status) ((val symbol) (window nk-window)))
(defmethod (setf status) ((val (eql :realized)) (window nk-window))
  (vom:info "[nk-window] set status to: ~a" val)
  (with-slots (wid width height foreground background title status devices) window
    ;; (GL:with-GL-lock
    ;;   (GL:foreground)
    ;;   (GL:minsize width height)
    ;;   (setf wid (GL:winopen title)))

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

    ;;; activate local mouse devices
    (dolist (device devices)
      (add-device (screen-of window) device))
    
    (setf (slot-value window 'status) :realized)))


(defmethod (setf status) ((val (eql :destroyed)) (window nk-window))
  (destroy window))

(defmethod (setf title) (string (window nk-window))
  (vom:info "[nk-window] window: ~a set title to: ~a" window string))

  ;; (GL:with-GL-lock
  ;;   (GL:winset (wid window))
  ;;   (GL:wintitle string)))

(defmethod (setf foreground) (color-desc (window nk-window))
  (setf (slot-value window 'foreground)
	(compute-GL-color (screen-of window) color-desc)))
  
(defmethod (setf background) (color-desc (window nk-window))
  (setf (slot-value window 'background)
	(compute-GL-color (screen-of window) color-desc)))

(defmethod x-dim ((window nk-window))
  (width window))

(defmethod y-dim ((window nk-window))
  (height window))

(defmethod dimensions ((window nk-window))
  (list (height window) (width window)))

;;; patch because obvius assumes the origin is at the top right
;;; hand corner.  If the window is resized, we have to update this
;;; as well as the ortho2() specifications.
(defmethod set-2d-viewport ((window nk-window) &key
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


(defmethod initialize-2d-viewport ((window nk-window))
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
(defmethod receive-window-dispatch ((window nk-window) device status)
  (vom:info "[receive-window-dispatch] window: ~a" window)
  (check-interests window device status))

(defmethod destroy :after ((window nk-window) &key &allow-other-keys)
  (vom:info "[nk-window] :after destroying window: ~a" window)
;;  (with-slots (devices) window

    ;;;
    ;;; remove all registered devices
    ;;;
;;    (dolist (device devices)
;;      (remove-device (screen-of window) device))

;;    (GL:with-GL-lock (GL:winclose (wid window)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GL 8-bit Window
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-simple-class 8bit-nk-window (nk-window) ()
  (:default-initargs :depth 8))

;;; Lucid bug 7126
;; [tho] not needed for now
(defmethod (setf status) ((val symbol) (window 8bit-nk-window)))
(defmethod (setf status) :after ((val (eql :realized)) (window 8bit-nk-window))
  (vom:info "[8bit-nk-window] set status to: ~a" val))
	   
  ;; (GL:with-GL-lock
  ;;   (GL:cmode)
  ;;   (GL:gconfig)))

;;; Supports setting color
(defmethod set-window-color ((window 8bit-nk-window) color-desc)
  (vom:info "[8bit-nk-window] set window color to: ~a" color-desc))

  ;; (let ((GL-cmindex (compute-GL-color (screen-of window) color-desc)))
  ;;   (GL:with-GL-lock (GL:color GL-cmindex))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GL 24-bit Window
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-simple-class 24bit-nk-window (nk-window) ()
  (:default-initargs :depth 24))

;;; Lucid bug 7126
;; [tho] not needed for now
(defmethod (setf status) ((val symbol) (window 24bit-nk-window)))
(defmethod (setf status) :after ((val (eql :realized)) (window 24bit-nk-window))
  (vom:info "[24bit-nk-window] set status to: ~a" val))
  ;; (GL:with-GL-lock
  ;;   (GL:RGBmode)
  ;;   (GL:gconfig)))

;;; Supports setting color
(defmethod set-window-color ((window 24bit-nk-window) color-desc)
  (vom:info "[8bit-nk-window] set status to: ~a" val))

  ;; (let ((GL-RGBA (compute-GL-color (screen-of window) color-desc)))
  ;;   (GL:with-GL-lock (GL:cpack GL-RGBA))))



