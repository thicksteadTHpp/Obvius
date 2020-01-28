;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  glfw-nk-blt.lisp
;;;  Author: Patrick C. Teo ,[THO]
;;;  Description: GL blting routines
;;;  Creation Date: 1993 2019 
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; adapted for use with glfw nk in 2019
;; renamed gl-image to gl-pixmap

(in-package :obvius)


;;;
;;; GL images (pixmaps)
;;;
;;; Images are raw 16bit- or 32bit- data which are accepted by
;;; the OPenGL graphics library routines for blting.
;;;
(def-simple-class GL-pixmap ()
  (width
   height
  (data   :initform nil)))

(def-simple-class 8bit-GL-pixmap (GL-pixmap) ())
(def-simple-class 24bit-GL-pixmap (GL-pixmap) ())


(defmethod dimensions ((image GL-pixmap))
  (list (height image) (width image)))

(defmethod x-dim ((image GL-pixmap)) (width image))
(defmethod y-dim ((image GL-pixmap)) (height image))


;;;
;;; GL Bltables
;;;
;;; Bltables are somewhat midway between the actual float data
;;; and the images (above).  For example, a bitmap will be a 1bit-bltable;
;;; when it is displayed, it is converted into a GL-pixmap (a 16bit- GL-pixmap
;;; or a 32bit- GL-pixmap depending on the screen depth).  Similarly,
;;; a 8bit gray image is blted into an 8bit-bltable; when it is displayed,
;;; it is also converted to a corresponding GL-pixmap.
;;;
(def-simple-class GL-bltable (frob)
  (screen-of					;; screen of bltable
   depth                                        ;; depth of bltable
   foreground 
   background 
   dimensions					;; zoomed dimensions
   (base-dimensions :initform 0)				;; unzoomed dimensions
   (data          :initform nil)		;; raw data of image
   (frob-x-offset :initform 0)
   (frob-y-offset :initform 0)
   (texture-id :initform 0)
   (nk-image :initform nil)
   ;;should be in pane
   (nk-rect  :initform nil)
   nk-func                                      ;;function for displaying needed by render-loop
   image)					;; GL pixmap used to display  ;;this is maybe the texture ID for OpenGL
  (:default-initargs
      :pane->frob-y (make-transform :coerce #'floor)
      :pane->frob-x (make-transform :coerce #'floor)))

;;;
;;; Specialized for each type of screen
;;;

(defgeneric make-bltable (GLFW-nk-screen base-dimensions &rest initargs &key bltable))


(defmethod initialize-instance :after ((bltable gl-bltable) &rest initargs)
  (declare (ignore initargs))
  (with-slots (data base-dimensions) bltable
    (setf data (make-array base-dimensions :element-type '(unsigned-byte 8) :initial-element 0))))


(defmethod static-arrays-of ((bltable GL-bltable))
  (list (data bltable) (image bltable)))

(defmethod destroy :after ((bltable GL-bltable) &key &allow-other-keys)
  (vom:info "[destroy} :after bltable")
  (with-slots (data image nk-image nk-rect texture-id) bltable
     (and data
	  (allocated-array-p data)
	  (free-array data))
    (and image
	 (allocated-array-p image)
	 (free-array image))
    (and nk-image
	 (claw:free nk-image))
    (and nk-rect
	 (claw:free nk-rect))
    (and (> texture-id 0)
	 (destroy-texture (screen-of bltable) texture-id))))
      ;; (in-gl-thread-of (screen-of bltable)
      ;; 	(vom:info "[destroy] bltable and texture: ~d" texture-id)
      ;; 	(gl:delete-texture texture-id)))))
	   

  ;; (when (and (data bltable)
  ;; 	     (allocated-array-p (data bltable)))
  ;;   (free-array (data bltable)))
  ;; (when (and (image bltable)
  ;; 	     (allocated-array-p (image bltable)))
  ;;   (free-array (image bltable)))
 
  
  ;; (and (slot-boundp bltable 'nk-image)
  ;;      (slot-value bltable 'nk-image)
  ;;      (claw:free nk-image))
  ;; (and (slot-boundp bltable 'nk-rect)
  ;;      (slot-value bltable 'nk-rect)
  ;;      (claw:free nk-rect)))


;;; called by both initialize-instance and reinitialize-instance
(defmethod shared-initialize :after ((bltable GL-bltable) slot-names
				     &rest initargs
				     &key foreground background)
  (declare (ignore slot-names initargs))
  (when foreground
    (setf (foreground bltable) foreground))
  (when background
    (setf (background bltable) background))
  bltable)

;;;
(defmethod (setf foreground) (color-desc (bltable GL-bltable))
  (setf (slot-value bltable 'foreground)
	(compute-GL-color (screen-of bltable) color-desc)))
  
(defmethod (setf background) (color-desc (bltable GL-bltable))
  (setf (slot-value bltable 'background)
	(compute-GL-color (screen-of bltable) color-desc)))



;;compute picture replaces the system-dependent-frob with
;; a new one
;; we must make sure that the old is destroyed and freed
(defmethod compute-picture :around ((pic gray) (im image))
  (vom:info "[compute-picture] :around on pic: ~a" pic)
  (with-slots (system-dependent-frob) pic
    (when system-dependent-frob
      (destroy system-dependent-frob)
      (setf system-dependent-frob nil)))
  (call-next-method))

;; (defmethod (setf system-dependent-frob) :around ((new-bltable gl-bltable) (pic picture))
;;   (vom:info "[setf system-dependent-frob] :around old bltable: ~a" (system-dependent-frob pic))
;;   (vom:info "[setf system-dependent-frob] :around new bltable: ~a" new-bltable)
;;   (call-next-method))
  
	    




;;;
;;; 1bit/8bit/24bit-GL Bltables
;;;
(def-simple-class 1bit-GL-bltable (GL-bltable) ()
  (:default-initargs :depth 1))

(def-simple-class 8bit-GL-bltable (GL-bltable) ()
  (:default-initargs :depth 8))

(def-simple-class 24bit-GL-bltable (GL-bltable) ()
  (:default-initargs :depth 24))




;; (defmethod initialize-instance :after ((bltable 1bit-GL-bltable) &rest initargs)
;;   (declare (ignore initargs)))

;;    ;; (with-slots (data base-dimensions) bltable
;;    ;;   (setf data (allocate-array base-dimensions :element-type 'bit :initial-element 0))))

;; (defmethod initialize-instance :after ((bltable 8bit-GL-bltable) &rest initargs)
;;   (declare (ignore initargs)))

;;    ;; (with-slots (data base-dimensions) bltable
;;    ;;   (setf data (allocate-array base-dimensions :element-type '(unsigned-byte 8) :initial-element 0))))

;; (defmethod initialize-instance :after ((bltable 24bit-GL-bltable) &rest initargs)
;;   (declare (ignore initargs)))

;;    ;; (with-slots (data base-dimensions) bltable
;;    ;;   (setf data (allocate-array base-dimensions :element-type '(unsigned-byte 32) :initial-element 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  make a the data array for the bltables
;;;  this may depend on the type of bltable
;;;  the data slot contains an intemediate value
;;; between obvius and openGL
;;;  for now this is an RGB or Gray array with 8 bits per channel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defmethod prepare-data ((blt gl-bltable))
;;  (setf (data blt) (allocate-array (base-dimensions blt) :element-type '(unsigned-byte 8))))


;;; [TODO] make an aroun dmthod which has an optionak lkey arguement
;;; that holds a eference to the picture!!! and then decide
;;; which kind of bltable to make
;;;
;;; Computes a bltable that is handled by this screen depth
;;;
;;; if the  depth arg is differebt from the screen depth
;;; we assume the user wants that specific depth so we use it and
;;; call next method which handles that.
;;; if depth and screen-depth is the same and a picture is given
;;; we use the new methodes to make a bltable according to
;;; the picture type e.g. a gray picture could have a 8bit bltable
(defmethod make-bltable :around ((screen glfw-nk-screen) base-dimensions
			 &rest initargs
			 &key
			 (picture nil picture-p)
			 (depth (depth screen))
			 (foreground (foreground screen))
			 (background (background screen))			 
			   bltable)
  (vom:info "[make-bltable] :around")
  (if (and (= (depth screen) depth)
	   picture-p)
      (make-bltable-according-to-picture screen picture base-dimensions)
      (progn
	(remf initargs :picture)
	(call-next-method))))
      
  



;;;;===================================================================================
;;;;
;;;; 24-bit Blting
;;;;
;;;;===================================================================================

;;;
;;; Computes a bltable that is handled by this screen depth
;;;
(defmethod make-bltable ((screen 24bit-GLFW-nk-screen) base-dimensions
			 &rest initargs
			 &key
			 (depth (depth screen))
			 (foreground (foreground screen))
			 (background (background screen))			 
			 bltable)
  (remf initargs :bltable)
  (unless (and (typep bltable 'GL-bltable)
	       (eq screen (screen-of bltable))
	       (= (depth bltable) depth)
	       (equal (base-dimensions bltable) base-dimensions))
    (let ((bltable-class (cond ((= depth 24) '24bit-GL-bltable)
			       ((= depth 8) '8bit-GL-bltable)
			       ((= depth 1) '1bit-GL-bltable)
			       (t (error "Can't handle depth ~d" depth)))))
      (when bltable (destroy bltable))
      (setq bltable
	    (make-instance bltable-class
			   :screen-of screen
			   :foreground foreground
			   :background background
			   :base-dimensions base-dimensions
			   :dimensions base-dimensions))))

  (setf (image bltable) nil)
  bltable)



;;;;===================================================================================
;;;;
;;;; 8-bit Blting
;;;;
;;;;===================================================================================
(defmethod make-bltable ((screen 8bit-GLFW-nk-screen) base-dimensions
			 &rest initargs
			 &key
			 (depth (depth screen))
			 (foreground (foreground screen))
			 (background (background screen))
			 bltable)
  (remf initargs :bltable)
  (unless (and (typep bltable '8bit-GLFW-nk-bltable)
	       (eq screen (screen-of bltable))
	       (= (depth bltable) depth)
	       (equal (base-dimensions bltable) base-dimensions))

    (let ((bltable-class (cond ((= depth 8) '8bit-GLFW-nk-bltable)
			       ((= depth 1) '1bit-GLFW-nk-bltable)
			       (t (error "Can't handle depth ~d" depth)))))
      (when bltable (destroy bltable))
      (setq bltable
	    (make-instance bltable-class
			   :screen-of screen
			   :foreground foreground
			   :background background
			   :base-dimensions base-dimensions
			   :dimensions base-dimensions))))
  (setf (image bltable) nil)
  bltable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; make -bltable according to picture types
;;; in the next version this will be the default interface
;;; for make bltable
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defmethod make-bltable-according-to-picture ((screen glfw-nk-screen) (pic gray) base-dimension
					      &rest initargs
					      &key
						(depth (depth screen))
						(foreground (foreground screen))
						(background (background screen))			 
						bltable)
  (setf (getf initargs :depth) 8)
  (apply #'make-bltable screen base-dimension initargs))

(defmethod make-bltable-according-to-picture ((screen glfw-nk-screen) (pic color-picture) base-dimension
					      &rest initargs
					      &key
						(depth (depth screen))
						(foreground (foreground screen))
						(background (background screen))			 
						bltable)
  (setf (getf initargs :depth) 24)
  (apply #'make-bltable screen base-dimension initargs))



;;fallback
(defmethod make-bltable-according-to-picture ((screen glfw-nk-screen) pic base-dimension
					      &rest initargs
					      &key
						(depth (depth screen))
						(foreground (foreground screen))
						(background (background screen))			 
						bltable)
  (apply #'make-bltable screen base-dimension initargs))
		





;;;
;;; Clear GL bltables
;;;

(defmethod clear ((bltable 1bit-GL-bltable) &key color)
  (declare (ignore color))
  (vom:info "[clear] 1bit-GL-bltable"))
  ;;(fill! (data bltable) 0))

(defmethod clear ((bltable 8bit-GL-bltable) &key
					      (color (background bltable)))
  (vom:info "[clear] 8bit-GL-bltable"))
;;  (fill! (data bltable) (compute-GL-color (screen-of bltable) color)))

(defmethod clear ((bltable 24bit-GL-bltable) &key
					       (color (background bltable)))
  (vom:info "[clear] 24bit-GL-bltable"))
;;  (fill! (data bltable) (compute-GL-color (screen-of bltable) color)))




;;;
;;; Computes 8bit-GL-image of 1bit-GL-bltable
;;; (Handles zooming)
;;;
(defmethod compute-bltable-GL-image ((window 8bit-nk-pane) (bltable 1bit-GL-bltable) zoom)
  (vom:info "[compute-bltable-GL-image] win 8bit 1bit-glfw-nk-bltable")
  (with-slots (data screen-of foreground background depth image dimensions base-dimensions) bltable
    (setf dimensions (list (ceiling (* (car base-dimensions) zoom))
			   (ceiling (* (cadr base-dimensions) zoom))))
    (when (null image)
      (setf image (similar data
			   :element-type '(unsigned-byte 16)
			   :dimensions dimensions :initial-element 0)))
    (cond ((= zoom 1)
	   (internal-paste-1bit-to-16bit data (x-dim data) (y-dim data)
					 image (x-dim image)
					 0 0
					 (second dimensions) (first dimensions)
					 background foreground))
	  ((> zoom 1)
	   (internal-supersample-1bit-to-16bit data (x-dim data) (y-dim data)
					       image (x-dim image)
					       0 0          ; dest starting corner
					       (second dimensions) (first dimensions)
					       (round zoom)
					       background foreground))
	  (t (internal-subsample-1bit-to-16bit data (x-dim data) (y-dim data)
					       image (second dimensions)
					       (round (/ 1 zoom))
					       background foreground)))))


;;;
;;; Computes 8bit-GL-image of 8bit-GL-bltable
;;; (Handles zooming)
;;;
(defmethod compute-bltable-GL-image ((window 8bit-nk-pane) (bltable 8bit-GL-bltable) zoom)
    (vom:info "[compute-bltable-GL-image] win-8bit 8bit-glfw-nk-bltable")
  (with-slots (data screen-of depth image dimensions base-dimensions) bltable
    (setf dimensions (list (ceiling (* (car base-dimensions) zoom))
			   (ceiling (* (cadr base-dimensions) zoom))))
    (when (null image)
      (setf image (similar data
			   :element-type '(unsigned-byte 16)
			   :dimensions dimensions :initial-element 0)))
    (cond ((= zoom 1)
	   (internal-paste-8bit-to-16bit data (x-dim data) (y-dim data)
					 0 0
					 image (x-dim image)
					 0 0
					 (second dimensions) (first dimensions)))
	  ((> zoom 1)
	   (internal-supersample-8bit-to-16bit data (x-dim data) (y-dim data)
					       0 0          ; source starting corner
					       image (x-dim image)
					       0 0          ; dest starting corner
					       (second dimensions) (first dimensions)
					       (round zoom)))
	  (t (internal-subsample-8bit-to-16bit data (x-dim data) (y-dim data)
					       image (second dimensions)
					       (round (/ 1 zoom)))))))

;;;
;;; Generic render routine for 8bit GL screens
;;;
(defmethod render ((window 8bit-nk-pane) (bltable GL-bltable) y-offset x-offset zoom)
    (vom:info "[render] win-8bit GLFW-nk-bltable")
  (with-slots (image pane->frob-y pane->frob-x dimensions base-dimensions) bltable
    (when (and image (/= (cadr dimensions) (ceiling (* (cadr base-dimensions) zoom))))
      (free-array image)
      (setf image nil))
    (compute-bltable-GL-image window bltable zoom)
    (let ((row (+ (floor (- (y-dim window) (y-dim bltable)) 2) y-offset))
	  (row-adjusted (- (floor (- (y-dim window) (y-dim bltable)) 2) y-offset))
	  (col (+ (floor (- (x-dim window) (x-dim bltable)) 2) x-offset)))
      (setf (slot-value pane->frob-y 'offset) (/ (- row) zoom)
	    (slot-value pane->frob-x 'offset) (/ (- col) zoom)
	    (slot-value pane->frob-y 'scale) (/ zoom)
	    (slot-value pane->frob-x 'scale) (/ zoom)))))
      ;; (GL:with-GL-lock
      ;; 	(GL:winset (wid window))
      ;; 	(GL:rectwrite col row-adjusted
      ;; 		      (1- (+ col (x-dim image)))
      ;; 		      (1- (+ row-adjusted (y-dim image)))
      ;; 		      image)))))





;;;
;;; Computes 24bit-GL-image of 1bit-GL-bltable
;;; (Handles zooming)
;;;
(defmethod compute-bltable-GL-image ((window 24bit-nk-pane) (bltable 1bit-GL-bltable) zoom)
    (vom:info "[compute-bltable-GL-image] win 24-bit 1bit-bltable")
  (with-slots (data screen-of foreground background depth image dimensions base-dimensions) bltable
    (setf dimensions (list (ceiling (* (car base-dimensions) zoom))
			   (ceiling (* (cadr base-dimensions) zoom))))))
    ;; (when (null image)
    ;;   (setf image (similar data
    ;; 			   :element-type '(unsigned-byte 32)
    ;; 			   :dimensions dimensions :initial-element 0)))
    ;; (cond ((= zoom 1)
    ;; 	   (internal-paste-1bit-to-24bit data (x-dim data) (y-dim data)
    ;; 					 image (x-dim image)
    ;; 					 0 0
    ;; 					 (second dimensions) (first dimensions)
    ;; 					 background foreground))
    ;; 	  ((> zoom 1)
    ;; 	   (internal-supersample-1bit-to-24bit data (x-dim data) (y-dim data)
    ;; 					       image (x-dim image)
    ;; 					       0 0          ; dest starting corner
    ;; 					       (second dimensions) (first dimensions)
    ;; 					       (round zoom)
    ;; 					       background foreground))
    ;; 	  (t (internal-subsample-1bit-to-24bit data (x-dim data) (y-dim data)
    ;; 					       image (second dimensions)
    ;; 					       (round (/ 1 zoom))
    ;; 					       background foreground)))))



(defparameter *swizzle-mask-gray* (let ((r (gl::foreign-enum-value '%gl:enum :red))
					(a (gl::foreign-enum-value '%gl:enum :one)))
				    (make-array 4 :element-type '(signed-byte 32) :initial-contents (list r r r a))))
;;;
;;; Computes 24bit-GL-image of 8bit-GL-bltable
;;; (Handles zooming)
;;;
(defmethod compute-bltable-GL-image ((window 24bit-nk-pane) (bltable 8bit-GL-bltable) zoom)
  (vom:info "[compute-bltable-GL-image] win 24-bit 8bit-bltable")
  ;;  (warn "8bit-GL-bltable not entirely compatible on 24bit-GL-window")
  (with-slots (nk-rect width height) window
    (with-slots (nk-image texture-id data screen-of depth image dimensions base-dimensions) bltable
    (setf dimensions (list (ceiling (* (car base-dimensions) zoom))
			   (ceiling (* (cadr base-dimensions) zoom))))
      (destructuring-bind (x &optional (y 1)) base-dimensions
	(ffa:with-pointer-to-array (data data-pointer :uint8 (array-total-size data) :copy-out)
	  (let ((tex (create-rgb-texture-from-gray x y data-pointer)))
	    (vom:info "[compute-bltable-GL-image] gen texture: ~d" tex)

      ;; (let ((tex (gl:gen-texture)))
      ;; 	(vom:info "[compute-bltable-GL-image] gen texture: ~d" tex)
      ;; 	(when (/= tex 0)
      ;; 	  (setf texture-id tex)   
      ;; 	  (gl:bind-texture :texture-2d tex)
      ;; 	  (%gl:tex-storage-2d :texture-2d 1 :RGB8 x y)
      ;; 	  (ffa:with-pointers-to-arrays ((data data-pointer :uint8 (array-total-size data) :copy-out)
      ;; 					(*swizzle-mask-gray* swizzle-mask :int32 4 :copy-out))
      ;; 	    (gl:tex-sub-image-2d :texture-2d 0 0 0 x y :red :unsigned-byte data-pointer)
      ;; 	    ;;(let ((swizzle-mask (cffi:foreign-alloc :int :initial-contents *swizzle-mask-gray*)))  ;;distribute the red channel over g and b channel
      ;; 	      ;;so we get a gray picture
      ;; 	      (%gl:tex-parameter-iv :texture-2d :texture-swizzle-rgba swizzle-mask)
      ;; 					;(cffi:foreign-free swizzle-mask)))
      ;; 	    )
      ;; 	  (gl:bind-texture :texture-2d 0)
	  ;;; nk stuff
	  (claw:c-let ((nid (:struct (%nk:image)) :free nil))
	    (%nk:image-id nid tex)
	    (setf width (second dimensions)
		  height (first dimensions)
		  nk-image nid
		  (slot-value bltable 'texture-id) tex))
	  (update-nk-rect window)
	  (vom:info "[compute-bltable-GL-image] gen texture: ~d   ... done" tex))))))) 



      
    ;; (when (null image)
    ;;   (setf image (similar data
    ;; 			   :element-type '(unsigned-byte 32)
    ;; 			   :dimensions dimensions :initial-element 0)))
    ;;))
    ;; (cond ((= zoom 1)
    ;; 	   (internal-paste-8bit-to-24bit data (x-dim data) (y-dim data)
    ;; 					 0 0
    ;; 					 image (x-dim image)
    ;; 					 0 0
    ;; 					 (second dimensions) (first dimensions)))
    ;; 	  ((> zoom 1)
    ;; 	   (internal-supersample-8bit-to-24bit data (x-dim data) (y-dim data)
    ;; 					       0 0          ; source starting corner
    ;; 					       image (x-dim image)
    ;; 					       0 0          ; dest starting corner
    ;; 					       (second dimensions) (first dimensions)
    ;; 					       (round zoom)))
    ;; 	  (t (internal-subsample-8bit-to-24bit data (x-dim data) (y-dim data)
    ;; 					       image (second dimensions)
    ;; 					       (round (/ 1 zoom)))))))





;;;
;;; Computes 24bit-GL-image of 24bit-GL-bltable
;;; (Handles zooming)
;;;
(defmethod compute-bltable-GL-image ((window 24bit-nk-pane) (bltable 24bit-GL-bltable) zoom)
      (vom:info "[compute-bltable-GL-image] win 24-bit 24bit-bltable")
  (with-slots (data screen-of depth image dimensions base-dimensions) bltable
    (setf dimensions (list (ceiling (* (car base-dimensions) zoom))
			   (ceiling (* (cadr base-dimensions) zoom))))))
    ;; (when (null image)
    ;;   (setf image (similar data
    ;; 			   :element-type '(unsigned-byte 32)
    ;; 			   :dimensions dimensions :initial-element 0)))
    ;; (cond ((= zoom 1)
    ;; 	   (internal-paste-24bit-to-24bit data (x-dim data) (y-dim data)
    ;; 					  0 0
    ;; 					  image (x-dim image)
    ;; 					  0 0
    ;; 					  (second dimensions) (first dimensions)))
    ;; 	  ((> zoom 1)
    ;; 	   (internal-supersample-24bit-to-24bit data (x-dim data) (y-dim data)
    ;; 					       0 0          ; source starting corner
    ;; 					       image (x-dim image)
    ;; 					       0 0          ; dest starting corner
    ;; 					       (second dimensions) (first dimensions)
    ;; 					       (round zoom)))
    ;; 	  (t (internal-subsample-24bit-to-24bit data (x-dim data) (y-dim data)
    ;; 						image (second dimensions)
    ;; 						(round (/ 1 zoom)))))))




;;;
;;; Generic render routine for 24bit GL screens
;;; will be called in eah fram of the render loop 

(defmethod render ((window 24bit-nk-pane) (bltable GL-bltable) y-offset x-offset zoom)
  (with-slots (nk-image texture-id image pane->frob-y pane->frob-x dimensions base-dimensions) bltable

    (when (and image (/= (cadr dimensions) (ceiling (* (cadr base-dimensions) zoom))))
      ;; we need to to recompute the picture due to zooming
      (and (allocated-array-p image) (free-array image))
      (setf image nil))

    ;;check if we need to render (compute) a new pixmap of bltable
    ;;or it is new
    ;;when texture-id is >0 then the bltable has a texture attached and a nk-image
    ;; (and (claw:wrapper-null-p nk-image)
    ;; 	 (or (null texture-id) (= 0 texture-id))
    ;; 	 (in-gl-thread-of (screen-of window) 
    ;; 	   (compute-bltable-GL-image window bltable zoom)
    ;; 	   (setf (slot-value bltable 'nk-func) (make-nk-image-func window bltable))))
    (and (claw:wrapper-null-p nk-image)
	 (or (null texture-id) (= 0 texture-id))
	 (in-gl-thread-of (screen-of window)
	   (compute-bltable-GL-image window bltable zoom)))
    
    (let ((row (+ (floor (- (y-dim window) (y-dim bltable)) 2) y-offset))
	  (row-adjusted (- (floor (- (y-dim window) (y-dim bltable)) 2) y-offset))
	  (col (+ (floor (- (x-dim window) (x-dim bltable)) 2) x-offset)))
      (setf (slot-value pane->frob-y 'offset) (/ (- row) zoom)
	    (slot-value pane->frob-x 'offset) (/ (- col) zoom)
	    (slot-value pane->frob-y 'scale) (/ zoom)
	    (slot-value pane->frob-x 'scale) (/ zoom)))))
      ;;TODO adapt to GLFW/nk
      ;; (GL:with-GL-lock
      ;; 	(GL:winset (wid window))
      ;; 	(GL:lrectwrite col row-adjusted
      ;; 		      (1- (+ col (x-dim image)))
      ;; 		      (1- (+ row-adjusted (y-dim image)))
      ;; 		      image)))))



;; old stuff
;; (send-to (screen-of window) (lambda (disp)
    ;; 				  (compute-bltable-gl-image window bltable zoom)
    ;; 				  (push-to-nk-render-loop disp (wid window)
    ;; 							  (nk-window-func (ctx rect)
    ;; 							    (let ((val (%nk:begin ctx "PIC" (slot-value bltable 'nk-rect)
    ;; 										  (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
    ;; 											  %nk:+window-minimizable+ %nk:+window-title+))))
    ;; 							      (unless (= val 0)
    ;; 								(%nk:layout-row-static ctx 256f0 256 1)
    ;; 								(%nk:image ctx (slot-value bltable 'nk-image))))))))



(defmethod render :after ((window 24bit-nk-pane) (bltable GL-bltable) y-offset x-offset zoom)
  (setf (status window) :realized))



(defun create-rgb-texture-from-gray (x y data)
  (ffa:with-pointer-to-array (*swizzle-mask-gray* swizzle-mask :int32 4 :copy-out)
    (let ((tex (gl:gen-texture)))
      (unless (= 0 tex)
	(gl:bind-texture :texture-2d tex)
	(gl:tex-parameter :texture-2d :texture-min-filter :linear)
	(gl:tex-parameter :texture-2d :texture-mag-filter :linear)
;;	(gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
;;	(gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
	(%gl:tex-parameter-iv :texture-2d :texture-swizzle-rgba swizzle-mask)
	(gl:tex-image-2d :texture-2d 0 :rgb x y 0 :red :unsigned-byte data)
	(gl:generate-mipmap :texture-2d)
	(gl:finish))
      (gl:bind-texture :texture-2d 0)
      tex)))

