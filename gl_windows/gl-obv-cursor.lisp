;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-cursor.lisp
;;;  Author: Patrick C. Teo
;;;  Description: Cursor manipulation routines for GL
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

(def-simple-class GL-cursor ()
  (depth                        ;; number of bits in cursor
   size-x size-y                ;; dimensions of pixmap
   (hot-x :initform 0)
   (hot-y :initform 0)          ;; hot spot (lower-left is 0,0)
   (colormap :initform nil)     ;; a list of colors from 1..3
   (pixmap   :initform nil)))

(def-simple-class GL-16x16x1-cursor (GL-cursor) ()
  (:default-initargs
      :depth 1
      :size-x 16 :size-y 16))

(def-simple-class GL-32x32x1-cursor (GL-cursor) ()
  (:default-initargs
      :depth 1
      :size-x 32 :size-y 32))

(def-simple-class GL-16x16x2-cursor (GL-cursor) ()
  (:default-initargs
      :depth 2
      :size-x 16 :size-y 16))

(def-simple-class GL-32x32x2-cursor (GL-cursor) ()
  (:default-initargs
      :depth 2
      :size-x 32 :size-y 32))




;;;
;;; Some example cursors
;;;

(def-simple-class GL-target (GL-16x16x1-cursor) ()
  (:default-initargs
      :hot-x 8
      :hot-y 8
      :colormap '((255 0 0))
      :pixmap
      (make-array '(32)
		  :element-type '(unsigned-byte 8)
		  :initial-contents
		  '(#x00 #x00 #x01 #x80 #x01 #x80 #x07 #xe0 #x09 #x90 #x11 #x88
		    #x13 #xc8 #x7e #x7e #x7e #x7e #x13 #xc8 #x11 #x88 #x09 #x90
		    #x07 #xe0 #x01 #x80 #x00 #x80 #x00 #x00))))

(def-simple-class GL-downarrow (GL-16x16x1-cursor) ()
  (:default-initargs
      :colormap '((255 0 0))
      :pixmap
      (make-array '(16)
		  :element-type '(unsigned-byte 16)
		  :initial-contents
		  '(#xfe00 #xfc00 #xf800 #xf800
		    #xfc00 #xde00 #x8f00 #x0780
		    #x03c0 #x01e0 #x00f0 #x0078
		    #x003c #x001e #x000e #x0004))))

(def-simple-class GL-hourglass (GL-16x16x1-cursor) ()
  (:default-initargs
      :colormap '((255 0 0))
      :pixmap
      (make-array '(16)
		  :element-type '(unsigned-byte 16)
		  :initial-contents
		  '(#x1ff0 #x1ff0 #x0820 #x0820
		    #x0820 #x0c60 #x06c0 #x0100
		    #x0100 #x06c0 #x0c60 #x0820
		    #x0820 #x0820 #x1ff0 #x1ff0))))

(def-simple-class GL-martini (GL-16x16x1-cursor) ()
  (:default-initargs
      :colormap '((255 0 0))
      :pixmap
      (make-array '(16)
		  :element-type '(unsigned-byte 16)
		  :initial-contents
		  '(#x1ff8 #x0180 #x0180 #x0180
		    #x0180 #x0180 #x0180 #x0180
		    #x0180 #x0240 #x0720 #x0b10
		    #x1088 #x3ffc #x4022 #x8011))))


(def-simple-class GL-flag (GL-32x32x2-cursor) ()
  (:default-initargs
      :colormap '((255 0 0) (0 0 255) (255 255 255))
      :pixmap
      (make-array '(128)
		  :element-type '(unsigned-byte 16)
		  :initial-contents
		  '(#x0000 #x0000 #x0000 #x0000
		    #x0000 #x0000 #x0000 #x0000
		    #x0000 #x0000 #x0000 #x0000
		    #xffff #xffff #xffff #xffff
		    #xffff #xffff #xffff #xffff		
		    #xffff #xffff #xffff #xffff
		    #xffff #xffff #xffff #xffff		
		    #xffff #xffff #xffff #xffff
		    #xffff #xffff #xffff #xffff		
		    #xffff #xffff #xffff #xffff
		    #x0000 #xffff #x6666 #xffff
		    #x6666 #xffff #x0000 #xffff
		    #x0000 #xffff #x6666 #xffff
		    #x6666 #xffff #x0000 #xffff		
		    #x0000 #xffff #x6666 #xffff
		    #x6666 #xffff #x0000 #xffff		
		    #x0000 #x0000 #x0000 #x0000
		    #x0000 #x0000 #x0000 #x0000		
		    #x0000 #x0000 #x0000 #x0000
		    #x0000 #x0000 #x0000 #x0000		
		    #xffff #xffff #xffff #xffff
		    #x0000 #x0000 #x0000 #x0000				
		    #xffff #xffff #xffff #xffff
		    #x0000 #x0000 #x0000 #x0000				
		    #xffff #xffff #xffff #xffff
		    #x0000 #x0000 #x0000 #x0000				
		    #xffff #xffff #xffff #xffff
		    #xffff #x0000 #xffff #x0000
		    #xffff #xffff #xffff #xffff
		    #xffff #x0000 #xffff #x0000		
		    #xffff #xffff #xffff #xffff
		    #xffff #x0000 #xffff #x0000))))


(defconstant GL-target-cursor (make-instance 'GL-target))
(defconstant GL-downarrow-cursor (make-instance 'GL-downarrow))
(defconstant GL-hourglass-cursor (make-instance 'GL-hourglass))
(defconstant GL-martini-cursor (make-instance 'GL-martini))
(defconstant GL-flag-cursor (make-instance 'GL-flag))



;;;
;;; GL Supported cursor types
;;;
(defmethod get-GL-cursor-type ((depth (eql 1)) (size-x (eql 16)) (size-y (eql 16))) GL:c16x1)
(defmethod get-GL-cursor-type ((depth (eql 1)) (size-x (eql 32)) (size-y (eql 32))) GL:c32x1)
(defmethod get-GL-cursor-type ((depth (eql 2)) (size-x (eql 16)) (size-y (eql 16))) GL:c16x2)
(defmethod get-GL-cursor-type ((depth (eql 2)) (size-x (eql 32)) (size-y (eql 32))) GL:c32x2)

(defmethod get-GL-cursor-type ((depth integer) (size-x integer) (size-y integer))
  (error "Cursor of depth=~d and size=(~d,~d) not supported"
	 depth size-y size-x))



;;;
;;; Sets the current cursor to the given cursor
;;;
;;; :default   -- sets to default cursor
;;; :crosshair -- sets to the crosshair cursor
;;;
(defmethod set-cursor ((cursor GL-cursor))
  (with-slots (depth size-x size-y hot-x hot-y colormap pixmap) cursor
    (GL:curstype (get-GL-cursor-type depth size-x size-y))
    (GL:with-GL-lock
      (GL:drawmode GL:cursordraw)
      (when colormap
	(loop for color in colormap
	      for index = 1 then (1+ index)
	      do
	      (GL:mapcolor index (first color) (second color) (third color))))
      (GL:drawmode GL:normaldraw)
      (GL:defcursor 1 pixmap)
      (GL:curorigin 1 hot-x hot-y)
      (GL:setcursor 1 0 0))))

(defmethod set-cursor ((cursor (eql :default)))
  (GL:with-GL-lock
    (GL:drawmode GL:cursordraw)
    (GL:mapcolor 1 255 0 0)
    (GL:drawmode GL:normaldraw)
    (GL:setcursor 0 0 0)))

(defmethod set-cursor ((cursor (eql :crosshair)))
  (GL:with-GL-lock
    (GL:drawmode GL:cursordraw)
    (GL:mapcolor 1 255 0 0)
    (GL:drawmode GL:normaldraw)
    (GL:curstype GL:ccross)
    (GL:defcursor 1 (make-array '(1) :element-type '(unsigned-byte 16)))
    (GL:setcursor 1 0 0)))
  



#|

(set-cursor (make-instance 'GL-flag))
(set-cursor (make-instance 'GL-target))
(set-cursor (make-instance 'GL-hourglass))
(set-cursor (make-instance 'GL-martini))
(set-cursor (make-instance 'GL-downarrow))
(set-cursor :crosshair)
(set-cursor :default)

|#
