;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-screen.lisp
;;;  Author: Patrick C. Teo
;;;  Description: Generic screen handling stuff
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GL Screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (eval load)
  (pushnew 'initialize-GL-window-system *initialization-functions*))

(defun initialize-GL-window-system ()
  (setf *screen-list* nil))


;;; "GL-screen" represents a connection to a single GL-screen.
;;; This class consists of obvius-dependent stuff in "screen"
;;; and a GL-dispatcher which handles events like mouse clicks etc.
;;;
;;; This is an abstract superclass which should not be used.
;;; Instead, each type of SGI screen is supported by
;;; specializing this class.  For example, the "8bit-GL-screen"
;;; class is meant for SGI machines with 8 bit screens.
;;;

(def-simple-class GL-screen (screen GL-dispatcher)
  (foreground
   background
   (depth       :type integer)
   (gray-depth  :type integer)
   (x-dim       :type integer)
   (y-dim       :type integer)
   (activated-p :type (member nil t) :initform nil)

   ;;; Gray LUT
   (gray-lut    :initform nil)
   (gray-shades :type (integer 2 255)
		:documentation "Determines the number of gray levels allocated in the colormap.")
   (gray-gamma  :type (float 0.1 10.0)
	        :documentation "Determines the gamma correction factor of the colormap.")
   (gray-dither :type (or (member nil t) number)
		:initform 64
		:documentation "Determines whether pictures are dithered into gray shades in the colormap.  If t, dither. If nil, just quantize.  If a number, the pictures will be dithered if the value of gray-shades is less than this number.")

   ;;; Color LUT
   (color-lut  :initform nil)
   (rgb-bits   :type cons
	       :documentation "List of three integers that determine the number of bits for dithering color pictures.")
   (rgb-gamma  :type cons
	       :documentation "List of three reals representing the gamma ramps of each color channel")

   ;;; Pseudo Color LUT
   (pseudo-color-lut      :initform nil)
   (pseudo-colors         :type (integer 0 255)
		          :documentation
		           "Determines the number of pseudo colors allocated in the colormap.")
   (pseudo-color-function :documentation
			  "Function used to allocate pseudo-colors, argument is an integer, returns a list of rgb values.")
   ))
   
   

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic GL Screen Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initialize-instance :after ((screen GL-screen) &rest initargs)
  (declare (ignore initargs))
  (push screen *screen-list*))

;;; called by both initialize-instance and reinitialize-instance
(defmethod shared-initialize :after ((screen GL-screen) slot-names
				     &rest initargs
				     &key foreground background)
  (declare (ignore slot-names initargs))
  (when foreground
    (setf (foreground screen) foreground))
  (when background
    (setf (background screen) background))
  screen)


;;; since most of the GL commands cannot be called until
;;; after winopen() is issued, we have to delay most
;;; of the initialization routines till after the winopen
;;; has been issued.
(defmethod activate-screen ((screen GL-screen))
  (unless (activated-p screen)
    (setf (x-dim screen) (GL:getgdesc GL:gd-xpmax))
    (setf (y-dim screen) (GL:getgdesc GL:gd-ypmax))
    (activate-GL-dispatcher screen)))


(defmethod (setf foreground) (color-desc (screen GL-screen))
  (setf (slot-value screen 'foreground)
	(compute-GL-color screen color-desc)))
  
(defmethod (setf background) (color-desc (screen GL-screen))
  (setf (slot-value screen 'background)
	(compute-GL-color screen color-desc)))

(defmethod dimensions ((screen GL-screen)) `(,(y-dim screen) ,(x-dim screen)))


;;; REQUIRED:: (make-screen)
;;; we should make it check platform and make instance of
;;; the appropriate screen class.

(defun make-screen (&rest initargs)
  (let* ((depth (+ (GL:getgdesc GL:gd-bits-norm-sng-red)
		   (GL:getgdesc GL:gd-bits-norm-sng-green)
		   (GL:getgdesc GL:gd-bits-norm-sng-blue)))
	 (GL-screen-type (cond ((= depth 8) '8bit-GL-screen)
			       ((= depth 24) '24bit-GL-screen)
			       (t (error "Unable to make screen of depth ~d~%" depth)))))
    (apply 'make-instance GL-screen-type initargs)))



;;; receive event and dispatch to the appropriate window
;;; if the window had registered to receive it.
(defmethod receive-dispatch ((screen GL-screen) window device data)
  (with-slots (pane-list) screen
    (let ((result (member window pane-list :test #'equal :key #'wid))) 
      (when (consp result)
	(receive-window-dispatch (car result) device data)))))

;;; hack to collect any unwanted events
(defgeneric receive-event (window device data)
  (:documentation
   "Receive an event from the screen's dispatcher"))

(defmethod settable-parameters ((class-name (eql 'GL-screen)))
  (append '(foreground
	    background
	    rgb-bits
	    rgb-gamma
	    pseudo-colors
	    pseudo-color-function
	    gray-shades
	    gray-gamma
	    gray-dither)
	  (call-next-method)))

(defmethod destroy ((screen GL-screen) &key &allow-other-keys)
  (when (find *current-pane* (pane-list screen))
	      (setq *current-pane* nil))
  (loop for pane in (pane-list screen) do
	(destroy pane))
  (GL:gexit)
  (setq *screen* (remove screen *screen-list*)))

;;; This should *really* not be here as it is "screen" method
(defmethod set-not-current ((the-screen screen))
  (loop for pane in (pane-list the-screen) do
	(loop for pic in (picture-stack pane) do
	      (set-not-current pic))))

