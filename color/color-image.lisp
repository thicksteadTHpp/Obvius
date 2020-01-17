;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: color-image.lisp
;;;  Author: Simoncelli/Heeger
;;;  Description: RGB and YIQ color images
;;;  Creation Date: 9/90
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '( make-color-image color-image-p color-transform rgb->yiq yiq->rgb))

;;; This is unfinished code.  We are writing a full color analysis
;;; package that keeps track of color-spaces, input devices (e.g.,
;;; photoreceptors, cameras, scanners), output devices (e.g.,
;;; monitors, printers), spectral sensitivities of these input and
;;; output devices, and spectral properties of surface reflectances.
;;; For the time being, this code defines simple color images that can
;;; be displayed as color pictures or as pasteups.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Color-image object

(defmacro color-image-p (obj)
  `(typep ,obj 'color-image))

(defmethod red-component ((im color-image) &key &allow-other-keys)
  (frame 0 im))
(defmethod green-component ((im color-image) &key &allow-other-keys)
  (frame 1 im))
(defmethod blue-component ((im color-image) &key &allow-other-keys)
  (frame 2 im))

(defmethod y-component ((im color-image) &key &allow-other-keys)
  (frame 0 im))
(defmethod i-component ((im color-image) &key &allow-other-keys)
  (frame 1 im))
(defmethod q-component ((im color-image) &key &allow-other-keys)
  (frame 2 im))

(defun make-color-image (imlist-or-dims &rest initargs &key name length display-type ->)
  (declare (ignore display-type length name))
  (when -> (setf (getf initargs :name) ->))
  (with-result ((result nil)
		`(:class color-image
		  ,@(if (every #'integerp imlist-or-dims)
			(list :dimensions imlist-or-dims)
			(list :viewable-list imlist-or-dims))
		  ,@initargs)
		'make-color-image imlist-or-dims)
    result))

(defmethod print-object ((c-im color-image) stream)
  (format stream "#<~A " (object-class-name c-im))
  (format stream "~S" (when (slot-boundp c-im 'name) (name c-im)))
  (format stream ">"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod color-transform ((im color-image) array &key ->)
  (matrix-mul-transpose im array :-> ->))
  
(defmethod rgb->yiq ((rgb color-image) &key ->)
  (with-result ((yiq ->) rgb 'rgb->yiq rgb)
    (let ((xform (make-array '(3 3) :initial-contents '((0.299 0.587 0.114)
							(0.596 -0.273 -0.322)
							(0.212 -0.522 0.315)))))
      (color-transform rgb xform :-> yiq))))

(defmethod yiq->rgb ((yiq color-image) &key ->)
  (with-result ((rgb ->) yiq 'yiq->rgb yiq)
    (let ((xform (make-array '(3 3) :initial-contents '((0.9959 0.9581 0.6189) 
							(1.0035 -0.2745 -0.6437) 
							(0.9926 -1.0996 1.6913)))))
      (color-transform yiq xform :-> rgb))))

#|
(setq al (gauss-out einstein))

(setq cim (make-color-image (list al al al)))
(setq new-cim (add cim cim))

(setq matrix (make-array '(3 3) :element-type 'single-float
			 :initial-contents '((1.0 1.0 0.0)
					     (1.0 -1.0 0.0)
					     (1.0 1.0 -1.0))))
(setq opponent-cim (matrix-mul-transpose cim matrix))
(rgb->yiq cim)

(setq cim1 (similar cim))
(matrix-mul matrix (columnize cim) :-> (columnize cim1))

(progn
  (load-image "/home/heeger/images/clown")
  (change-class clown 'color-image) 
  (setq little-clown (gauss-out clown)))
(display little-clown 'color-picture)
|#



;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
