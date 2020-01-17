;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: glfw-nk-font-helper.lisp
;;;  Author: THO
;;;  Description: helper for handling font creation (baking) in nuklear
;;;               and determining the size of drawn (rendered) fonts
;;;  Creation Date: 01/20
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;   
;;;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl:in-package :obv)

(def-simple-class font-helper ()
  (image  ;;image pointer
   font   ;;nk-font struct
   (atlas :initform (claw:calloc '(:struct (%nk:font-atlas))))
   (font-tex :initform 0) ;; the texture with the baked (rendered) font
   (null.device :initform (claw:calloc '(:struct (%nk:draw-null-texture)))) ;; nk-draw-null-texture should be set in the bodge-renderer
   (width :initform  (cffi:foreign-alloc :int :initial-element 0)) ;;image width allocated from lisp
   (height :initform (cffi:foreign-alloc :int :initial-element 0 )))) ;;image width allocated from lisp

(defun make-font-helper ()
  (make-instance 'font-helper))


;; if destroy is called outside the render thread send destroy texture
;; to the render thread otherwise no gl-context is available
(defmethod destroy ((fh font-helper) &rest args &key dispatcher &allow-other-keys)
  (declare (ignore args))
  
  (with-slots (atlas width height null.device font-tex) fh
    (cffi:foreign-free width)
    (cffi:foreign-free height)
    (claw:free null.device)
    (when (and atlas (not (claw:wrapper-null-p atlas)))
      (%nk:font-atlas-clear atlas)
      (claw:free atlas))
    (when (and (< 0 font-tex)
	       dispatcher)
      (destroy-texture dispatcher font-tex))))
      ;; (send-to dispatcher (lambda (disp)
      ;; 			    (declare (ignore disp))
      ;; 			    (vom:info "[font-helper] destroy texture ~d" font-tex)
      ;; 			    (gl:delete-texture font-tex))))))
  


(defmethod font-helper-width ((fh font-helper))
  (cffi:mem-ref (slot-value fh 'width) :int))

(defmethod font-helper-height ((fh font-helper))
  (cffi:mem-ref (slot-value fh 'height) :int))

(defgeneric bake-nk-default-font (font-helper))

(defmethod bake-nk-default-font ((fh font-helper))
  (with-slots (atlas font font-tex image width height null.device) fh
    (%nk:font-atlas-init-default atlas)
    (%nk:font-atlas-begin atlas)
    (setf font (%nk:font-atlas-add-default atlas 13f0 (cffi:null-pointer))
	  image (%nk:font-atlas-bake atlas width height %nk::+font-atlas-rgba32+)
	  font-tex (device-upload-font-atlas image (font-helper-width fh) (font-helper-height fh)))
    (%nk:font-atlas-end atlas (%nk:handle-id font-tex) (claw:ptr null.device))))
    
;;returns a pointer to the font->handle
;;needed for nk_init_default
;;returns a pointer to a nk_user_font struct
(defmethod font-helper-font-handle ((fh font-helper))
  (claw:c-val ((font (:struct (%nk:font)) (slot-value fh 'font)))
    (font :handle &)))



;; for font handling we need to upload the image
;; of a baked font to GL
(defun device-upload-font-atlas (image width height)
  (let ((tex (gl:gen-texture)))
    (if (/= 0 tex)
	(progn
	  (gl:bind-texture :texture-2d tex)
	  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
	  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
	  (gl:tex-image-2d :texture-2d 0 :RGBA width height 0 :rgba :unsigned-byte image))
	(vom:warn "[device-upload-font] Could not generate texture for font upload"))
    tex))

(defmethod font-helper-font-text-width ((fh font-helper) (text string))
  (claw:c-val ((font (:struct (%nk:font)) (slot-value fh 'font)))
    (nk-font-text-width (font &) (font :handle :height) text (length text))))


;;; try to use the callback in the nk_user_font struct - does not work in sbcl 
;; (defun test-width ()
;;   ;; (let ((cb (ctx :style :font :width &)) ;;(font :handle :width &))
;;   ;; 	(userdata (ctx :style :font :userdata &))
;;   ;; 	(height   (ctx :style :font :height))
;;   ;; 	(s (cffi:foreign-alloc :char :initial-contents (map 'list #'char-code "12345") :null-terminated-p nil))
;;   ;; 	(unicode (cffi:foreign-alloc :uint)))
;;     ;;(format t "~Text width ~d" (cffi:foreign-funcall-pointer cb () :pointer userdata :float height :pointer s :int 5 :float))
;;   (let ((s (cffi:foreign-alloc :char :initial-contents (map 'list #'char-code "12345") :null-terminated-p nil)))
;;     (claw:c-val ((font (:struct (%nk:font)) (slot-value glfw.font-helper 'font)))
;;       (format t "~Text width ~d" (cffi:foreign-funcall-pointer (font :handle :width &) () :pointer (font :handle :userdata &) :float (font :handle :height) :pointer s :int 5 :float)))
;;     (cffi:foreign-free s)))


;;this is a more or less accurate translation from c-code in nk_font_text_width from nuklear.h
;;needed because the font_width_callback from nuklear and bodge-nuklear doesn't work properly
;;maybe not needed for future releases
(defun nk-font-text-width (nk-font height text &optional (len (length text)))
  (when (<= len 0)
    (return-from nk-font-text-width 0f0))
  (when (cffi:null-pointer-p nk-font)
    (return-from nk-font-text-width 0f0))
  (when (claw:wrapper-null-p nk-font)
    (return-from nk-font-text-width 0f0))
  (cffi:with-foreign-object (unicode :uint)
    (cffi:with-foreign-string (ctext text)
      (let ((text-len 0)
	    (text-width 0f0)
	    (glyph-len 0)
	    (scale 0f0))
	(claw:c-val ((font (:struct (%nk:font)) nk-font))
	  (setf scale (float (/ height (font :info :height)) 1f0)
		glyph-len (%nk:utf-decode ctext unicode len)
		text-len glyph-len)
	  (when (<= glyph-len 0)
	    (return-from nk-font-text-width text-width))
	  (loop while (and (<= text-len len)
			   (> glyph-len 0))
		do
		(if (= (cffi:mem-ref unicode :uint) %nk:+utf-invalid+)
		    (return-from nil text-width))
		(claw:c-with ((g (:struct (%nk:font-glyph)) :from (%nk:font-find-glyph (font &) (cffi:mem-ref unicode :uint))))
		  (incf text-width (* (g :xadvance) scale))
		  (setf glyph-len (%nk:utf-decode (cffi:inc-pointer ctext text-len) unicode (- len text-len)))
		  (incf text-len glyph-len)))
	  text-width)))))



  
