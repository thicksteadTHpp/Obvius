;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-patches.lisp
;;;  Author: Patrick C. Teo
;;;  Description: 
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :obvius)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lucid-ffi.lisp
;;;
;;; Some modifications to the original lucid ffi definitions
;;; to make it easier to use by OBVIUS-GL.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(LCL:def-foreign-function (gl::winopen (:return-type :long))
    (name :string))

(LCL:def-foreign-function gl::wintitle
    (name :string))

(LCL:def-foreign-function gl::strwidth
    (str :string))

(LCL:def-foreign-function gl::charstr
    (str :string))

(LCL:def-foreign-function (gl::getmcolor (:name "getmcolor_macro"))
  (arg0 :signed-32bit)
  (arg1 (:array :signed-16bit))
  (arg2 (:array :signed-16bit))
  (arg3 (:array :signed-16bit)))

(LCL:def-foreign-function gl::qread
  (arg0 (:array :signed-16bit)))

(LCL:def-foreign-function gl::getorigin
  (arg0 (:array :signed-32bit))
  (arg1 (:array :signed-32bit)))

(LCL:def-foreign-function gl::getsize
  (arg0 (:array :signed-32bit))
  (arg1 (:array :signed-32bit)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; flipbook.lisp
;;;
;;; Ugly!  Dependency in flipbook.lisp on this function
;;; being present.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-subpic-dialog (pic old-subpic new-subpic))

