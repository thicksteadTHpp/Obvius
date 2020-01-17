;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  gl-init.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package 'gl)
(provide 'gl)

(format t ";;; Loading Native GL Foreign Function Interface")

;;; Load generic ffi routines
(obv:obv-source-load "gl-ffi-lucid")
(obv:obv-source-load "gl-ffi-lock")

;;; Load SGI GL interface
(obv:obv-source-load "gl-ffi-constants")
(obv:obv-source-load "gl-ffi-device-constants")
(obv:obv-source-load "gl-ffi-types")
(obv:obv-source-load "gl-ffi-functions")

;;; Load SGI Font Manager interface
(obv:obv-source-load "gl-ffi-fmclient-constants")
(obv:obv-source-load "gl-ffi-fmclient-types")
(obv:obv-source-load "gl-ffi-fmclient-functions")

