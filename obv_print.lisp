;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: obv_print.lisp
;;;  Author: [THO]
;;;  Description: top-level-printing and displaying
;;;  Creation Date:
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adapted for use in SBCL by [THO] in 2019
;; there are some of the definitions from lucid-repl.lisp in here
;; 
(in-package :obv)

;;;[tho] 2019 we use print-object to do all the repl stuff
;;; this is displaying pictures and pop off the eval-queue

;; (defmethod print-object :after ((val viewable) stream)
;;   (when (eval-queue-p)
;;     (status-message "[ATOMATIC] ...handling ~d open events" (eval-queue-length))
;;     (handle-queued-mouse-events))
;;   (when *auto-display-viewables*
;;     (status-message "[AUTOMATIC] displaying ...")
;;     (display val)))
