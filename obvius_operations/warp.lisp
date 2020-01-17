;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: warp.lisp
;;;  Author: Sokolov/Simoncelli
;;;  Description: lookup table warping
;;;  Creation Date: Spring 89
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

(export '(warp compose-warps))

;;; Warp the image according to the warping equation
;;;   B[i][j]  <-  A[i - wx[i][j]][j - wy[i][j]].
;;; edge-handlers are for handling indices beyond the image borders.
;;;  :clip  - copy edge pixel in direction of vector.
;;;  :zero  - act as if the image was zero outside its borders.
;;; *** Should make edge-handling independent of interpolator by using
;;; same edge-handling system as filters.
(defmethod warp ((im image) (vf image-pair)
		 &key edge-handler (interpolator :cubic) ->)
  (with-result ((result ->) (x-component vf)
		'warp im vf :edge-handler edge-handler :interpolator interpolator)
    (case interpolator
      (:cubic (bicubic-warp (data im) (data result) 
			    (data (y-component vf)) (data (x-component vf))
			    (y-dim im) (x-dim im)))
      (:linear
       (case edge-handler
	 (:clip (clip-warp (data im) (data result) 
			   (y-dim im) (x-dim im)
			   (data (y-component vf)) (data (x-component vf))
			   (y-dim vf) (x-dim vf)))
	 (t (bilinear-warp (data im) (data result) 
				 (data (y-component vf)) (data (x-component vf))
				 (y-dim im) (x-dim im)))))
      (t (error "Unkown interpolator: ~A" interpolator)))
    result))

(defmethod warp ((warpee image-pair) (warper image-pair)
		 &key edge-handler (interpolator :cubic)->)
  (with-result ((result ->) warpee
		'warp warpee warper :edge-handler edge-handler :interpolator interpolator)
    (warp (x-component warpee) warper :-> (x-component result)
	  :interpolator interpolator :edge-handler edge-handler)
    (warp (y-component warpee) warper :-> (y-component result)
	  :interpolator interpolator :edge-handler edge-handler)
    result))

;;; This yields a composed warp such that:
;;; (warp im (compose u v))  =  (warp v (warp u im))
(defmethod compose-warps ((u image-pair) (v image-pair)
			  &key edge-handler (interpolator :cubic) ->)
  (with-result ((result ->) u
		'compose u v :edge-handler edge-handler :interpolator interpolator)
    (cond ((or (eq result u) (eq result v))
	   (with-local-viewables ((x (similar result)))
	     (add (warp v u :-> x :edge-handler edge-handler :interpolator interpolator)
		 u :-> result)))
	  (t (add (warp v u :-> result :edge-handler edge-handler :interpolator interpolator)
		 u :-> result)))
    result))

(defmethod compose-warps ((mat1 viewable-matrix) (mat2 viewable-matrix) &key ->)
  (obv::binary-viewable-matrix-op 'compose-warps mat1 mat2 ->))

;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
