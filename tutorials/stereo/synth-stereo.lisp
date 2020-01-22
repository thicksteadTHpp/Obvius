
;;; Author: David Fleet
;;; Date: Feb 1994

(in-package :user)
(obv-require :matrix)

;;;========================================================================
;;; Generate warping vectors fields to generate left and right views of
;;; a slanted plane
;;;------------------------------------------------------------------------
;;; Approach: assume there is a generic plane (Z=0, the texture map) and
;;;   our 3d plane in the scene is an affinely deformed version of it.
;;;   Given ray through pixel (i,j), find its intersection on deformed
;;;   plane to find 3d point, and the corresponding intersection point
;;;   on the generic plane (from which we warp the texture.
;;;   This yields the warping maps to the left and right images.
;;;
;;;   To find warp from left to right view, take 3d point intersected
;;;   from left eye's view from ray tracer, transform 3d point from left
;;;   eye centred coordinates to right-eye centred coordinates, and then
;;;   perspectively project onto right-eye image plane.
;;;

;;;-----------------------------------------------------------------------
;;; basic CCW rotation and translation matrices homogeneous coord.
;;; asuming right-handed-coord.

(defun make-Rz (angle &key homo)
  (let (result 
        (a (* (/ pi 180.0) angle)))
     (setq result
       (if homo
         (make-matrix (list (list (cos a) (- (sin a)) 0  0)
                            (list (sin a) (cos a)     0  0)
                            (list 0       0           1  0)
                            (list 0       0           0  1)))
         (make-matrix (list (list (cos a) (- (sin a)) 0)
                            (list (sin a) (cos a)     0)
                            (list 0       0           1)))))
     result))

(defun make-Rx (angle &key homo)
  (let (result 
        (a (* (/ pi 180.0) angle)))
     (setq result
       (if homo
         (make-matrix (list (list 1  0       0           0)
                            (list 0  (cos a) (- (sin a)) 0)
                            (list 0  (sin a) (cos a)     0)
                            (list 0       0           0  1)))
         (make-matrix (list (list 1  0       0          )
                            (list 0  (cos a) (- (sin a)))
                            (list 0  (sin a) (cos a)    )))))
     result))

(defun make-Ry (angle &key homo)
  (let (result 
        (a (* (/ pi 180.0) angle)))
     (setq result
       (if homo
         (make-matrix (list (list (cos a)     0  (sin a) 0)
                            (list 0           1  0       0)
                            (list (- (sin a)) 0  (cos a) 0)
                            (list 0       0           0  1)))
         (make-matrix (list (list (cos a)     0  (sin a))
                            (list 0           1  0      )
                            (list (- (sin a)) 0  (cos a))))))
     result))


(defun make-trans (tx ty tz)
  (let (result)
     (setq result
         (make-matrix (list (list 1  0  0  tx)
                            (list 0  1  0  ty)
                            (list 0  0  1  tz)
                            (list 0  0  0  1))))
     result))


(defun make-persp (f)
  (let (result)
     (setq result
         (make-matrix (list (list 1  0  0          0)
                            (list 0  1  0          0)
                            (list 0  0  1          0)
                            (list 0  0  (/ 1.0 f) 1))))
     result))


;;;-----------------------------------------------------------------------
;;; Extract linear component of affine deformation to map points on generic
;;; plane (Z=0) to an affinely deformed version based on stereo geometry.
;;; The plane is rotated about Z and X axes first, and then differently
;;; about Y axis to simulate a convergent camera geometry with vergence
;;; angle d-ay (Angles in degrees -- transforms not homogeneous)

(defun left-right-obj-rotate (ax ay az d-ay &key homo)
   (let* ((Az (make-Rz az :homo homo))
          (Ax (make-Rx ax :homo homo))
          (Ay-l (make-Ry (+ ay d-ay) :homo homo))
          (Ay-r (make-Ry (- ay d-ay) :homo homo)) 
          Rl Rr tmp)
     (setq tmp (matrix-mul Ax Az)) 
     (setq Rl (matrix-mul Ay-l tmp)) 
     (setq Rr (matrix-mul Ay-r tmp)) 
     (values Rl Rr)))


;;;-----------------------------------------------------------------------
;;; Given: distance to plane along line of sight tz
;;;        vergence angle d-ay
;;; returns the effective baseline between cameras

(defun baseline (d-ay tz)
   (let ((a (* (/ pi 180.0) d-ay)))
      (* 2 (* tz (tan a)))))

;;;-----------------------------------------------------------------------
;;; find x-component and y-component of the map from pixel i,j in some view 
;;; to the generic plane (Z=0) and an affinely deformed version of it, where
;;; the affine deformation is a 3by3 rotation matrix A and translation tvec.
;;;
;;; This will NOT be the warp-map ... it gives 2d coord. in texture image.
;;; We should subtract pixel positions in texture image to get warp map
;;;
;;;   NOte that because all vectors here are row-vectors, the operation
;;;     (matrix-mul bvec A) amounts to Atranspose bvec if bvec were column
;;;     moreover, since A is orthogonal here, Atranspose = Ainverse

(defun find-texture-index (size A tvec f &key ->)
  (with-local-viewables
      ((x (make-ramp (list size size)))
       (xx (/. x (/ size 2)))
       (yy (transpose xx))
       (ff (fill! (make-image (list size size)) f))
       (bvec (make-image-matrix (list (list xx yy ff))))
       (avec (make-matrix (list 0 0 (- f))))
       (new-a (matrix-mul (-. avec tvec) A)) 
       (az (- (aref new-a 2)))
       (new-b (matrix-mul bvec A))
       (lambda (/. az (aref (matrix new-b) 0 2)))
       (rvec (my-add-vector-image new-a (*. new-b lambda))))
    (make-complex-image (list (aref (matrix rvec) 0 1)
			      (aref (matrix rvec) 0 0))
			:-> ->)))

;;; shades a region of the texture map with a checkboard
(defun shade-index-map (index)
  (let* ((rr (real-part index))
	 (ii (imaginary-part index)))
    (with-local-viewables
	((a1 (abs. ii))
	 (a2 (abs. rr))
	 (b1 (less-than a1 30.0))
	 (b2 (less-than a2 30.0))
	 (b (mul b1 b2))
	 (d1 (periodic-point-operation a1 #'(lambda (x)
					      (- (mod x 6.0) 3.0)) 6))
	 (d2 (periodic-point-operation a2 #'(lambda (x)
					      (- (mod x 6.0) 3.0)) 6))
	 (e1 (image-from-bit-image (greater-than d1 0.0)))
	 (e2 (image-from-bit-image (greater-than d2 0.0)))
	 (d (mul b (periodic-point-operation (+. e1 e2 :-> e2)
					    #'(lambda (x) (mod x 2.0)) 2)))
	 (e (mul d 6.0)))
      (+. (image-from-bit-image b) e))))

;;; subtract texture image from pixel location to get texture warp maps
(defun get-warp-maps (r-index l-index scale)
  (let* ((size (dimensions (real-part r-index))))
    (with-local-viewables
	((x (make-ramp size))
	 (y (transpose x))
	 (c (make-complex-image (list y x)))
	 (new-r (mul scale r-index))
	 (new-l (mul scale l-index)))
      (values (sub c new-r) (sub c new-l)))))

;;;------------------------------------------------------------------------
;;; find mapping from left view to right view
;;; - ray trace to find 3d points for each pixel location in left view
;;; - convert points to homogen coord
;;; - rotation to map points from left-centred coords to right-centred coords
;;; - apply perspective projection matrix, divide by 4th component
;;;   and extract first two terms of vector
;;; - take difference to compute warp map

(defun make-3dpoints (xx yy size A tvec f)
  (let* ((ff (+. f (make-image (list size size))))
	 (bvec (make-image-matrix (list (list xx yy ff))))
	 (avec (make-matrix (list 0 0 (- f))))
         (new-a (matrix-mul (-. avec tvec) A)) 
         (az (- (aref new-a 2)))
	 (new-b (matrix-mul bvec A))
	 (lambda (/. az (aref (matrix new-b) 0 2))))
    (my-add-vector-image avec (*. lambda bvec))))

(defun left-to-right (image-vector d-ay tz)
   (let* ((T1 (make-trans 0 0 (- tz)))
          (Ay (make-Ry (* 2.0 d-ay) :homo t)) 
          (T2 (make-trans 0 0 tz))
	  (tmp1 (matrix-mul Ay T1)) 
	  (tmp2 (matrix-mul T2 tmp1))
	  (new-vector (image-vector-to-homo-coord image-vector)))
     (matrix-mul-transpose new-vector tmp2)))

(defun persp-proj (image-vector f)
  (let* ((P (make-persp f))
	 tmp)
    (setq tmp (matrix-mul-transpose image-vector P))
    (make-complex-image
     (list (/. (aref (matrix tmp) 0 1) (aref (matrix tmp) 0 3))
	   (/. (aref (matrix tmp) 0 0) (aref (matrix tmp) 0 3))))))


(defun get-left-right-map (size Al dist f d-ay tvec)
  (let* ((x (make-ramp (list size size)))
	 (xx (/. x (/ size 2)))
	 (yy (transpose xx))
	 Xl-3vec Xr-3vec xr-2vec r-to-l)
    (setq Xl-3vec (make-3dpoints xx yy size Al tvec f))
    (setq Xr-3vec (left-to-right Xl-3vec d-ay dist))
    (setq xr-2vec (persp-proj Xr-3vec f))
    (setq r-to-l (*. (/ size 2.0)
		     (-. (make-complex-image (list yy xx)) xr-2vec)))))

;;;--------------------------------------
;;; basic utilities 

(defun image-vector-to-homo-coord (im-matrix)
  (let* ((dims (dimensions (aref (matrix im-matrix) 0 2)))
	 (h (+. 1.0 (make-image dims)))
	 tmp)
    (setq tmp (loop for i from 0 below 3
		    for im = (aref (matrix im-matrix) 0 i)
		    collect im))
    (setq tmp (append tmp (list h)))
    (make-image-matrix (list tmp))))

(defun my-add-vector-image (vector image-matrix)
  (let* (tmp result)
    (setq tmp (loop for i from 0 below (length vector)
		    for a = (aref vector i)
		    for im = (aref (matrix image-matrix) 0 i)
		    collect (+. im a)))
    (setq result (make-image-matrix (list tmp)))))


;;;--------------------------------------------------------------------------
;;; combine above results to find texture maps and left to right disparity map

(defun get-LR-warp-maps (ax ay az d-ay dist f
			 &key (size 128) (scale 1.0) (do3d-map t))
   (let* (r-index l-index r-map l-map r-to-l
	  (tvec (make-matrix (list 0 0 dist))))
      (format t "Baseline is: ~f ~%" (baseline d-ay dist))
      (multiple-value-bind (Ar Al)             ;;; are these reversed?
	  (left-right-obj-rotate ax ay az d-ay)
	(setq r-index (find-texture-index size Ar tvec f))
	(setq l-index (find-texture-index size Al tvec f))
        (multiple-value-setq (r-map l-map)
	  (get-warp-maps r-index l-index scale))
	(when do3d-map
	  (setq r-to-l (get-left-right-map size Al dist f d-ay tvec))))
      (values r-map l-map r-index l-index r-to-l)))

(defun make-stereo-pair (im 
			 &key (x-rot-angle -20) (y-rot-angle 0) (z-rot-angle 0)
			 (vergence 6) (dist 15) (f 1)
			 (size (x-dim im)) (scale 5) (do3d-map t))
  (multiple-value-bind (r-map l-map r-index l-index r-to-l)
      (get-LR-warp-maps x-rot-angle y-rot-angle z-rot-angle
			vergence dist f
			:size size :scale scale :do3d-map do3d-map)
    (values (make-image-pair (list (warp im l-map)
				   (warp im r-map)))
	    r-to-l)))
  

#|
;;-------testing--------------

(compile-load "~fleet/lisp/synth-stereo")

(setq einstein (load-image "~fleet/images/einstein"))
(setq sin1 (make-sin-grating '(128 128) :orientation 0 
			     :x-freq 0.5 :y-freq 0.5)
      sin2 (make-sin-grating '(128 128) :orientation 0 
			     :x-freq 0.5 :y-freq -0.5)
      plaid (+. sin1 sin2))

(multiple-value-setq (r-map l-map r-index l-index r-to-l)
    (get-LR-warp-maps -20 0 0 6 15 1 :size 128 :scale 5 :do3d-map t))

(shade-index-map l-index)
(shade-index-map r-index)

(progn
  (setq im (gauss-out einstein))
  (setq pair (make-image-pair (list (warp im l-map) (warp im r-map)))))

(setq plaid-pair (make-image-pair (list (warp plaid l-map)
					(warp plaid r-map))))

(setq tmp (warp (right-image pair) r-to-l))
(sub tmp (left-image pair))
|#
