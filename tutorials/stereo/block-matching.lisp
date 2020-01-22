
#|
(my-compile-load "stereo/block-matching")
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *lkernel-9*)

(setq *lkernel-9*
      '(0.012362755835056305 0.053224027156829834 0.12405790388584137
	0.1964934766292572 0.22759294509887695 0.1964934766292572
	0.12405790388584137 0.053224027156829834 0.012362755835056305))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Utilities for finding best match (max/min) of correlation/SSD

(defmethod frame-with-min-value ((mat image-matrix) &key ->)
  (with-result ((res ->) (list :class 'image-pair :dimensions (dimensions mat)))
    (with-result ((min-values ->) (list :class 'image :dimensions (dimensions mat)))
      (with-local-viewables
	  ((frame-fill (similar res))
	   (less-than-map (make-bit-image (dimensions mat))))
	(fill! min-values (maximum mat))
	(loop for j from 0 below (row-dim mat)
	      do
	      (loop for i from 0 below (col-dim mat)
		    for im = (aref (matrix mat) j i)
		    do
		    (less-than im min-values :-> less-than-map)
		    (point-minimum im min-values :-> min-values)
		    (fill! (y-component frame-fill) j)
		    (fill! (x-component frame-fill) i)
		    (mul less-than-map frame-fill :-> frame-fill)
		    (invert less-than-map :-> less-than-map)
		    (mul less-than-map res :-> res)
		    (add frame-fill res :-> res))))
      (values res min-values))))

(defmethod frame-with-max-value ((mat image-matrix) &key ->)
  (with-result ((res ->) (list :class 'image-pair :dimensions (dimensions mat)))
    (with-result ((max-values ->) (list :class 'image :dimensions (dimensions mat)))
      (with-local-viewables
	  ((frame-fill (similar res))
	   (greater-than-map (make-bit-image (dimensions mat))))
	(fill! max-values 0.0)
	(loop for j from 0 below (row-dim mat)
	      do
	      (loop for i from 0 below (col-dim mat)
		    for im = (aref (matrix mat) j i)
		    do
		    (greater-than im max-values :-> greater-than-map)
		    (point-maximum im max-values :-> max-values)
		    (fill! (y-component frame-fill) j)
		    (fill! (x-component frame-fill) i)
		    (mul greater-than-map frame-fill :-> frame-fill)
		    (invert greater-than-map :-> greater-than-map)
		    (mul greater-than-map res :-> res)
		    (add frame-fill res :-> res))))
      (values res max-values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sum-of-squared differences:

(defun compute-ssd-mat (im1 im2
		        &key
			(x-shift-range '(-8 8))
			(y-shift-range '(0 0))
			(blur-kernel (mul (sqrt 2) *lkernel-9*))
			(blur-level 1)
			(edge-handler :dont-compute))
  (with-local-viewables
      ((tmp (similar im1))
       (shift-im1 (similar im1))
       (squared-diff (similar im1)))
    (make-image-matrix
     (loop for y-shift from (first y-shift-range) to (second y-shift-range)
	   collect
	   (loop for x-shift from (first x-shift-range) to (second x-shift-range)
		 collect
		 (progn
		   (circular-shift im1 :x x-shift :y y-shift :-> shift-im1)
		   (square (sub shift-im1 im2 :-> tmp) :-> squared-diff)
		   (blur squared-diff
			 :kernel blur-kernel
			 :level blur-level
			 :edge-handler edge-handler)))))))

(defun compute-correlation-mat (im1 im2
				&key
				(x-shift-range '(-8 8))
				(y-shift-range '(0 0))
				(blur-kernel (mul (sqrt 2) *lkernel-9*))
				(blur-level 1)
				(edge-handler :dont-compute))
  (with-local-viewables
      ((tmp (similar im1))
       (shift-im1 (similar im1))
       (product (similar im1)))
    (make-image-matrix
     (loop for y-shift from (first y-shift-range) to (second y-shift-range)
	   collect
	   (loop for x-shift from (first x-shift-range) to (second x-shift-range)
		 collect
		 (progn
		   (circular-shift im1 :x x-shift :y y-shift :-> shift-im1)
		   (mul shift-im1 im2 :-> product)
		   (blur product
			 :kernel blur-kernel
			 :level blur-level
			 :edge-handler edge-handler)))))))

(defun block-matching-ssd (im1 im2
			   &key ->
			   (x-shift-range '(-8 8))
			   (y-shift-range '(0 0))
			   (blur-kernel (mul (sqrt 2) *lkernel-9*))
			   (blur-level 1)
			   (edge-handler :dont-compute))
  (with-result ((res ->) (list :class 'image-pair :dimensions (dimensions im1)))
    (with-local-viewables
	((ssd (compute-ssd-mat im1 im2
			       :x-shift-range x-shift-range
			       :y-shift-range y-shift-range
			       :blur-kernel blur-kernel
			       :blur-level blur-level
			       :edge-handler edge-handler))
	 (min-frame (frame-with-min-value ssd)))
      (add (first x-shift-range) (x-component min-frame) :-> (x-component res))
      (add (first y-shift-range) (y-component min-frame) :-> (y-component res)))
    res))

(defun block-matching-correlation (im1 im2
				   &key ->
				   (x-shift-range '(-8 8))
				   (y-shift-range '(0 0))
				   (blur-kernel (mul (sqrt 2) *lkernel-9*))
				   (blur-level 1)
				   (edge-handler :dont-compute))
  (with-result ((res ->) (list :class 'image-pair :dimensions (dimensions im1)))
    (with-local-viewables
	((correlation (compute-correlation-mat im1 im2
					       :x-shift-range x-shift-range
					       :y-shift-range y-shift-range
					       :blur-kernel blur-kernel
					       :blur-level blur-level
					       :edge-handler edge-handler))
	 (max-frame (frame-with-max-value correlation)))
      (add (first x-shift-range) (x-component max-frame) :-> (x-component res))
      (add (first y-shift-range) (y-component max-frame) :-> (y-component res)))
    res))


#|
;;; noise
(setq im1 (make-gaussian-noise '(64 64)))
(setq im2 (circular-shift im1 :x-shift 3 :y-shift -2))

(display (setq disparity (block-matching-ssd im1 im2
					     :x-shift-range '(-5 5)
					     :y-shift-range '(-5 5)))
	 'vector-field
	 :scale 4.0)

(display (setq disparity (block-matching-correlation im1 im2
						     :x-shift-range '(-5 5)
						     :y-shift-range '(-5 5)))
	 'vector-field
	 :scale 4.0)


;;; trees
(my-compile-load "obvius/image-hacks")
(setq path1 (format nil "/home/heeger/images/trees/trees-0~a.pic" 50))
(setq path2 (format nil "/home/heeger/images/trees/trees-0~a.pic" 52))
(setq im1 (load-image-no-header path1 256 233
				:data-type '(unsigned-byte 8)
				:skip-bytes 0))
(setq im2 (load-image-no-header path2 256 233
				:data-type '(unsigned-byte 8)
				:skip-bytes 0))
(setq disparity (block-matching-ssd im1 im2 :shift-range '(-8 8)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Nishihara, bit correlation.

;;; *** Should do interpolation to find peak.

(defvar *laplacian-11*)

(setq *laplacian-11*
      (data (make-synthetic-image
	     '(11 11)
	     #'(lambda (y x)
		 (let* ((w 4)
			(r-sqr (+ (sqr x) (sqr y)))
			(w-sqr (sqr w))
			(tmp (/ (* 4 r-sqr) w-sqr)))
		   (* (- 1 tmp) (exp (- tmp)))))
	     :x-range '(-5 5))))

(defun nishihara-correlation (im1 im2
			      &key ->
			      (x-shift-range '(-8 8))
			      (y-shift-range '(0 0))
			      (laplacian-kernel *laplacian-11*)
			      (blur-kernel (mul (sqrt 2) *lkernel-9*))
			      (blur-level 1)
			      (thresh nil)
			      (edge-handler :dont-compute))
  (with-result ((res ->) (list :class 'image-pair :dimensions (dimensions im1)))
    (with-local-viewables
	((tmp (similar im1))
	 (laplacian-filter (make-filter laplacian-kernel :edge-handler edge-handler))
	 (dog1 (apply-filter laplacian-filter im1))
	 (dog2 (apply-filter laplacian-filter im2))
	 (thresh-dog1 (greater-than dog1 0))
	 (thresh-dog2 (greater-than dog2 0))
	 (sign-dog1 (add -1 (mul 2 (coerce-to-float thresh-dog1 :-> tmp) :-> tmp)))
	 (sign-dog2 (add -1 (mul 2 (coerce-to-float thresh-dog2 :-> tmp) :-> tmp)))
	 (correlation (compute-correlation-mat sign-dog1 sign-dog2
					       :x-shift-range x-shift-range
					       :y-shift-range y-shift-range
					       :blur-kernel blur-kernel
					       :blur-level blur-level
					       :edge-handler edge-handler))
	 max-frame max-values)
      (multiple-value-setq (max-frame max-values)
	(frame-with-max-value correlation))
      (add (first x-shift-range) (x-component max-frame) :-> (x-component res))
      (add (first y-shift-range) (y-component max-frame) :-> (y-component res))
      (display (copy dog1))
      (display (copy dog2))
      (display (copy sign-dog1))
      (display (copy sign-dog2))
      ;;(display (copy max-values :-> "max-values"))
      (when thresh
	(with-local-viewables ((thresh-max-values (>. max-values thresh)))
	  (mul thresh-max-values res :-> res))))
    res))

#|
;;; Keith uses Laplacian of Gaussian filters.  What size operators to
;;; use?  Nishihara '84 suggests 10x10 with w=4:

(let ((w 4))
  (setq laplacian-10
	(make-synthetic-image
	 '(11 11)
	 #'(lambda (y x)
	     (let* ((r-sqr (+ (sqr x) (sqr y)))
		    (w-sqr (sqr w))
		    (tmp (/ (* 4 r-sqr) w-sqr)))
	       (* (- 1 tmp) (exp (- tmp)))))
	 :x-range '(-4.5 4.5))))
(magnitude (fft (paste laplacian-10 (make-image '(128 128))) :center t))

(let ((w 4))
  (setq laplacian-11
	(make-synthetic-image
	 '(11 11)
	 #'(lambda (y x)
	     (let* ((r-sqr (+ (sqr x) (sqr y)))
		    (w-sqr (sqr w))
		    (tmp (/ (* 4 r-sqr) w-sqr)))
	       (* (- 1 tmp) (exp (- tmp)))))
	 :x-range '(-5 5))))
(magnitude (fft (paste laplacian-11 (make-image '(128 128))) :center t))
|#

#|
;;; noise
(setq im1 (make-gaussian-noise '(64 64)))
(setq im2 (circular-shift im1 :x-shift 3 :y-shift -2))
(display
 (setq disparity (nishihara-correlation im1 im2
					:x-shift-range '(-5 5)
					:y-shift-range '(-5 5)))
 'vector-field :scale 4)

;;; trees
(my-compile-load "obvius/image-hacks")
(setq path1 (format nil "/home/heeger/images/trees/trees-0~a.pic" 32))
(setq path2 (format nil "/home/heeger/images/trees/trees-0~a.pic" 36))
(setq im1 (load-image-no-header path1 256 233
				:data-type '(unsigned-byte 8)
				:skip-bytes 0))
(setq im2 (load-image-no-header path2 256 233
				:data-type '(unsigned-byte 8)
				:skip-bytes 0))
(setq disparity (magnitude (nishihara-correlation im1 im2
						  :blur-level 1
						  :thresh nil
						  :x-shift-range '(0 16)
						  :y-shift-range '(0 0)
						  )))
(setp :pedestal 0 :scale 8)
(setp :pedestal 8 :scale 4)


;; Look at the correlation peak for transparent stereogram (need to
;; load anaglyphs.lisp):
(setq transparent-square-map
      (make-rectangles '(128 128)
		       '((128 128 64 64 -4 4))))
(setq dots (dot-pair-from-disparity-map transparent-square-map))
(setq stereopair (make-image-pair (list (coerce-to-float (frame 0 dots))
					(coerce-to-float (frame 1 dots)))))
(progn
  (setq laplacian-kernel *laplacian-11*)
  (setq edge-handler :dont-compute)
  (setq im1 (left-image stereopair))
  (setq im2 (right-image stereopair))
  (setq tmp (similar im1))
  (setq laplacian-filter (make-filter laplacian-kernel :edge-handler edge-handler))
  (setq dog1 (apply-filter laplacian-filter im1))
  (setq dog2 (apply-filter laplacian-filter im2))
  (setq thresh-dog1 (greater-than dog1 0))
  (setq thresh-dog2 (greater-than dog2 0))
  (setq sign-dog1 (add -1 (mul 2 (coerce-to-float thresh-dog1 :-> tmp) :-> tmp)))
  (setq sign-dog2 (add -1 (mul 2 (coerce-to-float thresh-dog2 :-> tmp) :-> tmp)))
  (setq correlation (compute-correlation-mat sign-dog1 sign-dog2
					     :x-shift-range '(-6 6)
					     :y-shift-range '(0 0)
					     :blur-kernel '(1)
					     :blur-level 0
					     :edge-handler edge-handler))
  (make-image (make-matrix (loop for i from 0 to 12
				 collect
				 (mean (aref (matrix correlation) 0 i))))))

|#
