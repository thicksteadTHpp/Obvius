
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code for making disparity maps

(defun make-rectangles (dims rectangles &key ->)
  "Each rectangle is a list of (y-dim x-dim y-center x-center value1 value2), where value2 is optional"
  (with-result ((result ->) (list :class 'image :dimensions dims))
    (loop for rectangle in rectangles
	  for rdims = (list (first rectangle) (second rectangle))
	  for top = (- (third rectangle) (floor (first rectangle) 2))
	  for left = (- (fourth rectangle) (floor (second rectangle) 2))
	  for value1 = (fifth rectangle)
	  for value2 = (sixth rectangle)
	  do
	  (with-local-viewables
	      ((tmp (if value2
			(make-randomized-patch rdims value1 value2)
			(fill! (make-image rdims) value1))))
	    (paste tmp result :y top :x left :-> result)))
    result))

(defun make-randomized-patch (dims value1 value2)
  (with-local-viewables
      ((tmp1 (make-random-dots dims :density 0.5))
       (tmp2 (coerce-to-float tmp1)))
    (mul (- value2 value1) tmp2 :-> tmp2)
    (add value1 tmp2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make random dot stereogram from disparity map

(defun dot-pair-from-disparity-map (dmap &key -> (dot-density 1/2))
  (with-result ((result ->) (list :class 'viewable-sequence
				  :size '(1 2)
				  :sub-viewable-spec
				  (list :class 'bit-image
					:dimensions (dimensions dmap))))
    ;; initialize left and right images to fill in potential holes
    (make-random-dots (dimensions dmap) :density dot-density :-> (frame 0 result))
    (make-random-dots (dimensions dmap) :density dot-density :-> (frame 1 result))

    (let* ((xdim-1 (- (x-dim dmap) 1))
	   (left-im-data (data (frame 0 result)))
	   (right-im-data (data (frame 1 result))))
      (with-local-viewables
	  ((dots (make-random-dots (dimensions dmap) :density dot-density)))
	;; fill in from dots to left and right images
	(loop-over-image-positions
	    ((val dots) (disparity dmap))
	    (y x)
	  (let* ((lx (- x (floor disparity 2)))
		 (rx (+ x (ceiling disparity 2))))
	    (when (< 0 lx xdim-1)
	      (setf (aref left-im-data y lx) val))
	    (when (< 0 rx xdim-1)
	      (setf (aref right-im-data y rx) val))))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Display pair is a viewable-sequence of bit-images
(defun display-anaglyph (pair)
  (let* ((red-val 1.0)
	 (im0 (copy (frame 0 pair)))
	 (im1 (copy (frame 1 pair)))
	 (im2 (mul im0 im1)))
    (display (make-viewable-sequence (list im0 im1 im2)
				     :display-type 'overlay))
    (setp :current-picture 0 :foreground 
	  (obv::find-true-color :red red-val :green 0.0 :blue 0.0))
    (setp :current-picture 1 :foreground
	  (obv::find-true-color :red 0.0 :green 1.0 :blue 0.0))
    (setp :current-picture 2 :foreground
	  (obv::find-true-color :red red-val :green 1.0 :blue 0.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(setf (obv::background (obv::current-screen)) :black)
(setq dmap (make-rectangles '(128 128) '((128 128 64 64 1)
					 (64 64 64 64 5))))

(setq stereopair (dot-pair-from-disparity-map dmap :dot-density 1/8))
(display-anaglyph stereopair)
|#

