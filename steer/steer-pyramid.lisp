;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: steer-pyramid.lisp
;;;  Author: Heeger, Freeman, Simoncelli
;;;  Description: pyramid-steerable-filter image-transforms.
;;;            Program which came up with the filter taps:  
;;;                      ~freeman/lisp/steer/pyramid-alias.lisp
;;;  Creation Date: summer '89
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History:

;;; Sept. 22, 1989 the non-separable pyramid tiling steering filters
;;; steer backwards from the way they should.  ie, for them, positive
;;; angle = clockwise.  For all the other filters, including the
;;; separable versions of the pyramid tiling steering filters,
;;; positive angle = counterclockwise.  Eventually, I'll fix the
;;; non-separable ones.  wtf.  See wtf labbook p. 52.

;;; April 19, 1990 freeman changed edge handling from nil (wrap
;;; around) to "reflect1".

;;; June 20, 1990 Freeman modified to make an end-filter to stop the
;;; recursion.  So, I added an end-filter to the definition of
;;; steerable-pyramid, and changed even-collapse, odd-collapse, and
;;; added *default-end-steerable-pyramid-filter*.  So the old default
;;; filters, stored as bpeven0 ...  are still there on disk, but the
;;; new filters are read in from bpevena0 ...

;;; july 3, 1991 modified this to read-in the newest version filters,
;;; bpevenb0...  See /u/freeman/lisp/steer/pyramid-alias.lisp for
;;; general comments about the steerable pyramid; search for "july 3".
;;; The x-y separable filters in this file are out of date.

;;; July 9, 1991 changed the edge-handler from reflect1 back to nil.
;;; That significantly reduced the reconstruction errors; see
;;; ~/lisp/steer/pyramid-alias.lisp.
;;;  OLDEST FILTERS, designed summer, 1989:  bpeven0, bpeven1, ...
;;;  NEWER FILTERS, designed summer, 1990:  bpevena0, bpevena1, ...
;;;  NEWEST FILTERS, designed summer, 1991:  bpevenb0, bpevenb1, ...
;;;  Still newest filters, oriented filters: July 1991: bpevenc0, ...
;;; (oriented filters only.  the two low-pass filters are the same as
;;; version b).
;;; July 20, 1991.  Use even filters which have NO dc response, even
;;; though they give slightly higher maximum freq error.  Therefore,
;;; use bpevenb0,1,2,3,4 with bpoddc0,1,2,3.

;;; Sep. 17, 1992    Simoncelli ported to obvius-2.2

;;; Mar 17, 1993 The two knuckleheads totally rewrote this code (sorry
;;; Bill) and ported to obvius-3.0

;;; Mar 9, 1994: EPS/DH: Finished writing/simplifying build/collapse
;;; code.  Wrote automatic filter design code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; References:  Freeman & Adelson, IEEE PAMI, 1991
;;;              Simoncelli, Freeman, Adelson, Heeger, IEEE IT, 1992.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '(steerable-pyramid make-steerable-pyramid
	  hi-filter hi-band
	  terminal-low-filter terminal-low-band
	  quadrature-steerable-pyramid quadrature-make-steerable-pyramid
	  even-pyramid odd-pyramid))

(obv-require :matrix)
(obv-require :pyramid)
(obv-require "steer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STEERABLE PYRAMID CLASS

;;; *** Should move this to viewable-classes.lisp, along with exports
;;; of slots.
(def-simple-class steerable-pyramid (pyramid)
  ((terminal-low-filter :initform nil)
   (terminal-low-band :initform nil)
   (hi-filter :initform nil)
   (hi-band :initform nil))
  (:default-initargs :display-type nil))

(defmethod inferiors-of ((pyr steerable-pyramid))
  (remove nil (append (list (original pyr) (low-band pyr)
			    (terminal-low-band pyr) (hi-band pyr))
		      (levels pyr))))

(defmacro steerable-pyramid-p (obj)
  `(typep ,obj 'steerable-pyramid))

;;; note: no display type for these guys for now, look at the
;;; sub-objects to look at them.  if :level is non-nil, initially
;;; builds pyramid to that level
(defun make-steerable-pyramid
    (image &rest initargs &key (level nil)
	   (low-filter (make-filter steerpyr-l1 :step-vector '(2 2)))
	   (hi-filter (make-filter steerpyr-h0))
	   (terminal-low-filter (make-filter steerpyr-l0))
	   (filters (map 'list #'make-filter steerpyr-b))
	   display-type name ->)
  (when -> (setf (getf initargs :name) ->))
  (remf initargs :level)
  (remf initargs :filters)
  (remf initargs :low-filter)
  (remf initargs :hi-filter)
  (remf initargs :terminal-low-filter)
  (with-result ((result nil)
		`(:class steerable-pyramid
		  :low-band nil
		  :hi-band nil
		  :original ,image
		  :forward-filters ,filters :inverse-filters ,filters
		  :forward-low-filter ,low-filter :inverse-low-filter ,low-filter
		  :terminal-low-filter ,terminal-low-filter
		  :hi-filter ,hi-filter
		  ,@initargs)
		'apply 'make-steerable-pyramid image initargs)
      (when level (build result level))
      result))

(defmethod set-result ((name t) (model steerable-pyramid))
  (check-type name viewable-name)
  (make-instance (class-of model)
		 :name name
		 :display-type (display-type model)
		 :forward-filters (forward-filters model)
		 :forward-low-filter (forward-low-filter model)
		 :inverse-filters (inverse-filters model)
		 :inverse-low-filter (inverse-low-filter model)
		 :hi-filter (hi-filter model)
		 :terminal-low-filter (terminal-low-filter model)
		 :original (original model)
		 :low-band (similar (low-band model))
		 :hi-band (similar (hi-band model))
		 :terminal-low-band (similar (terminal-low-band model))
		 :levels (mapcar #'similar (levels model))))

(defmethod initialize-instance ((pyr steerable-pyramid) &key &allow-other-keys)
  (call-next-method)
  (when (hi-band pyr) (pushnew pyr (superiors-of (hi-band pyr))))
  (when (terminal-low-band pyr) (pushnew pyr (superiors-of (terminal-low-band pyr)))))

(defmethod terminal-low-band ((pyr steerable-pyramid))
  (slot-value pyr 'terminal-low-band))

;;; *** Code for the original system diagram (as in the paper) is
;;; marked by ;;>.  The new system diagram works better (at least for
;;; the filters Eero was able to design).

(defmethod build ((pyr steerable-pyramid) level)
  (when (and (<= (length (levels pyr)) level) (slot-value pyr 'terminal-low-band))
    (let ((tmp (slot-value pyr 'terminal-low-band)))
      (setf (slot-value pyr 'terminal-low-band) nil)
      (setf (superiors-of tmp) nil)
      (destroy tmp)))
  (unless (low-band pyr)
    (setf (low-band pyr)
	  (apply-filter (terminal-low-filter pyr) (original pyr)))
    (pushnew pyr (superiors-of (low-band pyr))))
  (unless (hi-band pyr)
    (setf (hi-band pyr)
	  (apply-filter (hi-filter pyr) (original pyr)))
    (pushnew pyr (superiors-of (hi-band pyr))))
  (call-next-method)
  ;;> (setf (terminal-low-band pyr)
	;;> (apply-filter (terminal-low-filter pyr) (low-band pyr)))
  ;;> (pushnew pyr (superiors-of (terminal-low-band pyr)))
  pyr)

(defmethod build-level ((pyr steerable-pyramid) low)
  (let ((new-low (apply-filter (forward-low-filter pyr) low))
	(steerable-basis
	 (make-steerable-basis low :filter-list (forward-filters pyr))))
    (values new-low steerable-basis)))

(defmethod collapse ((pyr steerable-pyramid))
  (let ((res (loop with start-lev = (1- (length (levels pyr)))
		   for lev from start-lev downto 0
		   for low = (collapsible-low-band pyr)
		   then res
		   for res = (similar (access-band pyr :level lev :band 0))
		   do
		   (expand-filter (inverse-low-filter pyr) low :-> res)
		   (destroy low)
		   (loop for f in (inverse-filters pyr)
			 for n from 0
			 for b = (access-band pyr :level lev :band n)
			 do 	
			 (expand-filter f b :-> res :zero nil))
		   finally (return res))))
    ;;> (unless res (setq res (copy (terminal-low-band pyr))))
    (unless res (setq res (copy (low-band pyr))))
    (with-local-viewables
       ((tmp (expand-filter (terminal-low-filter pyr) res :zero t)))
     (expand-filter (hi-filter pyr) (hi-band pyr) :zero nil :-> tmp)
     (copy tmp :-> res))
    res))

#|
;;>
(defmethod collapsible-low-band ((pyr steerable-pyramid))
  (expand-filter (terminal-low-filter pyr) (terminal-low-band pyr)))
|#

(defmethod collapsible-low-band ((pyr steerable-pyramid))
  (copy (low-band pyr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FILTER DESIGN: simple least squares solutions.  These could be made
;;;; much better, but it might not be worth it...

#|
;;;; SIMPLE STEERABLE PYRAMID FILTER DESIGN 

;;; ORIGINAL steerable pyramid, as depicted in IEEE-IT article, but
;;; with L0 used as a pre-filter also:

;; ---- H0 ------------------------------------------------ H0 ----
;;   |                                                    |
;;   -- L0 --- Bi ---------------------------- Bi --- L0 --
;;          |                                      |
;;          -- L1 -- 2v --L0 --o-- L0 -- 2^ -- L1 --

(setq *dims* '(32 32))

;;; Bandpass filters:
(set 'b-filts
      (loop with num-bands = 4
	    with p = (- num-bands 1)
	    with r-fn = (log-raised-cos-fn2 :ctr-freq (/ pi 2))
	    for band from 0 below num-bands
	    for theta = 0.0 then (+ theta (/ pi num-bands))
	    for c = (cos theta) for s = (sin theta)
	    for dft = (make-synthetic-image
		       *dims*
		       #'(lambda (y x)
			   (let* ((r (sqrt (+ (sqr y) (sqr x))))
				  (a (/ (+ (* c x) (* s y)) r)))
			     (* (funcall r-fn r)
				(x^n a p)
				(sqrt 4/5))))
		       :x-range (pi-range (cadr *dims*))
		       :y-range (pi-range (car *dims*)))
	    do
	    ;;(abs. dft :-> dft)  ;symmetric filters
	    (display dft)
	    collect (design-filter dft '(9 9) :weight 0.5 )))

;;; Lowpass:
(set 'l1-filt
      (with-local-viewables
	  ((ctr (/ pi 4))
	   (r-fn (log-raised-cos-fn2 :ctr-freq ctr))
	   (dft (make-synthetic-image
		 *dims*
		 #'(lambda ( y x)
		     (let* ((r (sqrt (+ (sqr y) (sqr x)))))
		       (* 2 (if (> r ctr) (funcall r-fn r) 1.0))))
		 :x-range (pi-range (cadr *dims*))
		 :y-range (pi-range (car *dims*)))))
	(display dft)
	(design-filter dft '(13 13) :weight 1.0 :dc-weight 2.0  :step-vector '(2 2))))

;;; Terminal, and initial lowpass.
(set 'l0-filt
      (with-local-viewables
	  ((ctr (/ pi 2))
	   (r-fn (log-raised-cos-fn2 :ctr-freq ctr))
	   (dft (make-synthetic-image
		 *dims*
		 #'(lambda ( y x)
		     (let* ((r (sqrt (+ (sqr y) (sqr x)))))
		       (if (> r ctr) (funcall r-fn r) 1.0)))
		 :x-range (pi-range (cadr *dims*))
		 :y-range (pi-range (car *dims*)))))
	(display dft)
	(design-filter dft '(9 9) :weight 1.0 :dc-weight 2.0)))

;;; Highpass:
(set 'h0-filt
      (with-local-viewables
	  ((ctr pi)
	   (r-fn (log-raised-cos-fn2 :ctr-freq ctr))
	   (dft (make-synthetic-image
		 *dims*
		 #'(lambda ( y x)
		     (let* ((r (sqrt (+ (sqr y) (sqr x)))))
		       (if (> r ctr) 1.0 (funcall r-fn r))))
		 :x-range (pi-range (cadr *dims*))
		 :y-range (pi-range (car *dims*)))))
	(display dft)
	(design-filter dft '(9 9) :weight 0.3)))

;;; TESTING:
(let ((dims '(64 64)))
  (setq b-specs (mapcar #'(lambda (f) (power-spectrum f :dimensions dims))
			b-filts)
	h0-spec (power-spectrum h0-filt :dimensions dims)
	l0-spec (power-spectrum l0-filt :dimensions dims)
	l1-spec (power-spectrum l1-filt :dimensions dims))
  (mapcar #'(lambda (im) (*. im (* (car dims) (cadr dims)) :-> im)) 
	  (append (list h0-spec l0-spec l1-spec) b-specs)))

;;; Initial decomposition: should be flat:
(+. h0-spec (sqr. l0-spec))

;;; One level:
(+. h0-spec
    (*. l0-spec
	(+.  (nth 0 b-specs)
	     (nth 1 b-specs)
	     (nth 2 b-specs)
	     (nth 3 b-specs)
	     (*. (/. l1-spec 4)
		 (paste (subsample l0-spec :step-vector '(2 2))
			(make-image (dimensions l0-spec))
			:offset (mapcar #'(lambda (x) (/ x 4)) *dims*))))))

(let ((im (load-image "~/obvius-3.0/images/einstein")))
  (make-steerable-pyramid
   im
   :filters b-filts
   :low-filter l1-filt
   :terminal-low-filter l0-filt
   :hi-filter h0-filt))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New Simpler version (hi-lo splitting): does not require the
;;; terminal l0 filter.  Bandpass filter (Bi) design is now less
;;; constrained, since we don't care what their high-frequency
;;; response is like.
 
;; ---- H0 ------------------------------- H0 ----
;;   |                                         |
;;   -- L0 --- Bi ----------------- Bi --- L0 --
;;          |                           |
;;          -- L1 -- 2v --o-- 2^ -- L1 --

;;; Constraints:
;;; [1]  |L0|^2 + |H0|^2 = 1
;;; [2]  L1 goes to zero at pi/2 (subsample without aliasing)
;;; [3] (|L1|^2 + Sum |Bi|^2) |L0|^2 = |L0|^2
;;; I.e., L1 plus Bi's should be 1, but there's lots of room for slop
;;; at high frequencies since L0 gets very small as you approach pi.

;;; Design filters at compile-time, if they are not already there.  To
;;; design new filters with more orientation bands, change the value
;;; of num-bands and you may need to change bfilt-dims.
(eval-when (compile)
  (unless (and (boundp 'steerpyr-h0) (arrayp steerpyr-h0))
    (format t "~%;;; DESIGNING filters for steerable pyramid ...~%")
    (obv-require "llse-filter-design")
    (let ((*dims* '(32 32))
	  (num-bands 4)
	  (bfilt-dims '(9 9)))
      (setq steerpyr-b
	    (make-array
	     num-bands
	     :initial-contents
	     (loop with p = (- num-bands 1)
		   with a-const = (sqrt (/ (* (obv::x^n 2 (* 2 p)) (sqr (factorial p)))
					   (* (factorial (* 2 p)) (1+ p))))
		   with ctr = (/ pi 2)
		   with fn = (log-raised-cos-fn :ctr-freq ctr)
		   with col  = (make-image (list (car *dims*) 1))
		   with row  = (make-image (list 1 (cadr *dims*)))
		   with wt = (make-synthetic-image ;same as all-lfilt
			      *dims*
			      #'(lambda ( y x)
				  (let* ((r (sqrt (+ (sqr y) (sqr x)))))
				    (if (> r ctr) (funcall fn r) 1.0))) ; **magic
			      :x-range (pi-range (cadr *dims*))
			      :y-range (pi-range (car *dims*))
			      :-> (format nil "weighting"))
		   ;; initially (display wt)
		   for band from 0 below num-bands
		   for theta = 0.0 then (+ theta (/ pi num-bands))
		   for c = (cos theta)  for s = (sin theta)
		   for dft = (make-synthetic-image
			      *dims*
			      #'(lambda (y x)
				  (let* ((r (sqrt (+ (sqr y) (sqr x))))
					 (a (/ (+ (* c x) (* s y)) r)))
				    (* (if (> r ctr) 1.0 (funcall fn r))
				       (x^n a p)
				       a-const)))
			      :x-range (pi-range (cadr *dims*))
			      :y-range (pi-range (car *dims*)))
		   do
		   (paste row dft :-> dft) (paste col dft :-> dft)
		   ;; (display dft)
		   collect (data (design-filter dft bfilt-dims :weight wt)))))
      (setq steerpyr-l1 
	    (with-local-viewables
		((ctr (/ pi 4))
		 (fn (log-raised-cos-fn :ctr-freq ctr))
		 (dft (make-synthetic-image
		       *dims*
		       #'(lambda ( y x)
			   (let* ((r (sqrt (+ (sqr y) (sqr x)))))
			     (* (sqrt num-bands) (if (> r ctr) (funcall fn r) 1.0))))
		       :x-range (pi-range (cadr *dims*))
		       :y-range (pi-range (car *dims*)))))
	      ;; (display dft)
	      (data (design-filter dft '(17 17) :weight 1.0 :dc-weight 2.0
				   :step-vector '(2 2)))))
      (setq steerpyr-l0
	    (with-local-viewables
		((ctr (/ pi 2))
		 (fn (log-raised-cos-fn :ctr-freq ctr))
		 (dft (make-synthetic-image
		       *dims*
		       #'(lambda ( y x)
			   (let* ((r (sqrt (+ (sqr y) (sqr x)))))
			     (if (> r ctr) (funcall fn r) 1.0)))
		       :x-range (pi-range (cadr *dims*))
		       :y-range (pi-range (car *dims*)))))
	      ;; (display dft)
	      (data (design-filter dft '(9 9) :weight 1.0 :dc-weight 2.0))))
      (setq steerpyr-h0
	    (with-local-viewables
		((ctr pi)
		 (fn (log-raised-cos-fn :ctr-freq ctr))
		 (dft (make-synthetic-image
		       *dims*
		       #'(lambda ( y x)
			   (let* ((r (sqrt (+ (sqr y) (sqr x)))))
			     (if (> r ctr) 1.0 (funcall fn r))))
		       :x-range (pi-range (cadr *dims*))
		       :y-range (pi-range (car *dims*)))))
	      ;; (display dft)
	      (data (design-filter dft '(9 9) :weight 0.3)))))))

;;; Imbed a readable version of the filters in the COMPILED file:
(defconstant steerpyr-b #.steerpyr-b)
(defconstant steerpyr-l1 #.steerpyr-l1)
(defconstant steerpyr-l0 #.steerpyr-l0)
(defconstant steerpyr-h0 #.steerpyr-h0)

#| 
;;; TEST FILTERS:
(setq b-filts (map 'list #'make-filter steerpyr-b)
      l1-filt (make-filter steerpyr-l1 :step-vector '(2 2))
      h0-filt (make-filter steerpyr-h0)
      l0-filt (make-filter steerpyr-l0)
      junk nil)

(let ((dims '(64 64)))
  (setq b-specs (mapcar #'(lambda (f) (power-spectrum f :dimensions dims))
			b-filts)
	h0-spec (power-spectrum h0-filt :dimensions dims)
	l0-spec (power-spectrum l0-filt :dimensions dims)
	l1-spec (power-spectrum l1-filt :dimensions dims))
  (mapcar #'(lambda (im) (*. im (* (car dims) (cadr dims)) :-> im)) 
	  (append (list h0-spec l0-spec l1-spec) b-specs)))

(+. l0-spec h0-spec)

(+. h0-spec
    (*. l0-spec
	(+. (nth 0 b-specs) 
	    (nth 1 b-specs)
	    (nth 2 b-specs)
	    (nth 3 b-specs)
	    (/. l1-spec 4)
	    )))
|#

#|  ;;; TEST PYRAMID:
(obv-require :steer)			;loads this file & steer.lisp
(setq im (make-zone-plate '(128 128)))
(setq im (load-image "~/obvius/images/einstein"))
(setq pyr (make-steerable-pyramid im))
(build pyr 0)
(build pyr 1)
(build pyr 2)
(describe pyr)
(setq res (collapse pyr))
(mean-square-error res im)

(display (access pyr 1) 'flipbook)


(+. (square (steer (access pyr 0) 0.0))
    (square (steer (access pyr 0) (* 1/4 pi)))
    (square (steer (access pyr 0) (* 1/2 pi)))
    (square (steer (access pyr 0) (* 3/4 pi))))
(+. (square (steer (access pyr 1) 0.0))
    (square (steer (access pyr 1) (* 1/4 pi)))
    (square (steer (access pyr 1) (* 1/2 pi)))
    (square (steer (access pyr 1) (* 3/4 pi))))

(make-image-sequence
 (loop for i from 0 below 16
       for theta = (* i (/ 2-pi 16))
       collect (steer (access pyr 1) theta))
 :display-type 'flipbook)
|#
