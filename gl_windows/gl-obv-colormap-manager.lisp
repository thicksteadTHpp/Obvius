;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-colormap-manager.lisp
;;;  Author: Patrick C. Teo
;;;  Description: OBVIUS colormap manager since the GL colormap manager does
;;;               not afford enough control.
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)


;;;  We implement a colormap manager over GL's colormap
;;;  inorder to share colormap entries between windows.
;;;  This is used only for 8bit screens.  24bits screens
;;;  render in true color.

(def-simple-class GL-color ()
  ((red   :type integer :initform 0)      ;; 0..255
   (green :type integer :initform 0)      ;; 0..255
   (blue  :type integer :initform 0)))    ;; 0..255

(def-simple-class GL-colormap-cell (GL-color)
  ((status :type (member :reserved :free :used) :initform :free)
   (references :type integer :initform 0)                       ;; number of windows using this color
   (next-free :type (or (member nil) integer) :initform nil)))

(def-simple-class GL-colormap ()
  ((max-size :type integer)     				;; maximum size of colormap
   (size :type integer)         				;; current size of colormap usage
   (next-free :type (or (member nil) integer) :initform :free)  ;; initial index of the first free color
   (reserved :type cons)        				;; list of reserved color indices
   (red-depth :type integer)					;; number of bits per color for red
   (green-depth :type integer)					;; number of bits per color for green
   (blue-depth :type integer)					;; number of bits per color for blue
   colormap))							;; colormap array

(def-simple-class 8bit-GL-colormap (GL-colormap)
  ()
  (:default-initargs
      :max-size 256
      :red-depth 8
      :green-depth 8
      :blue-depth 8
      :reserved '(0 1 2 3 4 5 6 7)))


(defvar *foreign-R* (LCL:with-static-area (make-array '(1) :element-type '(signed-byte 16) :initial-element 0)))
(defvar *foreign-G* (LCL:with-static-area (make-array '(1) :element-type '(signed-byte 16) :initial-element 0)))
(defvar *foreign-B* (LCL:with-static-area (make-array '(1) :element-type '(signed-byte 16) :initial-element 0)))

(defmethod initialize-colormap ((colormap-class GL-colormap))
  (with-slots (max-size size next-free reserved colormap) colormap-class
    (setf size (length reserved))
    (setf colormap (make-array `(,max-size) :element-type 'GL-colormap-cell))
    (let ((next-free-set nil) (last-free nil))
    (loop for i from 0 below max-size
	  do
	  (if (member i reserved :test #'=)
	      (progn
		(GL:with-GL-lock (GL:getmcolor i *foreign-R* *foreign-G* *foreign-B*))
		(let ((color-R (aref *foreign-R* 0))
		      (color-G (aref *foreign-G* 0))
		      (color-B (aref *foreign-B* 0)))
		  (setf (aref colormap i)
			(make-instance 'GL-colormap-cell
				       :red color-R :green color-G :blue color-B :status :reserved :references 1))))
	      (progn
		(setf (aref colormap i) (make-instance 'GL-colormap-cell :status :free :references 0))
		(if (null next-free-set)
		    (setf next-free i
			  last-free i
			  next-free-set t)
		    (setf (next-free (aref colormap last-free)) i
			  last-free i))))))))


;;; creates a color instance and adds it to the screen's
;;; colormap.  returns the colormap index.
(defmethod make-color ((colormap-class GL-colormap)
		       &key
		       red green blue  ;; normalized colors values
		       (warn t))
  (with-slots (size max-size colormap) colormap-class
    (if (>= size max-size)
	(progn (when warn (warn "No more colormap entries")) nil)
	(with-slots (size next-free colormap red-depth green-depth blue-depth) colormap-class
	  (let*	((colormap-cell (aref colormap next-free))
		 (colormap-index next-free)
		 (red-color (round (* red (1- (expt 2 red-depth)))))
		 (green-color (round (* green (1- (expt 2 green-depth)))))
		 (blue-color (round (* blue (1- (expt 2 blue-depth))))))
	    (GL:with-GL-lock (GL:mapcolor colormap-index red-color green-color blue-color))
	    (setf (red colormap-cell)    red-color
		  (green colormap-cell)  green-color
		  (blue colormap-cell)   blue-color
		  (status colormap-cell) :used)
	    (setf (references colormap-cell) 1)
	    (setf next-free (next-free colormap-cell))
	    (setf (next-free colormap-cell) nil)
	    (incf size)
	    colormap-index)))))
  
;;; "frees" a colormap cell if it is not reserved
(defmethod free-colormap-cell ((colormap-class GL-colormap) cmindex &key (warn t))
  (with-slots (max-size colormap next-free size) colormap-class
    (if (or (< cmindex 0) (>= cmindex max-size))
	(error "Colormap index out of range")
	(if (not (eq (status (aref colormap cmindex)) :used))
	    (when warn (warn "Colormap cell either reserved or already free"))

	    ;;;
	    ;;; free colormap cell only when no other window is
	    ;;; referencing it.
	    ;;;
	    (when (zerop (decf (references (aref colormap cmindex))))
	      (setf (next-free (aref colormap cmindex)) next-free
		    (status (aref colormap cmindex)) :free)
	      (decf size)
	      (setf next-free cmindex))))))

;;; returns a colormap cell given its index
(defmethod find-colormap-cell ((colormap-class GL-colormap) cmindex)
  (with-slots (max-size colormap) colormap-class
    (if (or (< cmindex 0) (>= cmindex max-size))
	(error "Colormap index out of range")
	(if (eq (status (aref colormap cmindex)) :free)
	    (error "Colormap cell not allocated")
	    (aref colormap cmindex)))))


;;; finds a colormap cell that exactly matches the given RGB color
;;; returns nil otherwise.
;;; inefficient!!
(defmethod find-color ((colormap-class GL-colormap)
		       &key
		       red green blue)   ;; normalized color values
  (with-slots (max-size colormap red-depth green-depth blue-depth) colormap-class
    (let ((red-color (round (* red (1- (expt 2 red-depth)))))        ;; linear!
	  (green-color (round (* green (1- (expt 2 green-depth)))))
	  (blue-color (round (* blue (1- (expt 2 blue-depth))))))
      (loop for i from 0 below max-size
	    do
	    (unless (eq (status (aref colormap i)) :free)
	      (let* ((colormap-cell (aref colormap i)))
		(when (and (= (red colormap-cell) red-color)
			   (= (blue colormap-cell) blue-color)
			   (= (green colormap-cell) green-color))
		  (return i))))))))
		     

;;; makes a colormap cell only if there isn't already one that matches
(defmethod make-color-if-necessary ((colormap-class GL-colormap)
				    &key
				    red green blue  ;; normalized color values
				    (warn t))
  (let ((cmap-index (find-color colormap-class :red red :green green :blue blue)))
    (if (numberp cmap-index)
	(progn

	  ;;;
	  ;;; Increment the number of references to this colormap cell
	  ;;;
	  (incf (references (aref (colormap colormap-class) cmap-index)))
	  
	  cmap-index)
	
	(make-color colormap-class :red red :green green :blue blue :warn warn))))
	




;;;;
;;;; Debugging Utilities
;;;;

;;; prints the colormap either (:reserved :used :free)
(defmethod print-colormap ((colormap-class GL-colormap)
			   &key (which '(:reserved :used :free)))
  (with-slots (max-size colormap) colormap-class
    (loop for i from 0 below max-size do
	  (let ((colormap-cell (aref colormap i)))
	    (when (member (status colormap-cell) which :test #'eq)
	      (format t "Colormap cell [~d] <~A,~d>:  R=~d, G=~d, B=~d.~%"
		      i (status colormap-cell) (references colormap-cell)
		      (red colormap-cell) (green colormap-cell) (blue colormap-cell)))))))


(defmethod print-colormap-statistics ((colormap-class GL-colormap))
  (with-slots (max-size colormap (reserved-indices reserved)
			red-depth green-depth blue-depth) colormap-class
    (let* ((free 0) (reserved 0) (used 0))
      (loop for i from 0 below max-size do
	    (case (status (aref colormap i))
	      ((:reserved) (incf reserved))
	      ((:free) (incf free))
	      ((:used) (incf used))))
      
      (format t "Colormap with ~d R-bits, ~d G-bits and ~d B-bits.~%"
	      red-depth green-depth blue-depth)
      (format t "Reserved indices: ~A~%" reserved-indices) 
      (format t "Used: ~d  +  Reserved: ~d  +  Free: ~d  =  Total: ~d~%"
	      used reserved free max-size)

      (when (/= (+ used reserved free) max-size)
	(warn "Number of allocated, reserved and free cells do not sum to max-size")))))

  

(defmethod trace-free-colormap-cells ((colormap-class GL-colormap))
  (with-slots (next-free colormap) colormap-class
    (loop with temp-next-free = next-free 
	  until (null temp-next-free)
	  do
	  (format t "Free colormap-cell [~d]~%" temp-next-free)
	  (setf temp-next-free (next-free (aref colormap temp-next-free))))))
