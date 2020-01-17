;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: lucid-image-loops.lisp
;;;  Author:  Eero Simoncelli
;;;  Description: Macros for efficient image accessing and looping.
;;;  Creation Date: Summer, 1988.
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;        (C) 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)

(export '(with-image-pixels loop-over-image-pixels loop-over-image-positions))

;;; Allows reasonably efficient point operations on any number of images 
;;; (ie - loop over images, applying function at one pixel location at a time)
;;; The calling form resembles the form of "let":
;;;    (loop-over-image-pixels ((val1 im1)
;;;                             (val2 im2)
;;;                                 .
;;;                                 .
;;;                             (valn imn))
;;;                            &rest body)
;;; The body should be one or more expressions referring to the image
;;; pixel values by their bound symbols (val1, etc in the example above).
;;; The macro does a size check on the image arguments.
;;; IMPORTANT NOTE: Any setf operation on image values should explicitly 
;;; coerce the value to a float.
;;; As an example:
;;;
;;;(defun image-minimum (image)
;;;  (let ((min-val (iref im 0 0)))
;;;    (declare (float min-val))
;;;    (loop-over-image-pixels ((val image))
;;;      (when (> min-val val) (setq min-val val)))
;;;    min-val))
;;;
;;; NOTE: this could be made more efficient by setting a loop variable
;;; for the images whose values will be accessed within the loop: each
;;; currently does an aref!  This requires more parsing, however, since
;;; the setf clauses must still use aref (as opposed to the loop variable).
(defmacro loop-over-image-pixels (image-list . body)
  (let ((dims (gensym)) (sz (gensym)) (count (gensym))
	(vect-list (loop for item in image-list collect (gensym)))
	(type-list (loop for item in image-list collect (caddr item)))
	(offset-list (loop for item in image-list collect (gensym)))
	(pos-list (loop for item in image-list collect (gensym))))
    `(let* ((,dims (dimensions (check-size ,@(loop for item in image-list
						   for im = (cadr item)
						   collect im))))
	    (,sz (apply #'* ,dims))
	    ,@(loop for item in image-list
		    for im = (cadr item)
		    for vect in vect-list
		    collect `(,vect (parent-array (data ,im))))
	    ,@(loop for item in image-list
		    for im = (cadr item)
		    for offset in offset-list
		    collect `(,offset (displaced-start (data ,im)))))
       (declare (fixnum ,sz ,@(loop for offset in offset-list  collect offset))
		,@(loop for vect in vect-list 
			for type in type-list
			collect `(type (simple-array ,(or type 'single-float) (*)) ,vect)))
       (symbol-macrolet ,(loop for item in image-list
			       for var = (car item)
			       for vect in vect-list
			       for pos in pos-list
			       collect `(,var (aref ,vect ,pos)))
			(loop for ,count from 0 below ,sz
			      ,@(loop for pos in pos-list
				      for offset in offset-list
				      nconc `(for ,pos from ,offset))
			      do
			      ,@body)))))

;;; Another looping macro, identical to loop-over-image-pixels, except that 
;;; the x and y positions with the image are accessible within the body.
;;; As an example:
;;;
;;; (defun make-x-ramp (size)
;;;   (let ((im (make-image size :-> 'ramp)))
;;;     (loop-over-image-positions ((val im)) 
;;;                                (y x) 
;;;	                           (setf val (float x))) 
;;;       im))
;;;
(defmacro loop-over-image-positions (image-list (ypos xpos) . body)
  (let ((dims (gensym)) (xdim (gensym)) (ydim (gensym)) (sz (gensym))
	(type-list (loop for item in image-list collect (caddr item)))
	(vect-list (loop for item in image-list collect (gensym)))
	(pos-list (loop for item in image-list collect (gensym))))
    `(let* ((,dims (dimensions (check-size ,@(loop for item in image-list
						   for im = (cadr item)
						   collect im))))
	    (,xdim (cadr ,dims))   
	    (,ydim (car ,dims))
	    (,sz (* ,xdim ,ydim)) 
	    ,@(loop for item in image-list
		    for im = (cadr item)
		    for vect in vect-list
		    collect `(,vect (parent-array (data ,im))))
	    ,@(loop for item in image-list
		    for im = (cadr item)
		    for pos in pos-list
		    collect `(,pos (displaced-start (data ,im)))))
       (declare (fixnum ,sz ,xdim ,ydim
		        ,@(loop for pos in pos-list  collect pos))
		,@(loop for vect in vect-list 
			for type in type-list
			collect `(type (simple-array ,(or type 'single-float) (*)) ,vect)))
       (symbol-macrolet ,(loop for item in image-list
			       for var = (car item)
			       for vect in vect-list
			       for pos in pos-list
			       collect `(,var (aref ,vect ,pos)))
			(loop for ,ypos from 0 below ,ydim do
			      (loop for ,xpos from 0 below ,xdim do
				    ,@body 
				    ,@(loop for pos in pos-list
					    collect `(incf ,pos))))))))

#|
;;; Efficient way to access images.  This replaces calls to iref with calls to 
;;; aref.  Needs to be more robust! (ie should check whether images are actually
;;; being altered...
(defmacro with-image-pixels (image-list . body)
  (let ((vect-list (loop for item in image-list collect (gensym)))
	(type-list (loop for item in image-list collect (caddr item))))	
    `(let* ,(loop for im in image-list
		   for vect in vect-list
		   collect `(,vect (data ,im)))
	   (declare ,@(loop for vect in vect-list
			    for type in type-list
			    collect `(type (array ,(or type 'single-float) (* *)) ,vect)))
	   (symbol-macrolet ((iref aref)
			     ,@(loop for im in image-list
				     for vect in vect-list
				     collect `(,im ,vect)))
			    ,@body
			    ,@(loop for im in image-list
				    collect `(set-not-current ,im))))))

|#







;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
