;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: viewable-sequence.lisp
;;;  Author: heeger
;;;  Description: (temporal) sequences of viewables
;;;  Creation Date: 8/90
;;;  Major Modification: 11/91 (sequences inherit from viewable-matrices)
;;; ----------------------------------------------------------------
;;; This file is part of the Object-Oriented Picture System (OBVIUS),
;;; (C) Vision Science Group,  Media Laboratory,  
;;; Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)
(export '(make-viewable-sequence 
          viewable-sequence-p viewable-list
	  dimensions x-dim y-dim
	  sequence-length length. frame append-sequence sub-sequence
	  map. reduce.))

;;; Remember that much of the code in this file has to work for
;;; image-sequences, image-pairs, and complex-images also...

;;; Some of the imops work only on image-sequences.  Pasteup pictures
;;; are defined for image-sequences (and image-matrices), but not for
;;; general viewable-sequences.  Image-pairs inherit from
;;; image-sequences.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod viewable-list ((seq viewable-sequence))
  (listify (data seq)))

;;; *** Bug between this and flipbooks.  When you change the length of
;;; the viewable-list, it doesn't update the length of picture-list.
;;; *** Also kludge here to allow (setf (viewable-list seq) nil).  We
;;; should eventually change it back (old code listed below), but this
;;; kludge is needed for backward compatibility (e.g., for Eero's
;;; flow-utilities).
(defmethod (setf viewable-list) ((vbl-list list) (seq viewable-sequence))
  (let ((old-vbl-list (viewable-list seq)))
    (if vbl-list
	(progn
	  (unless (and (listp vbl-list)
		       (every #'(lambda (x) (viewable-p x)) vbl-list))
	    (error "Bad vbl-list ~a: must be a list of viewables" vbl-list))
	  (apply 'check-size vbl-list)
	  (setf (data seq) (make-array (list 1 (length vbl-list))
				       :initial-contents (list vbl-list)))
	  (loop for vbl in vbl-list do (pushnew seq (superiors-of vbl))))
	(setf (data seq) nil))
    (mapc #'(lambda (inf) (notify-of-superior-destruction inf seq)) old-vbl-list)
    (dolist (pic (pictures-of seq)) (set-not-current pic)))
  seq)

(defmethod print-object ((seq viewable-sequence) stream)
  (format stream "#<(~A of ~A) " (object-class-name seq)
	  (object-class-name (car (viewable-list seq))))
  (format stream "~S " (name seq))
  (format stream "(~A)>" (sequence-length seq)))

(defmacro viewable-sequence-p (obj)
  `(typep ,obj 'viewable-sequence))

;;; Dimensions, x-dim, and y-dim inherit from viewable-matrices.

(defmethod z-dim ((seq viewable-sequence))
  (sequence-length seq))

(defmethod sequence-length ((seq viewable-sequence))
  (list-length (viewable-list seq)))

(defmacro length. (seq)
  `(sequence-length ,seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make-Viewable-Sequence

;;; Vbl-list may be a list of vbls or nil.  If nil, must specify
;;; :length and :sub-viewable-spec.

(defun make-viewable-sequence (vbl-list &rest initargs
					&key length sub-viewable-spec display-type name ->)
  (declare (ignore length sub-viewable-spec display-type name))
  (when -> (setf (getf initargs :name) ->))
  (unless (or (null vbl-list)
	      (and (listp vbl-list)
		   (every #'(lambda (x) (viewable-p x)) vbl-list)))
    (error "Bad vbl-list ~a: must be nil or a list of viewables" vbl-list))
  (with-result ((result nil)
		`(:class viewable-sequence
		  :viewable-list ,vbl-list
		  ,@initargs)
		'apply 'make-viewable-sequence vbl-list initargs)
    result))

;;; Mostly relies on inherted initialize-instance (defined for
;;; viewable-matrix).  Handle :length converting it to :size.  Handle
;;; :viewable-list keyword by converting it to :data.  Handle
;;; :dimensions keyword by converting it to :sub-viewable-spec.  Also,
;;; handle dimensions = (z y x) by converting it to length and
;;; dimensions.
(defmethod initialize-instance ((seq viewable-sequence) &rest initargs
				&key length dimensions viewable-list
				&allow-other-keys)
  (when (and dimensions (= (length dimensions) 3))
    (setq length (car dimensions))
    (setq dimensions (cdr dimensions)))
  (when length
    (setf (getf initargs :size) (list 1 length))
    (remf initargs :length))
  (when viewable-list
    (setf (getf initargs :size) (list 1 (length viewable-list)))
    (setf (getf initargs :data) (make-array (getf initargs :size)
					    :initial-contents (list viewable-list)))
    (remf initargs :viewable-list))
  (when dimensions
    (setf (getf initargs :sub-viewable-spec) 
          (list :class 'image :dimensions dimensions))
    (remf initargs :dimensions))
  (apply #'call-next-method seq initargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set-result for Viewable-sequences.

;;; When calling with-result with a model-plist, the programmer must
;;; provide either (1) :viewable-list or (2) :length and
;;; :sub-viewable-spec.  These specify what kind of sub-viewables are
;;; to be created.  The :sub-viewable-spec is like a model-plist for
;;; the sub-viewable.

;;; Set-result (name,viewable-sequence) inherits from viewable-matrix.

;;; If user passes :length in model-plist, then check that result is
;;; right length.  If user passes a :viewable-list, call set-result on
;;; each sub-viewable to make sure they are compatible.  If user
;;; passes a :sub-viewable-spec, also call set-result... (the
;;; :sub-viewable-spec should look like a model-plist for the
;;; sub-viewable).
(defmethod set-result ((res viewable-sequence) (model-plist list))
  (when (and (getf model-plist :length)
	     (not (equal (getf model-plist :length)
			 (sequence-length res))))
    (error "Result sequence is wrong length: ~A" res))
  ;; Check that result sub-viewables are compatible with viewable-list/sub-viewable-spec.
  (cond ((getf model-plist :viewable-list)
	 (loop for model-vbl in (getf model-plist :viewable-list)
	       for res-vbl in (viewable-list res)
	       do (set-result res-vbl model-vbl)))
	((getf model-plist :sub-viewable-spec)
	 (loop with sub-model = (getf model-plist :sub-viewable-spec)
	       for res-vbl in (viewable-list res)
	       do (set-result res-vbl sub-model))))
  (call-next-method))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; frame, sub-seq, append-seq ops

(defmethod frame ((n number) (seq viewable-sequence) &key ((:-> res)))
  (unless (<= 0 n (- (sequence-length seq) 1))
    (error "Frame number out of bounds: ~A" n))
  (let ((sub-viewable (aref (data seq) 0 n)))
    (cond ((viewable-p res)
	   (copy sub-viewable :-> res))
	  ((null res) sub-viewable)
	  ((typep res 'viewable-name)	;string, symbol, or nil
	   (set-name sub-viewable res)
	   sub-viewable)
	  (t (error "bad result argument")))))

(defmethod frame ((seq viewable-sequence) (n number) &key ->)
  (frame n seq :-> ->))

;;; If no result arg is passed, this just appends the lists and hands
;;; back a (shared list structure) viewable-sequence.  If arg is
;;; passed, the sub-viewables are copied into the result.
(defmethod append-sequence ((seq1 viewable-sequence) (seq2 viewable-sequence) &key ->)
  (let ((appended-seq (append (viewable-list seq1) (viewable-list seq2))))
    (cond ((viewable-p ->)
	   (unless (eq (length appended-seq) (length. ->))
	     (error "result sequence has incorrect length"))
	   (mapcar #'(lambda (im res) (copy im :-> res))
		   appended-seq (viewable-list ->))
	   ->)
	  ((typep -> 'viewable-name)
	   (make-instance (class-of seq1)
			  :viewable-list appended-seq
			  :display-type (display-type seq1) :name ->))
	  (t (error "bad result argument")))))

;;; If no result arg is passed, just pass back the sublist (inside of
;;; a viewable-sequence).  Otherwise copy it.
(defmethod sub-sequence ((seq viewable-sequence) start-frame &optional end-frame &key ->)
  (let ((sub-seq (subseq (viewable-list seq) start-frame end-frame)))
    (cond ((viewable-p ->)
	   (unless (eq (length sub-seq) (length. ->))
	     (error "result sequence has incorrect length"))
	   (mapcar #'(lambda (im res) (copy im :-> res))
		   sub-seq (viewable-list ->))
	   ->)
	  ((typep -> 'viewable-name)
	   (make-instance (class-of seq)
			  :viewable-list sub-seq
			  :display-type (display-type seq) :name ->))
	  (t (error "bad result argument")))))

(defmethod reduce. ((func function) (seq viewable-sequence)
		    &rest keys &key start end from-end initial-value ->)
  (declare (ignore start end from-end initial-value))
  (remf keys :->)
  (with-result ((res ->)
		(aref (data seq) 0 0)
		'apply 'reduce. seq keys)
    (let ((vect (vectorize (data seq))))
      (apply #'reduce
	     #'(lambda (v1 v2) (funcall func v1 v2 :-> res))
	     vect
	     keys))
    res))

(defmethod reduce. ((seq viewable-sequence) (func function) &rest keys)
  (apply 'reduce. func seq keys))

;;; new function, modeled on common lisp map.  rgs can be more
;;; sequences, and a :-> argument.
(defmethod map. ((func function) (seq viewable-sequence) &rest args)
  (let* ((key-pos (position :-> args))
	 (res-arg (when key-pos (nth (1+ key-pos) args)))
	 (other-seqs (if key-pos (subseq args 0 key-pos) args))
	 (len (loop for s in (cons seq other-seqs) minimize (length. s)))
	 results)
    (when (and (viewable-sequence-p res-arg)
    	       (/= (length. res-arg) len))
      (error "Result sequence should be of length ~A" len))
    (setq results
	  (loop for i from 0 below len
		for res-vbl = (when (viewable-sequence-p res-arg) (frame res-arg i))
		for other-vbls = (mapcar #'frame (circular-list i) other-seqs)
		collect
		(apply func (frame seq i)
		       (append other-vbls (when res-vbl (list :-> res-vbl))))))
    (cond ((viewable-sequence-p res-arg)
	   res-arg)
	  ((every #'(lambda (x) (viewable-p x)) results)
	   (make-viewable-sequence results :-> res-arg))
	  ((notany #'(lambda (x) (viewable-p x)) results)
	   results)
	  (t (mapc #'(lambda (x) (when (viewable-p x) (destroy x))) results)
	     (error "Results are a mixture of viewables and non-viewables!")))))


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
