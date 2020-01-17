;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: fileio.lisp
;;;  Author: David Heeger
;;;  Description: image file loading and saving
;;;  Creation Date: summer '88
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)
(export '(load-image save-image save-picture load-image-sequence))
	  
;;; *** This badly needs a rewrite.  It should use the CL read function to
;;; parse the descriptor file of a datfile directory.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Filename, directory, and path utilities:

;;; Definitions for directory-p, directory-path, trim-right-delimiter
;;; moved to mcl-hacks.lisp and lucid-hacks.lisp

;;; Strip off directory names, leaving only filename
(defun extract-filename (path)
  (setq path (pathname (trim-right-delimiter path)))
  (file-namestring path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; getkey and setkey for descriptor files

;;; Patch so that standard datfiles (with colons etc) can be read.
;;; When an error occurs while trying to read a key, we go back to the
;;; file-position where the key begins, read the key as a symbol, and then
;;; read everything up to the closing paren as a string.
(defun df-getkey (path key &optional default)
  (with-open-file (stream (pathname path)
			  :direction :input)
    (loop for pos = (file-position stream)
	  for sexpr = (ignore-errors (read stream nil :eof))
	  do
	  (when (null sexpr)  ;if error reading, read as string
	    (let (str sym)
	      (file-position stream pos)
	      (loop for char = (read-char stream nil :eof)
		    until (or (eq char :eof)
			      (char= char #\()))
	      (setq sym (read stream nil :eof))     ;read key
	      (setq str (with-output-to-string (string-stream)  ;read string
			  (loop for prev-char = #\a then char
				for char = (read-char stream nil :eof)
				until (or (eq char :eof)
					  (and (char= #\) char)
					       (char/= #\\ prev-char)))
				do (write-char char string-stream))))
	      (setq sexpr (if (eq sym :eof) :eof (list sym str)))))
	  until (or (eq sexpr :eof)
		    (string-equal (format nil "~a" (car sexpr)) key))
	  finally
	  (return 
	    (if (eq sexpr :eof) 
		default
		(if (or (string-equal key "_dimensions")
			(> (length sexpr) 2))
		    (cdr sexpr)
		    (cadr sexpr)))))))

;;; file MUST already exist
;;; doesnt check if you write the same keyword more than once
(defun df-setkey (stream key value)
  (if (string-equal key "_dimensions")
      (format stream "(~a ~s ~s)~%" key (list-y-dim value) (list-x-dim value))
      (format stream "(~a ~s)~%" key value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code for load images

;;; General load function.  Loads images from datfiles or from
;;; raw data files.  Default result parameter is the filename.  Image is bound
;;; to a symbol with that print-name if *auto-bind-loaded-images* is t.
(defun load-image (path &key ->
			reverse-bytes
			ysize xsize)
  (setq path (trim-right-delimiter path))
  (let* ((result (or -> (if *auto-bind-loaded-images* 
			    (intern (string-upcase (extract-filename path)))
			    (extract-filename path))))
	 (new-img
	  (cond ((and (directory-p path)
		      (df-getkey 
		       (concatenate 'string (namestring (directory-path path))
				    "descriptor")
		       "_data_files"))
		 (load-image-sequence path :-> result))
		((directory-p path)
		 (image-from-datfile path :reverse-bytes reverse-bytes
				     :-> result))
		((probe-file path)
		 (cond ((and ysize xsize)
			(image-from-file path :ysize ysize :xsize xsize :-> result))
		       (t
			(status-message "Assuming image is square, 8 bits.")
			(image-from-file path :-> result))))
		(t (error "Bad filename: ~S" path)))))
    (set-history new-img 'load-image path)
    new-img))

(defun image-from-datfile (path &key (data-filename "data") reverse-bytes ->)
  (let* ((d-path (namestring (directory-path path)))
	 (descriptor-path (concatenate 'string d-path "descriptor"))
	 (data-path (concatenate 'string d-path data-filename))
	 (data-type (df-getkey descriptor-path "_data_type"))
	 (data-sets (df-getkey descriptor-path "_data_sets"))
	 (channels (df-getkey descriptor-path "_channels"))
	 (dimensions (df-getkey descriptor-path "_dimensions"))
	 (scale (df-getkey descriptor-path "scale"))
	 (pedestal (df-getkey descriptor-path "pedestal"))
	 (byte-order (df-getkey descriptor-path "byte-order"))
	 (info-list (df-getkey descriptor-path "info-list"))
	 the-result)
    (when (or (and data-sets (not (= data-sets 1)))
	      (and channels (not (= channels 1))))
      (error "Not yet supported data-sets=~d, channels=~d" data-sets channels))
    (with-result ((result ->) (list :class 'image :dimensions dimensions))
      (cond ((string-equal data-type "unsigned_1")
	     (load-byte-8-image data-path result))
	    ((string-equal data-type "unsigned_4") 
	     (load-byte-32-image data-path result byte-order reverse-bytes))
	    ((string-equal data-type "float") 
	     (load-float-image data-path result))
	    ((string-equal data-type "ascii") 
	     (load-ascii-image data-path result))
	    (t (error "Data Type ~S not supported" data-type)))
      (when (or scale pedestal)
	(linear-xform result (or scale 1.0) (or pedestal 0.0) :-> result))
      (setq the-result result))
    ;;; *** horribly ugly hack ***
    (setf (info-list the-result) info-list)
    the-result))

;;; Read an image from a raw data file
;;; assumes '(unsigned-byte 8)
;;;; *** SHould factor total size to get correct defaults for xsize/ysize
(defun image-from-file (path &key ysize xsize -> (skip-bytes 0))
  (when (not (and ysize xsize))
    (let ((file-length (with-open-file (stream (pathname path)
					:direction :input)
			 (file-length stream))))
      (setq ysize (round (sqrt file-length)))
      (setq xsize ysize)))
  (with-result ((result ->) (list :class 'image :dimensions (list ysize xsize)))
    (load-byte-8-image (namestring path) result :skip-bytes skip-bytes)
    result))

(defun load-ascii-image (path-string result)
  (with-status-message (format nil "Reading image data from ~S" path-string)
    (ascii-array-read (data result) path-string))
  result)

;;; skip-bytes keyword so that you can skip the header. -DH
(defun load-float-image (path-string result &key (skip-bytes 0))
  (with-status-message (format nil "Reading image data from ~S" path-string)
    (float-array-read (data result) path-string :skip-bytes skip-bytes))
  result)

(defun load-byte-8-image (path-string result &key (skip-bytes 0))
  (with-static-arrays ((temp (allocate-array (dimensions result)
					     :element-type '(unsigned-byte 8))))
    (with-status-message (format nil "Reading image data from ~S" path-string)
      (array-read temp path-string :skip-bytes skip-bytes))
    (with-status-message "Creating floating point image array"
      (convert-from-8bit result temp)))
  result)

(defvar *machine-byte-order* :forward)

(defun load-byte-32-image (path-string result byte-order reverse-bytes)
  (let ((temp (make-array (dimensions result)
			  :element-type '(unsigned-byte 32))))
    (with-status-message (format nil "Reading image data from ~S" path-string)
      (cond (reverse-bytes
	     (array-read-and-swap temp path-string))
	    ((eq byte-order *machine-byte-order*)
	     (array-read temp path-string))
	    ((not byte-order)
	     (status-message "Warning: Byte-order not specified...")
	     (array-read temp path-string))
	    ((not (eq byte-order *machine-byte-order*))
	     (array-read-and-swap temp path-string))))
    (with-status-message "Creating floating point image array"
      (convert-from-32bit result temp)))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code for saving images as datfiles
;;; currently implemented only for regular floating point images
;;; auto-scale only used for element-type '(unsigned-byte 8)
;;; reverse-bytes only used for element-type '(unsigned-byte 32)

(defmethod save-image ((im image) path
		       &key 
		       (element-type '(unsigned-byte 8))
		       (auto-scale t)
		       reverse-bytes
		       df-keys)
  (let* ((dir-path (namestring (directory-path path)))
	 (descriptor-path (concatenate 'string dir-path "descriptor"))
	 (data-path (concatenate 'string dir-path "data"))
	 (dimensions (dimensions im)))
    (unless (and (probe-file (pathname path)) (directory-p path))
      (create-directory path))
    (with-open-file (descriptor-stream (pathname descriptor-path)
				       :direction :output
				       :if-does-not-exist :create
				       :element-type :default)
      (df-setkey descriptor-stream "_data_sets" 1)
      (df-setkey descriptor-stream "_channels" 1)
      (df-setkey descriptor-stream "_dimensions" dimensions)
      (df-setkey descriptor-stream "_history" (constructor im))
      (loop for item in df-keys do
	    (df-setkey descriptor-stream (car item) (cadr item)))
      (with-status-message "Saving datfile"
	(cond ((or (equal element-type 'float)
		   (equal element-type 'single-float))
	       (save-float-image im descriptor-stream data-path))
	      ((equal element-type '(unsigned-byte 8))
	       (save-byte-8-image im descriptor-stream data-path auto-scale))
	      ((equal element-type '(unsigned-byte 32))
	       (save-byte-32-image im descriptor-stream data-path
				   :reverse-bytes reverse-bytes))
	      ((equal element-type 'character)
	       (save-ascii-image im descriptor-stream data-path))
	      (t (error "Data Type ~S not supported" element-type))))))
  dir-path)

;;; this only works on gray pictures of images!!
(defun save-picture (path)
  (let* ((pic (car (picture-stack *current-pane*)))
	 (im (viewable pic))
	 (dir-path (namestring (directory-path path)))
	 (descriptor-path (concatenate 'string dir-path "descriptor"))
	 (data-path (concatenate 'string dir-path "data"))
	 (dimensions (dimensions im)))
    (unless (and (probe-file (pathname path)) (directory-p path))
      (create-directory path))
    (with-open-file (descriptor-stream (pathname descriptor-path)
				       :direction :output
				       :if-does-not-exist :create
				       :element-type :default)
      (df-setkey descriptor-stream "_data_sets" 1)
      (df-setkey descriptor-stream "_channels" 1)
      (df-setkey descriptor-stream "_dimensions" dimensions)
      (with-status-message "Saving datfile"
	(save-byte-8-image im descriptor-stream data-path nil 
			   (scale pic) (pedestal pic)))))
  t)
			 
(defun save-ascii-image (im descriptor-stream path-string)
  (df-setkey descriptor-stream "_data_type" "ascii")
  (ascii-array-write (data im) path-string))

(defun save-float-image (im descriptor-stream path-string)
  (df-setkey descriptor-stream "_data_type" "float")
  (float-array-write (data im) path-string))

(defvar *unsigned-byte-32-range* (float (- (expt 2 30) 1)))
(defvar *unsigned-byte-8-range*  255.0)

(defun save-byte-8-image (im descriptor-stream path-string auto-scale 
			  &optional scale pedestal)
  (df-setkey descriptor-stream "_data_type" "unsigned_1")
  (setq scale (/ (or scale (if auto-scale (range im) *unsigned-byte-8-range*))
		 *unsigned-byte-8-range*)
	pedestal (or pedestal (if auto-scale (minimum im) 0.0)))
  (when (or scale auto-scale)
    (df-setkey descriptor-stream "scale" scale))
  (when (or pedestal auto-scale)
    (df-setkey descriptor-stream "pedestal" pedestal))
  (with-static-arrays ((temp (allocate-array (dimensions im) :element-type '(unsigned-byte 8))))
    (convert-to-8bit im temp (float pedestal) (float (/-0 1.0 scale 1.0)))
    (array-write temp path-string)))

(defun save-byte-32-image (im descriptor-stream path-string
			   &key reverse-bytes)
  (let* ((scale (/ (range im) *unsigned-byte-32-range*))
	 (pedestal (minimum im))
	 (other-byte-order (if (eq *machine-byte-order* :forward)
			       :reverse
			       :forward)))
    (df-setkey descriptor-stream "_data_type" "unsigned_4")
    (df-setkey descriptor-stream "scale" scale)
    (df-setkey descriptor-stream "pedestal" pedestal)
    (if reverse-bytes
	(df-setkey descriptor-stream "byte-order" other-byte-order)
	(df-setkey descriptor-stream "byte-order" *machine-byte-order*))
    (let ((temp (make-array (dimensions im)
			    :element-type '(unsigned-byte 32))))
      (convert-to-32bit im temp (float pedestal) (float (/-0 1.0 scale 1.0)))
      (if reverse-bytes
	  (array-swap-and-write temp path-string)
	  (array-write temp path-string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Loads image sequences from datfile directories with one descriptor file
;;; and multiple data files called dataN where N is a number.
(defun load-image-sequence (path 
			    &key 
			    (start-index 0) end-index
			    (->
			     (if *auto-bind-loaded-images* 
				 (intern (string-upcase (extract-filename path)))
				 (extract-filename path))))
  (when (not (directory-p path))
    (error "Bad filename for datfile: ~S" path))
  (setq path (trim-right-delimiter path))
  (let* ((dir-path (namestring (directory-path path)))
	 (descriptor-path (concatenate 'string dir-path "descriptor"))
	 (dimensions (df-getkey descriptor-path "_dimensions"))
	 (data-files (df-getkey descriptor-path "_data_files"))
	 (class (df-getkey descriptor-path "class" 'image-sequence)))
    (setq end-index (or end-index data-files))
    (with-result ((result ->) (list :class class
				    :dimensions dimensions
				    :length (- end-index start-index))
		  'load-image-sequence path
		  :start-index start-index :end-index end-index)
      (loop for i from start-index below end-index
	    for n from 0
	    for res = (frame n result)
	    do
	    (image-from-datfile path :data-filename (format nil "data~a" i) :-> res)
	    (unless (name res)
	      (set-name res (format nil "frame~a" i))))
      result)))

;;; This saves the sequence as a datfile with one descriptor file 
;;; and multiple data files called dataN where N is a number.
(defmethod save-image ((seq image-sequence) path 
		       &key 
		       (element-type '(unsigned-byte 8))
		       (auto-scale t)
		       reverse-bytes
		       df-keys
		       (start-index 0) 
		       (end-index (1- (length. seq))))
  (let* ((dir-path (namestring (directory-path path)))
	 (descriptor-path (concatenate 'string dir-path "descriptor"))
	 (data-path (concatenate 'string dir-path "data"))
	 (dims (dimensions seq)))
    (unless (and (probe-file (pathname path)) (directory-p path))
      (create-directory path))
    (with-open-file (descriptor-stream (pathname descriptor-path)
				       :direction :output
				       :if-does-not-exist :create
				       :element-type :default)
      (df-setkey descriptor-stream "_data_files" (- end-index start-index -1))
      (df-setkey descriptor-stream "_data_sets" 1)
      (df-setkey descriptor-stream "_channels" 1)
      (df-setkey descriptor-stream "_dimensions" dims)
      (df-setkey descriptor-stream "class" (object-class-name seq))
      (loop for item in df-keys do
	    (df-setkey descriptor-stream (car item) (cdr item)))
      (with-status-message "Saving datfile"
	(cond ((or (equal element-type 'float)
		   (equal element-type 'single-float))
	       (save-float-image-sequence seq descriptor-stream data-path
					  start-index end-index))
	      ((equal element-type '(unsigned-byte 8))
	       (save-byte-8-image-sequence seq descriptor-stream data-path auto-scale
					   start-index end-index))
	      ((equal element-type '(unsigned-byte 32))
	       (save-byte-32-image-sequence seq descriptor-stream data-path
					    start-index end-index
					    :reverse-bytes reverse-bytes))))))
  dir-path)
	   
(defun save-float-image-sequence (seq descriptor-stream path-string
				  start-index end-index)
  (df-setkey descriptor-stream "_data_type" "float")
  (loop for i from start-index to end-index do
	(float-array-write (data (frame seq i)) (format nil "~a~a" path-string i))))

(defun save-byte-8-image-sequence (seq descriptor-stream path-string auto-scale 
				   start-index end-index)
  (df-setkey descriptor-stream "_data_type" "unsigned_1")
  (let ((scale (if auto-scale (/ (range seq) *unsigned-byte-8-range*) 1.0))
	(pedestal (if auto-scale (minimum seq) 0.0)))
    (when auto-scale
      (df-setkey descriptor-stream "scale" scale)
      (df-setkey descriptor-stream "pedestal" pedestal))
    (with-static-arrays ((temp (allocate-array (dimensions seq) :element-type '(unsigned-byte 8))))
      (loop for i from start-index to end-index do
	    (convert-to-8bit (frame seq i) temp (float pedestal) (float (/ 1.0 scale)))
	    (array-write temp (format nil "~a~a" path-string i))))))

(defun save-byte-32-image-sequence (seq descriptor-stream path-string
				       start-index end-index
				       &key reverse-bytes)
  (let* ((scale (/ (range seq) *unsigned-byte-32-range*))
	 (pedestal (minimum seq))
	 (other-byte-order (if (eq *machine-byte-order* :forward)
			       :reverse
			       :forward)))
    (df-setkey descriptor-stream "_data_type" "unsigned_4")
    (df-setkey descriptor-stream "scale" scale)
    (df-setkey descriptor-stream "pedestal" pedestal)
    (if reverse-bytes
	(df-setkey descriptor-stream "byte-order" other-byte-order)
	(df-setkey descriptor-stream "byte-order" *machine-byte-order*))
    (let ((temp (make-array (dimensions seq)
			    :element-type '(unsigned-byte 32))))
      (loop for i from start-index to end-index do
	    (convert-to-32bit (frame seq i) temp (float pedestal) (float (/ 1.0 scale)))
	    (if reverse-bytes
		(array-swap-and-write temp (format nil "~a~a" path-string i))
		(array-write temp (format nil "~a~a" path-string i)))))))


;;; local Variables:
;;; buffer-read-only: t 
;;; End:
