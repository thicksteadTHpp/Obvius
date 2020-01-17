;;; -*- Package: OBVIUS; Syntax: Common-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: tiffio.lisp
;;;  Author: Carsten Schroeder (Dept. of Computer Science, Univ. of Hamburg)
;;;  Description: loading and saving of viewables from and to TIFF files
;;;  Creation Date: fall '92
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "OBVIUS")
(export '(load-tiff-viewable save-tiff-viewable
          *tiff-compression* *tiff-extension*
	  *photometric-min-interpretation*))


;;;
;;; Requires color package to be pre-loaded.
;;;
(obv-require :color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Description
;;; -----------

;;; This file provides the reading and writing of IMAGEs and 
;;; VIEWABLE-SEQUENCEs from and to TIFF files.  The components of 
;;; VIEWABLE-SEQUENCEs may be IMAGEs and VIEWABLE-SEQUENCEs, but no other
;;; kind of VIEWABLE.  There is no restriction that the components of a 
;;; VIEWABLE-SEQUENCE must be of the same class or size except for 
;;; COLOR-IMAGEs.  There is no explicit error checking; we totally rely on 
;;; the constructor functions or the with-result macro to enforce any 
;;; restrictions.

;;; Special care has been taken to reconstruct the hierarchical structure
;;; of a viewable when loading it from a TIFF file.  Of cause,
;;; this can only be done for TIFF files created by Obvius.
;;; Therefore, TIFF files are marked upon writing by setting the Software
;;; tag field of the first image page to "Obvius" (case sensitive!), 
;;; and the hierachical structure of the viewable to be saved is stored
;;; as a tree of class names (see the example below) under the :structure
;;; key in a keylist which is written to the DocumentName tag field of 
;;; the first image page.
;;; Using this structure tree the loading of a viewable from an Obvius TIFF
;;; file can be done in the same way as the saving with a recursive
;;; generic function: load-tiff (analogous to save-tiff).  Although the
;;; recursive calling starts at the root of the structure tree, note that
;;; -- when loading a TIFF file -- the viewables are created in a bottom-up
;;; manner starting at the leaves of the tree.

;;; In both cases -- loading and saving -- there are 4 methods: for 
;;; VIEWABLE-SEQUENCEs, BIT-IMAGEs, COLOR-IMAGEs, and IMAGEs.
;;; When loading a TIFF file we only have the class names stored in the 
;;; structure tree; therefore, there's a 5th method load-tiff specilized
;;; to symbols (see the comment below).

;;; COLOR-IMAGEs are not handled as sequences but as atomic images, 
;;; i.e. leaves of the structure tree of a viewable, because they are
;;; stored in a single page in multiple planes instead of multiple,
;;; consecutive pages.  If you want to change this, maybe because some other
;;; TIFF reader you are using doesn't support multi-plane pages, just 
;;; remove the methods specialized to COLOR-IMAGEs (viewable-structure,
;;; save-tiff, and load-tiff).  COLOR-IMAGEs will be handled as
;;; sequences then. 

;;; Information about atomic viewables, e.g. scale and pedestal, are
;;; stored in a keylist which is written to the PageName tag field of
;;; each page.

;;; If a TIFF file to be loaded was not created by Obvius we try to construct
;;; a flat VIEWABLE-SEQUENCE (not an IMAGE-SEQUENCE, because this doesn't
;;; allow for COLOR-IMAGEs as components (see the comment in the code of
;;; load-nonobvius-tiff)).  If the number of pages in the TIFF file or the
;;; length specified by START and END is 1 an atomic viewable is created
;;; instead of a sequence.  The following kinds of pages can be loaded:
;;;   1 bit, 1 sample				loaded as BIT-IMAGE
;;;   8 bit, 1 sample, gray image		loaded as IMAGE
;;;   8 bit, 1 sample, pallete color image	loaded as IMAGE upon request
;;;   8 bit, 3 samples, separate planes		loaded as COLOR-IMAGE
;;;   8 bit, 3 samples, contiguous planes	loaded as COLOR-IMAGE

;;; Notes
;;; -----

;;; 1. For every subclass of VIEWABLE-SEQUENCE there must be a constructor
;;;    function make-<viewable-class>!  See the comment in 
;;;    load-tiff (specialized to viewable-sequence).
;;;
;;; 2. The end index of load-tiff-viewable is exclusive as in
;;;    Common Lisp's SUBSEQ!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tiff-compression* tiff:compression-lzw
  "The compression scheme used when writing viewables to TIFF files.")


(defvar *tiff-extension* ".tif"
  "Extension to be stripped off when creating the viewable name from
the path name given to load-tiff-viewable.")


(defvar *photometric-min-interpretation* tiff:photometric-minisblack
  "Value used for interpreting BIT-IMAGEs when writing them to or
reading them from TIFF files.  May be set to
tiff:photometric-minisblack (default) or tiff:photometric-miniswhite.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operations for getting and handling the structure trees of
;;; viewables.  The following gives an example for such a structure tree:
;;;    (viewable-sequence (image-pair (image) (image))
;;;                       (image-pair (image) (image))
;;;                       (image-pair (image) (image))) 

;;; Determine the hierarchical structure of a viewable.

(defmethod viewable-structure ((viewable image))
  (list (type-of viewable)))

(defmethod viewable-structure ((viewable color-image))
  ;; color-image is treated as an atomic class, not as a viewable-sequence
  (declare (ignore viewable))
  (list 'color-image))

(defmethod viewable-structure ((viewable viewable-sequence))
  (cons (type-of viewable)
	(loop for i from 0 below (sequence-length viewable)
	      collect
	      (viewable-structure (frame i viewable)))))


;; Determine the number of atomic viewables from a structure tree.
(defun viewable-structure-atomic-length (structure)
  (if (null (cdr structure))
      1
      (reduce #'+ (mapcar #'viewable-structure-atomic-length
			  (cdr structure)))))

;; If the root of the viewable structure is viewable-sequence return
;; the length of this sequence.
(defun viewable-structure-sequence-length (structure)
  ;; Note: a single image has sequence length 0 (null cdr)!
  (length (cdr structure)))

;; If the root of the viewable structure is viewable-sequence return a
;; new structure consisting of the same root and the subsequence
;; specified by start and end.  It is analogous to the Common Lisp
;; function subseq, i.e. end is exclusive!
(defun sub-viewable-structure (structure start &optional end)
  ;; Error checking is done by subseq.
  (cons (car structure) (subseq (cdr structure) start end)))

;; Determine the number of atoming viewables in the subsequence from
;; 0 to start (exclusively).
(defun leading-atomic-viewables (structure start)
  (viewable-structure-atomic-length
   (sub-viewable-structure structure 0 start)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define a keylist used for the image description saved in a TIFF file.

;(declaim (inline set-key get-key keylist-to-string))

(defun make-keylist (&optional string)
  (cons
   (when string
     (let ((l (read-from-string string)))
       (if (listp l)
	   l
	   (error "String ~S does not contain a list." string))))
     nil))

(defun set-key (keylist key value)
  (setf (getf (car keylist) key) value))

(defun get-key (keylist key)
  (getf (car keylist) key))

(defun keylist-to-string (keylist)
  (format nil "(~{~%~S ~S~})" (car keylist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Saving viewables to TIFF files.

(defun save-tiff-viewable (viewable path &key (autoscale t))
  (let ((keylist (make-keylist))
	(structure (viewable-structure viewable)))
    (set-key keylist :structure structure)
    (set-key keylist :history (constructor viewable))
    (tiff:with-open-tiff-file (tif path
				   :direction :output
				   :if-exists :error)
      (status-message "Saving TIFF-file...")
      ;; Store information about the viewable in the first TIFF subfile.
      (tiff:set-documentname tif (keylist-to-string keylist))
      ;; Identify as Obvius image -- never forget writing this!
      (tiff:set-software tif "Obvius")
      (save-tiff tif viewable autoscale)
      (status-message "Saving TIFF-file...done")))
  nil)

(defmethod save-tiff (tif (image image) 
			  &optional autoscale subviewablep)
  (let ((keylist (make-keylist))
	(scale (/ (if autoscale (range image) *unsigned-byte-8-range*)
		  *unsigned-byte-8-range*))
	(pedestal (if autoscale (minimum image) 0.0)))
    (set-key keylist :scale scale)
    (set-key keylist :pedestal pedestal)
    (tiff:set-pagename tif (keylist-to-string keylist))
    (tiff:set-compression tif *tiff-compression*)
    (when subviewablep
      (tiff:set-newsubfiletype tif tiff:filetype-page))
    (tiff:set-photometric tif tiff:photometric-minisblack)
    (tiff:set-planarconfig tif tiff:planarconfig-contig)
    (tiff:set-samplesperpixel tif 1)
    (tiff:set-bitspersample tif 8)
    (tiff:set-imagelength tif (y-dim image))
    (tiff:set-imagewidth tif (x-dim image))
    (with-static-arrays ((temp (allocate-array (dimensions image)
					       :element-type
					       '(unsigned-byte 8))))
      (convert-to-8bit image 
		       temp
		       (float pedestal)
		       (float (/-0 1.0 scale 1.0)))
      (tiff:write-image-plane tif temp))
    (tiff:write-directory tif)))
  
(defmethod save-tiff (tif (image bit-image) 
			  &optional autoscale subviewablep)
  (declare (ignore autoscale))
  (let ((imagelength (y-dim image))
	(imagewidth (x-dim image)))
    (tiff:set-compression tif *tiff-compression*)
    (when subviewablep
	  (tiff:set-newsubfiletype tif tiff:filetype-page))
    (tiff:set-photometric tif *photometric-min-interpretation*)
    ;; msb2lsb is default.  We are doing this here anyway just to
    ;; remind ourself of the problems with bit-images; see Description
    ;; in tiff-ffi.lisp 
    (tiff:set-fillorder tif tiff:fillorder-msb2lsb)
    (tiff:set-planarconfig tif tiff:planarconfig-contig)
    (tiff:set-samplesperpixel tif 1)
    (tiff:set-bitspersample tif 1)
    (tiff:set-imagelength tif imagelength)
    (tiff:set-imagewidth tif imagewidth)
    (let ((scanline-bit-size (* (tiff:scanline-size tif) 8)))
      ;; test for necessary padding 
      (if (< imagewidth scanline-bit-size)
	  (with-static-arrays ((temp (allocate-array (list imagelength
							   scanline-bit-size)
						     :element-type
						     '(unsigned-byte 1)
						     :initial-element 0)))
            
	    (tiff:write-image-plane
	     tif
	     (copy-bit-array (data image) temp imagelength imagewidth)))
	(tiff:write-image-plane tif (data image))))
    (tiff:write-directory tif)))

(defmethod save-tiff (tif (image color-image) 
			  &optional autoscale subviewablep)
  (let ((keylist (make-keylist))
	(scale (make-array 3 :element-type 'float))
	(pedestal (make-array 3 :element-type 'float)))
    (dotimes (i 3)
      (setf (aref scale i) 
	    (/ (if autoscale (range (frame i image)) *unsigned-byte-8-range*)
	       *unsigned-byte-8-range*))
      (setf (aref pedestal i)
	    (if autoscale (minimum (frame i image)) 0.0)))
    (set-key keylist :scale0 (aref scale 0))
    (set-key keylist :scale1 (aref scale 1))
    (set-key keylist :scale2 (aref scale 2))
    (set-key keylist :pedestal0 (aref pedestal 0))
    (set-key keylist :pedestal1 (aref pedestal 1))
    (set-key keylist :pedestal2 (aref pedestal 2))
    (tiff:set-pagename tif (keylist-to-string keylist))
    (tiff:set-compression tif *tiff-compression*)
    (when subviewablep
      (tiff:set-newsubfiletype tif tiff:filetype-page))
    (tiff:set-photometric tif tiff:photometric-rgb)
    (tiff:set-planarconfig tif tiff:planarconfig-separate)
    (tiff:set-samplesperpixel tif 3)
    (tiff:set-bitspersample tif 8)
    (tiff:set-imagelength tif (y-dim image))
    (tiff:set-imagewidth tif (x-dim image))
    (with-static-arrays ((temp (allocate-array (dimensions image)
					       :element-type
					       '(unsigned-byte 8))))
      (dotimes (sample 3)
	(convert-to-8bit (frame sample image)
			 temp
			 (float (aref pedestal sample))
			 (float (/-0 1.0 (aref scale sample) 1.0)))
	(tiff:write-image-plane tif temp sample)))
    (tiff:write-directory tif)))

(defmethod save-tiff (tif (sequence viewable-sequence) 
			  &optional autoscale subviewablep)
  (declare (ignore subviewablep))
  (loop for i from 0 below (sequence-length sequence)
	do
	(save-tiff tif (frame i sequence) autoscale t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loading viewables from TIFF files which were written by Obvius.

(defvar *tiff-page* 0 
  "Counter for a sequential access to tiff pages.") 


;; *** end is exclusive as in Common Lisp's SUBSEQ!
(defun load-tiff-viewable (path &key -> (start 0) end)
  (let* ((name (string-upcase
		(string-right-trim *tiff-extension*
				   (extract-filename path))))
	 (viewable-name (if *auto-bind-loaded-images*
			    (intern name)
			    name))
	 (result (or -> viewable-name))
	 (viewable
	  (tiff:with-open-tiff-file (tif path :direction :input)
	    (if (string= (tiff:get-software tif) "Obvius")
		(let* ((keylist (make-keylist (tiff:get-documentname tif)))
		       (structure (get-key keylist :structure))
		       (sequence-length (viewable-structure-sequence-length
					 structure)))
		  ;; handle reading a subsequence
		  (if (zerop sequence-length)
		      (setf *tiff-page* 0)
		      (cond ((or end (> start 0))
			     ;; Range checking is done by
			     ;; sub-viewable-structure  
			     (setf structure 
				   (sub-viewable-structure structure
							   start
							   end))
			     (setf *tiff-page*
				   (leading-atomic-viewables structure
							     start)))
			    (t
			     (setf *tiff-page* 0))))
		  (load-tiff tif
			     (first structure)
			     :structure (rest structure)
			     :-> result
                             :path path))
	        (load-nonobvius-tiff tif :-> result
					 :start start
					 :end end
                                         :path path)))))
    (set-history viewable 'load-tiff-viewable path :start start :end end)
    viewable))


;;; Now we use the structure tree read from the TIFF file to reconstruct 
;;; the hierarchical structure of the viewable.  This is done with the
;;; recursive generic function load-tiff in the same way as we did the
;;; saving with save-tiff.  But in contrast to save-tiff, we don't
;;; have any instances of the viewables yet which could be used for
;;; the method dispatch;  we have to use the class names instead.  
;;; One possible way would be the use of eql-specializers for these
;;; class names, but then we couldn't make use of method inheritance
;;; and would have to write lots of methods, one for each viewable
;;; class.
;;; The other way is to use a method which is specialized for symbols and
;;; calls the same generic function with an instance of the class
;;; specified by the symbol.  The AMOP generic function class-prototype 
;;; make this fairly easy.
;;; This way doubles the number of calls to save-tiff but the resulting
;;; code is much simpler.
;;;
;;; Note that the creation of the viewables is done in a bottom-up manner
;;; starting with the leaves of the structure tree.  Therefore, no explicit
;;; error checking must be done here; we can rely on the error checking
;;; done by the constructor functions.

(defmethod load-tiff (tif (name symbol) &key structure -> path)
  (load-tiff 
   tif
   (clos::class-prototype (find-class name))
   :structure structure
   :-> ->
   :path path))

(defmethod load-tiff (tif (image image) &key structure -> path)
  (declare (ignore structure))
  (tiff:set-directory tif *tiff-page*)
  (let* ((keylist (make-keylist (tiff:get-pagename tif)))
	 (scale (get-key keylist :scale))
	 (pedestal (get-key keylist :pedestal)))
    (with-result ((result ->)
		  (list :class (type-of image)
			:dimensions (list (tiff:get-imagelength tif)
					  (tiff:get-imagewidth tif))))
      (status-message "Reading image data from TIFF-file...")
      (with-static-arrays ((temp (tiff:read-image-plane tif)))
        (status-message "Reading image data from TIFF-file...done")
        (status-message "Creating floating point image array...")
	(convert-from-8bit result temp)
        (status-message "Creating floating point image array...done"))
      (when (or scale pedestal)
	(linear-xform result (or scale 1.0) (or pedestal 0.0) :-> result))
      (set-history result 'load-tiff-viewable path)
      (incf *tiff-page*)
      result)))

(defmethod load-tiff (tif (image bit-image) &key structure -> path)
  (declare (ignore image structure))
  (tiff:set-directory tif *tiff-page*)
  (let ((imagelength (tiff:get-imagelength tif))
	(imagewidth (tiff:get-imagewidth tif))
        (scanline-size (tiff:scanline-size tif)))
    (cond (;; padding necessary
           (> (* scanline-size 8)          ; scanline size in bits
	      imagewidth)
	   (with-result ((result ->)
			 (list :class 'bit-image
			       :dimensions (list imagelength imagewidth)))
             (status-message "Reading image data from TIFF-file...")
	     (with-static-arrays ((temp (tiff:read-image-plane tif)))
               (status-message "Reading image data from TIFF-file...done")
               (status-message "Handle padding of bit-image...")
	       (copy-bit-array temp (data result) imagelength imagewidth))
               (status-message "Handle padding of bit-image...done")
	     (incf *tiff-page*)
             (set-history result 'load-tiff-viewable path)
	     result))
          (;; destructive reading
           (viewable-p ->)
	   (status-message "Reading image data from TIFF-file...")
           (tiff:read-image-plane-into tif (data ->))
	   (status-message "Reading image data from TIFF-file...done")
	   (set-history -> 'load-tiff-viewable path)
	   (incf *tiff-page*)
           ->)
	  (t
	   (status-message "Reading image data from TIFF-file...")
           (let ((result
	          (make-instance 'bit-image
			         :data (tiff:read-image-plane tif)
			         :name ->)))
	     (status-message "Reading image data from TIFF-file...done")
             (set-history result 'load-tiff-viewable path)
	     (incf *tiff-page*)
             result)))))


(defmethod load-tiff (tif (image color-image) &key structure -> path)
  (declare (ignore image structure))
  (tiff:set-directory tif *tiff-page*)
  (let* ((keylist (make-keylist (tiff:get-pagename tif)))
         (scale0 (get-key keylist :scale0))
	 (scale1 (get-key keylist :scale1))
	 (scale2 (get-key keylist :scale2))
	 (scale (if (and scale0 scale1 scale2)
                    (make-array 3
			        :element-type 'float
			        :initial-contents
			        (list scale0 scale1 scale2))
                    nil))
         (pedestal0 (get-key keylist :pedestal0))
	 (pedestal1 (get-key keylist :pedestal1))
	 (pedestal2 (get-key keylist :pedestal2))
	 (pedestal (if (and pedestal0 pedestal1 pedestal2)
                       (make-array 3
			           :element-type 'float
			           :initial-contents
			           (list pedestal0 pedestal1 pedestal2))
                       nil)))
    (with-result ((result ->)
		  (list :class 'color-image
			:dimensions (list (tiff:get-imagelength tif)
					  (tiff:get-imagewidth tif))
			:length 3))
      (dotimes (sample 3)
	(status-message "Reading image data from TIFF-file...")
	(with-static-arrays ((temp (tiff:read-image-plane tif sample)))
          (status-message "Reading image data from TIFF-file...done")
	  (status-message "Creating floating point image array...")
	  (convert-from-8bit (frame sample result) temp)
	  (status-message "Creating floating point image array...done"))
        (set-history (frame sample result)
                     'load-tiff-viewable path)
	(when (or scale pedestal)
	  (linear-xform (frame sample result)
			(if scale (aref scale sample) 1.0)
			(if pedestal (aref pedestal sample) 0.0)
			:-> (frame sample result))))
      (set-history result 'load-tiff-viewable path)
      (incf *tiff-page*)
      result)))

(defmethod load-tiff (tif (sequence viewable-sequence)
                          &key structure -> path)
  ;; Unfortunately, we can't use make-instance here because this
  ;; doesn't provide any error checking; this is done by with-result.
  ;; On the other hand, we cannot use with-result because we don't know
  ;; the length of the sequence in advance -- the viewable tree must
  ;; be re-created in a bottom-up manner by first reading in the leaves
  ;; from the TIFF file.
  ;; So we have to contruct the name of the class specific constructor
  ;; function (which uses with-result) and call this.  
  (let ((result
         (funcall (make-internal-symbol 'make- (type-of sequence))
	          (loop for substructure in structure
                        for i from 0 below (length structure)
		        collect
		        (load-tiff tif 
			           (first substructure) 
			           :structure (rest substructure)
                                   :-> (if (viewable-p ->)
                                           (frame i ->)
                                           nil)
                                   :path path))
	          :-> ->)))
    (set-history result 'load-tiff-viewable path)
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loading viewables from Non-Obvius TIFF files.

;; *** end is exclusive as in Common Lisp's SUBSEQ!
(defun load-nonobvius-tiff (tif  &key -> (start 0) end path)
  (let ((pages (tiff:number-of-pages tif)))
    (unless end
      (setf end pages))
    (when (or (> end pages) (< end 1))
      (error "END is out of range 1..~D: ~D." pages end))
    (when (>= start end)
      (error "START is out of range 0..~D: ~D." (1- end) start))
    (setf *tiff-page* start)
    (if (> (- end start) 1)
	(make-viewable-sequence   ; change this to make-image-sequence
				  ; if COLOR-IMAGEs are allowed as components 
	 (loop for page from start below end
	       collect
	       (load-tiff-page tif
			       :-> (if (viewable-p ->)
				       (frame page ->)
				     nil)
			       :path path))
	 :-> ->)
        (load-tiff-page tif :-> -> :path path))))


(defun load-tiff-page (tif &key -> path)
  (tiff:set-directory tif *tiff-page*)
  (let ((bits-per-sample (tiff:get-bitspersample tif))
	(samples-per-pixel (tiff:get-samplesperpixel tif))
	(planar-config (tiff:get-planarconfig tif))
	(photometric (tiff:get-photometric tif)))
    (cond (;; BIT-IMAGE
	   (and (= bits-per-sample 1)
 		(= samples-per-pixel 1))
	   (let ((result (load-tiff tif 'bit-image :-> -> :path path)))
	     (if (not (equal photometric *photometric-min-interpretation*))
                 (invert result :-> result) 
		 result)))

	  (;; IMAGE
	   (and (= bits-per-sample 8)
		(= samples-per-pixel 1))
	   (when (equal photometric tiff:photometric-palette)
	     (cerror "Load as IMAGE discarding the colormap."
		     "TIFF page contains an 8bit palette color image."))
	   (let ((result (load-tiff tif 'image :-> -> :path path)))
	     (if (equal photometric tiff:photometric-miniswhite)
		 (negate result :-> result)
		 result)))

	  (;; COLOR-IMAGE separate
	   (and (= bits-per-sample 8)
		(= samples-per-pixel 3)
		(equal planar-config tiff:planarconfig-separate))
	   (load-tiff tif 'color-image :-> -> :path path))

	  (;; COLOR-IMAGE contig
	   (and (= bits-per-sample 8)
		(= samples-per-pixel 3)
		(equal planar-config tiff:planarconfig-contig))
	   (load-contig-multi-plane tif :-> -> :path path))

	  (t
	   (error "Cannot read TIFF image page.~%~
		   Bits per sample:            ~D~%~
		   Samples per pixel:          ~D~%~
		   Planar configuration:       ~A~%~
		   Photometric interpretation: ~A~%~
		   Try using the low level TIFF library for reading."
		  bits-per-sample
		  samples-per-pixel
		  (planar-config-readable planar-config)
		  (photometric-readable photometric))))))


;; Load a contiguous multi-plane image with 8 bits per pixel and 
;; 3 samples per pixel, and return a COLOR-IMAGE.
(defun load-contig-multi-plane (tif &key -> path)
  (tiff:set-directory tif *tiff-page*)
  (let* ((imagelength (tiff:get-imagelength tif))
         (imagewidth (tiff:get-imagewidth tif))
         (dimensions (list imagelength imagewidth)))
    (with-result ((result ->)
		  (list :class 'color-image
		        :dimensions dimensions
		        :length 3)
                  'load-tiff-viewable path)
      (with-static-arrays (;; Arrays of element type (unsigned-byte 24)
                           ;; are upgraded to (unsigned-byte 32).
                           ;; Therefore, we allocate a one-dimensional
                           ;; one-byte array of tripple length
                           ;; and call tiff:read-image-plane-into
                           ;; instead of tiff:read-image-plane.
                           (contig (allocate-array (list imagelength
                                                         (* 3 imagewidth))
                                                   :element-type
                                                   '(unsigned-byte 8)))
                           (red (allocate-array dimensions
						:element-type
						'(unsigned-byte 8)))
			   (green (allocate-array dimensions
						  :element-type
						  '(unsigned-byte 8)))
			   (blue (allocate-array dimensions
						 :element-type
						 '(unsigned-byte 8))))
        (let ((separate (make-array 3
                                    :initial-contents
                                    (list red green blue))))
          (status-message "Reading image data from TIFF-file...")
	  (tiff:read-image-plane-into tif contig)
          (status-message "Reading image data from TIFF-file...done")
          (status-message "Separating image planes...")
	  (tiff:separate-image-planes contig
				      (aref separate 0)
				      (aref separate 1)
				      (aref separate 2))
          (status-message "Separating image planes...done")
	  (dotimes (sample 3)
	    (status-message "Creating floating point image array...")
	    (convert-from-8bit (frame result sample) 
			       (aref separate sample))
	    (status-message "Creating floating point image array...done")
            (set-history (frame result sample) 'load-tiff-viewable path))))
      (incf *tiff-page*)
      result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliaries

;; *** This should be done in the TIFF library!
(defun planar-config-readable (config)
  (cond ((equal config tiff:planarconfig-contig)
         "Contiguous")
        ((equal config tiff:planarconfig-separate)
         "Separate")
        (t 
         (error "Unknown value for PlanarConfiguration tag: ~S" config))))

;; *** This should be done in the TIFF library!
(defun photometric-readable (photometric)
  (cond ((equal photometric tiff:photometric-miniswhite)
         "Min is white")
        ((equal photometric tiff:photometric-minisblack)
         "Min is black")
        ((equal photometric tiff:photometric-rgb)
         "RGB")
        ((equal photometric tiff:photometric-palette)
         "Palette")
        ((equal photometric tiff:photometric-mask)
         "Mask")
        ((equal photometric tiff:photometric-separated)
         "Separated")
        ((equal photometric tiff:photometric-ycbcr)
         "YCBCR")
        (t
         (error "Unknown value for PhotometricInterpretation tag: ~S" 
                photometric))))


(defun make-internal-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  ;; Binding of *print-case* to get symbols printed uppercase!
  (let ((*print-case* :upcase))
    (intern (format nil "~{~A~}" args))))


;; This might be done using paste, but paste doesn't seem to be
;; defined for bit-images.
(defun copy-bit-array (arr result rows cols)
  (declare (type (array bit (* *)) arr result)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (loop for row from 0 below rows
	do
	(loop for col from 0 below cols
	      do
	      (setf (aref result row col) (aref arr row col))))
  result)




;;; local Variables:
;;; buffer-read-only: nil
;;; End:
