;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: memory.lisp
;;;  Author: Simoncelli 
;;;  Description:  Static array allocator.
;;;  Creation Date: 11/90
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)

(export '(allocate-array free-array allocated-array-p with-static-arrays
	  expand-heap heap-status  *auto-expand-heap* *heap-growth-rate*
	  all-viewables named-viewables pictured-viewables
	  ogc scrounge! purge!))

;;; We construct a static array allocator which works by displacing
;;; sub-arrays into large static vectors.
;;; Purpose: 1) static arrays required for passing to foreign functions.
;;;          2) prevent large arrays from being garbage collected.
;;;          3) improved virtual memory swapping behavior.
;;; Unfortunately, we also lose by doing this since static space is
;;; permanent, and is never GC'ed.  Therefore, the allocated arrays
;;; must be "freed".

;;; For OBVIUS, since it is likely that typical code will forget to
;;; free some of the arrays, we also provide a crude
;;; obvius-garbage-collector (ogc), which preserves all arrays that
;;; belong to viewables which are accessable via symbols or pictures.
;;; ALL OTHER VIEWABLE STATIC DATA IS LOST!

;;; Note that, aside from the garbage collection scheme, the memory allocator
;;; does not depend on the OBVIUS system.

(defvar *heap-growth-rate* (min (* 1024 1024) array-dimension-limit)
  "Number of elements to expand the heap when out of memory")
(eval-when (load eval) (setf (get '*heap-growth-rate* :type)
			     `(integer ,(* 256 1024) ,array-dimension-limit)))

(defvar *auto-expand-heap* nil
  "When non-nil, expand heap automatically when out of memory")
(eval-when (load eval) (setf (get '*auto-expand-heap* :type) '(member t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Static Heap object

;;; For efficiency, we keep two structures containing the free arrays.
;;; One is an hlist (defined below) of free arrays sorted by size, and
;;; is used when allocating arrays.  The other is a plist of hlists,
;;; keyed by parent arrays, containing free arrays sorted by starting
;;; location in memory and is used when freeing arrays (since
;;; overlapping or abutting arrays must be merged).  We could use a
;;; hash table, but there will typically be only a small number of
;;; (large) base-arrays.  The type-synonyms slot is a list of all
;;; types which will create arrays (when passed to make-array) of the
;;; given type.
(eval-when (eval load compile)
  (defstruct (static-heap (:print-function static-heap-printer))
    type
    (type-synonyms nil)
    (free-arrays-by-size (hlist))
    (free-arrays-by-parent nil)))

;;; List of current memory pools for different types.
(defvar *static-heaps* nil)

(defun static-heap-printer (heap stream depth)
  (declare (ignore depth))
  (format stream "#<STATIC-HEAP ~A ~A>"
	  (static-heap-type heap)
	  (static-heap-total-size heap)))

(defun static-heap-base-arrays (heap)
  (loop for sub-plist = (static-heap-free-arrays-by-parent heap)
	then (cddr sub-plist)
	until (null sub-plist)
	collect (car sub-plist)))

(defun static-heap-total-size (heap)
  (loop for sub-plist = (static-heap-free-arrays-by-parent heap)
	then (cddr sub-plist)
	until (null sub-plist)
	sum (array-total-size (car sub-plist))))

(defun static-heap-free-size (heap)
  (loop for arr in (hlist-list (static-heap-free-arrays-by-size heap))
	sum (array-total-size arr)))

(defun static-heap-number-of-blocks (heap)
  (list-length (hlist-list (static-heap-free-arrays-by-size heap))))

(defun static-heap-largest-block-size (heap)
  (let ((largest-free (car (last (hlist-list (static-heap-free-arrays-by-size heap))))))
    (if largest-free (array-total-size largest-free) 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Top-level (exported) functions:

;;; Check that obj is an array allocated from one of the static heaps, and that it
;;; has not been freed, and does not overlap any freed array.
(defun allocated-array-p (obj)
  (when (arrayp obj)
    (let* ((heap (find-heap-of-type (array-element-type obj)))
	   (parent (parent-array obj))
	   hlist next-free-array)
      (when (and heap (setq hlist (getf (static-heap-free-arrays-by-parent heap) parent)))
	(setq next-free-array
	      (find (displaced-start obj) (hlist-list hlist) :key 'displaced-end :test #'<))
	(or (not next-free-array)
	    (and (>= (displaced-start next-free-array) (displaced-end obj)) t))))))

;;; Allocated a static (displaced array) suitable for passing to
;;; foreign functions.  The result is guaranteed to begin on an even
;;; word boundary.  If there is no heap of the given type, a new one
;;; is created.  NOTE: the memory allocator is designed to be
;;; efficient for allocation of reasonable numbers (i.e. hundreds) of
;;; fairly large arrays (i.e., not millions of tiny ones).
(defun allocate-array (dims &key
			    (element-type 'single-float)
			      (initial-element nil initial-element-p))
  (vom:debug "[allocate-array] of dim: ~a of type ~a" dims element-type)
  (let* ((heap (find-heap-of-type element-type :if-not-found :create))
	 (alloc-array-size (if (consp dims) (apply #'* dims) dims))
	 free-array-size alloc-array rem-arrays)
    (multiple-value-bind (free-array word-boundary-index)
	(find-free-array heap alloc-array-size)
      (vom:debug "[allocate-array] found free array of size ~d" (array-total-size free-array))
      (setq element-type (static-heap-type heap)) ;may be altered!
      (setq free-array-size (array-total-size free-array))
      (setq alloc-array 
	    (make-array dims :element-type element-type
			:displaced-to free-array
			:displaced-index-offset word-boundary-index))
      (setq rem-arrays
	    (nconc
	     (when (> word-boundary-index 0)
	       (vom:debug "[allocate-array] word-boundary-index>0 = ~d" word-boundary-index)
	       (list (make-array word-boundary-index
				 :element-type element-type
				 :displaced-to free-array)))
	     (when (> (- free-array-size word-boundary-index alloc-array-size) 0)
	       (vom:debug "[allocate-array] (> (- free-array-size word-boundary-index alloc-array-size) 0) = ~d" (- free-array-size word-boundary-index alloc-array-size))
	       (list (make-array (- free-array-size word-boundary-index alloc-array-size)
				 :element-type element-type
				 :displaced-to free-array
				 :displaced-index-offset
				 (+ alloc-array-size word-boundary-index))))))
      (vom:debug "[allocate-array] try to delete free array rem-arrays ~d" (length rem-arrays)) 
      (delete-free-array free-array heap :replacement-subarrays rem-arrays))
    ;; Set inital value
    (when initial-element-p
      (obvius::catch-errors
       (obvius::fill! alloc-array (coerce initial-element element-type))
       (warn "Array not filled with inital-element ~A" initial-element)))
    alloc-array))

;;; Can free sub-arrays of allocated arrays.  Just gives warnings if
;;; arr is not an array, or if arr was not allocated from a heap.
;;; Returns t if array was freed, nil otherwise.  Newer version allows
;;; suppression of warning. EJC
(defun free-array (arr &key (suppress-warning T))
  (vom:debug "[free-array]")
  (if (arrayp arr)
      (let ((heap (find-heap-of-type (array-element-type arr))))
	(if heap
	    (if (getf (static-heap-free-arrays-by-parent heap) (parent-array arr))
		(insert-free-array arr heap :consolidate-p t)
		(unless suppress-warning
		  (warn "Free-array: ~A was not allocated from ~A" arr heap)))
	    (unless suppress-warning
	      (warn "Free-array: ~A was not allocated from ~A" arr heap))))
      (unless suppress-warning
	(warn "Free-array called with non-array argument: ~A" arr))))

#|
(defun free-array (arr)
  (if (arrayp arr)
      (let ((heap (find-heap-of-type (array-element-type arr) :if-not-found :warn)))
	(when heap
	  (if (getf (static-heap-free-arrays-by-parent heap) (parent-array arr))
	      (insert-free-array arr heap :consolidate-p t)
	      (warn "Free-array: ~A was not allocated from ~A" arr heap))))
      (warn "Free-array called with a non-array argument: ~A" arr)))
|#
;;; This macro can be used in place of let* when the values being
;;; assigned to the local variables are static-arrays which should be
;;; destroyed upon leaving the macro.  New version allows using
;;; non-static arrays in list, so that one can use (similar) EJC
(defmacro with-static-arrays (arr-list &body body)
  (let* ((vars (loop for item in arr-list
		     for var = (if (symbolp item) item (car item))
		     collect var)))
    `(let* ,vars
      (unwind-protect
	   (progn ,@(loop for item in arr-list
			  when (listp item) collect `(setq ,@item))
		  ,@body)
	,@(loop for arr in vars
		collect `(when ,arr (free-array ,arr :suppress-warning t)))))))

#|
(defmacro with-static-arrays (arr-list &body body)
  (let* ((vars (loop for item in arr-list
		     for var = (if (symbolp item) item (car item))
		     collect var)))
    `(let* ,vars
      (unwind-protect
	   (progn ,@(loop for item in arr-list
			  when (listp item) collect `(setq ,@item))
		  ,@body)
	,@(loop for arr in vars
		collect `(when ,arr (free-array ,arr)))))))
|#

;;; Expand the heap by consing a static array and adding it to the
;;; list of static space in the heap.
(defun expand-heap (type-or-heap &optional (size *heap-growth-rate*) return-block)
  (vom:debug "~&[expand heap]")
  (let (heap type base-array)
    (setq heap (if (static-heap-p type-or-heap)
		   type-or-heap
		   (find-heap-of-type type-or-heap :if-not-found :create)))
    (setq type (static-heap-type heap))
    (setq size (obvius::next-word-boundary size type))
    (format t ";;; Expanding heap ~A by ~D elements ...~%" heap size)
    (vom:debug "~&[expand heap] ...make new static array of size ~d and type ~a" size type)
    (setq base-array (make-static-array size type))
    (setf (getf (static-heap-free-arrays-by-parent heap) base-array)
	  (hlist base-array))
    (insert-into-hlist base-array
		       (static-heap-free-arrays-by-size heap)
		       :key #'array-total-size)
    (if return-block base-array t)))

;;; Report information about the different type heaps.  Lines are 66
;;; characters long.
(defun heap-status ()
  "Print out information about the static array allocator.  For each type of
static heap, prints the number of used elements, the number of free elements,
the number of contiguous blocks (i.e. fragmentation), and the size of the
largest block."
  (format t "~%Static Heap Status:~%")
  (format t "  ~22A~11@A~11@A~11@A~10@A~%" "Type" "Used" "Free" "Max Block" "# Blocks")
  (format t "  ~65,,,'-<~>~%")
  (loop for heap in *static-heaps*
	for type = (static-heap-type heap)
	for free = (static-heap-free-size heap)
	for used = (- (static-heap-total-size heap) free)
	for largest = (static-heap-largest-block-size heap)
	for num = (static-heap-number-of-blocks heap)
	do
	(format t "  ~22A~11@A~11@A~11@A~10@A~%" type used free largest num))
  (format t "  ~65,,,'-<~>~%~%")
  (values))

;;; Use this to verify the integrity of the static heaps.  For each
;;; item in *static-heaps*, it checks:
;;;   1) item is a static heap
;;;   2) synonym-types of the heap create arrays of correct type.
;;;   3) parent-list of heap is a plist.
;;;   4) parent-list keys are static arrays of correct type.
;;;   4) parent-list arrays are sub-arrays of the correct parent.
;;;   5) parent-list arrays are non-overlapping and in memory order.
;;;   6) size-list of heap is an hlist.
;;;   7) size-list arrays are in order of size.
;;;   8) parent-list and size-list contain the SAME arrays.
;;; Prints out all free arrays.
(defun run-heap-diagnostics ()
  (loop with diff
	initially (format t "~%Static Heap Free Arrays:~%")
	for heap in *static-heaps*
	for heap-type = (static-heap-type heap)
	for size-hlist = (static-heap-free-arrays-by-size heap)
	for arrays-by-size = (hlist-list size-hlist)
	for parent-plist = (static-heap-free-arrays-by-parent heap)
	for arrays-by-parent = nil
	do
	(unless (static-heap-p heap)  (warn "~A is not a Static heap!!" heap))
	(dolist (synonym (static-heap-type-synonyms heap))
	  (unless (equal heap-type
			 (array-element-type (make-array 0 :element-type synonym)))
	    (warn "Type ~A does not seem to be a synonym for ~A" synonym heap-type)))
	(if (> (list-length (static-heap-type-synonyms heap)) 1)
	    (format t "  ~A ~A:~%" heap-type
		    (remove heap-type (static-heap-type-synonyms heap) :test #'equal))
	    (format t "  ~A:~%" heap-type))
	(unless (and (listp arrays-by-parent)
		     (evenp (list-length arrays-by-parent)))
	  (warn "arrays-by-parent ~A is not a plist" arrays-by-parent))
	(loop for the-plist = parent-plist then (cddr the-plist) until (null the-plist)
	      for the-parent = (car the-plist)
	      for sub-arrays = (hlist-list (cadr the-plist))
	      do
	      (unless (hlist-p (cadr the-plist))
		(warn "Improper hlist in arrays-by-parent: ~A" (cadr the-plist)))
	      (unless (static-array-p the-parent)
		(warn "Base-vector ~A is not a static array" the-parent))
	      (unless (eq (array-element-type the-parent) (static-heap-type heap))
		(warn "Base vector ~A is of incorrect type" the-parent))	      
	      (format t "    Base-vector: ~A~%" the-parent)
	      (loop for array in sub-arrays
		    for array-parent = (parent-array array)
		    for prev-end = nil then array-end
		    for array-start = (displaced-start array)
		    for array-end = (+ array-start (array-total-size array))
		    do
		    (unless (eq array-parent the-parent)
		      (warn "Sub-array ~A is not a child of base-vector ~A!"
			    array the-parent))
		    (when (and prev-end (> prev-end array-start))
		      (warn "parent-list arrays are overlapping or out of order"))
		    (push array arrays-by-parent)
		    (format t "      ~D to ~D: ~A~%"
			    array-start (+ array-start (array-total-size array)) array)))
	(unless (hlist-p size-hlist)
	  (warn "arrays-by-size is not a proper hlist: ~A" size-hlist))	
	(loop for array in arrays-by-size
	      for prev-size = nil then array-size
	      for array-size = (array-total-size array)
	      do (when (and prev-size (> prev-size array-size))
		   (warn "size-list arrays are out of order")))
	(when (setq diff (set-difference arrays-by-parent arrays-by-size))
	  (warn "~A are on the parent-list but not on the size-list!" diff))
	(when (setq diff (set-difference arrays-by-size arrays-by-parent))
	  (warn "~A are on the size-list but not on the parent-list!" diff))
	finally (format t "~%")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Functions for accessing and (destructively) modifying the heaps.

;;; Find heap of this type.  If none exists, create one, or give an
;;; error depending on if-not-found
(defun find-heap-of-type (type &key if-not-found)
  (let* ((test #'(lambda (item list) (member item list :test #'equal)))
	 (heap (find type *static-heaps* :test test :key 'static-heap-type-synonyms))
	 actual-type)
    (when (null heap)
      (setq actual-type (array-element-type (make-array 0 :element-type type)))
      ;; check for heap of aliased type.
      (setq heap (find actual-type *static-heaps* :test test
		       :key 'static-heap-type-synonyms))
      (cond (heap (push type (static-heap-type-synonyms heap)))
	    ((eq if-not-found :error)
	     (error "Can't find a static memory heap of type ~A" type))
	    ((eq if-not-found :warn)
	     (warn "Can't find a static memory heap of type ~A"type))
	    ((eq if-not-found :create)
	     (setq heap (make-static-heap :type actual-type
					  :type-synonyms
					  (if (equal type actual-type)
					      (list actual-type)
					      (list type actual-type))))
	     (setq *static-heaps* (cons heap *static-heaps*)))))
    heap))

;;; Return a free array and the index to the next word boundary, which
;;; is larger than size.  If none, then give a cerror!
(defun find-free-array (heap minsize)
  (vom:debug "[find-free-array] size: ~d " minsize)
  (loop with type = (static-heap-type heap)
	for array in (hlist-list (static-heap-free-arrays-by-size heap))
	for array-size = (array-total-size array)
	for index = (displaced-start array)
	for offset = (- (obvius::next-word-boundary index type) index)
     do (when (>= (- array-size offset) minsize)
	  (vom:debug "[find-free-array] found free array")
	  (return (values array offset)))
     finally (let ((expansion (max *heap-growth-rate* minsize)))
	       (vom:debug "[find-free-array] no free array -> try to EXPAND ")
		  (when (not *auto-expand-heap*)
		    (cerror "Expand the ~A static memory heap~%"
			    "Not enough static memory space of type ~A"
			    type)
		    ;; *** should read in heap-expansion here!
		    (setq expansion (max *heap-growth-rate* minsize)))
		  (setq array (expand-heap heap expansion t))
		  (return (values array 0)))))

;;; Hairy but efficient.  Pushes the array into both free-array lists,
;;; merging it with neighbors if consolidate-p is non-nil.  Assumes
;;; that arr is a sub-array of one of the parent arrays of heap.
(defun insert-free-array (arr heap &key consolidate-p)
  (vom:debug "[insert-free-array]")
  (let* ((parent (parent-array arr))
	 (type (array-element-type arr))
	 (arrays-by-parent (getf (static-heap-free-arrays-by-parent heap) parent))
	 (arrays-by-size (static-heap-free-arrays-by-size heap)))
    (unless arrays-by-parent (error "~A was not allocated from ~A." arr heap))
    (if consolidate-p
	(loop with arr-start = (displaced-start arr)
	      with arr-end = (+ arr-start (array-total-size arr))
	      with the-list = (find-cadr-sublist arr-start arrays-by-parent
						 :key 'displaced-end :test #'<=)
	      for cdr-list = (cdr the-list)
	      until (null cdr-list)	;reached the end
	      for cadr-arr = (car cdr-list)
	      for cadr-start = (displaced-start cadr-arr)
	      until (> cadr-start arr-end) ;no more overlapping arrays
	      for cadr-end = (+ cadr-start (array-total-size cadr-arr))
	      do
	      ;; make merged array:
	      (setq arr-start (min cadr-start arr-start))
	      (setq arr-end (max cadr-end arr-end))
	      (setq arr (make-array (- arr-end arr-start)
				    :element-type type
				    :displaced-to parent
				    :displaced-index-offset arr-start))
	      ;; remove overlapping array from both free lists:
	      (rplacd the-list (cdr cdr-list))
	      (delete-from-hlist cadr-arr arrays-by-size)
	      finally
	      (progn			;add merged array to both lists
		(rplacd the-list (cons arr cdr-list))
		(insert-into-hlist arr arrays-by-size :key #'array-total-size)))
	(progn
	  (insert-into-hlist arr arrays-by-parent :key 'displaced-start)
	  (insert-into-hlist arr arrays-by-size :key #'array-total-size)))
    t))

;;; Delete arr from free lists in heap.  Replace with
;;; replacement-arrays, which should be (ordered) sub-arrays of arr.
;;; NOTE: Error if array is not a free-array of heap!
(defun delete-free-array (arr heap &key replacement-subarrays)
  ;;(declare (optimize (debug 3)))
  (vom:debug "[delete-free-array] size ~d replacement-length ~d " (array-total-size arr) (length replacement-subarrays))
  (let* ((parent (undisplaced-parent arr))
	 (arrays-by-parent (getf (static-heap-free-arrays-by-parent heap) parent))
	 (arrays-by-size (static-heap-free-arrays-by-size heap))
	 found1 found2)
    (setq found1 (delete-from-hlist arr arrays-by-size))
    ;; Do arrays-by-parent in-line for efficiency (see code in delete-from-hlist).
    (setq arrays-by-parent (find-cadr-sublist arr arrays-by-parent :test #'eq))
    (vom:debug "[delete-free-array] arrays-by-parent length ~d" (length arrays-by-parent))
    (setq found2 (cdr arrays-by-parent)) ;found2 is nil or first elt is arr
    (vom:debug "[delete-free-array] found1: ~a found2:  ~a" (if found1 T) (if found2 T ))
    ;;dangerous for large arrays
    ;;(vom:debug "[delete-free-array] found2: ~a " found2)
;;    (break)
    (if (and found1 found2)
	(progn
	  (rplacd arrays-by-parent
		  (append replacement-subarrays (delete arr found2 :test #'eq)))
	  (dolist (sub-array replacement-subarrays)
	    (insert-into-hlist sub-array arrays-by-size :key #'array-total-size)))
	(error "~A not found in ~A of ~A"
	       arr
	       (cond (found1 "free-arrays-by-size")
		     (found2 "free-arrays-by-parent")
		     (t "either free array list"))
	       heap)))
  arr)

;;; Called by the garbage-collector with a list of arrays to be
;;; preserved.  Very scary, hairy function!
(defun rebuild-heaps-from-allocated-arrays (array-list &key verbose)
  (let (original-heap-sizes parent-plist)
    (dolist (heap *static-heaps*)
      (setf (getf original-heap-sizes heap) (static-heap-free-size heap)))
    ;; Sort array-list into a plist keyed on parent arrays:
    (loop for arr in array-list
	  for parent = (parent-array arr)
	  do
	  (setf (getf parent-plist parent) (cons arr (getf parent-plist parent))))
    ;; Set free-array lists of heaps to contain only the base arrays:
    (loop for heap in *static-heaps*
	  for base-arrays = (sort (static-heap-base-arrays heap) #'<
				  :key #'array-total-size)
	  do
	  (setf (static-heap-free-arrays-by-size heap) (apply 'hlist base-arrays))
	  (setf (static-heap-free-arrays-by-parent heap) nil)
	  (dolist (base-array base-arrays)
	    (setf (getf (static-heap-free-arrays-by-parent heap) base-array)
		  (hlist base-array))))
    ;; Remove allocated arrays from the free-array lists of their heaps:
    (loop for sub-list = parent-plist then (cddr sub-list) until (null sub-list)
	  for base-array = (car sub-list)
	  for allocated-arrays = (cadr sub-list)
	  for type = (array-element-type base-array)
	  for heap = (find-heap-of-type type :if-not-found :create)
	  for free-hlist = (getf (static-heap-free-arrays-by-parent heap) base-array)
	  do
	  (if free-hlist
	      (loop for alloc-array in allocated-arrays
		    for alloc-start = (displaced-start alloc-array)
		    for alloc-end = (+ alloc-start (array-total-size alloc-array))
		    for free-parent = (find alloc-start (hlist-list free-hlist)
					    :key 'displaced-end :test #'<=)
		    for free-start = (displaced-start free-parent)
		    for free-end = (+ free-start (array-total-size free-parent))
		    for rem-arrays =
		    (nconc (when (> alloc-start free-start)
			     (list (make-array (- alloc-start free-start)
					       :element-type type
					       :displaced-to free-parent)))
			   (cond ((> free-end alloc-end)
				  (list (make-array (- free-end alloc-end)
						    :element-type type
						    :displaced-to free-parent
						    :displaced-index-offset
						    (- alloc-end free-start))))
				 ((< free-end alloc-end)
				  (warn "Allocated array ~A overlaps another" alloc-array)
				  nil)
				 (t nil)))
		    do (delete-free-array free-parent heap
					  :replacement-subarrays rem-arrays))
	      (warn "These arrays are not allocated static arrays: ~A"
		    allocated-arrays)))
    (when verbose
      (loop for sub-list = original-heap-sizes
	    then (cddr sub-list) until (null sub-list)
	    for heap = (car sub-list)
	    for orig-free = (cadr sub-list)
	    do (format t "~A elements recovered for ~A heap~%"
		       (- (static-heap-free-size heap) orig-free)
		       (static-heap-type heap))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Auxilliary functions

;;; In Lucid 4.0, all foreign-compatible arrays must begin on even
;;; word boundaries (8 bytes).  This function rounds val up to the
;;; next boundary, assuming it corresponds to the given data type.
(defun next-word-boundary (val type)
  (let ((sizeof (sizeof type)))
    (* (/ 8 sizeof)  (ceiling (* sizeof val) 8))))

;;; The following three functions return parent array and
;;; index-offset, or array and zero if array is not displaced.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; [tho] changed 2016-08-23
;;; let parent array return the undisplaced parent
;;; i think this is what the original lucid implementation did
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parent-array (arr)
  (multiple-value-bind (p i) (displaced-array-p arr)
    (declare (ignore i))
    (or p arr)))

;;[tho] 2016
;;finds the parent array which is not a displaced array
(defun undisplaced-parent (arr)
  (multiple-value-bind (p i) (array-displacement arr)
    (declare (ignore i))
    (if p
	(undisplaced-parent p)
	arr)))

(defun displaced-start (arr)
  (multiple-value-bind (p i) (displaced-array-p arr)
    (if p i 0)))

;;displacement relative to undisplaced parent array
(defun total-displacement (arr)
  (multiple-value-bind (p i) (array-displacement arr)
    (if p (+ i (total-displacement p)) 0)))

;;; Gives an error if arg is not an array!
(defun displaced-end (arr)
  (+ (array-total-size arr) (displaced-start arr)))


;;; redefinition of displaced-array-p
;;; [tho] 2016-08-23
(defun displaced-array-p (arr)
  (values (undisplaced-parent arr) (total-displacement arr)))



;;; Define an object called an hlist (header list) which is an ordered
;;; list whose car is the symbol :head.  This structure is easier to
;;; destructively insert and delete from than normal lists.
(defun hlist (&rest list-elements)
  (cons :head list-elements))

(defun hlist-p (thing)
  (and (consp thing)
       (eq (car thing) :head)
       (listp (cdr thing))))

(defun hlist-list (hlist)
  (cdr hlist))

;;; Destructively insert the-item into the list just before the first
;;; element for which the test returns non-nil, or at the end of the
;;; list.  NOTE that key is applied to the-item (a bit different from
;;; the Common Lisp functions).  Ignore the first item in the-list.
(defun insert-into-hlist
    (the-item the-list &key (key #'identity) (test #'<=))
  (loop with item-val = (funcall key the-item)
	for sub-list = the-list then cdr-list
	for cdr-list on (cdr the-list)
	until (null cdr-list)
	for cadr-item = (car cdr-list)
	until (funcall test item-val (funcall key cadr-item))
	finally (rplacd sub-list (cons the-item cdr-list)))
  the-list)

;;; Modified Common Lisp delete function.  Returns t or nil indicating
;;; if item was found.  Ignores the first item.
(defun delete-from-hlist
    (the-item the-list &key (test #'eq) (key #'identity))
  (let* ((sub-list (find-cadr-sublist the-item the-list
				      :test test :key key))
	 (cdr-list (cdr sub-list)))
    (when cdr-list
      (rplacd sub-list (delete the-item cdr-list :test test))
      t)))

;;; Return first sublist of the-list with cadr matching the item, or
;;; (last the-list) if no match is found.
(defun find-cadr-sublist
    (the-item the-list &key (test #'eq) (key #'identity))
  (loop for sub-list = the-list then cdr-list
	for cdr-list on (cdr the-list)
	for cadr-val = (funcall key (car cdr-list))
	until (funcall test the-item cadr-val)
	finally (return sub-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Garbage Collection.  These functions are specific to OBVIUS,
;;;; whereas the functions above are essentially independent of OBVIUS.

;;; Exported version of ogc that prints useful information.
(defmacro scrounge! ()
  '(ogc :verbose t))

;;; This is the OBVIUS garbage collector.  It preserves all viewables
;;; that have pictures, or symbol names that or in the current
;;; package's symbol table, or are connected to such a viewable by
;;; superiors-of or inferiors-of pointers.  Can be useful if you
;;; forget to free temporary images, etc...  NOTE: this screws up if
;;; you change packages.  BEWARE: If you have viewables which are not
;;; currently displayed, are not bound to symbols, and are not
;;; superiors or inferiors of such viewables, you will lose them!
(defun ogc (&key (verbose t))
  (declare (special *protected-viewables*))
  (with-status-message "Running OBVIUS garbage collector"
    (let (vbl-name-plist array-list)
      (with-scheduling-inhibited
	  (multiple-value-bind (vbl-list pic-list) (pictured-viewables)
	    ;; Append all viewables with symbol names.
	    (dolist (vbl (named-viewables))
	      (pushnew vbl vbl-list)
	      (setf (getf vbl-name-plist (class-of vbl))
		    (cons (name vbl) (getf vbl-name-plist (class-of vbl)))))
	    ;; Append viewables in *protected-viewables* list:
	    (setq vbl-list (append *protected-viewables* vbl-list))
	    ;; Recursively add superiors and inferiors to vbl-list:
	    (add-relatives-to vbl-list)
	    ;; Collect static arrays:
	    (setq array-list
		  (nconc (loop for vbl in vbl-list append (static-arrays-of vbl))
			 (loop for pic in pic-list append (static-arrays-of pic))))
	    (setq array-list (delete-duplicates array-list :test #'eq))
	    (rebuild-heaps-from-allocated-arrays array-list :verbose verbose)))
      (when (and verbose vbl-name-plist)
	(heap-status)
	(format t "Named viewables (preserved):~%")
	(loop for sub-list = vbl-name-plist then (cddr sub-list) until (null sub-list)
	      for class = (class-name (car sub-list))
	      for names = (cadr sub-list)
	      do
	      (format t "  ~a:  ~{~<~%~1:;~s~>~^  ~}~%" class names)
	      finally (format t "~%")))
      nil)))

;;; Massive cleanup: destroys all existing viewables.  Reclaims all
;;; memory.  Use destroy-viewables to 1) include all inferiors of
;;; preserved-viewables, 2) avoid destruction-of-inferior errors
(defun purge! (&key preserved-viewables (suppress-warning preserved-viewables))
  (unless suppress-warning
    (cerror "Destroy all viewables, reclaiming all memory."
	    "Are you sure you want to destroy all existing viewables?"))
  ;; Destroy all non-protected viewables
  (let* ((*protected-viewables* (append preserved-viewables
					*protected-viewables*))
	 (all-vbls (all-viewables)))
    (declare (special *protected-viewables*))
    (setq all-vbls (sort-by-superiors! all-vbls))
    (dolist (vbl all-vbls) 
      (unless (or (member vbl *protected-viewables*) ;check if vbl is protected
		  (superiors-of vbl))	;or if it has an undestroyed superior
	(destroy vbl :silent t :suppress-error t))))
  ;; Rebuild heaps, preserving everything that is left:
  (let* ((vbls (all-viewables))
	 (pics (loop for vbl in vbls append (pictures-of vbl)))
	 array-list)
    (setq array-list
	  (nconc (loop for vbl in vbls append (static-arrays-of vbl))
		 (loop for pic in pics append (static-arrays-of pic))))
    (rebuild-heaps-from-allocated-arrays array-list :verbose t)))

#|
;;; old version 7/93 DH
(defun purge! (&key preserved-viewables (suppress-warnings preserved-viewables))
  (unless suppress-warnings
    (cerror "Destroy all viewables, reclaiming all memory."
	    "Are you sure you want to destroy all existing viewables?"))
  ;; Destroy all non-protected viewables
  (let* ((*protected-viewables* (append preserved-viewables
					*protected-viewables*))
	 (all-vbls (all-viewables)))
    (declare (special *protected-viewables*))
    (setq all-vbls (sort-by-superiors! all-vbls))
    (dolist (vbl all-vbls) 
      (unless (or (member vbl *protected-viewables*) ;check if vbl is protected
		  (superiors-of vbl))	;or if it has an undestroyed superior
	(destroy vbl :silent t))))
  ;; Rebuild heaps, preserving everything that is left:
  (let* ((vbls (all-viewables))
	 (pics (loop for vbl in vbls append (pictures-of vbl)))
	 array-list)
    (setq array-list
	  (nconc (loop for vbl in vbls append (static-arrays-of vbl))
		 (loop for pic in pics append (static-arrays-of pic))))
    (rebuild-heaps-from-allocated-arrays array-list :verbose t)))
|#

;;; Return all (non-orphaned) viewables.  This function is exported.
;;; It must be kept compatible with ogc!!
(defun all-viewables ()
  (declare (special *protected-viewables*))
  (let (vbl-list)
    (setq vbl-list (nunion (pictured-viewables) (named-viewables)))
    (setq vbl-list (append *protected-viewables* vbl-list))
    (add-relatives-to vbl-list)
    vbl-list))

;;; Find all symbols in the 'user package and the *package* package
;;; that are bound to viewables.
(defun named-viewables ()	
  ;; (declare (special @ @@ @@@ cl-user:*** cl-user:** cl-user:* cl-user:/// cl-user://
  ;; 		    cl-user:/ cl-user:+++ cl-user:++ cl-user:+ cl-user:-))
  (let ((ignored-symbols '(@ @@ @@@ cl-user::*** cl-user::** cl-user::* cl-user::/// cl-user:://
			   cl-user::/ cl-user::+++ cl-user::++ cl-user::+ cl-user::-))
	vbl-list sym-list vbl)
    ;; Cons named viewables in *package* and 'user:
    (dolist (pkg (adjoin *package* (list (find-package 'user))))
      (do-symbols (sym pkg)
	(when (and (boundp sym)
		   (not (member sym ignored-symbols))
		   (named-viewable-p (setq vbl (symbol-value sym))))
	  (setq sym-list (pushnew sym sym-list))
	  (setq vbl-list (pushnew vbl vbl-list)))))
    (values vbl-list sym-list)))

;;; Return all viewables with pictures.
(defun pictured-viewables ()	
  (declare (special *screen-list*))
  (let (vbl-list pic-list)
    (dolist (screen *screen-list*)
      (dolist (pane (pane-list screen))
	(dolist (pic (picture-stack pane))
	  (pushnew pic pic-list)
	  (pushnew (viewable pic) vbl-list)
	  )))
    (values vbl-list pic-list)))


;;; This will recursively (and destructively) add the superiors and
;;; inferiors of the viewables to the list.  The trick is that it adds
;;; the superiors and inferiors to the cdr of the list.  Thus the
;;; outer loop will go over the newly added inferiors and superiors.
;;; Avoids infinite loops since we always check the whole list for
;;; membership before adding a new element!
(defun add-relatives-to (vbl-list)
  (loop for sub-list = vbl-list then (cdr sub-list) until (null sub-list)
	for vbl = (car sub-list)
	do
	(dolist (sup (superiors-of vbl))
	  (unless (member sup vbl-list)
	    (rplacd sub-list (cons sup (cdr sub-list)))))
	(dolist (inf (inferiors-of vbl))
	  (unless (member inf vbl-list)
	    (rplacd sub-list (cons inf (cdr sub-list))))))
  vbl-list)

#| Testing:
(compile-file "~/obvius/memory")
(load "~/obvius/memory")
(in-package :obvius)

(defvar foo nil)
(defvar foo-type '(signed-byte 8))
(defun print-foo ()
  (loop initially (format t "~%Foo:~%")
	for i from 0
	for arr in foo
	do
	(format t "~A:  ~A to ~A:  ~A~%"
		i (displaced-start arr) (displaced-end arr) arr)))

(expand-heap foo-type 200)
(obvius::run-heap-diagnostics)

(let* ((dims 11)
       (arr (allocate-array dims :element-type foo-type)))
  (setq foo (append foo (list arr)))
  (print-foo)
  (obvius::run-heap-diagnostics))

;;; Free one:
(let ((i 1))
  (free-array (nth i foo))
  (setq foo (delete (nth i foo) foo))
  (print-foo)
  (obvius::run-heap-diagnostics))

;;; Free all:
(loop for arr in foo
      do (free-array arr)
      finally (progn
		(setq foo nil)
		(obvius::run-heap-diagnostics)))
|#



;;; Local Variables:
;;; buffer-read-only: t 
;;; End:


