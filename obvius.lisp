;;;; obvius01.lisp

(in-package #:obvius)


;;; "obvius01" goes here. Hacks and glory await!

;;for testing and debugging
(declaim (optimize (debug 3)))



;;; load the foreign library
;; don't ship code with that
;; (cffi:load-foreign-library (merge-pathnames #P"emacs/clisp/obvius/bin/obv_gl.so" (user-homedir-pathname)))

(cffi:load-foreign-library (merge-pathnames #p"bin/obv_gl.so" *obvius-directory-path*))

;;set up the logging level

(vom:config t :info)

(defun float (val &optional (other 1.0f0))
  (cl:float val other))

;;returns a double float
(defun float* (val &optional (other 1.0d0))
  (cl:float val other))

(defun dfloat (val)
  (the double-float (cl:float val 1d0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  exporting to png
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lerp (x x1 f1 x2 f2)
  (/ (+ (* f2 (- x x1))
	(* f1 (- x2 x)))
     (- x2 x1)))

(defmacro def-typed-lerp (fname type)
  `(progn
    (declaim (inline ,fname))
    (defun ,fname (x x1 f1 x2 f2)
      (declare (type ,type x x1 f1 x2 f2)
	       (optimize (speed 3) (space 0) (safety 0) (debug 0)))
      (/ (+ (* f2 (- x x1))
	    (* f1 (- x2 x)))
	 (- x2 x1)))))

(def-typed-lerp dlerp double-float)
(def-typed-lerp flerp single-float)
(def-typed-lerp lerp  number)

(declaim (inline clamp-to-8bit))
(defun clamp-to-8bit (val)
  (min (max 0 val) 255))

;;;this is a few bytes shorter on sbcl
(declaim (inline clamp-0-255))
(defun clamp-0-255 (val)
  (declare (type (signed-byte 32) val)
	   (optimize (speed 3) (debug 0) (space 0) (safety 0)))
  (if (< val 0) 0
      (if (> val 255) 255
	  val)))
	 

(defmacro def-typed-8bit-lerp (fname type &optional (low 0) (high 255))
  (assert (typep low '(signed-byte 32)))
  (assert (typep high '(signed-byte 32)))
  `(progn
     (declaim (inline ,fname))
       (defun ,fname (x x1 x2)
	 (declare (type ,type x x1 x2)
		  (optimize (speed 3) (safety 0) (debug 0)))
	 (symbol-macrolet ((low (coerce ,low ',type))
			   (high (coerce ,high ',type)))
	   (clamp-0-255 (round (/ (+ (* high (- x x1))
				     (* low (- x2 x)))
				  (- x2 x1))))))))
			   


(def-typed-8bit-lerp dlerp->8bit double-float)
(def-typed-8bit-lerp flerp->8bit single-float)
(def-typed-8bit-lerp lerp->8bit number)

;;for integers we can spare the / as round has an optional divisor
(defmacro def-typed-8bit-int-lerp (fname type &optional (low 0) (high 255))
  (assert (typep low '(signed-byte 32)))
  (assert (typep high '(signed-byte 32)))
  `(progn
     (declaim (inline ,fname))
       (defun ,fname (x x1 x2)
	 (declare (type ,type x x1 x2)
		  (optimize (speed 3) (safety 0) (debug 0)))
	 (symbol-macrolet ((low (coerce ,low ',type))
			   (high (coerce ,high ',type)))
	   (clamp-0-255 (round (+ (* high (- x x1))
				  (* low (- x2 x)))
			       (- x2 x1)))))))

(def-typed-8bit-int-lerp ilerp->8bit (signed-byte 32) 0 255)
(def-typed-8bit-int-lerp ulerp->8bit (unsigned-byte 32) 0 255)



;; single-array-op
;; operation an a single array
;; the result will be saved in the result array
;; var will be suucessivly bound to array elements 
(defmacro single-array-op ((var array result) &body body)
  (let ((a1 (gensym))
	(res (gensym)))
    `(let ((,a1 ,array)
	   (,res ,result))
       (dotimes (i (array-total-size ,res) ,res)
	 (setf (row-major-aref ,res i)
	       (let ((,var (row-major-aref ,a1 i)))
		 ,@body))))))

(defun array-min-max (array)
  (let ((min (row-major-aref array 0))
	(max (row-major-aref array 0)))
    (dotimes (n (array-total-size array ))
      (let ((el (row-major-aref array n)))
	(or (and (< el min)
		 (setf min el))
	    (and (> el max)
		 (setf max el)))))
    (values min max)))


;;take out data-min data-max as optional -> make them required and make the optional part in to-png
(defun array-to-8bit-vector (array scale-min scale-max)
  (if (typep (array-element-type array) '(unsigned-byte 8))
      (if (=  1 (array-rank array))
	  array  ;; nothing to do as it is already a 8-bit vector
	  (make-array (array-total-size array) ;;otherwise make a vector from n-dim array
		      :element-type '(unsigned-byte 8)
		      :displaced-to array))
      (flet ((to-array-data-type (val)
	       (coerce val (array-element-type array))))
	(let ((min (to-array-data-type scale-min))
	      (max (to-array-data-type scale-max))
	      (result (make-array (array-total-size array) :element-type '(unsigned-byte 8))))
	  (macrolet ((array-op-with (func)
		       `(dotimes (i (array-total-size array) result)
			  (setf (row-major-aref result i) (,func (row-major-aref array i) min max)))))
	    (typecase (array-element-type array)
	      (double-float (array-op-with dlerp->8bit))
	      (single-float (array-op-with flerp->8bit))
	      (T (cond ((subtypep (array-element-type array) '(unsigned-byte 32)) (array-op-with ulerp->8bit))
		       ((subtypep (array-element-type array) '(signed-byte 32)) (array-op-with ilerp->8bit))
		       ((subtypep (array-element-type array) 'number)  (array-op-with lerp->8bit))
		       (T (error "cannot convert array of type ~a to 8bit" (array-element-type array)))))))))))


(defun gray-array-to-RGB8-vector (array scale-min scale-max)
  (flet ((to-array-data-type (val)
	   (coerce val (array-element-type array))))
    (let ((min (to-array-data-type scale-min))
	  (max (to-array-data-type scale-max))
	  (result (make-array (* 3 (array-total-size array)) :element-type '(unsigned-byte 8))))
      (macrolet ((array-op-with (func)
		   `(loop for i fixnum from 0 below (array-total-size array)
			  for out fixnum from 0 below (array-total-size result) by 3 do
			 (let ((val (,func (row-major-aref array i) min max)))
			   (setf (row-major-aref result out) val
				 (row-major-aref result (+ 1 out)) val
				 (row-major-aref result (+ 2 out)) val))
			 finally (return result))))
	(typecase (array-element-type array)
	  (double-float (array-op-with dlerp->8bit))
	  (single-float (array-op-with flerp->8bit))
	  (T (cond ((subtypep (array-element-type array) '(unsigned-byte 32)) (array-op-with ulerp->8bit))
		   ((subtypep (array-element-type array) '(signed-byte 32)) (array-op-with ilerp->8bit))
		   ((subtypep (array-element-type array) 'number)  (array-op-with lerp->8bit))
		   (T (error "cannot convert array of type ~a to 8bit RGB" (array-element-type array))))))))))




(defun write-gray-array-to-png-file (array filename scale-min scale-max &key RGB)
(let ((png (if RGB
	       (make-instance 'zpng:png :color-type :truecolor
			      :width (array-dimension array 1)  ;;columns
			      :height (array-dimension array 0) ;;rows
			      :image-data (gray-array-to-rgb8-vector array scale-min scale-max))
	       (make-instance 'zpng:png :color-type :grayscale
			      :width (array-dimension array 1)  ;;columns
			      :height (array-dimension array 0) ;;rows
			      :image-data (array-to-8bit-vector array scale-min scale-max)))))
      (zpng:write-png png filename)))


(defun to-png (array &key ((:-> filename) (in-tempdir (timestamped-temp-file "_XXXX_XXXX.png"))) (scale-max 255) (scale-min 0) RGB)
  (write-gray-array-to-png-file array filename scale-min scale-max :RGB RGB)
  filename)


#+SBCL
(defun simple-array-vector (array)
  (declare (simple-array array))
  (if (sb-kernel:array-header-p array)
      (sb-kernel:%array-data-vector array)
       array))

#+SBCL
(defun foreign-pointer-to-array (array)
  (sb-sys:vector-sap (simple-array-vector array)))

#+SBCL
(defmacro with-pointer-to ((pointer-var array) &body body)
  (let ((a (gensym)))
    `(let ((,a (sb-sys::array-storage-vector ,array)))
       (sb-sys:with-pinned-objects (,a)
	 (let ((,pointer-var (sb-sys:vector-sap ,a)))
	   ,@body)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; random file names
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *alphanums* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")


(defun make-random-file-name (pattern &optional (random-char-func (let ((len (length *alphanums*)))
								    (lambda ()
								      (elt *alphanums* (random len))))))
  "returns a temp-filename according to pattern, in which all capitalX will be replaced by
   random characters. The optional random-.char-function should be a function which takes norarguements and
   returns a random char"
  (map 'string #'(lambda (c)
		   (if (char= c #\X)
		       (funcall random-char-func)
		       c))
       pattern))
		


(defun timestamped-temp-file (&optional (pattern "_XXXX_XXXX"))
  "returns a temp file name with timestamp of form year-month-day-hours-minutes-seconds
   in front of pattern. all capital X in pattrn will be replaced by random characters"
  (multiple-value-bind (seconds minutes hours day month year day-of-week daylight-saving time-zone)
	   (get-decoded-time)
    (declare (ignore day-of-week daylight-saving time-zone))
     (concatenate 'string
		  (format nil "~4,'0d_~2,'0d_~2,'0d_~2,'0d_~2,'0d_~2,'0d" year month day hours minutes seconds)
		  (make-random-file-name pattern)
		  )))


(defun make-test-images ()
  (let ((*auto-expand-heap* T))
    (declare (special *auto-expand-heap*))
    (load-image (translate-logical-pathname "obv:images;einstein"))
    (load-image (translate-logical-pathname "obv:images;reagan"))
    (to-png (data einstein) :-> (in-tempdir "einstein-GRAY.png") )
    (to-png (data reagan) :-> (in-tempdir "reagan-GRAY.png") )
  ))

;;makes an array wiht the same element-type and size of dim
;;optional  offset
(defun displace-to (base-array dim &key displaced-index-offset )
  (if displaced-index-offset
      (make-array dim :displaced-to base-array
		      :element-type (array-element-type base-array)
		      :displaced-index-offset displaced-index-offset)
      (make-array dim :displaced-to base-array
		      :element-type (array-element-type base-array))))
