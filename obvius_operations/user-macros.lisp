;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: user-macros.lisp
;;;  Author: Simoncelli
;;;  Description: Macros to emulate the functions of Common Lisp.
;;;  Creation Date: March, 1990
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package obvius)
(export '(+. -. *. /.
	  sqr. sqrt.  expt. exp. log.  abs.  mod.
	  sin. cos. min. max. average.
	  realpart. imagpart. phase.
	  incf. decf. fill.
	  append. subseq. mapcar.
	  round. truncate. floor.))

;;;; Convention here is that functions ending with a "." should behave
;;;; like the corresponding Common Lisp function, applying the operation
;;;; to each point in the image (as if in parallel).

;;; *** make this take multiple arguments (like the Common Lisp function).
(defmacro append. (seq1 seq2 &rest keys &key ->)
  `(append-sequence ,seq1 ,seq2 ,@keys))

(defmacro subseq. (seq start-frame &optional end-frame &rest keys &key ->)
  `(sub-sequence ,seq ,start-frame ,end-frame ,@keys))

(defmacro +. (viewable1 viewable2 &rest viewables-and-keywords)
  `(viewable-reduce 'add ,viewable1 ,viewable2 ,@viewables-and-keywords))

(defmacro *. (viewable1 viewable2 &rest viewables-and-keywords)
  `(viewable-reduce 'mul ,viewable1 ,viewable2 ,@viewables-and-keywords))

;;; For single arg, negate.  For multiple args, subtract all from first.
(defmacro -. (viewable &rest viewables-and-keywords)
  (let ((first-keyword (or (position-if #'keywordp viewables-and-keywords)
			   (length viewables-and-keywords))))
    (cond ((= first-keyword 0)		;negate
	   `(sub 0.0 ,viewable ,@viewables-and-keywords))
	  ((= first-keyword 1)
	   `(sub ,viewable ,@viewables-and-keywords))
	  (t
	   `(viewable-reduce 'sub ,viewable ,@viewables-and-keywords)))))

(defmacro /. (viewable &rest viewables-and-keywords)
  (let ((first-keyword (or (position-if #'keywordp viewables-and-keywords)
			   (length viewables-and-keywords))))
    (cond ((= first-keyword 0)		;negate
	   `(div 1.0 ,viewable ,@viewables-and-keywords))
	  ((= first-keyword 1)
	   `(div ,viewable ,@viewables-and-keywords))
	  (t
	   `(viewable-reduce 'div ,viewable ,@viewables-and-keywords)))))

;;; Compute minimum pixel as a unary operation, or find min at each
;;; pixel if N-ary.
(defmacro min. (viewable &rest viewables-and-keywords)
  (let ((first-keyword (or (position-if #'keywordp viewables-and-keywords)
			   (length viewables-and-keywords))))
    (if (= first-keyword 0)           ;if unary, find minimum of all pixels
	`(minimum ,viewable ,@viewables-and-keywords)
	`(viewable-reduce 'point-minimum ,viewable ,@viewables-and-keywords))))

(defmacro max. (viewable &rest viewables-and-keywords)
  (let ((first-keyword (or (position-if #'keywordp viewables-and-keywords)
			   (length viewables-and-keywords))))
    (if (= first-keyword 0)		;if unary, find maximum of all pixels
	`(maximum ,viewable ,@viewables-and-keywords)
	`(viewable-reduce 'point-maximum ,viewable ,@viewables-and-keywords))))

;;; If unary, this should be a single list of items.
(defun average. (item1-or-list &rest rest-items)
  (let* ((num-args (or (position-if #'keywordp rest-items) (length rest-items))))
    (cond ((= num-args 0)
	   (unless (listp item1-or-list)
	     (error "Arguments should be a single list of items, or the items themselves."))
	   (apply 'average item1-or-list))
	  (t (div (apply 'viewable-reduce 'add item1-or-list rest-items)
		  (1+ num-args))))))

(defmacro sqr. (thing &rest keys &key ->)
  `(square ,thing ,@keys))

(defmacro sqrt. (thing &rest keys &key ->)
  `(square-root ,thing ,@keys))

(defmacro expt. (thing1 thing2 &rest keys &key ->)
  `(power ,thing1 ,thing2 ,@keys))

;;; e^im
(defmethod exp. ((im image) &key ->)
  (with-result ((result ->) im 'exp. im)
    (internal-exp (data im) (data result) (total-size im))
    result))

(defmacro log. (thing &rest keys &key base ->)
  (remf keys :base)
  (let ((res (gensym))
	(b (gensym)))
    `(let ((,res (natural-logarithm ,thing ,@keys))
	   (,b ,base))
      (when ,b (div ,res (log ,b) :-> ,res))
      ,res)))

(defmacro abs. (thing &rest keys &key ->)
  `(abs-value ,thing ,@keys))

(defmacro mod. (thing value &rest keys &key ->)
  (let ((modulus (gensym)))
    `(let ((,modulus ,value))
      (periodic-point-operation ,thing #'identity ,modulus
                                :binsize ,modulus ,@keys))))

(defmacro sin. (thing &rest keys &key ->)
  `(periodic-point-operation ,thing #'sin 2-pi
    :binsize (/ 2-pi (get-default 'discrete-function :size))
    ,@keys))

(defmacro cos. (thing &rest keys &key ->)
  `(periodic-point-operation ,thing #'cos 2-pi
    :binsize (/ 2-pi (get-default 'discrete-function :size))
    ,@keys))

(defmacro realpart. (complex-image &rest keys &key ->)
  `(real-part ,complex-image ,@keys))
  
(defmacro imagpart. (complex-image &rest keys &key ->)
  `(imaginary-part ,complex-image ,@keys))

(defmacro phase. (complex-image &rest keys &key ->)
  `(complex-phase ,complex-image ,@keys))

(defmacro incf. (viewable &optional incr-viewable)
  (let ((vbl (gensym)))
    `(let ((,vbl ,viewable))		;compute once only!
      (add ,vbl ,(or incr-viewable 1.0) :-> ,vbl))))

(defmacro decf. (viewable &optional incr-viewable)
  (let ((vbl (gensym)))
    `(let ((,vbl ,viewable))		;compute once only!
      (sub ,vbl ,(or incr-viewable 1.0) :-> ,vbl))))

;;; Destructively fills image with value **** Make this a macro
;;; calling a method (perhaps set-values!)
(defmacro fill. (thing &optional (value 0))
  `(fill! ,thing ,value))

(defmethod round. ((vbl viewable) &key (divisor 1.0) ->)
  (with-result ((result ->) vbl 'round. vbl :divisor divisor)
    (round. (data vbl) :divisor divisor :-> (data result))
    result))

(defmethod truncate. ((vbl viewable) &key (divisor 1.0) ->)
  (with-result ((result ->) vbl 'truncate. vbl :divisor divisor)
    (truncate. (data vbl) :divisor divisor :-> (data result))
    result))

(defmethod floor. ((vbl viewable) &key (divisor 1.0) ->)
  (with-result ((result ->) vbl 'floor. vbl :divisor divisor)
    (floor. (data vbl) :divisor divisor :-> (data result))
    result))

;;; Very slow if not compiled!
(defmacro mapcar. (fn &rest vbls-and-result)
  (let* ((pos (position :-> vbls-and-result))
	 (vbl-ims (if pos (butlast vbls-and-result pos) vbls-and-result))
	 (res-im (when pos (nth (1+ pos) vbls-and-result)))
	 (vbls (loop for i from 0 below (length vbl-ims) collect (gensym)))
	 (vals (loop for i from 0 below (length vbl-ims) collect (gensym)))
	 (res-val (gensym))
	 (res (gensym)))
    `(let ,(mapcar 'list vbls vbl-ims)
      (with-result ((,res ,res-im)
		    ,(car vbls)
		    'mapcar. ,fn ,@vbls)
	(loop-over-image-pixels
	    ,(cons (list res-val res) (mapcar 'list vals vbls))
	  (setf ,res-val (funcall ,fn ,@vals)))
	,res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tail-recursive application of binary viewable operation to
;;; multiple args, modelled on the Common Lisp function "reduce".
;;; Func should take two viewables and any number of keywords.
(defun viewable-reduce (func vbl1 vbl2 &rest vbls-and-keywords)
  (let* ((first-keyword (position-if #'keywordp vbls-and-keywords))
	 (the-keywords (and first-keyword (nthcdr first-keyword vbls-and-keywords)))
	 (the-vbls (subseq vbls-and-keywords 0 first-keyword))
	 (res-arg (getf the-keywords :->)))
    (loop while (remf the-keywords :->)) ;remove all result args from keyword list.
    ;; If result is one of the latter arguments, cons a temporary result vbl
    (if (and res-arg (find res-arg the-vbls :test 'eq))
	(with-local-viewables ((temp (apply func vbl1 vbl2 the-keywords)))
	  (loop for vbl in the-vbls do
		(apply func temp vbl :-> temp the-keywords)
		finally (return (copy temp :-> res-arg))))
	(loop with res = (if res-arg
			     (apply func vbl1 vbl2 :-> res-arg the-keywords)
			     (apply func vbl1 vbl2 the-keywords))
	      for vbl in the-vbls do
	      (setq res (apply func res vbl :-> res the-keywords))
	      finally (return res)))))

#| 
The following is a list of all of the symbols from the CommonLisp package that might
be made into "." functions in OBVIUS.  A few of them are not actually lisp functions,
but simple OBVIUS functions -- these are in lowercase.

DONE
----
  + - * / MAX MIN
  SQRT sqr ABS
  INCF DECF
  MOD SIN COS 
  IMAGPART PHASE REALPART 
  EXP LOG EXPT 
  average
  FILL APPEND SUBSEQ
  IF
  > < = >= <=
 ROUND TRUNCATE FLOOR
  MAPCAR

TODO ****
---------
1+ 1- [probably not necessary]
TAN ACOS ASIN ATAN SINH COSH TANH ASINH ACOSH ATANH
GCD LCM CEILING REM
RANDOM 

WHEN UNLESS COND [maybe in terms of IF]

ZEROP MINUSP /= PLUSP EVENP ODDP

EVERY SOME NOTANY NOTEVERY

BIT-XOR LOGNOR BIT-ORC1 BOOLE LOGXOR BIT-ORC2 LOGNOT NOT LOGORC2 BIT-AND
LOGAND LOGBITP SIGNUM BIT-IOR NSET-EXCLUSIVE-OR BIT-NOT LOGTEST BIT-EQV
LOGCOUNT BIT-NOR LOGIOR BIT-ANDC2 OR AND BIT-ANDC1 BIT-NAND LOGANDC2

COMPLEX CIS 

POSITION-IF POSITION FIND SEARCH FIND-IF-NOT SUBST-IF-NOT COUNT SUBSTITUTE-IF
COUNT-IF-NOT REMOVE-IF-NOT REPLACE FIND-IF POSITION-IF-NOT
|#

#| Old Versions didn't take multiple args:
(defmacro +. (thing1 thing2 &key ((:-> res)))
  `(add ,thing1 ,thing2 :-> ,res))

(defmacro -. (thing1 thing2 &key ((:-> res)))
  `(sub ,thing1 ,thing2 :-> ,res))

(defmacro *. (thing1 thing2 &key ((:-> res)))
  `(mul ,thing1 ,thing2 :-> ,res))

(defmacro /. (thing1 thing2 &key ((:-> res)))
  `(div ,thing1 ,thing2 :-> ,res))
|#


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
