;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: functions-tutorial.lisp
;;;  Author: Patrick Teo
;;;  Description: LISP functions and macros.
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; We define LISP functions with the DEFUN construct.
;;;
;;; It's syntax is as follows:-
;;;           (defun <function-name> <parameter-list>
;;;              <function-body>)
;;;
;;; The best way to explain functions is to go straight ahead
;;; and write some.

;;; The following is a simple function to compute the factorial
;;; of a number recursively.
(defun factorial-recursive (n)
  (if (= n 0) 1
      (* n (factorial-recursive (1- n)))))

;;; Let's compute 4!.
(factorial-recursive 4)

;;; Note that we could have written this function iteratively as well.


;;; The following is an implementation of (nth).  It returns the nth
;;; element of a list (where the first element in the list corresponds
;;; to the zeroeth element).
(defun our-nth (n l)
  (if (= n 0) (car l)
      (nth (1- n) (cdr l))))

;;; Returns 'three
(our-nth 3 '(zero one two three four five six))


;;;
;;; (flatten-list) takes a possibly nested list and returns
;;; a list with no nesting.
;;;
(defun flatten-list (l)
  (cond ((null l) nil)
	((atom l) (list l))
	(t (append (flatten-list (car l)) (flatten-list (cdr l))))))

;;; Returns '(1 2 3 4 5 6 7 8 9 10)
(flatten-list '((1 2) ((3 4 5) ((6 7 (8)) 9)) 10))
  

;;;
;;; We can also implement a binary tree using lists.  Let each
;;; node/leaf in the tree be represented by a 3 element list.
;;; The first element in the list is the data.  The second and
;;; third elements are the left and right children.
;;;

;;;
;;; The following inserts an element into a binary tree.
;;;
(defun insert-binary-search-tree (elt tree)
  (cond ((null tree) (list elt nil nil))
	((<= elt (first tree))
	 (list (first tree) (insert-binary-search-tree elt (second tree)) (third tree)))
	(t (list (first tree) (second tree) (insert-binary-search-tree elt (third tree))))))

;;; Let's try out the following example.
(setq *my-tree* nil)
(dolist (n '(5 1 2 3 6 4 9 7 8))
  (setq *my-tree* (insert-binary-search-tree n *my-tree*)))

;;; Take a look at *my-tree*.

;;;
;;; In order traversal of the binary tree.
;;;
(defun print-tree-in-order (tree)
  (unless (null tree)
    (print-tree-in-order (second tree))
    (print (first tree))
    (print-tree-in-order (third tree))))

;;; The following does an inorder traversal of *my-tree*.
(print-tree-in-order *my-tree*)




;;; So far, we've encountered functions with only one nested expression in them.
;;; Functions can also take a list of expressions in which case
;;; each expression is evaluated sequentially.  The value
;;; of the function then is the value of the last expression.  Hence,
;;; there is an implicit enclosing block.

;;; This function prints a couple of lines and returns the symbol 'done.
(defun print-stuff ()
  (print "Line 1")
  (print "Line 2")
  (print "Line 3")
  'done)

(print-stuff)


;;; Instead of listing out all the arguments, we can use KEYS.

(defun key-example (n &key (our-key-1 1) (our-key-2 2))
  (print n)
  (print our-key-1)
  (print our-key-2))

(key-example 0)
(key-example 0 :our-key-2 4 :our-key-1 2)

;;; The &key parameter specifies that following it is a list of
;;; keys or (key default-value) pairs.  If no default value is
;;; provided, NIL is assumed to be the default.


;;; We could also write functions that have optional arguments.
(defun optional-example (x &optional y (z "z"))
  (print x)
  (print y)
  (print z))

(optional-example "x")
(optional-example "x" "y")
(optional-example "x" "y" "not z")

;;; The &optional parameter specifies that following is a list
;;; of arguments or (argument default-value) pairs.  Again, if
;;; no default value is provided, NIL is assumed to be the default.


;;; We can also write functions that takes a variable number of arguments.

(defun rest-example (x &rest list-of-args)
  (print x)
  (print list-of-args))

(rest-example "x")
(rest-example "x" "y" "z")

;;; The &rest parameter specifies that following is an
;;; argument (not (argument default-value) pairs) to which
;;; the rest of the arguments following the required argument
;;; are bound as a list.


;;;
;;; Understanding what functions really are (and LAMBDA expressions)
;;;
;;; Each variable can contain two possible bindings: a value-binding
;;; and a function-binding, i.e. a symbol can have a value and
;;; a function bound to it.  Try the following example:-

(defun foo (n) n)
(setq foo 1)

;;; Returns the "value" bound to the symbol foo.  If we had
;;; simply typed foo, we would have got the same result as well.
(symbol-value 'foo)

;;; Returns the "function" bound to the symbol foo.
(symbol-function 'foo)


;;; The "function" bound to the symbol foo is a name-less function
;;; or what is known as a LAMBDA list.  There is quite a bit of theory
;;; on lambda functions; we're not going to talk about them
;;; here but only examine its implementation in LISP.

;;; We can create a lambda list (function) manually.
;;;
;;; Type this in:
;;;      #'(lambda (x) (* x x))
;;;
;;; #' is a sort of function quote operator.

;;; The following is the same as describing the function with a (defun).
;;; Lambda lists also take the &key, &optional and &rest parameters.
;;; We could easily write our own (defun) function based on what we
;;; know about lambda lists.
(setf (symbol-function 'my-sqr) #'(lambda (x) (* x x)))

;;; Returns 9
(my-sqr 3)

;;; Lambda lists are usually used in functions which take functions
;;; as parameters for which we do not wish to define a function explicitly.
;;; For example,

;;; (mapcar) applies the function (my-sqr) to each element of the list
;;; and accumulates the result in another list.
(mapcar 'my-sqr '(1 2 3 4 5))

;;; We could also have done it with lambda lists.
(mapcar #'(lambda (x) (* x x)) '(1 2 3 4 5))


;;;
;;; Funcall AND Apply
;;;
;;; Although functions are not really first class objects in LISP
;;; (they are in Scheme), we can manipulate them farely decently.
;;; Lambda lists are essentially what gets passed around.

;;; FUNCALL applies the function to an arbitrary number of arguments

(funcall '+ 1 2 3 4 5)

;;; this is equivalent to:-
(+ 1 2 3 4 5)

;;; On the other hand, APPLY applies the function to an actual list of
;;; arguments.  This is especially useful if we do not know before hand
;;; how many arguments there are going to be.  So, we can
;;; create a list dynamically and use it as the argument list to the
;;; function.

;;; This is equivalent to the above.
(apply '+ '(1 2 3 4 5))

;;; However, as a convenience (apply) can also take arguments not in the
;;; list.  These arguments have to be placed before the list.

;;; Again, this is also equivalent to the above.
(apply '+ 1 2 '(3 4 5))


;;; Why do we want to manipulate functions?  It is because we can write
;;; very useful general functions that operate on arbitrary data types.

;;; The following is an implementation of (mapcar):-

(defun my-mapcar (func list)
  (if (null list) nil
      (cons (funcall func (car list) ) (cdr list))))

;;; The same example was presented earlier using LISP's (mapcar).
(my-mapcar '(lambda (x) (* x x)) '(1 2 3 4 5))

;;;
;;; An implementation of the member function which allows
;;; arbitrary equivalence tests.
;;;
(defun my-member (elt list &key (test 'equal))
  (cond ((null list) nil)
	((funcall test elt (car list)) list)
	(t (my-member elt (cdr list) :test test))))

(my-member "brown"
	   '("the" "quick" "brown" "fox" "jumps" "over" "the" "lazy" "dog")
	   :test 'string=)


;;;
;;; Macros are very similar to functions syntactically however
;;; semantically they have one useful feature.  While the arguments
;;; to a function are evaluated and then passed on to it, the
;;; arguments to macros are not pre-evaluated.
;;;

;;; For example:- 

(+ (+ 1 2) 3)

;;; Here, (+ 1 2) is evaluated and together with 3, passed to the + function
;;; at the top level.

;;; However, we can do that if the if..then..else construct because we do not
;;; want both the then and else part to be evaluated before the test.

;;; In order to do so, we have to use a macro.

(defmacro my-if (expr then else)
  (if (eval expr)
      `(quote ,then)
      `(quote ,else)))

(my-if (> 1 2) (+ 1 2) (* 3 4))
(my-if (< 1 2) (+ 1 2) (* 3 4))

;;; This macro returns the then-expression without evaluating
;;; it if the test evalues to a non-NIL value and return the
;;; else-expression otherwise.  The returned expression is the
;;; evaluated at the top-level by LISP.

;;; How do macros work then?  First the arguments are passed to
;;; the macro syntactically; hence expr = '(> 1 2), then = '(+ 1 2)
;;; and else = '(* 3 4) literally.  The test was evaluated using
;;; (eval expr) and if the result was true `(quote ,then) was
;;; returned.
;;;
;;; Now what was all that?  The backquote operator is similar to
;;; the regular quote operator. It,  however, allows you to "step
;;; out" of the quote.  The backquote inhibits evaluation while the
;;; comma forces evaluation.  For example, `(quote ,then),
;;; returns a list of (quote <then-expr> where <then-expr> is
;;; the result of evaluating 'then'.
;;;
;;; As a result, the macro returns a list '(quote (+ 1 2)).  This
;;; expression is then evaluated by the LISP interpretor
;;; by (eval '(quote (+ 1 2))) which gives us '(+ 1 2).
;;; If we had not inserted the quote operator in there,
;;; (eval '(+ 1 2)) would have returned 3.
;;;
;;; Also, when designing macros, it is very important to remember
;;; that the returned value will be evaluated by the LISP interpretor.
;;;

;;;
;;; The above macro can also be written as:-
;;;
(defmacro my-if-2 (expr then else)
  `(if ,expr
    (quote ,then)
    (quote ,else)))

(my-if-2 (> 1 2) (+ 1 2) (* 3 4))
(my-if-2 (< 1 2) (+ 1 2) (* 3 4))

;;;
;;; This is slightly different.  The return value of this
;;; macro is not as simple the 'then' or 'else' expression but
;;; is an entire if-then-else expression.   To see the
;;; macro expansion, go to one of the examples and type
;;; C-c <return>.
;;;
;;; The first example expands to:-
;;;
;;; '(if (> 1 2) '(+ 1 2) (* 3 4))
;;;
;;; while the previous macro expands to
;;;
;;; '(* 3 4)
;;;

;;;
;;; Finally, let's examine a rather simple yet useful macro.
;;;

;;; The print-db macro takes a variable number of arguments
;;; and prints each expression as well as the result of evaluating
;;; that expression.   The return value is the value obtained
;;; by evaluating the last expression.
(defmacro print-db (&rest forms)
  `(prog1 (progn
            ,@(loop for form in forms
                    nconc (list `(print (quote ,form) *debug-io*)
                                `(write-string "  " *debug-io*)
                                `(prin1 ,form *debug-io*))))
    (terpri *debug-io*)))

;;; Try this out!
(print-db (+ 1 2) (* 3 4))

;;; To understand what's going on in the macro, we've used C-c <return>
;;; to derive the macro expansion.  We can see clearly what is going on.
;;; The first PROG1 ensures that its return value is the value of the
;;; first form (we don't want the return value to be '(terpri *debug-io*).
;;; The inner PROGN ensures that its return value is the value of the
;;; last form.
(PROG1
  (PROGN (PRINT '(+ 1 2) *DEBUG-IO*)          ;; prints the list '(+ 1 2), the original expression
         (WRITE-STRING "  " *DEBUG-IO*)       ;; prints a space
         (PRIN1 (+ 1 2) *DEBUG-IO*)           ;; prints the result of evaluating (+ 1 2).
         (PRINT '(* 3 4) *DEBUG-IO*)
         (WRITE-STRING "  " *DEBUG-IO*)
         (PRIN1 (* 3 4) *DEBUG-IO*))
  (TERPRI *DEBUG-IO*))                        ;; prints a carriage return


;;; How does the macro do that?  Let's take a closer look.
;;; First, it uses the backquote operator to return a list
;;; with the PROG1 form and the inner PROGN form.  So far,
;;; that's simple enough.

;;; The ,@ operator is similar to the , operator.  The expression
;;; (loop for form in forms ... ) is evaluated.  The (loop) expression
;;; iterates over each argument provided and collects the result
;;; of evaluating each 'form' and puts it in (list `(print ....)).
;;; Notice that (quote, form) is also used to prevent evaluation.
;;; The ,@ operator then splices its result into the list created
;;; by the backquote operator.

;;; The following example will illustrate the difference between the
;;; , and ,@ operators.

(setq the-second-elt '(2 3 4))

;;; You will need to type these.

;;; `(1 ,the-second-elt 5)              ;; => '(1 (2 3 4) 5)
;;; `(1 ,@the-second-elt 5)             ;; => '(1 2 3 4 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
