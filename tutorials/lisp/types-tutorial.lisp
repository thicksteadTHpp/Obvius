;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: types-tutorial.lisp
;;;  Author: Patrick Teo
;;;  Description: Introduction to LISP and its data types
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; LISP is most commonly known for its lists but it also has
;;; a rich variety of data types which includes complex numbers,
;;; multidimensional arrays, strings and hash tables.

;;; Numbers in LISP come in many different varieties.  The 'simplest'
;;; form is the INTEGER.  After performing a division, LISP attempts to
;;; maintain a form known as RATIONALS.  If the rationals become too unwieldy
;;; to handle it converts it to FLOATS.  If a negative square root is
;;; encountered, it promotes the number type to a COMPLEX type if necessary.
;;; All this is done automatically and you need not be concerned with
;;; it under normal circumstances.

(+ 1 2)                 ;; adds 1 and 2 => 3
(+ 1 2 3 4)             ;; adds 1, 2, 3 and 4 => 10
(/ 22 7)                ;; divides 22 by 7 => 22/7, a rational
(asin (/ 1 (sqrt 2)))   ;; takes the arc sine of 1/sqrt(2) => 0.785398, a float
(sqrt -1)               ;; takes the square root of minus one

(+ 1.0 2)               ;; coerces a floating point addition

;;; The following is a list of arithmetic operations that are commonly used:-
;;;
;;; (+ <num-1> <num-2> ...)          -- adds all the arguments
;;; (- <num-1> <num-2> ...)          -- subtracts <num-2> ... from <num-1>
;;; (* <num-1> <num-2> ...)          -- multiplies all the arguments together
;;; (/ <num-1> <num-2> ...)          -- divides <num-1> by (<num-2> ...)
;;; (rem <number> <divisor>)         -- returns the remainder of <number>/<divisor>
;;; (min <num-1> <num-2> ...)        -- returns the minimum of all the arguments
;;; (max <num-1> <num-2> ...)        -- returns the maximum of all the arguments
;;; (1+ <num>)                       -- returns <num>+1
;;; (1- <num>)                       -- returns <num>-1
;;; (floor <num>)                    -- rounds down the number
;;; (ceiling <num>)                  -- rounds up the number
;;; (round <num>)                    -- rounds off the number
;;; (exp <power>)                    -- raises e to the power <power>
;;; (expt <base> <power>)            -- raises <base> to the power <power>
;;; (log <number> &optional <base>)  -- takes the log of <number> to the base <base>
;;;                                     which defaults to e.
;;; (sqrt <number>)                  -- takes the square root of <number>
;;; (isqrt <number>)                 -- takes the integer square root of <number> (rounds down)
;;; (abs <number>)                   -- returns the absolute of <number>
;;; (sin <radians>)                  -- returns the sine of <radians>
;;; (cos <radians>)                  -- returns the cosine of <radians>
;;; (tan <radians>)                  -- returns the tangent of <radians>
;;; (asin <number>)                  -- returns the arc sine of <number>
;;; (acos <number>)                  -- returns the arc cosine of <number>
;;; (atan <number>)                  -- returns the arc tangent of <number>
;;; (atan <y> &optional <x>)         -- returns the arc tangent of <y>/<x>
;;;                                     the signs of y/x are used to derive the
;;;                                     quadrant information.
;;; (random <num> &optional <state>) -- returns a random number [0, <num)
;;; pi                               -- evaluates to the value of PI.
;;; 

;;; The simplest data type in LISP is a symbol which can be
;;; used to name variables.  The following are examples of
;;; symbols :-
;;;
;;; psych-267
;;; gabor-filter
;;; destroy!
;;; 3x3-filter
;;;
;;; LISP allows a wider range of characters that can be used to
;;; specify symbols than C.  At the top level of the LISP interpretor,
;;; symbols evaluate to their symbol-value (i.e. the value bound to
;;; the symbol).  Later, we'll see that symbols can also have functions
;;; bound to them in addition to regular values.
;;;

;;; We assign values to symbols with (setq).

(setq a 1)             ;; a = 1
(setq b 2)             ;; b = 2

(setf a 1)             ;; These do the same thing in this example but (setf)
(setf b 2)             ;; is more general as we will see later.


;;; The most glaring data type in LISP is the list and is the
;;; reason for all the parentheses.  A list is a sequence of elements
;;; separated by spaces within parenthesis.  Each element may be
;;; of a different data type.

;; '(1 2 3 4 5)           ;; list of numbers
;; '(a b c d e)           ;; list of symbols
;; '("a" "b" "c")         ;; list of symbols
;; '(a 1 b 2)             ;; mixed list of symbols and numbers
;; '(1 (2 (3 4)) 5)       ;; nested lists

;;; Why do we have the quote in front of the list?  In general, lists are
;;; evaluated by the interpretor in a very special way.  The first element
;;; is taken to be the name of the function and the rest of the elements as
;;; arguments to be passed to this function.  Hence, (1 2 3 4 5)
;;; would result in apply 2 3 4 5 to the function 1 which doesn't make
;;; sense.  On the other hand, '(1 2 3 4 5) is a synonym for
;;; (quote (1 2 3 4 5)).  This results in the list (1 2 3 4 5) being passed
;;; to the function quote which simply returns it.  Although LISP usually
;;; evaluates its arguments first in which case we would have still
;;; had to evaluate (1 2 3 4 5), the (quote) function is a special-form
;;; in that LISP doesn't evaluate its arguments prior to calling the function.

;;; Lists are widely used in LISP programs as data structures so we'll
;;; devote quite a bit of time to exploring them.

;;; First, let's make a list of numbers
(setq numbers '(1 2 4 3 6 5 6))

;;; Verify that it's actually there by typing numbers (you'll need to type
;;; this in yourself).
;;;
;;; numbers

;;; (cons <element> <list>) => (<element> . <list>)
;;; cons takes an element and adds it to the front of the list.
(cons 9 numbers)

;;; Now take a look at numbers again by typing it.

;;; What happened to number 9?  Actually, the (cons) operator
;;; creates a new list (not quite but let's consider it to be that way
;;; for now).  So, if we want 'numbers' to have that 9 in front,
;;; we need to assign it.  (cons) is a non-destructive operator.
;;; In "pure" LISP, all operators "should" be non-destructive.
;;; However, it's inefficient to always do this and LISP has
;;; destructive operations which actually modifies the list
;;; passed.

;;; Now, if we look at numbers again, it'll have number 9
(setq numbers (cons 9 numbers))

;;; Type numbers to verify this.

;;; Reverses the list.  (reverse) is non-destructive, so we
;;; would have to (setq) it to numbers if we wanted to modify
;;; numbers.
(reverse numbers)

;;; Appends one list to another (non-destructive).
(append numbers numbers)

;;; Tests if 2 is an element of the list numbers.  Note that
;;; (memeber) doesn't check recursively, i.e. if you have nested
;;; lists, this wouldn't work.  Returns the list with the element
;;; as the first element of that list if the element can be found;
;;; otherwise returns NIL.
(member 2 numbers)
(member 20 numbers)

;;; Returns the length of the list
(length numbers)

;;;; Makes a list containing all its arguments
(list 1 2 3 4 5 6)

;;; Now, try this:-
(setq list-1 (list 2 (+ 2 7) (* 3 4)))
(setq list-2 '(2 (+ 2 7) (* 3 4)))

;;; (list) makes a list containing all of its arguments.
;;; However, all the arguments are evaluated prior to
;;; being sent to (list).  In the second example, the quote
;;; operator inhibits evaluation of its argument.

;;; Nil, () and '().
;;; As far as lisp is concerned, the following three are
;;; identical.  Nil is also used to mean false
;;; in boolean operations as we shall se later.
;;;
;;; Type them in for yourself and see.
;;;   Nil
;;;   ()
;;;   '()

;;;
;;; car, cdr, first, second, nth...
;;;

;;; (car) returns the first element of the list.  By definition,
;;; if numbers is nil, a nil is returned as well.
(car numbers)

;;; (cdr) returns a list of all the elements except the first.
(cdr numbers)

;;; Returns the second element of the list.  Actually,
;;; (cdr numbers) returns a list whose first element
;;; is the second element of the original list and
;;; (car <of-that-result>) returns that first element.
(car (cdr numbers))


;;; Synonyms and shorthands.

;;; This is the same as the previous expression.
(cadr numbers)

;;; In general, we can cascade as many as we like but gets quite
;;; unreadable.

;;; Returns the fourth element of the list.
(car (cdr (cdr (cdr numbers))))
(cadddr numbers)

;;; We could also have used:-
(fourth numbers)

;;; Dependending on the implementation of LISP that you're using, you may
;;; have fifth, sixth, .... The general and more useful
;;; form is:-

;;; (nth) returns the fourth number (zero is the first element).
(nth 3 numbers)


;;;
;;; The Cons Cell
;;;

;;; CONS cells are the only time you'll hear about pointers
;;; in LISP.

;;; We have mentioned what lists are but it's useful to
;;; understand what really goes on behind the scene.

;;; Lists are made up of CONS cells (also known as dotted pairs).
;;; Each cons cell is a 2-tuple represented as (a . b) (note the
;;; dot) where a is the first element and b the second.  In most
;;; cases, b is a pointer to another CONS cell.  Graphically,
;;;
;;;                      +---+---+
;;;                      | a | b |
;;;                      +---+---+
;;;

;;; So, what are lists?  We will illustrate with an example.
(setq short-list '(1 (2 (3) 4)))

;;; is equivalent to :-
;;;
;;; '( 1 . ( ( 2 . ( ( 3 . NIL) . (4 . NIL) ) ) . NIL ) )
;;;
;;;     and
;;;
;;; (cons 1 (cons (cons 2 (cons (cons 3 nil) (cons 4 nil))) nil))
;;;
;;; (You can evaluate both of these expressions in LISP and they'll
;;;  give the same expression.)
;;;
;;; Graphically, we have,
;;;  
;;;  +---+---+   +---+---+
;;;  | 1 | X +-->| X | X-+-->NIL
;;;  +---+---+   +-+-+---+
;;;                |
;;;                |   +---+---+
;;;                +-->| 2 | X |
;;;                    +---+-+-+
;;;                          |
;;;                          |    +---+---+
;;;                          +--->| X | X |
;;;                               +-+-+-+-+
;;;                                 |   |
;;;                                 |   |    +---+---+
;;;                                 |   +--->| 4 | X-+-->NIL
;;;                                 |        +---+---+    
;;;                                 |
;;;                                 |        +---+---+   
;;;                                 +------->| 3 | X-+-->NIL
;;;                                          +---+---+
;;;

;;; So, why do we need to know all this?  Well, remember
;;; when we learnt about cons and the result of
;;; evaluating (cons 9 numbers)?
;;;
;;; What actually happened was a new cons cell was created
;;; which has as its first element the number 9 and the
;;; second element, a pointer to the original list.
;;;
;;;                   +---+---+
;;;                   | 9 | X-+--> numbers
;;;                   +---+---+
;;;

;;; For you C hackers out there, you'd probably be saying
;;; that we can really do some neat stuff here (and also
;;; mess up badly).  Consider the following example.

(setq a '(2 3 4))
(setq b (cons 4 a))

;;; The following sets the value of the first element in
;;; the CONS cell. This is a destructive operation!  Similarly,
;;; (setf (cdr a) something) sets the second element of the cell.
(setf (car a) 'two)

;;; Take a look at the list a.

;;; Now, take a guess what 'b' is.

;;; Lo-and-behold!  What happened?  Well, as we said, what really
;;; happened was we only created a new cons cell with a pointer to
;;; the old.  A new list is not created.

;;; Now, try this:-
(setf a '(4 5 6))

;;; What do you think 'b' is?

;;; 'b" remains the same.  Why?  What happened was we created a new
;;; list '(4 5 6) and bounded that to the symbol 'a'. 'b' continues to
;;; point to the old list that 'a' used to point to.

;;; If you're thinking about it already, the answer is "yes," you can
;;; create circular lists but be careful there is no way to
;;; print it out as the printing will go on forever!

;;; So far, we have only worked with lists of numbers and nested lists
;;; of numbers.  We can also have lists of strings or symbols or other
;;; data types.  We could have lists with different data types.





;;;
;;;  Property Lists and Association Lists
;;;

;;; A common use of lists is as PROPERTY LISTS.
;;; Property lists are lists of the form:-
;;; (symbol-1 value-1 symbol-2 value-2 symbol-3 value-3 ...)
;;; 
;;; For example, a simple property list could be a name-telephone number
;;; list.
;;;
(setq *phone-book*
      '(patrick "7-1234"
	ivan    "7-4321"
	jean    "7-2332"))

;;; The following returns the value of the symbol 'ivan'.
;;; (getf) returns the value if successful and NIL otherwise.
(getf *phone-book* 'ivan)

;;; The following changes the value of the symbol 'patrick'.
(setf (getf *phone-book* 'patrick) "7-4559")

;;; The following removes the symbol-value pair from the list.
;;; (remf) returns T if successful and NIL otherwise.  It is
;;; a destructive operation.
(remf *phone-book* 'jean)


;;; A similar idea can be implemented as ASSOCIATION LISTS.
;;; Association lists are lists of the form:-
;;; ((symbol-1 . value-1) (symbol-2 . value-2) ...)  
;;;
;;; Let's try out the previous example:-
;;;
;;; (Note: we are using dot operators -- this is not *really* necessary
;;;  though but is generally the way people do it.)

(setq *phone-book*
      '((patrick . "7-1234")
	(ivan    . "7-4321")
	(jean    . "7-2332")))


;;; The following returns the (symbol . value) pair associated with the
;;; symbol 'patrick.
(assoc 'patrick *phone-book*)

;;; The following modifies the value associated with the returned
;;; (symbol . value) pair.
(setf (cdr (assoc 'patrick *phone-book*)) "7-5723")

;;; See what *phone-book* is.

;;; The following returns the (symbol . value) pair associated with the value
;;; "7-4321". Note we had to include :test #'string=.  This tells rassoc to use
;;; the (string=) function to test between the given value and the values
;;; in the list.  We could have given any function including ones that we
;;; write.
(rassoc "7-4321" *phone-book* :test #'string=)

;;; The following adds a (symbol . value) pair to *phone-book*.  Since (acons)
;;; is non-destructive, we need to set our original list to itself.
(setf *phone-book*
      (acons 'alison "7-9876" *phone-book*))

;;; The following removes the (symbol . value) pair from the list.
(setf *phone-book*
      (remove (assoc 'patrick *phone-book*) *phone-book*))


;;;
;;; Arrays
;;;
;;; Another useful data type is the array data type.  Again, the array
;;; data structure is also very extensive, we will introduce the more common
;;; uses of it and refer you to Steele for more details.
;;;

;;;
;;; The following creates an 2D array of size 20x30 of type float
;;; and initializes it to 0.0.
;;;
(setq my-array (make-array '(20 30)
			   :element-type 'float
			   :initial-element 0.0))


;;;
;;; The following creates a 3D array initialized to the given contents.
;;;
(make-array '(4 2 3)
	    :initial-contents
	    '(((a b c) (1 2 3))
	      ((d e f) (3 1 2))
	      ((g h i) (2 3 1))
	      ((j k l) (0 0 0))))

;;; (aref) reads element (2,10) of my-array.
(aref my-array 2 10)

;;; Sets element (5,3) of my-array to 1.0.
(setf (aref my-array 5 3) 1.0)

;;; (array-rank) returns the number of dimensions of my-array.
(array-rank my-array)

;;; (array-dimension) returns the size of a given dimension.
(array-dimension my-array 0)
(array-dimension my-array 1)

;;; (array-dimensions) returns a list whose elements are the
;;; dimensions of the array.
(array-dimensions my-array)

;;; (array-total-size) returns the total number of elements.
;;; in the array.
(array-total-size my-array)

;;; (array-in-bounds-p) returns T if the index is within
;;; the bounds of the array and NIL otherwise.
(array-in-bounds-p my-array 10 10)
(array-in-bounds-p my-array 10 30)


;;;
;;; Boolean datatype
;;;
;;; There is no real boolean data type in LISP.  Most LISP predicates
;;; consider NIL to be false and any non-NIL value to be true.
;;; The following is a series of LISP predicates operating on
;;; symbols, numbers and lists.
;;;

;;;
;;; Simple Predicates
;;;

;;; (listp) returns T if the argument is a list, NIL otherwise.
(listp nil)
(listp '(1 2 3))
(listp 1)

;;; Sometimes it's useful to see if it's a list that contains
;;; at least one element (i.e. not a NIL list).  The (consp)
;;; operator does just that.
(consp nil)
(consp 1)
(consp '(1 2 3))

;;; (symbolp) returns T if the argument is a symbol, NIL otherwise.
(symbolp 1)
(symbolp 'a)
(symbolp '(1 2 3))

;;; (atom) returns T if the argument is an atom, NIL otherwise.
(atom 1)
(atom 'a)
(atom '(1 2 3))

;;; (null) returns T if the argument is NIL, NIL otherwise.
(null 'a)
(null '(1 2 3))
(null nil)

;;; Boolean NOT operator. Returns T if the argument is NIL, NIL otherwise.
(not '(1 2 3))
(not nil)
(not t)

;;;
;;; equal/eq/eql/=
;;;
;;; Lisp has three different notions of equality and causes confusion
;;; for many people.  (The last two are equivalent and the most intuitive
;;; is equal.)
;;;
;;; (equal) returns T if the printed outputs "look" the same.
;;; (eq) checks if the memory locations of the pointers are the
;;; same.  This can be used for numbers, symbols and other atoms.
;;; Lastly, (eql) and (=) operates only on numbers.

(setq a '(1 2 3))
(setq b '(1 2 3))
(setq c b)

(equal a b)

;;; 'a' and 'b' are not (eq) because they point to different structures
;;; while 'b' and 'c' point to the same CONS cell.
(eq a b)
(eq b c)

;;; Clearly, (eq) is the most efficient and should be used when possible.
;;; However, it can often lead to bugs as you might expect.  So, be
;;; absolutely certain that you know what you are doing.  Otherwise, (equal)
;;; is the safest to use (but unfortunately, the most inefficient). (eql)
;;; is the most efficient for numbers.


;;;
;;; Logical Operators
;;;
;;; Some of the common logical operators include (and), (or) and (not).  We've
;;; already seen (not) in the previous section.  The operation for (and) and
;;; (or) are almost identical.

;;; The (and) operator takes any number of arguments and returns the last argument
;;; if there are no NIL elements or returns a NIL otherwise.

(and (> 1 2) (< 2 3) (> 4 5))
(and "a" "b" "c")
(and "a" (> 2 3) "c")

;;; (and) performs short-circuit (lazy) evaluation.  So, if a NIL is encountered
;;; before the end of the list, (and) will not evaluate its other arguments.

;;; Similarly, the (or) operator takes any number of arguments and returns the
;;; first non-NIL argument or returns NIL otherwise.

(or (> 1 2) (< 2 3) (> 4 5))
(or (> 1 2) "b" "c")
(or (> 1 2) (> 3 4))

;;; (or) also performs short-circuit evaluation, i.e. if it encounters
;;; a non-NIL argument it doesn't evaluate the other arguments.

;;; Because of this feature, (or) can be used in place of (if).  It's
;;; entirely a matter of style.  

;;;
;;; Predicates on Numbers
;;;
;;; (zerop <number>)       - returns T if <number> is zero, NIL otherwise.
;;; (plusp <number>)       - returns T if <number> is strictly greater than zero, NIL otherwise.
;;; (minusp <number>)      - returns T if <number> is strictly less than zero, NIL otherwise.
;;; (oddp <number>)        - returns T if <number> is odd, NIL otherwise.
;;; (evenp <number>)       - returns T if <number> is even (or zero), NIL otherwise.
;;;

;;;
;;; Comparisons on Numbers
;;;
;;; (= number &rest <more-numbers>)   - returns T if all the numbers are the same.
;;; (/= number &rest <more-numbers>)  - returns T if all the numbers are different.
;;; (< number &rest <more-numbers>)   - returns T if the numbers are monotonically increasing.
;;; (> number &rest <more-numbers>)   - returns T if the numbers are monotonically decreasing.
;;; (<= number &rest <more-numbers>)  - returns T if the numbers are monotonically non-decreasing.
;;; (>= number &rest <more-numbers>)  - returns T if the numbers are monotonically non-increasing.
;;;


;;;
;;; Aggregate structures
;;;
;;; Aggregate structures can be created by (defstruct) which
;;; is very similar to C's structs and Pascal's records.  However,
;;; we will not go over them as CLOS (Common Lisp Object System) is
;;; more useful.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
