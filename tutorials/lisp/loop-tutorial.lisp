;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: loop-tutorial.lisp
;;;  Author: Teo
;;;  Description:
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; In vanilla LISP, iteration is usually performed with
;;; tail-end recursion or the (do) or (do*) forms.
;;; The LOOP macro provides a very powerful tool in
;;; describing commonly used iterative paradigms like
;;; the while loop or the until loop.
;;;
;;; The LOOP macro is rather complicated as it has many
;;; possible variations; we will only describe the more
;;; commonly used ones.  If you are interested in knowing more,
;;; we suggest you refer to Steele.

;;; As usual, let's begin with an example so as to get
;;; a feel for what the LOOP macro is like :-

;;; This loop creates a list of pairs of numbers, the
;;; second being the square of the first.
(loop for x from 1 to 10
      for y = (sqr x)
      collect (list x y))

;;; In this example, the LOOP macro is used over numbers.
;;; As we will soon see, the LOOP macro can be operated
;;; over other data types as well.

;;; But as far as numbers, we could also have done the
;;; following:-

(loop for x downfrom 10 to 1
      for y = (sqr x)
      collect (list x y))

;;; or equivalently,

(loop for x from 10 downto 1
      for y = (sqr x)
      collect (list x y))

;;; Either the downfrom clause or the downto clause is
;;; necessary to indicate that the iteration should
;;; proceed decrementally.

;;; In C, we usually find loops that iterate from 0.
;;; This is achieved similarly with the 'below' clause.

(loop for x from 0 below 10            ;; will not include 10
      for y = (sqr x)
      collect (list x y))

;;; or

(loop for x from 10 above 0             ;; will not include 0
      for y = (sqr x)
      collect (list x y))

;;; The BY clause specifies the amount the iterator increases
;;; by as shown below.

(loop for x from 0 below 10 by 2
      for y = (sqr x)
      collect (list x y))


;;; The = clause indicates assignment while a for and =
;;; combination indicates that the assignment should be done
;;; at every iteration.  Sometimes we'd like the assignment
;;; to be done only once and this can be done with the
;;; WITH clause.  For example,

;;; Notice that the second element of every tuple is 1.
(loop for x from 1 to 10
      with y = (sqr x)
      collect (list x y))


;;; Another useful numerical iterator is the REPEAT clause
;;; Consider the following alternative to what we have been
;;; doing :-

(loop for x = 1 then (1+ x)
      for y = (sqr x)
      repeat 10
      collect (list x y))

;;; The FOR-=-THEN clause combination specifies that the
;;; variable 'x' is initially assigned the value 1 and
;;; subsequently computed by (1+ x).   Here, the REPEAT 10
;;; clause controls the iteration.


;;; So far, we have only collected the results into a list.
;;; We could also have done other operations like count the
;;; number of times a certain expression evalues to a non-NIL
;;; value or sum over a variable as the following examples show.

;;; Computes the sum of the numbers 1, 2, ..., 10.
(loop for i from 1 to 10
      sum i)

;;; Computes the sum of the numbers 1, 2, ..., 10 but this time
;;; introducing an additional variable 'count'.

(loop for i from 1 to 10
      sum i into count
      finally (return count))

;;; The FINALLY clause is performed at the end of the iteration
;;; and in this case returns the value of count.

;;; This counts the number of even numbers during the iteration.
;;; The COUNT clause counts the number of times its expression
;;; evaulates to a non-NIL value.

(loop for i from 1 to 10
      count (evenp i))

;;; Similarly, count can be made to use a local variable.

(loop for i from 1 to 10
      count (evenp i) into num-evens
      finally (return num-evens))

;;; Again, we need to use the FINALLY clause because the
;;; LOOP expression doesn't know to return num-evens.

;;; Likewise, the MINIMIZE and MAXIMIZE clauses compute
;;; the minimum and maximum value returned from evaluating
;;; its expression.

(loop for i from 1 to 10
      minimize i)
(loop for i from 1 to 10
      maximize i)


;;; We can further control when we perform these actions
;;; using the IF...{ELSE}, WHEN...{ELSE} and UNLESS...{ELSE}
;;; clauses.

;;; For example, the following creates a list of all the
;;; even numbers from 1 to 10.
(loop for i from 1 to 10
      when (evenp i) collect i)

;;; This returns a list containing two elements: the first
;;; is a list of all the even numbers and the second, the
;;; odd numbers between 1 and 10.
(loop for i from 1 to 10
      when (evenp i) collect i into even-nums
      else collect i into odd-nums
      finally (return (list odd-nums even-nums)))


;;; The following LOOP returns a list contain two elements:
;;; the first is a list of all the elements smaller or equal to
;;; the first element and the first element itself while
;;; the second is a list of all the elements larger
;;; than this first element.
(let ((list-of-nums '(4 5 1 2 3 6 7 10 9 8)))
  (loop for i in list-of-nums
	with first-elt = (car list-of-nums)
	if (<= first-elt i)
	     collect i into less-bag
	else
	     collect i into more-bag
	finally (return (list less-bag more-bag))))

;;; This is a simple routine that can be used to implement quick sort.


;;; The LOOP macro can be extended to do arbitrary operations
;;; with the DO clause.

(loop for x from 1 to 10
      for y = (sqr x)
      do
      (print x)                   ;; there is an implicit (progn) here
      (print y))


;;; Also, the LOOP macro can be used to behave like a WHILE loop
;;; or an UNTIL loop.  Note however that the UNTIL expression
;;; is evaluated at the beginning of the loop; hence, it is
;;; equivalent to a WHILE (not expr).

(loop for i from 1 to 10
      while (< i 5)
      collect i)

(loop for i from 1 to 10
      until (not (< i 5))
      collect i)


;;; As you might have noticed, the LOOP macro can be iterated over
;;; lists.

(loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
      for a = (first numlist)
      and for b = (second numlist)
      and for c = (third numlist)
      collect (list c b a))

;;; The new AND clause causes parallel binding of the various FOR
;;; clauses.  This is similar to LET while the default (without
;;; the AND clause) is akin to LET*.

;;; The LOOP macro also allows destructuring of lists which is
;;; extremely useful as it faciliates the writing of succint
;;; code.  Consider the above example rewritten using restructuring.

(loop for (a b c) in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
      collect (list c b a))

;;; The expression following the FOR clause is used as a template
;;; to bind between the element of the list and the variables within
;;; the template.  Consider the following further examples.

(loop for ((a . b) (c . d))
      in '(((1.2 . 2.4) (3 . 4)) ((3.4 . 4.6) (5 . 6)))
      collect (list a b c d))

;;; If we had only wanted to extract the first of the two-tuple
;;; in the above example, we can set that part of the template
;;; to NIL.

(loop for ((a . b) nil)
      in '(((1.2 . 2.4) (3 . 4)) ((3.4 . 4.6) (5 . 6)))
      collect (list a b))


;;; Similarly, the following extracts the first and third element
;;; from the given list.
(loop for (a nil b) = '(1 2 3)
      do (return (list a b)))


;;; The LOOP macro can also be used to iterate over hash tables,
;;; vectors and packages.  We refer you to Steele for further
;;; examples.


;;; Finally, sometimes it's desirable to compute a predicate
;;; over the entire iteration and compute either the intersection
;;; or union of all the results.  The ALWAYS, NEVER and THEREIS
;;; clauses are provided for that purpose.

;;; The ALWAYS clause terminates the loop if its expression
;;; ever evaluates to NIL; in this case, it returns NIL.
;;; Otherwise, it returns T by default.

(loop for i from 0 to 10
      always (< i 11))

;;; The NEVER clause terminates the loop if its expression
;;; evaluates to a non-NIL value; in this case, it returns NIL.
;;; Otherwise, it returns T by default.

(loop for i from 0 to 10
      never (> i 11))

;;; The THEREIS clause is a little different.  It terminates the
;;; loop if ever its expression evaluates to a non-NIL value and
;;; returns that value.  Otherwise, it returns NIL.

(loop for i from 0 to 10
      thereis (evenp i))

;;; Unlike the WHILE and UNTIL clauses, in the case of these
;;; three clauses, when there is a premature termination of the loop
;;; the FINALLY clause is not evaluated.

(loop never t
      finally (print "You won't see this"))

;;; However, if the iteration completes normally, the FINALLY
;;; clause will be evaluated.

(loop for i from 1 to 10
      thereis (> i 10)
      finally (print "You should see this"))


;;; We used the (return) expression in some previous examples.
;;; This expression causes a non-local exit from the loop similar to
;;; that caused by the three clauses here and as such will not
;;; result in the execution of the FINALLY clause.

(loop for i from 1 to 10
      when (= i 5) do (return (print "5 is enough"))
      finally (print "You won't see this"))

;;; On the other hand, the (loop-finish) expression terminates
;;; the loop normally and returns any accumulated result.

(loop for i from 1 to 10
      when (= i 5) do (loop-finish)
      finally (print "You will see this"))

(loop for i from 1 to 10
      collect i
      when (= i 5) do (loop-finish))


;;; Type declarations.  Although optional the types of local variables
;;; can be specified immediately following the variable.  This information
;;; is passed to the compiler and might result in more efficient code.

;;; For example,

(loop for i integer from 1 to 10
      do (print i))

(loop for ((a . b) (c . d)) ((float . float) (integer . integer))
      in '(((1.2 . 2.4) (3 . 4)) ((3.4 . 4.6) (5 . 6)))
      collect (list a b c d))

;;; This indicates that all the variables a, b, c and d are to
;;; be treated as floats.
(loop for ((a . b) (c . d)) float
      in '(((1.2 . 2.4) (3 . 4)) ((3.4 . 4.6) (5 . 6)))
      collect (list a b c d))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
