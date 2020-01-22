;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: control-tutorial.lisp
;;;  Author: Teo
;;;  Description:
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; There are many different control constructs in LISP.
;;; We will begin with the most ubiquitous.

;;;
;;; IF-THEN-ELSE
;;;
;;; The IF-THEN-ELSE control construct implemented in LISP has
;;; the following format:-
;;;
;;;      (if <expr-1> <expr-2> <expr-3>)
;;;
;;; If <expr-1> evaluates to a non-NIL value, <expr-2> is evaluated
;;; and the result is returned as the result of the IF form.  Otherwise,
;;; <expr-3> is evaluated and returned.  Note that <expr-2> and <expr-3>
;;; do not ever both get evaluated.  Unlike regular functions, the
;;; arguments are not evaluated prior to calling IF.
;;;

;;; Returns a 2 because 1 is non-nil
(if 1 2 3)

;;; Returns a 2 because the first expression evaluates to NIL.
(if (> 1 2) 1 2)

;;; Nexted IF-THEN-ELSE
(defun min-3 (a b c)
  (if (< a b)
      (if (< a c) a c)
      (if (< b c) b c)))

;;; Returns 1
(min-3 2 1 3)


;;;
;;; Dropping the ELSE
;;;
;;; We can drop the ELSE by leaving out the third expression.  If
;;; the result of evaluating the first expression is NIL, a NIL
;;; is returned.

;;; Returns 1
(if (< 1 2) 1)

;;; Returns NIL
(if (> 1 2) 1)


;;;
;;; WHEN AND UNLESS
;;;
;;; Similar to dropping the ELSE in an IF, the WHEN construct
;;; can also be used to achieve this.
;;;
;;; (when (<test>)               (unless (<test>)
;;;     <consequent-1>                <consequent-1>
;;;     <consequent-2>                <consequent-2> 
;;;        .                             .
;;;        .                             .
;;;     <consequent-n>)               <consequent-n>)
;;;
;;; Unlike the IF construct (without the ELSE), a variable number
;;; of forms can follow the test.  If the <test> evalues to a non-NIL
;;; value, the consequents are evaluated in turn.
;;;
;;; Conversely, the UNLESS construct will evaluate its consequent
;;; forms only if <test> evalutes to NIL.
;;;


;;;
;;; COND
;;;
;;; The COND construct is another very useful construct especially
;;; when we have many choices.  The format for COND is as follows:-
;;;
;;; (cond (<test-1>
;;;        <consequent-1-1> <consequent-1-2> ...)
;;;       (<test-2>
;;;        <consequent-2-1> <consequent-2-2> ...)
;;;        .
;;;        .
;;;       (t <consequent-t-1> <consequent-t-2> ...))
;;;
;;; The optional 't' test will always succeed and is used as a default/catch-all.
;;; However, if it is not provided and none of the tests succeed, a NIL is returned.
;;;

;;; Now, consider the following examples:-

(defun what-am-i (thing)
  (cond ((null thing)
	 (print "Empty list"))
	((consp thing)
	 (print "List"))
	((numberp thing)
	 (print "Number"))
	((symbolp thing)
	 (print "Symbol"))
	(t (print "I don't know"))))

;;; Returns "List"
(what-am-i '(1 2 3))

;;; Returns "Symbol"
(what-am-i '+)

;;; Returns "Empty List"
(what-am-i nil)

;;; Returns "I don't know"
(what-am-i (make-array '(10)))


;;; Unlike IF, the COND construct allows multiple expressions
;;; following each test.  Technically speaking, there is an implicit
;;; PROGN.  We will cover that in more detail later.  Each expression
;;; is evaluated in turn and the result of the last expression evaluated
;;; is returned as the result of the COND.

;;; Note that there is no restriction on the tests.  Hence, if there
;;; are more than one test that could potentially evaluate to true
;;; (non-NIL that is), the first one specified in the COND will be the one
;;; triggered.  The other tests will be skipped altogether.


;;;
;;; CASE
;;;
;;; The CASE construct is a special purpose construct very similar
;;; to the switch() construct in C and the case construct in PASCAL.
;;; It is particularly useful when we are trying to classify.
;;; It's format is as follows :-
;;;
;;; (case <keyform>
;;;       (<keylist-1>
;;;        <consequent-1-1> <consequent-1-2> ...)
;;;       (<keylist-2>
;;;        <consequent-2-1> <consequent-1-2> ...)
;;;        .
;;;        .
;;;       (t <consequent-t-1> <consequent-t-2> ...))
;;;
;;; <keyform> is evaluated once and tested using (member :test 'equal)
;;; with each <keylist>.  If (member) succeeds, the consequent forms
;;; are evaluated in order.  Note that CASE does not evaluate the
;;; elements in the keylist at all.
;;;

(defun bottom-line-on-Shakespearean-play (symbol)
  (case symbol
    ((Macbeth)
     "Somebody's head gets cut off.")
    ((Hamlet)
     "Somebody gets posioned.")
    ((Titus-Andronicus)
     "Somebody's tounge gets cut off.")
    ((Lear)
     "Somebody gets hanged.")
    ((Coriolanus Julius-Caesar)
     "Somebody gets stabbed to death.")
    (t "Oh, that must be one of the others.")))

;;; Returns "Somebody gets stabbed to death."
(bottom-line-on-Shakespearean-play 'Julius-Caesar)


;;;
;;; PROGN
;;;
;;; This construct allows the creation of blocks of forms which are
;;; sequentially evaluated.  This is particular useful when using
;;; the IF construct as the construct allows only a single form
;;; as its consequent.
;;;
;;; The format for PROGN goes like this:-
;;;
;;; (progn <expr-1>
;;;        <expr-2>
;;;         .
;;;         .
;;;        <expr-n>)
;;;
;;; The result of the PROGN form is the result of evaluating the last
;;; form in the block.

(if "Merchant Of Venice"
    (progn (print "All that glitters is not gold")
	   (print "Often have you heard that told")
	   (print "Many a man whose heart hath sold")
	   (print "For my outside to be hold")))


;;;
;;; PROG1
;;;
;;; Sometimes it is useful to return the value of the first form
;;; instead of the last.  An alternative to the PROGN construct
;;; is the PROG1 construct which does just that.
;;;
;;; It's format is identical to (prog1) :-
;;;
;;; (prog1 <expr-1>
;;;        <expr-2>
;;;         .
;;;         .
;;;        <expr-n>)
;;;
;;; The result of the (progn) is the result of evaluating <expr-1>.
;;;

;;; Returns 2 although all the forms are evaluated (i.e. a, b and c have been assigned).
(prog1
    (setq a 2)
    (setq b 3)
    (setq c 4))


;;; Now, we will look at one of the most often used construct the LET
;;; and LET* constructs.  Its introduction has been delayed because in order
;;; to completely understand what's going on, we need to tackle the way
;;; LISP performs scoping.  We will first present the syntax of the constructs
;;; and then later explain the scoping/shadowing that goes on.

;;;
;;; LET and LET*
;;;
;;; The LET construct is used to introduce "local variables" which are automatically
;;; unbound on exiting.  It's format is:-
;;;
;;; (let ((<variable-1> <value-1>)
;;;       (<variable-2> <value-2>)
;;;        .
;;;        .
;;;       (<variable-n> <value-n>))
;;;
;;;    <expr-1>
;;;    <expr-2>
;;;     .
;;;     .
;;;    <expr-m>)
;;;
;;; The local variables <variable-1> through <variable-n> are created and their
;;; associated values computed and assigned to them.  They will shadow any
;;; other variable of the same name while in the LET form.  <expr-1>
;;; through <expr-m> are then evaluated sequentially.  The result of the LET form
;;; is the result of the last evaluated form.
;;;

(setq a 1)                           ;; global 'a' assigned value 1
(let ((a 3))                         ;; local copy of 'a' created and assigned the value 3
  (print a)
  (setq a 4)                         ;; local 'a' set to 4
  (print a))
                                     ;; local 'a' unbound and 'a' refers to the global 'a'

;;; Now check what the value of a is.  It should be 1 because
;;; the LET declares a local variable of the same name which
;;; shadows the globally present variable 'a'.

;;; As expected, variables declared in inner LET's will shadow those of the
;;; same name declared in outer LET's.

(let ((a 1))                         ;; first local copy of 'a' created and assigned the value 1
  (print a)
  (let ((a 2))                       ;; second local copy of 'a' created and assigned the value 2
    (print a)
    (setq a 3))                      ;; second local copy of 'a' set to 3
                                     ;; second local copy of 'a' unbound and 'a' now
                                     ;; refers to the first local copy of it.
  (print a))                         ;; first local copy of 'a' unbound and 'a' refers to
                                     ;; the global copy.



;;;
;;; Sometimes it is useful to be able to use the result of a recently computed
;;; local variable to compute another local variable.  This is achieved by using
;;; LET* whose format is the same as LET :-
;;;
;;; (let* ((<variable-1> <value-1>)
;;;        (<variable-2> <value-2>)
;;;         .
;;;         .
;;;        (<variable-n> <value-n>))
;;;
;;;    <expr-1>
;;;    <expr-2>
;;;     .
;;;     .
;;;    <expr-m>)
;;;
;;; 

(let* ((a 1)                      ;; local copy of 'a' set to 1
       (b (+ a 2)))               ;; local copy of 'b' computed as the local copy of 'a' + 1
  (print a)
  (print b))


;;; LET and LET* can also be nested arbitrarily.


;;;
;;; LEXICAL (STATIC) AND DYNAMIC SCOPING
;;;
;;; Lisp uses static scoping by default but can be requested
;;; to perform dynamic scoping if required.  Scoping determines
;;; which variables are accessed when there are more than one
;;; variable sharing the same name.  The main idea is that
;;; more "recent" variables take precedence over less "recent"
;;; variables.  Here is where the distinction between static
;;; and dynamic scoping arise.

;;; In static scoping, what we mean as far as the text of the
;;; function goes is that variables defined within inner scopes
;;; (let's, let*'s, defun's etc) take precedence over those in
;;; outer scopes.   This seems to make sense intuitively
;;; and is adopted by most procedural languages, C and
;;; PASCAL included.
;;;

;;; In dynamic scoping, what we mean is that variables
;;; which are created recently take precedence over variables
;;; created earlier.  The emphasis is on creation and not
;;; declaration.

(defun my-other-function (n)
  (my-function))

(defun my-function ()
  (1+ n))


;;; You will get an error if you evaluate this!
(my-function 2)

;;; The function (my-other-function) calls (my-function)
;;; with no arguments. But within (my-function), the variable
;;; 'n' refers to the last variable named 'n' that was created
;;; which in this case was the argument of (my-other-function).
;;; Hence, (my-function 2) should return 3.

;;; However, as LISP by default performs static scoping, we
;;; need to tell LISP explicitly that the 'n' in my-function
;;; is to be derived from the run-time stack.  Note that
;;; we need to inform LISP both in (my-other-function) as well
;;; as in (my-function).

(defun my-other-function (n)
  (declare (special n))
  (my-function))

(defun my-function ()
  (declare (special n))
  (1+ n))

(my-other-function 3)

;;; Dynamic scoping should be used sparingly.  Most of the
;;; time you could do without it.


;;;
;;; Now, for some cool stuff.  We can create unaccessible
;;; "variables" by using lexical scoping.  This is used when
;;; we want to be sure that the variable doesn't get modified
;;; by any other function.  We will illustrate this with
;;; a suite of functions implementing a stack.
;;;

(let ((unaccessible-stack nil))

  ;;; predicate returns T if stack is empty; NIL otherwise
  (defun empty-stack-p ()
    (null unaccessible-stack))

  ;;; removes all the elements from the stack
  (defun flush-stack ()
    (setf unaccessible-stack nil))

  ;;; pushes a new element onto the stack
  (defun push-stack (new-elt)
    (setf unaccessible-stack (cons new-elt unaccessible-stack))
    new-elt)

  ;;; pop the top most element off the stack; if the stack
  ;;; is empty, returns NIL.
  (defun pop-stack ()
    (if (null unaccessible-stack) nil
	(prog1 (car unaccessible-stack)
	  (setf unaccessible-stack (cdr unaccessible-stack))))))


(dolist (n '(1 2 3 4)) (push-stack n))
(pop-stack)


;;; The variable 'unaccessible-stack' can only be accessed by the functions
;;; defined within the (let).  The LISP garbage collector doesn't get rid
;;; of it because it is referenced by the DEFUN's.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
