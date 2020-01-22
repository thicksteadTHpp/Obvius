;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: clos-tutorial.lisp
;;;  Author: Teo
;;;  Description:
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; PRELIMINARIES
;;;
;;; CLOS (Common Lisp Object System) is LISP implementation of
;;; the object-oriented programming paradigm.  The focus of all
;;; the ideas surrounding CLOS is the CLASS.  The class putting
;;; it simply is an aggregate data type very much similar to a
;;; struct in C and record in PASCAL.  It can have any number
;;; of fields which are known as SLOTS.  Classes can be subclasses
;;; or superclasses of other classes.  Classes inherit all the fields
;;; and some properties from their superclasses.  However, we do not
;;; work directly on classes.  Instead, an INSTANCE of the class
;;; is created.  It is useful to think of the class as being the
;;; template and the instance of the class, the actual allocated
;;; data structure.
;;;
;;; METHODS are functions which operate on these classes.  Unlike
;;; regular LISP functions, these methods do not operate on all
;;; LISP objects.  Instead, we specify the classes for which they
;;; are APPLICABLE in their definition.  Again, all methods belonging
;;; to a class' superclass are applicable to the class itself;
;;; in a sense, they are inherited.
;;;
;;; CLOS allows overloading of methods, i.e. you can have 
;;; several methods with the same name but with different parameter
;;; specializations.  However, all methods of the same name must
;;; have the same number of arguments.  In order to decide which
;;; method to call, CLOS automatically creates a GENERIC DISPATCH
;;; which is a sort of meta-method.
;;;
;;; How do we decide which method to call then?  Generally speaking,
;;; the method that is most specific is invoked.  However,
;;; CLOS also allows you to invoke all the methods (in some order)
;;; as well.  This is useful if some initialization is required
;;; on the part of the superclass before the subclasses' code is
;;; called.
;;;
;;; 

;;;
;;; Let's look at a simple example:-
;;;

(defclass 3-vector ()
  ( (x :accessor x :initarg :x :initform 0 :type 'number :documentation "x-dim")
    (y :accessor y :initarg :y :initform 0 :type 'number :documentation "y-dim")
    (z :accessor z :initarg :z :initform 0 :type 'number :documentation "z-dim") )
  (:documentation "This is a class for three-dimensional vectors.") )

;;; This creates a class called '3-vector' which has three
;;; SLOTS, one for each dimension.  The :accessor x keyword-form
;;; pair creates a function to read and write from that slot.
;;; The :initarg :x keyword-form pair associates the keyword :x
;;; as a key that can be used to initialize the slot x when
;;; we create an instance of the class.  :initform 0 sets the
;;; value of the slot to zero if none is provided when creating
;;; an instance.  :type 'number informs CLOS that the type of the
;;; data in the slot is a number.  This can result in
;;; optimizations subsequently but does not guarantee error checking.
;;;
;;; Following the slot options, are the class options.  We have
;;; provided a documentation for the class with the :documentation
;;; keyword.


;;; So far, we've only created a class, the template.  Let's
;;; now allocate a variable that's of that class.

(setq my-vec (make-instance '3-vector :x 1.0 :y 1.0 :z 1.0))

;;; This creates an INSTANCE of the class 3-vector with the initial
;;; value of x, y and z all set to 1.0 and binds it to the variable my-vec.
;;; Since initial arguments were provided to all the slots, none
;;; of the defaults were used.  If we had not given an initial argument
;;; to any of the slots, the default :initform would have been used.
;;; If :initform was not provided, the slot would remain unbound!
;;; To see what's in the instance my-vec, we can type:-

(describe my-vec)

;;; Actually describe can be used on any LISP object.

;;; The functions that were automatically created when defining
;;; the class can now be used on the instance.  For instance,

;;; Returns the x value of the vector.
(x my-vec)

;;; Likewise, this sets the y value of the vector to 2.0
(setf (y my-vec) 2.0)
(y my-vec)


;;; Now, we shall write functions that operate on the class 3-vector.
;;; In CLOS terminology, these are known as METHODS.  Methods can
;;; be thought off as functions which also specify the class they operate on
;;; although with inheritence this issue becomes a little more complicated.
;;; We'll cover that in more detail later.

;;; Computes the L2 norm of the given vector
(defmethod vec-norm ((vec 3-vector))
  (sqrt (+ (sqr (x vec))
	   (sqr (y vec))
	   (sqr (z vec)))))

;;; If we are expecting to use the slots more frequently,
;;; we could have used a built in macro.

(defmethod vec-norm ((vec 3-vector))
  (with-slots (x y z) vec
    (sqrt (+ (sqr x) (sqr y) (sqr z)))))

(vec-norm my-vec)

;;; Alternatively, there are several other ways of accessing the slots.
;;; If no accessor (or reader or writer) functions are defined,
;;; the slots can be accessed by their names.  The following
;;; is an equivalent definition of (vec-norm) using (slot-value)
;;; instead of the accessor.

(defmethod vec-norm ((vec 3-vector))
  (sqrt (+ (sqr (slot-value vec 'x))
           (sqr (slot-value vec 'y))
           (sqr (slot-value vec 'z)))))

;;; Since when a default value is not supplied in the definition of a class,
;;; that slot will not be bound, we will need to test if it is
;;; bound to a value otherwise attempting to read it will result
;;; in an error.

;;; Returns T if the slot is bound and NIL otherwise.
(slot-boundp my-vec 'x)


;;; In the same spirit as (with-slots), (with-accessors) uses
;;; the names of the accessors instead of the slots.  In this
;;; example, the names of the accessors coincide with the names
;;; of the slots but in practice they do not need to.

;;; Hence, an equivalent definition of (vec-norm) would be:-

(defmethod vec-norm ((vec 3-vector))
  (with-accessors ((x-var x)                            ;; the first element refers to the local
                   (y-var y) (z-var z)) vec             ;; variable name while the second the
    (sqrt (+ (sqr x-var) (sqr y-var) (sqr z-var)))))    ;; accessor name.


;;; Note that in this case, we cannot use the accessor name
;;; by default (as in the (with-slots) case).  We need to
;;; specify a name for the local variable which can be the
;;; same as the accessor name, i.e. (with-accessors ((x x) ...)
;;; as in the above example.


;;; Normalizes the vector and returns the original norm of
;;; the vector.  This method is destructive as it changes the
;;; value of the slots of the instance.
(defmethod vec-normalize ((vec 3-vector))
  (let ((norm (vec-norm vec)))
    (with-slots (x y z) vec
      (setf x (/ x norm))
      (setf y (/ y norm))      
      (setf z (/ z norm)))
    norm))
  
(vec-normalize my-vec)

;;; This should be 1.0 (or close to it).
(vec-norm my-vec)


;;; Like (defun)'s, we can use any number of arguments.

;;; Adds two vectors and 
(defmethod vec-2-add ((vec-1 3-vector) (vec-2 3-vector))
  (with-slots ((x1 x) (y1 y) (z1 z)) vec-1
    (with-slots ((x2 x) (y2 y) (z2 z)) vec-2
      (make-instance '3-vector
                     :x (+ x1 x2)
                     :y (+ y1 y2)
                     :z (+ z1 z2)))))

(setq vec-1 (make-instance '3-vector :x 1.0 :y 1.0 :z 1.0))
(setq vec-2 (make-instance '3-vector :x 1.0 :y 1.0 :z 1.0))
(setq vec-sum (vec-2-add vec-1 vec-2))
(describe vec-sum)


;;; We do not need to specify a class for each argument either 
;;; in which case any class will work.  We will discuss more of
;;; this subsequently.

;;; Scales the vector by a scalar.  This operation is destructive.
(defmethod vec-scale ((vec 3-vector) scale)
  (with-slots (x y z) vec
    (setf x (* x scale)          ;;; (setf) actually allows you to specify
          y (* y scale)          ;;; multiple slot-value pairs.
          z (* z scale))))

(vec-scale my-vec 2.0)

;;; Actually, most of the regular non-CLOS types have CLOS "class"
;;; counterparts.  In general, they are of the same name.
;;; For example, (vec-scale) could have been written with
;;; the scale specialized to be a number.

(defmethod vec-scale ((vec 3-vector) (scale number))
  (with-slots (x y z) vec
    (setf x (* x scale)
          y (* y scale)
          z (* z scale))))
      
(vec-scale my-vec 3.0)


;;; The following is a short list of LISP types that can
;;; be used as class specializers:-
;;;
;;;              array
;;;              bit-vector
;;;              character
;;;              complex
;;;              cons
;;;              float
;;;              integer
;;;              list
;;;              null
;;;              number
;;;              ratio
;;;              rational
;;;              sequence
;;;              string
;;;              symbol
;;;              T
;;;              vector
;;;
;;;



;;; Types and Classes

;;; As with regular non-CLOS LISP, we can query the TYPE of
;;; the data bounded to a symbol.  In CLOS, the type of the
;;; class is the symbol name of the class.

;;; Returns a symbol representing the type of 'my-vec'
(type-of my-vec)

;;; Predicate which returns T if my-vec is of type '3-vector.
(typep my-vec '3-vector)

;;; Alternatively, we could query the CLASS of an object.
;;; The distinction between the two is that the CLASS is
;;; a structure of its own and most often another CLOS class!
;;; On the other hand, the TYPE is but a symbol (in the spirit
;;; of non-CLOS lisp).  Actually, the CLOS that we are discussing
;;; is implemented using a meta-CLOS language.

;;; Returns the actual CLASS structure of my-vec.  This is required
;;; by some built-in CLOS functions.
(class-of my-vec)

;;; Returns the class name of the class returned by (class-of my-vec).
;;; In essence, this is the same as the TYPE of the class.
(class-name (class-of my-vec))

;;; Sometimes we would like to find the CLASS structure given its
;;; class name, (find-class) can be used then.

;;; Returns the CLASS structure of 3-vector.
(find-class '3-vector)

;;; We can see that it is exactly the same structure as the CLASS structure
;;; returned by (class-of my-vec).
(eq (class-of my-vec) (find-class '3-vector))

;;; If no CLASS structure for that given class name can be found,
;;; an error is hailed by default.  We can instruct (find-class) to return
;;; NIL by adding NIL as the second argument:-

(find-class 'foo NIL)



;;;
;;; SIMPLE AND MULTIPLE INHERITANCE
;;;

;;; After getting a feel for CLOS, let's figure out what actually goes
;;; on and what more we can do.  There are two fundamental ideas which
;;; are very related to each other: INHERITANCE and PRECEDENCE.

;;; The first is of INHERITANCE.  Remember that we could specify in
;;; the CLASS definition, the names of SUPERCLASSES of our new class.
;;; One of the relationships between a superclass and a subclass is
;;; inheritance.  Inheritance takes the form of both data (the slots and
;;; the defaults) as well as the methods (the automatically created
;;; accessor functions as well as user-defined methods).

;;; Hence, in any typical CLOS program, you will encounter classes which
;;; are subclasses of some other classes and which are superclasses
;;; of some other classes.  We can imagine this to be like an inverted
;;; tree (in the data structure sense of a tree which is itself inverted).
;;; All classes implictly include T as a superclass.  We can verify that
;;; by the following:-

(typep my-vec T)
(typep my-vec '3-vector)

;;; Returns T!
(subtypep '3-vector T)

;;; Both expressions evaluate to true because (typep) actually
;;; returns T if its first argument is of the type specified
;;; by its second argument or if it is a subclass (direct or indirect)
;;; of it.

;;; All user-defined classes also implicity inherit another class
;;; known as standard-object.  This allows CLOS to implement
;;; generic methods for all objects like (print-object) and
;;; (describe).  As such, we can write methods to specialize
;;; these generic methods.  Hence, we can customize the way our
;;; instances are manipulated by LISP.

;;; Now, let us consider the inheritance of slots and slot options.

;;; Every slot of the superclass is inherited by the subclass.
;;; If there is a slot in the subclass whose name coincides with
;;; another slot in one of its superclasses only one copy of that
;;; slot is present in the subclass.  However, the manner in which
;;; slot options are treated is different for different slot options.

;;; Consider the following example:-

(defclass basic-lock ()
  ((name :initarg :name)))

(defclass simple-lock (basic-lock)
  ((name :initform "Simple Lock")))

;;; simple-lock will have a slot called 'name' that has an (:initarg :name)
;;; and an (:initform "Simple Lock") slot option.

;;; :accessor, :reader and :writer slot options are not inherited.  Hence,
;;; any accessor, reader or writer defined in the superclass will be
;;; defined for the superclass and not inherited by the subclass.  This
;;; does not mean that these methods cannot operate on instances of the
;;; subclass because these methods although defined for the superclass,
;;; are applicable to the subclass.

;;; As expected, the :documentation slot is also inherited by shadowing.
;;; So, the documentation for that slot is the documentation defined in
;;; the most specific class.

(defclass shape ()
  ((name :documentation "Name of Shape")))

(defclass triangle ()
  ((name :documentation "Name of Triangle")))

(describe (find-class 'shape))
(describe (find-class 'triangle))

;;; The :initarg slot option is inherited by union.  Hence, the accessors
;;; defined in superclasses as well as in the new subclass can all be used
;;; as initializers for that slot.  On the other hand, the slot option
;;; :initform is inherited by shadowing.

;;; The :type slot option is a little different; it is inherited by "and."
;;; What this means is the resultant type is the '(and <type-1> <type-2> ..)
;;; of all the types defined for that slot by all the superclasses as well
;;; as in the current subclass.

;;; We briefly alluded to the fact that methods are also "inherited" by
;;; subclasses.  This feature depends on two separate mechanisms taken
;;; care of by a generic dispatch:-
;;;
;;; 1.  Selecting the set of applicable methods
;;; 2.  Ranking the applicable methods in order of precedence.
;;;
;;; Now, what do we mean by a method is applicable?  You have already seen
;;; this in the examples before but let's define it more formally.
;;; A method is applicable if each of its require parameters is satisfied
;;; as follows :-
;;;
;;;        a.  <var>
;;;            An argument with no class specifier is equivalent to one
;;;            with type T and is applicable if (typep <arg> T), i.e.
;;;            if the argument is of type T (or a subclass of it).
;;;
;;;        b.  (<var> <class-name>)
;;;            The argument is applicable if (typep <arg> <class-name>), i.e.
;;;            if the argument is a subclass or the same class as <class-name>.
;;;
;;;        c.  (<var> (eql <form>))
;;;            The argument is applicable if (eql <arg> '<object>) where
;;;            <object> is the result of evaluating the <form>.

;;; We have not seen the last one, so let's try an example.

(defmethod divide ((dividend number) (divisor number))
  (/ dividend divisor))

(defmethod divide ((dividend number) (zero (eql 0)))
  (error "Cannot divide by zero."))

;;; The first method is called.
(divide 3 2)

;;; The second method is called.
(divide 3 0)


;;; We've got ahead of ourselves a little here.  In the second example above,
;;; both methods are actually applicable but the second method is more
;;; specific (i.e. it takes precedence).  This brings us to step two of the
;;; generic dispatch.  After selecting the set of applicable methods, it
;;; sorts these methods in order of precedence and selects the one with
;;; the highest precedence.

;;; The rule of assigning precedence is simple.  Specializers of the form
;;; (eql <form>) are more specific than class specifications.  If two
;;; methods are specialized by class specifications, the class precedence
;;; of the arguments in the order in which they appear in the definition
;;; dictate the precedence of these methods.

;;; This brings us to what we mean by PRECEDENCE of classes.  Class precedence
;;; is governed by two rules:
;;;
;;; 1.  A class always has precedence over its superclasses.
;;; 2.  The order in which the superclasses are defined in a class' definition
;;;     defines the precedence of its immediate superclasses (where the 
;;;     superclass defined first has the highest precedence).
;;;

(defclass my-stream () ())
(defclass my-input-stream (my-stream) ())
(defclass my-char-stream (my-stream) ())

(defclass my-char-input-stream (my-char-stream my-input-stream) ())

;;; The precedence for the above classes are as follows (in order of decreasing
;;; precedence) :-
;;;
;;; my-char-input-stream -> my-char-stream -> my-input-stream -> my-stream
;;;

;;; In general, the partial ordering of superclasses and subclasses
;;; needs to be converted to a total ordering.  It is quite likely that there
;;; may be more than one total ordering satisfying the partial ordering
;;; or there might be none at all.   In the latter case, an error is hailed.
;;; In the former situation, CLOS applies several other rules to perform this
;;; topological sort.  This is further described in Keene.
;;;


;;;
;;; Method qualifiers
;;;
;;; Although methods can be overloaded, several methods may be called
;;; in response to a single method call.  This is achieved with the
;;; use of method qualifiers: :before, :after and :around.  The
;;; methods that we've encountered so far do not have method qualifiers
;;; are known as primary methods.

;;; Consider the following example.

(defclass simple-class () ())
(defclass not-so-simple-class (simple-class) ())

(defmethod qualifier-test :before ((class simple-class))
   (print "before simple-class"))
(defmethod qualifier-test :after ((class simple-class))
   (print "after simple-class"))
(defmethod qualifier-test ((class simple-class))
   (print "primary simple-class"))

(defmethod qualifier-test :before ((class not-so-simple-class))
   (print "before not-so-simple-class"))
(defmethod qualifier-test :after ((class not-so-simple-class))
   (print "after not-so-simple-class"))
(defmethod qualifier-test ((class not-so-simple-class))
   (print "primary not-so-simple-class"))

(qualifier-test (make-instance 'not-so-simple-class))
(qualifier-test (make-instance 'simple-class))

;;; The order of execution is therefore:-
;;;
;;; 1.  Evaluate :before methods in most-specific-first order.
;;; 2.  Evaluate most specific primary method.
;;; 3.  Evaluate :after methods in least-specific-first order.
;;;
;;; This is particularly useful if you require superclasses to do
;;; some pre/post processing around the subclasses' code.
;;;

;;; The :around qualifier is a little different.  If defined at all
;;; it is executed first and the rest of the methods may not be
;;; executed at all.  Typically, within an :around method, the
;;; (call-next-method) function is called.  This invokes the
;;; three steps described above.

(defmethod qualifier-test :around ((class simple-class))
  (print "Enter around simple-class")
  (call-next-method)
  (print "Exit around simple-class"))

(defmethod qualifier-test :around ((class not-so-simple-class))
  (print "Enter around not-simple-class")
  (call-next-method)
  (print "Exit around not-simple-class"))

(qualifier-test (make-instance 'not-so-simple-class))
(qualifier-test (make-instance 'simple-class))

;;; While the values of the :before and :after methods are ignored
;;; (the returned value is the value of the primary method), the
;;; returned value here is the value returned by the :around method.
;;;
;;; If there are several applicable :around methods, only the most specific
;;; is called.  Actually, when (call-next-method) is called, the next
;;; most specific :around method is called; if none is found, then
;;; the regular framework of :before, primary and :after methods are called.
;;;
;;; :around methods are most useful in wrapping the method calls within
;;; some computation.
;;;

;;;
;;; Specializing built-in methods
;;;
;;; CLOS allows built-in methods to be specialized.  The most
;;; common built-in methods to be specialized are the
;;; (print-object), (describe) and (initialize-instance) objects.

;;; The (print-object) methods is used by LISP whenever the
;;; object needs to be printed.  For example, we can specialize
;;; the printing of our 3-vector as follows :-

(defmethod print-object ((vec 3-vector) stream)
  (format stream "#<~S x=~d, y=~d, z=~d>"
	  (type-of vec)
	  (x vec) (y vec) (z vec)))

(make-instance '3-vector)

;;; Similarly,

;;; You will get a continuable error.  Select to flush the
;;; existing definition.
(defmethod describe ((vec 3-vector))
  (format t "~S is a ~S with coordinates x=~d, y=~d, z=~d."
	  vec (type-of vec) (x vec) (y vec) (z vec))
  (values))                                                 ;; does not return any value.

(describe (make-instance '3-vector))


;;; (initialize-instance) is called when an instance is initialized
;;; by CLOS.  This means initializing the slots to the supplied :initargs
;;; or with the defined :initforms etc.  If you overwrite this method
;;; with a primary method, no initialization of that sort will be done!
;;; So, typically, either a :before or :after method is defined.

(defmethod initialize-instance :before ((vec 3-vector) &rest init-args)
  (print "Before default initialization"))

(defmethod initialize-instance :after ((vec 3-vector) &rest init-args)
  (print "After default initialization")
  (terpri))                               ;; carriage return

(make-instance '3-vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:
