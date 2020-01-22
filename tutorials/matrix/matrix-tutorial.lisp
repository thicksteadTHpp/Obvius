;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: matrix-tutorial.lisp
;;;  Author: Heeger
;;;  Description: 
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This tutorial is a review of some basic concepts in linear algebra.  For a
;; detailed introduction, consult a linear algebra text.  Linear Algebra and
;; its Applications by Gilbert Strang (Harcourt, Brace, Jovanovich, 1988) is
;; excellent.


;;; Evaluate this to get started:

(progn
  (obv-require :matrix)
  (obv-require :statistics))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Matrix multiplication in Obvius:

;; Obvius provides several functions for doing matrix multiplication.

(setq A (make-matrix '((1 2)
		       (3 4))))
(setq B (make-matrix '((-1 2)
		       (3 -4))))

;; Multiply A by B:
(print-values (matrix-mul A B))

;; Multiply A-transpose by B:
(print-values (matrix-transpose-mul A B))

;; Multiply A by B-transpose:
(print-values (matrix-mul-transpose A B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SVD (Singular Value Decomposition):

;; The SVD decomposes a matrix into the product of the three
;; components:
;;     A = U S V^t
;; where ^t means transpose.  A is the original NxM matrix, U is an NxN
;; orthonormal matrix, V is an MxM orthonormal matrix, and S is an NxM matrix
;; with non-zero elements only along the diagonal.  Note that Obvius returns S
;; as a vector whose length is the smaller of the two dimensions.

;; Let's try it on a randomly filled 10x5 matrix:
(setq A (make-array '(10 5) :element-type 'single-float))
(randomize A 1.0 :-> A)
(multiple-value-setq (S U V) (singular-value-decomposition A))

;; We can check to see that the decomposition worked.  Multiplying the
;; U, S, and V matrices should give back the original matrix.  First
;; we need to put the entries of S into a matrix:
(setq Smat (make-array (dimensions A) :element-type 'single-float :initial-element 0.0))
(loop for i from 0 below (x-dim S) do
      (setf (aref Smat i i) (aref S i)))

;; Then we use matrix multiplication to "reconstruct" A:
(setq new-A (matrix-mul-transpose (matrix-mul U Smat) V))

;; Then we look at the difference.  The "range" function returns the range of
;; values in the difference matrix (max minus min), the minimum value in the
;; difference matrix, and then the maximum value in the difference matrix.  All
;; of these numbers should be pretty close to zero (1e-6 is small enough):
(range (sub A new-A))

;; Check that U and V are orthonormal matrices.  The "identity-p" function
;; returns the matrix if the matrix is (very close to being) an identity
;; matrix.  It returns nil if the matrix is not an identity matrix:
(and (identity-p (matrix-transpose-mul V V))
     (identity-p (matrix-mul-transpose V V))
     (identity-p (matrix-transpose-mul U U))
     (identity-p (matrix-mul-transpose U U)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Four fundamental subspaces:

;; Consider a matrix A as a linear transform, y=Ax, that transforms N-dimension
;; vectors, x, into M-dimensional vectors, y.

;; If the matrix A is singular then there is some subspace of x (some subspace
;; of M-space) that is mapped to zero, Ax=0.  This is called the
;; "row-nullspace" of A.

;; There is also a subspace of y (some subspace of M-space) that can be reached
;; by A.  This is called the "column-space" of A.  The dimension of the
;; column-space is called the rank of A.  The rank of a matrix is, therefore,
;; equal to the number of non-zero singular values.

;; The SVD explicitly constructs orthonormal bases for the row-nullspace and
;; column-space of A.  The columns of U, whose same-numbered elements in S are
;; non-zero, are an orthonormal set of basis vectors that span the column-space
;; of A.  The remaining colums of U span the row-nullspace of A^t (also called
;; the column-nullspace of A).

;; The columns of V (rows of V^t), whose same-numbered elements in S are zero,
;; are an orthonormal set of vectors that span the row-nullspace of A.  The
;; remaining columns of V span the column-space of A^t (also called the
;; row-space of A).

;; Obvius provides functions to get each of these basis sets.  Here's a simple
;; example:
(print-values (setq m (make-matrix '((1 0 0)
				     (0 1 0)))))

;; Basis vectors for the col-space of this matrix are the columns of the
;; following matrix:
(print-values (col-space m))

;; Basis vectors for the row-space are the rows of the following matrix.  Note
;; that there are only 2 of them spanning a two dimensional subspace of 3
;; space:
(print-values (row-space m))

;; Together the row-space and row-nullspace-basis matrices span all of 3 space.
;; A basis vector for the row-null-space is given by the row of the following
;; matrix:
(print-values (row-null-space m))

;; For our matrix, there is no col nullspace so the following gives an error
;; message:
(print-values (col-null-space m))

;; The row-space and row-null-space are orthogonal.  So we get all zeros if we
;; multiply the rows of one by the rows of the other:
(print-values (matrix-mul-transpose (row-space m) (row-null-space m)))

;; Here's another example in which one of the columns is a linear combination
;; of the others.
(print-values (setq A (make-matrix '((1 -1 0)
				     (0 1 -1)
				     (1 0 -1)))))

;; It is easy to see that A has rank 2 by noting that you can get the third
;; column of A by summing the first 2 columns and then multiplying by -1.  In
;; fact, the vector (1,1,-1) is in the column nullspace of A.  So is any scalar
;; multiple of (1,1,-1).  It is called the column nullspace because it takes
;; the columns to zero: (1,1,-1) A = 0.
(print-values (col-null-space A))
(print-values (matrix-mul (vectorize (col-null-space A)) A))

;; The vector (1,1,1) is in the row-nullspace of A.  So is any scalar multiple
;; of (1,1,1).  It's called the row-nullspace because it takes to rows to zero:
;; A (1,1,1)^t = 0.
(print-values (row-null-space A))
(print-values (matrix-mul A (columnize (row-null-space A))))

;; And the other two spaces:
(print-values (row-space A))
(print-values (col-space A))

;; Row-space and row-null-space are orthogonal:
(print-values (matrix-mul-transpose (row-space A) (row-null-space A)))

;; Col-space and col-null-space are orthogonal:
(print-values (matrix-transpose-mul (col-space A) (col-null-space A)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Solving Ax=b

(setq A (make-matrix '((1 -1 0)
		       (0 1 -1)
		       (1 0 -1))))

;; The product Ax is always a combination of the columns of A:

;;         (1 -1  0) (x0)      (1)      (-1)      ( 0)
;;    Ax = (0  1 -1) (x1) = x0 (0) + x1 ( 1) + x2 (-1)
;;         (1  0 -1) (x2)      (1)      ( 0)      (-1)

;; To solve Ax=b is to find a combination of the columns that gives b.  We
;; consider all possible combinations Ax, coming from all choices of x.  Those
;; products form the column space of A.  In the example, the columns lie in
;; 3-dimensional space, but their combinations fill out a plane (the matrix has
;; rank 2).  The plane goes through the origin since one of the combinations
;; has weights x0=x1=x2=0.  Some vectors b do not lie on the plane, and for
;; them Ax=b can not be solved exactly.  The system Ax=b has an exact solution
;; only when the right side b is in the column space of A.

;; The vector b=(2,3,4) is not in the column space of A so Ax=b has no
;; solution.  The vector b=(2,3,5) does lie in the plane (spanned by the
;; columns of A) so there is a solution, x=(5,3,0).
(setq x (make-matrix '(5 3 0)))
(print-values (matrix-mul A (columnize x)))

;; However, there are other solutions as well.  x=(6,4,1) will work:
(setq x (make-matrix '(6 4 1)))
(print-values (matrix-mul A (columnize x)))

;; In fact, we can take x=(5,3,0) and add any scalar multiple of y=(1,1,1)
;; since (1,1,1) is in the null space.  We can write it this way:
;;    A (x + c y) = Ax + A (cy) = Ax + c Ay = Ax + c 0 = Ax

;; If an NxN matrix A has linearly independent columns then
;;  (1) the row nullspace contains only the point 0
;;  (2) the solution to Ax=b (if there is one) is unique
;;  (3) the rank of A is N
;; In general any two solutions to Ax=b differ by a vector in the row
;; nullspace.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Regression: When b is not in the column space of A, we can still find a
;;; vector x that comes the closest to solving Ax=b.

;; We motivate this by thinking about fitting empirical data.  We change
;; notation a bit (hopefully without too much confusion) and try to solve Ap=y
;; instead of Ax=b.  Same thing really.  It's just a bit more convenient this
;; way.

;; Let's simulate collecting some data in a certain experiment.  For each of a
;; number of test conditions, x, we measure an outcome, y.  And let's assume
;; that the relationship between y's and x's is given by a polynomial:
;;       y = 3 x^2 + x - 4.
;; It should be obvious that this was an arbitrary choice.  Let's use this
;; formula to simulate some data.  First, we make a vector of a bunch of x
;; values (evenly spaced between 0 and 1).  Then we compute y for each of those
;; x values:
(setq N 10)
(setq xvals (make-matrix (loop for i from 0 below N
			       collect
			       (/ i N))))
(setq yvals (make-matrix (loop for i from 0 below N
			       for x = (aref xvals i)
			       collect
			       (+ (* 3 (sqr x)) x -4))))

;; Look at the data:
(make-scatter yvals xvals)

;; Now let's play like we don't know the exact relationship between y and x.
;; In particular, we have the data (xvals and yvals) and we know that y is a
;; second-order polynomial of x:
;;    p0 + p1 x + p2 X^2
;; but we don't know the parameter values p=(p0,p1,p2)

;; How might we solve for those parameter values?  We build a matrix A whose
;; columns depend on the x values.  In particular, the first column of A has a
;; bunch of x^2 values, the second column of A has a bunch of x values and the
;; third column has a bunch of 1's.
(setq A (make-array (list N 3) :element-type 'single-float))
(loop for i from 0 below N
      for x = (aref xvals i)
      do
      (setf (aref A i 0) (sqr x))
      (setf (aref A i 1) x)
      (setf (aref A i 2) 1.0))

;; Then we solve: y = A p, where we know y and we just constructed A.  If A
;; were a square (full rank) matrix, this would be easy; we'd just invert A to
;; get: p = A-inverse y.  Since A is not square, we need to work a bit harder.
;; The function "matrix-inverse" uses the SVD to compute the pseudo-inverse of
;; A.  And since yvals was defined above as a (1xN) row vector, we use the
;; function "columnize" to turn it into a column vector before multiplying:
(print-values (setq p (matrix-mul (matrix-inverse A) (columnize yvals))))

;; This worked perfectly because there was no noise in the data.  Let's do it
;; again with (simulated) noisy data.  The function "gaussian-noise" returns a
;; random draw from a normal distribution, given a mean and variance.  Simulate
;; the noisy data:
(progn
  (setq N 10)
  (setq xvals (make-matrix (loop for i from 0 below N
				 collect
				 (/ i N))))
  (setq yvals (make-matrix (loop for i from 0 below N
				 for x = (aref xvals i)
				 collect
				 (+ (* 3 (sqr x)) x -4 (gaussian-noise 0.0 0.1)))))
  (make-scatter yvals xvals))

;; And estimate the parameters from the noisy data:
(print-values (setq p (matrix-mul (matrix-inverse A) (columnize yvals))))

;; With more data points it will work better.  Simulate the noisy data:
(progn
  (setq N 200)
  (setq xvals (make-matrix (loop for i from 0 below N
				 collect
				 (/ i N))))
  (setq yvals (make-matrix (loop for i from 0 below N
				 for x = (aref xvals i)
				 collect
				 (+ (* 3 (sqr x)) x -4 (gaussian-noise 0.0 0.1)))))
  (make-scatter yvals xvals))

;; And estimate the parameters from the noisy data:
(progn
  (setq A (make-array (list N 3) :element-type 'single-float))
  (loop for i from 0 below N
	for x = (aref xvals i)
	do
	(setf (aref A i 0) (sqr x))
	(setf (aref A i 1) x)
	(setf (aref A i 2) 1.0))
  (print-values (setq p (matrix-mul (matrix-inverse A) (columnize yvals)))))

;; The xvals and yvals are vectors; according to our (second-order polynomial)
;; model, each element of yvals is a quadratic function of the corresponding
;; element of xvals.  A is an Nx3 matrix, and the col-space of A is a
;; 3-dimensional subspace of N space.  In particular, the columns of A are a
;; basis for all possible second-order polynomials.  The product, Ap, is a
;; particular linear combination of those basis vectors, hence, it is a
;; particular second order polynomial.  If y=Ap (for some/any p), then we say
;; that y is in the column space of A.  That's what we had in the noiseless
;; case.  The y vector was exactly equal to Ap for the right choice of p.
;; After adding noise, however, y was no longer in the column space of A.
;; There was no choice of p such that y=Ap exactly.  The regression solution
;; found a p such that Ap came as close as possible to y.  In other words, it
;; found y-est = A p-est, where the distance between y and y-est was minimized
;; given that y-est was forced to be in the column space of A.

;; All of this generalizes to many other situations, e.g., to models other than
;; second-order polynials).  In particular, standard linear regression is a
;; special case in which p2 is zero.  In that case, A only has 2 columns (x's
;; and 1's).  We can do it for other hairier (non-polynomial) functions too.
;; Let's simulate noisy data for: y = 10 log(x) + 20.
(progn
  (setq N 100)
  (setq xvals (make-matrix (loop for i from 0 below N
				 collect
				 (/ (+ 1 i) N))))
  (setq yvals (make-matrix (loop for i from 0 below N
				 for x = (aref xvals i)
				 collect
				 (+ (* 10 (log x 10)) 20 (gaussian-noise 0 1)))))
  (make-scatter yvals xvals))

;; And estimate the parameters from the noisy data:
(progn
  (setq A (make-array (list N 2) :element-type 'single-float))
  (loop for i from 0 below N
	for x = (aref xvals i)
	do
	(setf (aref A i 0) (log x 10))
	(setf (aref A i 1) 1.0))
  (print-values (setq p (matrix-mul (matrix-inverse A) (columnize yvals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Covariance...

;; This section of the tutorial deals with multi-dimensional data sets.  Each
;; data point represents one test condition, e.g., a data point might be the
;; height and weight of a person represented as a vector (height,weight).  Or
;; each data point might be the intensity values of an image represented as a
;; very long vector (p1,p2,...,pN), where p_i is the intensity at the i_th
;; pixel.

;; Here's a function that generates a simulated multi-dimensional data set.  N
;; is the length of each data vector.  M is the number of simulated data
;; points.  You pass: M along with the desired mean and covariance.  The
;; function "gaussian-noise" generates random draws from a multi-dimensional
;; normal distribution.  Compile this function using C-c c:

(defun make-data (M mean covariance)
  (unless (= (length mean) (row-dim covariance) (col-dim covariance))
    (error "Dimensions of mean and covariance don't match")) 
  (let* ((N (length mean))
	 (data-transpose (make-array (list M N) :element-type 'single-float))
	 (data (make-array (list N M) :element-type 'single-float)))
    (loop for i from 0 below M do
	  (gaussian-noise mean covariance :-> (displaced-row i data-transpose)))
    (matrix-transpose data-transpose :-> data)
    data))

;; Now we choose a mean and covariance.  Then we call our "make-data" function
;; to generate the data.  Then we call "make-scatter" to display a scatter
;; plot of the data.  In this example, we generate a 2-dimensional data set so
;; we can easily look at a scatter plot of the data.

(setq mean (make-matrix '(10 2)))
(setq covariance (make-matrix '((1 0.8)
				(0.8 1))))
(setq data (make-data 100 mean covariance))
(make-scatter (displaced-row 1 data) (displaced-row 0 data))
(setp :x-range '(7 13) :x-tick-step 1
      :y-range '(-1 5) :y-tick-step 1
      :zoom 32)

;; Data is an 2xM array.  The columns of data are the data points that got
;; plotted in the scatter plot.  The xvalues are in the first row of data, and
;; the yvalues are in the second row.  Note how we used the "displaced-row"
;; function to make the scatter plot; look at the definition of this function
;; using C-c dot.  If you don't know about displaced arrays, look it up in a
;; common lisp book; they are very useful.  The correlation coefficient in
;; this example is 0.8, so the scatter plot slopes up and to the right at a 45
;; degree angle.

;; Next let's compute the mean of the data set using the function "mean-cols".
;; It will be close (but not exactly equal) to the mean that we specified at
;; the outset.
(print-values (mean-cols data))

;; We compute the covariance of the data in two steps.  First we use the
;; "sub-cols" function to subtract the mean of the cols from each of the
;; columns to give us a new matrix D.  Then we compute D D^t, and divide by
;; the number of data points, M.
(setq D (sub-cols data (mean-cols data)))
(setq D-Dt (div (matrix-mul-transpose D D) (col-dim D)))

;; The empirical covariance is close to the covariance that we specified at the
;; outset.
(print-values D-Dt)

;; Obviously, the empirical covariance would be closer if we used a bigger data
;; set (larger M).  Try it with M=1000.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Principle Components Analysis:

;; Compute the svd of D.  The cols of u span the col-space of D.  These are
;; also the eigenvectors of the covariance, D D^t.  We grab the two
;; eigenvectors using the "col" function that makes a column vector by copying
;; a column from a matrix:
(multiple-value-setq (s u v) (svd D))
(print-values (setq e0 (col 0 u)))
(print-values (setq e1 (col 1 u)))

;; Show that these are eigenvectors of D D^t.  For v to be an eigenvector of a
;; matrix A means that: Av=lv for some scalar l.  In other words, passing v
;; through the matrix only affects its length, not its direction.
(setq D-Dt (div (matrix-mul-transpose D D) (col-dim D)))
(print-values (setq D-Dt-e0 (matrix-mul D-Dt e0)))
(print-values (setq D-Dt-e1 (matrix-mul D-Dt e1)))

;; We use the "scalar-multiple" function to check that these really are
;; eigenvectors.  This function returns nil if two vectors are not scalar
;; multiples of one another:
(scalar-multiple e0 e1)

;; It returns the scalar if the two vectors are scalar multiples:
(scalar-multiple e0 D-Dt-e0)
(scalar-multiple e1 D-Dt-e1)

;; The eigenvector corresponding to the largest eigenvalue is called the
;; first principal component.  The "svd" function returns the eigenvectors in
;; order, so e0 is the eigenvector with the largest eigenvalue.  The first
;; principal component is the unit vector with the largest projection onto the
;; data set.  The e0 that you computed should be very close to (.707,.707):
(print-values e0)

;; Note that you might also have gotten (-.707,-.707).  That's just as good.
;; With either sign, it points in the elongated direction of the scatter plot,
;; hence it has the largest projection onto the data set.  The other
;; eigenvector, e1, points in the perpendicular direction.  The e1 that you
;; computed should be very close to (-.707,.707) or (.707,-.707):
(print-values e1)

;; Again, the first principal component, e0, is the unit vector with the
;; largest projection onto the data set.  The projection of e0 onto the data
;; is:
(vector-length (matrix-transpose-mul e0 D))

;; It's helpful to draw the shape of the matrices on a piece of paper: data is
;; a (2xM) matrix, e0 is (2x1) column vector, so (matrix-transpose-mul e0 data)
;; is a (1xM) row vector .  Each element of this row vector is the length of
;; the projection of a single data point onto the unit vector e0.  The
;; vector-length of this row vector gives the sum of the projection lengths.

;; The projection onto the other eigenvector is smaller:
(vector-length (matrix-transpose-mul e1 D))

;; The point is that you know a lot about a data point by knowing only the
;; projection of that data point onto e0.  Let's compute the vector-distance
;; between a data point and its projection onto e0:
(setq data-point (col 0 D))
(vector-distance data-point (mul (dot-product e0 data-point) e0))

;; This distance is pretty small because you know a lot just by knowing
;; the projection onto e0.  And now, the average of the vector-distances (for
;; all the data points):
(/ (loop for i from 0 below (col-dim D)
	 for data-point = (col i D)
	 sum
	 (vector-distance data-point (mul (dot-product e0 data-point) e0)))
   (col-dim D))

;; Compare this number to what you would get by using some other arbitrary unit
;; vector.  Evaluate the following a bunch of times.  It picks a random unit
;; vector, prints that vector, then projects the data set onto that vector.
;; The numbers you get will always be bigger than or equal to what you got
;; using e0.  When the random unit vector is close to e0, you get a smaller
;; number.  When it is far from e0, you get a larger number.
(progn
  (print-values (setq unit-vector (normalize (randomize (similar e0) 1.0))))
  (/ (loop for i from 0 below (col-dim D)
	   for data-point = (col i D)
	   sum
	   (vector-distance data-point (mul (dot-product unit-vector data-point) unit-vector)))
     (col-dim D)))

;; Each data point is a vector of 2 numbers.  If you had to summarize each
;; data point with one number, what number would you choose?


;; Let's do it all again for a data set with a different covariance matrix:
(setq mean (make-matrix '(0 0)))
(setq covariance (make-matrix '((4 0)
				(0 1))))
(setq data (make-data 100 mean covariance))
(make-scatter (displaced-row 1 data) (displaced-row 0 data))
(setp :x-range '(-5 5) :x-tick-step 1
      :y-range '(-5 5) :y-tick-step 1
      :zoom 16)
(setq D (sub-cols data (mean-cols data)))
(multiple-value-setq (s u v) (svd D))
(print-values (setq e0 (col 0 u)))
(print-values (setq e1 (col 1 u)))

;; And for a higher dimensional data set:
(setq mean (make-matrix '(0 0 0)))
(setq covariance (make-matrix '((4 0 0)
				(0 2 0)
				(0 0 1))))
(setq data (make-data 100 mean covariance))
;; Note that data is now a 3x100 matrix.
(setq D (sub-cols data (mean-cols data)))
(multiple-value-setq (s u v) (svd D))
(print-values (setq e0 (col 0 u)))
(print-values (setq e1 (col 1 u)))
(print-values (setq e1 (col 2 u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Total Least Squares: Find unit vector x that comes closest to satisfying
;;; Ax=0.

;; *** Unfinished ***

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:

