;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: generic-fns.lisp
;;;  Author: Eero Simoncelli
;;;  Description: Generic function defs for exported methods
;;;  Creation Date: 3/91
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file exists solely to get the arg-names and doc-strings
;;; right!

#|
;;; *** Many functions are missing from this file! Here's the list:
(do-external-symbols (sym 'obvius)
  (when (and (fboundp sym)
	     (typep (symbol-function sym) 'clos::standard-generic-function))
    (print sym)))
|#

(in-package obvius)

(export '(fill!  zero!  copy  similar

	  add  sub  mul  div  linear-xform  negate
	  abs-value  square  square-root  power natural-logarithm
	  point-operation  periodic-point-operation
	  gamma-correct
	  clip round. truncate.  floor. quantize magnitude square-magnitude

	  mean-square-error mean-abs-error max-abs-error
	  square-error abs-error 
	  point-minimum point-maximum
	  greater-than less-than equal-to
	  greater-than-or-equal-to less-than-or-equal-to
	  correlate convolve
	  
	  minimum  maximum  range entropy
	  minimum-location  maximum-location
	  mean  variance  skew  kurtosis

	  flip-x  flip-y  flip-xy
	  circular-shift  transpose  paste  side-by-side
	  crop  make-slice
	  upsample downsample subsample

	  apply-filter expand-filter blur gauss-in gauss-out
	  fft hilbert-transform

	  matrix-mul outer-product cross-product
	  matrix-transpose-mul matrix-mul-transpose
	  matrix-transpose normalize

	  make-histogram zero-crossings
	  print-values
	  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; These are to avoid export errors when loading modules:

(export '(color-image make-color-image color-image-p color-transform rgb->yiq yiq->rgb))
(export '(color-picture))
(export '(gaussian-pyramid make-gaussian-pyramid gaussian-pyramid-p
	  laplacian-pyramid make-laplacian-pyramid laplacian-pyramid-p
	  build access collapse
	  mul sub div add square-error abs-error linear-xform clip
	  abs-value square square-root copy zero! point-operation 
	  apply-filter expand-filter blur
	  greater-than greater-than-or-equal-to less-than less-than-or-equal-to
	  minimum maximum range))
(export '(overlay))
(export '(contour-plot))
(export '(singular-value-decomposition svd singular-values with-static-svd with-svd
	  principal-components row-space row-null-space col-space col-null-space
	  singular-values left-singular-matrix right-singular-matrix
	  solve-eigenvalue matrix-inverse condition-number determinant
	  quadratic-decomposition))
(export '(qr-decomposition
	  col-space-qr row-space-qr
	  col-null-space-qr row-null-space-qr
	  determinant-qr))
(export '(regress))
(export '(row rows displaced-row displaced-rows col cols
	  normalize-rows normalize-cols
	  add-rows sub-rows mul-rows div-rows add-cols sub-cols
	  paste-rows paste-cols append-rows append-cols shuffle-rows swap-rows sort-rows
	  sum-rows mean-rows covariance-rows sum-cols mean-cols covariance-cols))
(export '(check-probability check-integer
          weibull inverse-weibull
	  detection-weibull inverse-detection-weibull 2afc-weibull inverse-2afc-weibull
          detection-log-normal inverse-detection-log-normal
          chi-square cumulative-chi-square cumulative-f
          poisson cumulative-poisson poisson-noise factorial
          cumulative-binomial binomial binomial-noise bernoulli-noise
	  binomial-coefficient log-binomial-coefficient
	  standard-deviation sample-variance sample-standard-deviation standard-error
          confidence-interval mean-confidence-interval))
(export '(stepit-fit))
(export '(with-simplex run-simplex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric fill! (arg value)
  (:documentation
   "Destructively fill arg with value pointwise."))

(defgeneric zero! (arg)
  (:documentation
   "Destructively fill arg with zeros."))

(defgeneric copy (arg &key ->)
  (:documentation
   "Make a copy of arg, with no shared structure."))

(defgeneric similar (arg &key ->)
  (:documentation
   "Make a new, empty, viewable of the same size and shape as arg."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Arithmetic Operations:

(defgeneric add (arg1 arg2 &key ->)
  (:documentation "Pointwise addition."))

(defgeneric sub (arg1 arg2 &key ->)
  (:documentation "Pointwise subtraction."))

(defgeneric mul (arg1 arg2 &key ->)
  (:documentation "Pointwise multiplication."))

(defgeneric div (arg1 arg2 &key zero-val suppress-warning ->)
  (:documentation
   "Pointwise division.  Divide by zero errors signal a warning and
values are set to plus or minus the value of zero-val.  The warnings
can be avoided by setting suppress-warning to t"))

(defgeneric negate (arg &key ->)
  (:documentation "Pointwise multiplication by -1."))

(defgeneric linear-xform (arg scale offset &key ->)
  (:documentation "Pointwise linear transform: (scale * arg) + offset."))

(defgeneric square (arg &key ->)
  (:documentation "Pointwise squaring operator."))

(defgeneric square-root (arg &key ->)
  (:documentation "Pointwise square-root operator."))

(defgeneric abs-value (arg &key ->)
  (:documentation "Pointwise absolute-value operator."))

(defgeneric power (arg1 arg2 &key ->)
  (:documentation "Pointwise power: arg1 ^ arg2."))

(defgeneric natural-logarithm (arg &key zero-val ->)
  (:documentation
   "Pointwise natural-log.  Log of zero signals a warning and values
are set to the value of :zero-val. :zero-val defaults to
*div-by-zero-result*."))

(defgeneric gamma-correct (arg gamma &key below above binsize ->)
  (:documentation
   "Gamma correction.  Clips between :below and :above.  Calls
point-operation with gamma-correction function and :binsize keyword."))

(defgeneric point-operation (vbl function &key binsize ->)
  (:documentation
   "Apply a function to a viewable, point by point.  If the binsize
keyword is a number, a lookup table is computed with the given
binsize.  If binsize is nil (the default), the function will be called
at each point.  The default binsize is set to create a lookup table of
256 values spanning the range of the viewable."))

(defgeneric periodic-point-operation (vbl function period &key binsize ->)
  (:documentation
   "Apply function to viewable, point-by-point, premapping all viewable
values into the range [0,period).  Note that the function doesn't
actually have to periodic by itself.  If binsize is not nil, creates a
lookup table for the function over the range of the period with this
resolution and computes the function using linear interpolation.
Otherwise, call the function at each point."))

(defgeneric clip (arg min max &key ->)
  (:documentation
   "Pointwise clip operation.  Limits the values of arg to be between
min and max."))

(defgeneric round. (arg &key divisor ->)
  (:documentation
   "Point operation that rounds to nearest integer.  If divisor is
supplied, values are divided by this number before rounding (same as
Common Lisp round function)."
   ))

(defgeneric truncate. (arg &key divisor ->)
  (:documentation
   "Point operation that truncates to nearest integer (always toward
zero).  If divisor is supplied, values are divided by this number
before truncation (same as Common Lisp truncate function)."
   ))

(defgeneric floor. (arg &key divisor ->)
  (:documentation
   "Point operation that computes greatest integer less than value.  If
divisor is supplied, values are divided by this number before flooring
(same as Common Lisp floor function)."
   ))

(defgeneric quantize (arg  &key binsize origin ->)
  (:documentation
   "Pointwise (scalar) quantization.  :binsize defaults to the range of
values in arg divided by (get-default 'discrete-function :size).
:origin specifies the center-point of one of the bins and defaults to
the mean value of arg.  Quantized values are set to the mean value
of each bin." ))

(defgeneric magnitude (arg &key ->)
  (:documentation "Magnitude of complex image."))

(defgeneric square-magnitude (arg &key ->)
  (:documentation "Square magnitude of complex image."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Comparison Operations

(defgeneric correlate (arg1 arg2 &key x y x-dim y-dim ->)
  (:documentation "Correlate arg1 with arg2, circular-shifting one over the other."))

(defgeneric convolve (arg1 arg2 &key x y x-dim y-dim ->)
  (:documentation "Convolve arg1 with arg2, circular-shifting one over the other."))

(defgeneric mean-square-error (arg1 arg2)
  (:documentation "Compute mean value of squared difference of arg1 and arg2."))

(defgeneric almost-equal (arg1 arg2 &key tolerance)
  (:documentation "Compare arg1 and arg2 up to an additive tolerance."))

(defgeneric mean-abs-error (arg1 arg2)
  (:documentation "Compute mean value of abs-value of difference of arg1 and arg2."))

(defgeneric max-abs-error (arg1 arg2)
  (:documentation "Compute maximum of abs-value of difference of arg1 and arg2."))

(defgeneric point-minimum (arg1 arg2 &key ->)
  (:documentation "Pointwise minimum."))

(defgeneric point-maximum (arg1 arg2 &key ->)
  (:documentation "Pointwise maximum."))

(defgeneric square-error (arg1 arg2 &key ->)
  (:documentation "Pointwise squared difference."))

(defgeneric abs-error (arg1 arg2 &key ->)
  (:documentation "Pointwise abs-value difference."))

(defgeneric greater-than (arg1 arg2 &key ->)
  (:documentation "Pointwise comparison.  Returns a binary viewable:
                   1 if arg1>arg2, 0 otherwise"))

(defgeneric less-than (arg1 arg2 &key ->)
  (:documentation
   "Pointwise comparison.  Returns a binary viewable: 1 if arg1<arg2, 0
otherwise"))

(defgeneric equal-to (arg1 arg2 &key ->)
  (:documentation
   "Pointwise comparison.  Returns a binary viewable: 1 if arg1=arg2, 0
otherwise"))

(defgeneric greater-than-or-equal-to (arg1 arg2 &key ->)
  (:documentation
   "Pointwise comparison.  Returns a binary viewable: 1 if arg1>=arg2, 0
otherwise"))

(defgeneric less-than-or-equal-to (arg1 arg2 &key ->)
  (:documentation
   "Pointwise comparison.  Returns a binary viewable: 1 if arg1<=arg2, 0
otherwise"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Statistics:

(defgeneric mean (arg &key ignore-zeros)
  (:documentation
   "Mean value of histogram of viewable values.  If ignore-zeros is t,
ignore all points with value = 0."))

(defgeneric variance (arg &key ignore-zeros)
  (:documentation
   "Variance of histogram of viewable values.  If ignore-zeros is t,
ignore all points with value = 0."))

(defgeneric skew (arg)
  (:documentation
   "Skew (3rd order moment) of histogram of viewable values.  If
ignore-zeros is t, ignore all points with value = 0."))

(defgeneric kurtosis (arg)
  (:documentation
   "Kurtosis (4th order moment) of histogram of viewable values.  If
ignore-zeros is t, ignore all points with value = 0."))

(defgeneric minimum (arg)
  (:documentation "Minimum value of viewable values."))

(defgeneric maximum (arg)
  (:documentation "Minimum value of viewable values."))

(defgeneric range (arg)
  (:documentation
   "Range of values in arg.  Returns, as multiple values range, minimum,
and maximum."))

;;; ars are same as make-histogram
(defgeneric entropy (arg &key size binsize bincenter range)
  (:documentation "No doc."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Geometric Operations (for 2D viewables only)

(defgeneric circular-shift (arg &key y x -> &allow-other-keys)
  (:documentation "Shifts by given number of pixels, wrapping around at the edges"))

(defgeneric transpose (arg &key ->)
  (:documentation "Transpose."))

(defgeneric paste (arg base &key y x -> &allow-other-keys)
  (:documentation "Pastes arg into base at specified position."))

(defgeneric crop (arg &key y x y-dim x-dim -> &allow-other-keys)
  (:documentation "Crop to specified size."))

(defgeneric make-slice (arg &key x y z ->)
  (:documentation
   "Slice of viewable. x, y, and z are mutually exclusive. Z used only on sequences."))

(defgeneric side-by-side (arg1 arg2 &key space ->)
  (:documentation "Pastes both args next to each other with space in between."))

(defgeneric subsample (arg &key start-vector step-vector hex-start ->)
  (:documentation "No doc written."))

(defgeneric downsample (arg &key start-vector step-vector hex-start ->)
  (:documentation "No doc written."))

(defgeneric upsample (arg &key start-vector step-vector hex-start ->)
  (:documentation "No doc written."))

;;; *** Add these... But they are not generic functions...
;; flip-x
;; flip-y

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Synthetic image operations:

;;; *** must add these, but they are not generic functions...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Filter operations

(defgeneric apply-filter (filter arg &key direction ->)
  (:documentation "No doc written."))

(defgeneric expand-filter (filter arg &key zero direction ->)
  (:documentation "No doc written."))

(defgeneric blur (arg &key kernel level edge-handler ->)
  (:documentation
   "Blur arg with specified filter kernel.  Filters and downsamples,
then filters and upsamples.  Keyword :level specifies how many times
to repeat  filter/downsample/upsample operations."))

(defgeneric gauss-in (arg &key kernel resample edge-handler ->)
  (:documentation "Filters with specified kernel and upsamples by resample factor."))

(defgeneric gauss-out (arg &key kernel resample edge-handler ->)
  (:documentation "Filters with specified kernel and downsamples by resample factor."))

(defgeneric fft (arg &key inverse ->)
  (:documentation "Computes FFT or inverse FFT on arg."))

(defgeneric hilbert-transform (arg &key orientation ->)
  (:documentation "Hilbert transform about give orientation axis."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Matrix

;;; Do these, but they are not generic-fns

;; qr-decompose, basis-for-left-nullspace, determinant,
;; singular-value-decompose, identity-matrix, matrix-inverse

(defgeneric matrix-mul (arg1 arg2 &key ->)
  (:documentation "Args may be vectors or arrays."))

(defgeneric outer-product (vec1 vec2 &key ->)
  (:documentation ""))

(defgeneric cross-product (vec1 vec2 &key ->)
  (:documentation ""))

(defgeneric matrix-transpose-mul (arg1 arg2 &key ->)
  (:documentation "Args may be vectors or arrays."))

(defgeneric matrix-mul-transpose (arg1 arg2 &key ->)
  (:documentation "Args may be vectors or arrays."))

(defgeneric matrix-transpose (arg &key ->)
  (:documentation ""))

(defgeneric normalize (vec &key ->)
  (:documentation "Returns multiple values, normalized vector and norm of vector."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Misc

(defgeneric make-histogram (arg &key range binsize bincenter size name)
  (:documentation "No doc yet."))

(defgeneric zero-crossings (arg &key ->)
  (:documentation "No doc yet."))

(defgeneric print-values (arg &key x y x-size y-size)
  (:documentation "Print values starting at (y,x)."))


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
