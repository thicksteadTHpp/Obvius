;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: 2d-filters-tutorial.lisp
;;;  Author: Heeger
;;;  Description:
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; You should run this tutuorial only after running:
;;;   <obvius>/tutorials/signal-processing/linear-systems-tutorial.lisp
;;;   <obvius>/tutorials/signal-processing/sampling.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2D DFT:

;; First we set up our familiar Fourier transform matrices.  The rows
;; of these matrices are sines (imaginary part) and cosines (real
;; part) that make up the projection functions of the DFT for a finite
;; length, 1D signal (see linear-systems-tutorial.lisp).

(progn
  (setq Fourier-real (make-array '(32 32) :element-type 'single-float))
  (loop for k from 0 below 32 do
	(loop for n from 0 below 32 do
	      (setf (aref Fourier-real k n)
		    (* (sqrt 1/32) (cos (* 2/32 pi k n))))))
  (setq Fourier-imag (make-array '(32 32) :element-type 'single-float))
  (loop for k from 0 below 32 do
	(loop for n from 0 below 32 do
	      (setf (aref Fourier-imag k n)
		    (* (sqrt 1/32) (sin (* 2/32 pi k n)))))))

(make-complex-image (list (make-image Fourier-imag)
			  (make-image Fourier-real))
		    :-> "DFT projection functions")

;; In Obvius, a "complex-image" is a data structure that contains two
;; sub-images, one is the real-part and the other is the imaginary
;; part.  The make-complex-image function takes a list of two images
;; (first one is the imaginary part and the second one is the real
;; part).  The imaginary part is displayed on the left and the real
;; part is displayed on the right.

;; These very same matrices are used to compute the DFT of 2D images,
;; simply by applying the 1D DFT to both the rows and the cols of the
;; 2D image.  Let's try it on a sinusoidal grating test image:

(setq sin (make-sin-grating '(32 32) :x-freq (/ (* 2 pi 8) 64)
			    :y-freq (/ (* 2 pi 8) 64)))

;; First we multiply by the Fourier matrices on the left, that is,
;; computing the 1D DFT of each of the columns of the image:

(setq DFT-cols
      (make-complex-image
       (list
	(make-image (matrix-mul Fourier-imag (data sin)))
	(make-image (matrix-mul Fourier-real (data sin))))))

;; The first column of the real part of DFT-cols is the real part of
;; the DFT of the first column of sin:

(make-slice (real-part DFT-cols) :x 1 :-> "1st col of DFT")
(make-image (matrix-mul Fourier-real (columnize (data (make-slice sin :x 1))))
	    :-> "DFT of 1st col")

;; Likewise for the 2nd col (and all the other cols).  And likewise
;; for the imaginary part of each col:

(make-slice (imaginary-part DFT-cols) :x 1 :-> "1st col of DFT")
(make-image (matrix-mul Fourier-imag (columnize (data (make-slice sin :x 1))))
	    :-> "DFT of 1st col")

;; We use the same Fourier matrix to compute DFTs of each of the rows
;; of the image.  To compute the DFT of the cols we took the image
;; data, D, and the Fourier matrix, F, and multiplied on the left:
;;          DFT cols = F D
;; Now we multiply on the right to get the DFT of the rows:
;;          DFT rows = D F

(setq DFT-rows
      (make-complex-image
       (list
	(make-image (matrix-mul (data sin) Fourier-imag))
	(make-image (matrix-mul (data sin) Fourier-real)))))

;; We get the full 2D DFT by putting the two steps together.  First we
;; compute the DFT of the cols of an image.  Then we take that result
;; and compute the DFT of its rows:

(progn
  (setq DFT-real (sub (matrix-mul (data (real-part DFT-cols)) Fourier-real)
		      (matrix-mul (data (imaginary-part DFT-cols)) Fourier-imag)))
  (setq DFT-imag (add (matrix-mul (data (real-part DFT-cols)) Fourier-imag)
		      (matrix-mul (data (imaginary-part DFT-cols)) Fourier-real)))
  (setq DFT (make-complex-image
	     (list (make-image DFT-imag)
		   (make-image DFT-real)))))

And the magnitude:

(magnitude DFT)

;; What do the various transform coefficients mean?  The top left
;; corner of the DFT is the dc component (i.e., the average of the
;; image), computed by taking the average of the averages of each of
;; the columns.  The entire first row is the DFT of the averages of
;; the columns.  That is, we could have first computed the average of
;; each column to get a 1D signal and then taken the DFT of that 1D
;; signal.  For the second row, we could have first computed the 1
;; cycle per image component of each column to get a 1D signal and
;; then taken the DFT of that 1D signal.  For the first column, we
;; could have first computed the average of each row to get a 1D
;; signal, and then taken the DFT of that 1D signal.  Likewise every
;; other row and column.

;; In Obvius, the DFT can be computed efficiently using the fft
;; routine:

(setq fft (fft sin))
(mean-square-error DFT (fft sin))

;; Let's look at the DFT of some real images:

(load-image (merge-pathnames "images/einstein" obv::*obvius-directory-path*))
(setq fft-einstein (fft (gauss-out einstein) :post-center t))
(setq mag-einstein (magnitude fft-einstein))

;; Note that you don't see much.  That's because the dc component (and
;; low frequency components dominate).  Often, it is useful to display
;; the log of the magnitude of the DFT:

(setq log-mag-einstein (point-operation mag-einstein #'log))

;; Load some other images and look at their DFTs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2D DFT Examples:

;; Here we synthesize a number of test images and look at their power
;; spectra.  In each case, try to guess what the power spectrum will
;; look like before you compute it.

;; We'll zoom these images so they're easier to see:
(set-default 'gray :zoom 2)

(display (setq constant (fill! (make-image '(64 64)) 1.0))
	 'gray :pedestal 0 :scale 1)
(setq constant-ft (power-spectrum constant :center t))

(setq impulse (make-impulse '(64 64)))
(display (setq impulse-power (power-spectrum impulse :center t))
	 'gray :pedestal 0.0 :scale 2.5e-4)

(setq sin-x8 (make-sin-grating '(64 64) :x-freq (/ (* 2 pi 8) 64) :y-freq 0))
(setq sin-x8-power (power-spectrum sin-x8 :center t))

(setq sin-x4 (make-sin-grating '(64 64) :x-freq (/ (* 2 pi 4) 64) :y-freq 0))
(setq sin-x4-power (power-spectrum sin-x4 :center t))

(setq sin-x16 (make-sin-grating '(64 64) :x-freq (/ (* 2 pi 16) 64) :y-freq 0))
(setq sin-x16-power (power-spectrum sin-x16 :center t))

(setq sin-y8 (make-sin-grating '(64 64) :y-freq (/ (* 2 pi 8) 64) :x-freq 0))
(setq sin-y8-power (power-spectrum sin-y8 :center t))
 
(setq sin-x8-y8 (make-sin-grating '(64 64)
				  :x-freq (/ (* 2 pi 8) 64)
				  :y-freq (/ (* 2 pi 8) 64)))
(setq sin-x8-y8-power (power-spectrum sin-x8-y8 :center t))

(let ((sigma-x 4)
      (sigma-y 4))
  (setq gaussian1 (make-synthetic-image
		   '(64 64)
		   #'(lambda (y x)
		       (exp (- (+ (/ (sqr x) (* 2 (sqr sigma-x)))
				  (/ (sqr y) (* 2 (sqr sigma-y)))))))
		   :x-range '(-32 31)
		   :y-range '(-32 31))))
(setq gaussian1-power (power-spectrum gaussian1 :center t))

(let ((sigma-x 2)
      (sigma-y 2))
  (setq gaussian2 (make-synthetic-image
		   '(64 64)
		   #'(lambda (y x)
		       (exp (- (+ (/ (sqr x) (* 2 (sqr sigma-x)))
				  (/ (sqr y) (* 2 (sqr sigma-y)))))))
		   :x-range '(-32 31)
		   :y-range '(-32 31))))
(setq gaussian2-power (power-spectrum gaussian2 :center t))

(let ((sigma-x 2)
      (sigma-y 1))
  (setq gaussian3 (make-synthetic-image
		   '(64 64)
		   #'(lambda (y x)
		       (exp (- (+ (/ (sqr x) (* 2 (sqr sigma-x)))
				  (/ (sqr y) (* 2 (sqr sigma-y)))))))
		   :x-range '(-32 31)
		   :y-range '(-32 31))))
(setq gaussian3-power (power-spectrum gaussian3 :center t))

(let ((sigma-x 4)
      (sigma-y 4)
      (k 8))
  (setq gabor1 (make-synthetic-image
		'(64 64)
		#'(lambda (y x)
		    (* (exp (- (+ (/ (sqr x) (* 2 (sqr sigma-x)))
				  (/ (sqr y) (* 2 (sqr sigma-y))))))
		       (sin (/ (* 2 pi k x) 64))))
		:x-range '(-32 31)
		:y-range '(-32 31))))
(setq gabor1-power (power-spectrum gabor1 :center t))

(let ((sigma-x 2)
      (sigma-y 2)
      (k 8))
  (setq gabor2 (make-synthetic-image
		'(64 64)
		#'(lambda (y x)
		    (* (exp (- (+ (/ (sqr x) (* 2 (sqr sigma-x)))
				  (/ (sqr y) (* 2 (sqr sigma-y))))))
		       (sin (/ (* 2 pi k x) 64))))
		:x-range '(-32 31)
		:y-range '(-32 31))))
(setq gabor2-power (power-spectrum gabor2 :center t))

(let ((sigma-x 2)
      (sigma-y 1)
      (k 8))
  (setq gabor3 (make-synthetic-image
		'(64 64)
		#'(lambda (y x)
		    (* (exp (- (+ (/ (sqr x) (* 2 (sqr sigma-x)))
				  (/ (sqr y) (* 2 (sqr sigma-y))))))
		       (sin (/ (* 2 pi k x) 64))))
		:x-range '(-32 31)
		:y-range '(-32 31))))
(setq gabor3-power (power-spectrum gabor3 :center t))

(let ((sigma-x 2)
      (sigma-y 1)
      (k 16))
  (setq gabor4 (make-synthetic-image
		'(64 64)
		#'(lambda (y x)
		    (* (exp (- (+ (/ (sqr x) (* 2 (sqr sigma-x)))
				  (/ (sqr y) (* 2 (sqr sigma-y))))))
		       (sin (/ (* 2 pi k x) 64))))
		:x-range '(-32 31)
		:y-range '(-32 31))))
(setq gabor4-power (power-spectrum gabor4 :center t))

(let ((sigma-x 1)
      (sigma-y 1/2)
      (k 16))
  (setq gabor5 (make-synthetic-image
		'(64 64)
		#'(lambda (y x)
		    (* (exp (- (+ (/ (sqr x) (* 2 (sqr sigma-x)))
				  (/ (sqr y) (* 2 (sqr sigma-y))))))
		       (sin (/ (* 2 pi k x) 64))))
		:x-range '(-32 31)
		:y-range '(-32 31))))
(setq gabor5-power (power-spectrum gabor5 :center t))

(setq separable-function
      (make-synthetic-image
       '(64 64)
       #'(lambda (y x)
	   (* y (exp (- (/ (sqr y) (sqr 1/12))))
	      x (exp (- (/ (sqr x) (sqr 1/12))))
	      ))))
(setq separable-magnitude (sqrt. (power-spectrum separable-function :center t)))

(setq non-separable-function
      (make-synthetic-image
       '(64 64)
       #'(lambda (y x)
	   (* (+ x y)
	      (exp (- (+ (/ (sqr x) (sqr 1/12)) (/ (sqr y) (sqr 1/12)))))))))
(setq non-separable-magnitude (sqrt. (power-spectrum non-separable-function :center t)))

;; Set the zoom default back to what it was before...
(set-default 'gray :zoom 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2D Filtering:

;; Make a pair of orientation selective filters and look at
;; their orientation selective responses:

(setq hfilt (make-filter '((-0.107 0.0 0.107)
			   (-0.245 0.0 0.245)
			   (-0.107 0.0 0.107))))

(setq vfilt (make-filter '((-0.107 -0.245 -0.107)
			   ( 0.0    0.0    0.0)
			   ( 0.107  0.245  0.107))))

(setq disc (make-disc '(64 64)))
(apply-filter hfilt disc)
(apply-filter vfilt disc)

;; The apply-filter method does a convolution of the filter kernel
;; with the image.  Apply-filter is also smart about subsampling and
;; handling edges (take a look at the OBVIUS documentation for
;; details).

;; Let's look at the frequency responses of these two filters:

(power-spectrum hfilt :dimensions '(64 64))
(power-spectrum vfilt :dimensions '(64 64))

;; These two filters were designed to tile (evenly cover) orientation.
;; The sum of their frequency responses is an annulus of spatial
;; frequencies.

(+. (power-spectrum hfilt :dimensions '(64 64))
    (power-spectrum vfilt :dimensions '(64 64)))

;; A zone plate is an image of a radially symmetric frequency sweep,
;; cos(r-sqrd).  Zone plates are another way to look at orientation and
;; frequency selectivity.

(setq zone (make-zone-plate '(64 64)))
(square (apply-filter hfilt zone))
(square (apply-filter vfilt zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Separable Filters:

;; A separable function is one that can be written as the product of two
;; functions, each of which depends on only one variable:
;;
;;        f(x,y) = f1(x) f2(y)

;; An NxN non-separable convolution requires N^2 multiplications for each
;; pixel.  For separable filters, we can do the convolutions more efficiently.
;; We convolve with two one-dimensional filters, one of them applied to the
;; rows and the other applied to the columns.  This requires only 2N
;; multiplications at each pixel.  When N is big (the filter kernels are big),
;; this savings is significant.

;; The two filters defined above can be expressed as separable filters.  Let's
;; just consider the horizonal filter (hfilt).  The two dimensional convolution
;; kernel for that filter is a 3x3 matrix of values that can be expressed as
;; the outer product of two vectors:

(setq hfilt-kernel (make-matrix '((-0.107 0.0 0.107)
				  (-0.245 0.0 0.245)
				  (-0.107 0.0 0.107))))
(setq hfilt-sep-kernel (outer-product (make-matrix '(0.233 0.534 0.233))
				      (make-matrix '(-0.459 0.0 0.459))))
(mean-square-error hfilt-kernel hfilt-sep-kernel)

;; Consequently, we can do the convolutions separably.  The :direction keyword
;; specifies which direction to apply a filter in (0 means apply the filter
;; across the rows and 1 means apply it down the columns):

(progn
  (setq filt1 (make-filter '(0.233 0.534 0.233)))
  (setq filt2 (make-filter '(-0.459 0.0 0.459)))
  (setq convolved-rows (apply-filter filt2 disc :direction 0))
  (setq convolved-row-and-cols (apply-filter filt1 convolved-rows :direction 1)))
(mean-square-error convolved-row-and-cols (apply-filter hfilt disc))

;; Why does this work?  It's because convolution is associative:
;;      (filt1 * filt2) * image = filt1 * (filt2 * image)
;; where * means convolution.

;; We can redefine hfilt and vfilt as separable-filters so that OBVIUS will
;; keep track of which kernel to apply to the rows and which to apply to the
;; columns.  The function make-separable-filter takes two arguments; either two
;; filters or two lists.  The first one is the y- (column) filter and the
;; second one is the x- (row) filter.

(setq hfilt (make-separable-filter
	     '(0.233 0.534 0.233)
	     '(-0.459 0.0 0.459)))
(setq vfilt (make-separable-filter
	     '(-0.459 0.0 0.459)
	     '(0.233 0.534 0.233)))
(setq hresponse (apply-filter hfilt disc))
(setq vresponse (apply-filter vfilt disc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Separable Filtering and Matrix Multiplication:

;; Convolution with a separable filter can be expressed as matrix
;; multiplication using two Toeplitz (circulant) matrices:
;;           C D R
;; where D is the image data, C and R are Toeplitz matrices.  The rows
;; of C filter the columns of D and the columns of R filter the rows
;; of D.

;; Here's an example, using the separable hfilt defined above:

(setq hfilt (make-separable-filter
	     '(0.233 0.534 0.233)
	     '(-0.459 0.0 0.459)))

(progn
  (setq Cmatrix (make-array '(64 64) :element-type 'single-float))
  (setq Cmatrix-row (make-matrix (append '(0.233 0.534 0.233) (list-of-length 61 0.0))))
  (loop for i from 0 below 64
	for shift = (- i 1)
	for row = (displaced-row i Cmatrix)
	do
	(circular-shift Cmatrix-row :x-shift shift :-> row))
  (setq Rmatrix (make-array '(64 64) :element-type 'single-float))
  (setq Rmatrix-row (make-matrix (append '(-0.459 0.0 0.459) (list-of-length 61 0.0))))
  (loop for i from 0 below 64
	for shift = (- i 1)
	for row = (displaced-row i Rmatrix)
	do
	(circular-shift Rmatrix-row :x-shift shift :-> row))
  (matrix-transpose Rmatrix :-> Rmatrix))

(display (make-image Cmatrix) 'gray :zoom 4 :pedestal -.1 :scale .2)
(display (make-image Rmatrix) 'gray :zoom 4 :pedestal -.1 :scale .2)

(progn
  (setq new-hresponse (make-image '(64 64)))
  (matrix-mul (matrix-mul Cmatrix (data disc)) Rmatrix :-> (data new-hresponse))
  new-hresponse)

(mean-square-error hresponse new-hresponse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Non-Separable Filters:

;; Separable filters aren't sufficient for certain applications.  For example,
;; here's a a diagonal filter:

(setq dfilt (make-filter '((-0.2139 -0.2451 0.0)
			    (-0.2451 0.0 0.2451)
			    (0.0 0.2451 0.2139))))

;; Frequency response:

(power-spectrum dfilt :dimensions '(64 64))
(square (apply-filter dfilt zone))

;; Response to disc:

(apply-filter dfilt disc)

;; Using non-separable filters is less efficient.  In the above example it
;; doesn't make much of a difference because the filter is very small (3x3).
;; But it can make a substantial difference for larger filters.

;; Nonseparable filters can (always) be expressed as a linear sum of separable
;; filters.  Sometimes this is particularly easy to do.  For example, our
;; diagonal filter is just the sum of hfilt plus vfilt:

(setq new-dfilt (add hfilt vfilt))
(mean-square-error new-dfilt dfilt)

;; We can compute the response of dfilt using two separable convolutions:

(setq hresponse (apply-filter hfilt disc))
(setq vresponse (apply-filter vfilt disc))
(setq dresponse (add hresponse vresponse))

;; So far, this looks like a silly thing to do.  Computing dresponse directly
;; (using a nonseparable filter as above) took 9 multiplies per pixel.  The new
;; way (adding hresponse plus vresponse) takes a total of 12 multiplies per
;; pixel.  But imagine what it would have been like if the filter kernel was
;; 9x9 instead of 3x3.  In that (9x9) case, hresponse and vresponse would each
;; require 18 multiplies per pixel.  Computing d1response using nonseparable
;; convolution would require 81 multiplies per pixel.

;; We'll use this trick (computing nonseparable responses as linear sums of
;; separable responses) a lot in the tutorial on steerable filters and
;; steerable pyramids.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2D Subsampling:

;; Everything we've learned about 1D subsampling applies in 2D as well.  We can
;; perfectly reconstruct a subsampled image as long as the image is
;; appropriately (according to the Nyquist sampling theorem) bandlimited.

;; Here's a simple example:

(setq gaussian (make-synthetic-image
		'(64 64)
		#'(lambda (y x) (exp (- (/ (+ (sqr (- x 32)) (sqr (- y 32)))
					   (sqr 12)))))
		:x-range '(0 63)))
(setq dft-gaussian (fft gaussian :center t))
(setq mag-gaussian (magnitude dft-gaussian))
(setq impulses (make-synthetic-image
		'(64 64)
		#'(lambda (y x) (if (and (zerop (mod y 4)) (zerop (mod x 4)))
				    1.0
				    0.0))
		:x-range '(0 63)))
(setq gauss-impulses (mul gaussian impulses))
(setq dft-gauss-impulses (fft gauss-impulses :center t))
(setq mag-gauss-impulses (magnitude dft-gauss-impulses))

;; We get replicas of the original Fourier transform when we multiply by an
;; impulse train.  We can reconstruct the original image by pulling out the
;; correct replica:

(setq box (make-synthetic-image
	   '(64 64)
	   #'(lambda (y x) (if (and (< 23 x 40) (< 23 y 40))
			       16.0
			       0.0))
	   :x-range '(0 63)))
(setq reconstructed-gauss (real-part (fft (mul box dft-gauss-impulses) :center t :inverse t)))
(mean-square-error reconstructed-gauss gaussian)

;; In Obvius, we can use the downsample function to do subsampling:

(setq down-gauss (downsample gaussian :start-vector '(0 0) :step-vector '(4 4)))

;; This returns a 16x16 image taking every fourth sample of the original.
;; Obvius also has an upsample function that expands the size of an image
;; filling the in between pixels with zeros:

(setq down-then-up-gauss (upsample down-gauss :start-vector '(0 0) :step-vector '(4 4)))

;; This is the same as we got above, multiplying by the impulses:

(mean-square-error down-then-up-gauss gauss-impulses)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Filtering and Subsampling.

;; When we use a pre-filter to bandlimit an image, the filtered image can be
;; subsampled without aliasing.

;; First look at the dft of an image that is not band limited:
(setq al (crop einstein :x 60 :y 50 :x-dim 128 :y-dim 128))
(setq dft-al (fft al :center t))
(display (magnitude dft-al) 'gray :pedestal 0.0 :scale 10)

;; Now blur the image to produce a (more or less) band limited result:
(setq filt (make-separable-filter '(1/16 1/4 3/8 1/4 1/16)
				  '(1/16 1/4 3/8 1/4 1/16)))
(setq blur-al (apply-filter filt (apply-filter filt (apply-filter filt al))))
(setq dft-blur-al (fft blur-al :center t))
(display (magnitude dft-blur-al) 'gray  :pedestal 0.0 :scale 10)

;; Now multiply the blurred image by impulses:
(setq impulses (make-synthetic-image
		'(128 128)
		#'(lambda (y x) (if (and (zerop (mod y 2)) (zerop (mod x 2)))
				    1.0
				    0.0))
		:x-range '(0 127)))
(setq sampled-blur-al (mul blur-al impulses))
(setq dft-sampled-blur-al (fft sampled-blur-al :center t))
(display (magnitude dft-sampled-blur-al) 'gray  :pedestal 0.0 :scale 10)

;; Notice that the replicas in the freq domain don't overlap (at least, the
;; overlap is insignificant).  So we can reconstruct blur-al from
;; sampled-blur-al:

(setq box (make-synthetic-image
	   '(128 128)
	   #'(lambda (y x) (if (and (< 32 x 97) (< 33 y 97))
			       4.0
			       0.0))
	   :x-range '(0 127)))
(setq reconstructed-blur-al (real-part (fft (mul box dft-sampled-blur-al)
					    :center t :inverse t)))
(mean-square-error reconstructed-blur-al blur-al)

;; Now we downsample (equivalent to multiplying by the impulses and then
;; throwing away the zero values in between).

(setq sub-blur-al (downsample blur-al :step-vector '(2 2)))
(setq dft-sub-blur-al (fft sub-blur-al :center t))
(display (magnitude dft-sub-blur-al) 'gray  :pedestal 0.0 :scale 10)

;; And reconstruct:

(setq reconstructed-dft-blur-al
      (mul 2 (paste dft-sub-blur-al (make-complex-image '(128 128))
		    :y 32 :x 32)))
(setq new-reconstructed-blur-al (real-part (fft reconstructed-dft-blur-al
						:center t :inverse t)))
(mean-square-error reconstructed-blur-al blur-al)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Quadrature Pairs and Energy Mechanisms:

;; A quadrature pair (also called a Hilbert transform pair) is a pair of
;; filters that have the same frequency response magnitude, but that differ by
;; 90 degrees in phase.  The Hilbert transform is a 90 deg phase shift: it
;; keeps the Fourier magnitude the same but shifts the phase of each frequency
;; component by 90 degrees.  For example, the Hilbert transform of a cosinusoid
;; is a sinusoid.

;; Here we construct two separable filters that respond to vertical image
;; features.  The y-component of each filter is the same low-pass.  The
;; x-components are different from one another.  One of them is even symmetric
;; (0 phase) and the other one is odd symmetric (90 deg phase).

(setq 1d-low-pass (make-filter '(7.598E-4 0.01759 0.1660 0.6383
				 1.0
				 0.6383 0.1660 0.01759 7.5987E-4)))
(setq 1d-even-filt (make-filter '(-0.009356 -0.1148 -0.3964 0.06010
				  0.9213
				  0.06010 -0.3964 -0.1148 -0.009356)))
(setq 1d-odd-filt (make-filter '(-0.01045 -0.06578 0.1063 0.8042
				 0.0
				 -0.8042 -0.1063 0.06578 0.01045)))

(display (setq even-filt (make-separable-filter 1d-low-pass 1d-even-filt
						:edge-handler :reflect1))
	 'gray :zoom 8)
(display (setq odd-filt (make-separable-filter 1d-low-pass 1d-odd-filt
					       :edge-handler :reflect1))
	 'gray :zoom 8)

;; Let's look at the frequency responses:

(set-default 'gray :zoom 2)
(setq even-power (power-spectrum even-filt :center t :dimensions '(64 64)))
(setq odd-power (power-spectrum odd-filt :center t :dimensions '(64 64)))

;; Note that the power spectra of the two filters are very similar.  To get a
;; better comparision, let's take a horizontal slice through the middle of each
;; power spectrum, and overlay the two plots.

(display (make-viewable-sequence
	  (list (make-slice even-power :y 32)
		(make-slice odd-power :y 32)))
	 'overlay)

;; These two filters are perfect Hilbert transforms of one another but they are
;; pretty close.

;; Let's look at the responses to a zone plate:

(setq zone-even (apply-filter even-filt zone))
(setq zone-odd (apply-filter odd-filt zone))
(setq zone-energy (add (square zone-even) (square zone-odd)))

;; The last step computed the sum of the squares of the even and odd phase
;; responses.  We call this an "energy mechanism", because it responds to the
;; "local Fourier energy" of the image, regardless of the "local phase".

;; To demonstrate why these energy mechanisms are useful, let's consider the
;; problem of analyzing orientation in an image.  We'd like to measure the
;; local orientation for each small patch of the image.  As simple examples,
;; let's consider a vertical bar and a vertical line.

(setq edge (make-left '(64 64)))
(setq line (mul edge (circular-shift edge :x 31)))

;; Both of these images have the same (vertical) orientation, but their phases
;; are different.  We want to measure the orientation of these image features
;; without having to worry about what the phase is.  The energy mechanism gives
;; us a response that is more or less phase independent:

(progn
  (setq edge-even (apply-filter even-filt edge))
  (setq edge-odd (apply-filter odd-filt edge))
  (setq edge-energy (add (square edge-even) (square edge-odd))))

(progn
  (setq line-even (apply-filter even-filt line))
  (setq line-odd (apply-filter odd-filt line))
  (setq line-energy (add (square line-even) (square line-odd))))

;; Let's look at the vertical energy on a real image:

(set-default 'gray :zoom 1)
(display
 (setq vert-energy (add (square (apply-filter even-filt einstein))
			(square (apply-filter odd-filt einstein))))
 'gray :pedestal 0 :scale 1e4 :zoom 1)

;; And the horizontal energy:

(display (setq hor-even-filt (make-separable-filter 1d-even-filt 1d-low-pass
						    :edge-handler :reflect1))
	 'gray :zoom 8)
(display (setq hor-odd-filt (make-separable-filter 1d-odd-filt 1d-low-pass
						   :edge-handler :reflect1))
	 'gray :zoom 8)
(display
 (setq hor-energy (add (square (apply-filter hor-even-filt einstein))
		       (square (apply-filter hor-odd-filt einstein))))
 'gray :pedestal 0 :scale 1e4 :zoom 1)

;; Here's another way to think about it.  Consider the pair of filters, taken
;; together, as a single complex valued filter.  The odd kernel is the
;; imaginary part of the complex filter and the even kernel is the real part of
;; the complex filter.  The impulse response of this complex filter (for a real
;; valued input) is a complex image:

(setq complex-impulse-response
      (make-complex-image
       (list (apply-filter odd-filt impulse)
	     (apply-filter even-filt impulse))))

;; The frequency response is the DFT of its impulse response:

;; Note that the frequency response of the complex filter is real-valued (the
;; imaginary part is essentially zero):

(range (imaginary-part complex-frequency-response))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Steerable Filters:

;; Here are our two (horizontal and vertical filters) again, along with their
;; frequency responses and their responses to a disc image:

(setq hfilt (make-separable-filter
	     '(0.233 0.534 0.233)
	     '(-0.459 0.0 0.459)))
(setq vfilt (make-separable-filter
	     '(-0.459 0.0 0.459)
	     '(0.233 0.534 0.233)))
(power-spectrum hfilt :dimensions '(64 64))
(power-spectrum vfilt :dimensions '(64 64))
(setq disc (make-disc '(64 64)))
(setq hresponse (apply-filter hfilt disc))
(setq vresponse (apply-filter vfilt disc))

;; Hfilt and vfilt are made up of two one-dimensional (sampled) functions.  One
;; of those functions is a low-pass filters and the other is the first
;; derivative of that low-pass filter.  So hfilt and vfilt are directional
;; derivative operators, hfilt computes the derivative in the x-direction and
;; vfilt computes the derivative in the y-direction.

;; To take derivatives in the other directions, we can use simple combinations
;; of hfilt and vfilt.  For example, the derivative operators for the two
;; diagonal orientations are:

(setq pfilt (mul (/ (sqrt 2) 2) (add hfilt vfilt)))
(setq qfilt (mul (/ (sqrt 2) 2) (sub hfilt vfilt)))

;; Let's look at their frequency responses:

(power-spectrum pfilt :dimensions '(64 64))
(power-spectrum qfilt :dimensions '(64 64))

;; And the responses of these filters for a disc image:

(setq presponse (apply-filter pfilt disc))
(setq qresponse (apply-filter qfilt disc))

;; Here we've convolved the original image with these two new filters.  The two
;; new filters are just linear sums/differences of hfilt and vfilt.  We
;; actually didn't have to do these convolutions since we had already computed
;; hresponse and vresponse (the responses to hfilt and vfilt).  The responses
;; to the diagonal filters are linear sums/differences of the responses to
;; hfilt and vfilt:

(mean-square-error presponse (mul (/ (sqrt 2) 2) (add hresponse vresponse)))
(mean-square-error qresponse (mul (/ (sqrt 2) 2) (sub hresponse vresponse)))

;; Why does this work?  It's because convolution distributes across addition:
;;    (hfilt * image) + (vfilt * image) = (hfilt + vfilt) * image
;; where * means convolution.

;; These first derivative filters are examples of what we call "steerable"
;; filters.  We start with a pair of "basis filters" (hfilt and vfilt) to
;; compute a pair of "basis images" (hresponse and vresponse).  Then we can get
;; derivatives in any other orientation by taking linear sums of hresponse and
;; vresponse.  Compile the following function (using C-c c):

(defun steer1 (hresponse vresponse angle)
  (add (mul (cos angle) hresponse)
       (mul (sin angle) vresponse)))

;; Then evaluate this expression to make a sequence of derivatives in different
;; orientations.  Use C-M-right to display the sequences:

(make-image-sequence (loop for i from 0 below 10
			   collect
			   (steer1 hresponse vresponse (* 2-pi (/ i 10)))))

;; And do the same on a real image:

(load-image (merge-pathnames "images/einstein"
			     obv::*obvius-directory-path*))
(progn
  (setq hresp (apply-filter hfilt einstein))
  (setq vresp (apply-filter vfilt einstein))
  (make-image-sequence (loop for i from 0 below 10
			     collect
			     (steer1 hresp vresp (* 2-pi (/ i 10))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2nd Derivative Steerable Filters:

;; The "steering" property is not restricted to first derivative filters.
;; Obvius provides facilities (described next) for working with steerable
;; filters.  For a complete theoretical treatment of steerable filters see:
;; Freeman and Adelson, The design and use of steerable filters, IEEE Trans. on
;; Pattern Anal. and Mach. Intelligence, 13:891-906, 1991.

;; Load the steerable filtering code:
(obv-require "steer")

;; For example, we can also steer 2nd derivative filters (and their responses),
;; but for 2nd derivatives we need to start with three basis filters (recall
;; that we had only two 1st derivative filters).  With higher-order derivatives
;; (3rd order, etc) we need more and more basis filters.  We call the basis
;; filters a basis set because they span the space of all rotations of each
;; basis filter.  As is typical of a linear basis, we have some freedom about
;; how to choose the basis filters.  For 2nd derivatives we could choose three
;; oriented directional derivatives, rotated 60 degrees from each other.
;; Instead we choose to use three separable filters:

(setq g2-filters (make-g2-steerable-filters))
(first g2-filters)
(second g2-filters)
(third g2-filters)

;; Their power spectra:

(power-spectrum (first g2-filters) :dimensions '(64 64))
(power-spectrum (second g2-filters) :dimensions '(64 64))
(power-spectrum (third g2-filters) :dimensions '(64 64))

;; Make separable steerable basis set for default steerable filter
;; (2nd deriv of Gaussian, "G2")
(setq zone (make-zone-plate '(64 64)))
(setq zone-sb (make-steerable-basis zone))

;; Steer to 45 degrees: this could be done by convolving with a new filter.
;; But we compute it here using the steer function that takes an appropriate
;; linear sum of the the basis images.  The arguments to steer are a
;; steerable-basis and an angle specification.  
(setq zone-0 (steer zone-sb 0.0))
(setq zone-45 (steer zone-sb (/ pi 4)))

;; We can also make a steerable basis set for a quadrature pair of filters.
;; The default filters are 2nd deriv of Gaussian (G2), and it's Hilbert
;; transform (H2).  We haven't written a display routine for the quadrature
;; steerable basis object (for the time being) so nothing gets displayed.
(setq zone-qsb (make-quadrature-steerable-basis zone))

;; To display the G2 (even phase) basis set:
(even-steerable-basis zone-qsb)   

;; To display the H2 (odd phase) basis set (note there are 4 of them):
(odd-steerable-basis zone-qsb)    

;; Steer the odd phase basis set to 30 degrees:
(steer (odd-steerable-basis zone-qsb) (/ pi 6))

;;; Compute "energy", square-root[ G2^2 + H2^2 ], steered to pi/6
;;; radians:
(directional-magnitude zone-qsb (/ pi 6))

;; Make a steerable basis set for einstein:
(setq al (gauss-out einstein))
(setq basis (make-quadrature-steerable-basis al))

;; Compute energy at different orientations:
(directional-magnitude basis 0.0)
(directional-magnitude basis (/ pi 2))

;; Motion without movement (Freeman, Adelson, and Heeger, Motion Without
;; Movement, Computer Graphics, 25:27-30, 1991) is a technique for displaying
;; patterns that appear to move continuously without changing their positions.
;; The method uses a quadrature pair of oriented filters to vary the local
;; phase, giving the sensation of motion.  Here's an example (use C-M-right to
;; display the sequence):
(progn
  (setq even (steer (even-steerable-basis basis) 0.0))
  (setq odd (steer (odd-steerable-basis basis) 0.0))
  (make-image-sequence (loop for i from 0 below 8
			     for phase = (* i (/ 2-pi 8))
			     collect
			     (add (mul (cos phase) even)
				  (mul (sin phase) odd)))))

;; Space-variant filtering.  The angle arguement to the steer function can also
;; be an image, in which case the basis is steered to a different angle at each
;; pixel.  Here we use this to make a funky image sequence of einstein:
(setq angle-image (make-synthetic-image
		   '(128 128)
		   #'(lambda (y x) (atan y x))))
(setq even (steer (even-steerable-basis basis) angle-image))
(setq odd (steer (odd-steerable-basis basis) angle-image))
(make-image-sequence (loop for i from 0 below 8
			   for phase = (* i (/ 2-pi 8))
			   collect
			   (add (mul (cos phase) even)
				(mul (sin phase) odd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Local Variables:
;;; buffer-read-only: t 
;;; fill-column: 79
;;; End:

