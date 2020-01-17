;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: lucid-ffi.lisp
;;;  Author: David Heeger
;;;  Description: collect all the foreign function interface into one place
;;;  Creation Date: Apr '89
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obv)


;;;[tho] 2016-08-08
;;try to tweak it to compile under sbcl

;;define the foreign-types used by lcl
;;  (cffi:defctype :array :pointer)
;; (cffi:defctype :single-float :float)
;; (cffi:defctype :fixnum :int)
;; (cffi:defctype :unsigned-32bit :uint32)
;; (cffi:defctype :string (:pointer :char))




;;; WARNING: this file must be compiled in production mode.  It may
;;; also work if you compile it in one lisp process and load into
;;; another.  If neither of these conditions holds, then you will get
;;; errors about #'LUCID::SET-%PTR-REF-FLOAT being undefined!

#|
;;; In case they ever specialize these:
(defconstant float-array :array)  ; (:array :single-float)
(defconstant fixnum-array :array) ; (:array :unsigned-32bit)
(defconstant uchar-array :array)  ; :array
|#

#|
;;; Put the library-loading function onto the initialization list.
(setq *initialization-functions*
      (pushnew 'load-C-libraries *initialization-functions*))
|#

;;; Used to insert the foreign address of the array into the
;;; vector-of-arrays, at position specified by index.
(LCL:def-foreign-function install-foreign-pointer
    (array (:array :single-float))
    (vector-of-arrays (:array  :unsigned-32bit))
    (index :fixnum))

;;; array-ops.lisp
(LCL:def-foreign-function (memcpy (:return-type :pointer))
    (a1 :array) (a2 :array) (n :fixnum))
(LCL:def-foreign-function memset
    (address :array) val (size :fixnum))
(LCL:def-foreign-function (internal-scalar-multiple (:return-type :signed-32bit))
    (arr-1 (:array :single-float)) (arr-2 (:array :single-float))
    (ratio :double-float) (size :fixnum) (tolerance :double-float))
(LCL:def-foreign-function (internal-sc-almost-equal (:return-type :signed-32bit))
    (arr (:array :single-float)) (val :double-float)
    (size :fixnum) (tolerance :double-float))
(LCL:def-foreign-function (internal-almost-equal (:return-type :signed-32bit))
    (arr1 (:array :single-float)) (arr2 (:array :single-float))
    (size :fixnum) (tolerance :double-float))

;;; arrayio-c.lisp
;; **** Screwed up: return type is actually a file pointer!
;;[tho] 2016-08-15 changed to return pointer
(LCL:def-foreign-function (fopen (:return-type :pointer))
    (path :string) (mode :string))
(LCL:def-foreign-function fclose
    (fp :pointer))
(LCL:def-foreign-function fread
    (buf :array) (size :fixnum) (num :fixnum) (fp :pointer))
(LCL:def-foreign-function fwrite
    (buf :array) (size :fixnum) (num :fixnum) (fp :pointer))
(LCL:def-foreign-function fprintf
    (fp :pointer) (string :string))
(LCL:def-foreign-function byteswap
    (buf (:array :unsigned-32bit)) (size :fixnum))
(LCL:def-foreign-function swapcopy
    (in (:array :unsigned-32bit)) (out (:array :unsigned-32bit)) (size :fixnum))

;; original code
;; (LCL:def-foreign-function (fopen (:return-type :unsigned-32bit))
;;     (path :string) (mode :string))
;; (LCL:def-foreign-function fclose
;;     (fp :unsigned-32bit))
;; (LCL:def-foreign-function fread
;;     (buf :array) (size :fixnum) (num :fixnum) (fp :unsigned-32bit))
;; (LCL:def-foreign-function fwrite
;;     (buf :array) (size :fixnum) (num :fixnum) (fp :unsigned-32bit))
;; (LCL:def-foreign-function fprintf
;;     (fp :unsigned-32bit) (string :string))
;; (LCL:def-foreign-function byteswap
;;     (buf (:array :unsigned-32bit)) (size :fixnum))
;; (LCL:def-foreign-function swapcopy
;;     (in (:array :unsigned-32bit)) (out (:array :unsigned-32bit)) (size :fixnum))



;;; color.lisp

;;; color.lisp
(LCL:def-foreign-function internal-cieluv 
    (xyz (:array :single-float)) (white (:array :single-float))
    (result (:array :single-float)))
(LCL:def-foreign-function internal-cieluv-rows
    (xyz (:array :single-float)) (white (:array :single-float))
    (result (:array :single-float)) (rows :fixnum))
(LCL:def-foreign-function internal-cielab 
    (xyz (:array :single-float)) (white (:array :single-float))
    (result (:array :single-float)))
(LCL:def-foreign-function internal-cielab-rows
    (xyz (:array :single-float)) (white (:array :single-float))
    (result (:array :single-float)) (rows :fixnum))

;;; matrix.lisp
#|
(LCL:def-foreign-function internal-pinverse
    (mat (:array :single-float)) (imat (:array :single-float))
    (nrows :fixnum) (ncols :fixnum))
|#

;;; image-sequence.lisp
#+Sun (LCL:def-foreign-function usleep
	  (usecs :unsigned-32bit))

;;; fft.lisp
(LCL:def-foreign-function internal-fft
    (rdata (:array :single-float)) (idata (:array :single-float))
    (dims (:array :unsigned-32bit)) (ndims :fixnum) (isign :fixnum)) 
(LCL:def-foreign-function internal-realft 
    (rdata (:array :single-float)) (idata (:array :single-float))
    (dims (:array :unsigned-32bit)) (ndims :fixnum) (isign :fixnum)) 

;;; hardcopy.lisp

;;; hardcopy.lisp
(LCL:def-foreign-function internal-chartohex
    (path :string) (array (:array :unsigned-8bit)) (ydim :fixnum) (xdim :fixnum)
    (line-buffer (:array :unsigned-8bit)))
#|
(LCL:def-foreign-function internal-chartohex
    (path :string) (array (:array :unsigned-8bit)) (ydim :fixnum) (xdim :fixnum))
|#

;;; gray.lisp
(LCL:def-foreign-function internal-dither-into-8bit-lut
    (im (:array :single-float)) (x-size :fixnum) (y-size :fixnum) 
    (a (:array :unsigned-8bit)) (a-x-size :fixnum)
    (pedestal :double-float) (scale :double-float)
    (x-offset :fixnum) (y-offset :fixnum)
    (lut (:array :unsigned-8bit)) (lut-size :fixnum))

;;; bitmap.lisp
(LCL:def-foreign-function internal-dither-to-1bit
    (im (:array :single-float)) (xdim :fixnum) (ydim :fixnum)
    (a (:array :unsigned-32bit)) (axdim :fixnum) (aydim :fixnum)
    (min :double-float) (max :double-float))

(LCL:def-foreign-function internal-dither-into-1bit
    (im (:array :single-float)) (x-size :fixnum) (y-size :fixnum) 
    (a (:array :unsigned-32bit)) (a-x-size :fixnum)
    (pedestal :double-float) (scale :double-float)
    (x-offset :fixnum) (y-offset :fixnum))

;;; surface-plot.lisp:
(LCL:def-foreign-function internal-perspective-projection
    (data (:array :single-float)) (y-dim :fixnum) (x-dim :fixnum)
    (y-res (:array :single-float)) (x-res (:array :single-float))
    (y-step :fixnum) (x-step :fixnum)
    (transform-matrix (:array :single-float)))
(LCL:def-foreign-function internal-perspective-projection-int
    (data (:array :single-float)) (y-dim :fixnum) (x-dim :fixnum)
    (y-res (:array :signed-32bit)) (x-res (:array :signed-32bit))
    (y-step :fixnum) (x-step :fixnum)
    (transform-matrix (:array :single-float)))
(LCL:def-foreign-function internal-orthographic-projection
    (data (:array :single-float)) (y-dim :fixnum) (x-dim :fixnum)
    (y-res (:array :single-float)) (x-res (:array :single-float))
    (y-step :fixnum) (x-step :fixnum)
    (transform-matrix (:array :single-float)))
(LCL:def-foreign-function internal-orthographic-projection-int
    (data (:array :single-float)) (y-dim :fixnum) (x-dim :fixnum)
    (y-res (:array :signed-32bit)) (x-res (:array :signed-32bit))
    (y-step :fixnum) (x-step :fixnum)
    (transform-matrix (:array :single-float)))
(LCL:def-foreign-function (internal-compute-surface (:return-type :signed-32bit))
    (y-data (:array :single-float)) (x-data (:array :single-float))
    (y-dim :fixnum) (x-dim :fixnum)
    (y-min-buf (:array :unsigned-32bit)) (y-max-buf (:array :unsigned-32bit))
    (buf-size :fixnum)
    (y0-lines (:array :unsigned-32bit)) (x0-lines (:array :unsigned-32bit))
    (y1-lines (:array :unsigned-32bit)) (x1-lines (:array :unsigned-32bit))
    (max-lines :fixnum))

;;; flipbook.lisp
(LCL:def-foreign-function (select-fds #-Irix (:name "_select") #+Irix (:name "select"))
    (numfds :fixnum) ins outs errs sleep-time)

;;; x-blt.lisp
(LCL:def-foreign-function internal-replicate-8bit
    (src (:array :unsigned-8bit)) (src-xdim :fixnum) (src-ydim :fixnum)
    (start-x :fixnum) (start-y :fixnum)
    (dst (:array :unsigned-8bit)) (dst-xdim :fixnum)
    (dx1 :fixnum) (dy1 :fixnum) (dx2 :fixnum) (dy2 :fixnum)
    (zoom :fixnum))
(LCL:def-foreign-function internal-subsample-8bit
    (src (:array :unsigned-8bit)) (src_xdim :fixnum) (src_ydim :fixnum)
    (dst (:array :unsigned-8bit)) (dst_xdim :fixnum) (skip :fixnum))
(LCL:def-foreign-function internal-paste-8bit-region
    (src :array) (src-xdim :fixnum) (src-ydim :fixnum)
    (start-x :fixnum) (start-y :fixnum)
    (dst :array) (dst-xdim :fixnum)
    (dx1 :fixnum) (dy1 :fixnum) (dx2 :fixnum) (dy2 :fixnum))
(LCL:def-foreign-function internal-subsample-8bit-region
    (src :array) (src-xdim :fixnum) (src-ydim :fixnum)
    (start-x :fixnum) (start-y :fixnum)
    (dst :array) (dst-xdim :fixnum)
    (dx1 :fixnum) (dy1 :fixnum) (dx2 :fixnum) (dy2 :fixnum)
    (skip :fixnum))
(LCL:def-foreign-function internal-replicate-1bit
    (src :array) (src-xdim :fixnum) (src-ydim :fixnum)
    (dst :array) (dst-xdim :fixnum)
    (zoom :fixnum))
(LCL:def-foreign-function internal-subsample-1bit
    (src :array) (src_xdim :fixnum) (src_ydim :fixnum)
    (dst :array) (dst_xdim :fixnum) (skip :fixnum))

;;; filter.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; [tho] 2016-08-22
;;;  changed the naming of the c-functions
;;;   all have a obv_ prefix
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(LCL:def-foreign-function (internal-wrap-filter (:name "obv_internal_wrap_filter"))
    (image (:array :single-float)) (x_dim :fixnum) (y_dim :fixnum)
    (filt (:array :single-float)) (x_fdim :fixnum) (y_fdim :fixnum)
    (xgrid_start :fixnum) (xgrid_step :fixnum) 
    (ygrid_start :fixnum) (ygrid_step :fixnum)
    (result (:array :single-float)) (hex :fixnum ))
(LCL:def-foreign-function (internal-wrap-expand (:name "obv_internal_wrap_expand")) 
    (image (:array :single-float)) (filt (:array :single-float))
    (x_fdim :fixnum) (y_fdim :fixnum)
    (xgrid_start :fixnum) (xgrid_step :fixnum)
    (ygrid_start :fixnum) (ygrid_step :fixnum) 
    (result (:array :single-float)) (x_dim :fixnum) (y_dim :fixnum) (hex :fixnum))
(LCL:def-foreign-function (internal-filter (:name "obv_internal_filter")) 
    (image (:array :single-float)) (x_dim :fixnum) (y_dim :fixnum)
    (filt (:array :single-float)) (temp (:array :single-float))
    (x_fdim :fixnum) (y_fdim :fixnum)
    (xgrid_start :fixnum) (xgrid_step :fixnum)
    (ygrid_start :fixnum) (ygrid_step :fixnum) 
    (result (:array :single-float)) (edges :string))
(LCL:def-foreign-function (internal-expand (:name "obv_internal_expand")) 
    (image (:array :single-float)) (filt (:array :single-float))
    (temp (:array :single-float)) (x_fdim :fixnum) (y_fdim :fixnum) 
    (xgrid_start :fixnum) (xgrid_step :fixnum) (ygrid_start :fixnum) 
    (ygrid_step :fixnum) (result (:array :single-float))
    (x_dim :fixnum) (y_dim :fixnum) (edges :string))
(LCL:def-foreign-function internal-hex-filter 
    (image (:array :single-float)) (x_dim :fixnum) (y_dim :fixnum)
    (filt (:array :single-float)) (temp (:array :single-float)) 
    (x_fdim :fixnum) (y_fdim :fixnum) (xgrid_start :fixnum) 
    (xgrid_step :fixnum) (ygrid_start :fixnum) (ygrid_step :fixnum) 
    (result (:array :single-float)) (edges :string) (stag :fixnum))
(LCL:def-foreign-function internal-hex-expand 
    (image (:array :single-float)) (filt (:array :single-float))
    (temp (:array :single-float)) (x_fdim :fixnum) (y_fdim :fixnum) 
    (xgrid_start :fixnum) (xgrid_step :fixnum) (ygrid_start :fixnum) 
    (ygrid_step :fixnum) (result (:array :single-float))
    (x_dim :fixnum) (y_dim :fixnum) (edges :string) (stag :fixnum))

;;; param-filter.lisp
(LCL:def-foreign-function internal-p-filter 
    (image (:array :single-float)) (x_dim :fixnum) (y_dim :fixnum)
    (indices (:array :unsigned-32bit)) (coeffs (:array :single-float)) (coeff_dim :fixnum)
    (filt (:array :single-float)) (temp (:array :single-float))
    (x_fdim :fixnum) (y_fdim :fixnum)
    (xgrid_start :fixnum) (xgrid_step :fixnum) (ygrid_start :fixnum) (ygrid_step :fixnum) 
    (result (:array :single-float)) (edges :string) (stag :fixnum))
(LCL:def-foreign-function internal-p-expand 
    (image (:array :single-float)) (indices (:array :unsigned-32bit))
    (coeffs (:array :single-float)) (coeff_dim :fixnum) 
    (filt (:array :single-float)) (temp (:array :single-float))
    (x_fdim :fixnum) (y_fdim :fixnum) (xgrid_start :fixnum) (xgrid_step :fixnum)
    (ygrid_start :fixnum) (ygrid_step :fixnum)
    (result (:array :single-float)) (x_dim :fixnum) (y_dim :fixnum) 
    (edges :string) (stag :fixnum))

;;; imops.lisp
(LCL:def-foreign-function (internal-logistic (:return-type :fixnum))
    (arr (:array :single-float)) (res (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function (internal-sgn (:return-type :fixnum))
    (arr (:array :single-float)) (res (:array :single-float))
    (threshold :double-float) (size :fixnum))
(LCL:def-foreign-function (internal-16bit-to-float (:return-type :fixnum))
    (arr (:array :single-float)) (res (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function (internal-8bit-to-float (:return-type :fixnum))
    (arr (:array :single-float)) (res (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-add 
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (res (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-sc-add 
    (im (:array :single-float)) (res (:array :single-float))
    (size :fixnum) (val :double-float))
(LCL:def-foreign-function internal-sub 
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (res (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-sc-sub 
    (im (:array :single-float)) (res (:array :single-float))
    (size :fixnum) (val :double-float))
(LCL:def-foreign-function internal-mul 
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (res (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-sc-mul 
    (im (:array :single-float)) (res (:array :single-float))
    (size :fixnum) (val :double-float))
(LCL:def-foreign-function (internal-div (:return-type :fixnum))
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (res (:array :single-float)) (size :fixnum) (zeroval :double-float))
(LCL:def-foreign-function (internal-sc-div (:return-type :fixnum))
    (im (:array :single-float)) (res (:array :single-float))
    (size :fixnum) (val :double-float) (zeroval :double-float))
(LCL:def-foreign-function internal-linear-xform 
    (im (:array :single-float)) (res (:array :single-float))
    (size :fixnum) (scale :double-float) (offset :double-float))
(LCL:def-foreign-function internal-negate
    (im (:array :single-float)) (result (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-square 
    (im (:array :single-float)) (result (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-sqrt 
    (im (:array :single-float)) (result (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-abs 
    (im (:array :single-float)) (result (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-pow-sc 
    (im (:array :single-float)) (result (:array :single-float))
    (size :fixnum) (power :double-float))
(LCL:def-foreign-function internal-sc-pow 
    (im (:array :single-float)) (result (:array :single-float))
    (size :fixnum) (power :double-float))
(LCL:def-foreign-function internal-pow 
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (result (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function (internal-ln (:return-type :fixnum))
    (im (:array :single-float)) (result (:array :single-float))
    (size :fixnum) (zeroval :double-float))
(LCL:def-foreign-function internal-exp 
    (im (:array :single-float)) (result (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-pointop 
    (im (:array :single-float)) (result (:array :single-float))
    (size :fixnum) (table (:array :single-float)) 
    (tsize :fixnum) (origin :double-float) (increment :double-float))
(LCL:def-foreign-function internal-periodic-pointop 
    (im (:array :single-float)) (result (:array :single-float))
    (size :fixnum) (table (:array :single-float)) 
    (tsize :fixnum) (binsize :double-float))
(LCL:def-foreign-function internal-circular-shift 
    (im (:array :single-float)) (result (:array :single-float))
    (xsz :fixnum) (ysz :fixnum) 
    (xshift :fixnum) (yshift :fixnum) (xgcf :fixnum) (ygcf :fixnum))
(LCL:def-foreign-function internal8-circular-shift 
    (im (:array :unsigned-8bit)) (result (:array :unsigned-8bit))
    (xsz :fixnum) (ysz :fixnum) 
    (xshift :fixnum) (yshift :fixnum) (xgcf :fixnum) (ygcf :fixnum))
(LCL:def-foreign-function internal-flip-x 
    (im (:array :single-float)) (res (:array :single-float))
    (ydim :fixnum) (xdim :fixnum))
(LCL:def-foreign-function internal-flip-y 
    (im (:array :single-float)) (res (:array :single-float))
    (ydim :fixnum) (xdim :fixnum))
(LCL:def-foreign-function internal-flip-xy 
    (im (:array :single-float)) (res (:array :single-float))
    (ydim :fixnum) (xdim :fixnum))
(LCL:def-foreign-function internal-clip 
    (im (:array :single-float)) (res (:array :single-float)) (size :fixnum) 
    (below :double-float) (above :double-float))
(LCL:def-foreign-function internal-im-thresh 
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (res (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-thresh 
    (im (:array :single-float)) (res (:array :single-float))
    (size :fixnum) (thresh :double-float))
(LCL:def-foreign-function internal-round
    (im (:array :single-float)) (res (:array :single-float))
    (sz :fixnum) (binsize :double-float))
(LCL:def-foreign-function internal-truncate
    (im (:array :single-float)) (res (:array :single-float))
    (sz :fixnum) (binsize :double-float))
(LCL:def-foreign-function internal-floor
    (im (:array :single-float)) (res (:array :single-float))
    (sz :fixnum) (binsize :double-float))
(LCL:def-foreign-function internal-quantize 
    (im (:array :single-float)) (result (:array :single-float)) (size :fixnum) 
    (origin :double-float) (binsize :double-float))
(LCL:def-foreign-function internal-const 
    (im (:array :single-float)) (val :double-float) (size :fixnum))
(LCL:def-foreign-function internal-make-ramp
    (result (:array :single-float)) (size :fixnum) (xsize :fixnum)
    (start :double-float) (ystep :double-float) (xstep :double-float))
(LCL:def-foreign-function internal-random
    (im (:array :single-float)) (size :fixnum) (seed :unsigned-32bit))
(LCL:def-foreign-function internal-copy-array 
    (im (:array :single-float)) (res (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal32-copy-array 
    (im (:array :fixnum)) (res (:array :fixnum)) (size :fixnum))
(LCL:def-foreign-function internal-transpose 
    (im (:array :single-float)) (res (:array :single-float))
    (ydim :fixnum) (xdim :fixnum))
(LCL:def-foreign-function internal-paste 
    (im (:array :single-float)) (c1 :fixnum) (sy1 :fixnum) (sx1 :fixnum) 
    (sy2 :fixnum) (sx2 :fixnum) res (c2 :fixnum) 
    (dy :fixnum) (dx :fixnum))
(LCL:def-foreign-function internal-sq-err 
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (result (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-abs-err 
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (result (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-sum-of-squares 
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (result (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-sqrt-sum-of-squares 
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (result (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-phase 
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (result (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-im-min
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (res (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-sc-im-min
    (im (:array :single-float)) (res (:array :single-float))
    (size :fixnum) (val :double-float))
(LCL:def-foreign-function internal-im-max
    (im1 (:array :single-float)) (im2 (:array :single-float))
    (res (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-sc-im-max
    (im (:array :single-float)) (res (:array :single-float))
    (size :fixnum) (val :double-float))
(LCL:def-foreign-function (internal-min (:return-type :double-float))
    (im (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function (internal-max (:return-type :double-float))
    (im (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-min-of
    (im (:array :single-float)) (size :fixnum) (val-and-pos (:array :single-float)))
(LCL:def-foreign-function internal-max-of 
    (im (:array :single-float)) (size :fixnum) (val-and-pos (:array :single-float)))
(LCL:def-foreign-function internal32-min-of
    (im (:array :unsigned-32bit)) (size :fixnum) (val-and-pos (:array :unsigned-32bit)))
(LCL:def-foreign-function internal32-max-of 
    (im (:array :unsigned-32bit)) (size :fixnum) (val-and-pos (:array :unsigned-32bit)))
(LCL:def-foreign-function internal-range 
    (im (:array :single-float)) (size :fixnum) (return-vals (:array :single-float)))
(LCL:def-foreign-function (internal-sum-of (:return-type :double-float))
    (im (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function (internal-product-of (:return-type :double-float))
    (im (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function (internal-mean (:return-type :double-float))
    (im (:array :single-float)) (size :fixnum) (ignore-zeros :fixnum))
(LCL:def-foreign-function (internal-variance (:return-type :double-float))
    (im (:array :single-float)) (size :fixnum) (ignore-zeros :fixnum))
(LCL:def-foreign-function (internal-third-moment (:return-type :double-float))
    (im (:array :single-float)) (size :fixnum) (mean :double-float))
(LCL:def-foreign-function (internal-fourth-moment (:return-type :double-float))
    (im (:array :single-float)) (size :fixnum) (mean :double-float))
;(LCL:def-foreign-function (internal-entropy (:return-type :double-float))
;  (im (:array :single-float)) (size :fixnum) (hist (:array :unsigned-32bit))
;  (hsize :fixnum) (offset :fixnum))
(LCL:def-foreign-function internal-histogram 
    (im (:array :single-float)) (size :fixnum)
    (hist (:array :unsigned-32bit)) (hsize :fixnum) 
    (origin :double-float) (binsize :double-float))
(LCL:def-foreign-function (internal-mean-sq-err (:return-type :double-float))
    (im1 (:array :single-float)) (im2 (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function (internal-mean-abs-err (:return-type :double-float))
    (im1 (:array :single-float)) (im2 (:array :single-float)) (size :fixnum))
;(LCL:def-foreign-function internal-print-image
;   (im (:array :single-float)) (xdim :fixnum) (ydim :fixnum) (x1 :fixnum) (x2 :fixnum)
;   (y1 :fixnum) (y2 :fixnum))
(LCL:def-foreign-function internal-8bit-to-f 
    (a (:array :unsigned-8bit)) (im (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-32bit-to-f 
    (a (:array :unsigned-32bit)) (im (:array :single-float)) (size :fixnum))
(LCL:def-foreign-function internal-f-to-32bit 
    (im (:array :single-float)) (a (:array :unsigned-32bit)) (size :fixnum) 
    (pedestal :double-float) (scale :double-float))
(LCL:def-foreign-function internal-f-to-8bit 
    (im (:array :single-float)) (a (:array :unsigned-8bit)) (size :fixnum) 
    (pedestal :double-float) (scale :double-float) (floor :fixnum) (ceiling :fixnum))
(LCL:def-foreign-function internal-f-into-8bit 
    (im (:array :single-float)) (im-x-size :fixnum) (im-y-size :fixnum) 
    (a (:array :unsigned-8bit)) (a-size :fixnum)
    (pedestal :double-float) (scale :double-float) 
    (floor :fixnum) (ceiling :fixnum) (x-offset :fixnum) (y-offset :fixnum))
(LCL:def-foreign-function internal-f-into-8bit-lut
    (im (:array :single-float)) (im-x-size :fixnum) (im-y-size :fixnum) 
    (a (:array :unsigned-8bit)) (a-x-size :fixnum) (pedestal :double-float)
    (scale :double-float) (x-offset :fixnum) (y-offset :fixnum)
    (lut (:array :unsigned-8bit)) (lut-size :fixnum))

;;; matrix.lisp
(LCL:def-foreign-function (internal-matrix-mul (:language :c) (:return-type :signed-32bit))
  (matrix1 (:array :single-float)) (rows1 :fixnum) (cols1 :fixnum)      
  (matrix2 (:array :single-float)) (rows2 :fixnum) (cols2 :fixnum)      
  (matrix3 (:array :single-float)) (rows3 :fixnum) (cols3 :fixnum))
(LCL:def-foreign-function (internal-matrix-mul-transpose (:language :c) (:return-type :signed-32bit))
  (matrix1 (:array :single-float)) (rows1 :fixnum) (cols1 :fixnum)      
  (matrix2 (:array :single-float)) (rows2 :fixnum) (cols2 :fixnum)      
  (matrix3 (:array :single-float)) (rows3 :fixnum) (cols3 :fixnum))
(LCL:def-foreign-function (internal-matrix-transpose-mul (:language :c) (:return-type :signed-32bit))
  (matrix1 (:array :single-float)) (rows1 :fixnum) (cols1 :fixnum)      
  (matrix2 (:array :single-float)) (rows2 :fixnum) (cols2 :fixnum)      
  (matrix3 (:array :single-float)) (rows3 :fixnum) (cols3 :fixnum))
(LCL:def-foreign-function (internal-matrix-transpose (:language :c) (:return-type :signed-32bit))
  (matrix1 (:array :single-float)) (rows1 :fixnum) (cols1 :fixnum)      
  (matrix2 (:array :single-float)))
(LCL:def-foreign-function (internal-dot-product (:return-type :double-float))
    (arr-1 (:array :single-float)) (arr-2 (:array :single-float)) (size :fixnum))

(LCL:def-foreign-function (internal-qf-row-arr (:return-type :double-float))
    (row (:array :single-float)) (arr (:array :single-float))
    (rows :fixnum) (cols :fixnum))


(LCL:def-foreign-function (internal-svd (:language :c) (:return-type :signed-32bit))
  (matrix-t (:array :single-float)) ; transposed matrix, will be destroyed
  (rows :fixnum)			; dimensions of matrix
  (cols :fixnum)			; dimensions of matrix
  (tmp-s (:array :single-float))	; vector of length (+ (min row col) 1)
  (u-t (:array :single-float))		; matrix of size (row row)
  (v-t (:array :single-float))		; matrix of size (col col)
  (tmp-col (:array :single-float))	; vector of length rows
  (tmp-row (:array :single-float))	; vector of length cols
  )

;;; stepit.lisp
(LCL:def-foreign-function (stepit_fit (:language :c)  (:return-type :double-float))
    (error-function :pointer)
  (parameters (:array :single-float))
  (lower-bounds (:array :single-float))
  (upper-bounds (:array :single-float))
  (smallest-steps (:array :single-float))
  (initial-steps (:array :single-float))
  (err (:array :single-float))
  (dx (:array :single-float))
  (xs (:array :single-float))
  (dlx (:array :single-float))
  (xosc (:array :single-float))
  (salvo (:array :single-float))
  (fstor (:array :single-float))
  (vec (:array :single-float))
  (fosc (:array :single-float))
  (masks (:array :fixnum))
  (jflat (:array :fixnum))
  (num-parameters :fixnum)
  (max-function-calls :fixnum)
  (ntrac :fixnum)
  (mosq :fixnum))

(LCL:def-foreign-callable (stepit-foreign-error-function (:return-type :double-float)) ()
  (stepit-error-function))

(defun load-tiff-library (path)
  (LCL::load-foreign-libraries nil (list path)))

;;; warper.lisp
(LCL:def-foreign-function bilinear-warp
    (in (:array :single-float)) (out (:array :single-float))
    (y-flow (:array :single-float)) (x-flow (:array :single-float))
    (y-dim :fixnum) (x-dim :fixnum))
(LCL:def-foreign-function bicubic-warp
    (in (:array :single-float)) (out (:array :single-float))
    (y-flow (:array :single-float)) (x-flow (:array :single-float))
    (y-dim :fixnum) (x-dim :fixnum))
(LCL:def-foreign-function wrap-warp
    (in (:array :single-float)) (out (:array :single-float))
    (y-flow (:array :single-float)) (x-flow (:array :single-float))
    (y-dim :fixnum) (x-dim :fixnum))
(LCL:def-foreign-function clip-warp
    (in (:array :single-float)) (out (:array :single-float))
    (from-y-dim :fixnum) (from-x-dim :fixnum)
    (y-flow (:array :single-float)) (x-flow (:array :single-float))
    (to-y-dim :fixnum) (to-x-dim :fixnum))

;;; row-ops.lisp
(LCL:def-foreign-function (internal-add-arr-row (:return-type :signed-32bit))
    (arr (:array :single-float)) (row (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-sub-arr-row (:return-type :signed-32bit))
    (arr (:array :single-float)) (row (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-sub-row-arr (:return-type :signed-32bit))
    (row (:array :single-float)) (arr (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-mul-arr-row (:return-type :signed-32bit))
    (arr (:array :single-float)) (row (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-div-arr-row (:return-type :signed-32bit))
    (arr (:array :single-float)) (row (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-div-row-arr (:return-type :signed-32bit))
    (row (:array :single-float)) (arr (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))

(LCL:def-foreign-function (internal-add-arr-col (:return-type :signed-32bit))
    (arr (:array :single-float)) (col (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-sub-arr-col (:return-type :signed-32bit))
    (arr (:array :single-float)) (col (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-sub-col-arr (:return-type :signed-32bit))
    (col (:array :single-float)) (arr (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-mul-arr-col (:return-type :signed-32bit))
    (arr (:array :single-float)) (col (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-div-arr-col (:return-type :signed-32bit))
    (arr (:array :single-float)) (col (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-div-col-arr (:return-type :signed-32bit))
    (col (:array :single-float)) (arr (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))

(LCL:def-foreign-function (internal-row-sum-of-square (:return-type :signed-32bit))
    (arr (:array :single-float)) (res (:array :single-float))
    (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-row-sum (:return-type :signed-32bit))
    (arr (:array :single-float)) (vec (:array :single-float)) (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-col-sum (:return-type :signed-32bit))
    (arr (:array :single-float)) (vec (:array :single-float)) (rows :fixnum) (cols :fixnum))
(LCL:def-foreign-function (internal-row-swap (:return-type :signed-32bit))
    (arr (:array :single-float)) (row-1 :fixnum) (row-2 :fixnum) (cols :fixnum))


;;; Local Variables:
;;; buffer-read-only: t 
;;; End:
