;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File:  gl-obv-lucid-ffi.lisp
;;;  Author: Patrick C. Teo
;;;  Description: FFI routines used by OBVIUS implementation with GL
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :obvius)

(LCL:def-foreign-function internal-color-f-into-24bit-lut
  (R-im (:array :single-float))
  (G-im (:array :single-float))
  (B-im (:array :single-float))
  (im-x-size :fixnum)
  (im-y-size :fixnum) 
  (a (:array :unsigned-32bit))
  (a-x-size :fixnum)
  (pedestal :double-float)
  (scale :double-float)
  (x-offset :fixnum)
  (y-offset :fixnum)
  (R-lut (:array :unsigned-8bit))
  (G-lut (:array :unsigned-8bit))
  (B-lut (:array :unsigned-8bit))  
  (lut-size :fixnum))

(LCL:def-foreign-function internal-color-into-24bit
  (R-bytes (:array :unsigned-8bit))
  (G-bytes (:array :unsigned-8bit))
  (B-bytes (:array :unsigned-8bit))
  (bytes-xsize :fixnum)
  (dest (:array :unsigned-32bit))
  (x-size :fixnum)
  (y-size :fixnum)  
  (x-offset :fixnum)
  (y-offset :fixnum))

(LCL:def-foreign-function internal-gray-f-into-24bit-lut
  (im (:array :single-float))
  (im-x-size :fixnum)
  (im-y-size :fixnum) 
  (a (:array :unsigned-32bit))
  (a-x-size :fixnum)
  (pedestal :double-float)
  (scale :double-float)
  (x-offset :fixnum)
  (y-offset :fixnum)
  (lut (:array :unsigned-8bit))
  (lut-size :fixnum))


(LCL:def-foreign-function obv::internal-paste-8bit-to-16bit
    (src (:array :unsigned-8bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (sx :fixnum)
    (sy :fixnum)
    (dst (:array :unsigned-16bit))
    (dst-xdim :fixnum)
    (dx1 :fixnum)
    (dy1 :fixnum)
    (dx2 :fixnum)
    (dy2 :fixnum))

(LCL:def-foreign-function obv::internal-supersample-8bit-to-16bit
    (src (:array :unsigned-8bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (sx :fixnum)
    (sy :fixnum)
    (dst (:array :unsigned-16bit))
    (dst-xdim :fixnum)
    (dx1 :fixnum)
    (dy1 :fixnum)
    (dx2 :fixnum)
    (dy2 :fixnum)
    (zoom :fixnum))

(LCL:def-foreign-function obv::internal-subsample-8bit-to-16bit
    (src (:array :unsigned-8bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (dst (:array :unsigned-16bit))
    (dst-xdim :fixnum)
    (zoom :fixnum))

(LCL:def-foreign-function obv::internal-paste-1bit-to-16bit
    (src (:array :bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (dst (:array :unsigned-16bit))
    (dst-xdim :fixnum)
    (dx1 :fixnum)
    (dy1 :fixnum)
    (dx2 :fixnum)
    (dy2 :fixnum)
    (zero-value :fixnum)
    (one-value :fixnum))

(LCL:def-foreign-function obv::internal-supersample-1bit-to-16bit
    (src (:array :bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (dst (:array :unsigned-16bit))
    (dst-xdim :fixnum)
    (dx1 :fixnum)
    (dy1 :fixnum)
    (dx2 :fixnum)
    (dy2 :fixnum)
    (zoom :fixnum)
    (zero-value :fixnum)
    (one-value :fixnum))

(LCL:def-foreign-function obv::internal-subsample-1bit-to-16bit
    (src (:array :bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (dst (:array :unsigned-16bit))
    (dst-xdim :fixnum)
    (zoom :fixnum)
    (zero-value :fixnum)
    (one-value :fixnum))

(LCL:def-foreign-function obv::internal-paste-24bit-to-24bit
    (src (:array :unsigned-32bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (sx :fixnum)
    (sy :fixnum)
    (dst (:array :unsigned-32bit))
    (dst-xdim :fixnum)
    (dx1 :fixnum)
    (dy1 :fixnum)
    (dx2 :fixnum)
    (dy2 :fixnum))

(LCL:def-foreign-function obv::internal-supersample-24bit-to-24bit
    (src (:array :unsigned-32bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (sx :fixnum)
    (sy :fixnum)
    (dst (:array :unsigned-32bit))
    (dst-xdim :fixnum)
    (dx1 :fixnum)
    (dy1 :fixnum)
    (dx2 :fixnum)
    (dy2 :fixnum)
    (zoom :fixnum))

(LCL:def-foreign-function obv::internal-subsample-24bit-to-24bit
    (src (:array :unsigned-32bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (dst (:array :unsigned-32bit))
    (dst-xdim :fixnum)
    (zoom :fixnum))

(LCL:def-foreign-function obv::internal-paste-8bit-to-24bit
    (src (:array :unsigned-8bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (sx :fixnum)
    (sy :fixnum)
    (dst (:array :unsigned-32bit))
    (dst-xdim :fixnum)
    (dx1 :fixnum)
    (dy1 :fixnum)
    (dx2 :fixnum)
    (dy2 :fixnum))

(LCL:def-foreign-function obv::internal-supersample-8bit-to-24bit
    (src (:array :unsigned-8bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (sx :fixnum)
    (sy :fixnum)
    (dst (:array :unsigned-32bit))
    (dst-xdim :fixnum)
    (dx1 :fixnum)
    (dy1 :fixnum)
    (dx2 :fixnum)
    (dy2 :fixnum)
    (zoom :fixnum))

(LCL:def-foreign-function obv::internal-subsample-8bit-to-24bit
    (src (:array :unsigned-8bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (dst (:array :unsigned-32bit))
    (dst-xdim :fixnum)
    (zoom :fixnum))


(LCL:def-foreign-function obv::internal-paste-1bit-to-24bit
    (src (:array :bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (dst (:array :unsigned-32bit))
    (dst-xdim :fixnum)
    (dx1 :fixnum)
    (dy1 :fixnum)
    (dx2 :fixnum)
    (dy2 :fixnum)
    (zero-value :fixnum)
    (one-value :fixnum))

(LCL:def-foreign-function obv::internal-supersample-1bit-to-24bit
    (src (:array :bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (dst (:array :unsigned-32bit))
    (dst-xdim :fixnum)
    (dx1 :fixnum)
    (dy1 :fixnum)
    (dx2 :fixnum)
    (dy2 :fixnum)
    (zoom :fixnum)
    (zero-value :fixnum)
    (one-value :fixnum))

(LCL:def-foreign-function obv::internal-subsample-1bit-to-24bit
    (src (:array :bit))
    (src-xdim :fixnum)
    (src-ydim :fixnum)
    (dst (:array :unsigned-32bit))
    (dst-xdim :fixnum)
    (zoom :fixnum)
    (zero-value :fixnum)
    (one-value :fixnum))



(LCL:def-foreign-function obv::internal-draw-lines
    (y0 (:array :signed-32bit))
    (x0 (:array :signed-32bit))
    (y1 (:array :signed-32bit))
    (x1 (:array :signed-32bit))
    (y-offset :fixnum)
    (x-offset :fixnum)
    (nlines :fixnum))

(LCL:def-foreign-function obv::internal-draw-circles
    (y-origin (:array :signed-32bit))
    (x-origin (:array :signed-32bit))
    (radius :fixnum)
    (fill-flag :fixnum)
    (ncircles :fixnum))

(LCL:def-foreign-function obv::internal-draw-rects
    (y0 (:array :signed-32bit))
    (x0 (:array :signed-32bit))
    (y1 (:array :signed-32bit))
    (x1 (:array :signed-32bit))
    (fill-flag :fixnum)
    (nrects :fixnum))

#|
;;; OLD

(LCL:def-foreign-function internal-left-shift-32bit
  (src (:array :unsigned-32bit))
  (dst (:array :unsigned-32bit))
  (num-bits :fixnum)
  (size :fixnum))
  
(LCL:def-foreign-function internal-f-into-24bit-lut
  (im (:array :single-float))
  (im-x-size :fixnum)
  (im-y-size :fixnum) 
  (a (:array :unsigned-32bit))
  (a-x-size :fixnum)
  (pedestal :double-float)
  (scale :double-float)
  (x-offset :fixnum)
  (y-offset :fixnum)
  (lut (:array :unsigned-8bit))
  (lut-size :fixnum))
|#
