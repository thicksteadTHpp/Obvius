;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  fmclient-constants.lisp
;;;
;;;  Font Manager Constants
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package 'gl)


;;;
;;; Overall font information subscripts: fmfontinfo
;;;
;;;

(def-exported-constant "FMFPRINTERMATCHED" 0)
(def-exported-constant "FMFMATRIX00" 1)
(def-exported-constant "FMFMATRIX01" 2)
(def-exported-constant "FMFMATRIX10" 3)
(def-exported-constant "FMFMATRIX11" 4)
(def-exported-constant "FMFTYPE"     5)
(def-exported-constant "FMFENCODING" 6)
(def-exported-constant "FMFFIXED_WIDTH" 7)
(def-exported-constant "FMFXORIG" 8)
(def-exported-constant "FMFYORIG" 9)
(def-exported-constant "FMFXSIZE" 10)
(def-exported-constant "FMFYSIZE" 11)
(def-exported-constant "FMFNGLYPHS" 12)
(def-exported-constant "FMFBITSDEEP" 13)
(def-exported-constant "FMFWIDTH" 14)
(def-exported-constant "FMFRESOLUTION" 15)
(def-exported-constant "FMFWEIGHT" 16)

;;; enumeration of fmfontinfo->type
(def-exported-constant "FMBITMAPFONT"	    0)
(def-exported-constant "FMWIDTHFONT"	    1)
(def-exported-constant "FMPRINTERWIDTHFONT"	2)
(def-exported-constant "FMSPLINEFONT"	    3)
(def-exported-constant "FMVECTORFONT"	    4)
(def-exported-constant "FMGREYSCALEFONT"  5)
(def-exported-constant "FMRGBFONT"	    6)
(def-exported-constant "FMGREEKFONT"	    7)
(def-exported-constant "FMPERSPECTIVEFONT"   8)
(def-exported-constant "FMCOLORMAP"	    9)
(def-exported-constant "FMMIXED"	    10)

;;; enumeration of fmfontinfo->encoding
(def-exported-constant "FMASCII"	0)
(def-exported-constant "FMADOBE"	1)
(def-exported-constant "FMJISC6226"	2)
(def-exported-constant "FMCYRILLIC"	3)
(def-exported-constant "FMHANGUL"	4)
(def-exported-constant "FMDEVENAGERI"	5)



;;;
;;; Character glyph information subscripts: fmglyphinfo
;;;
(def-exported-constant "FMXSIZE"	0)
(def-exported-constant "FMYSIZE"	1)
(def-exported-constant "FMXORIG"	2)
(def-exported-constant "FMYORIG"	3)
(def-exported-constant "FMXMOVE"	4)
(def-exported-constant "FMYMOVE"	5)
(def-exported-constant "FMGTYPE"	6)
(def-exported-constant "FMBITSDEEP"	7)


;;; fmglyphinfo->gtype
(def-exported-constant "FMNOTDEFINED"	    0)
(def-exported-constant "FMBITMAPGLYPH"    1)
(def-exported-constant "FMIMAGERGBGLYPH"  2)
(def-exported-constant "FMIMAGECGLYPH"    3)
(def-exported-constant "FMLINEGLYPH"	    4)
(def-exported-constant "FMSHAPEGLYPH"	    5)

(def-exported-constant "FMCACHE_QUANTUM" 100000)
