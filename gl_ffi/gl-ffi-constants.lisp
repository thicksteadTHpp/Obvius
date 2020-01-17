;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  gl-constants.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package 'gl)

;;; miscellaneous
(def-exported-constant "NULL" 0)
(def-exported-constant "FALSE" 0)
(def-exported-constant "TRUE" 1)

;;; various hardware/software limits */
(def-exported-constant "ATTRIBSTACKDEPTH" 10)
(def-exported-constant "VPSTACKDEPTH" 8)
(def-exported-constant "MATRIXSTACKDEPTH" 32)
(def-exported-constant "NAMESTACKDEPTH" 1025)

;;; special pre-defined tages
(def-exported-constant "STARTTAG" -2)
(def-exported-constant "ENDTAG" -3)

;;; names for colors in color map loaded by greset
(def-exported-constant "BLACK" 0)
(def-exported-constant "RED" 1)
(def-exported-constant "GREEN" 2)
(def-exported-constant "YELLOW" 3)
(def-exported-constant "BLUE" 4)
(def-exported-constant "MAGENTA" 5)
(def-exported-constant "CYAN" 6)
(def-exported-constant "WHITE" 7)

;;; popup colors
(def-exported-constant "PUP_CLEAR" 0)
(def-exported-constant "PUP_COLOR" 1)
(def-exported-constant "PUP_BLACK" 2)
(def-exported-constant "PUP_WHITE" 3)

;;; drawmode and mswapbuffers
(def-exported-constant "NORMALDRAW" #x010)
(def-exported-constant "PUPDRAW" #x020)
(def-exported-constant "OVERDRAW" #x040)
(def-exported-constant "UNDERDRAW" #x080)
(def-exported-constant "CURSORDRAW" #x100)
(def-exported-constant "DUALDRAW" #x200)

;;; defpattern
(def-exported-constant "PATTERN16" 16)
(def-exported-constant "PATTERN32" 32)
(def-exported-constant "PATTERN64" 64)

(def-exported-constant "PATTERN_16_SIZE" 16)
(def-exported-constant "PATTERN_32_SIZE" 64)
(def-exported-constant "PATTERN_64_SIZE" 256)

;;; defines for readsource
(def-exported-constant "SRC_AUTO" 0)
(def-exported-constant "SRC_FRONT" 1)
(def-exported-constant "SRC_BACK" 2)
(def-exported-constant "SRC_ZBUFFER" 3)
(def-exported-constant "SRC_PUP" 4)
(def-exported-constant "SRC_OVER" 5)
(def-exported-constant "SRC_UNDER" 6)
(def-exported-constant "SRC_FRAMEGRABBER" 7)

;;; defines for blendfunction
(def-exported-constant "BF_ZERO" 0)
(def-exported-constant "BF_ONE" 1)
(def-exported-constant "BF_DC" 2)
(def-exported-constant "BF_SC" 2)
(def-exported-constant "BF_MDC" 3)
(def-exported-constant "BF_MSC" 3)
(def-exported-constant "BF_SA" 4)
(def-exported-constant "BF_MSA" 5)
(def-exported-constant "BF_DA" 6)
(def-exported-constant "BF_MDA" 7)
(def-exported-constant "BF_MIN_SA_MDA" 8)

;;; defines for afunction
(def-exported-constant "AF_NEVER" 0)
(def-exported-constant "AF_LESS" 1)
(def-exported-constant "AF_EQUAL" 2)
(def-exported-constant "AF_LEQUAL" 3)
(def-exported-constant "AF_GREATER" 4)
(def-exported-constant "AF_NOTEQUAL" 5)
(def-exported-constant "AF_GEQUAL" 6)
(def-exported-constant "AF_ALWAYS" 7)

;;; defines for zfunction
(def-exported-constant "ZF_NEVER" 0)
(def-exported-constant "ZF_LESS" 1)
(def-exported-constant "ZF_EQUAL" 2)
(def-exported-constant "ZF_LEQUAL" 3)
(def-exported-constant "ZF_GREATER" 4)
(def-exported-constant "ZF_NOTEQUAL" 5)
(def-exported-constant "ZF_GEQUAL" 6)
(def-exported-constant "ZF_ALWAYS" 7)

;;; defines for zsource
(def-exported-constant "ZSRC_DEPTH" 0)
(def-exported-constant "ZSRC_COLOR" 1)

;;; defines for pntsmooth
(def-exported-constant "SMP_OFF" #x0)
(def-exported-constant "SMP_ON" #x1)
(def-exported-constant "SMP_SMOOTHER" #x2)

;;; defines for linesmoother
(def-exported-constant "SML_OFF" #x0)
(def-exported-constant "SML_ON" #x1)
(def-exported-constant "SML_SMOOTHER" #x2)
(def-exported-constant "SML_END_CORRECT" #x4)

;;; defines for polysmooth
(def-exported-constant "PYSM_OFF" 0)
(def-exported-constant "PYSM_ON" 1)
(def-exported-constant "PYSM_SHRINK" 2)

;;; defines for dither modes
(def-exported-constant "DT_OFF" 0)
(def-exported-constant "DT_ON" 1)

;;; defines for setpup
(def-exported-constant "PUP_NONE" 0)
(def-exported-constant "PUP_GREY" #x1)
(def-exported-constant "PUP_BOX" #x2)
(def-exported-constant "PUP_CHECK" #x4)

;;; defines for glcompat
(def-exported-constant "GLC_OLDPOLYGON" 0)
(def-exported-constant "GLC_ZRANGEMAP" 1)
(def-exported-constant "GLC_MQUEUERATE" 2)
(def-exported-constant "GLC_SOFTATTACH" 3)
(def-exported-constant "GLC_MANAGEBG" 4)
(def-exported-constant "GLC_SLOWMAPCOLORS" 5)
(def-exported-constant "GLC_INPUTCHANGEBUG" 6)
(def-exported-constant "GLC_NOBORDERBUG" 7)

;;; value for GLC_MQUEUERATE which is compatible with 3.3
(def-exported-constant "GLC_COMPATRATE" 15)

;;; defines for curstype
(def-exported-constant "C16X1" 0)
(def-exported-constant "C16X2" 1)
(def-exported-constant "C32X1" 2)
(def-exported-constant "C32X2" 3)
(def-exported-constant "CCROSS" 4)

;;; defines for shademodel
(def-exported-constant "FLAT" 0)
(def-exported-constant "GOURAUD" 1)

;;; defines for logicop
(def-exported-constant "LO_ZERO" #x0)
(def-exported-constant "LO_AND" #x1)
(def-exported-constant "LO_ANDR" #x2)
(def-exported-constant "LO_SRC" #x3)
(def-exported-constant "LO_ANDI" #x4)
(def-exported-constant "LO_DST" #x5)
(def-exported-constant "LO_XOR" #x6)
(def-exported-constant "LO_OR" #x7)
(def-exported-constant "LO_NOR" #x8)
(def-exported-constant "LO_XNOR" #x9)
(def-exported-constant "LO_NDST" #xa)
(def-exported-constant "LO_ORR" #xb)
(def-exported-constant "LO_NSRC" #xc)
(def-exported-constant "LO_ORI" #xd)
(def-exported-constant "LO_NAND" #xe)
(def-exported-constant "LO_ONE" #xf)

;;; define for scrnselect
(def-exported-constant "INFOCUSSCRN" -2)

;;; defines for stencil
(def-exported-constant "ST_KEEP" 0)
(def-exported-constant "ST_ZERO" 1)
(def-exported-constant "ST_REPLACE" 2)
(def-exported-constant "ST_INCR" 3)
(def-exported-constant "ST_DECR" 4)
(def-exported-constant "ST_INVERT" 5)
(def-exported-constant "SF_NEVER" 0)
(def-exported-constant "SF_LESS" 1)
(def-exported-constant "SF_EQUAL" 2)
(def-exported-constant "SF_LEQUAL" 3)
(def-exported-constant "SF_GREATER" 4)
(def-exported-constant "SF_NOTEQUAL" 5)
(def-exported-constant "SF_GEQUAL" 6)
(def-exported-constant "SF_ALWAYS" 7)

;;; defines for scrsubdivide
(def-exported-constant "SS_OFF" 0)
(def-exported-constant "SS_DEPTH" 1)

;;; defines for polymode
(def-exported-constant "PYM_FILL" 1)
(def-exported-constant "PYM_POINT" 2)
(def-exported-constant "PYM_LINE" 3)
(def-exported-constant "PYM_HOLLOW" 4)
(def-exported-constant "PYM_LINE_FAST" 5)

;;; defines for fogvertex
(def-exported-constant "FG_OFF" 0)
(def-exported-constant "FG_ON" 1)
(def-exported-constant "FG_DEFINE" 2)
(def-exported-constant "FG_VTX_EXP" 2)   ;;; aka fg-define
(def-exported-constant "FG_VTXLIN" 3)
(def-exported-constant "FG_PIX_EXP" 4)
(def-exported-constant "FG_PIX_LIN" 5)
(def-exported-constant "FG_VTX_EXP2" 6)
(def-exported-constant "FG_PIX_EXP2" 7)

;;; defines for pixmode
(def-exported-constant "PM_SHIFT" 0)
(def-exported-constant "PM_EXPAND" 1)
(def-exported-constant "PM_C0" 2)
(def-exported-constant "PM_C1" 3)
(def-exported-constant "PM_ADD24" 4)
(def-exported-constant "PM_SIZE" 5)
(def-exported-constant "PM_OFFSET" 6)
(def-exported-constant "PM_STRIDE" 7)
(def-exported-constant "PM_TTOB" 8)
(def-exported-constant "PM_RTOL" 9)
(def-exported-constant "PM_ZDATA" 10)
(def-exported-constant "PM_WARP" 11)
(def-exported-constant "PM_RDX" 12)
(def-exported-constant "PM_RDY" 13)
(def-exported-constant "PM_CDX" 14)
(def-exported-constant "PM_CDY" 15)
(def-exported-constant "PM_XSTART" 16)
(def-exported-constant "PM_YSTART" 17)
(def-exported-constant "PM_VO1" 1000)  ;;; internal use only

;;; defines for nmode
(def-exported-constant "NAUTO" 0)
(def-exported-constant "NNORMALIZE" 1)

;;; defines for acbuf
(def-exported-constant "AC_CLEAR" 0)
(def-exported-constant "AC_ACCUMULATE" 1)
(def-exported-constant "AC_CLEAR_ACCUMULATE" 2)
(def-exported-constant "AC_RETURN" 3)
(def-exported-constant "AC_MULT" 4)
(def-exported-constant "AC_ADD" 5)

;;; defines for clipplane
(def-exported-constant "CP_OFF" 0)
(def-exported-constant "CP_ON" 1)
(def-exported-constant "CP_DEFINE" 2)

;;; defines for scrbox
(def-exported-constant "SB_RESET" 0)
(def-exported-constant "SB_TRACK" 1)
(def-exported-constant "SB_HOLD" 2)

;;; defines for readdisplay
(def-exported-constant "RD_FREEZE" #x01)
(def-exported-constant "RD_ALPHAONE" #x02)
(def-exported-constant "RD_IGNORE_UNDERLAY" #x04)
(def-exported-constant "RD_IGNORE_OVERLAY" #x08)
(def-exported-constant "RD_IGNORE_PUP" #x10)
(def-exported-constant "RD_OFFSCREEN" #x20)

;;;
;;; Start defines for getgdesc
;;;

(def-exported-constant "GD_XPMAX" 0)
(def-exported-constant "GD_YPMAX" 1)
(def-exported-constant "GD_XMMAX" 2)
(def-exported-constant "GD_YMMAX" 3)
(def-exported-constant "GD_ZMIN" 4)
(def-exported-constant "GD_ZMAX" 5)
(def-exported-constant "GD_BITS_NORM_SNG_RED" 6)
(def-exported-constant "GD_BITS_NORM_SNG_GREEN" 7)
(def-exported-constant "GD_BITS_NORM_SNG_BLUE" 8)
(def-exported-constant "GD_BITS_NORM_DBL_RED" 9)
(def-exported-constant "GD_BITS_NORM_DBL_GREEN" 10)
(def-exported-constant "GD_BITS_NORM_DBL_BLUE" 11)
(def-exported-constant "GD_BITS_NORM_SNG_CMODE" 12)
(def-exported-constant "GD_BITS_NORM_DBL_CMODE" 13)
(def-exported-constant "GD_BITS_NORM_SNG_MMAP" 14)
(def-exported-constant "GD_BITS_NORM_DBL_MMAP" 15)
(def-exported-constant "GD_BITS_NORM_ZBUFFER" 16)
(def-exported-constant "GD_BITS_OVER_SNG_CMODE" 17)
(def-exported-constant "GD_BITS_UNDER_SNG_CMODE" 18)
(def-exported-constant "GD_BITS_PUP_SNG_CMODE" 19)
(def-exported-constant "GD_BITS_NORM_SNG_ALPHA" 21)
(def-exported-constant "GD_BITS_NORM_DBL_ALPHA" 22)
(def-exported-constant "GD_BITS_CURSOR" 23)
(def-exported-constant "GD_OVERUNDER_SHARED" 24)
(def-exported-constant "GD_BLEND" 25)
(def-exported-constant "GD_CIFRACT" 26)
(def-exported-constant "GD_CROSSHAIR_CINDEX" 27)
(def-exported-constant "GD_DITHER" 28)
(def-exported-constant "GD_LINESMOOTH_CMODE" 30)
(def-exported-constant "GD_LINESMOOTH_RGB" 31)
(def-exported-constant "GD_LOGICOP" 33)
(def-exported-constant "GD_NSCRNS" 35)
(def-exported-constant "GD_NURBS_ORDER" 36)
(def-exported-constant "GD_NBLINKS" 37)
(def-exported-constant "GD_NVERTEX_POLY" 39)
(def-exported-constant "GD_PATSIZE_64" 40)
(def-exported-constant "GD_PNTSMOOTH_CMODE" 41)
(def-exported-constant "GD_PNTSMOOTH_RGB" 42)
(def-exported-constant "GD_PUP_TO_OVERUNDER" 43)
(def-exported-constant "GD_READSOURCE" 44)
(def-exported-constant "GD_READSOURCE_ZBUFFER" 48)
(def-exported-constant "GD_STEREO" 50)
(def-exported-constant "GD_SUBPIXEL_LINE" 51)
(def-exported-constant "GD_SUBPIXEL_PNT" 52)
(def-exported-constant "GD_SUBPIXEL_POLY" 53)
(def-exported-constant "GD_TRIMCURVE_ORDER" 54)
(def-exported-constant "GD_WSYS" 55)
(def-exported-constant "GD_ZDRAW_GEOM" 57)
(def-exported-constant "GD_ZDRAW_PIXELS" 58)
(def-exported-constant "GD_SCRNTYPE" 61)
(def-exported-constant "GD_TEXTPORT" 62)
(def-exported-constant "GD_NMMAPS" 63)
(def-exported-constant "GD_FRAMEGRABBER" 64)
(def-exported-constant "GD_TIMERHZ" 66)
(def-exported-constant "GD_DBBOX" 67)
(def-exported-constant "GD_AFUNCTION" 68)
(def-exported-constant "GD_ALPHA_OVERUNDER" 69)
(def-exported-constant "GD_BITS_ACBUF" 70)
(def-exported-constant "GD_BITS_ACBUF_HW" 71)
(def-exported-constant "GD_BITS_STENCIL" 72)
(def-exported-constant "GD_CLIPPLANES" 73)
(def-exported-constant "GD_FOGVERTEX" 74)
(def-exported-constant "GD_LIGHTING_TWOSIDE" 76)
(def-exported-constant "GD_POLYMODE" 77)
(def-exported-constant "GD_POLYSMOOTH" 78)
(def-exported-constant "GD_SCRBOX" 79)
(def-exported-constant "GD_TEXTURE" 80)
(def-exported-constant "GD_FOGPIXEL" 81)
(def-exported-constant "GD_TEXTURE_PERSP" 82)
(def-exported-constant "GD_MUXPIPES" 83)

;;; return value for inquiries when there is no limit
(def-exported-constant "GD_NOLIMIT" -2)

;;; return values for gd-wsys
(def-exported-constant "GD_WSYS_NONE" 0)
(def-exported-constant "GD_WSYS_4S" 1)

;;; return values for gd-scrntype
(def-exported-constant "GD_SCRNTPYE_WM" 0)
(def-exported-constant "GD_SCRNTPYE_NOWM" 1)

;;; end defines for getgdesc


;;;
;;; Start NURBS interface definitions
;;;

;;; NURBS Redering Properties
(def-exported-constant "N_PIXEL_TOLERANCE" 1)
(def-exported-constant "N_CULLING" 2)
(def-exported-constant "N_DISPLAY" 3)
(def-exported-constant "N_ERRORCHECKING" 4)
(def-exported-constant "N_SUBDIVISIONS" 5)
(def-exported-constant "N_S_STEPS" 6)
(def-exported-constant "N_T_STEPS" 7)
(def-exported-constant "N_TILES" 8)
(def-exported-constant "N_TMP1" 9)
(def-exported-constant "N_TMP2" 10)
(def-exported-constant "N_TMP3" 11)
(def-exported-constant "N_TMP4" 12)
(def-exported-constant "N_TMP5" 13)
(def-exported-constant "N_TMP6" 14)

(def-exported-constant "N_FILL" 1.0)
(def-exported-constant "N_OUTLINE_POLY" 2.0)
(def-exported-constant "N_OUTLINE_PATCH" 5.0)
(def-exported-constant "N_ISOLINE_S" 12.0)

;;;
;;; FLAGS FOR NURBS SURFACES AND CURVES 			
;;; WARNING: Any changes to these flags should be checked against the 
;;; decoding macros in nurbs.h.
;;;
;;; Bit: 876 5432 10 
;;;     |ttt|nnnn|rr|   :  rr - 2 bits = 1 if rational coordinate exists
;;;	 	       : nnn - 4 bits for number of coordinates
;;;		       : ttt - 3 bits for type of data (color, position, etc.)
;;;	
;;;
;;; NURBS data type
;;; N_T_ST	 	0	 parametric space data
;;; N_T_XYZ		1	 model space data
;;; N_T_TEX		2	 texture coordinate data
;;; N_T_RGBA		3	 color data
;;;
;;; Number of coordinates per datum
;;; N_COORD2	 	2	 2 coords
;;; N_COORD3		3	 3 coords
;;; N_COORD4		4	 4 coords
;;; N_COORD5		5	 5 coords
;;;
;;; rational or non-rational data and position in memory 
;;; N_NONRATIONAL	0	 non-rational data
;;; N_RATIONAL		1	 rational data with rat coord after rest
;;;
;;; N_MKFLAG(t,n,r) ((t<<6) | (n<<2) | r)
;;;	
;;;
(def-exported-constant "N_ST" #x8)	;; N_MKFLAG( N_T_ST,  N_COORD2, N_NONRATIONAL )
(def-exported-constant "N_STW" #xd)	;; N_MKFLAG( N_T_ST,  N_COORD3, N_RATIONAL )
(def-exported-constant "N_XYZ" #x4c)	;; N_MKFLAG( N_T_XYZ, N_COORD3, N_NONRATIONAL )
(def-exported-constant "N_XYZW" #x51)	;; N_MKFLAG( N_T_XYZ, N_COORD4, N_RATIONAL )
(def-exported-constant "N_TEX" #x88)	;; N_MKFLAG( N_T_TEX, N_COORD2, N_NONRATIONAL )
(def-exported-constant "N_TEXW" #x8d)	;; N_MKFLAG( N_T_TEX, N_COORD3, N_RATIONAL )
(def-exported-constant "N_RGBA" #xd0)	;; N_MKFLAG( N_T_RGBA, N_COORD4, N_NONRATIONAL )
(def-exported-constant "N_RGBAW" #xd5);; N_MKFLAG( N_T_RGBA, N_COORD5, N_RATIONAL )

;;; New versions of above constants
(def-exported-constant "N_P2D" #x8)	;; N_MKFLAG( N_T_ST,  N_COORD2, N_NONRATIONAL )
(def-exported-constant "N_P2DR" #xd) 	;; N_MKFLAG( N_T_ST,  N_COORD3, N_RATIONAL )
(def-exported-constant "N_V3D" #x4c)	;; N_MKFLAG( N_T_XYZ, N_COORD3, N_NONRATIONAL )
(def-exported-constant "N_V3DR" #x51)	;; N_MKFLAG( N_T_XYZ, N_COORD4, N_RATIONAL ) 
(def-exported-constant "N_T2D" #x88)	;; N_MKFLAG( N_T_TEX, N_COORD2, N_NONRATIONAL )
(def-exported-constant "N_T2DR" #x8d)	;; N_MKFLAG( N_T_TEX, N_COORD3, N_RATIONAL )
(def-exported-constant "N_C4D" #xd0)	;; N_MKFLAG( N_T_RGBA, N_COORD4, N_NONRATIONAL )
(def-exported-constant "N_C4DR" #xd5)	;; N_MKFLAG( N_T_RGBA, N_COORD5, N_RATIONAL )

;;; end NURBS interface definitions



;;;
;;; Start lighting model defines 
;;;

(def-exported-constant "LMNULL" 0.0)

;;; Matrix modes
(def-exported-constant "MSINGLE" 0)
(def-exported-constant "MPROJECTION" 1)
(def-exported-constant "MVIEWING" 2)
(def-exported-constant "MTEXTURE" 3)

;;; Light constants
(def-exported-constant "MAXLIGHTS" 8)
(def-exported-constant "MAXRESTRICTIONS" 4)

;;; Material properties
(def-exported-constant "DEFMATERIAL" 0)
(def-exported-constant "EMISSION" 1)
(def-exported-constant "AMBIENT" 2)
(def-exported-constant "DIFFUSE" 3)
(def-exported-constant "SPECULAR" 4)
(def-exported-constant "SHININESS" 5)
(def-exported-constant "COLORINDEXES" 6)
(def-exported-constant "ALPHA" 7)

;;; Light properties
(def-exported-constant "DEFLIGHT" 100)
(def-exported-constant "LCOLOR" 101)
(def-exported-constant "POSITION" 102)
(def-exported-constant "SPOTDIRECTION" 103)
(def-exported-constant "SPOTLIGHT" 104)

;;; Lighting model properties
(def-exported-constant "DEFLMODEL" 200)
(def-exported-constant "LOCALVIEWER" 201)
(def-exported-constant "ATTENUATION" 202)
(def-exported-constant "ATTENUATION2" 203)
(def-exported-constant "TWOSIDE" 204)

;;; Target constants
(def-exported-constant "MATERIAL" 1000)
(def-exported-constant "BACKMATERIAL" 1001)
(def-exported-constant "LIGHT0" 1100)
(def-exported-constant "LIGHT1" 1101)
(def-exported-constant "LIGHT2" 1102)
(def-exported-constant "LIGHT3" 1103)
(def-exported-constant "LIGHT4" 1104)
(def-exported-constant "LIGHT5" 1105)
(def-exported-constant "LIGHT6" 1106)
(def-exported-constant "LIGHT7" 1107)
(def-exported-constant "LMODEL" 1200)

;;; Lmcolor modes
(def-exported-constant "LMC_COLOR" 0)
(def-exported-constant "LMC_EMISSION" 1)
(def-exported-constant "LMC_AMBIENT" 2)
(def-exported-constant "LMC_DIFFUSE" 3)
(def-exported-constant "LMC_SPECULAR" 4)
(def-exported-constant "LMC_AD" 5)
(def-exported-constant "LMC_NULL" 6)

;;; End lighting model defines


;;;
;;; Start texturing defines
;;;

;;; texdef param token values
(def-exported-constant "TX_MINFILTER" #x100)
(def-exported-constant "TX_MAGFILTER" #x200)
(def-exported-constant "TX_WRAP" #x300)
(def-exported-constant "TX_WRAP_S" #x310)
(def-exported-constant "TX_WRAP_T" #x320)
(def-exported-constant "TX_TILE" #x400)
(def-exported-constant "TX_BORDER" #x500)
(def-exported-constant "TX_NULL" #x000)

;;; texture filter choices
(def-exported-constant "TX_POINT" #x110)
(def-exported-constant "TX_BILINEAR" #x220)
(def-exported-constant "TX_MIPMAP" #x120)
(def-exported-constant "TX_MIPMAP_POINT" #x121)
(def-exported-constant "TX_MIPMAP_LINEAR" #x122)
(def-exported-constant "TX_MIPMAP_BILINEAR" #x123)
(def-exported-constant "TX_MIPMAP_TRILINEAR" #x124)

;;; texture wrapping access choices
(def-exported-constant "TX_REPEAT" #x301)
(def-exported-constant "TX_CLAMP" #x302)
(def-exported-constant "TX_SELECT" #x303)

;;; texture targets
(def-exported-constant "TX_TEXTURE_0" 0)

;;; texture environment definitions
(def-exported-constant "TV_MODULATE" #x101)
(def-exported-constant "TV_BLEND" #x102)
(def-exported-constant "TV_DECAL" #x103)
(def-exported-constant "TV_COLOR" #x200)
(def-exported-constant "TV_NULL" #x000)

;;; texture environment targets
(def-exported-constant "TV_ENV0" 0)

;;; defines for texgen
(def-exported-constant "TX_S" 0)
(def-exported-constant "TX_T" 1)
(def-exported-constant "TG_OFF" 0)
(def-exported-constant "TG_ON" 1)
(def-exported-constant "TG_CONTOUR" 2)
(def-exported-constant "TG_LINEAR" 3)
(def-exported-constant "TG_SPHEREMAP" 4)
(def-exported-constant "TG_REFRACTMAP" 5)	;; not yet approved!

;;; End texturing defines


;;;
;;; Start Distributed Graphics Library defines
;;;

(def-exported-constant "DGLSINK" 0)	         ;; sink connection
(def-exported-constant "DGLLOCAL" 1)	         ;; local connection
(def-exported-constant "DGLTSOCKET" 2)	 ;; tcp socket connection
(def-exported-constant "DGL4DDN" 3)	         ;; 4DDN (DECnet)

;;; End Distributed Graphics Library defines

;;;
;;; Start obsolete defines - included for backwards compatibility
;;;

;;; #define GLDEF			__GL_GL_H__

(def-exported-constant "PUP_CURSOR" 1)

(def-exported-constant "FATAL" 1)             ;; exit from program after printing message
(def-exported-constant "WARNING" 2)         ;; print message and continue
(def-exported-constant "ASK_CONT" 3)       ;; ask if program should continue
(def-exported-constant "ASK_RESTART" 4) ;; ask if program should be restarted

;;; high-resolution monitor
(def-exported-constant "XMAXSCREEN" 1279)
(def-exported-constant "YMAXSCREEN" 1023)

;;; medium-resolution monitor
(def-exported-constant "XMAXMEDIUM" 1023)
(def-exported-constant "YMAXMEDIUM" 767)

;;; RS-170
(def-exported-constant "XMAX170" 645)
(def-exported-constant "YMAX170" 484)

;;; PAL
(def-exported-constant "XMAXPAL" 779)
(def-exported-constant "YMAXPAL" 574)

;;; End obsolete defines









