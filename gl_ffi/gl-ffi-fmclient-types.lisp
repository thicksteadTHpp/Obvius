;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  fmclient-types.lisp
;;;
;;;  Font Manager Types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package 'gl)


;;; typedef long *fmfonthandle;
(def-exported-foreign-synonym-type "fmfonthandle" (:pointer long))


;;; typedef struct fmfontinfo {
;;;	long printermatched;
;;;	long reserved0;
;;;	double matrix00;
;;;	double matrix01;
;;; 	double matrix10;
;;;	double matrix11;
;;;	long type;		/* see fmfontinfo->type  below */
;;;	long encoding;
;;;	long fixed_width;
;;;	long xorig;
;;;	long yorig;
;;;	long xsize;
;;;	long ysize;
;;;	long height;
;;;	long nglyphs;
;;;	long bitsdeep;	    /* if nonzero, the depth of all glyphs in font */
;;;	long width;	    /* the largest setwidth in the font */
;;;	long resolution;
;;;	long weight;	    /* [0..1000], the # of black bits that would be on
;;;			       if this were an of 1000-pixel chars */
;;;	long reserved1;
;;;	long reserved2;
;;;	long reserved3;
;;;	long padding[27];   /* for future expansion */
;;; } fmfontinfo;

(def-exported-foreign-struct "fmfontinfo"
  (printermatched :type long)
  (reserved0 :type long)
  (matrix00 :type double)
  (matrix01 :type double)
  (matrix10 :type double)
  (matrix11 :type double)
  (type :type long)
  (encoding :type long)
  (fixed-width :type long)
  (xorig :type long)
  (yorig :type long)
  (xsize :type long)
  (ysize :type long)
  (height :type long)
  (nglyphs :type long)
  (bitsdeep :type long)
  (width :type long)
  (resolution :type long)
  (weight :type long)
  (reserved1 :type long)
  (reserved2 :type long)
  (reserved3 :type long)
  (padding :type (:array long (27))))


;;; typedef struct fmglyphinfo {
;;;	long xsize, ysize;		/* dimensions of the glyph in pixels */
;;;	long xorig, yorig;		/* origin */
;;; 	float xmove, ymove;		/* move  */
;;;	long gtype;			/* The type of glyph this is. */
;;;	long bitsdeep;			/* depth of the pixels, if pixels */
;;; } fmglyphinfo;

(def-exported-foreign-struct "fmglyphinfo"
  (xsize :type long)
  (ysize :type long)
  (xmove :type float)
  (ymove :type float)
  (gtype :type long)
  (bitsdeep :type long))

