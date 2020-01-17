;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  fmclient-functions.lisp
;;;
;;;  Font Manager Functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package 'gl)

;;; int		fmcachelimit();
(def-exported-c-function ("fmcachelimit" (:return-type int)))

;;; void		fmconcatpagematrix(double [3][2]);
(def-exported-c-function "fmconcatpagematrix"
    ((:array double (3 2) :row-major)))

;;; void		fmenumerate();
(def-exported-c-function "fmenumerate")

;;; fmfonthandle	fmfindfont(const char *);
(def-exported-c-function ("fmfindfont" (:return-type fmfonthandle))
    (String))

;;; void		fmfreefont(fmfonthandle);
(def-exported-c-function "fmfreefont"
    (fmfonthandle))

;;; char		*fmfontpath(); /* pass back the path */
(def-exported-c-function ("fmfontpath" (:return-type String)))

;;; void		fmsetpath(const char *);/* pass in the path */
(def-exported-c-function "fmsetpath"
    (String))

;;; int		fmgetcacheused();
(def-exported-c-function ("fmgetcacheused" (:return-type int)))

;;; long		fmgetchrwidth(fmfonthandle, const unsigned char);
(def-exported-c-function ("fmgetchrwidth" (:return-type long))
    (fmfonthandle u-char))

;;; int		fmgetcomment(fmfonthandle, int, char *);
(def-exported-c-function ("fmgetcomment" (:return-type int))
    (fmfonthandle int String))

;;; int		fmgetfontinfo(fmfonthandle, fmfontinfo *);
(def-exported-c-function ("fmgetfontinfo" (:return-type int))
    (fmfonthandle (:pointer fmfontinfo)))

;;; int		fmgetfontname(fmfonthandle, int len, char *);
(def-exported-c-function ("fmgetfontname" (:return-type int))
    (fmfonthandle int String))

;;; void		fmgetpagematrix(double [3][2]);
(def-exported-c-function "fmgetpagematrix"
    ((:array double (3 2) :row-major)))

;;; long		fmgetstrwidth(fmfonthandle, const char *);
(def-exported-c-function ("fmgetstrwidth" (:return-type long))
    (fmfonthandle String))

;;; long		fmgetwholemetrics(fmfonthandle, fmglyphinfo *);
(def-exported-c-function ("fmgetwholemetrics" (:return-type long))
    (fmfonthandle (:pointer fmglyphinfo)))

;;; void		fminitpagematrix();
(def-exported-c-function "fminitpagematrix")

;;; fmfonthandle	fmmakefont(fmfonthandle, double[3][2]);
(def-exported-c-function ("fmmakefont" (:return-type fmfonthandle))
    (fmfonthandle (:array double (3 2) :row-major)))

;;; void		fmcacheenable();
(def-exported-c-function "fmcacheenable")

;;; void		fmcachedisable();
(def-exported-c-function "fmcachedisable")

;;; long		fmoutchar(fmfonthandle, const unsigned int);
(def-exported-c-function ("fmoutchar" (:return-type long))
    (fmfonthandle u-int))

;;; void		fmprintermatch(int);
(def-exported-c-function "fmprintermatch"
    (int))

;;; int		fmprstr(const char *);
(def-exported-c-function ("fmprstr" (:return-type int))
    (String))

;;; void		fmrotatepagematrix(double);
(def-exported-c-function "fmrotatepagematrix"
    (double))

;;; fmfonthandle	fmscalefont(fmfonthandle, double);
(def-exported-c-function ("fmscalefont" (:return-type fmfonthandle))
    (fmfonthandle double))

;;; void		fmsetcachelimit(int);
(def-exported-c-function "fmsetcachelimit"
    (int))

;;; void		fmsetfont(fmfonthandle);
(def-exported-c-function "fmsetfont"
    (fmfonthandle))

;;; void		fmsetpagematrix(double [3][2]);
(def-exported-c-function "fmsetpagematrix"
    ((:array double (3 2) :row-major)))

;;; void		fmscalepagematrix(double);
(def-exported-c-function "fmscalepagematrix"
    (double))

