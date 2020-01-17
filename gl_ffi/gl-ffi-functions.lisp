;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  gl-functions.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package 'gl)

;;; extern GLXconfig *      GLXgetconfig( void *, long, GLXconfig * );
(def-exported-c-function ("GLXgetconfig" (:return-type (:pointer GLXconfig)))
    ((:pointer void) long (:pointer GLXconfig)))

;;; extern void     acbuf( long, float );
(def-exported-c-function "acbuf"
    (long float))

;;; extern void     acsize( long );
(def-exported-c-function "acsize"
    (long))

;;;???
;;; extern void     addtopup( long, String, ... );

;;; extern void     afunction( long, long );
(def-exported-c-function "afunction"
    (long long))

;;; extern void     arc( Coord, Coord, Coord, Angle, Angle );
(def-exported-c-function "arc"
    (Coord Coord Coord Angle Angle))

;;; extern void     arcf( Coord, Coord, Coord, Angle, Angle );
(def-exported-c-function "arcf"
    (Coord Coord Coord Angle Angle))
    
;;; extern void     arcfi( Icoord, Icoord, Icoord, Angle, Angle );
(def-exported-c-function "arcfi"
    (Icoord Icoord Icoord Angle Angle))

;;; extern void     arcfs( Scoord, Scoord, Scoord, Angle, Angle );
(def-exported-c-function "arcfs"
    (Scoord Scoord Scoord Angle Angle))

;;; extern void     arci( Icoord, Icoord, Icoord, Angle, Angle );
(def-exported-c-function "arci"
    (Icoord Icoord Icoord Angle Angle))
  
;;; extern void     arcs( Scoord, Scoord, Scoord, Angle, Angle );
(def-exported-c-function "arcs"
    (Scoord Scoord Scoord Angle Angle))

;;; extern void     attachcursor( Device, Device );
(def-exported-c-function "attachcursor"
    (Device Device))

;;; extern void     backbuffer( Boolean );
(def-exported-c-function "backbuffer"
    (Boolean))

;;; extern void     backface( Boolean );
(def-exported-c-function "backface"
    (Boolean))

;;; extern void     bbox2( Screencoord, Screencoord, Coord, Coord, Coord, Coord );
(def-exported-c-function "bbox2"
    (Screencoord Screencoord Coord Coord Coord Coord))

;;; extern void     bbox2i( Screencoord, Screencoord, Icoord, Icoord, Icoord, Icoord );
(def-exported-c-function "bbox2i"
    (Screencoord Screencoord Icoord Icoord Icoord Icoord))

;;; extern void     bbox2s( Screencoord, Screencoord, Scoord, Scoord, Scoord, Scoord );
(def-exported-c-function "bbox2s"
    (Screencoord Screencoord Scoord Scoord Scoord Scoord))

;;; extern void     bgnclosedline( void );
(def-exported-c-function "bgnclosedline")

;;; extern void     bgncurve( void );
(def-exported-c-function "bgncurve")

;;; extern void     bgnline( void );
(def-exported-c-function "bgnline")

;;; extern void     bgnpoint( void );
(def-exported-c-function "bgnpoint")

;;; extern void     bgnpolygon( void );
(def-exported-c-function "bgnpolygon")

;;; extern void     bgnqstrip( void );
(def-exported-c-function "bgnqstrip")

;;; extern void     bgnsurface( void );
(def-exported-c-function "bgnsurface")

;;; extern void     bgntmesh( void );
(def-exported-c-function "bgntmesh")

;;; extern void     bgntrim( void );
(def-exported-c-function "bgntrim")

;;; extern void     blankscreen( Boolean );
(def-exported-c-function "blankscreen"
    (Boolean))

;;; extern void     blanktime( long );
(def-exported-c-function "blanktime"
    (long))

;;; extern void     blendfunction( long, long );
(def-exported-c-function "blendfunction"
    (long long))

;;; extern void     blink( short, Colorindex, short, short, short );
(def-exported-c-function "blink"
    (short Colorindex short short short))

;;; extern long     blkqread( short[], short );
(def-exported-c-function "blkqread"
    ((:array short) short))

;;; extern void     c3f( float[3] );
(def-exported-c-function "c3f"
    ((:array float (3))))

;;; extern void     c3i( long[3] );
(def-exported-c-function "c3i"
    ((:array long (3))))

;;; extern void     c3s( short[3] );
(def-exported-c-function "c3s"
    ((:array short (3))))

;;; extern void     c4f( float[4] );
(def-exported-c-function "c4f"
    ((:array float (4))))

;;; extern void     c4i( long[4] );
(def-exported-c-function "c4i"
    ((:array long (4))))

;;; extern void     c4s( short[4] );
(def-exported-c-function "c4s"
    ((:array short (4))))

;;; ???
;;; extern void     callfunc( __PFV_, long, ... );          /* not recommended */

;;; extern void     callobj( Object );
(def-exported-c-function "callobj"
    (Object))

;;; extern void     charstr( String );
(def-exported-c-function "charstr"
    (String))

;;; extern void     chunksize( long );
(def-exported-c-function "chunksize"
    (long))

;;; extern void     circ( Coord, Coord, Coord );
(def-exported-c-function "circ"
    (Coord Coord Coord))

;;; extern void     circf( Coord, Coord, Coord );
(def-exported-c-function "circf"
    (Coord Coord Coord))

;;; extern void     circfi( Icoord, Icoord, Icoord );
(def-exported-c-function "circfi"
    (Icoord Icoord Icoord))

;;; extern void     circfs( Scoord, Scoord, Scoord );
(def-exported-c-function "circfs"
    (Scoord Scoord Scoord))

;;; extern void     circi( Icoord, Icoord, Icoord );
(def-exported-c-function "circi"
    (Icoord Icoord Icoord))

;;; extern void     circs( Scoord, Scoord, Scoord );
(def-exported-c-function "circs"
    (Scoord Scoord Scoord))

;;; extern void     clear( void );
(def-exported-c-function "clear")

;;; extern void     clearhitcode( void );                   /* not recommended */
(def-exported-c-function "clearhitcode")

;;; extern void     clipplane( long, long, float[] );
(def-exported-c-function "clipplane"
    (long long (:array float)))

;;; extern void     clkoff( void );
(def-exported-c-function "clkoff")

;;; extern void     clkon( void );
(def-exported-c-function "clkon")

;;; extern void     closeobj( void );
(def-exported-c-function "closeobj")

;;; extern void     cmode( void );
(def-exported-c-function "cmode")

;;; extern void     cmov( Coord, Coord, Coord );
(def-exported-c-function "cmov"
    (Coord Coord Coord))

;;; extern void     cmov2( Coord, Coord );
(def-exported-c-function "cmov2"
    (Coord Coord))

;;; extern void     cmov2i( Icoord, Icoord );
(def-exported-c-function "cmov2i"
    (Icoord Icoord))

;;; extern void     cmov2s( Scoord, Scoord );
(def-exported-c-function "cmov2s"
    (Scoord Scoord))

;;; extern void     cmovi( Icoord, Icoord, Icoord );
(def-exported-c-function "cmovi"
    (Icoord Icoord Icoord))

;;; extern void     cmovs( Scoord, Scoord, Scoord );
(def-exported-c-function "cmovs"
    (Scoord Scoord Scoord))

;;; extern void     color( Colorindex );
(def-exported-c-function "color"
    (Colorindex))

;;; extern void     colorf( float );
(def-exported-c-function "colorf"
    (float))

;;; extern void     compactify( Object );
(def-exported-c-function "compactify"
    (Object))

;;; extern void     concave( Boolean );
(def-exported-c-function "concave"
    (Boolean))

;;; extern void     cpack( unsigned long );
(def-exported-c-function "cpack"
    (u-long))

;;; extern void     crv( Coord[4][3] );
(def-exported-c-function "crv"
    ((:array Coord (4 3) :row-major)))

;;; extern void     crvn( long, Coord[][3] );
(def-exported-c-function "crvn"
    (long (:array Coord)))

;;; extern void     curorigin( short, short, short );
(def-exported-c-function "curorigin"
    (short short short))

;;; extern void     cursoff( void );
(def-exported-c-function "cursoff")

;;; extern void     curson( void );
(def-exported-c-function "curson")

;;; extern void     curstype( long );
(def-exported-c-function "curstype"
    (long))

;;; extern void curvebasis( short );
(def-exported-c-function "curvebasis"
    (short))

;;; extern void curveit( short );
(def-exported-c-function "curveit"
    (short))

;;; extern void curveprecision( short );
(def-exported-c-function "curveprecision"
    (short))

;;; extern void cyclemap( short, short, short );
(def-exported-c-function "cyclemap"
    (short short short))

;;; extern void czclear( unsigned long, long );
(def-exported-c-function "czclear"
  (u-long long))

;;; extern void dbtext( char[8] );
(def-exported-c-function "dbtext"
  ((:array char (8))))

;;; extern void defbasis( short, Matrix );
(def-exported-c-function "defbasis"
    (short Matrix))

;;; extern void defcursor( short, unsigned short[128] );
(def-exported-c-function "defcursor"
    (short (:array u-short (128))))

;;; extern void deflinestyle( short, Linestyle );
(def-exported-c-function "deflinestyle"
    (short Linestyle))

;;; extern void defpattern( short, short, unsigned short[] );
(def-exported-c-function "defpattern"
    (short short (:array u-short)))

;;; ???  ;;; extern long defpup( String, ... );

;;; extern void defrasterfont( short, short, short, Fontchar[], short, unsigned short[] );
(def-exported-c-function "defrasterfont"
    (short short short (:array Fontchar) short (:array u-short)))

;;; extern void delobj( Object );
(def-exported-c-function "delobj"
    (Object))

;;; extern void deltag( Tag );
(def-exported-c-function "deltag"
    (Tag))

;;; extern void depthcue( Boolean );
(def-exported-c-function "depthcue"
    (Boolean))

;;; extern void dglclose( long );
(def-exported-c-function "dglclose"
    (long))

;;; extern long dglopen( String, long );
(def-exported-c-function "dglopen"
    (String long))

;;; extern void dither( long );
(def-exported-c-function "dither"
    (long))

;;; extern long dopup( long );
(def-exported-c-function "dopup"
    (long))

;;; extern void doublebuffer( void );
(def-exported-c-function "doublebuffer")

;;; extern void draw( Coord, Coord, Coord );
(def-exported-c-function "draw"
    (Coord Coord Coord))

;;; extern void draw2( Coord, Coord );
(def-exported-c-function "draw2"
    (Coord Coord))

;;; extern void draw2i( Icoord, Icoord );
(def-exported-c-function "draw2i"
    (Icoord Icoord))

;;; extern void draw2s( Scoord, Scoord );
(def-exported-c-function "draw2s"
    (Scoord Scoord))

;;; extern void drawi( Icoord, Icoord, Icoord );
(def-exported-c-function "drawi"
    (Icoord Icoord Icoord))

;;; extern void drawmode( long );
(def-exported-c-function "drawmode"
    (long))

;;; extern void draws( Scoord, Scoord, Scoord );
(def-exported-c-function "draws"
    (Scoord Scoord Scoord))

;;; extern void editobj( Object );
(def-exported-c-function "editobj"
    (Object))

;;; extern void endclosedline( void );
(def-exported-c-function "endclosedline")

;;; extern void endcurve( void );
(def-exported-c-function "endcurve")

;;; extern long endfeedback( void * );
(def-exported-c-function ("endfeedback" (:return-type long))
    ((:pointer void)))

;;; extern void endfullscrn( void );
(def-exported-c-function "endfullscrn")

;;; extern void endline( void );
(def-exported-c-function "endline")

;;; extern long endpick( short[] );
(def-exported-c-function "endpick"
    ((:array short)))

;;; extern void endpoint( void );
(def-exported-c-function "endpoint")

;;; extern void endpolygon( void );
(def-exported-c-function "endpolygon")

;;; extern void endpupmode( void ); /* obsolete */
(def-exported-c-function "endpupmode")

;;; extern void endqstrip( void );
(def-exported-c-function "endqstrip")

;;; extern long endselect( short[] );
(def-exported-c-function ("endselect" (:return-type long))
    ((:array short)))

;;; extern void endsurface( void );
(def-exported-c-function "endsurface")

;;; extern void endtmesh( void );
(def-exported-c-function "endtmesh")

;;; extern void endtrim( void );
(def-exported-c-function "endtrim")

;;; extern void feedback( void *, long );
(def-exported-c-function "feedback"
    ((:pointer void) long))

;;; extern void finish( void );
(def-exported-c-function "finish")

;;; extern void fogvertex( long, float[] );
(def-exported-c-function "fogvertex"
    (long (:array float)))

;;; extern void font( short );
(def-exported-c-function "font"
    (short))

;;; extern void foreground( void );
(def-exported-c-function "foreground")

;;; extern void freepup( long );
(def-exported-c-function "freepup"
    (long))

;;; extern void frontbuffer( Boolean );
(def-exported-c-function "frontbuffer"
    (Boolean))

;;; extern void frontface( Boolean );
(def-exported-c-function "frontface"
    (Boolean))

;;; extern void fudge( long, long );
(def-exported-c-function "fudge"
    (long long))

;;; extern void fullscrn( void );
(def-exported-c-function "fullscrn")

;;; extern void gammaramp( short[256], short[256], short[256] );
(def-exported-c-function "gammaramp"
    ((:array short (256)) (:array short (256)) (:array short (256))))

;;; extern void gbegin( void ); /* not recommended */
(def-exported-c-function "gbegin")

;;; extern void gconfig( void );
(def-exported-c-function "gconfig")

;;; extern Object genobj( void );
(def-exported-c-function ("genobj" (:return-type Object)))

;;; extern Tag gentag( void );
(def-exported-c-function ("gentag" (:return-type Tag)))
    
;;; extern long getbackface( void );
(def-exported-c-function ("getbackface" (:return-type long)))

;;; extern long getbuffer( void );
(def-exported-c-function ("getbuffer" (:return-type long)))

;;; extern Boolean getbutton( Device );
(def-exported-c-function ("getbutton" (:return-type Boolean))
    (Device))

;;; extern Boolean getcmmode( void );
(def-exported-c-function ("getcmmode" (:return-type Boolean)))

;;; extern long getcolor( void );
(def-exported-c-function ("getcolor" (:return-type long)))

;;; extern void getcpos( short *, short * );
(def-exported-c-function "getcpos"
    ((:pointer short) (:pointer short)))

;;; extern void getcursor( short *, Colorindex *, Colorindex *, Boolean * );
(def-exported-c-function "getcursor"
    ((:pointer short) (:pointer Colorindex) (:pointer Colorindex) (:pointer Boolean)))

;;; extern Boolean getdcm( void );
(def-exported-c-function ("getdcm" (:return-type Boolean)))

;;; extern void getdepth( Screencoord *, Screencoord * ); /* obsolete */
(def-exported-c-function "getdepth"
    ((:pointer Screencoord) (:pointer Screencoord)))

;;; extern long getdescender( void );
(def-exported-c-function ("getdescender" (:return-type long)))

;;; extern void getdev( long, Device[], short[] );
(def-exported-c-function "getdev"
    (long (:array Device) (:array short)))

;;; extern long getdisplaymode( void );
(def-exported-c-function ("getdisplaymode" (:return-type long)))

;;; extern long getdrawmode( void );
(def-exported-c-function ("getdrawmode" (:return-type long)))

;;; extern long getfont( void );
(def-exported-c-function ("getfont" (:return-type long)))

;;; extern long getgdesc( long );
(def-exported-c-function ("getgdesc" (:return-type long))
    (long))

;;; extern void getgpos( Coord *, Coord *, Coord *, Coord * );
(def-exported-c-function "getgpos"
    ((:pointer Coord) (:pointer Coord) (:pointer Coord) (:pointer Coord)))

;;; extern long getheight( void );
(def-exported-c-function ("getheight" (:return-type long)))

;;; extern long gethitcode( void ); /* not recommended */
(def-exported-c-function ("gethitcode" (:return-type long)))

;;; extern Boolean getlsbackup( void ); /* not recommended */
(def-exported-c-function ("getlsbackup" (:return-type Boolean)))

;;; extern long getlsrepeat( void );
(def-exported-c-function ("getlsrepeat" (:return-type long)))

;;; extern long getlstyle( void );
(def-exported-c-function ("getlstyle" (:return-type long)))

;;; extern long getlwidth( void );
(def-exported-c-function ("getlwidth" (:return-type long)))

;;; extern long getmap( void );
(def-exported-c-function ("getmap" (:return-type long)))

;;; extern void getmatrix( Matrix );
(def-exported-c-function "getmatrix"
    (Matrix))

;;; extern void getmcolor( Colorindex, short *, short *, short * );
(def-exported-c-function "getmcolor"
    (Colorindex (:pointer short) (:pointer short) (:pointer short)))

;;; extern long getmmode( void );
(def-exported-c-function ("getmmode" (:return-type long)))

;;; extern long getmonitor( void );
(def-exported-c-function ("getmonitor" (:return-type long)))

;;; extern void getnurbsproperty( long, float * );
(def-exported-c-function "getnurbsproperty"
    (long (:pointer float)))

;;; extern Object getopenobj( void );
(def-exported-c-function ("getopenobj" (:return-type Object)))

;;; extern void getorigin( long *, long * );
(def-exported-c-function "getorigin"
    ((:pointer long) (:pointer long)))

;;; extern long getothermonitor( void ); /* obsolete */
(def-exported-c-function ("getothermonitor" (:return-type long)))

;;; extern long getpattern( void );
(def-exported-c-function ("getpattern" (:return-type long)))

;;; extern long getplanes( void );
(def-exported-c-function ("getplanes" (:return-type long)))

;;; extern void getport( String ); /* obsolete */
(def-exported-c-function "getport"
    (String))

;;; extern Boolean getresetls( void ); /* not recommended */
(def-exported-c-function ("getresetls" (:return-type Boolean)))

;;; extern void getscrbox( long *, long *, long *, long * );
(def-exported-c-function "getscrbox"
    ((:pointer long) (:pointer long) (:pointer long) (:pointer long)))

;;; extern void getscrmask( Screencoord *, Screencoord *, Screencoord *, Screencoord * );
(def-exported-c-function "getscrmask"
    ((:pointer Screencoord) (:pointer Screencoord) (:pointer Screencoord) (:pointer Screencoord)))

;;; extern long getshade( void ); /* obsolete */
(def-exported-c-function ("getshade" (:return-type long)))

;;; extern void getsize( long *, long * );
(def-exported-c-function "getsize"
    ((:pointer long) (:pointer long)))

;;; extern long getsm( void );
(def-exported-c-function ("getsm" (:return-type long)))

;;; extern long getvaluator( Device );
(def-exported-c-function ("getvaluator" (:return-type long))
    (Device))

;;; extern long getvideo( long );
(def-exported-c-function ("getvideo" (:return-type long))
    (long))

;;; extern void getviewport( Screencoord *, Screencoord *, Screencoord *, Screencoord * );
(def-exported-c-function "getviewport"
    ((:pointer Screencoord) (:pointer Screencoord) (:pointer Screencoord) (:pointer Screencoord)))

;;; extern long getwritemask( void );
(def-exported-c-function ("getwritemask" (:return-type long)))
    
;;; extern long getwscrn( void );
(def-exported-c-function ("getwscrn" (:return-type long)))

;;; extern Boolean getzbuffer( void );
(def-exported-c-function ("getzbuffer" (:return-type Boolean)))

;;; extern void gexit( void );
(def-exported-c-function "gexit")

;;; extern void gflush( void );
(def-exported-c-function "gflush")

;;; extern void ginit( void ); /* not recommended */
(def-exported-c-function "ginit")

;;; extern void glcompat( long, long );
(def-exported-c-function "glcompat"
    (long long))

;;; extern long GLXlink( void *, GLXconfig * );
(def-exported-c-function ("GLXlink" (:return-type long))
    ((:pointer void) (:pointer GLXconfig)))

;;; extern long GLXunlink( void *, unsigned long );
(def-exported-c-function ("GLXunlink" (:return-type long))
    ((:pointer void) u-long))

;;; extern long GLXwinset( void *, unsigned long );
(def-exported-c-function ("GLXwinset" (:return-type long))
    ((:pointer void) u-long))

;;; extern void greset( void ); /* not recommended */
(def-exported-c-function "greset")

;;; extern void gRGBcolor( short *, short *, short * );
(def-exported-c-function "gRGBcolor"
    ((:pointer short) (:pointer short) (:pointer short)))

;;; extern void gRGBcursor( short *, short *, short *, short *, short *, short *, short *, Boolean * ); /* obsolete */
(def-exported-c-function "gRGBcursor"
    ((:pointer short) (:pointer short) (:pointer short)
     (:pointer short) (:pointer short) (:pointer short)
     (:pointer short) (:pointer Boolean)))

;;; extern void gRGBmask( short *, short *, short * );
(def-exported-c-function "gRGBmask"
    ((:pointer short) (:pointer short) (:pointer short)))

;;; extern void gselect( short[], long );
(def-exported-c-function "gselect"
    ((:array short) long))

;;; extern void gsync( void );
(def-exported-c-function "gsync")

;;; extern long gversion( char[12] );
(def-exported-c-function ("gversion" (:return-type long))
    ((:array char (12))))

;;; extern void iconsize( long, long );
(def-exported-c-function "iconsize"
    (long long))

;;; extern void icontitle( String );
(def-exported-c-function "icontitle"
    (String))

;;; extern void imakebackground( void );
(def-exported-c-function "imakebackground")

;;; extern void initnames( void );
(def-exported-c-function "initnames")

;;; extern Boolean ismex( void ); /* obsolete */
(def-exported-c-function ("ismex" (:return-type Boolean)))

;;; extern Boolean isobj( Object );
(def-exported-c-function ("isobj" (:return-type Boolean))
    (Object))

;;; extern Boolean isqueued( Device );
(def-exported-c-function ("isqueued" (:return-type Boolean))
    (Device))

;;; extern Boolean istag( Tag );
(def-exported-c-function ("istag" (:return-type Boolean))
    (Tag))

;;; extern void keepaspect( long, long );
(def-exported-c-function "keepaspect"
    (long long))

;;; extern void lampoff( Byte ); /* not recommended */
(def-exported-c-function "lampoff"
    (Byte))

;;; extern void lampon( Byte ); /* not recommended */
(def-exported-c-function "lampon"
    (Byte))

;;; extern void linesmooth( unsigned long );
(def-exported-c-function "linesmooth"
    (u-long))

;;; extern void linewidth( short );
(def-exported-c-function "linewidth"
    (short))

;;; extern void linewidthf( float );
(def-exported-c-function "linewidthf"
    (float))

;;; extern void lmbind( short, short );
(def-exported-c-function "lmbind"
    (short short))

;;; extern void lmcolor( long );
(def-exported-c-function "lmcolor"
    (long))

;;; extern void lmdef( short, short, short, float[] );
(def-exported-c-function "lmdef"
    (short short short (:array float)))

;;; extern void loadmatrix( Matrix );
(def-exported-c-function "loadmatrix"
    (Matrix))

;;; extern void loadname( short );
(def-exported-c-function "loadname"
    (short))

;;; extern void logicop( long );
(def-exported-c-function "logicop"
    (long))

;;; extern unsigned long readdisplay( Screencoord, Screencoord, Screencoord, Screencoord, unsigned long[], unsigned long );
(def-exported-c-function ("readdisplay" (:return-type u-long))
    (Screencoord Screencoord Screencoord Screencoord (:array u-long) long))

;;; extern void lookat( Coord, Coord, Coord, Coord, Coord, Coord, Angle );
(def-exported-c-function "lookat"
    (Coord Coord Coord Coord Coord Coord Angle))

;;; extern long lrectread( Screencoord, Screencoord, Screencoord, Screencoord, unsigned long[] );
(def-exported-c-function ("lrectread" (:return-type long))
    (Screencoord Screencoord Screencoord Screencoord (:array u-long)))

;;; extern void lrectwrite( Screencoord, Screencoord, Screencoord, Screencoord, unsigned long[] );
(def-exported-c-function "lrectwrite"
    (Screencoord Screencoord Screencoord Screencoord (:array u-long)))

;;; extern void lRGBrange( short, short, short, short, short, short, long, long );
(def-exported-c-function "lRGBrange"
    (short short short short short short long long))

;;; extern void lsbackup( Boolean ); /* not recommended */
(def-exported-c-function "lsbackup"
    (Boolean))

;;; extern void lsetdepth( long, long );
(def-exported-c-function "lsetdepth"
    (long long))

;;; extern void lshaderange( Colorindex, Colorindex, long, long );
(def-exported-c-function "lshaderange"
    (Colorindex Colorindex long long))

;;; extern void lsrepeat( long );
(def-exported-c-function "lsrepeat"
    (long))

;;; extern void makeobj( Object );
(def-exported-c-function "makeobj"
    (Object))

;;; extern void maketag( Tag );
(def-exported-c-function "maketag"
    (Tag))

;;; extern void mapcolor( Colorindex, short, short, short );
(def-exported-c-function "mapcolor"
    (Colorindex short short short))

;;; extern void mapw( Object, Screencoord, Screencoord, Coord *, Coord *, Coord *, Coord *, Coord *, Coord * );
(def-exported-c-function "mapw"
    (Object Screencoord Screencoord
	    (:pointer Coord) (:pointer Coord) (:pointer Coord)
	    (:pointer Coord) (:pointer Coord) (:pointer Coord)))

;;; extern void mapw2( Object, Screencoord, Screencoord, Coord *, Coord * );
(def-exported-c-function "mapw2"
    (Object Screencoord Screencoord (:pointer Coord) (:pointer Coord)))

;;; extern void maxsize( long, long );
(def-exported-c-function "maxsize"
    (long long))

;;; extern void minsize( long, long );
(def-exported-c-function "minsize"
    (long long))

;;; extern void mmode( short );
(def-exported-c-function "mmode"
    (short))

;;; extern void move( Coord, Coord, Coord );
(def-exported-c-function "move"
    (Coord Coord Coord))

;;; extern void move2( Coord, Coord );
(def-exported-c-function "move2"
    (Coord Coord))

;;; extern void move2i( Icoord, Icoord );
(def-exported-c-function "move2i"
    (Icoord Icoord))

;;; extern void move2s( Scoord, Scoord );
(def-exported-c-function "move2s"
    (Scoord Scoord))

;;; extern void movei( Icoord, Icoord, Icoord );
(def-exported-c-function "movei"
    (Icoord Icoord Icoord))

;;; extern void moves( Scoord, Scoord, Scoord );
(def-exported-c-function "moves"
    (Scoord Scoord Scoord))

;;; extern void mswapbuffers( long );
(def-exported-c-function "mswapbuffers"
    (long))

;;; extern void multimap( void );
(def-exported-c-function "multimap")

;;; extern void multmatrix( Matrix );
(def-exported-c-function "multmatrix"
    (Matrix))

;;; extern void n3f( float[3] );
(def-exported-c-function "n3f"
    ((:array float (3))))

;;; extern long newpup( void );
(def-exported-c-function ("newpup" (:return-type long)))

;;; extern void newtag( Tag, Tag, Offset );
(def-exported-c-function "newtag"
    (Tag Tag Offset))

;;; extern void nmode( long );
(def-exported-c-function "nmode"
    (long))

;;; extern void noborder( void );
(def-exported-c-function "noborder")

;;; extern void noise( Device, short );
(def-exported-c-function "noise"
    (Device short))

;;; extern void noport( void );
(def-exported-c-function "noport")

;;; extern void normal( Coord[3] ); /* obsolete */
(def-exported-c-function "normal"
    ((:array Coord (3))))

;;; extern void nurbscurve( long, double[], long, double[], long, long);
(def-exported-c-function "nurbscurve"
    (long (:array double) long (:array double) long long))

;;; extern void nurbssurface( long, double[], long, double[], long, long, double[], long, long, long );
(def-exported-c-function "nurbssurface"
    (long (:array double) long (:array double) long long (:array double) long long long))

;;; extern void objdelete( Tag, Tag );
(def-exported-c-function "objdelete"
    (Tag Tag))

;;; extern void objinsert( Tag );
(def-exported-c-function "objinsert"
    (Tag))

;;; extern void objreplace( Tag );
(def-exported-c-function "objreplace"
    (Tag))

;;; extern void onemap( void );
(def-exported-c-function "onemap")

;;; extern void ortho( Coord, Coord, Coord, Coord, Coord, Coord );
(def-exported-c-function "ortho"
    (Coord Coord Coord Coord Coord Coord))

;;; extern void ortho2( Coord, Coord, Coord, Coord );
(def-exported-c-function "ortho2"
    (Coord Coord Coord Coord))

;;; extern void overlay( long );
(def-exported-c-function "overlay"
    (long))

;;; extern void pagecolor( Colorindex ); /* not recommended */
(def-exported-c-function "pagecolor"
    (Colorindex))

;;; extern void passthrough( short );
(def-exported-c-function "passthrough"
    (short))

;;; extern void patch( Matrix, Matrix, Matrix );
(def-exported-c-function "patch"
    (Matrix Matrix Matrix))

;;; extern void patchbasis( long, long );
(def-exported-c-function "patchbasis"
    (long long))

;;; extern void patchcurves( long, long );
(def-exported-c-function "patchcurves"
    (long long))

;;; extern void patchprecision( long, long );
(def-exported-c-function "patchprecision"
    (long long))

;;; extern void pclos( void );
(def-exported-c-function "pclos")

;;; extern void pdr( Coord, Coord, Coord );
(def-exported-c-function "pdr"
    (Coord Coord Coord))

;;; extern void pdr2( Coord, Coord );
(def-exported-c-function "pdr2"
    (Coord Coord))

;;; extern void pdr2i( Icoord, Icoord );
(def-exported-c-function "pdr2i"
    (Icoord Icoord))

;;; extern void pdr2s( Scoord, Scoord );
(def-exported-c-function "pdr2s"
    (Scoord Scoord))

;;; extern void pdri( Icoord, Icoord, Icoord );
(def-exported-c-function "pdri"
    (Icoord Icoord Icoord))

;;; extern void pdrs( Scoord, Scoord, Scoord );
(def-exported-c-function "pdrs"
    (Scoord Scoord Scoord))

;;; extern void perspective( Angle, float, Coord, Coord );
(def-exported-c-function "perspective"
    (Angle float Coord Coord))

;;; extern void pick( short[], long );
(def-exported-c-function "pick"
    ((:array short) long))

;;; extern void picksize( short, short );
(def-exported-c-function "picksize"
    (short short))

;;; extern void pixmode( long, long );
(def-exported-c-function "pixmode"
    (long long))

;;; extern void pixmodef( long, float );
(def-exported-c-function "pixmodef"
    (long float))

;;; extern void pmv( Coord, Coord, Coord );
(def-exported-c-function "pmv"
    (Coord Coord Coord))

;;; extern void pmv2( Coord, Coord );
(def-exported-c-function "pmv2"
    (Coord Coord))

;;; extern void pmv2i( Icoord, Icoord );
(def-exported-c-function "pmv2i"
    (Icoord Icoord))

;;; extern void pmv2s( Scoord, Scoord );
(def-exported-c-function "pmv2s"
    (Scoord Scoord))

;;; extern void pmvi( Icoord, Icoord, Icoord );
(def-exported-c-function "pmvi"
    (Icoord Icoord Icoord))

;;; extern void pmvs( Scoord, Scoord, Scoord );
(def-exported-c-function "pmvs"
    (Scoord Scoord Scoord))

;;; extern void pnt( Coord, Coord, Coord );
(def-exported-c-function "pnt"
    (Coord Coord Coord))

;;; extern void pnt2( Coord, Coord );
(def-exported-c-function "pnt2"
    (Coord Coord))

;;; extern void pnt2i( Icoord, Icoord );
(def-exported-c-function "pnt2i"
    (Icoord Icoord))

;;; extern void pnt2s( Scoord, Scoord );
(def-exported-c-function "pnt2s"
    (Scoord Scoord))

;;; extern void pnti( Icoord, Icoord, Icoord );
(def-exported-c-function "pnti"
    (Icoord Icoord Icoord))

;;; extern void pnts( Scoord, Scoord, Scoord );
(def-exported-c-function "pnts"
    (Scoord Scoord Scoord))

;;; extern void pntsize( short );
(def-exported-c-function "pntsize"
    (short))

;;; extern void pntsizef( float );
(def-exported-c-function "pntsizef"
    (float))

;;; extern void pntsmooth( unsigned long );
(def-exported-c-function "pntsmooth"
    (u-long))

;;; extern void polarview( Coord, Angle, Angle, Angle );
(def-exported-c-function "polarview"
    (Coord Angle Angle Angle))

;;; extern void polf( long, Coord[][3] );
(def-exported-c-function "polf"
    (long (:array Coord)))

;;; extern void polf2( long, Coord[][2] );
(def-exported-c-function "polf2"
    (long (:array Coord)))
  
;;; extern void polf2i( long, Icoord[][2] );
(def-exported-c-function "polf2i"
    (long (:array Icoord)))

;;; extern void polf2s( long, Scoord[][2] );
(def-exported-c-function "polf2s"
    (long (:array Scoord)))

;;; extern void polfi( long, Icoord[][3] );
(def-exported-c-function "polfi"
    (long (:array Icoord)))

;;; extern void polfs( long, Scoord[][3] );
(def-exported-c-function "polfs"
    (long (:array Scoord)))

;;; extern void poly( long, Coord[][3] );
(def-exported-c-function "poly"
    (long (:array Coord)))

;;; extern void poly2( long, Coord[][2] );
(def-exported-c-function "poly2"
    (long (:array Coord)))

;;; extern void poly2i( long, Icoord[][2] );
(def-exported-c-function "poly2i"
    (long (:array Icoord)))

;;; extern void poly2s( long, Scoord[][2] );
(def-exported-c-function "poly2s"
    (long (:array Scoord)))

;;; extern void polyi( long, Icoord[][3] );
(def-exported-c-function "polyi"
    (long (:array Icoord)))

;;; extern void polymode( long );
(def-exported-c-function "polymode"
    (long))

;;; extern void polys( long, Scoord[][3] );
(def-exported-c-function "polys"
    (long (:array Scoord)))

;;; extern void polysmooth( long );
(def-exported-c-function "polysmooth"
    (long))

;;; extern void popattributes( void );
(def-exported-c-function "popattributes")

;;; extern void popmatrix( void );
(def-exported-c-function "popmatrix")

;;; extern void popname( void );
(def-exported-c-function "popname")

;;; extern void popviewport( void );
(def-exported-c-function "popviewport")

;;; extern void prefposition( long, long, long, long );
(def-exported-c-function "prefposition"
    (long long long long))

;;; extern void prefsize( long, long );
(def-exported-c-function "prefsize"
    (long long))

;;; extern void pupmode( void ); /* obsolete */
(def-exported-c-function "pupmode")

;;; extern void pushattributes( void );
(def-exported-c-function "pushattributes")

;;; extern void pushmatrix( void );
(def-exported-c-function "pushmatrix")

;;; extern void pushname( short );
(def-exported-c-function "pushname"
    (short))

;;; extern void pushviewport( void );
(def-exported-c-function "pushviewport")

;;; extern void pwlcurve( long, double[], long, long );
(def-exported-c-function "pwlcurve"
    (long (:array double) long long))

;;; extern long qcontrol( long, long, short[], long, short[] );
(def-exported-c-function ("qcontrol" (:return-type long))
    (long long (:array short) long (:array short)))

;;; extern void qdevice( Device );
(def-exported-c-function "qdevice"
    (Device))

;;; extern void qenter( Device, short );
(def-exported-c-function "qenter"
    (Device short))

;;; extern long qgetfd( void );
(def-exported-c-function ("qgetfd" (:return-type long)))

;;; extern long qread( short * );
(def-exported-c-function ("qread" (:return-type long))
    ((:pointer short)))

;;; extern void qreset( void );
(def-exported-c-function "qreset")

;;; extern long qtest( void );
(def-exported-c-function "qtest")

;;; extern void rcrv( Coord[4][4] );
(def-exported-c-function "rcrv"
    ((:array Coord (4 4))))

;;; extern void rcrvn( long, Coord[][4] );
(def-exported-c-function "rcrvn"
    (long (:array Coord)))

;;; extern void rdr( Coord, Coord, Coord );
(def-exported-c-function "rdr"
    (Coord Coord Coord))

;;; extern void rdr2( Coord, Coord );
(def-exported-c-function "rdr2"
    (Coord Coord))

;;; extern void rdr2i( Icoord, Icoord );
(def-exported-c-function "rdr2i"
    (Icoord Icoord))

;;; extern void rdr2s( Scoord, Scoord );
(def-exported-c-function "rdr2s"
    (Scoord Scoord))

;;; extern void rdri( Icoord, Icoord, Icoord );
(def-exported-c-function "rdri"
    (Icoord Icoord ICoord))

;;; extern void rdrs( Scoord, Scoord, Scoord );
(def-exported-c-function "rdrs"
    (Scoord Scoord Scoord))

;;; extern long readpixels( short, Colorindex[] );
(def-exported-c-function ("readpixels" (:return-type long))
    (short (:array Colorindex)))

;;; extern long readRGB( short, RGBvalue[], RGBvalue[], RGBvalue[] );
(def-exported-c-function ("readRGB" (:return-type long))
    (short (:array RGBvalue) (:array RGBvalue) (:array RGBvalue)))

;;; extern void readsource( long );
(def-exported-c-function "readsource"
    (long))

;;; extern void rect( Coord, Coord, Coord, Coord );
(def-exported-c-function "rect"
    (Coord Coord Coord Coord))

;;; extern void rectcopy( Screencoord, Screencoord, Screencoord, Screencoord, Screencoord, Screencoord );
(def-exported-c-function "rectcopy"
    (Screencoord Screencoord Screencoord Screencoord Screencoord Screencoord))

;;; extern void rectf( Coord, Coord, Coord, Coord );
(def-exported-c-function "rectf"
    (Coord Coord Coord Coord))

;;; extern void rectfi( Icoord, Icoord, Icoord, Icoord );
(def-exported-c-function "rectfi"
    (Icoord Icoord Icoord Icoord))

;;; extern void rectfs( Scoord, Scoord, Scoord, Scoord );
(def-exported-c-function "rectfs"
    (Scoord Scoord Scoord Scoord))

;;; extern void recti( Icoord, Icoord, Icoord, Icoord );
(def-exported-c-function "recti"
    (Icoord Icoord Icoord Icoord))

;;; extern long rectread( Screencoord, Screencoord, Screencoord, Screencoord, Colorindex[] );
(def-exported-c-function ("rectread" (:return-type long))
    (Screencoord Screencoord Screencoord Screencoord (:array Colorindex)))

;;; extern void rects( Scoord, Scoord, Scoord, Scoord );
(def-exported-c-function "rects"
    (Scoord Scoord Scoord Scoord))

;;; extern void rectwrite( Screencoord, Screencoord, Screencoord, Screencoord, Colorindex[] );
(def-exported-c-function "rectwrite"
    (Screencoord Screencoord Screencoord Screencoord (:array Colorindex)))

;;; extern void rectzoom( float, float );
(def-exported-c-function "rectzoom"
    (float float))

;;; extern void resetls( Boolean ); /* not recommended */
(def-exported-c-function "resetls"
    (Boolean))

;;; extern void reshapeviewport( void );
(def-exported-c-function "reshapeviewport")

;;; extern void RGBcolor( short, short, short );
(def-exported-c-function "RGBcolor"
    (short short short))

;;; extern void RGBcursor( short, short, short, short, short, short, short ); /* obsolete */
(def-exported-c-function "RGBcursor"
    (short short short short short short short))

;;; extern void RGBmode( void );
(def-exported-c-function "RGBmode")

;;; extern void RGBrange( short, short, short, short, short, short, Screencoord, Screencoord ); /* obsolete */
(def-exported-c-function "RGBrange"
    (short short short short short short Screencoord Screencoord))

;;; extern void RGBwritemask( short, short, short );
(def-exported-c-function "RGBwritemask"
    (short short short))

;;; extern void ringbell( void );
(def-exported-c-function "ringbell")

;;; extern void rmv( Coord, Coord, Coord );
(def-exported-c-function "rmv"
    (Coord Coord Coord))

;;; extern void rmv2( Coord, Coord );
(def-exported-c-function "rmv2"
    (Coord Coord))

;;; extern void rmv2i( Icoord, Icoord );
(def-exported-c-function "rmv2i"
    (Icoord Icoord))

;;; extern void rmv2s( Scoord, Scoord );
(def-exported-c-function "rmv2s"
    (Scoord Scoord))

;;; extern void rmvi( Icoord, Icoord, Icoord );
(def-exported-c-function "rmvi"
    (Icoord Icoord Icoord))

;;; extern void rmvs( Scoord, Scoord, Scoord );
(def-exported-c-function "rmvs"
    (Scoord Scoord Scoord))

;;; extern void rot( float, char );
(def-exported-c-function "rot"
    (float char))

;;; extern void rotate( Angle, char );
(def-exported-c-function "rotate"
    (Angle char))

;;; extern void rpatch( Matrix, Matrix, Matrix, Matrix );
(def-exported-c-function "rpatch"
    (Matrix Matrix Matrix Matrix))

;;; extern void rpdr( Coord, Coord, Coord );
(def-exported-c-function "rpdr"
    (Coord Coord Coord))

;;; extern void rpdr2( Coord, Coord );
(def-exported-c-function "rpdr2"
    (Coord Coord))

;;; extern void rpdr2i( Icoord, Icoord );
(def-exported-c-function "rpdr2i"
    (Icoord Icoord))

;;; extern void rpdr2s( Scoord, Scoord );
(def-exported-c-function "rpdr2s"
    (Scoord Scoord))

;;; extern void rpdri( Icoord, Icoord, Icoord );
(def-exported-c-function "rpdri"
    (Icoord Icoord Icoord))

;;; extern void rpdrs( Scoord, Scoord, Scoord );
(def-exported-c-function "rpdrs"
    (Scoord Scoord Scoord))

;;; extern void rpmv( Coord, Coord, Coord );
(def-exported-c-function "rpmv"
    (Coord Coord Coord))

;;; extern void rpmv2( Coord, Coord );
(def-exported-c-function "rpmv2"
    (Coord Coord))

;;; extern void rpmv2i( Icoord, Icoord );
(def-exported-c-function "rpmv2i"
    (Icoord Icoord))

;;; extern void rpmv2s( Scoord, Scoord );
(def-exported-c-function "rpmv2s"
    (Scoord Scoord))

;;; extern void rpmvi( Icoord, Icoord, Icoord );
(def-exported-c-function "rpmvi"
    (Icoord Icoord Icoord))

;;; extern void rpmvs( Scoord, Scoord, Scoord );
(def-exported-c-function "rpmvs"
    (Scoord Scoord Scoord))

;;; extern void sbox( Coord, Coord, Coord, Coord );
(def-exported-c-function "sbox"
    (Coord Coord Coord Coord))

;;; extern void sboxf( Coord, Coord, Coord, Coord );
(def-exported-c-function "sboxf"
    (Coord Coord Coord Coord))

;;; extern void     sboxfi( Icoord, Icoord, Icoord, Icoord );
(def-exported-c-function "sboxfi"
    (Icoord Icoord Icoord Icoord))

;;; extern void     sboxfs( Scoord, Scoord, Scoord, Scoord );
(def-exported-c-function "sboxfs"
    (Scoord Scoord Scoord Scoord))

;;; extern void     sboxi( Icoord, Icoord, Icoord, Icoord );
(def-exported-c-function "sboxi"
    (Icoord Icoord Icoord Icoord))

;;; extern void     sboxs( Scoord, Scoord, Scoord, Scoord );
(def-exported-c-function "sboxs"
    (Scoord Scoord Scoord Scoord))

;;; extern void     scale( float, float, float );
(def-exported-c-function "scale"
    (float float float))

;;; extern void     sclear( unsigned long );
(def-exported-c-function "sclear"
    (u-long))

;;; extern void     scrbox( long );
(def-exported-c-function "scrbox"
    (long))

;;; extern void     screenspace( void );
(def-exported-c-function "screenspace")

;;; extern void     scrmask( Screencoord, Screencoord, Screencoord, Screencoord );
(def-exported-c-function "scrmask"
    (Screencoord Screencoord Screencoord Screencoord))
		 
;;; extern long     scrnattach( long );
(def-exported-c-function ("scrnattach" (:return-type long))
    (long))

;;; extern long     scrnselect( long );
(def-exported-c-function ("scrnselect" (:return-type long))
    (long))

;;; extern void     scrsubdivide( long, float[] );
(def-exported-c-function "scrsubdivide"
    (long (:array float)))

;;; extern void     setbell( Byte );
(def-exported-c-function "setbell"
    (Byte))

;;; extern void     setcursor( short, Colorindex, Colorindex );
(def-exported-c-function "setcursor"
    (short Colorindex Colorindex))

;;; extern void     setdblights( unsigned long );
(def-exported-c-function "setdblights"
    (u-long))

;;; extern void     setdepth( Screencoord, Screencoord );   /* obsolete */
(def-exported-c-function "setdepth"
    (Screencoord Screencoord))

;;; extern void     setlinestyle( short );
(def-exported-c-function "setlinestyle"
    (short))

;;; extern void     setmap( short );
(def-exported-c-function "setmap"
    (short))

;;; extern void     setmonitor( short );
(def-exported-c-function "setmonitor"
    (short))

;;; extern void     setnurbsproperty( long, float );
(def-exported-c-function "setnurbsproperty"
    (long float))

;;; extern void     setpattern( short );
(def-exported-c-function "setpattern"
    (short))

;;; extern void     setpup( long, long, unsigned long );
(def-exported-c-function "setpup"
    (long long u-long))

;;; extern void     setshade( Colorindex );
(def-exported-c-function "setshade"
    (ColorIndex))

;;; extern void     setvaluator( Device, short, short, short );
(def-exported-c-function "setvaluator"
    (Device short short short))

;;; extern void     setvideo( long, long );
(def-exported-c-function "setvideo"
    (long long))

;;; extern void     shademodel( long );
(def-exported-c-function "shademodel"
    (long))

;;; extern void     shaderange( Colorindex, Colorindex, Screencoord, Screencoord ); /* obsolete */
(def-exported-c-function "shaderange"
    (Colorindex Colorindex Screencoord Screencoord))

;;; extern void     singlebuffer( void );
(def-exported-c-function "singlebuffer")

;;; extern void     smoothline( long );                     /* obsolete */
(def-exported-c-function "smoothline"
    (long))

;;; extern void     spclos( void );                         /* obsolete */
(def-exported-c-function "spclos")

;;; extern void     splf( long, Coord[][3], Colorindex[] );
(def-exported-c-function "splf"
    (long (:array Coord) (:array Colorindex)))

;;; extern void     splf2( long, Coord[][2], Colorindex[] );
(def-exported-c-function "splf2"
    (long (:array Coord) (:array Colorindex)))

;;; extern void     splf2i( long, Icoord[][2], Colorindex[] );
(def-exported-c-function "splf2i"
    (long (:array Icoord) (:array Colorindex)))

;;; extern void     splf2s( long, Scoord[][2], Colorindex[] );
(def-exported-c-function "splf2s"
    (long (:array Scoord) (:array Colorindex)))
  
;;; extern void     splfi( long, Icoord[][3], Colorindex[] );
(def-exported-c-function "splfi"
    (long (:array Icoord) (:array Colorindex)))

;;; extern void     splfs( long, Scoord[][3], Colorindex[] );
(def-exported-c-function "splfs"
    (long (:array Scoord) (:array Colorindex)))

;;; extern void     stencil( long, unsigned long, long, unsigned long, long, long, long );
(def-exported-c-function "stencil"
    (long u-long long u-long long long long))

;;; extern void     stensize( long );
(def-exported-c-function "stensize"
    (long))

;;; extern void     stepunit( long, long );
(def-exported-c-function "stepunit"
    (long long))

;;; extern long     strwidth( String );
(def-exported-c-function "strwidth"
    (String))

;;; extern void     subpixel( Boolean );
(def-exported-c-function "subpixel"
    (Boolean))

;;; extern void     swapbuffers( void );
(def-exported-c-function "swapbuffers")

;;; extern void     swapinterval( short );
(def-exported-c-function "swapinterval"
    (short))

;;; extern void     swaptmesh( void );
(def-exported-c-function "swaptmesh")

;;; extern long     swinopen( long );
(def-exported-c-function ("swinopen" (:return-type long))
    (long))

;;; extern void     swritemask( unsigned long );
(def-exported-c-function "swritemask"
    (u-long))

;;; extern void     t2d( double[2] );
(def-exported-c-function "t2d"
    ((:array double (2))))

;;; extern void     t2f( float[2] );
(def-exported-c-function "t2f"
    ((:array float (2))))

;;; extern void     t2i( long[2] );
(def-exported-c-function "t2i"
    ((:array long (2))))

;;; extern void     t2s( short[2] );
(def-exported-c-function "t2s"
    ((:array short (2))))

;;; extern void     tevbind( long, long );
(def-exported-c-function "tevbind"
    (long long))

;;; extern void     tevdef( long, long, float[] );
(def-exported-c-function "tevdef"
    (long long (:array float)))

;;; extern void     texbind( long, long );
(def-exported-c-function "texbind"
    (long long))

;;; extern void     texdef2d( long, long, long, long, unsigned long[], long, float[] );
(def-exported-c-function "texdef2d"
    (long long long long (:array u-long) long (:array float)))

;;; extern void     texgen( long, long, float[] );
(def-exported-c-function "texgen"
    (long long (:array float)))
  
;;; extern void     textcolor( Colorindex );                /* not recommended */
(def-exported-c-function "textcolor"
    (Colorindex))

;;; extern void     textinit( void );                       /* not recommended */
(def-exported-c-function "textinit")

;;; extern void     textport( Screencoord, Screencoord, Screencoord, Screencoord ); /* not recommended */
(def-exported-c-function "textport"
    (Screencoord Screencoord Screencoord Screencoord))

;;; extern void     tie( Device, Device, Device );
(def-exported-c-function "tie"
    (Device Device Device))

;;; extern void     tpoff( void );                          /* not recommended */
(def-exported-c-function "tpoff")

;;; extern void     tpon( void );                           /* not recommended */
(def-exported-c-function "tpon")

;;; extern void     translate( Coord, Coord, Coord );
(def-exported-c-function "translate"
    (Coord Coord Coord))

;;; extern void     underlay( long );
(def-exported-c-function "underlay"
    (long))

;;; extern void     unqdevice( Device );
(def-exported-c-function "unqdevice"
    (Device))

;;; extern void     v2d( double[2] );
(def-exported-c-function "v2d"
    ((:array double (2))))

;;; extern void     v2f( float[2] );
(def-exported-c-function "v2f"
    ((:array float (2))))
  
;;; extern void     v2i( long[2] );
(def-exported-c-function "v2i"
    ((:array long (2))))

;;; extern void     v2s( short[2] );
(def-exported-c-function "v2s"
    ((:array short (2))))

;;; extern void     v3d( double[3] );
(def-exported-c-function "v3d"
    ((:array double (3))))

;;; extern void     v3f( float[3] );
(def-exported-c-function "v3f"
    ((:array float (3))))

;;; extern void     v3i( long[3] );
(def-exported-c-function "v3i"
    ((:array long (3))))

;;; extern void     v3s( short[3] );
(def-exported-c-function "v3s"
    ((:array short (3))))

;;; extern void     v4d( double[4] );
(def-exported-c-function "v4d"
    ((:array double (4))))

;;; extern void     v4f( float[4] );
(def-exported-c-function "v4f"
    ((:array float (4))))

;;; extern void     v4i( long[4] );
(def-exported-c-function "v4i"
    ((:array long (4))))

;;; extern void     v4s( short[4] );
(def-exported-c-function "v4s"
    ((:array short (4))))

;;; extern void     videocmd( long );
(def-exported-c-function "videocmd"
    (long))

;;; extern void     viewport( Screencoord, Screencoord, Screencoord, Screencoord );
(def-exported-c-function "viewport"
    (Screencoord Screencoord Screencoord Screencoord))

;;; extern long     winattach( void );                      /* obsolete */
(def-exported-c-function ("winattach" (:return-type long)))

;;; extern void     winclose( long );
(def-exported-c-function "winclose"
    (long))

;;; extern void     winconstraints( void );
(def-exported-c-function "winconstraints")

;;; extern long     windepth( long );
(def-exported-c-function ("windepth" (:return-type long))
    (long))

;;; extern void     window( Coord, Coord, Coord, Coord, Coord, Coord );
(def-exported-c-function "window"
    (Coord Coord Coord Coord Coord Coord))

;;; extern long     winget( void );
(def-exported-c-function ("winget" (:return-type long)))

;;; extern void     winmove( long, long );
(def-exported-c-function "winmove"
    (long long))

;;; extern long     winopen( String );
(def-exported-c-function ("winopen" (:return-type long))
    (String))

;;; extern void     winpop( void );
(def-exported-c-function "winpop")

;;; extern void     winposition( long, long, long, long );
(def-exported-c-function "winposition"
    (long long long long))

;;; extern void     winpush( void );
(def-exported-c-function "winpush")

;;; extern void     winset( long );
(def-exported-c-function "winset"
    (long))

;;; extern void     wintitle( String );
(def-exported-c-function "wintitle"
    (String))

;;; extern void     wmpack( unsigned long );
(def-exported-c-function "wmpack"
    (u-long))

;;; extern void     writemask( Colorindex );
(def-exported-c-function "writemask"
    (Colorindex))

;;; extern void     writepixels( short, Colorindex[] );
(def-exported-c-function "writepixels"
    (short (:array Colorindex)))

;;; extern void     writeRGB( short, RGBvalue[], RGBvalue[], RGBvalue[] );
(def-exported-c-function "writeRGB"
    (short (:array RGBvalue) (:array RGBvalue) (:array RGBvalue)))

;;; extern void     xfpt( Coord, Coord, Coord );            /* not recommended */
(def-exported-c-function "xfpt"
    (Coord Coord Coord))

;;; extern void     xfpt2( Coord, Coord );                  /* not recommended */
(def-exported-c-function "xfpt2"
    (Coord Coord))

;;; extern void     xfpt2i( Icoord, Icoord );               /* not recommended */
(def-exported-c-function "xfpt2i"
    (Icoord Icoord))

;;; extern void     xfpt2s( Scoord, Scoord );               /* not recommended */
(def-exported-c-function "xfpt2s"
    (Scoord Scoord))

;;; extern void     xfpt4( Coord, Coord, Coord, Coord );    /* not recommended */
(def-exported-c-function "xfpt4"
    (Coord Coord Coord Coord))

;;; extern void     xfpt4i( Icoord, Icoord, Icoord, Icoord ); /* not recommended */
(def-exported-c-function "xfpt4i"
    (Icoord Icoord Icoord Icoord))

;;; extern void     xfpt4s( Scoord, Scoord, Scoord, Scoord ); /* not recommended */
(def-exported-c-function "xfpt4s"
    (Scoord Scoord Scoord Scoord))

;;; extern void     xfpti( Icoord, Icoord, Icoord );        /* not recommended */
(def-exported-c-function "xfpti"
    (Icoord Icoord Icoord))

;;; extern void     xfpts( Scoord, Scoord, Scoord );        /* not recommended */
(def-exported-c-function "xfpts"
    (Scoord Scoord Scoord))

;;; extern void     zbuffer( Boolean );
(def-exported-c-function "zbuffer"
    (Boolean))

;;; extern void     zclear( void );
(def-exported-c-function "zclear")

;;; extern void     zdraw( Boolean );
(def-exported-c-function "zdraw"
    (Boolean))

;;; extern void     zfunction( long );
(def-exported-c-function "zfunction"
    (long))

;;; extern void     zsource( long );                        /* not recommended */
(def-exported-c-function "zsource"
    (long))

;;; extern void     zwritemask( unsigned long );
(def-exported-c-function "zwritemask"
    (u-long))

