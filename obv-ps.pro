%!
%% PS-Adobe-1.0
%% OBVIUS PostScript prolog
%% D. Heeger/T. Darrell 4/89

%% Greyscale image display
%% n m b doimage - 
%% followed by a string of hex image data
/doimage {
   /b exch def /m exch def /n exch def
	/pix n string def
   n m b [n 0 0 m  0 m]
   { currentfile pix readhexstring pop }
   image
} def
/spotsize { %% adjust the resolution of the dither
   /perinch exch def
   currentscreen 3 -1 roll
   pop perinch
   3 1 roll setscreen
} def
/invert {
   /normtrans currenttransfer def
   /curtran currenttransfer cvlit def
   /newtran curtran length 3 add array def
   newtran 0 {1 exch sub} putinterval
   newtran 3 curtran putinterval
   newtran cvx settransfer
   /invtrans currenttransfer  def
} def

%% to-x to-y from-x from-y drawline -
/drawline { newpath moveto lineto stroke } bind def

%% for displaying captions and comments...
%% *** This needs a rewrite to allow user control.  Generalized screens?
/bigfont /Times-Italic findfont 18 scalefont def
/medfont /Courier findfont 12 scalefont def
/littlefont /Helvetica findfont 14 scalefont def
/nameshow { 1 -1 scale bigfont setfont show  1 -1 scale} def
/commentshow { 1 -1 scale littlefont setfont show  1 -1 scale} def
/drawtext { moveto 1 -1 scale medfont setfont show  1 -1 scale} def

%% initialize default image cooridate system
0 720 translate 1 -1 scale

%% default dither resolution; optimized for 300 dpi
%% 60 spotsize

%% line drawing parameters
.5 setlinewidth   % half a pixel
1 setlinecap      % rounded ends

%%EndProlog
