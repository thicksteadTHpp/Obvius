/*
 * gl-lucid-macros.c
 *
 * This file contains all C-macros to perform casting of argument
 * and return types.  This is because Lucid doesn't support all
 * C primitive types.
 */

#include <sys/types.h>
/*#include <gl.h>*/
/*#include <fmclient.h>*/

/* original prototype: void acbuf(long arg0, float arg1) */
void acbuf_macro(long arg0, double arg1)
{
     acbuf( arg0, (float) arg1);
}

/* original prototype: void arc(Coord arg0, Coord arg1, Coord arg2, Angle arg3, Angle arg4) */
void arc_macro(double arg0, double arg1, double arg2, int arg3, int arg4)
{
     arc((Coord) arg0, (Coord) arg1, (Coord) arg2, (Angle) arg3, (Angle) arg4);
}

/* original prototype: void arcf(Coord arg0, Coord arg1, Coord arg2, Angle arg3, Angle arg4) */
void arcf_macro(double arg0, double arg1, double arg2, int arg3, int arg4)
{
     arcf((Coord) arg0, (Coord) arg1, (Coord) arg2, (Angle) arg3, (Angle) arg4);
}

/* original prototype: void arcfi(Icoord arg0, Icoord arg1, Icoord arg2, Angle arg3, Angle arg4) */
void arcfi_macro(Icoord arg0, Icoord arg1, Icoord arg2, int arg3, int arg4)
{
     arcfi( arg0,  arg1,  arg2, (Angle) arg3, (Angle) arg4);
}

/* original prototype: void arcfs(Scoord arg0, Scoord arg1, Scoord arg2, Angle arg3, Angle arg4) */
void arcfs_macro(int arg0, int arg1, int arg2, int arg3, int arg4)
{
     arcfs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2, (Angle) arg3, (Angle) arg4);
}

/* original prototype: void arci(Icoord arg0, Icoord arg1, Icoord arg2, Angle arg3, Angle arg4) */
void arci_macro(Icoord arg0, Icoord arg1, Icoord arg2, int arg3, int arg4)
{
     arci( arg0,  arg1,  arg2, (Angle) arg3, (Angle) arg4);
}

/* original prototype: void arcs(Scoord arg0, Scoord arg1, Scoord arg2, Angle arg3, Angle arg4) */
void arcs_macro(int arg0, int arg1, int arg2, int arg3, int arg4)
{
     arcs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2, (Angle) arg3, (Angle) arg4);
}

/* original prototype: void attachcursor(Device arg0, Device arg1) */
void attachcursor_macro(int arg0, int arg1)
{
     attachcursor((Device) arg0, (Device) arg1);
}

/* original prototype: void bbox2(Screencoord arg0, Screencoord arg1, Coord arg2, Coord arg3, Coord arg4, Coord arg5) */
void bbox2_macro(int arg0, int arg1, double arg2, double arg3, double arg4, double arg5)
{
     bbox2((Screencoord) arg0, (Screencoord) arg1, (Coord) arg2, (Coord) arg3, (Coord) arg4, (Coord) arg5);
}

/* original prototype: void bbox2i(Screencoord arg0, Screencoord arg1, Icoord arg2, Icoord arg3, Icoord arg4, Icoord arg5) */
void bbox2i_macro(int arg0, int arg1, Icoord arg2, Icoord arg3, Icoord arg4, Icoord arg5)
{
     bbox2i((Screencoord) arg0, (Screencoord) arg1,  arg2,  arg3,  arg4,  arg5);
}

/* original prototype: void bbox2s(Screencoord arg0, Screencoord arg1, Scoord arg2, Scoord arg3, Scoord arg4, Scoord arg5) */
void bbox2s_macro(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5)
{
     bbox2s((Screencoord) arg0, (Screencoord) arg1, (Scoord) arg2, (Scoord) arg3, (Scoord) arg4, (Scoord) arg5);
}

/* original prototype: void blink(short arg0, Colorindex arg1, short arg2, short arg3, short arg4) */
void blink_macro(int arg0, int arg1, int arg2, int arg3, int arg4)
{
     blink((short) arg0, (Colorindex) arg1, (short) arg2, (short) arg3, (short) arg4);
}

/* original prototype: void blkqread(short arg0[], short arg1) */
void blkqread_macro(short arg0[], int arg1)
{
     blkqread( arg0, (short) arg1);
}

/* original prototype: void circ(Coord arg0, Coord arg1, Coord arg2) */
void circ_macro(double arg0, double arg1, double arg2)
{
     circ((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void circf(Coord arg0, Coord arg1, Coord arg2) */
void circf_macro(double arg0, double arg1, double arg2)
{
     circf((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void circfs(Scoord arg0, Scoord arg1, Scoord arg2) */
void circfs_macro(int arg0, int arg1, int arg2)
{
     circfs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: void circs(Scoord arg0, Scoord arg1, Scoord arg2) */
void circs_macro(int arg0, int arg1, int arg2)
{
     circs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: void cmov(Coord arg0, Coord arg1, Coord arg2) */
void cmov_macro(double arg0, double arg1, double arg2)
{
     cmov((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void cmov2(Coord arg0, Coord arg1) */
void cmov2_macro(double arg0, double arg1)
{
     cmov2((Coord) arg0, (Coord) arg1);
}

/* original prototype: void cmov2s(Scoord arg0, Scoord arg1) */
void cmov2s_macro(int arg0, int arg1)
{
     cmov2s((Scoord) arg0, (Scoord) arg1);
}

/* original prototype: void cmovs(Scoord arg0, Scoord arg1, Scoord arg2) */
void cmovs_macro(int arg0, int arg1, int arg2)
{
     cmovs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: void color(Colorindex arg0) */
void color_macro(int arg0)
{
     color((Colorindex) arg0);
}

/* original prototype: void colorf(float arg0) */
void colorf_macro(double arg0)
{
     colorf((float) arg0);
}

/* original prototype: void curorigin(short arg0, short arg1, short arg2) */
void curorigin_macro(int arg0, int arg1, int arg2)
{
     curorigin((short) arg0, (short) arg1, (short) arg2);
}

/* original prototype: void curvebasis(short arg0) */
void curvebasis_macro(int arg0)
{
     curvebasis((short) arg0);
}

/* original prototype: void curveit(short arg0) */
void curveit_macro(int arg0)
{
     curveit((short) arg0);
}

/* original prototype: void curveprecision(short arg0) */
void curveprecision_macro(int arg0)
{
     curveprecision((short) arg0);
}

/* original prototype: void cyclemap(short arg0, short arg1, short arg2) */
void cyclemap_macro(int arg0, int arg1, int arg2)
{
     cyclemap((short) arg0, (short) arg1, (short) arg2);
}

/* original prototype: void defbasis(short arg0, Matrix arg1) */
void defbasis_macro(int arg0, Matrix arg1)
{
     defbasis((short) arg0,  arg1);
}

/* original prototype: void defcursor(short arg0, u_short arg1[128]) */
void defcursor_macro(int arg0, u_short arg1[128])
{
     defcursor((short) arg0,  arg1);
}

/* original prototype: void deflinestyle(short arg0, Linestyle arg1) */
void deflinestyle_macro(int arg0, int arg1)
{
     deflinestyle((short) arg0, (Linestyle) arg1);
}

/* original prototype: void defpattern(short arg0, short arg1, u_short arg2[]) */
void defpattern_macro(int arg0, int arg1, u_short arg2[])
{
     defpattern((short) arg0, (short) arg1,  arg2);
}

/* original prototype: void defrasterfont(short arg0, short arg1, short arg2, Fontchar arg3[], short arg4, u_short arg5[]) */
void defrasterfont_macro(int arg0, int arg1, int arg2, Fontchar arg3[], int arg4, u_short arg5[])
{
     defrasterfont((short) arg0, (short) arg1, (short) arg2,  arg3, (short) arg4,  arg5);
}

/* original prototype: void draw(Coord arg0, Coord arg1, Coord arg2) */
void draw_macro(double arg0, double arg1, double arg2)
{
     draw((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void draw2(Coord arg0, Coord arg1) */
void draw2_macro(double arg0, double arg1)
{
     draw2((Coord) arg0, (Coord) arg1);
}

/* original prototype: void draw2s(Scoord arg0, Scoord arg1) */
void draw2s_macro(int arg0, int arg1)
{
     draw2s((Scoord) arg0, (Scoord) arg1);
}

/* original prototype: void draws(Scoord arg0, Scoord arg1, Scoord arg2) */
void draws_macro(int arg0, int arg1, int arg2)
{
     draws((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: void font(short arg0) */
void font_macro(int arg0)
{
     font((short) arg0);
}

/* original prototype: Boolean getbutton(Device arg0) */
Boolean getbutton_macro(int arg0)
{
     return( getbutton((Device) arg0) );
}

/* original prototype: void getmcolor(Colorindex arg0, short *arg1, short *arg2, short *arg3) */
void getmcolor_macro(int arg0, short *arg1, short *arg2, short *arg3)
{
     getmcolor((Colorindex) arg0,  arg1,  arg2,  arg3);
}

/* original prototype: long getvaluator(Device arg0) */
long getvaluator_macro(int arg0)
{
     return( getvaluator((Device) arg0) );
}

/* original prototype: Boolean isqueued(Device arg0) */
Boolean isqueued_macro(int arg0)
{
     return( isqueued((Device) arg0) );
}

/* original prototype: void lampoff(Byte arg0) */
void lampoff_macro(int arg0)
{
     lampoff((Byte) arg0);
}

/* original prototype: void lampon(Byte arg0) */
void lampon_macro(int arg0)
{
     lampon((Byte) arg0);
}

/* original prototype: void linewidth(short arg0) */
void linewidth_macro(int arg0)
{
     linewidth((short) arg0);
}

/* original prototype: void linewidthf(float arg0) */
void linewidthf_macro(double arg0)
{
     linewidthf((float) arg0);
}

/* original prototype: void lmbind(short arg0, short arg1) */
void lmbind_macro(int arg0, int arg1)
{
     lmbind((short) arg0, (short) arg1);
}

/* original prototype: void lmdef(short arg0, short arg1, short arg2, float arg3[]) */
void lmdef_macro(int arg0, int arg1, int arg2, float arg3[])
{
     lmdef((short) arg0, (short) arg1, (short) arg2,  arg3);
}

/* original prototype: void loadname(short arg0) */
void loadname_macro(int arg0)
{
     loadname((short) arg0);
}

/* original prototype: u_long readdisplay(Screencoord arg0, Screencoord arg1, Screencoord arg2, Screencoord arg3, u_long arg4[], long arg5) */
u_long readdisplay_macro(int arg0, int arg1, int arg2, int arg3, u_long arg4[], long arg5)
{
     return( readdisplay((Screencoord) arg0, (Screencoord) arg1, (Screencoord) arg2, (Screencoord) arg3,  arg4,  arg5) );
}

/* original prototype: void lookat(Coord arg0, Coord arg1, Coord arg2, Coord arg3, Coord arg4, Coord arg5, Angle arg6) */
void lookat_macro(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5, int arg6)
{
     lookat((Coord) arg0, (Coord) arg1, (Coord) arg2, (Coord) arg3, (Coord) arg4, (Coord) arg5, (Angle) arg6);
}

/* original prototype: long lrectread(Screencoord arg0, Screencoord arg1, Screencoord arg2, Screencoord arg3, u_long arg4[]) */
long lrectread_macro(int arg0, int arg1, int arg2, int arg3, u_long arg4[])
{
     return( lrectread((Screencoord) arg0, (Screencoord) arg1, (Screencoord) arg2, (Screencoord) arg3,  arg4) );
}

/* original prototype: void lrectwrite(Screencoord arg0, Screencoord arg1, Screencoord arg2, Screencoord arg3, u_long arg4[]) */
void lrectwrite_macro(int arg0, int arg1, int arg2, int arg3, u_long arg4[])
{
     lrectwrite((Screencoord) arg0, (Screencoord) arg1, (Screencoord) arg2, (Screencoord) arg3,  arg4);
}

/* original prototype: void lRGBrange(short arg0, short arg1, short arg2, short arg3, short arg4, short arg5, long arg6, long arg7) */
void lRGBrange_macro(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, long arg6, long arg7)
{
     lRGBrange((short) arg0, (short) arg1, (short) arg2, (short) arg3, (short) arg4, (short) arg5,  arg6,  arg7);
}

/* original prototype: void lshaderange(Colorindex arg0, Colorindex arg1, long arg2, long arg3) */
void lshaderange_macro(int arg0, int arg1, long arg2, long arg3)
{
     lshaderange((Colorindex) arg0, (Colorindex) arg1,  arg2,  arg3);
}

/* original prototype: void mapcolor(Colorindex arg0, short arg1, short arg2, short arg3) */
void mapcolor_macro(int arg0, int arg1, int arg2, int arg3)
{
     mapcolor((Colorindex) arg0, (short) arg1, (short) arg2, (short) arg3);
}

/* original prototype: void mapw(Object arg0, Screencoord arg1, Screencoord arg2, Coord *arg3, Coord *arg4, Coord *arg5, Coord *arg6, Coord *arg7, Coord *arg8) */
void mapw_macro(Object arg0, int arg1, int arg2, Coord *arg3, Coord *arg4, Coord *arg5, Coord *arg6, Coord *arg7, Coord *arg8)
{
     mapw( arg0, (Screencoord) arg1, (Screencoord) arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8);
}

/* original prototype: void mapw2(Object arg0, Screencoord arg1, Screencoord arg2, Coord *arg3, Coord *arg4) */
void mapw2_macro(Object arg0, int arg1, int arg2, Coord *arg3, Coord *arg4)
{
     mapw2( arg0, (Screencoord) arg1, (Screencoord) arg2,  arg3,  arg4);
}

/* original prototype: void mmode(short arg0) */
void mmode_macro(int arg0)
{
     mmode((short) arg0);
}

/* original prototype: void move(Coord arg0, Coord arg1, Coord arg2) */
void move_macro(double arg0, double arg1, double arg2)
{
     move((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void move2(Coord arg0, Coord arg1) */
void move2_macro(double arg0, double arg1)
{
     move2((Coord) arg0, (Coord) arg1);
}

/* original prototype: void move2s(Scoord arg0, Scoord arg1) */
void move2s_macro(int arg0, int arg1)
{
     move2s((Scoord) arg0, (Scoord) arg1);
}

/* original prototype: void moves(Scoord arg0, Scoord arg1, Scoord arg2) */
void moves_macro(int arg0, int arg1, int arg2)
{
     moves((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: void noise(Device arg0, short arg1) */
void noise_macro(int arg0, int arg1)
{
     noise((Device) arg0, (short) arg1);
}

/* original prototype: void ortho(Coord arg0, Coord arg1, Coord arg2, Coord arg3, Coord arg4, Coord arg5) */
void ortho_macro(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5)
{
     ortho((Coord) arg0, (Coord) arg1, (Coord) arg2, (Coord) arg3, (Coord) arg4, (Coord) arg5);
}

/* original prototype: void ortho2(Coord arg0, Coord arg1, Coord arg2, Coord arg3) */
void ortho2_macro(double arg0, double arg1, double arg2, double arg3)
{
     ortho2((Coord) arg0, (Coord) arg1, (Coord) arg2, (Coord) arg3);
}

/* original prototype: void pagecolor(Colorindex arg0) */
void pagecolor_macro(int arg0)
{
     pagecolor((Colorindex) arg0);
}

/* original prototype: void passthrough(short arg0) */
void passthrough_macro(int arg0)
{
     passthrough((short) arg0);
}

/* original prototype: void pdr(Coord arg0, Coord arg1, Coord arg2) */
void pdr_macro(double arg0, double arg1, double arg2)
{
     pdr((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void pdr2(Coord arg0, Coord arg1) */
void pdr2_macro(double arg0, double arg1)
{
     pdr2((Coord) arg0, (Coord) arg1);
}

/* original prototype: void pdr2s(Scoord arg0, Scoord arg1) */
void pdr2s_macro(int arg0, int arg1)
{
     pdr2s((Scoord) arg0, (Scoord) arg1);
}

/* original prototype: void pdrs(Scoord arg0, Scoord arg1, Scoord arg2) */
void pdrs_macro(int arg0, int arg1, int arg2)
{
     pdrs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: void perspective(Angle arg0, float arg1, Coord arg2, Coord arg3) */
void perspective_macro(int arg0, double arg1, double arg2, double arg3)
{
     perspective((Angle) arg0, (float) arg1, (Coord) arg2, (Coord) arg3);
}

/* original prototype: void picksize(short arg0, short arg1) */
void picksize_macro(int arg0, int arg1)
{
     picksize((short) arg0, (short) arg1);
}

/* original prototype: void pixmodef(long arg0, float arg1) */
void pixmodef_macro(long arg0, double arg1)
{
     pixmodef( arg0, (float) arg1);
}

/* original prototype: void pmv(Coord arg0, Coord arg1, Coord arg2) */
void pmv_macro(double arg0, double arg1, double arg2)
{
     pmv((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void pmv2(Coord arg0, Coord arg1) */
void pmv2_macro(double arg0, double arg1)
{
     pmv2((Coord) arg0, (Coord) arg1);
}

/* original prototype: void pmv2s(Scoord arg0, Scoord arg1) */
void pmv2s_macro(int arg0, int arg1)
{
     pmv2s((Scoord) arg0, (Scoord) arg1);
}

/* original prototype: void pmvs(Scoord arg0, Scoord arg1, Scoord arg2) */
void pmvs_macro(int arg0, int arg1, int arg2)
{
     pmvs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: void pnt(Coord arg0, Coord arg1, Coord arg2) */
void pnt_macro(double arg0, double arg1, double arg2)
{
     pnt((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void pnt2(Coord arg0, Coord arg1) */
void pnt2_macro(double arg0, double arg1)
{
     pnt2((Coord) arg0, (Coord) arg1);
}

/* original prototype: void pnt2s(Scoord arg0, Scoord arg1) */
void pnt2s_macro(int arg0, int arg1)
{
     pnt2s((Scoord) arg0, (Scoord) arg1);
}

/* original prototype: void pnts(Scoord arg0, Scoord arg1, Scoord arg2) */
void pnts_macro(int arg0, int arg1, int arg2)
{
     pnts((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: void pntsize(short arg0) */
void pntsize_macro(int arg0)
{
     pntsize((short) arg0);
}

/* original prototype: void pntsizef(float arg0) */
void pntsizef_macro(double arg0)
{
     pntsizef((float) arg0);
}

/* original prototype: void polarview(Coord arg0, Angle arg1, Angle arg2, Angle arg3) */
void polarview_macro(double arg0, int arg1, int arg2, int arg3)
{
     polarview((Coord) arg0, (Angle) arg1, (Angle) arg2, (Angle) arg3);
}

/* original prototype: void pushname(short arg0) */
void pushname_macro(int arg0)
{
     pushname((short) arg0);
}

/* original prototype: void qdevice(Device arg0) */
void qdevice_macro(int arg0)
{
     qdevice((Device) arg0);
}

/* original prototype: void qenter(Device arg0, short arg1) */
void qenter_macro(int arg0, int arg1)
{
     qenter((Device) arg0, (short) arg1);
}

/* original prototype: void rdr(Coord arg0, Coord arg1, Coord arg2) */
void rdr_macro(double arg0, double arg1, double arg2)
{
     rdr((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void rdr2(Coord arg0, Coord arg1) */
void rdr2_macro(double arg0, double arg1)
{
     rdr2((Coord) arg0, (Coord) arg1);
}

/* original prototype: void rdr2s(Scoord arg0, Scoord arg1) */
void rdr2s_macro(int arg0, int arg1)
{
     rdr2s((Scoord) arg0, (Scoord) arg1);
}

/* original prototype: void rdrs(Scoord arg0, Scoord arg1, Scoord arg2) */
void rdrs_macro(int arg0, int arg1, int arg2)
{
     rdrs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: long readpixels(short arg0, Colorindex arg1[]) */
long readpixels_macro(int arg0, Colorindex arg1[])
{
     return( readpixels((short) arg0,  arg1) );
}

/* original prototype: long readRGB(short arg0, RGBvalue arg1[], RGBvalue arg2[], RGBvalue arg3[]) */
long readRGB_macro(int arg0, RGBvalue arg1[], RGBvalue arg2[], RGBvalue arg3[])
{
     return( readRGB((short) arg0,  arg1,  arg2,  arg3) );
}

/* original prototype: void rect(Coord arg0, Coord arg1, Coord arg2, Coord arg3) */
void rect_macro(double arg0, double arg1, double arg2, double arg3)
{
     rect((Coord) arg0, (Coord) arg1, (Coord) arg2, (Coord) arg3);
}

/* original prototype: void rectcopy(Screencoord arg0, Screencoord arg1, Screencoord arg2, Screencoord arg3, Screencoord arg4, Screencoord arg5) */
void rectcopy_macro(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5)
{
     rectcopy((Screencoord) arg0, (Screencoord) arg1, (Screencoord) arg2, (Screencoord) arg3, (Screencoord) arg4, (Screencoord) arg5);
}

/* original prototype: void rectf(Coord arg0, Coord arg1, Coord arg2, Coord arg3) */
void rectf_macro(double arg0, double arg1, double arg2, double arg3)
{
     rectf((Coord) arg0, (Coord) arg1, (Coord) arg2, (Coord) arg3);
}

/* original prototype: void rectfs(Scoord arg0, Scoord arg1, Scoord arg2, Scoord arg3) */
void rectfs_macro(int arg0, int arg1, int arg2, int arg3)
{
     rectfs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2, (Scoord) arg3);
}

/* original prototype: long rectread(Screencoord arg0, Screencoord arg1, Screencoord arg2, Screencoord arg3, Colorindex arg4[]) */
long rectread_macro(int arg0, int arg1, int arg2, int arg3, Colorindex arg4[])
{
     return( rectread((Screencoord) arg0, (Screencoord) arg1, (Screencoord) arg2, (Screencoord) arg3,  arg4) );
}

/* original prototype: void rects(Scoord arg0, Scoord arg1, Scoord arg2, Scoord arg3) */
void rects_macro(int arg0, int arg1, int arg2, int arg3)
{
     rects((Scoord) arg0, (Scoord) arg1, (Scoord) arg2, (Scoord) arg3);
}

/* original prototype: void rectwrite(Screencoord arg0, Screencoord arg1, Screencoord arg2, Screencoord arg3, Colorindex arg4[]) */
void rectwrite_macro(int arg0, int arg1, int arg2, int arg3, Colorindex arg4[])
{
     rectwrite((Screencoord) arg0, (Screencoord) arg1, (Screencoord) arg2, (Screencoord) arg3,  arg4);
}

/* original prototype: void rectzoom(float arg0, float arg1) */
void rectzoom_macro(double arg0, double arg1)
{
     rectzoom((float) arg0, (float) arg1);
}

/* original prototype: void RGBcolor(short arg0, short arg1, short arg2) */
void RGBcolor_macro(int arg0, int arg1, int arg2)
{
     RGBcolor((short) arg0, (short) arg1, (short) arg2);
}

/* original prototype: void RGBcursor(short arg0, short arg1, short arg2, short arg3, short arg4, short arg5, short arg6) */
void RGBcursor_macro(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{
     RGBcursor((short) arg0, (short) arg1, (short) arg2, (short) arg3, (short) arg4, (short) arg5, (short) arg6);
}

/* original prototype: void RGBrange(short arg0, short arg1, short arg2, short arg3, short arg4, short arg5, Screencoord arg6, Screencoord arg7) */
void RGBrange_macro(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7)
{
     RGBrange((short) arg0, (short) arg1, (short) arg2, (short) arg3, (short) arg4, (short) arg5, (Screencoord) arg6, (Screencoord) arg7);
}

/* original prototype: void RGBwritemask(short arg0, short arg1, short arg2) */
void RGBwritemask_macro(int arg0, int arg1, int arg2)
{
     RGBwritemask((short) arg0, (short) arg1, (short) arg2);
}

/* original prototype: void rmv(Coord arg0, Coord arg1, Coord arg2) */
void rmv_macro(double arg0, double arg1, double arg2)
{
     rmv((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void rmv2(Coord arg0, Coord arg1) */
void rmv2_macro(double arg0, double arg1)
{
     rmv2((Coord) arg0, (Coord) arg1);
}

/* original prototype: void rmv2s(Scoord arg0, Scoord arg1) */
void rmv2s_macro(int arg0, int arg1)
{
     rmv2s((Scoord) arg0, (Scoord) arg1);
}

/* original prototype: void rmvs(Scoord arg0, Scoord arg1, Scoord arg2) */
void rmvs_macro(int arg0, int arg1, int arg2)
{
     rmvs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: void rot(float arg0, char arg1) */
void rot_macro(double arg0, char arg1)
{
     rot((float) arg0,  arg1);
}

/* original prototype: void rotate(Angle arg0, char arg1) */
void rotate_macro(int arg0, char arg1)
{
     rotate((Angle) arg0,  arg1);
}

/* original prototype: void rpdr(Coord arg0, Coord arg1, Coord arg2) */
void rpdr_macro(double arg0, double arg1, double arg2)
{
     rpdr((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void rpdr2(Coord arg0, Coord arg1) */
void rpdr2_macro(double arg0, double arg1)
{
     rpdr2((Coord) arg0, (Coord) arg1);
}

/* original prototype: void rpdr2s(Scoord arg0, Scoord arg1) */
void rpdr2s_macro(int arg0, int arg1)
{
     rpdr2s((Scoord) arg0, (Scoord) arg1);
}

/* original prototype: void rpdrs(Scoord arg0, Scoord arg1, Scoord arg2) */
void rpdrs_macro(int arg0, int arg1, int arg2)
{
     rpdrs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: void rpmv(Coord arg0, Coord arg1, Coord arg2) */
void rpmv_macro(double arg0, double arg1, double arg2)
{
     rpmv((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void rpmv2(Coord arg0, Coord arg1) */
void rpmv2_macro(double arg0, double arg1)
{
     rpmv2((Coord) arg0, (Coord) arg1);
}

/* original prototype: void rpmv2s(Scoord arg0, Scoord arg1) */
void rpmv2s_macro(int arg0, int arg1)
{
     rpmv2s((Scoord) arg0, (Scoord) arg1);
}

/* original prototype: void rpmvs(Scoord arg0, Scoord arg1, Scoord arg2) */
void rpmvs_macro(int arg0, int arg1, int arg2)
{
     rpmvs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: void sbox(Coord arg0, Coord arg1, Coord arg2, Coord arg3) */
void sbox_macro(double arg0, double arg1, double arg2, double arg3)
{
     sbox((Coord) arg0, (Coord) arg1, (Coord) arg2, (Coord) arg3);
}

/* original prototype: void sboxf(Coord arg0, Coord arg1, Coord arg2, Coord arg3) */
void sboxf_macro(double arg0, double arg1, double arg2, double arg3)
{
     sboxf((Coord) arg0, (Coord) arg1, (Coord) arg2, (Coord) arg3);
}

/* original prototype: void sboxfs(Scoord arg0, Scoord arg1, Scoord arg2, Scoord arg3) */
void sboxfs_macro(int arg0, int arg1, int arg2, int arg3)
{
     sboxfs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2, (Scoord) arg3);
}

/* original prototype: void sboxs(Scoord arg0, Scoord arg1, Scoord arg2, Scoord arg3) */
void sboxs_macro(int arg0, int arg1, int arg2, int arg3)
{
     sboxs((Scoord) arg0, (Scoord) arg1, (Scoord) arg2, (Scoord) arg3);
}

/* original prototype: void scale(float arg0, float arg1, float arg2) */
void scale_macro(double arg0, double arg1, double arg2)
{
     scale((float) arg0, (float) arg1, (float) arg2);
}

/* original prototype: void scrmask(Screencoord arg0, Screencoord arg1, Screencoord arg2, Screencoord arg3) */
void scrmask_macro(int arg0, int arg1, int arg2, int arg3)
{
     scrmask((Screencoord) arg0, (Screencoord) arg1, (Screencoord) arg2, (Screencoord) arg3);
}

/* original prototype: void setbell(Byte arg0) */
void setbell_macro(int arg0)
{
     setbell((Byte) arg0);
}

/* original prototype: void setcursor(short arg0, Colorindex arg1, Colorindex arg2) */
void setcursor_macro(int arg0, int arg1, int arg2)
{
     setcursor((short) arg0, (Colorindex) arg1, (Colorindex) arg2);
}

/* original prototype: void setdepth(Screencoord arg0, Screencoord arg1) */
void setdepth_macro(int arg0, int arg1)
{
     setdepth((Screencoord) arg0, (Screencoord) arg1);
}

/* original prototype: void setlinestyle(short arg0) */
void setlinestyle_macro(int arg0)
{
     setlinestyle((short) arg0);
}

/* original prototype: void setmap(short arg0) */
void setmap_macro(int arg0)
{
     setmap((short) arg0);
}

/* original prototype: void setmonitor(short arg0) */
void setmonitor_macro(int arg0)
{
     setmonitor((short) arg0);
}

/* original prototype: void setnurbsproperty(long arg0, float arg1) */
void setnurbsproperty_macro(long arg0, double arg1)
{
     setnurbsproperty( arg0, (float) arg1);
}

/* original prototype: void setpattern(short arg0) */
void setpattern_macro(int arg0)
{
     setpattern((short) arg0);
}

/* original prototype: void setshade(Colorindex arg0) */
void setshade_macro(int arg0)
{
     setshade((Colorindex) arg0);
}

/* original prototype: void setvaluator(Device arg0, short arg1, short arg2, short arg3) */
void setvaluator_macro(int arg0, int arg1, int arg2, int arg3)
{
     setvaluator((Device) arg0, (short) arg1, (short) arg2, (short) arg3);
}

/* original prototype: void shaderange(Colorindex arg0, Colorindex arg1, Screencoord arg2, Screencoord arg3) */
void shaderange_macro(int arg0, int arg1, int arg2, int arg3)
{
     shaderange((Colorindex) arg0, (Colorindex) arg1, (Screencoord) arg2, (Screencoord) arg3);
}

/* original prototype: void swapinterval(short arg0) */
void swapinterval_macro(int arg0)
{
     swapinterval((short) arg0);
}

/* original prototype: void textcolor(Colorindex arg0) */
void textcolor_macro(int arg0)
{
     textcolor((Colorindex) arg0);
}

/* original prototype: void textport(Screencoord arg0, Screencoord arg1, Screencoord arg2, Screencoord arg3) */
void textport_macro(int arg0, int arg1, int arg2, int arg3)
{
     textport((Screencoord) arg0, (Screencoord) arg1, (Screencoord) arg2, (Screencoord) arg3);
}

/* original prototype: void tie(Device arg0, Device arg1, Device arg2) */
void tie_macro(int arg0, int arg1, int arg2)
{
     tie((Device) arg0, (Device) arg1, (Device) arg2);
}

/* original prototype: void translate(Coord arg0, Coord arg1, Coord arg2) */
void translate_macro(double arg0, double arg1, double arg2)
{
     translate((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void unqdevice(Device arg0) */
void unqdevice_macro(int arg0)
{
     unqdevice((Device) arg0);
}

/* original prototype: void viewport(Screencoord arg0, Screencoord arg1, Screencoord arg2, Screencoord arg3) */
void viewport_macro(int arg0, int arg1, int arg2, int arg3)
{
     viewport((Screencoord) arg0, (Screencoord) arg1, (Screencoord) arg2, (Screencoord) arg3);
}

/* original prototype: void window(Coord arg0, Coord arg1, Coord arg2, Coord arg3, Coord arg4, Coord arg5) */
void window_macro(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5)
{
     window((Coord) arg0, (Coord) arg1, (Coord) arg2, (Coord) arg3, (Coord) arg4, (Coord) arg5);
}

/* original prototype: void writemask(Colorindex arg0) */
void writemask_macro(int arg0)
{
     writemask((Colorindex) arg0);
}

/* original prototype: void writepixels(short arg0, Colorindex arg1[]) */
void writepixels_macro(int arg0, Colorindex arg1[])
{
     writepixels((short) arg0,  arg1);
}

/* original prototype: void writeRGB(short arg0, RGBvalue arg1[], RGBvalue arg2[], RGBvalue arg3[]) */
void writeRGB_macro(int arg0, RGBvalue arg1[], RGBvalue arg2[], RGBvalue arg3[])
{
     writeRGB((short) arg0,  arg1,  arg2,  arg3);
}

/* original prototype: void xfpt(Coord arg0, Coord arg1, Coord arg2) */
void xfpt_macro(double arg0, double arg1, double arg2)
{
     xfpt((Coord) arg0, (Coord) arg1, (Coord) arg2);
}

/* original prototype: void xfpt2(Coord arg0, Coord arg1) */
void xfpt2_macro(double arg0, double arg1)
{
     xfpt2((Coord) arg0, (Coord) arg1);
}

/* original prototype: void xfpt2s(Scoord arg0, Scoord arg1) */
void xfpt2s_macro(int arg0, int arg1)
{
     xfpt2s((Scoord) arg0, (Scoord) arg1);
}

/* original prototype: void xfpt4(Coord arg0, Coord arg1, Coord arg2, Coord arg3) */
void xfpt4_macro(double arg0, double arg1, double arg2, double arg3)
{
     xfpt4((Coord) arg0, (Coord) arg1, (Coord) arg2, (Coord) arg3);
}

/* original prototype: void xfpt4s(Scoord arg0, Scoord arg1, Scoord arg2, Scoord arg3) */
void xfpt4s_macro(int arg0, int arg1, int arg2, int arg3)
{
     xfpt4s((Scoord) arg0, (Scoord) arg1, (Scoord) arg2, (Scoord) arg3);
}

/* original prototype: void xfpts(Scoord arg0, Scoord arg1, Scoord arg2) */
void xfpts_macro(int arg0, int arg1, int arg2)
{
     xfpts((Scoord) arg0, (Scoord) arg1, (Scoord) arg2);
}

/* original prototype: long fmgetchrwidth(fmfonthandle arg0, u_char arg1) */
long fmgetchrwidth_macro(fmfonthandle arg0, int arg1)
{
     return( fmgetchrwidth( arg0, (u_char) arg1) );
}
