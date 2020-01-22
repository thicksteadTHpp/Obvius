/*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: warper.c
;;;  Author: Sokolov/Simoncelli
;;;  Description: Warp an image according to a flow field.
;;;  Creation Date:
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#include <math.h>
#include <string.h>

#define bound(x, n)	((x)<0 ? 0 : ((x)>(n) ? (n) : (x)))
#define mod(x, n) ((x)>0?((x)%(n)) : ((n)-(-(x)%(n))%(n)))

/* *** Would be nice to fix this to use the edge handlers in edges.c */
/* *** SHould re-hack to take im and result of different sizes: warper size
  must match result */

/* 
FROM_WARP warps array A into B according to inverse warp map [wx,wy].
The warp equation is

  B[i][j]  <-  A[i - wx[i][j]][j - wy[i][j]]

where we use linear interpolation to determine between-pixel values;
should be augmented to allow a user-selected filter mask instead of
linear interpolation...  References to pixels outside of A are
considered to be zero.
*/

bilinear_warp(im, res, wy, wx, ydim, xdim)
register float *im, *res, *wx, *wy;
register int xdim, ydim;
  {
  register int i, j, ia0, ja0, ia1, ja1;
  register float x, y, xy, wrpx, wrpy;

  for(j=0; j<ydim; j++)
    for(i=0; i<xdim; i++)
	{
	wrpx = i - *wx++;
	wrpy = j - *wy++;
	ia0 = floor(wrpx);  /* (int) doesn't work - breaks for negative nums! */
	ja0 = floor(wrpy);
	if ((ia0 >= 0) && (ja0 >= 0) && (ia0 < xdim-1) && (ja0 < ydim-1))
	    {
	    ja0 *= xdim;
	    ja1 = ja0 + xdim;
	    ia1 = ia0+1;
	    x = wrpx - ((int) wrpx);
	    y = wrpy - ((int) wrpy);
	    xy = x*y;
	    *res = im[ia0+ja0]*(1.0-x-y+xy) +
	           im[ia0+ja1]*(y-xy) + 
		   im[ia1+ja0]*(x-xy) +
		   im[ia1+ja1]*xy;
	    }
	else *res = 0.0;
	res++;
	}
  }

/* Cubic Spline interpolated warping. Fits a (separable) cubic
function to a 4x4 neighborhood. -Eero */
bicubic_warp(im, res, wy, wx, ydim, xdim)
  float *im, *res, *wx, *wy;
  int xdim, ydim;
  {
  register float *im_ptr;	/* accessed 38x in loop */
  register int base;		/* accessed 30x in loop */
  register float frac, temp;	/* accessed 20x in loop */
  register float frac2, frac3;  /* accessed 10x in loop */
  int x_inbounds_1, x_inbounds0, x_inbounds1, x_inbounds2; /* 6x in loop */
  float x_coeff_1, x_coeff0, x_coeff1, x_coeff2; /* 5x in loop */
  int i,j;

  memset(res, 0, xdim*ydim*sizeof(float));
  for(j=0; j<ydim; j++)
    for(i=0; i<xdim; i++)
	{
	/* Compute X fractional position and base index */
	frac = i - *wx++;
	base = floor(frac); /* was (int) - didn't work for negative nums! */
	frac = frac - base;
	frac2 = frac*frac;
	frac3 = frac*frac2;
	base--;
	im_ptr = im + base;

	x_inbounds_1 = ((base >= 0) && (base < xdim))?1:0;
	if (x_inbounds_1) x_coeff_1 = (frac2/2.0 - frac/3.0 - frac3/6.0);
	base++;
	x_inbounds0  = ((base >= 0) && (base < xdim))?1:0;
	if (x_inbounds0)  x_coeff0  = (1.0 - frac/2.0 - frac2 + frac3/2.0);
	base++;
	x_inbounds1  = ((base >= 0) && (base < xdim))?1:0;
	if (x_inbounds1)  x_coeff1 = (frac + frac2/2.0 - frac3/2.0);
	base++;
	x_inbounds2  = ((base >= 0) && (base < xdim))?1:0;
	if (x_inbounds2)  x_coeff2 = (frac3/6.0 - frac/6.0);

	/* Compute Y fractional position and base index */
	frac = j - *wy++;
	base = floor(frac);
	frac = frac - base;
	frac2 = frac*frac;
	frac3 = frac*frac2;
	base--;
	im_ptr += base*xdim;

	if ((base >= 0) && (base < ydim))  /* y_inbounds_1 */
	    {
	    temp = 0.0;
	    if (x_inbounds_1) temp += x_coeff_1 * *im_ptr;
	    im_ptr++;
	    if (x_inbounds0)  temp += x_coeff0  * *im_ptr;
	    im_ptr++;
	    if (x_inbounds1)  temp += x_coeff1  * *im_ptr;
	    im_ptr++;
	    if (x_inbounds2)  temp += x_coeff2  * *im_ptr;
	    im_ptr -= 3;
	    *res += temp * (frac2/2.0 - frac/3.0 - frac3/6.0);
	    }
	base++;  im_ptr += xdim;
	if ((base >= 0) && (base < ydim))  /* y_inbounds0 */
	    {
	    temp = 0.0;
	    if (x_inbounds_1) temp += x_coeff_1 * *im_ptr;
	    im_ptr++;
	    if (x_inbounds0)  temp += x_coeff0  * *im_ptr;
	    im_ptr++;
	    if (x_inbounds1)  temp += x_coeff1  * *im_ptr;
	    im_ptr++;
	    if (x_inbounds2)  temp += x_coeff2  * *im_ptr;
	    im_ptr -= 3;	    
	    *res += temp * (1 - frac/2.0 - frac2 + frac3/2.0);
	    }
	base++;  im_ptr += xdim; 
	if ((base >= 0) && (base < ydim))  /* y_inbounds1 */
	    {
	    temp = 0.0;
	    if (x_inbounds_1) temp += x_coeff_1 * *im_ptr;
	    im_ptr++;
	    if (x_inbounds0)  temp += x_coeff0  * *im_ptr;
	    im_ptr++;
	    if (x_inbounds1)  temp += x_coeff1  * *im_ptr;
	    im_ptr++;
	    if (x_inbounds2)  temp += x_coeff2  * *im_ptr;
	    im_ptr -= 3;	    
	    *res += temp * (frac + frac2/2.0 - frac3/2.0);
	    }
	base++;  im_ptr += xdim;
	if ((base >= 0) && (base < ydim))  /* y_inbounds2 */
	    {
	    temp = 0.0;
	    if (x_inbounds_1) temp += x_coeff_1 * *im_ptr;
	    im_ptr++;
	    if (x_inbounds0)  temp += x_coeff0  * *im_ptr;
	    im_ptr++;
	    if (x_inbounds1)  temp += x_coeff1  * *im_ptr;
	    im_ptr++;
	    if (x_inbounds2)  temp += x_coeff2  * *im_ptr;
	    im_ptr -= 3;	    
	    *res += temp * (frac3/6.0 - frac/6.0);
	    }
	res++;
	}
  }

/* Copy edge pixel on attempts to warp outside of source image */
clip_warp(a, b, from_ydim, from_xdim, wy, wx, to_ydim, to_xdim)
register float *a, *b, *wx, *wy;
register int from_xdim, from_ydim, to_xdim, to_ydim;
  {
  register int i, j, ia0, ja0, ia1, ja1;
  register float x, y, xy, wrpx, wrpy;

  for(j=0; j<to_ydim; j++)
    for(i=0; i<to_xdim; i++)
	{
	wrpx = i - *wx++;
	wrpy = j - *wy++;
	ia0 = (int) wrpx;
	ja0 = (int) wrpy;
	ia0 = bound(ia0, from_xdim-2);
	ja0 = bound(ja0, from_ydim-2);
	  {
	  ja0 *= from_xdim;
	  ja1 = ja0 + from_xdim;
	  ia1 = ia0+1;
	  x = wrpx - ((int) wrpx);
	  y = wrpy - ((int) wrpy);
	  xy = x*y;
	  *b = a[ia0+ja0]*(1.0-x-y+xy) +
	       a[ia0+ja1]*(y-xy) + 
	       a[ia1+ja0]*(x-xy) +
	       a[ia1+ja1]*xy;
	  }
	b++;
	}
  }

/* Wrap-around at edges.  Linear interpolation */
wrap_warp(a, b, wy, wx, ydim, xdim)
register float *a, *b, *wx, *wy;
register int xdim, ydim;
  {
  register int i, j, ia0, ja0, ia1, ja1;
  register float x, y, xy, wrpx, wrpy;

  for(j=0; j<ydim; j++)
    for(i=0; i<xdim; i++)
	{
	wrpx = i - *wx++;
	wrpy = j - *wy++;
	ia0 = (int) wrpx;
	ja0 = (int) wrpy;
	ia0 = mod(ia0, xdim);
	ja0 = mod(ja0, xdim);
	  {
	  ja0 *= xdim;
	  ja1 = ja0 + xdim;
	  ia1 = ia0+1;
	  x = wrpx - ((int) wrpx);
	  y = wrpy - ((int) wrpy);
	  xy = x*y;
	  *b = a[ia0+ja0]*(1.0-x-y+xy) +
	       a[ia0+ja1]*(y-xy) + 
	       a[ia1+ja0]*(x-xy) +
	       a[ia1+ja1]*xy;
	  }
	b++;
	}
  }


/* 
TO_WARP: This is the "forward" warping operation; 
   a[i][j]  ->  b[wx[i][j]+i][wy[i][j]+j]. 
**** THIS DOES NOT WORK PROPERLY  -- IT DOESN'T INTERPOLATE ...
*/

to_warp(a, b, wy, wx, ydim, xdim)
register float *a, *b, *wx, *wy;
int xdim, ydim;
  {
  register int i, j, ia0, ja0, ia1, ja1;
  register float x, y, xy, wrpx, wrpy;
  memset(b, 0, xdim*ydim*sizeof(float));
  for(j=0; j<ydim; j++)
    for(i=0; i<xdim; i++)
	{
	wrpx = i + *wx++;
	wrpy = j + *wy++;
	ia0 = (int) wrpx;
	ja0 = (int) wrpy;
	if((ia0 >= 0) && (ja0 >= 0) && (ia0 < xdim-1) && (ja0 < ydim-1))
	    {
	    ja0 *= xdim;
	    ja1 = ja0 + xdim;
	    ia1 = ia0+1;
	    x = wrpx - ((int) wrpx);
	    y = wrpy - ((int) wrpy);
	    xy = x*y;
	    b[ia0+ja0] += (*a)*(1.0-x-y+xy);
	    b[ia0+ja1] += (*a)*(y-xy);
	    b[ia1+ja0] += (*a)*(x-xy);
	    b[ia1+ja1] += (*a)*xy;
	    }
	a++;
	}
  }

/* ??? */
gen_warp(a, b, wy, wx, from_ydim, from_xdim, to_ydim, to_xdim)
register float *a, *b, *wx, *wy;
register int from_xdim, from_ydim, to_xdim, to_ydim;
  {
  register int i, j, ia0, ja0, ia1, ja1;
  register float x, y, xy, wrpx, wrpy;

  for(j=0; j<to_ydim; j++)
    for(i=0; i<to_xdim; i++)
	{
	wrpx = i - *wx++;
	wrpy = j - *wy++;
	ia0 = (int) wrpx;
	ja0 = (int) wrpy;
	if ((ia0 >= 0) && (ja0 >= 0) && (ia0 < from_xdim-1) && (ja0 < from_ydim-1))
	    {
	    ja0 *= from_xdim;
	    ja1 = ja0 + from_xdim;
	    ia1 = ia0+1;
	    x = wrpx - ((int) wrpx);
	    y = wrpy - ((int) wrpy);
	    xy = x*y;
	    *b =
	      a[ia0+ja0]*(1.0-x-y+xy) +
		a[ia0+ja1]*(y-xy) + 
		  a[ia1+ja0]*(x-xy) +
		    a[ia1+ja1]*xy;
	    }
	else *b = 0.0;
	b++;
	}
  }
