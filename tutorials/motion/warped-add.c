/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: warped-add.c
;;;  Author: Eero Simoncelli
;;;  Description: Utility for computing flow fields
;;;  Creation Date: 6/93
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#include <math.h>
#include <stdio.h>

#define AND &&
#define OR ||

#define my_round(x)   ( nint((double) x) )
#define my_ceiling(x) ((int) ceil((double) (x)))

/* OLD VERSIONS: busted for boundary cases
#define my_round(x)  ( ((x)>=0.0) ? ((int) ((x)+0.5)) : ((int) ((x)-0.5)) )
#define my_floor(x) ( ((x)>=0.0) ? ((int) (x)) : ((int) ((x)-1.0)) )
#define my_ceiling(x)  ( ((x)>=0.0) ? ((int) ((x)+1.0)) : ((int) (x)) )
*/

/* SPARC ROUND FUNCTIONS:
aint - round toward zero.
anint - round (boundaries rounded up).
ceil, floor - as usual
nint - like anint, but returns an int.
*/

/* 
Add a set of images together, weighted by the elements of filt,
warping according to an INTEGER warp map .  Images is a vector of
images.  Its length should be equal to f_dim.  f_ctr is the center tap
number of the filter.  It also specifies which frame is the center
frame (other frames warped toward this one), and the frame with pixels
aligned with the warp vectors.  Don't compute a value if the filter
hangs over the edge of the image.  
*/
int_warped_add(images, xwarp, ywarp, res, x_dim, y_dim, filt, f_dim, f_ctr)
  register float *xwarp, *ywarp;
  register int f_ctr, x_dim, y_dim, f_dim;
  register float *filt;
  register float *res;
  float **images;
  {
  register int warped_x, warped_y;
  register int frame, index;
  register int y_shift, x_shift;
  register int y_pos, x_pos; 
  register float fval;
  register float *img;
  register int compute_flag = 1;

  /* copy center frame first */
  for (y_pos=(x_dim * y_dim), fval=filt[f_ctr], img=images[f_ctr], index=0; 
       index<y_pos;
       index++)
    res[index] = fval * img[index];

  for (index=y_pos=0; y_pos<y_dim; y_pos++)
    for (x_pos=0;  x_pos<x_dim; x_pos++, index++)
	{
	compute_flag = 1;
	y_shift=nint((double) ywarp[index]); 
	x_shift=nint((double) xwarp[index]);

	/* warp prev frames forward: */
	for (warped_y = y_pos - y_shift, warped_x = x_pos - x_shift, frame=f_ctr-1;
	     (frame >= 0) AND compute_flag;
	     frame--, warped_y -= y_shift, warped_x -= x_shift)
	    {
	    if ((0 <= warped_x) AND (warped_x < x_dim) 
		AND (0 <= warped_y) AND (warped_y < y_dim))
	        { res[index] += filt[frame] * images[frame][warped_y * x_dim + warped_x]; }
	    else 
	        { res[index] = 0.0; compute_flag = 0; }
	    }

	/* warp later frames backward: */
	for (warped_y = y_pos + y_shift, warped_x = x_pos + x_shift, frame=f_ctr+1;
	     (frame < f_dim) AND compute_flag;
	     frame++, warped_y+=y_shift, warped_x+=x_shift)
	    {
	    if ((0 <= warped_x) AND (warped_x < x_dim)
		AND (0 <= warped_y) AND (warped_y < y_dim))
	        { res[index] += filt[frame] * images[frame][warped_y * x_dim + warped_x]; }
	    else 
	        { res[index] = 0.0; compute_flag = 0; }
	    }
	}

  }

/*	old: works
for (y_shift=ywarp[index], x_shift=xwarp[index], frame=f_ctr+1;
	     (frame < f_dim) AND compute_flag;
	     frame++, y_shift+=ywarp[index], x_shift+=xwarp[index])
	    {
	    warped_x = x_pos + my_round(x_shift);
	    warped_y = y_pos + my_round(y_shift);

	    if ((0 <= warped_x) AND (warped_x < x_dim)
		AND (0 <= warped_y) AND (warped_y < y_dim))
	        { res[index] += filt[frame] * images[frame][warped_y * x_dim + warped_x]; }
*/

bilinear_warped_add(images, xwarp, ywarp, res, x_dim, y_dim, filt, f_dim, f_ctr)
  register float *xwarp, *ywarp;
  register int f_ctr, x_dim, y_dim, f_dim;
  register float *filt;
  register float *res;
  float **images;
  {
  register float frac_x, frac_y;
  register int int_x, int_y;
  register float warped_x, warped_y;
  register float *img;
  register int frame, index, y_pos, x_pos; 
  register float fval;
  register int compute_flag = 1;

  /* copy center frame first */
  for (y_pos=(x_dim * y_dim), fval=filt[f_ctr], img=images[f_ctr], index=0; 
       index<y_pos;
       index++)
    res[index] = fval * img[index];

  for (index=y_pos=0; y_pos<y_dim; y_pos++)
    for (x_pos=0;  x_pos<x_dim; x_pos++, index++)
	{
	compute_flag = 1;

	/* warp prev frames forward: */
	for (warped_y = y_pos - ywarp[index], warped_x = x_pos - xwarp[index], frame=f_ctr-1;
	     (frame >= 0) AND compute_flag;
	     frame--, warped_y -= ywarp[index], warped_x -= xwarp[index])
	    {
	    int_x = my_ceiling(warped_x);   int_y = my_ceiling(warped_y);
	    frac_x = int_x - warped_x;      frac_y = int_y - warped_y;
	    img = images[frame];
	    if ((0 < int_x) AND (int_x < x_dim) AND (0 < int_y) AND (int_y < y_dim))
	        { 
		res[index] += filt[frame] * 
		  (  img[int_y*x_dim + int_x] * (1.0-frac_y) * (1.0-frac_x)
		   + img[int_y*x_dim + (int_x-1)] * (1.0-frac_y) * frac_x
		   + img[(int_y-1)*x_dim + int_x] * frac_y * (1.0-frac_x)
		   + img[(int_y-1)*x_dim + (int_x-1)] * frac_y * frac_x );
		}
	    else  { compute_flag = 0; res[index] = 0.0; }
	    }
	/* warp later frames backward: */
	for (warped_y = y_pos + ywarp[index], warped_x = x_pos + xwarp[index], frame=f_ctr+1;
	     (frame < f_dim) AND compute_flag;
	     frame++, warped_y += ywarp[index], warped_x += xwarp[index])
	    {
	    int_x = my_ceiling(warped_x);   int_y = my_ceiling(warped_y);
	    frac_x = int_x - warped_x;      frac_y = int_y - warped_y;
	    img = images[frame];
	    if ((0 < int_x) AND (int_x < x_dim) AND (0 < int_y) AND (int_y < y_dim))
	        {
		res[index] += filt[frame] * 
		  (  img[int_y*x_dim + int_x] * (1.0-frac_y) * (1.0-frac_x)
		   + img[int_y*x_dim + (int_x-1)] * (1.0-frac_y) * frac_x
		   + img[(int_y-1)*x_dim + int_x] * frac_y * (1.0-frac_x)
		   + img[(int_y-1)*x_dim + (int_x-1)] * frac_y * frac_x );
		}
	    else  { compute_flag = 0; res[index] = 0.0; }
	    }
	}

  }

/* 
Insert ptr into vector at location specified by index (this is used by 
Common Lisp to make a vector of arrays).
*/
install_foreign_pointer(ptr,vector,index)
  register float **vector;
  register int index;
  register float *ptr;
  {
  vector[index] = ptr;
  }


/* ********************************************************************** 
Replacements for busted OBVIUS versions: should fix quantize too
********************************************************************** */
internal_round (im,res,size,binsize)
  register float *im, *res;
  register int size;
  register double binsize;
  {
  register int i;

  if (binsize == 1.0)
    for (i=0 ; i<size; i++) res[i] = (float) anint((double) im[i]);
  else
    for (i=0 ; i<size; i++) res[i] = (float) anint((double) (im[i]/binsize));
  }


internal_truncate (im,res,size,binsize)
  register float *im, *res;
  register int size;
  register double binsize;
  {
  register int i;

  if (binsize == 1.0)
    for (i=0 ; i<size; i++) res[i] = (float) aint((double) im[i]);
  else
    for (i=0 ; i<size; i++) res[i] = (float) aint((double) (im[i]/binsize));
  }

internal_floor (im,res,size,binsize)
  register float *im, *res;
  register int size;
  register double binsize;
  {
  register int i;

  if (binsize == 1.0)
    for (i=0 ; i<size; i++) res[i] = (float) floor((double) im[i]);
  else
    for (i=0 ; i<size; i++) res[i] = (float) floor ((double) (im[i]/binsize));
  }

internal_ceil (im,res,size,binsize)
  register float *im, *res;
  register int size;
  register double binsize;
  {
  register int i;

  if (binsize == 1.0)
    for (i=0 ; i<size; i++) res[i] = (float) ceil((double) im[i]);
  else
    for (i=0 ; i<size; i++) res[i] = (float) ceil((double) (im[i]/binsize));
  }

internal_acos(im,result,size)
  register float *im, *result;
  register int size;
  {
  register int i;

  for (i=0; i<size; i++) result[i] = (float) acos((double) im[i]);
  }


/* **********************************************************************
***** OLD VERSION: two frames at a time.

dir=0:   res[x,y] = image1[x,y] + imaeg2[x+wx,y+wy]
dir=1:   res[x,y] = image1[x-wx,y-wy] + image2[x,y]  

warped_add(image1, image2, xwarp, ywarp, res, x_dim, y_dim,direction)
  register float *xwarp, *ywarp;
  register int x_dim, y_dim;
  register float *res;
  register float *image1, *image2;
  {
  register int y_pos, x_pos, index; 
  register int warped_x, warped_y;

  if (direction)
    for (y_pos=0, index=0; y_pos<y_dim; y_pos++)
	{
	for (x_pos=0;  x_pos<x_dim; x_pos++, index++)
	    {
	    warped_x = x_pos - my_round(xwarp[index]);
	    warped_y = y_pos - my_round(ywarp[index]);
	    
	    if ((0 <= warped_x) AND (warped_x < x_dim)
		AND (0 <= warped_y) AND (warped_y < y_dim))
	      res[index] = image1[warped_y*y_dim + warped_x] + image2[index];
	    }
	}
  else
    for (y_pos=0, index=0; y_pos<y_dim; y_pos++)
	{
	for (x_pos=0;  x_pos<x_dim; x_pos++, index++)
	    {
	    warped_x = x_pos + my_round(xwarp[index]);
	    warped_y = y_pos + my_round(ywarp[index]);
	    
	    if ((0 <= warped_x) AND (warped_x < x_dim)
		AND (0 <= warped_y) AND (warped_y < y_dim))
	      res[index] = image1[index] + image2[warped_y*y_dim + warped_x];
	    }
	}
  }
*/


/* For emacs:
;; Local Variables:
;; compile-command: "cc -c -O4 -fsingle /u/eero/lisp/motion2/warped-add.c"
;; buffer-read-only: t
;; End:
*/

