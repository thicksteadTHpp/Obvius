/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: imops.c
;;;  Author: Eero Simoncelli
;;;  Description: Floating point (2D) image operations.
;;;  Creation Date: Summer/Fall 1988
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;          Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

/* 
  NOTE: The code in this file must be kept consistent with the corresponding lisp
  code found in imops.lisp.   The foreign function interface is in lucid-ffi.lisp.

  Tests on Sun SPARC machines indicate that using pointers is NOT more
  efficient than indexing arrays, so we use indices for legibility.
*/

#include <stdio.h>
#include <math.h>
#include <memory.h>
#include "obv.h"

#define sgn(a)  ( ((a)>0.0)?1.0:(((a)<0.0)?-1.0:0.0) )
#define MAXBINS 1024

internal_add(im1,im2,res,size)
  register float *im1, *im2, *res;
  register int size;
  {
  register int i;

  for (i=0; i<size; i++)  res[i] = im1[i] + im2[i];
  }

internal_sc_add(im,res,size,val)
  register float *im, *res;
  register double val;
  register int size;
  {
  register int i;

  for (i=0; i<size; i++)  res[i] = im[i] + val;
  }
  
internal_sub(im1,im2,res,size)
  register float *im1, *im2, *res;
  register int size;
  {
  register int i;

  for (i=0; i<size; i++)  res[i] = im1[i] - im2[i];
  }
  
internal_sc_sub(im,res,size,val)
  register float *im, *res;
  register int size;
  register double val;
  {
  register int i;

  for (i=0; i<size; i++)  res[i] = val - im[i];
  }

internal_mul(im1,im2,res,size)
  register float *im1, *im2, *res;
  register int size;
  {
  register int i;

  for (i=0; i<size; i++)  res[i] = im1[i] * im2[i];
  }
  
internal_sc_mul(im,res,size,val)
  register float *im, *res;
  register int size;
  register double val;
  {
  register int i;

  for (i=0; i<size; i++)  res[i] = im[i] * val;
  }
  
/* Return number of divide-by-zero cases */
int internal_div(im1,im2,res,size,zeroval)
  register float *im1, *im2, *res;
  register int size;
  double zeroval;
  {
  register int i;
  int count = 0;

  for (i=0; i<size; i++)
    if (im2[i] != 0.0)
      res[i] = im1[i] / im2[i];
    else
        { /* Old code used just the sign. Hence, if numerator was zero,
             result got set to zero. I changed it so result gets set to
             zeroval, since 0/0 is just as undefined as 1/0. EJC 7.25.91 */
          /* Old code:    res[i] = sgn(im1[i]) * zeroval; */
          res[i] = (im1[i] ? sgn(im1[i]) * zeroval : zeroval);
          count++;
        }
  return(count);
  }

int internal_sc_div(im,res,size,val,zeroval)
  register float *im, *res;
  register double val;
  double zeroval;
  register int size;
  {
  register int i;
  int count = 0;

  for (i=0; i<size; i++) 
    if (im[i] != 0.0)  
      res[i] = val / im[i];
    else 
	{
	res[i] = sgn(val) * zeroval;
	count++;
	}
  return(count);
  }

internal_linear_xform(im,result,size,scale,offset)
  register float *im, *result;
  register double scale, offset;
  register int size;
  {
  register int i;
  
  for (i=0; i<size; i++)  result[i] = offset + (scale * im[i]);
  }
  
internal_negate(im,result,size)
  register float *im, *result;
  register int size;
  {
  register int i;

  for (i=0; i<size; i++) result[i] = -im[i];
  }

internal_square(im,result,size)
  register float *im, *result;
  register int size;
  {
  register int i;
  register float val;

  for (i=0; i<size; i++)
      {
      val = im[i];
      result[i] = val * val;
      }
  }
  
internal_sqrt(im, res, size)
  register float *im, *res;
  register int size;
  {
  register int i;
  register float val;
  
  for (i=0; i<size; i++)
      {
      val = im[i]; 
      res[i] = (val < 0.0) ? 
	(float) (- sqrt((double) (-val))) : 
	  (float) sqrt((double) val);
      }
  }

internal_abs(im,result,size)
  register float *im, *result;
  register int size;
  {
  register int i;
  
  for (i=0; i<size; i++)  result[i] = (im[i] >= 0.0) ? im[i] : -im[i];
    /* fabs((double) im[i]); */
  }
  
internal_pow_sc(im,result,size,power)
  register float *im, *result;
  register double power;
  register int size;
  {
  register int i;
  register float val;

  for (i=0; i<size; i++)
      {
      val = im[i];
      if ( val == 0.0 ) result[i] = 0.0;
      else result[i] = sgn(val) * pow( fabs((double) val), ((double) power));
      }
  }

internal_sc_pow(im,result,size,base)
  register float *im, *result;
  register double base;
  register int size;
  {
  register int i;

  for (i=0; i<size; i++)  result[i] = pow( base, ((double) im[i]) );
  }

internal_pow(im1,im2,result,size)
  register float *im1, *im2, *result;
  register int size;
  {
  register int i;
  register float val;

  for (i=0; i<size; i++)
      {
      val = im1[i];
      result[i] = sgn(val) * pow( fabs( (double) val ), ( (double) im2[i] ) );
      }
  }

/* Computing ln is faster than log2 on Suns... */
internal_ln(im,res,size,zeroval)
  register float *im, *res;
  register int size;
  double zeroval;
  {
  register int i;
  register float val;
  int count = 0;
  
  for (i=0; i<size; i++)
      {
      val = im[i];
      if (val != 0.0) 
         res[i] = log( fabs( (double) val ) );   /* log base e */
      else 
         {
         res[i] = sgn(val) * zeroval;
	 count++;
	 }
      }
  return(count);
  }

internal_exp(im,res,size)
  register float *im, *res;
  register int size;
  {
  register int i;
  register float val;
  
  for (i=0; i<size; i++)
      {
      val= im[i];
      res[i] = exp( (double) val );      /* e^val */
      }
  }

int internal_sgn(arr,res,threshold,size)
float *arr,*res,threshold;
int size;
{
  register int i;

  for (i=size;i;i--) {
    if (*arr>threshold) *res = 1.0;
    else if (*arr<threshold) *res = -1.0;
    else *res = 0.0;
    arr++;
    res++;
  }
  return(0);
}

int internal_16bit_to_float(arr,res,size)
short int *arr;
float *res;
int size;
{
  register int i;

  for (i=size;i;i--) {
    *res++ = (float)*arr++;
  }
  return(0);
}

int internal_8bit_to_float(arr,res,size)
unsigned char *arr;
float *res;
int size;
{
  register int i;

  for (i=size;i;i--) {
    *res++ = (float)*arr++;
  }
  return(0);
}

int internal_logistic(arr,res,size)
float *arr,*res;
int size;
{
  register int i;

  for (i=size;i;i--) {
    *res++ = 1.0 / (1.0 + exp(-*arr++));
  }
  return(0);
}

/* Use linear interpolation on a lookup table */
internal_pointop (im, res, size, lut, lutsize, origin, increment)
  register float *im, *res, *lut;
  register double origin, increment; 
  register int size, lutsize;
  {
  register int i, index;
  register float pos;

  lutsize = lutsize - 2;	/* Maximum index value */
  if (increment > 0)
    for (i=0; i<size; i++)
	{
	pos = (im[i] - origin) / increment;
	index = (int) pos;
	index = (index < 0) ? 0 : ( (index > lutsize) ? lutsize : index );
	res[i] = lut[index] + (lut[index+1] - lut[index]) * (pos - index);
	}
  else
    for (i=0; i<size; i++) res[i] = *lut;
  }

/* Apply periodic lookup table function (with linear interpolation) to
   image.  Only one period of the function is specified.  The period of
   the function must be (lutsize-1)*binsize, and the lookup table must be of
   size lutsize.  The last entry in the table should be the same as the first! */
internal_periodic_pointop (im, res, size, lut, lutsize, binsize)
  register float *im, *res, *lut;
  register double binsize;
  register int size, lutsize;
  {
  register int i, index, mod_index;
  register double pos;

  lutsize -= 1;
  for(i=0; i<size; i++)
      {
      pos =  (im[i] / binsize);
      index = (pos<0) ? ((int) (pos-1.0)) : ((int) pos);
      mod_index = index % lutsize;
      if (mod_index < 0) mod_index += lutsize; /* machine dependent! */
      res[i] = lut[mod_index] + (lut[mod_index+1] - lut[mod_index]) * (pos - index);
      }
  }

internal_clip(im, res, size, below, above)
  register float *im, *res;
  register double below, above;
  register int size;
  {
  register int i;
  register float val;
  
  for (i=0; i<size; i++)
      {
      val = im[i];
      res[i] = (val < below) ? below : ( (val > above) ? above : val);
      }
  }

internal_im_thresh(im1, im2, res, size)
  register float *im1, *im2, *res;
  register int size;
  {
  register int i;
  
  for (i=0; i<size; i++)  res[i] = (im1[i] >= im2[i]) ? 1.0 : 0.0;
  }

internal_thresh(im, res, size, thresh)
  register float *im, *res;
  double thresh;
  register int size;
  {
  register int i;
  
  for (i=0; i<size; i++)  res[i] = (im[i] >= thresh) ? 1.0 : 0.0;
  }

internal_round (im,res,size,binsize)
  register float *im, *res;
  register int size;
  register double binsize;
  {
  register int i;

  if (binsize == 1.0)
    for (i=0 ; i<size; i++) 
	{
	if (im[i] > 0.0)
	  res[i] = (float) ((int) (im[i] + 0.5));
	else
	  res[i] = (float) ((int) (im[i] - 0.5));
	}
  else
    for (i=0 ; i<size; i++) 
	{
	if (im[i] > 0.0)
	  res[i] = binsize * ((int) ((im[i]/binsize) + 0.5));
	else
	 res[i] = binsize * ((int) ((im[i]/binsize) - 0.5));
	} 
  }

internal_truncate (im,res,size,binsize)
  register float *im, *res;
  register int size;
  register double binsize;
  {
  register int i;

  if (binsize == 1.0)
    for (i=0 ; i<size; i++) res[i] = (float) ((int) im[i]);
  else
    for (i=0 ; i<size; i++) res[i] = (float) ((int) (im[i]/binsize));
  }

internal_floor (im,res,size,binsize)
  register float *im, *res;
  register int size;
  register double binsize;
  {
  register int i;

  if (binsize == 1.0)
    for (i=0 ; i<size; i++) 
	{
	if (im[i] > 0.0)
	  res[i] = (float) ((int) im[i]);
	else
	  res[i] = (float) ((int) (im[i] - 1.0));
	}
  else
    for (i=0 ; i<size; i++) 
	{
	if (im[i] > 0.0)
	  res[i] = binsize * ((int) (im[i]/binsize));
	else
	 res[i] = binsize * ((int) ((im[i]/binsize) - 1.0));
	} 
  }

/* Uniform quantization, with quantized values set to mean of each bin. Origin should
be less than all values in im. */
internal_quantize(im,result,size,origin,binsize)
  register float *im, *result;
  register double binsize, origin;
  register int size;
  {
  register int i, temp;
  double binval[MAXBINS];
  int count[MAXBINS];
  int flag = 0;
  
  for (i=0; i<MAXBINS; i++)  { binval[i] = 0.0;  count[i] = 0; }

  /* compute number of pixels and sum of pixel values in each bin */
  for (i=0; i<size; i++)
      {
      temp = (int) ((im[i] - origin)/binsize + 0.5); /* compute index */
      if ((temp < MAXBINS) && (temp >= 0)) 
	  {  binval[temp] += im[i];  count[temp]++;  }
      else 
	{
	fprintf(stderr,"Error in INTERNAL_QUANTIZE: not enough bins!!\n");
	return(-1);
	}
      result[i] = (float) temp;	/* hold index for later */
      }

  /* find mean value of bin*/  
  for (i=0; i<MAXBINS; i++)
    if (count[i] > 1)  binval[i] = binval[i] / count[i];
  
  /* quantize res to the values in binval */
  for (i=0; i<size; i++)  result[i] = binval[ (int) result[i] ];
  }
  
internal_const(im, val, size)
  register float *im;
  double val;
  register int size;
  {
  register int i;
  register float fval = val;
  
  if (val == 0.0)
    memset(im, 0, size*sizeof(float));
  else
    for (i=0; i<size; i++)  im[i] = fval;
  }

/* Start is the value of the ramp in the upper left hand corner. Ystep
and xstep are the slopes in the y and x directions, respectively. */
internal_make_ramp (im, size, xsize, start, ystep, xstep)
  register float *im;
  register int size, xsize;
  register double ystep, xstep;
  double start;
  {
  register double val;
  register int y;
  register double xval;
  register int x;
  
  for (x=0, xval=start; x<xsize; x++, xval+=xstep)
    for (y=x, val=xval; y<size; y+=xsize, val+=ystep)
      im[y] = val;
  }

/* Make a uniform [0.0 1.0) random image. */
internal_random (im, size, seed)
  register float *im;
  register int size;
  long seed;
  {
  register int i;
  double drand48();

  srand48(seed);
  for (i=0; i<size; i++)  im[i] = (float) drand48();
  }

/* Circularly shift image by given amount.  gcf must be the greatest 
   common factor of sz and shift.  Works even if res=im! */
internal_circular_shift(im, res, xsz, ysz, xshift, yshift, xgcf, ygcf)
  float *im, *res;
  register int xsz, ysz;
  int xgcf, ygcf;
  register int xshift, yshift;
  {
  register float prev, swap;
  register float *line, *rline;
  register int pos, count;
  register int loopsize, base;
  
  ysz *= xsz; yshift *= xsz; ygcf *= xsz;
  loopsize = xsz/xgcf;
  for (line=im, rline=res; line<im+ysz; line+=xsz, rline+=xsz)
    for (base=0; base<xgcf; base++)
	{
	prev = line[base];
	pos = (base + xshift) % xsz;
	for (count=0; count<loopsize; count++)
	    {
	    swap = line[pos];  rline[pos] = prev;  prev = swap;
	    pos = (pos + xshift) % xsz;
	    }
	}
  loopsize = ysz/ygcf;
  if (yshift != 0)         /* Don't bother doing it if shift amount is zero */
      for (rline=res; rline<res+xsz; rline++)
	for (base=0; base<ygcf; base+=xsz)
	    {
	    prev = rline[base];
	    pos = (base + yshift) % ysz;
	    for (count=0; count<loopsize; count++)
		{
		swap = rline[pos];  rline[pos] = prev;  prev = swap;
		pos = (pos + yshift) % ysz;
		}
	    }
  }

internal8_circular_shift(im, res, xsz, ysz, xshift, yshift, xgcf, ygcf)
  char *im, *res;
  register int xsz, ysz;
  int xgcf, ygcf;
  register int xshift, yshift;
  {
  register char prev, swap;
  register char *line, *rline;
  register int pos, count;
  register int loopsize, base;
  
  ysz *= xsz; yshift *= xsz; ygcf *= xsz;
  loopsize = xsz/xgcf;
  for (line=im, rline=res; line<im+ysz; line+=xsz, rline+=xsz)
    for (base=0; base<xgcf; base++)
	{
	prev = line[base];
	pos = (base + xshift) % xsz;
	for (count=0; count<loopsize; count++)
	    {
	    swap = line[pos];  rline[pos] = prev;  prev = swap;
	    pos = (pos + xshift) % xsz;
	    }
	}
  loopsize = ysz/ygcf;
  if (yshift != 0)         /* Don't bother doing it if shift amount is zero */
      for (rline=res; rline<res+xsz; rline++)
	for (base=0; base<ygcf; base+=xsz)
	    {
	    prev = rline[base];
	    pos = (base + yshift) % ysz;
	    for (count=0; count<loopsize; count++)
		{
		swap = rline[pos];  rline[pos] = prev;  prev = swap;
		pos = (pos + yshift) % ysz;
		}
	    }
  }

/*
   If shift is left or up, do the looping in a reverse sense so that copying 
   can be done in place on the same array. No clipping is done.
   NB:: WORKS ON FLOAT ARRAYS!
*/
internal_paste(im, c1, sy1, sx1, sy2, sx2, res, c2, dy, dx)
  register float *im, *res;
  register int c1, c2, sx1, sx2, sy1, sy2, dx, dy;
  {
  register int i, j, ii, jj;
  if(im != res)
    for (i=sy1; i<sy2; i++, dy++)
      memcpy(res+(dy*c2+dx), im+(i*c1+sx1), sizeof(float) * (sx2-sx1));
  else
    for((dy>sy1)? (i=sy2-1, ii=(dy+sy2-sy1-1)) : (i=sy1, ii=dy); 
	(dy>sy1)? (i>=0) : (i<sy2); (dy>sy1)? (i--, ii--) : (i++, ii++))
      if(dy==sy1 && dx>sx1)
	  {
	  for(j=sx2-1, jj=(dx+sx2-sx1-1); j>=0; j--, jj--)
	    res[ii*c2+jj] = im[i*c1+j];
	  }
      else
	memcpy(res+ii*c2+dx, im+i*c1+sx1, sizeof(float) * (sx2-sx1));
  /*  Use this if memcpy won't overwrite itself
    for(j=sx1, jj=dx; j<sx2; j++, jj++)
    res[ii*c2+jj] = im[i*c1+j];
    */
  }

/* Im and res may be the same array, IF THEY ARE SQUARE. Dimensions are
for im.  This could probably be made faster... */
internal_transpose(im, res, ydim, xdim)
  register float *im, *res;
  register int xdim, ydim;
  {
  register int x, y;
  register int x_xdim, x_ydim, y_xdim, y_ydim;
  register float temp;
  int min_dim = (xdim<=ydim) ? xdim : ydim;

  /* first, do minimal square upper corner */
  for (y = y_xdim = y_ydim = 0;  y < min_dim;  y++, y_xdim += xdim, y_ydim += ydim)
    for (x = x_xdim = x_ydim = 0;  x <= y;  x++, x_xdim += xdim, x_ydim += ydim)
	{
	temp = im[y_xdim+x];
	res[y_ydim+x] = im[x_xdim+y];
	res[x_ydim+y] = temp;
	}
  
  if (xdim < ydim)
    for (y = xdim, y_xdim = xdim*xdim;  y < ydim;  y++, y_xdim += xdim)
      for (x = x_ydim = 0;  x < xdim;  x++, x_ydim += ydim)
	res[x_ydim+y] = im[y_xdim+x];
  else
    for (x = ydim, x_ydim = ydim*ydim;  x < xdim;  x++, x_ydim += ydim)
      for (y = y_xdim = 0;  y < ydim;  y++, y_xdim+= xdim)
	res[x_ydim+y] = im[y_xdim+x];
  }

/* Flip image in the x direction (about the y axis).  Works for res = im, too  */
internal_flip_x(im,res,ydim,xdim)
  register float *im, *res;
  int ydim, xdim;
  {
  register int x_left, x_right, y_left, y_right;
  register float temp;
  int xdim_2 = (xdim + 1) / 2;

  for (y_left = 0, y_right = xdim-1; 
       y_left < xdim*ydim;
       y_left = y_right+1, y_right+=xdim)
    for (x_left = y_left, x_right = y_right;
	 x_left < (y_left+xdim_2); 
	 x_left++,  x_right--)
	{
	temp = im[x_left];
	res[x_left] = im[x_right];
	res[x_right] = temp;
	}
  }

internal_flip_y(im,res,ydim,xdim)
  register float *im, *res;
  int ydim, xdim;
  {
  register int x_top, x_bott, y_top, y_bott;
  register float temp;
  int ydim_2 = ((ydim + 1) / 2) * xdim;

  for (x_top = 0, x_bott = (ydim-1)*xdim;
       x_top < xdim;
       x_top++,  x_bott++)
    for (y_top = x_top, y_bott = x_bott;
	 y_top < ydim_2;
	 y_top += xdim, y_bott -= xdim)
	{
	temp = im[y_top];
	res[y_top] = im[y_bott];
	res[y_bott] = temp;
	}
  }

internal_flip_xy(im,res,ydim,xdim)
  register float *im, *res;
  int ydim, xdim;
  {
  register float *im2, *res2;
  register float temp;
  register float *im_stop = im + (xdim*ydim+1)/2;
  
  for (im2=im+xdim*ydim-1, res2=res+xdim*ydim-1; im<im_stop; im++, res++, im2--, res2--)
      {
      temp = *im2;
      *res2 = *im;
      *res = temp;
      }
  }

internal_copy_array(im1,im2,size)
  float *im1, *im2;
  int size;
  {
  memcpy(im2, im1, size*sizeof(*im1));
  }
  
internal32_copy_array(im1,im2,size)
  int *im1, *im2;
  int size;
  {
  memcpy(im2, im1, size*sizeof(*im1));
  }

internal_im_min(im1,im2,res,size)
  register float *im1, *im2, *res;
  register int size;
  {
  register int i;
  register float val1, val2;
  for (i=0; i<size; i++)
      {
      val1 = im1[i];  val2 = im2[i];
      if (val2 < val1) res[i] = val2;
      else res[i] = val1;
      }
  }

internal_sc_im_min(im,res,size,val)
  register float *im, *res;
  register double val;
  register int size;
  {
  register int i;
  register float im_val;
  for (i=0; i<size; i++)
      {
      im_val = im[i];
      if (im_val < val) res[i] = im_val;
      else res[i] = val;
      }
}

/* return an image which has max at each pixel of im1 and im2 */
internal_im_max(im1,im2,res,size)
  register float *im1, *im2, *res;
  register int size;
  {
  register int i;
  register float val1, val2;
  for (i=0; i<size; i++)
      {
      val1 = im1[i];  val2 = im2[i];
      if (val2 > val1) res[i] = val2;
      else res[i] = val1;
      }
  }

internal_sc_im_max(im,res,size,val)
  register float *im, *res;
  register double val;
  register int size;
  {
  register int i;
  register float im_val;
  for (i=0; i<size; i++)
      {
      im_val = im[i];
      if (im_val > val) res[i] = im_val;
      else res[i] = val;
      }
}

internal_sq_err(im1,im2,result,size)
  register float *im1, *im2, *result;
  register int size;
  {
  register double val;
  register int i;
  
  for (i=0; i<size; i++)
      {
      val = im1[i] - im2[i];
      result[i] = val * val;
      }
  }
  
internal_abs_err(im1,im2,result,size)
  register float *im1, *im2, *result;
  register int size;
  {
  register int i;

  for (i=0; i<size; i++)  result[i] = fabs( (double) (im1[i] - im2[i]));
  }
  
internal_sum_of_squares(im1, im2, result, size)
  register float *im1, *im2, *result;
  register int size;
  {  
  register int i;
  
  for (i=0; i<size; i++)
    result[i] = im1[i] * im1[i] + im2[i] * im2[i];
  }
  
internal_sqrt_sum_of_squares(im1, im2, result, size)
  register float *im1, *im2, *result;
  register int size;
  {  
  register int i;
  
  for (i=0; i<size; i++)
      result[i] = sqrt( (double) ((im1[i] * im1[i]) + (im2[i] * im2[i])));
  }

/* return complex phase, with branch cut at pi. im1 = real, im2 = imaginary */
internal_phase(im1, im2, result, size)
  register float *im1, *im2, *result;
  register int size;
  {  
  register int i;
  register double im2val, im1val;
  
  for (i=0; i<size; i++)
      {
      im1val = im1[i];  im2val = im2[i];
      if (im1val == 0.0)
	  {
	  if (im2val == 0.0) result[i] = 0.0;
	  else if (im2val < 0.0) result[i] = -M_PI/2.0;
	  else result[i] = M_PI/2.0;
	  }
      else if (im1val < 0.0)
	  {
	  if (im2val == 0.0) result[i] = - M_PI;
	  if (im2val < 0.0) result[i] = atan(im2val / im1val) - M_PI;
	  else result[i] = atan(im2val / im1val) + M_PI;
	  }
      else result[i] = atan(im2val / im1val);
      }
  }

/* ------- operations below produce scalar results ---- */
  
/* return minimum value and (linear) position in array */
double internal_min(im,size)
  register float *im;
  register int size;
  {
  register int i;
  register float mn = *im;

  for (i=0; i<size; i++)
    if (im[i] < mn) mn = im[i];
  return((double) mn);
  }
  
double internal_max(im,size)
  register float *im;
  register int size;
  {
  register int i;
  register float mx = *im;

  for (i=0; i<size; i++)
      { 
      if (im[i] > mx) mx = im[i]; 
      }
  return((double) mx);
  }
  
/* return minimum value and (linear) position in array */
internal_min_of(im,size,vals)
  register float *im;
  register int size;
  float *vals;
  {
  register int i;
  register float mn = *im;
  register int mn_pos = 0;

  for (i=0; i<size; i++)
    if (im[i] < mn)
      { mn = im[i]; mn_pos = i; }
  vals[0] = mn;
  vals[1] = (float) mn_pos;
  }
  
internal_max_of(im,size,vals)
  register float *im;
  register int size;
  float *vals;
  {
  register int i;
  register float mx = *im;
  register int mx_pos = 0;

  for (i=0; i<size; i++)
    if (im[i] > mx) 
      { mx = im[i]; mx_pos = i; }
  vals[0] = mx;
  vals[1] = (float) mx_pos;
  }
  
internal32_min_of(im,size,vals)
  register int *im;
  register int size;
  int *vals;
  {
  register int i;
  register int mn = *im;
  register int mn_pos = 0;

  for (i=0; i<size; i++)
    if (im[i] < mn)
      { mn = im[i]; mn_pos = i; }
  vals[0] = mn;
  vals[1] = mn_pos;
  }
  
internal32_max_of(im,size,vals)
  register int *im;
  register int size;
  int *vals;
  {
  register int i;
  register int mx = *im;
  register int mx_pos = 0;

  for (i=0; i<size; i++)
    if (im[i] > mx) 
      { mx = im[i]; mx_pos = i; }
  vals[0] = mx;
  vals[1] = mx_pos;
  }
  
/* return both min and max */
internal_range(im, size, vals)
  register float *im;
  float *vals;
  register int size;
  {
  register int i;
  register float mn, mx, val;

  mn = mx = *im;
  for (i=0; i<size; i++)
      {
      val = im[i];
      if (val > mx) mx = val;
      if (val < mn) mn = val;
      }
  vals[0] = mn;
  vals[1] = mx;
  }
  
double internal_sum_of(im,size)
  register float *im;
  register int size;
  {
  register double sum = 0.0;
  register int i;
  
  for (i=0; i<size; i++)
    sum += im[i];
  return((double) sum);
  }

double internal_product_of(im,size)
  register float *im;
  register int size;
  {
  register double prod = 1.0;
  register int i;
  
  for (i=0; i<size; i++)
    prod *= im[i];
  return((double) prod);
  }

double internal_mean (im, size, ignore)
  register float *im;
  register int size;
  int ignore;
  {
  register double sum = 0.0;
  register int i, num = size;

  if (ignore) 
      {
      num = 0;
      for (i=0; i<size; i++)
	  if (im[i] != 0.0) 
	      {
	      num += 1;
	      sum += im[i];
	      }
      }
  else for (i=0; i<size; i++)
    sum += im[i];
  return((double) (sum/num));
  }

double internal_variance (im, size, ignore)
  register float *im;
  register int size;
  int ignore;
  {
  register int i, num = size;
  register double vsum = 0.0;
  register double sum = 0.0;
  register double temp;
  
  if (ignore) 
      {
      num = 0;
      for (i=0; i<size; i++)
	  if ((temp = im[i]) != 0.0) 
	      {
	      num += 1;
	      sum += temp;
	      vsum += temp*temp;
	      }
      }
  else for (i=0; i<size; i++)
      {
      temp = im[i];
      sum += temp;
      vsum += temp*temp;
      }
  temp = (vsum - (sum * sum / num)) / num;
  return((double) temp);
  }

/* Compute third moment about the mean */
double internal_third_moment(im, size, mean)
  register float *im;
  register int size;
  register double mean;
  {
  register int i;
  register double val, sum = 0.0;


  for (i=0; i<size; i++)
      { 
      val = im[i] - mean;
      sum += val * val * val;
      }
  return((double) (sum/size));
  }

/* Compute fourth moment about the mean */
double internal_fourth_moment(im, size, mean)
  register float *im;
  register int size;
  register double mean;
  {
  register int i;
  register double val, sum = 0.0;


  for (i=0; i<size; i++)
      { 
      val = im[i] - mean;
      val = val * val;
      sum += val * val;
      }
  return((double) (sum/size));
  }

/* Assumes hist is zeroed! */
internal_histogram(im,size,hist,hsize,origin,binsize)
  register float *im, *hist;
  register double binsize, origin;
  register int size, hsize;
  {
  register int i, temp;
  
  for (i=0; i<size; i++)
      {
      temp = (int) ((im[i] - origin)/binsize + 0.5);
      if ((temp < hsize) && (temp >= 0))  hist[temp] += 1.0;
      /* else printf("Error in INTERNAL_HISTOGRAM: image value %f outside of range\n",im[i]);
       */
      }
  }

/* Old function for computing entropy */
double internal_entropy(im,size,hist,hsize,origin)
  register float *im;
  int size, *hist, hsize, origin;
  {
  register int temp, i;
  register double dtemp, val = 0.0;
  register double logtwo = log(2.0);
  
  for (i=0; i<hsize; i++)  hist[i] = 0;
  for (i=0; i<size; i++)
      {
      temp = (int) (im[i] - origin + 0.5);
      if ((temp < hsize) && (temp >= 0)) hist[temp] += 1;
      else fprintf(stderr,"Error in INTERNAL_ENTROPY: image value outside of range!\n");
      }

  /* check histogram is ok */
  for (temp=i=0; i<hsize; i++)   temp += hist[i];
  if (temp != size) 
      {
      fprintf(stderr,"ERROR IN INTERNAL_ENTROPY: HISTOGRAM INCONSISTENT!\n");
      return((double) -1.0);
      }
  for (i=0; i<hsize; i++)
    if (hist[i] > 0)
	{
	dtemp = ((double) hist[i])/((double) size);
	val = val - dtemp*log(dtemp)/logtwo;
	}
  return((double) val);
  }

double internal_mean_sq_err(im1,im2,size)
  register float *im1, *im2;
  register int size;
  {
  register double val, sum = 0.0;
  register int i;
  
  for (i=0; i<size; i++)
      {
      val = im1[i] - im2[i];
      sum += val * val;
      }
  return((double) (sum/size));
  }
  
double internal_mean_abs_err(im1,im2,size)
  register float *im1, *im2;
  register int size;
  {
  register double sum = 0.0;
  register int i;

  for (i=0; i<size; i++)
    sum += fabs( (double) (im1[i] - im2[i]));
  return((double) (sum/size));
  }
  
/* ------------ routines producing side-effects ------------ */
  
internal_print_image(im,xdim,ydim,x1,y1,x2,y2, max_vals)
  register float *im;
  double max_vals;
  register int xdim,ydim,x1,y1,x2,y2;
  {
  int i,j, xstep,ystep, lin_pos,pos;
   
   xstep = ceil((x2 - x1 + 1)/max_vals);
   ystep = ceil((y2 - y1 + 1)/max_vals);

   printf("\n%5s","");
   for (i=x1; i<=x2; i+=xstep)
	printf("%6d",i);
   printf("\n%5s","");
   for (i=x1; i<=x2; i+=xstep)
	printf("%6s","-----");

   lin_pos = y1*xdim;
   for (j=y1; j<=y2; j+=ystep, lin_pos+=ystep*xdim)
	{
        printf("\n%4d|",j);
	for (i=x1, pos=lin_pos+i; i<=x2; i+=xstep, pos+=xstep)
	     {
	     printf("%6.1f",im[pos]);
	     }
	}
   printf("\n");
   }

internal_8bit_to_f(a, im, size)
  register float *im;
  register unsigned char *a;
  register int size;
  {
  register int i;
  
  for (i=0; i<size; i++)
    im[i] = (float) a[i];
  }

internal_32bit_to_f(a, im, size)
  register float *im;
  register unsigned long *a;
  register int size;
  {
  register int i;
  
  for (i=0; i<size; i++)
    im[i] = (float) a[i];
  }

internal_f_to_32bit(im, a, size, ped, scale)
  register float *im;
  register unsigned long *a;
  register int size;
  register double scale;
  double ped;
  {
  register int int_ped = (int) (ped * scale + 0.5);
  register int i;

  for (i=0; i<size; i++)
    a[i] = ((unsigned long) (im[i] * scale + 0.5)) - int_ped;
  }

internal_f_into_8bit_lut(im, im_xsz, im_ysz, a, a_xsz, ped, scale, 
			 x_off, y_off,lut, lutsz)
  register float *im;
  register double scale, ped;
  unsigned char *a;
  int im_xsz, im_ysz, a_xsz, x_off, y_off, lutsz;
  unsigned char *lut;
  {
  register int ceiling = lutsz-1;

  register unsigned char *a_pos = a+y_off*a_xsz+x_off;
  register float *im_pos = im, *im_end = im+im_xsz*im_ysz;
  register int temp, xa_pos=0;
  register int int_ped = (int) (ped * scale);

  while (im_pos<im_end)
      {
      if (xa_pos++ == im_xsz)
	  {
	  xa_pos=1;
	  a_pos += a_xsz - im_xsz;
	  }
      temp = (int) (*im_pos++ * scale + 0.5) - int_ped;
      *a_pos++ = lut[((temp > 0)?((temp < ceiling)?temp:ceiling):0)];
      }
  }

internal_f_into_8bit(im, im_xsz, im_ysz, a, a_xsz, ped, scale, x_off, y_off,
			floor, ceiling)
  register float *im;
  register double scale, ped;
  unsigned char *a;
  register int floor, ceiling;
  int im_xsz, im_ysz, a_xsz, x_off, y_off;
  {
  register unsigned char * a_pos=a+y_off*a_xsz+x_off;
  register float * im_pos=im, *im_end = im+im_xsz*im_ysz;
  register int temp, xa_pos=0;
  register int int_ped = (int) (ped * scale);

  while (im_pos<im_end)
      {
      if (xa_pos++ == im_xsz)
	  {
	  xa_pos=1;
	  a_pos += a_xsz - im_xsz;
	  }
      temp = (int) (*im_pos++ * scale + 0.5) - int_ped;
      *a_pos++ = (temp > floor)?((temp < ceiling)?temp:ceiling):floor;
      }
  }

internal_f_to_8bit(im, a, size, ped, scale, floor, ceiling)
  register float *im;
  register unsigned char * a;
  register int size;
  register double scale, ped;
  register int floor, ceiling;
  {
  register int temp, int_ped = (int) (ped * scale + 0.5);
  register int i;

  if (scale == 1.0)
    for (i=0; i<size; i++)
      {
      temp = ((int) im[i] + 0.5) - int_ped;
      a[i] = (temp > floor) ? ((temp < ceiling) ? temp : ceiling) : floor;
      }
  else 
    for (i=0; i<size; i++)
      {
      temp = ((int) (im[i] * scale + 0.5)) - int_ped;
      a[i] = (temp > floor) ? ((temp < ceiling) ? temp : ceiling) : floor;
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



/* Local Variables: */
/* buffer-read-only: t */
/* End: */

