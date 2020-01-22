/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: p-convolve.c
;;;  Author: Eero Simoncelli
;;;  Description: Convolution with parameterized filters for 2D images
;;;  Creation Date: Spring, 1986.
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#include <stdio.h>
#include <math.h>
#include "convolve.h"

/* --------------------------------------------------------------------

Please don't change this code unless you're sure you understand it.
It is VERY messy to debug.

This is very similar to the routine in convolve.c.  The convolution is
done in nine separate pieces. The edge regions are computed exactly as
in the internal_filter function in convolve.c.  The center region is
computed in a more efficient manner: The indices parameter is a 2D
array of integers indexing into the floating point coeffs vector.  A
zero always indicates a zero value in the kernel.  Thus the zeroth
element of the coeffs vector is ignored.

------------------------------------------------------------------------ */

internal_p_filter(image, x_dim, y_dim, indices, coeffs, coeff_dim, 
		  filt, temp, x_fdim, y_fdim, xgrid_start, xgrid_step,
		  ygrid_start,ygrid_step,result,edges)
  register int *indices;
  register float *coeffs, *image, *temp, *result;
  float *filt; 
  register int x_dim,y_dim,x_fdim,y_fdim,coeff_dim;
  int xgrid_start,xgrid_step,ygrid_start,ygrid_step;
  char *edges;
  { 
  register double sum;
  register int index, x_filt, im_pos, y_filt_lin;
  register int y_im_lin, x_pos, filt_size = x_fdim*y_fdim;
  register int y_pos, res_pos;
  register int last_ctr_col = x_dim - x_fdim;
  int last_ctr_row = (y_dim - y_fdim) * x_dim;
  int first_row, first_col;
  int x_fmid = x_fdim/2;
  int y_fmid = y_fdim/2;
  int x_stop = x_fdim - x_fmid + 1;
  int y_stop = y_fdim - y_fmid + 1;
  int ygrid_step_full = ygrid_step*x_dim;
  int prev_res_pos, x_res_dim = (x_dim-xgrid_start+xgrid_step-1)/xgrid_step;
  int rt_edge_res_pos;
  fptr reflect = edge_function(edges);  /* look up edge-handling function */
  if (!reflect) 
      {
      fprintf(stderr,"Unknown edge handler: %s\n",edges);
      return(-1);
      }
  
  for (y_pos=ygrid_start-y_fmid-1,res_pos=0;    /* top */
       y_pos<0;
       y_pos+=ygrid_step)
      {
      for (x_pos=xgrid_start-x_fmid-1;        /* top-left corner */
	   x_pos<0;
	   x_pos+=xgrid_step,res_pos++)
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,FILTER);
	  sum=0.0;
	  for (y_filt_lin=x_fdim,x_filt=y_im_lin=0;
	       y_filt_lin<=filt_size;
	       y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	    for (im_pos=y_im_lin;
		 x_filt<y_filt_lin;
		 x_filt++,im_pos++)
	      sum+=image[im_pos]*temp[x_filt];
	  result[res_pos] = sum;
	  }
      first_col = x_pos+1;
      (*reflect)(filt,x_fdim,y_fdim,0,y_pos,temp,FILTER);
      for (x_pos=first_col;	            /* top edge */
	   x_pos<last_ctr_col;
	   x_pos+=xgrid_step,res_pos++) 
	  {
	  sum=0.0;
	  for (y_filt_lin=x_fdim,x_filt=y_im_lin=0;
	       y_filt_lin<=filt_size;
	       y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	    for (im_pos=x_pos+y_im_lin;
		 x_filt<y_filt_lin;
		 x_filt++,im_pos++)
	      sum+=image[im_pos]*temp[x_filt];
	  result[res_pos] = sum;
	  }
      rt_edge_res_pos = res_pos + x_res_dim;   /* save this for later ... */
      for (x_pos+=(1-last_ctr_col);         /* top-right corner */
	   x_pos<x_stop;
	   x_pos+=xgrid_step,res_pos++) 
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,FILTER);
	  sum=0.0;
	  for (y_filt_lin=x_fdim,x_filt=y_im_lin=0;
	       y_filt_lin<=filt_size;
	       y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	    for (im_pos=y_im_lin+last_ctr_col;
		 x_filt<y_filt_lin;
		 x_filt++,im_pos++)
	      sum+=image[im_pos]*temp[x_filt];
	  result[res_pos] = sum;
	  }
      }                        /* end top */   

  first_row = x_dim*(y_pos+1);   /* need this to go down the sides */
  prev_res_pos = res_pos;
  for (x_pos=xgrid_start-x_fmid-1;           /* left edge */
       x_pos<0;
       x_pos+=xgrid_step)
      {
      res_pos = prev_res_pos;
      (*reflect)(filt,x_fdim,y_fdim,x_pos,0,temp,FILTER);
      for (y_pos=first_row;
	   y_pos<last_ctr_row;
	   y_pos+=ygrid_step_full, res_pos+=x_res_dim)
	  {
	  sum=0.0;
	  for (y_filt_lin=x_fdim,x_filt=0,y_im_lin=y_pos;
	       y_filt_lin<=filt_size;
	       y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	    for (im_pos=y_im_lin;
		 x_filt<y_filt_lin;
		 x_filt++,im_pos++)
	      sum+=image[im_pos]*temp[x_filt];
	  result[res_pos] = sum;
	  }
      prev_res_pos++;
      }
  /**  This is the main computational loop  **/
  for (y_pos=first_row;		/* center region of image */
       y_pos<last_ctr_row;
       y_pos+=ygrid_step_full)
      {
      res_pos = prev_res_pos;
      for (x_pos=first_col;
	   x_pos<last_ctr_col;
	   x_pos+=xgrid_step,res_pos++) 
	  {
	  for (x_filt=1;x_filt<coeff_dim;x_filt++) temp[x_filt] = 0.0;
	  for (y_filt_lin=x_fdim,x_filt=0,y_im_lin=y_pos;
	       y_filt_lin<=filt_size;
	       y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	    for (im_pos=x_pos+y_im_lin;
		 x_filt<y_filt_lin;
		 x_filt++,im_pos++)
		{
		index = indices[x_filt];
		if (index) temp[index] += image[im_pos];
		}
	  for (sum=0.0,x_filt=1;x_filt<coeff_dim;x_filt++) 
	    sum += (temp[x_filt]*coeffs[x_filt]);
	  result[res_pos] = sum;
	  }
      prev_res_pos+=x_res_dim;
      }
  prev_res_pos = rt_edge_res_pos;
  for (x_pos+=(1-last_ctr_col);                  /* right edge */
       x_pos<x_stop;
       x_pos+=xgrid_step) 
      {
      res_pos = prev_res_pos;
      (*reflect)(filt,x_fdim,y_fdim,x_pos,0,temp,FILTER);
      for (y_pos=first_row;
	   y_pos<last_ctr_row;
	   y_pos+=ygrid_step_full, res_pos+=x_res_dim)
	  {
	  sum=0.0;
	  for (y_filt_lin=x_fdim,x_filt=0,y_im_lin=y_pos;
	       y_filt_lin<=filt_size;
	       y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	    for (im_pos=y_im_lin+last_ctr_col;
		 x_filt<y_filt_lin;
		 x_filt++,im_pos++)
	      sum+=image[im_pos]*temp[x_filt];
	  result[res_pos] = sum;
	  }
      prev_res_pos++;
      }				/* end mid */
  
  res_pos -= (x_res_dim - 1);            /* go to lower left corner */
  for (y_pos=((y_pos-last_ctr_row)/x_dim)+1;     	/* bottom */
       y_pos<y_stop;
       y_pos+=ygrid_step) 
      {
      for (x_pos=xgrid_start-x_fmid-1;         /* bottom-left corner */
	   x_pos<0;
	   x_pos+=xgrid_step,res_pos++)
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,FILTER);
	  sum=0.0;
	  for (y_filt_lin=x_fdim,x_filt=0,y_im_lin=last_ctr_row;
	       y_filt_lin<=filt_size;
	       y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	    for (im_pos=y_im_lin;
		 x_filt<y_filt_lin;
		 x_filt++,im_pos++)
	      sum+=image[im_pos]*temp[x_filt];
	  result[res_pos] = sum;
	  }
      (*reflect)(filt,x_fdim,y_fdim,0,y_pos,temp,FILTER);
      for (x_pos=first_col;		        /* bottom edge */
	   x_pos<last_ctr_col;
	   x_pos+=xgrid_step,res_pos++) 
	  {
	  sum=0.0;
	  for (y_filt_lin=x_fdim,x_filt=0,y_im_lin=last_ctr_row;
	       y_filt_lin<=filt_size;
	       y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	    for (im_pos=x_pos+y_im_lin;
		 x_filt<y_filt_lin;
		 x_filt++,im_pos++)
	      sum+=image[im_pos]*temp[x_filt];
	  result[res_pos] = sum;
	  }
      for (x_pos+=1-last_ctr_col;	/* bottom-right corner */
	   x_pos<x_stop;
	   x_pos+=xgrid_step,res_pos++) 
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,FILTER);
	  sum=0.0;
	  for (y_filt_lin=x_fdim,x_filt=0,y_im_lin=last_ctr_row;
	       y_filt_lin<=filt_size;
	       y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	    for (im_pos=y_im_lin+last_ctr_col;
		 x_filt<y_filt_lin;
		 x_filt++,im_pos++)
	      sum+=image[im_pos]*temp[x_filt];
	  result[res_pos] = sum;
	  }
      }				/* end bottom */
  }			/* end of internal_filter */


/* --------------------------------------------------------------------

  Upsample image according to grid parameters and then convolve with
  filt, adding values into result array.

  WARNING: this subroutine ADDS the expanded image into the result, so
  the user must zero the result before invocation! 

------------------------------------------------------------------------ */	 

internal_p_expand(image,indices,coeffs,coeff_dim,filt,temp,x_fdim,y_fdim,
		  xgrid_start,xgrid_step,
		  ygrid_start,ygrid_step,result,x_dim,y_dim,edges,hex)
  register int *indices;
  register float *coeffs, *image, *temp, *result;
  float *filt; 
  register int x_fdim,y_fdim,x_dim,y_dim,coeff_dim;
  int xgrid_start,xgrid_step,ygrid_start,ygrid_step,hex;
  char *edges;
  {
  int x_fmid = x_fdim/2;
  int y_fmid = y_fdim/2;
  int x_stop = x_fmid+1;  
  int y_stop = y_fmid+1;
  register double val;
  register int filt_size = x_fdim*y_fdim;
  register int x_filt, x_res,y_res_lin, y_filt_lin;
  register int compute = 1;
  int start_compute = hex - 1; 
  int x_pos,y_pos, x_im;
  int last_row = (y_dim - y_fdim) * x_dim;
  int last_col = x_dim - x_fdim;
  fptr reflect = edge_function(edges);	 
  if (!reflect) return(-1);
  
  for (y_pos=ygrid_start-y_fmid-1,x_im=0; /* top */
       y_pos<0;
       y_pos+=ygrid_step)
      {
      if (hex)
	  {
	  start_compute = start_compute ? 0 : 1;    /* alternate on different lines */
	  compute = start_compute;
	  }
      for (x_pos=xgrid_start-x_fmid-1; 
	   x_pos<0;
	   x_pos+=xgrid_step,x_im++)
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,EXPAND);
	  val = image[x_im];
	  if (compute)
	    for (y_filt_lin=y_res_lin=0;
		 y_filt_lin<filt_size;
		 y_res_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_res=y_res_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		result[x_res] += val*temp[x_filt];
	  if (hex) compute = compute ? 0 : 1;      /* switch compute */
	  }
      (*reflect)(filt,x_fdim,y_fdim,0,y_pos,temp,EXPAND);
      for (x_pos++;		/* start at 1 */
	   x_pos<last_col;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  val = image[x_im];
	  if (compute)
	    for (y_filt_lin=y_res_lin=0;
		 y_filt_lin<filt_size;
		 y_res_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_res=x_pos+y_res_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		result[x_res]  += val*temp[x_filt];
	  if (hex) compute = compute ? 0 : 1;
	  }
      for (x_pos+=(1-last_col);
	   x_pos<x_stop;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,EXPAND);
	  val = image[x_im];		      
	  if (compute)
	    for (y_filt_lin=y_res_lin=0;
		 y_filt_lin<filt_size;
		 y_res_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_res=y_res_lin+last_col,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		result[x_res] += val*temp[x_filt];
	  if (hex) compute = compute ? 0 : 1;
	  }
      }				/* end top */   
  
  for (y_pos=(x_dim*(y_pos+1));	/* mid */
       y_pos<last_row;                
       y_pos+=(ygrid_step*x_dim))
      {
      if (hex)
	  {
	  start_compute = start_compute ? 0 : 1;    /* alternate on different lines */
	  compute = start_compute;
	  }
      for (x_pos=xgrid_start-x_fmid-1; 
	   x_pos<0;
	   x_pos+=xgrid_step,x_im++)
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,0,temp,EXPAND);
	  val = image[x_im];
	  if (compute)
	    for (y_filt_lin=0,y_res_lin=y_pos;
		 y_filt_lin<filt_size;
		 y_res_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_res=y_res_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		result[x_res] += val*temp[x_filt];
	  if (hex) compute = compute ? 0 : 1;
	  }
      /**  This is the main computational loop  **/
      for (x_pos++;		/* start at 1 */
	   x_pos<last_col;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  val = image[x_im];
	  for (x_filt=1;x_filt<coeff_dim;x_filt++) temp[x_filt] = val*coeffs[x_filt];
	  if (compute)
	    for (y_filt_lin=0,y_res_lin=y_pos;
		 y_filt_lin<filt_size;
		 y_res_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_res=x_pos+y_res_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		if (indices[x_filt]) result[x_res] += temp[indices[x_filt]];
	  if (hex) compute = compute ? 0 : 1;     
	  }
      for (x_pos+=(1-last_col);
	   x_pos<x_stop;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,0,temp,EXPAND);
	  val = image[x_im];		      
	  if (compute)
	    for (y_filt_lin=0,y_res_lin=y_pos;
		 y_filt_lin<filt_size;
		 y_res_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_res=y_res_lin+last_col,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		result[x_res] += val*temp[x_filt];
	  if (hex) compute = compute ? 0 : 1;     
	  }
      }				/* end mid */
  
  for (y_pos=((y_pos-last_row)/x_dim)+1;
       y_pos<y_stop;		/* bottom */
       y_pos+=ygrid_step) 
      {
      if (hex)
	  {
	  start_compute = start_compute ? 0 : 1;    /* alternate on different lines */
	  compute = start_compute;
	  }
      for (x_pos=xgrid_start-x_fmid-1; 
	   x_pos<0;
	   x_pos+=xgrid_step,x_im++)
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,EXPAND);
	  val = image[x_im];
	  if (compute)
	    for (y_filt_lin=0,y_res_lin=last_row;
		 y_filt_lin<filt_size;
		 y_res_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_res=y_res_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		result[x_res] += val*temp[x_filt];
	  if (hex) compute = compute ? 0 : 1;
	  }
      (*reflect)(filt,x_fdim,y_fdim,0,y_pos,temp,EXPAND);
      for (x_pos++;		/* start at 1 */
	   x_pos<last_col;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  val = image[x_im];
	  if (compute)
	    for (y_filt_lin=0, y_res_lin=last_row;
		 y_filt_lin<filt_size;
		 y_res_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_res=x_pos+y_res_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		result[x_res]  += val*temp[x_filt];
	  if (hex) compute = compute ? 0 : 1;
	  }
      for (x_pos+=1-last_col;
	   x_pos<x_stop;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,EXPAND);
	  val = image[x_im];		      
	  if (compute)
	    for (y_filt_lin=0,y_res_lin=last_row;
		 y_filt_lin<filt_size;
		 y_res_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_res=y_res_lin+last_col,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		result[x_res] += val*temp[x_filt];
	  if (hex) compute = compute ? 0 : 1;
	  }
      }				/* end bottom */
  }				/* end of internal_expand */
  

/* Local Variables: */
/* buffer-read-only: t */
/* End: */

