/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: hex-convolve.c
;;;  Author: Eero Simoncelli
;;;  Description: General convolution for 2D images
;;;  Creation Date: Spring, 1986.
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#include <math.h>
#include "convolve.h"

/* --------------------------------------------------------------------

  This file contains the old (pre-10/89) convolution code.  It is still
  used for hexagonal filtering.  Rectangular code has been made more
  efficient.
  Convolve filt with image, subsampling according to grid parameters,
  with values placed into result array.  Parameter temp is a temporary
  array the size of the filter.  edges is a string -- see convolve.h.
  The convolution is done in 9 sections, where the border sections use
  specially computed edge-handling filters.  Subsampling is done on a
  rectangular grid specified by the grid parameters.  If the hex
  parameter is non-zero, only every other sample is computed on each line
  (the others are set to zero).  The computation on consecutive lines is
  staggered.  If the hex parameter is 1, computation begins with the
  first sample on even-numbered lines and with the second on odd.  If it 
  is 2 (or more), the reverse occurs.

------------------------------------------------------------------------ */

internal_hex_filter(image, x_dim, y_dim, filt, temp, x_fdim, y_fdim,
		xgrid_start,xgrid_step,ygrid_start,ygrid_step,result,edges,hex)
  register float *image, *temp, *result;
  float *filt; 
  register int x_dim,y_dim,x_fdim,y_fdim;
  int xgrid_start,xgrid_step,ygrid_start,ygrid_step,hex;
  char *edges;
  { 
  int x_fmid = x_fdim/2;
  int y_fmid = y_fdim/2;
  int x_stop = x_fdim - x_fmid + 1;
  int y_stop = y_fdim - y_fmid + 1;
  register double sum;
  register int x_filt, x_im, y_im_lin, y_filt_lin;
  register int filt_size = x_fdim*y_fdim;
  register int x_pos,y_pos, x_res;
  register int compute = 1;
  int start_compute = hex - 1; 
  int last_row = (y_dim - y_fdim) * x_dim;
  int last_col = x_dim - x_fdim;
  fptr reflect = edge_function(edges);
  if (!reflect) return(-1);
  
  for (y_pos=ygrid_start-y_fmid-1,x_res=0;          /* top */
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
	   x_pos+=xgrid_step,x_res++)
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,FILTER);
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=y_im_lin=0;
		 y_filt_lin<filt_size;
		 y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_im=y_im_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=image[x_im]*temp[x_filt];
	  result[x_res] = sum;
	  if (hex) compute = compute ? 0 : 1;      /* switch compute */
	  }
      (*reflect)(filt,x_fdim,y_fdim,0,y_pos,temp,FILTER);
      for (x_pos++;		/* start at 1 */
	   x_pos<last_col;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=y_im_lin=0;
		 y_filt_lin<filt_size;
		 y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_im=x_pos+y_im_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=image[x_im]*temp[x_filt];
	  result[x_res] = sum;
	  if (hex) compute = compute ? 0 : 1;
	  }
      for (x_pos+=(1-last_col);
	   x_pos<x_stop;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,FILTER);
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=y_im_lin=0;
		 y_filt_lin<filt_size;
		 y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_im=y_im_lin+last_col,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=image[x_im]*temp[x_filt];
	  result[x_res] = sum;
	  if (hex) compute = compute ? 0 : 1;
	  }
      }				/* end top */   
  
  for (y_pos=x_dim*(y_pos+1);	/* mid */
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
	   x_pos+=xgrid_step,x_res++)
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,0,temp,FILTER);
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im_lin=y_pos;
		 y_filt_lin<filt_size;
		 y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_im=y_im_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=image[x_im]*temp[x_filt];
	  result[x_res] = sum;
	  if (hex) compute = compute ? 0 : 1;
	  }
      (*reflect)(filt,x_fdim,y_fdim,0,0,temp,FILTER);
      for (x_pos++;		/* start at 1 */
	   x_pos<last_col;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im_lin=y_pos;
		 y_filt_lin<filt_size;
		 y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_im=x_pos+y_im_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=image[x_im]*temp[x_filt];
	  result[x_res] = sum;
	  if (hex) compute = compute ? 0 : 1;
	  }
      for (x_pos+=(1-last_col);
	   x_pos<x_stop;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,0,temp,FILTER);
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im_lin=y_pos;
		 y_filt_lin<filt_size;
		 y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_im=y_im_lin+last_col,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=image[x_im]*temp[x_filt];
	  result[x_res] = sum;
	  if (hex) compute = compute ? 0 : 1;
	  }
      }				/* end mid */
  
  for (y_pos=((y_pos-last_row)/x_dim)+1;
       y_pos<y_stop ;		/* bottom */
       y_pos+=ygrid_step) 
      {
      if (hex)
	  {
	  start_compute = start_compute ? 0 : 1;    /* alternate on different lines */
	  compute = start_compute;
	  }
      for (x_pos=xgrid_start-x_fmid-1; 
	   x_pos<0;
	   x_pos+=xgrid_step,x_res++)
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,FILTER);
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im_lin=last_row;
		 y_filt_lin<filt_size;
		 y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_im=y_im_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=image[x_im]*temp[x_filt];
	  result[x_res] = sum;
	  if (hex) compute = compute ? 0 : 1;
	  }
      (*reflect)(filt,x_fdim,y_fdim,0,y_pos,temp,FILTER);
      for (x_pos++;		/* start at 1 */
	   x_pos<last_col;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im_lin=last_row;
		 y_filt_lin<filt_size;
		 y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_im=x_pos+y_im_lin,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=image[x_im]*temp[x_filt];
	  result[x_res] = sum;
	  if (hex) compute = compute ? 0 : 1;
	  }
      for (x_pos+=1-last_col;
	   x_pos<x_stop;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  (*reflect)(filt,x_fdim,y_fdim,x_pos,y_pos,temp,FILTER);
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im_lin=last_row;
		 y_filt_lin<filt_size;
		 y_im_lin+=x_dim,y_filt_lin+=x_fdim)
	      for (x_im=y_im_lin+last_col,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=image[x_im]*temp[x_filt];
	  result[x_res] = sum;
	  if (hex) compute = compute ? 0 : 1;
	  }
      }				/* end bottom */
  }				/* end of internal_filter */


/* --------------------------------------------------------------------

  Upsample image according to grid parameters and then convolve with
  filt, adding  values into result array.

  WARNING: this subroutine ADDS the expanded image into the result, so
  the user must zero the result before invocation! 

------------------------------------------------------------------------ */	 

internal_hex_expand(image,filt,temp,x_fdim,y_fdim,xgrid_start,xgrid_step,
		  ygrid_start,ygrid_step,result,x_dim,y_dim,edges,hex)
  register float *image, *temp, *result;
  float *filt; 
  register int x_fdim,y_fdim,x_dim,y_dim;
  int xgrid_start,xgrid_step,ygrid_start,ygrid_step,hex;
  char *edges;
  {
  int x_fmid = x_fdim/2;
  int y_fmid = y_fdim/2;
  int x_stop = x_fdim - x_fmid + 1;
  int y_stop = y_fdim - y_fmid + 1;
  register double val;
  register int filt_size = x_fdim*y_fdim;
  register int x_filt, x_res,y_res_lin, y_filt_lin;
  register int x_pos,y_pos, x_im;
  register int last_col = x_dim - x_fdim;
  register int compute = 1;
  int start_compute = hex - 1; 
  int last_row = (y_dim - y_fdim) * x_dim;
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
      (*reflect)(filt,x_fdim,y_fdim,0,0,temp,EXPAND);
      for (x_pos++;		/* start at 1 */
	   x_pos<last_col;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  val = image[x_im];
	  if (compute)
	    for (y_filt_lin=0,y_res_lin=y_pos;
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

