/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: wrap.c
;;;  Author: Eero Simoncelli
;;;  Description: Circular convolution on 2D float images.
;;;  Creation Date: Spring 1986 
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#include "convolve.h"
#include <stdio.h>

#define MAX_IM_SIZE 2048

/* 
Performs correlation (i.e., convolution with filter(-x,-y))
followed by subsampling.  The operations are combined to avoid
unnecessary computation of the convolution samples that are to be
discarded in the subsampling operation.  The convolution is done in 9
sections so that mod operation is not performed unnecessarily.  The
grid_start parameters specify the starting location (first sample) of
the subsampling grid.  The grid_step parameters specify the step size
of the subsampling grid.  The stag parameter should be 0 or 1 and
indicates whether the subsampling grid is rectangular or quincunx (0
=> rectangular).  
*/

obv_internal_wrap_filter(image,x_dim,y_dim,filt,x_fdim,y_fdim,
	      xgrid_start,xgrid_step,ygrid_start,ygrid_step,result,stag)
  register float *filt, *result;
  float *image;
  register int x_dim,y_dim,x_fdim,y_fdim;
  int xgrid_start,xgrid_step,ygrid_start,ygrid_step,stag;
  {
  register double sum;
  register int filt_size = x_fdim*y_fdim;
  float *imval[MAX_IM_SIZE];
  register int x_filt, x_im,y_im, y_filt_lin;
  register int compute = 1;
  int last_ctr_col = x_dim - x_fdim;
  int last_ctr_row = y_dim - y_fdim;
  int x_fmid = x_fdim/2;
  int y_fmid = y_fdim/2;
  int x_stop = x_dim - x_fmid;
  int y_stop = y_dim - y_fmid;
  int start_compute = stag - 1; 
  register int x_pos,y_pos, x_res;
  
  if (y_dim > MAX_IM_SIZE) 
      {
      printf("INTERNAL_WRAP: Image too large to filter!");
      return(1);
      }
  
  for (y_pos=y_im=0;y_pos<y_dim;y_pos++,y_im+=x_dim)
    imval[y_pos] = (image+y_im);
  
  for (y_pos=ygrid_start-y_fmid, x_res=0;           /* top */
       y_pos<0;
       y_pos+=ygrid_step)
      {
      if (stag)
	  {
	  start_compute = start_compute ? 0 : 1;    /* alternate on different lines */
	  compute = start_compute;
	  }
      for (x_pos=xgrid_start-x_fmid; 
	   x_pos<0;
	   x_pos+=xgrid_step,x_res++)
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im=y_pos;
		 y_filt_lin<filt_size;
		 y_im++,y_filt_lin+=x_fdim)
	      for (x_im=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=imval[(y_im+y_dim)%y_dim][(x_im+x_dim)%x_dim]*filt[x_filt];
	  result[x_res] = sum;
	  if (stag) compute = compute ? 0 : 1;      /* switch compute */
	  }
      for (; x_pos<last_ctr_col;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im=y_pos;
		 y_filt_lin<filt_size;
		 y_im++,y_filt_lin+=x_fdim)
	      for (x_im=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=imval[(y_im+y_dim)%y_dim][x_im]*filt[x_filt];
	  result[x_res] = sum;
	  if (stag) compute = compute ? 0 : 1;
	  }
      for ( ; x_pos<x_stop;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im=y_pos;
		 y_filt_lin<filt_size;
		 y_im++,y_filt_lin+=x_fdim)
	      for (x_im=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=imval[(y_im+y_dim)%y_dim][x_im%x_dim]*filt[x_filt];
	  result[x_res] = sum;
	  if (stag) compute = compute ? 0 : 1;
	  }
      }   
  
  for (;y_pos<last_ctr_row;	/* mid */
       y_pos+=ygrid_step)
      {
      if (stag)
	  {
	  start_compute = start_compute ? 0 : 1;    /* alternate on different lines */
	  compute = start_compute;
	  }
      for (x_pos=xgrid_start-x_fmid; 
	   x_pos<0;
	   x_pos+=xgrid_step,x_res++)
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im=y_pos;
		 y_filt_lin<filt_size;
		 y_im++,y_filt_lin+=x_fdim)
	      for (x_im=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=imval[y_im][(x_im+x_dim)%x_dim]*filt[x_filt];
	  result[x_res] = sum;
	  if (stag) compute = compute ? 0 : 1;
	  }
      for (; x_pos<last_ctr_col;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im=y_pos;
		 y_filt_lin<filt_size;
		 y_im++,y_filt_lin+=x_fdim)
	      for (x_im=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=imval[y_im][x_im]*filt[x_filt];
	  result[x_res] = sum;
	  if (stag) compute = compute ? 0 : 1;
	  }
      for (; x_pos<x_stop;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im=y_pos;
		 y_filt_lin<filt_size;
		 y_im++,y_filt_lin+=x_fdim)
	      for (x_im=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=imval[y_im][x_im%x_dim]*filt[x_filt];
	  result[x_res] = sum;
	  if (stag) compute = compute ? 0 : 1;
	  }
      }
  
  for (;y_pos<y_stop ;		/* bottom */
       y_pos+=ygrid_step) 
      {
      if (stag)
	  {
	  start_compute = start_compute ? 0 : 1;    /* alternate on different lines */
	  compute = start_compute;
	  }
      for (x_pos=xgrid_start-x_fmid; 
	   x_pos<0;
	   x_pos+=xgrid_step,x_res++)
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im=y_pos;
		 y_filt_lin<filt_size;
		 y_im++,y_filt_lin+=x_fdim)
	      for (x_im=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=imval[y_im%y_dim][(x_im+x_dim)%x_dim]*filt[x_filt];
	  result[x_res] = sum;
	  if (stag) compute = compute ? 0 : 1;
	  }
      for (; x_pos<last_ctr_col;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im=y_pos;
		 y_filt_lin<filt_size;
		 y_im++,y_filt_lin+=x_fdim)
	      for (x_im=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=imval[y_im%y_dim][x_im]*filt[x_filt];
	  result[x_res] = sum;
	  if (stag) compute = compute ? 0 : 1;
	  }
      for (; x_pos<x_stop ;
	   x_pos+=xgrid_step,x_res++) 
	  {
	  sum=0.0;
	  if (compute)
	    for (y_filt_lin=0,y_im=y_pos;
		 y_filt_lin<filt_size;
		 y_im++,y_filt_lin+=x_fdim)
	      for (x_im=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_im++)
		sum+=imval[y_im%y_dim][x_im%x_dim]*filt[x_filt];
	  result[x_res] = sum;
	  if (stag) compute = compute ? 0 : 1;
	  }
      }     /* end bottom */
  }	/* end of internal_wrap_filter */



/* 
Performs upsampling (padding with zeros) followed by convolution.
The operations are combined to avoid unnecessary multiplication of
filter samples with zeros in the upsampled image.  The convolution is
done in 9 sections so that mod operation is not performed
unnecessarily.  Arguments are described in the comment above
internal_wrap_filter.
WARNING: this subroutine ADDS the expanded image into
the result, so the user must zero the result before invocation!  
*/

obv_internal_wrap_expand(image,filt,x_fdim,y_fdim,
	      xgrid_start,xgrid_step,ygrid_start,ygrid_step,result,
	      x_dim,y_dim,stag)
  register float *filt, *result;
  float *image; 
  register int x_fdim,y_fdim,x_dim,y_dim;
  int xgrid_start,xgrid_step,ygrid_start,ygrid_step,stag;
  {
  register double val;
  register int filt_size = x_fdim*y_fdim;
  float *imval[MAX_IM_SIZE];
  register int x_filt, x_res,y_res, y_filt_lin;
  register int compute = 1;
  int last_ctr_col = x_dim - x_fdim;
  int last_ctr_row = y_dim - y_fdim;
  int x_fmid = x_fdim/2;
  int y_fmid = y_fdim/2;
  int x_stop = x_dim - x_fmid;
  int y_stop = y_dim - y_fmid;
  int start_compute = stag - 1; 
  register int x_pos,y_pos, x_im;
  
  if (y_dim > MAX_IM_SIZE) 
      {
      printf("INTERNAL_WRAP: Image too large to expand!");
      return(1);
      }
  
  for (y_pos=y_res=0;y_pos<y_dim;y_pos++,y_res+=x_dim)
    imval[y_pos] = (result+y_res);
  
  for (y_pos=ygrid_start-y_fmid, x_im=0;	/* top */
       y_pos<0;
       y_pos+=ygrid_step)
      {
      if (stag)
	  {
	  start_compute = start_compute ? 0 : 1;    /* alternate on different lines */
	  compute = start_compute;
	  }
      for (x_pos=xgrid_start-x_fmid; 
	   x_pos<0;
	   x_pos+=xgrid_step,x_im++)
	  {
	  val = image[x_im];
	  if (compute)
	    for (y_filt_lin=0,y_res=y_pos;
		 y_filt_lin<filt_size;
		 y_res++,y_filt_lin+=x_fdim)
	      for (x_res=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		imval[(y_res+y_dim)%y_dim][(x_res+x_dim)%x_dim]
		  += val*filt[x_filt];
	  if (stag) compute = compute ? 0 : 1;      /* switch compute */
	  }
      for (; x_pos<last_ctr_col;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  val = image[x_im];
	  if (compute)
	    for (y_filt_lin=0,y_res=y_pos;
		 y_filt_lin<filt_size;
		 y_res++,y_filt_lin+=x_fdim)
	      for (x_res=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		imval[(y_res+y_dim)%y_dim][x_res]
		  += val*filt[x_filt];
	  if (stag) compute = compute ? 0 : 1;
	  }
      for ( ; x_pos<x_stop ;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  val = image[x_im];		      
	  if (compute)
	    for (y_filt_lin=0,y_res=y_pos;
		 y_filt_lin<filt_size;
		 y_res++,y_filt_lin+=x_fdim)
	      for (x_res=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		imval[(y_res+y_dim)%y_dim][x_res%x_dim]
		  += val*filt[x_filt];
	  if (stag) compute = compute ? 0 : 1;
	  }
      }   /* end top */
  
  for (;y_pos<last_ctr_row;	/* mid */
       y_pos+=ygrid_step)
      {
      if (stag)
	  {
	  start_compute = start_compute ? 0 : 1;    /* alternate on different lines */
	  compute = start_compute;
	  }
      for (x_pos=xgrid_start-x_fmid; 
	   x_pos<0;
	   x_pos+=xgrid_step,x_im++)
	  {
	  val = image[x_im];	 
	  if (compute)
	    for (y_filt_lin=0,y_res=y_pos;
		 y_filt_lin<filt_size;
		 y_res++,y_filt_lin+=x_fdim)
	      for (x_res=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		imval[y_res][(x_res+x_dim)%x_dim]
		  += val*filt[x_filt];
	  if (stag) compute = compute ? 0 : 1;
	  }
      for (; x_pos<last_ctr_col;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  val = image[x_im];
	  if (compute)
	    for (y_filt_lin=0,y_res=y_pos;
		 y_filt_lin<filt_size;
		 y_res++,y_filt_lin+=x_fdim)
	      for (x_res=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		imval[y_res][x_res] += val*filt[x_filt];
	  if (stag) compute = compute ? 0 : 1;
	  }
      for (; x_pos<x_stop ;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  val = image[x_im];		      
	  if (compute)
	    for (y_filt_lin=0,y_res=y_pos;
		 y_filt_lin<filt_size;
		 y_res++,y_filt_lin+=x_fdim)
	      for (x_res=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		imval[y_res][x_res%x_dim]+= val*filt[x_filt];
	  if (stag) compute = compute ? 0 : 1;
	  }
      }             /* end mid */
  
  for (;y_pos<y_stop ;		/* bottom */
       y_pos+=ygrid_step) 
      {
      if (stag)
	  {
	  start_compute = start_compute ? 0 : 1;    /* alternate on different lines */
	  compute = start_compute;
	  }
      for (x_pos=xgrid_start-x_fmid; 
	   x_pos<0;
	   x_pos+=xgrid_step,x_im++)
	  {
	  val = image[x_im];
	  if (compute)
	    for (y_filt_lin=0,y_res=y_pos;
		 y_filt_lin<filt_size;
		 y_res++,y_filt_lin+=x_fdim)
	      for (x_res=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		imval[y_res%y_dim][(x_res+x_dim)%x_dim]
		  += val*filt[x_filt];
	  if (stag) compute = compute ? 0 : 1;
	  }
      for (; x_pos<last_ctr_col;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  val = image[x_im];		      
	  if (compute)
	    for (y_filt_lin=0,y_res=y_pos;
		 y_filt_lin<filt_size;
		 y_res++,y_filt_lin+=x_fdim)
	      for (x_res=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		imval[y_res%y_dim][x_res] += val*filt[x_filt];
	  if (stag) compute = compute ? 0 : 1;
	  }
      for (; x_pos<x_stop;
	   x_pos+=xgrid_step,x_im++) 
	  {
	  val = image[x_im];
	  if (compute)
	    for (y_filt_lin=0,y_res=y_pos;
		 y_filt_lin<filt_size;
		 y_res++,y_filt_lin+=x_fdim)
	      for (x_res=x_pos,x_filt=y_filt_lin;
		   x_filt<y_filt_lin+x_fdim;
		   x_filt++,x_res++)
		imval[y_res%y_dim][x_res%x_dim]
		  += val*filt[x_filt];
	  if (stag) compute = compute ? 0 : 1;
	  }
      }          /* end bottom */
  }		/* end of internal_wrap_expand */



/* Local Variables: */
/* buffer-read-only: t */
/* End: */
