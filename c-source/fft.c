/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: fft.c
;;;  Author: Simoncelli
;;;  Description: Multi-dimensional FFT routine.
;;;  Creation Date:
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

/* 
==============================================================================
   Fast Fourier Transform algorithm from "Numerical Recipes in C"
   by Press, Flannery, Teukolsky, and Vetterling.
   Modified to take two input arrays (real and imaginary parts) instead
   of a single interleaved complex array.
===============================================================================
*/

#include <math.h>
#include "obv.h"

#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr 

/* Replaces rdata and idata by their ndim-dimenionsional DFT, if isign 
is 1.  If isign is -1, the inverse DFT is performed (without 
normalization.  nn[0..ndim-1] is an integer array containing the 
lengths of each dimension, which MUST ALL BE POWERS OF 2.  */

internal_fft (rdata, idata, nn, ndim, isign)
  float *rdata, *idata;
  int nn[], ndim, isign;
  {
  register int i1, i2, i3, i2rev, i3rev, ip1, ip2, ip3, ifp1, ifp2;
  register int k1, k2;
  int ibit, idim, n, nprev, nrem, ntot;
  register float tempi, tempr;
  register double wr, wi;
  double theta,  wpi, wpr, wtemp;

  rdata--;   /* offset because the program in the book indexes starting at 1 */
  idata--;
  
  ntot = 1;
  for (idim=0; idim<ndim; idim++)
    ntot *= nn[idim];
  nprev = 1;
  for (idim=ndim-1; idim>=0; idim--)
      {
      n = nn[idim];
      nrem = ntot/(n*nprev);
      ip1 = nprev;
      ip2 = ip1*n;
      ip3 = ip2*nrem;
      i2rev = 1;
      for (i2=1; i2<=ip2; i2+=ip1)
	  {
	  if (i2 < i2rev) 
	    for (i1=i2; i1<=i2+ip1-1; i1++)
	      for (i3=i1; i3<=ip3; i3+=ip2)
		  {
		  i3rev = i2rev + i3 - i2;
		  SWAP(rdata[i3],rdata[i3rev]);
		  SWAP(idata[i3],idata[i3rev]);
		  }
	  ibit = ip2 >> 1;
	  while (ibit >= ip1 && i2rev > ibit)
	      {
	      i2rev -= ibit;
	      ibit >>= 1;
	      }
	  i2rev += ibit;
	  }

      ifp1 = ip1;
      while (ifp1 < ip2)
	  {
	  ifp2 = ifp1 << 1;
	  theta = isign * 2.0 * M_PI / (ifp2/ip1);
	  wtemp = sin(0.5*theta);
	  wpr = -2.0 * wtemp * wtemp;
	  wpi = sin(theta);
	  wr = 1.0;
	  wi = 0.0;
	  for (i3=1; i3<=ifp1; i3+=ip1)
	      {
	      for (i1=i3; i1<=i3+ip1-1; i1++)
		for (i2=i1; i2<=ip3; i2+=ifp2)
		    {
		    k1 = i2;
		    k2 = k1 + ifp1;
		    tempr = wr * rdata[k2] - wi * idata[k2];
		    tempi = wr * idata [k2] + wi * rdata[k2];
		    rdata[k2] = rdata[k1] - tempr;
		    idata[k2] = idata[k1] - tempi;
		    rdata[k1] += tempr;
		    idata[k1] += tempi;
		    }
	      wr = (wtemp=wr) * wpr - wi * wpi + wr;
	      wi = wi * wpr + wtemp * wpi + wi;
	      }
	  ifp1 = ifp2;
	  }
      nprev *= n;
      }
  }


internal_realft (rdata, idata, nn, ndim, isign)
  float *rdata, *idata;
  int nn[], ndim, isign;
  {
  }


/* Local Variables: */
/* buffer-read-only: t */
/* End: */
