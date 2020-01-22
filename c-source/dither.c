/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;  File: dither.c 
;;;  Author: Simoncelli/Sokolov 
;;;  Description: Simple, fast error-propagating dither routines
;;;  Creation Date: 
;;;  ---------------------------------------------------------------- 
;;;    Object-Based Vision and Image Understanding System (OBVIUS), 
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,   
;;;              Massachusetts Institute of Technology.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#include <stdio.h>
#include <math.h>

/*  Some definitions for 1bit dithering */
typedef  unsigned int MY_BYTE;
#define BYTE_SIZE  (8*sizeof(MY_BYTE))
#define MAX_BYTE   (1<<(BYTE_SIZE-1))
#define START_MASK 1
#define SHIFT_OP   <<

/* Original 1bit dither algorithm by Sokolov.  Assumes contiguous bits
 * across rows.  Lucid 4.0 passes bit-arrays to C as packed 32 bit
 * arrays.  Assumes bit image is SAME SIZE as float image: Ignores
 * a_xdim and a_ydim.  NOTE: algorithm is slightly different than
 * internal_dither_into_8bit_lut.  It penalizes clipping deviations as
 * well as quantization deviations!  It also tries to assign min to 0
 * and max to 1.  internal_dither_into_8bit_lut would not penalize
 * clipping deviations, and would assign the middle of each bin so
 * that 0.75*min+0.25max would be assigned to 0 and 0.25*min+0.75max
 * to 1. */
internal_dither_to_1bit(img, x_size, y_size, a, a_x_size, a_y_size, min, max)
  register float *img;
  register double min, max;
  MY_BYTE *a;
  int x_size, y_size, a_x_size, a_y_size;
  {
  register float threshold = (min+max)/2.0, val;
  register MY_BYTE bitmask = START_MASK;
  register MY_BYTE byte = 0;
  register int i;
  int f_size = x_size * y_size;

  for (i=0; i<f_size; i++)
      {
      val = img[i];
      if (val > threshold)
	  {
	  byte |= bitmask;
	  threshold += max - val;
	  }
      else 
	  {
	  byte &= ~bitmask;
	  threshold += min - val;
	  }
      bitmask = bitmask SHIFT_OP 1;
      if ( !((bitmask > 0) && (bitmask <= MAX_BYTE)) )
	  {
	  *a = byte;
	  a++;
	  byte = 0;
	  bitmask = START_MASK;
	  }
      }
  }

/* Another 1bit dither, with args matching the 8bit dither, that
 * dithers into sub-arrays.  a_x_size, x_offset, and y_offset should
 * be in BITS.  Assumes contiguous bits across rows of a, since Lucid
 * 4.0 passes bit-arrays to C as packed 32 bit arrays, with rows padded
 * to even word boundaies.  This one uses
 * the same algorithm as the 8bit dither (see note above): I think it
 * looks better than internal_dither_to_1bit.  Assumes no overhang
 * (i.e. img fits into array)! -EPS */
internal_dither_into_1bit(img, x_size, y_size, a, a_x_size, ped, scale,
			  x_offset, y_offset)
  float *img;
  register double scale, ped;
  register int x_size;
  int y_size;
  MY_BYTE *a;
  int a_x_size;
  int x_offset, y_offset;
  {
  register MY_BYTE bitmask = START_MASK;
  register double val, index;
  register double error = 0.0;
  register int im_pos, col; 
  register int a_pos, a_lin;

  for (im_pos=0, a_lin=a_x_size*y_offset+x_offset; 
       im_pos<x_size*y_size; 
       a_lin+=a_x_size)
      {
      for (a_pos=(a_lin/BYTE_SIZE), bitmask=(START_MASK SHIFT_OP (a_lin%BYTE_SIZE)), col=0;
	   col<x_size;
	   col++, im_pos++) 
	  {
	  val = (double) img[im_pos] - ped;
	  index = (double) floor( (double) ((val-error)*scale) );
	  error += (double) ((index+0.5) / scale) - val;
	  if (index > 0.5) a[a_pos] |= bitmask;
	  else a[a_pos] &= ~bitmask;
	  bitmask = bitmask SHIFT_OP 1;
	  if ( !((bitmask > 0) && (bitmask <= MAX_BYTE)) )
	      {
	      a_pos++;
	      bitmask = START_MASK;
	      }
	  }
      }
  }

/* 8bit dither using lookup table. Lookup table should be a byte array
   with entries corresponding to indexes of sequential grey entries in
   the colormap.   Assumes no overhang (i.e. img fits into array)! -EPS */
internal_dither_into_8bit_lut(img, x_size, y_size, a, a_x_size, ped, scale, 
			      x_offset, y_offset, lut, lut_size)
  register float *img;
  register int x_size;
  int y_size;
  unsigned char *a;
  int a_x_size;
  register double ped, scale;
  int x_offset, y_offset;
  unsigned char *lut;
  register int lut_size;
  {
  register double error = 0.0;
  register double val, index;
  register int int_index, im_pos, a_pos, col, a_lin;

  for (im_pos=0, a_lin=a_x_size*y_offset+x_offset; 
       im_pos<x_size*y_size; 
       a_lin+=a_x_size)
      {
      for (a_pos=a_lin, col=0; col<x_size; a_pos++, col++, im_pos++) 
	  {
	  val = (double) img[im_pos] - ped;
	  index = (double) floor( (double) ((val-error)*scale) );
	  error += (double) ((index+0.5) / scale) - val;
	  int_index = (int) index;
	  a[a_pos] = lut[((int_index >= 0)?((int_index < lut_size)?int_index:lut_size-1):0)];
	  }
      }
  }


/* 8bit dither using lookup table. Lookup table should be a byte array
   with entries corresponding to indexes of sequential grey entries in
   the colormap. -EPS */
internal_dither_to_8bit_lut(img, size, ped, scale, a, lut, lut_size)
  register float *img;
  int size;
  register double ped;
  double scale;
  register unsigned char *lut;
  register unsigned char *a;
  register int lut_size;
  {
  register double error = 0.0;
  register double val, index;
  register int int_index, i;

  for (i=0; i<size; i++)
      {
      val = (double) img[i] - ped;
      index = (double) floor( (double) ((val-error)*scale) );
      error += (double) ((index+0.5) / scale) - val;
      int_index = (int) index;
      a[i] = lut[((int_index >= 0)?((int_index < lut_size)?int_index:lut_size-1):0)];
      }
  }

/* This doesn't work: don't bother trying it. */

/*
internal_dither_zoom(im, a0, xdim, ydim, min, max)
  register float *im;
  double min, max;
  register unsigned short int * a0;
  int xdim, ydim;
  {
  register float error=0.0, binsize=(max-min)/5.0;
  register int i, j, n=0;
  register unsigned short int *a1, bit, gray, bitmask, byte0, byte1;
  a1 = a0 + xdim;
  for(i=0; i<ydim; i++)
      {
      j=0;
      while(j<xdim)
	  {
	  byte0 = byte1  = 0;
	  bitmask=LEFTMOST;
	  while(bitmask && j++<xdim)
	      {
	      gray = (*im-min-error) / binsize;
	      error += gray*binsize - (*im - min);
	      switch(gray)
		  {
		    case 2:
		  bit = (n + n/4 +1) %4;
		  (bit & 0x2) ? 
		    byte0 |= (bitmask >> (bit & 0x1)) :
		      byte1 |= (bitmask >> (bit & 0x1));
		    case 1:
		  bit = n % 4;
		  (bit & 0x2) ? 
		    byte0 |= (bitmask >> (bit & 0x1)) :
		      byte1 |= (bitmask >> (bit & 0x1));
		    case 0:
		  break;
		    case 3:
		    case 4:
		  bit =  bitmask | bitmask >> 1;
		  byte0 |= bit;
		  byte1 |= bit;
		  if(gray==4) break;
		  bit = n % 4;
		  bit & 0x2 ? 
		    byte0 &= ((~bitmask) >> (bit & 0x1)) :
		      byte1 &= ((~bitmask) >> (bit & 0x1));
		  }
	      bitmask >>= 2;
	      n++;
	      im++;
	      printf("bin - %d, bytes: %x %x\n", gray, byte0 , byte1);
	      
	      }
	  *a0++ = byte0;
	  *a1++ = byte1;
	  }
      a0 += (2*xdim+15)/16;
      a1 += (2*xdim+15)/16;
      }
  }
*/


/* Local Variables: */
/* buffer-read-only: t */
/* End: */

