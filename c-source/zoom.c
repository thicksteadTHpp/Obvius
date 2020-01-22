/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: zoom.c
;;;  Author: Eero Simoncelli
;;;  Description: Upsampling and Downsampling for 8bit pictures
;;;  Creation Date: Summer/Fall 1988
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#include <memory.h>

/* ---------------------- 8-bit ---------------------------- */

/* Upsample, using pixel replication.  Sy and sx indicate the starting corner 
of the source array.  dy1, dx1, dy2, dx2 indicate the bounding rectangle of the
destination array */
internal_replicate_8bit(src, src_xdim, src_ydim, sx, sy, 
		   dst, dst_xdim, dx1, dy1, dx2, dy2, zoom)
  unsigned char *src, *dst;
  int src_ydim, src_xdim, sx, sy, dst_xdim, dx1, dx2, dy1, dy2;
  register int zoom;
  {
  register unsigned char *dst_row, *src_row, *dstp, *srcp;
  register unsigned char *last_row = dst + dst_xdim*dy2, *last_col;
  register int i, dx2_dx1 = dx2-dx1;

  for (dst_row=dst + dst_xdim*dy1 + dx1, src_row=src + src_xdim*sy + sx; 
       dst_row < last_row; 
       dst_row += dst_xdim*zoom, src_row += src_xdim)
      {
      last_col = dst_row + dx2_dx1;
      /* Copy one row */
      for(dstp=dst_row, srcp=src_row; dstp < last_col; dstp += zoom, srcp++)
	memset(dstp, *srcp, zoom);
      /* Copy that row over several times */
      for(i=1, dstp=dst_row+dst_xdim; i<zoom; i++, dstp += dst_xdim)
	memcpy(dstp, dst_row, dx2_dx1);
      }
  }

internal_subsample_8bit(src, src_xdim, src_ydim, dst, dst_xdim, skip)
  register unsigned char *src, *dst;
  int src_xdim, src_ydim, dst_xdim;
  register int skip;
  {
  register int src_pos, dst_pos, col;
  register int src_start, dst_start, row;

  for (row=src_start=dst_start=0; 
       row<src_ydim; 
       dst_start+=dst_xdim, row+=skip, src_start=(row*src_xdim))
    for (col=0, src_pos=src_start, dst_pos=dst_start;
	 col<src_xdim; 
	 col+=skip, src_pos+=skip, dst_pos++)
      dst[dst_pos] = src[src_pos];
  }

/* subsample only a region of an 8bit image */
void
internal_subsample_8bit_region(src, src_xdim, src_ydim, sx, sy, 
			       dst, dst_xdim, dx1, dy1, dx2, dy2, skip)
register unsigned char *src, *dst;
int src_ydim, src_xdim, sx, sy, dst_xdim, dx1, dx2, dy1, dy2;
register int skip;
{
  register int src_pos, dst_pos, dst_col;
  register int src_start, dst_start, dst_row;

  for (dst_row = dy1, dst_start = dst_xdim*dy1 + dx1, src_start = src_xdim*sy + sx;
       dst_row < dy2;
       dst_row++, dst_start += dst_xdim, src_start += skip*src_xdim)
    for (dst_col = dx1, src_pos = src_start, dst_pos = dst_start;
	 dst_col < dx2;
	 dst_col++, src_pos += skip, dst_pos++)
      dst[dst_pos] = src[src_pos];
}

/* paste an 8it region (zoom w/ zoom = 1.0) */
void
internal_paste_8bit_region(src, src_xdim, src_ydim, sx, sy,
			   dst, dst_xdim, dx1, dy1, dx2, dy2)
unsigned char *src, *dst;
int src_ydim, src_xdim, sx, sy, dst_xdim, dx1, dx2, dy1, dy2;
{
  register unsigned char *dst_row, *src_row, *dstp, *srcp;
  register unsigned char *last_row = dst + dst_xdim*dy2, *last_col;
  register int i, dx2_dx1 = dx2 - dx1;

 for (dst_row = dst + dst_xdim*dy1 + dx1, src_row = src + src_xdim*sy + sx; 
      dst_row < last_row; 
      dst_row += dst_xdim, src_row += src_xdim)
   memcpy(dst_row, src_row, dx2_dx1);
}  

/* ---------------------- 1-bit ---------------------------- */

typedef  unsigned int MY_BYTE;
#define BYTE_SIZE  (8*sizeof(MY_BYTE))

/* FOr now, this is a simple version that doesn't do sub-arrays.  dimensions
are measured in BITS! Assume dst array is zeroed! Assumes zoom < 32. 
bit-arrays are passed by lucid padded to even words.
*/

internal_replicate_1bit(src, src_xdim, src_ydim, dst, dst_xdim, zoom)
  MY_BYTE *src, *dst;
  unsigned int src_xdim, src_ydim, dst_xdim;
  register int zoom;
  {
  MY_BYTE *src_row, *dst_row;
  MY_BYTE *last_src_row;
  unsigned int src_bit_xdim = src_xdim;
  register unsigned int count;
  MY_BYTE *src_ptr;
  MY_BYTE src_bit;
  register MY_BYTE *dst_ptr;
  register unsigned int dst_shift;
  register MY_BYTE base_byte = (1<<zoom)-1;  /* should contain zoom 1's */

  /* convert xdim's to 32bit units */
  src_xdim = (src_xdim / BYTE_SIZE) + ((src_xdim%BYTE_SIZE) ? 1 : 0);
  dst_xdim = (dst_xdim / BYTE_SIZE) + ((dst_xdim%BYTE_SIZE) ? 1 : 0);
  last_src_row = src+(src_xdim*src_ydim);

  /* For each source row */
  for (src_row = src, dst_row = dst;
       src_row < last_src_row;
       dst_row += dst_xdim * zoom, src_row += src_xdim)
      {
      /* Go across source row, for each bit */
      for (src_ptr = src_row, src_bit = 1, count = 0, dst_ptr = dst_row, dst_shift = 0;
	   count < src_bit_xdim;
	   count++, src_bit <<= 1)
	  {
	  /* Check if we need to switch to next src byte */
	  if (src_bit == 0) { src_bit = 1; src_ptr++; }
	  if (*src_ptr&src_bit)	/* If bit is on */
	      {
	      *dst_ptr |= (base_byte << dst_shift);
	      dst_shift += zoom;
	      if (dst_shift >= BYTE_SIZE) /* must shift into next byte */
		  {
		  dst_ptr++;
		  dst_shift -= BYTE_SIZE; /* mod BYTE_SIZE */
		  /* if shifted past edge of byte, write into next byte */
		  if (dst_shift > 0) *dst_ptr |= (base_byte >> (zoom - dst_shift));
		  }
	      }
	  else
	      {
	      dst_shift += zoom;
	      if (dst_shift >= BYTE_SIZE)
		  {
		  dst_ptr++;
		  dst_shift -= BYTE_SIZE; /* mod BYTE_SIZE */
		  }
	      }
	  }
      /* Copy that row over several times */
      for (dst_ptr = dst_row+dst_xdim, count = 1;
	   count < zoom;
	   dst_ptr += dst_xdim, count++)
	memcpy(dst_ptr, dst_row, (dst_xdim * sizeof(MY_BYTE)));
      }
  }

internal_subsample_1bit(src, src_xdim, src_ydim, dst, dst_xdim, skip)
  MY_BYTE *src, *dst;
  unsigned int src_xdim, src_ydim, dst_xdim;
  register int skip;
  {
  MY_BYTE *src_row, *dst_row, *last_src_row;
  MY_BYTE *src_ptr, *dst_ptr;
  MY_BYTE src_bit, dst_bit;
  unsigned dst_bit_xdim = dst_xdim;
  unsigned int src_bit_shift, count;

  /* convert xdim's to 32bit units */
  src_xdim = (src_xdim / BYTE_SIZE) + ((src_xdim%BYTE_SIZE) ? 1 : 0);
  dst_xdim = (dst_xdim / BYTE_SIZE) + ((dst_xdim%BYTE_SIZE) ? 1 : 0);
  last_src_row = src+(src_xdim*src_ydim);

  for (src_row = src, dst_row = dst;
       src_row < last_src_row;
       dst_row += dst_xdim, src_row += src_xdim * skip)
      {
      for (src_ptr = src_row, src_bit = 1, src_bit_shift = 0, 
	        count = 0, dst_ptr = dst_row, dst_bit = 1;
	   count < dst_bit_xdim;
	   count++)
	  {
	  if (*src_ptr & src_bit)
	    *dst_ptr |= dst_bit;
	  if ( !(dst_bit <<= 1) ) 
	      {
	      dst_bit = 1;
	      dst_ptr++;
	      }
	  for (src_bit_shift += skip; src_bit_shift >= BYTE_SIZE; src_bit_shift -= BYTE_SIZE)
	    src_ptr++;
	  src_bit = (1 << src_bit_shift);
	  }
      }
  }

