/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: gl-obv-blt.c
;;;  Author: Patrick C. Teo
;;;  Description: Blting code into 8bit GL colormap/24bit frame buffer
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/
#include <memory.h>


void internal_color_into_24bit(r_bytes, g_bytes, b_bytes, bytes_xsize, dest, x_size, y_size, x_offset, y_offset)
     register unsigned char    *r_bytes, *g_bytes, *b_bytes;
     register long             *dest;
     register int              bytes_xsize, x_size, y_size, x_offset, y_offset;
{
     register long       *dest_pos = dest + y_offset*x_size + x_offset;
     register int        xdest_pos = 0, size = x_size * y_size;

     while (size-->0) {
          if (xdest_pos++ == x_size) {
	       xdest_pos = 1;
	       dest_pos += x_size - bytes_xsize;
	  }
	  *dest++ = ((long)(*r_bytes++)) | (((long)(*g_bytes++)) << 8) | (((long)(*b_bytes++)) << 16);
     }
}


void internal_color_f_into_24bit_lut(r_im, g_im, b_im,
				     im_xsz, im_ysz, a, a_xsz, ped, scale, 
				     x_off, y_off, 
				     r_lut, g_lut, b_lut, lutsz)
     register float 	*r_im, *g_im, *b_im;
     register double 	scale, ped;
     long 		*a;
     int 		im_xsz, im_ysz, a_xsz, x_off, y_off, lutsz;
     unsigned char 	*r_lut, *g_lut, *b_lut;
{
     register int 	ceiling = lutsz-1;
     register long 	*a_pos = a+y_off*a_xsz+x_off, r_lut_val, g_lut_val, b_lut_val;
     register float 	*r_im_pos = r_im, *r_im_end = r_im+im_xsz*im_ysz;
     register float     *g_im_pos = g_im, *b_im_pos = b_im;
     register int 	temp, xa_pos=0;
     register int 	int_ped = (int) (ped * scale);
     
     while (r_im_pos<r_im_end) {
	  if (xa_pos++ == im_xsz) {
	       xa_pos=1;
	       a_pos += a_xsz - im_xsz;
	  }
	  temp = (int) (*r_im_pos++ * scale + 0.5) - int_ped;
	  r_lut_val = (long) r_lut[((temp > 0)?((temp < ceiling)?temp:ceiling):0)];
	  temp = (int) (*g_im_pos++ * scale + 0.5) - int_ped;
	  g_lut_val = (long) g_lut[((temp > 0)?((temp < ceiling)?temp:ceiling):0)];
	  temp = (int) (*b_im_pos++ * scale + 0.5) - int_ped;
	  b_lut_val = (long) b_lut[((temp > 0)?((temp < ceiling)?temp:ceiling):0)];
	  
	  *a_pos++ = r_lut_val | (g_lut_val << 8) | (b_lut_val << 16);
     }
}


void internal_gray_f_into_24bit_lut(im, im_xsz, im_ysz, a, a_xsz, ped, scale, 
				    x_off, y_off, lut, lutsz)
     register float 	*im;
     register double 	scale, ped;
     unsigned long	*a;
     int 		im_xsz, im_ysz, a_xsz, x_off, y_off, lutsz;
     unsigned char 	*lut;
{
     register int 		ceiling = lutsz-1;
     register unsigned long 	*a_pos = a+y_off*a_xsz+x_off, lut_val;
     register float 		*im_pos = im, *im_end = im+im_xsz*im_ysz;
     register int 		temp, xa_pos=0;
     register int 		int_ped = (int) (ped * scale);

     while (im_pos<im_end) {
	  if (xa_pos++ == im_xsz) {
	       xa_pos=1;
	       a_pos += a_xsz - im_xsz;
	  }
	  temp = (int) (*im_pos++ * scale + 0.5) - int_ped;
	  lut_val = (unsigned long) lut[((temp > 0)?((temp < ceiling)?temp:ceiling):0)];
	  *a_pos++ = lut_val | (lut_val << 8) | (lut_val << 16);
     }
}


void internal_paste_24bit_to_24bit(src, src_xdim, src_ydim, sx, sy,
				  dst, dst_xdim, dx1, dy1, dx2, dy2)
     long		*src;
     int		src_xdim, src_ydim, sx, sy;
     long		*dst;
     int		dst_xdim, dx1, dy1, dx2, dy2;
{
     register long	*src_row, *srcp;
     register long	*dst_row, *first_dst_row, *dstp, *last_dstp;
     
     for (dst_row = dst + dst_xdim*(dy2-1) + dx1, src_row = src + src_xdim*sy + sx,
	  first_dst_row = dst + dst_xdim*dy1 + dx1;
	  dst_row >= first_dst_row;
	  dst_row -= dst_xdim, src_row += src_xdim)

	  /*
	   * Copy a single row.
	   */
	  for (srcp = src_row, dstp = dst_row, last_dstp = dst_row + dx2; 
	       dstp < last_dstp;
	       srcp++, dstp++)
	       
	       *dstp = *srcp;
}


void internal_supersample_24bit_to_24bit(src, src_xdim, src_ydim, sx, sy,
					 dst, dst_xdim, dx1, dy1, dx2, dy2, zoom)
     long		*src;
     int		src_xdim, src_ydim, sx, sy;
     long		*dst;
     int		dst_xdim, dx1, dy1, dx2, dy2, zoom;
{
     register int	i;
     register long	*src_row, *srcp;
     register long	*dst_row, *first_dst_row, *dstp, *last_dstp;
     
     for (dst_row = dst + dst_xdim*(dy2-1) + dx1, src_row = src + src_xdim*sy + sx,
	  first_dst_row = dst + dst_xdim*dy1 + dx1;
	  dst_row >= first_dst_row;
	  dst_row -= dst_xdim * zoom, src_row += src_xdim) {

	  /*
	   * Copy a single row.
	   */
	  for (srcp = src_row, dstp = dst_row, last_dstp = dst_row + dx2; 
	       dstp < last_dstp;
	       srcp++)
	       
	       /*
		* Zoom that row
		*/
	       for (i=0; (i < zoom) && (dstp < last_dstp); i++, dstp++)
		    *dstp = *srcp;

	  /*
	   * Replicate that row.
	   */
	  for (i=0, dstp = dst_row - dst_xdim;
	       (i < zoom - 1) && (dstp >= first_dst_row);
	       dstp -= dst_xdim, i++)
	       memcpy((char *)dstp, (char *)dst_row, sizeof(long)*(dx2 - dx1));
     }
}


void internal_subsample_24bit_to_24bit(src, src_xdim, src_ydim, dst, dst_xdim, zoom)
     long	*src;
     int	src_xdim, src_ydim;
     long	*dst;
     int	dst_xdim, zoom;
{
     register long	*src_row, *srcp, *first_src_row, *last_srcp;
     register long	*dst_row, *dstp;
     
     for (dst_row = dst, src_row = src + src_xdim*(src_ydim-1), first_src_row = src;
	  src_row >= first_src_row;
	  dst_row += dst_xdim, src_row -= src_xdim*zoom)

	  /*
	   * Copy a single row.
	   */
	  for (srcp = src_row, dstp = dst_row, last_srcp = src_row + src_xdim;
	       srcp < last_srcp;
	       srcp+=zoom, dstp++)
	       
	       *dstp = *srcp;
}





void internal_paste_8bit_to_16bit(src, src_xdim, src_ydim, sx, sy,
				  dst, dst_xdim, dx1, dy1, dx2, dy2)
     unsigned char	*src;
     int		src_xdim, src_ydim, sx, sy;
     unsigned short	*dst;
     int		dst_xdim, dx1, dy1, dx2, dy2;
{
     register unsigned char	*src_row, *srcp;
     register unsigned short	*dst_row, *first_dst_row, *dstp, *last_dstp;
     
     for (dst_row = dst + dst_xdim*(dy2-1) + dx1, src_row = src + src_xdim*sy + sx,
	  first_dst_row = dst + dst_xdim*dy1 + dx1;
	  dst_row >= first_dst_row;
	  dst_row -= dst_xdim, src_row += src_xdim)

	  /*
	   * Copy a single row.
	   */
	  for (srcp = src_row, dstp = dst_row, last_dstp = dst_row + dx2; 
	       dstp < last_dstp;
	       srcp++, dstp++)
	       
	       *dstp = (short) *srcp;
}

void internal_paste_8bit_to_24bit(src, src_xdim, src_ydim, sx, sy,
				  dst, dst_xdim, dx1, dy1, dx2, dy2)
     unsigned char	*src;
     int		src_xdim, src_ydim, sx, sy;
     long		*dst;
     int		dst_xdim, dx1, dy1, dx2, dy2;
{
     register unsigned char	*src_row, *srcp;
     register long		*dst_row, *first_dst_row, *dstp, *last_dstp;
     
     for (dst_row = dst + dst_xdim*(dy2-1) + dx1, src_row = src + src_xdim*sy + sx,
	  first_dst_row = dst + dst_xdim*dy1 + dx1;
	  dst_row >= first_dst_row;
	  dst_row -= dst_xdim, src_row += src_xdim)

	  /*
	   * Copy a single row.
	   */
	  for (srcp = src_row, dstp = dst_row, last_dstp = dst_row + dx2; 
	       dstp < last_dstp;
	       srcp++, dstp++)
	       
	       *dstp = (long) ((*srcp) | ((*srcp)<<8)) | ((*srcp) << 16);
}


/*
 * Zoom by replication (yikes!)
 */
void internal_supersample_8bit_to_16bit(src, src_xdim, src_ydim, sx, sy,
					dst, dst_xdim, dx1, dy1, dx2, dy2, zoom)
     unsigned char	*src;
     int		src_xdim, src_ydim, sx, sy;
     unsigned short	*dst;
     int		dst_xdim, dx1, dy1, dx2, dy2, zoom;
{
     register int		i;
     register unsigned char	*src_row, *srcp;
     register unsigned short	*dst_row, *first_dst_row, *dstp, *last_dstp;
     
     for (dst_row = dst + dst_xdim*(dy2-1) + dx1, src_row = src + src_xdim*sy + sx,
	  first_dst_row = dst + dst_xdim*dy1 + dx1;
	  dst_row >= first_dst_row;
	  dst_row -= dst_xdim * zoom, src_row += src_xdim) {

	  /*
	   * Copy a single row.
	   */
	  for (srcp = src_row, dstp = dst_row, last_dstp = dst_row + dx2; 
	       dstp < last_dstp;
	       srcp++)
	       
	       /*
		* Zoom that row
		*/
	       for (i=0; (i < zoom) && (dstp < last_dstp); i++, dstp++)
		    *dstp = (unsigned short) *srcp;

	  /*
	   * Replicate that row.
	   */
	  for (i=0, dstp = dst_row - dst_xdim;
	       (i < zoom - 1) && (dstp >= first_dst_row);
	       dstp -= dst_xdim, i++)
	       memcpy((char *)dstp, (char *)dst_row, sizeof(unsigned short)*(dx2 - dx1));
     }
}


void internal_supersample_8bit_to_24bit(src, src_xdim, src_ydim, sx, sy,
					dst, dst_xdim, dx1, dy1, dx2, dy2, zoom)
     unsigned char	*src;
     int		src_xdim, src_ydim, sx, sy;
     long		*dst;
     int		dst_xdim, dx1, dy1, dx2, dy2, zoom;
{
     register int		i;
     register unsigned char	*src_row, *srcp;
     register long		*dst_row, *first_dst_row, *dstp, *last_dstp;
     
     for (dst_row = dst + dst_xdim*(dy2-1) + dx1, src_row = src + src_xdim*sy + sx,
	  first_dst_row = dst + dst_xdim*dy1 + dx1;
	  dst_row >= first_dst_row;
	  dst_row -= dst_xdim * zoom, src_row += src_xdim) {

	  /*
	   * Copy a single row.
	   */
	  for (srcp = src_row, dstp = dst_row, last_dstp = dst_row + dx2; 
	       dstp < last_dstp;
	       srcp++)
	       
	       /*
		* Zoom that row
		*/
	       for (i=0; (i < zoom) && (dstp < last_dstp); i++, dstp++)
		    *dstp = (long) ((*srcp) | ((*srcp)<<8)) | ((*srcp) << 16);

	  /*
	   * Replicate that row.
	   */
	  for (i=0, dstp = dst_row - dst_xdim;
	       (i < zoom - 1) && (dstp >= first_dst_row);
	       dstp -= dst_xdim, i++)
	       memcpy((char *)dstp, (char *)dst_row, sizeof(long)*(dx2 - dx1));
     }
}




void internal_subsample_8bit_to_16bit(src, src_xdim, src_ydim, dst, dst_xdim, zoom)
     unsigned char	*src;
     int		src_xdim, src_ydim;
     unsigned short	*dst;
     int		dst_xdim, zoom;
{
     register unsigned char	*src_row, *srcp, *first_src_row, *last_srcp;
     register unsigned short	*dst_row, *dstp;
     
     for (dst_row = dst, src_row = src + src_xdim*(src_ydim-1), first_src_row = src;
	  src_row >= first_src_row;
	  dst_row += dst_xdim, src_row -= src_xdim*zoom)

	  /*
	   * Copy a single row.
	   */
	  for (srcp = src_row, dstp = dst_row, last_srcp = src_row + src_xdim;
	       srcp < last_srcp;
	       srcp+=zoom, dstp++)
	       
	       *dstp = (unsigned short) *srcp;
}


void internal_subsample_8bit_to_24bit(src, src_xdim, src_ydim, dst, dst_xdim, zoom)
     unsigned char	*src;
     int		src_xdim, src_ydim;
     long		*dst;
     int		dst_xdim, zoom;
{
     register unsigned char	*src_row, *srcp, *first_src_row, *last_srcp;
     register long		*dst_row, *dstp;
     
     for (dst_row = dst, src_row = src + src_xdim*(src_ydim-1), first_src_row = src;
	  src_row >= first_src_row;
	  dst_row += dst_xdim, src_row -= src_xdim*zoom)

	  /*
	   * Copy a single row.
	   */
	  for (srcp = src_row, dstp = dst_row, last_srcp = src_row + src_xdim;
	       srcp < last_srcp;
	       srcp+=zoom, dstp++)

	      *dstp = (long) ((*srcp) | ((*srcp)<<8)) | ((*srcp) << 16);
}




typedef unsigned int LucidBitType;

/*
 * No offsets into the bitmap.
 */
void internal_paste_1bit_to_16bit(src, src_xdim, src_ydim,
				  dst, dst_xdim, dx1, dy1, dx2, dy2,
				  zero_value, one_value)
     LucidBitType	*src;					/* Lucid's BIT */
     int		src_xdim, src_ydim;
     unsigned short	*dst;
     int		dst_xdim, dx1, dy1, dx2, dy2;
     int		zero_value, one_value;			/* should be short's but Lucid can't handle them */
     
{
     int		src_bit_xdim, count;
     unsigned short	value, *dstp, *dst_row, *first_dst_row;
     LucidBitType	src_bit, *srcp;

     src_bit_xdim = src_xdim;

     /*
      * Convert src_xdim to BYTE_SIZE.
      */
     src_xdim = (src_xdim / (8 * sizeof(LucidBitType))) + ((src_xdim % (8 * sizeof(LucidBitType))) ? 1 : 0);
     
     for (srcp = src, first_dst_row = dst + dst_xdim*dy1 + dx1, dst_row = dst + dst_xdim*(dy2-1) + dx1;
	  dst_row >= first_dst_row;
	  srcp++, dst_row -= dst_xdim) 

	  /*
	   * Paste one row.
	   */
	  for (count = 0, src_bit = 1, dstp = dst_row; count < src_bit_xdim; count++, src_bit <<= 1, dstp++) {

	       /*
		* Check if we need to increment the srcp.
		*/
	       if (src_bit == 0) { src_bit = 1; srcp++; }
     
	       if (src_bit & (*srcp)) {	                      /* Bit 1 */
		    value = (unsigned short) one_value;
	       } else {                                       /* Bit 0 */
		    value = (unsigned short) zero_value;
	       }

	       *dstp = value;
	  }
}


void internal_paste_1bit_to_24bit(src, src_xdim, src_ydim,
				  dst, dst_xdim, dx1, dy1, dx2, dy2,
				  zero_value, one_value)
     LucidBitType	*src;					/* Lucid's BIT */
     int		src_xdim, src_ydim;
     long		*dst;
     int		dst_xdim, dx1, dy1, dx2, dy2;
     int		zero_value, one_value;			/* should be short's but Lucid can't handle them */
     
{
     int		src_bit_xdim, count;
     long		value, *dstp, *dst_row, *first_dst_row;
     LucidBitType	src_bit, *srcp;

     src_bit_xdim = src_xdim;

     /*
      * Convert src_xdim to BYTE_SIZE.
      */
     src_xdim = (src_xdim / (8 * sizeof(LucidBitType))) + ((src_xdim % (8 * sizeof(LucidBitType))) ? 1 : 0);
     
     for (srcp = src, first_dst_row = dst + dst_xdim*dy1 + dx1, dst_row = dst + dst_xdim*(dy2-1) + dx1;
	  dst_row >= first_dst_row;
	  srcp++, dst_row -= dst_xdim) 

	  /*
	   * Paste one row.
	   */
	  for (count = 0, src_bit = 1, dstp = dst_row; count < src_bit_xdim; count++, src_bit <<= 1, dstp++) {

	       /*
		* Check if we need to increment the srcp.
		*/
	       if (src_bit == 0) { src_bit = 1; srcp++; }
     
	       if (src_bit & (*srcp)) {	                      /* Bit 1 */
		    value = one_value;
	       } else {                                       /* Bit 0 */
		    value = zero_value;
	       }

	       *dstp = value;
	  }
}




void internal_supersample_1bit_to_16bit(src, src_xdim, src_ydim,
					dst, dst_xdim, dx1, dy1, dx2, dy2, zoom,
					zero_value, one_value)
     LucidBitType	*src;					/* Lucid's BIT */
     int		src_xdim, src_ydim;
     unsigned short	*dst;
     int		dst_xdim, dx1, dy1, dx2, dy2, zoom;
     int		zero_value, one_value;			/* should be short's but Lucid can't handle them */
{
     int		src_bit_xdim, count, i;
     unsigned short	value, *dstp, *dst_row, *first_dst_row;
     LucidBitType	src_bit, *srcp;

     src_bit_xdim = src_xdim;

     /*
      * Convert src_xdim to BYTE_SIZE.
      */
     src_xdim = (src_xdim / (8 * sizeof(LucidBitType))) + ((src_xdim % (8 * sizeof(LucidBitType))) ? 1 : 0);
     
     for (srcp = src, first_dst_row = dst + dst_xdim*dy1 + dx1, dst_row = dst + dst_xdim*(dy2-1) + dx1;
	  dst_row >= first_dst_row;
	  srcp++, dst_row -= dst_xdim * zoom) {

	  /*
	   * Paste one row.
	   */
	  for (count = 0, src_bit = 1, dstp = dst_row; count < src_bit_xdim; count++, src_bit <<= 1) {

	       /*
		* Check if we need to increment the srcp.
		*/
	       if (src_bit == 0) { src_bit = 1; srcp++; }
     
	       if (src_bit & (*srcp)) {	                      /* Bit 1 */
		    value = (unsigned short) one_value;
	       } else {                                       /* Bit 0 */
		    value = (unsigned short) zero_value;
	       }

	       /*
		* Zoom that row.
		*/
	       for (i=0; i < zoom; i++, dstp++)
		    *dstp = value;
	  }

	  /*
	   * Replicate that row.
	   */
	  for (i=0, dstp = dst_row - dst_xdim; i < zoom-1; i++, dstp -= dst_xdim)
	       memcpy( (char *)dstp, (char *)dst_row, sizeof(unsigned short)*(dx2 - dx1) );
	     
     }
}



void internal_supersample_1bit_to_24bit(src, src_xdim, src_ydim,
					dst, dst_xdim, dx1, dy1, dx2, dy2, zoom,
					zero_value, one_value)
     LucidBitType	*src;					/* Lucid's BIT */
     int		src_xdim, src_ydim;
     long		*dst;
     int		dst_xdim, dx1, dy1, dx2, dy2, zoom;
     int		zero_value, one_value;			/* should be short's but Lucid can't handle them */
{
     int		src_bit_xdim, count, i;
     long		value, *dstp, *dst_row, *first_dst_row;
     LucidBitType	src_bit, *srcp;

     src_bit_xdim = src_xdim;

     /*
      * Convert src_xdim to BYTE_SIZE.
      */
     src_xdim = (src_xdim / (8 * sizeof(LucidBitType))) + ((src_xdim % (8 * sizeof(LucidBitType))) ? 1 : 0);
     
     for (srcp = src, first_dst_row = dst + dst_xdim*dy1 + dx1, dst_row = dst + dst_xdim*(dy2-1) + dx1;
	  dst_row >= first_dst_row;
	  srcp++, dst_row -= dst_xdim * zoom) {

	  /*
	   * Paste one row.
	   */
	  for (count = 0, src_bit = 1, dstp = dst_row; count < src_bit_xdim; count++, src_bit <<= 1) {

	       /*
		* Check if we need to increment the srcp.
		*/
	       if (src_bit == 0) { src_bit = 1; srcp++; }
     
	       if (src_bit & (*srcp)) {	                      /* Bit 1 */
		    value = one_value;
	       } else {                                       /* Bit 0 */
		    value = zero_value;
	       }

	       /*
		* Zoom that row.
		*/
	       for (i=0; i < zoom; i++, dstp++)
		    *dstp = value;
	  }

	  /*
	   * Replicate that row.
	   */
	  for (i=0, dstp = dst_row - dst_xdim; i < zoom-1; i++, dstp -= dst_xdim)
	       memcpy( (char *)dstp, (char *)dst_row, sizeof(long)*(dx2 - dx1) );
	     
     }
}




void internal_subsample_1bit_to_16bit(src, src_xdim, src_ydim,
				      dst, dst_xdim, zoom,
				      zero_value, one_value)
     LucidBitType	*src;					/* Lucid's BIT */
     int		src_xdim, src_ydim;
     unsigned short	*dst;
     int		dst_xdim, zoom;
     int		zero_value, one_value;			/* should be short's but Lucid can't handle them */
{
     int		src_bit_xdim, count, src_bit_pos, old_src_bit_pos;
     unsigned short	value, *dstp, *dst_row;
     LucidBitType	src_bit, *srcp, *first_src_row, *src_row;

     src_bit_xdim = src_xdim;

     /*
      * Convert src_xdim to BYTE_SIZE.
      */
     src_xdim = (src_xdim / (8 * sizeof(LucidBitType))) + ((src_xdim % (8 * sizeof(LucidBitType))) ? 1 : 0);
     
     for (src_row = src + src_xdim*(src_ydim-1), first_src_row = src, dst_row = dst;
	  src_row >= first_src_row;
	  src_row -= src_xdim*zoom, dst_row += dst_xdim) 

	  /*
	   * Paste one row.
	   */
	  for (count = 0, src_bit = 1, src_bit_pos = 0, dstp = dst_row, srcp = src_row; 
	       count < src_bit_xdim; 
	       count+=zoom, dstp++) {
     
	       if (src_bit & (*srcp)) {	                      /* Bit 1 */
		    value = (unsigned short) one_value;
	       } else {                                       /* Bit 0 */
		    value = (unsigned short) zero_value;
	       }

	       *dstp = value;
	       
	       /*
		* Shift the src_bit and check if we need to increment the srcp.
		*/
	       old_src_bit_pos = src_bit_pos;
	       src_bit_pos = (src_bit_pos + zoom) % (8 * sizeof(LucidBitType));

	       if (src_bit_pos < old_src_bit_pos) {		/* shift into next LucidBitType */
		    srcp++; src_bit = 1;
		    src_bit <<= src_bit_pos;
	       } else {						/* still in the same LucidBitType */
		    src_bit <<= zoom;
	       }
	  }
}



void internal_subsample_1bit_to_24bit(src, src_xdim, src_ydim,
				      dst, dst_xdim, zoom,
				      zero_value, one_value)
     LucidBitType	*src;					/* Lucid's BIT */
     int		src_xdim, src_ydim;
     long		*dst;
     int		dst_xdim, zoom;
     int		zero_value, one_value;			/* should be short's but Lucid can't handle them */
{
     int		src_bit_xdim, count, src_bit_pos, old_src_bit_pos;
     long		value, *dstp, *dst_row;
     LucidBitType	src_bit, *srcp, *first_src_row, *src_row;

     src_bit_xdim = src_xdim;

     /*
      * Convert src_xdim to BYTE_SIZE.
      */
     src_xdim = (src_xdim / (8 * sizeof(LucidBitType))) + ((src_xdim % (8 * sizeof(LucidBitType))) ? 1 : 0);
     
     for (src_row = src + src_xdim*(src_ydim-1), first_src_row = src, dst_row = dst;
	  src_row >= first_src_row;
	  src_row -= src_xdim*zoom, dst_row += dst_xdim) 

	  /*
	   * Paste one row.
	   */
	  for (count = 0, src_bit = 1, src_bit_pos = 0, dstp = dst_row, srcp = src_row; 
	       count < src_bit_xdim; 
	       count+=zoom, dstp++) {
     
	       if (src_bit & (*srcp)) {	                      /* Bit 1 */
		    value = one_value;
	       } else {                                       /* Bit 0 */
		    value = zero_value;
	       }

	       *dstp = value;
	       
	       /*
		* Shift the src_bit and check if we need to increment the srcp.
		*/
	       old_src_bit_pos = src_bit_pos;
	       src_bit_pos = (src_bit_pos + zoom) % (8 * sizeof(LucidBitType));

	       if (src_bit_pos < old_src_bit_pos) {		/* shift into next LucidBitType */
		    srcp++; src_bit = 1;
		    src_bit <<= src_bit_pos;
	       } else {						/* still in the same LucidBitType */
		    src_bit <<= zoom;
	       }
	  }
}





#ifdef OLD

void internal_left_shift_32bit(src, dst, num_bits, size)
     register long	*src, *dst;
     int		num_bits;
     register int	size;
{
     for (; size>0; size--)
	  *dst++ = *src++ << num_bits;
}

void internal_f_into_24bit_lut(im, im_xsz, im_ysz, a, a_xsz, ped, scale, 
			       x_off, y_off, lut, lutsz)
     register float 	*im;
     register double 	scale, ped;
     long 		*a;
     int 		im_xsz, im_ysz, a_xsz, x_off, y_off, lutsz;
     unsigned char 	*lut;
{
     register int 	ceiling = lutsz-1;
     register long 	*a_pos = a+y_off*a_xsz+x_off;
     register float 	*im_pos = im, *im_end = im+im_xsz*im_ysz;
     register int 	temp, xa_pos=0;
     register int 	int_ped = (int) (ped * scale);
     
     while (im_pos<im_end) {
	  if (xa_pos++ == im_xsz) {
	       xa_pos=1;
	       a_pos += a_xsz - im_xsz;
	  }
	  temp = (int) (*im_pos++ * scale + 0.5) - int_ped;
	  *a_pos++ = (long) lut[((temp > 0)?((temp < ceiling)?temp:ceiling):0)];
     }
}

#endif
