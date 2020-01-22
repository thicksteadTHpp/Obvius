/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: gl-obv-draw.c
;;;  Author: Patrick C. Teo
;;;  Description: Routines to draw multiple GL primitives 
;;;  Creation Date: 1993
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

/*#include <gl/gl.h>*/

#define INFINITY  1000000

void internal_draw_lines(y0, x0, y1, x1, y_offset, x_offset, nlines)
     long	*y0, *x0;
     long	*y1, *x1;
     int	y_offset, x_offset;
     int	nlines;
{
     int  count=0;
     int  first_time = 1;
     long vert_start[2], vert_end[2];

     vert_end[0] = vert_end[1] = -INFINITY;

     while (nlines > 0) {
	  vert_start[0] = *x0 + x_offset; vert_start[1] = *y0 + y_offset;
	  if ((vert_start[0] == vert_end[0]) && (vert_start[1] == vert_end[1])) {
	       vert_end[0] = *x1 + x_offset; vert_end[1] = *y1 + y_offset;

	       /*
		* Don't bother with points.
		*/
	       if (!((vert_start[0] == vert_end[0]) && (vert_start[1] == vert_end[1])))
		    v2i(vert_end);

	  } else {
	       /*
		* End previous line.
		*/
	       if (first_time)
		    first_time = 0;
	       else
		    endline();

	       /*
		* Start a new line.
		*/
	       vert_end[0] = *x1 + x_offset; vert_end[1] = *y1 + y_offset;

	       bgnline();
	       v2i(vert_start);

	       /*
		* Don't bother with points.
		*/
	       if (!((vert_start[0] == vert_end[0]) && (vert_start[1] == vert_end[1])))
		    v2i(vert_end);
	  }
	  nlines--; y0++; x0++; y1++; x1++;
     }

     /*
      * End last line.
      */
     endline();
}
	  

	       


void internal_draw_circles(y_origin, x_origin, radius, fill_p, ncircles)
     long	*y_origin, *x_origin;
     int	radius, fill_p, ncircles;
{
     if (fill_p) 				/* filled circles */
	  for (; ncircles > 0; ncircles--, y_origin++, x_origin++)
	       circfi(*x_origin, *y_origin, radius);
     else					/* empty circles */
	  for (; ncircles > 0; ncircles--, y_origin++, x_origin++)
	       circi(*x_origin, *y_origin, radius);
}


void internal_draw_rects(y0, x0, y1, x1, fill_p, nrects)
     long	*y0, *x0;
     long	*y1, *x1;
     int	fill_p, nrects;
{
     if (fill_p) 				/* filled rectangles */
	  for (; nrects > 0; nrects--, y0++, x0++, y1++, x1++)
	       rectfi(*x0, *y0, *x1, *y1);
     else					/* empty rectangles */
	  for (; nrects > 0; nrects--, y0++, x0++, y1++, x1++)
	       recti(*x0, *y0, *x1, *y1);
}

