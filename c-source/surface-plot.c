/*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: surface.c
;;;  Author: Ciamac Moallemi <ciamac@media.mit.edu>
;;;  Description: Surface plotting routines
;;;  Creation Date: Mon Jun  1 21:32:30 1992
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1992, Vision & Modeling Group,  Media Laboratory,
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#include <stdio.h>
#include <math.h>
#include <memory.h>

/*
The algorithm to remove hidden lines is not very clean at all.
It is adapted from Foley and Van Dam.  Ideally we would want to do
this with arbitrary floating point structures, but this would be
very slow and gross.  Thus, the rendering is done in a pixellated
fashion, which may lead to aliasing errors.  One solution would
be to simply make sure the x-dim and y-dim of the pane/frob
you are rendering to are large enough.
*/

/*
Because of we are pixelating, we must actually render lines,
thus we use Bresenham's alogorithm (see Foley & Van Dam). In 
a global sense this is kind of gross, as we render the lines once
then the graphics device renders the lines again.  Alas, this
is the only clean way to do this without breaking significant
abstraction barriers.
*/

#define FALSE 0
#define TRUE  1

#define abs(x)    ((x) >= 0 ? (x) : -(x))
#define sgn(x)    ((x) > 0 ? 1 : ((x) < 0 ? -1 : 0))
#define max(a, b) (a >= b ? a : b)
#define min(a, b) (b >= a ? a : b)


/* 
A structure to hold data required to compute the next pixel in 
the line.
*/
struct line 
{
  int y0, x0, y1, x1;
  int dy, sy, dx, sx;
  int small_slope;
  int e, e_inc, e_noinc;
  double m, b; /* used for left/right of line calculations */
};

/*
Initialize the line structure for a given line.  Note that the order
of drawing of the line may be flipped.
*/
static void
start_line (l, y0, x0, y1, x1)
struct line *l;
int y0, x0, y1, x1;
{
  int dx, dy;
  
  dy = y1 - y0;
  l->dy = abs(dy);
  dx = x1 - x0;
  l->dx = abs(dx);
  l->small_slope = l->dx > l->dy;

  if (dy != 0)
    {
      l->m = ((float) dx) / ((float) dy); /* 1 / slope */
      l->b = ((float) x0) - ((float) y0) * l->m;
    }
  
  if (l->small_slope) /* |slope| < 1 */
    {
      /* always increment in x */
      if (dx < 0)
	{
	  l->y0 = y1; l->x0 = x1;
	  l->y1 = y0; l->x1 = x0;
	  dy = - dy;
	}
      else
	{
	  l->y0 = y0; l->x0 = x0;
	  l->y1 = y1; l->x1 = x1;
	}
      l->sy = sgn(dy);
      l->e_noinc = 2 * l->dy;
      l->e = l->e_noinc - l->dx;
      l->e_inc = l->e - l->dx;
    }
  else
    {
      /* always increment in y */
      if (dy < 0)
	{
	  l->y0 = y1; l->x0 = x1;
	  l->y1 = y0; l->x1 = x0;
	  dx = - dx;
	}
      else
	{
	  l->y0 = y0; l->x0 = x0;
	  l->y1 = y1; l->x1 = x1;
	}
      l->sx = sgn(dx);
      l->e_noinc = 2 * l->dx;
      l->e = l->e_noinc - l->dy;
      l->e_inc = l->e - l->dy;
    }

}


/*
If xy is the current point, generate the next point on the line.
Returns 0 if there is no next point.
*/
static int
next_point(l, x, y)
struct line *l;
int *x, *y;
{
  
  if (l->small_slope)
    {
      
      if (*x == l->x1)
	return(0); /* no more points */
      (*x)++;
      
      if (l->e < 0)
	l->e += l->e_noinc;
      else
	{
	  (*y) += l->sy;
	  l->e += l->e_inc;
	}
    }
  else
    {
      
      if (*y == l->y1)
	return(0); /* no more points */
      (*y)++;

      if (l->e < 0)
	l->e += l->e_noinc;
      else
	{
	  (*x) += l->sx;
	  l->e += l->e_inc;
	}
    }

  return(1);
}

/*
Is xy to the left of l?  In a right-handed coordinate system.
*/
static int
is_left(l, x, y)
struct line *l;
int x, y;
{
  if (l == NULL)
    return(TRUE);
  if (l->x0 == l->x1)
    return(x <= l->x0);
  if (l->y0 == l->y1)
    return(y <= l->y0);
  return(x <= y * l->m + l->b);
}

/*
Several macros to speed up hidden line removal.
*/

/*
Add a given line to the list of lines drawn.  If we exceed te size
of the list, no more lines will be drawn.  An error should probably be
signalled, however.
*/
#define plot_line(y0, x0, y1, x1)  {  if (*num_lines < max_lines) \
					{ y0_line_buf[*num_lines] = y0; \
					  x0_line_buf[*num_lines] = x0; \
					  y1_line_buf[*num_lines] = y1; \
					  x1_line_buf[*num_lines] = x1; \
					  (*num_lines)++; } }
/*
Macros that deal with the hidden line buffers.
*/
#define hidden_bound(x)       (x < 0 ? 0 : x >= hidden_buf_size ? \
			       hidden_buf_size - 1 : x)
#define hidden_low(x)         (hidden_low_buf[x])
#define prev_low(x)           (prev_low_buf[x])
#define set_hidden_low(x, y)  {  if (y < hidden_low_res[x] || \
				     hidden_low_res[x] < 0) \
				       hidden_low_res[x] = y; }
#define raw_hidden_low(x, y)  {  hidden_low_res[x] = y; }
#define hidden_high(x)        (hidden_high_buf[x])
#define prev_high(x)           (prev_high_buf[x])
#define set_hidden_high(x, y) {  if (y > hidden_high_res[x] || \
				     hidden_high_res[x] < 0) \
				   hidden_high_res[x] = y; }
#define raw_hidden_high(x, y) {  hidden_high_res[x] = y; }

/* 
Plot a line after removing is hidden part(s).
*/
static void 
generate_line(l, prev_l,
	      hidden_low_buf, hidden_high_buf,
	      prev_low_buf, prev_high_buf,
	      hidden_low_res, hidden_high_res, hidden_buf_size,
	      y0_line_buf, x0_line_buf,
	      y1_line_buf, x1_line_buf,
	      num_lines, max_lines, first_line)
struct line *l, *prev_l;
int *hidden_low_buf, *hidden_high_buf;
int *prev_low_buf, *prev_high_buf;
int *hidden_low_res, *hidden_high_res;
int *y0_line_buf, *x0_line_buf, *y1_line_buf, *x1_line_buf;
int *num_lines;
int max_lines, first_line, hidden_buf_size;
{
  int x, y;
  register int xh;
  register int visible_above, visible_below, prev_above, prev_below;
  register int last_y, last_x, prev_x, prev_y;
  register float *m, *b;

  /* generate a line */
  x = l->x0;
  y = l->y0;

  /* for the first line, no removal is needed */
  if (first_line)
    {
      do
	{
	  xh = hidden_bound(x);
	  raw_hidden_low(xh, y);
	  raw_hidden_high(xh, y);
	}
      while (next_point(l, &x, &y));
      plot_line(l->y0, l->x0, l->y1, l->x1);
      return;
    }

  /* core removal stuff */
  /* 
    Complex series of logic, continually renders the line,
    updates a buffer, and looks at two hidden line buffers
    and decides which parts of the line should be drawn 
    the multi-buffer logic is homebrew, but it works 99.9% of the 
    time, much better than the buggy garbage described in 
    Foley and Van Dam.
  */

  /*
    Note that we check line orientation at every pixel.  Perhaps
    with more accounting this would not be necessary.
  */

  xh = hidden_bound(x);

  if (hidden_high(xh) < 0)
    visible_above = TRUE;
  else if (y >= hidden_high(xh))
    visible_above = TRUE;
  else if (y < prev_high(xh) && prev_high(xh) >= 0)
    visible_above = FALSE;
  else
    visible_above = is_left(prev_l, x, y);

  if (hidden_low(xh) < 0)
    visible_below = TRUE;
  else if (y <= hidden_low(xh))
    visible_below = TRUE;
  else if (y > prev_low(xh) && prev_low(xh) >= 0)
    visible_below = FALSE;
  else
    visible_below = is_left(prev_l, x, y);

  prev_above = visible_above;
  prev_below = visible_below;
  
  if (visible_above || visible_below)
    {
      if (visible_above && visible_below)
	visible_below = FALSE;
      last_x = x;
      last_y = y;
    }

  prev_x = x;
  prev_y = y;

  do 
    {
      xh = hidden_bound(x);

      if (hidden_high(xh) < 0)
	visible_above = TRUE;
      else if (y >= hidden_high(xh))
	visible_above = TRUE;
      else if (y <= prev_high(xh) && prev_high(xh) >= 0)
	visible_above = FALSE;
      else
	visible_above = is_left(prev_l, x, y);
      
      if (hidden_low(xh) < 0)
	visible_below = TRUE;
      else if (y <= hidden_low(xh))
	visible_below = TRUE;
      else if (y >= prev_low(xh) && prev_low(xh) >= 0)
	visible_below = FALSE;
      else
	visible_below = is_left(prev_l, x, y);

      if (visible_below && visible_above)
	visible_below = FALSE;
      
      if (visible_above)
        {
          if (prev_above)
	    {
	      set_hidden_high(xh, y);
	    }
	  else if (prev_below)
            {
	      plot_line(last_y, last_x, prev_y, prev_x);
	      last_y = y;
	      last_x = x;
            }
          else /* prev_hidden */
	    {
	      last_y = y;
	      last_x = x;
	    }
        }
      else if (visible_below)
        {
	  if (prev_below) 
	    {
	      set_hidden_low(xh, y);
	    }
	  else if (prev_above)
            {
	      plot_line(last_y, last_x, prev_y, prev_x);
	      last_y = y;
	      last_x = x;
            }
          else /* prev_hidden */
	    {
	      last_y = y;
	      last_x = x;
	    }
        }
      else /* hidden */
	{
          if (prev_above)
	    {
	      plot_line(last_y, last_x, prev_y, prev_x);
	    }
	  else if (prev_below)
	    {
	      plot_line(last_y, last_x, prev_y, prev_x);
	    }
	  
        }

      prev_below = visible_below;
      prev_above = visible_above;

      prev_y = y;
      prev_x = x;

    }
  while (next_point(l, &x, &y));
  
  /* final line */
  if (prev_above)
    {
      plot_line(last_y, last_x, prev_y, prev_x);
    }
  else if (prev_below)
    {
      plot_line(last_y, last_x, prev_y, prev_x);
    }
}


/* macro to draw the line */
#define gen_line(l, prev_l, first_line) generate_line( \
				l, prev_l, \
				hidden_low_buf, hidden_high_buf, \
				prev_low_buf, prev_high_buf, \
				hidden_low_res, hidden_high_res,  \
				hidden_buf_size, \
				y0_line_buf, x0_line_buf, \
				y1_line_buf, x1_line_buf, \
				&num_lines, max_lines, first_line)

/*
Calculate the surface lines to be drawn, removing hidden ones.
Returns the number of lines drawn.
*/
int
internal_compute_surface(y_data, x_data, y_data_dim, x_data_dim,
			 hidden_low_buf, hidden_high_buf, hidden_buf_size,
			 y0_line_buf, x0_line_buf, 
			 y1_line_buf, x1_line_buf, 
			 max_lines)
register float *y_data, *x_data;
int y_data_dim, x_data_dim;
register int *hidden_low_buf, *hidden_high_buf;
int *y0_line_buf, *x0_line_buf, *y1_line_buf, *x1_line_buf;
int max_lines, hidden_buf_size;
{
  register int data_cnt, temp, buf_start, buf_end;
  register int *hidden_low_res, *hidden_high_res;
  register int *prev_low_buf, *prev_high_buf;
  register int y_val, prev_cnt;
  struct line line_1, line_2;
  register struct line *l, *prev_l, *temp_l;
  register float x_data1, x_data2, x_data3;
  int num_lines = 0;

#ifdef DEBUG
  fprintf (stderr, "x-dim %d y-dim %d buffer-size %d\n", 
	   x_data_dim, y_data_dim, hidden_buf_size);
#endif


  if (y_data_dim < 1) 
    return(0); /* nothing to do */

  /* initialize horizon buffers */
  hidden_low_res = &hidden_low_buf[hidden_buf_size];
  prev_low_buf = &hidden_low_buf[2 * hidden_buf_size];
  hidden_high_res = &hidden_high_buf[hidden_buf_size];
  prev_high_buf = &hidden_high_buf[2 * hidden_buf_size];
  /* clear all buffers */
  for (data_cnt = 0; data_cnt < 3 * hidden_buf_size; data_cnt++)
    hidden_low_buf[data_cnt] = hidden_high_buf[data_cnt] = -1;
  
  /* first line */
  l = &line_1;
  for (data_cnt = 0; data_cnt < x_data_dim - 1; data_cnt++)
    {
      start_line(l, (int) y_data[data_cnt], (int) x_data[data_cnt],
		 (int) y_data[data_cnt + 1], (int) x_data[data_cnt + 1]);
      
      /* forward-line */
      gen_line(l, NULL, TRUE);
    }

#ifndef NO_MEMCPY
  memcpy(hidden_low_buf, hidden_low_res, hidden_buf_size * sizeof(int));
  memcpy(prev_low_buf, hidden_low_res, hidden_buf_size * sizeof(int));
  memcpy(hidden_high_buf, hidden_high_res, hidden_buf_size * sizeof(int));  
  memcpy(prev_high_buf, hidden_high_res, hidden_buf_size * sizeof(int));
#else
  for (data_cnt = 0; data_cnt < hidden_buf_size; data_cnt++)
    {
      prev_low_buf[data_cnt] = hidden_low_buf[data_cnt] = hidden_low_res[data_cnt];
      prev_high_buf[data_cnt] = hidden_high_buf[data_cnt] = hidden_high_res[data_cnt];
    }
#endif

  for (y_val = 1; y_val < y_data_dim; y_val++)
    {
      l = &line_1;
      prev_l = &line_2;

      /* initial down-line */
      data_cnt = y_val * x_data_dim;
      prev_cnt = (y_val - 1) * x_data_dim;
      start_line(prev_l, (int) y_data[prev_cnt], (int) x_data[prev_cnt],
		 (int) y_data[data_cnt], (int) x_data[data_cnt]);
      gen_line(prev_l, NULL, FALSE);
      
      for (data_cnt = y_val * x_data_dim + 1,
	   prev_cnt = (y_val - 1) * x_data_dim + 1;
	   data_cnt < (y_val + 1) * x_data_dim;
	   data_cnt++, prev_cnt++)
	{

	  /* forward-line */	  
	  start_line(l, (int) y_data[data_cnt-1], (int) x_data[data_cnt-1],
		     (int) y_data[data_cnt], (int) x_data[data_cnt]);
	  gen_line(l, prev_l, FALSE);
	  
	  /* down-line */
	  start_line(l, (int) y_data[prev_cnt], (int) x_data[prev_cnt],
		     (int) y_data[data_cnt], (int) x_data[data_cnt]);
	  gen_line(l, prev_l, FALSE);

	  /* swap */
	  temp_l = prev_l;
	  prev_l = l;
	  l = temp_l;
 
	  /* keep horizon buffers up to date */

	  /* we assign the elements to a temporary variable because
	   * the SGI cc compiler can't handle the expression tree depth
	   * -PT
	   */
	  x_data1 = x_data[prev_cnt];
	  x_data2 = x_data[data_cnt-1];
	  x_data3 = x_data[data_cnt];
	  
	  buf_start = hidden_bound(min(x_data1, min(x_data2, x_data3)));
	  buf_end = hidden_bound(max(x_data1, max(x_data2, x_data3)));

/*
	  buf_start = hidden_bound(min(x_data[prev_cnt],
				       min(x_data[data_cnt-1], x_data[data_cnt])));
	  buf_end = hidden_bound(max(x_data[prev_cnt],
				     max(x_data[data_cnt-1], x_data[data_cnt])));
*/
#ifndef NO_MEMCPY
	  memcpy(&hidden_low_buf[buf_start], &hidden_low_res[buf_start], 
		 (buf_end - buf_start + 1) * sizeof(int));
	  memcpy(&hidden_high_buf[buf_start], &hidden_high_res[buf_start], 
		 (buf_end - buf_start + 1) * sizeof(int));
#else
	  for(temp = buf_start; temp <= buf_end; temp++)
	    {
	      hidden_low_buf[temp] = hidden_low_res[temp];
	      hidden_high_buf[temp] = hidden_high_res[temp];
	    }	     
#endif
	}

#ifndef NO_MEMCPY
      memcpy(prev_low_buf, hidden_low_buf, hidden_buf_size * sizeof(int));
      memcpy(prev_high_buf, hidden_high_buf, hidden_buf_size * sizeof(int));    
#else
      for(temp = 0; temp < hidden_buf_size; temp++)
	{
	  prev_low_buf[temp] = hidden_low_buf[temp];
	  prev_high_buf[temp] = hidden_high_buf[temp];
	}
#endif

/* code may break with less than one line */
#ifdef NOT
      /* INCORRECT */
      /* keep horizon buffers up to date */
      buf_start = hidden_bound(min(x_data[prev_cnt],
				   min(x_data[data_cnt-1], x_data[data_cnt])));
      buf_end = hidden_bound(min(x_data[prev_cnt],
				 min(x_data[data_cnt-1], x_data[data_cnt])));
      /* maybe use a copy?? */
      for(temp = buf_start; temp <= buf_end; temp++)
	{
	  hidden_low_buf[temp] = hidden_low_res[temp];
	  hidden_low_buf[temp] = hidden_low_res[temp];
	}
#endif
      
    }

#ifdef DEBUG
  fprintf (stderr, "num-lines %d max-lines %d\n", num_lines, max_lines);
#endif

  return(num_lines);
}

