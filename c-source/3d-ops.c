/*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: 3d-ops.c
;;;  Author: Ciamac Moallemi <ciamac@media.mit.edu>
;;;  Description: 3-d Operations
;;;  Creation Date: Tue Jun  9 16:26:16 1992
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1992, Vision & Modeling Group,  Media Laboratory,
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#include <math.h>

/* This code does perspective projection of range data */

/* Eventually, will add C code to do texture-mapped triangulated surfaces */

/*
Given a grid of z-values, subsample and use a perspective
projection to generate xy coordinates.  The results are floating
point.
*/
void 
internal_perspective_projection(data, y_dim, x_dim, y_res, x_res, 
				y_step, x_step, matrix)
register float *matrix, *data;
register float *x_res, *y_res;
int x_dim, y_dim, x_step, y_step;
{
  register float z, x1, y1, z1;
  register int x, y, pos;
    
  for (y = 0; y < y_dim; y += y_step)
    {
      for (x = 0, pos = y * x_dim; 
	   x < x_dim; 
	   x += x_step, pos += x_step, x_res++, y_res++)
	{
	  z = data[pos];
	  x1 = x * matrix[0] + y * matrix[1] + z * matrix[2] + matrix[3];
	  y1 = x * matrix[4] + y * matrix[5] + z * matrix[6] + matrix[7];
	  z1 = x * matrix[8] + y * matrix[9] + z * matrix[10] + matrix[11];
	  if (z1 == 0.0) 
	    {
	      *x_res = 0.0;
	      *y_res = 0.0;
	    }
	  else
	    {
	      *x_res = x1 / z1;
	      *y_res = y1 / z1;
	    }
	}
    }
}

/*
Given a grid of z-values, subsample and use a perspective
projection to generate xy coordinates.  The results are
truncated to integers.
*/
void 
internal_perspective_projection_int(data, y_dim, x_dim, y_res, x_res, 
				    y_step, x_step, matrix)
register float *matrix, *data;
register int *x_res, *y_res;
int x_dim, y_dim, x_step, y_step;
{
  register float z, x1, y1, z1;
  register int x, y, pos;
    
  for (y = 0; y < y_dim; y += y_step)
    {
      for (x = 0, pos = y * x_dim; 
	   x < x_dim; 
	   x += x_step, pos += x_step, x_res++, y_res++)
	{
	  z = data[pos];
	  x1 = x * matrix[0] + y * matrix[1] + z * matrix[2] + matrix[3];
	  y1 = x * matrix[4] + y * matrix[5] + z * matrix[6] + matrix[7];
	  z1 = x * matrix[8] + y * matrix[9] + z * matrix[10] + matrix[11];
	  if (z1 == 0.0) 
	    {
	      *x_res = 0;
	      *y_res = 0;
	    }
	  else
	    {
	      *x_res = (int) x1 / z1;
	      *y_res = (int) y1 / z1;
	    }
	}
    }
}

/*
Given a grid of z-values, subsample and use an orthographic
projection to generate xy coordinates.  The result are floating
point.
*/
void 
internal_orthographic_projection(data, y_dim, x_dim, y_res, x_res, 
				 y_step, x_step, matrix)
register float *matrix, *data;
register float *x_res, *y_res;
int x_dim, y_dim, x_step, y_step;
{
  register float z;
  register int y, x, pos;

  for (y = 0; y < y_dim; y += y_step)
    {
      for (x = 0, pos = y * x_dim; 
	   x < x_dim; 
	   x += x_step, pos += x_step, x_res++, y_res++)
	{
	  z = data[pos];
	  *x_res = x * matrix[0] + y * matrix[1] + z * matrix[2] + matrix[3];
	  *y_res = x * matrix[4] + y * matrix[5] + z * matrix[6] + matrix[7];
	}
    }    
}

/*
Given a grid of z-values, subsample and use an orthographic
projection to generate xy coordinates.  The results are
truncated to integers.
*/
void 
internal_orthographic_projection_int(data, y_dim, x_dim, y_res, x_res, 
				     y_step, x_step, matrix)
register float *matrix, *data;
register int *x_res, *y_res;
int x_dim, y_dim, x_step, y_step;
{
  register float z;
  register int y, x, pos;

  for (y = 0; y < y_dim; y += y_step)
    {
      for (x = 0, pos = y * x_dim; 
	   x < x_dim; 
	   x += x_step, pos += x_step, x_res++, y_res++)
	{
	  z = data[pos];
	  *x_res = (int) x * matrix[0] + y * matrix[1] + z * matrix[2] + matrix[3];
	  *y_res = (int) x * matrix[4] + y * matrix[5] + z * matrix[6] + matrix[7];
	}
    }    
}

