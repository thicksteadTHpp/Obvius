/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: color.c
;;;  Author: EJ Chichilnisky
;;;  Description: Color specific operations that must be fast.
;;;  Creation Date: Fall 1992
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#include <math.h>


#define CIE_CUTOFF 0.008856

/* Wyszecki & Stiles, p. 828 */
int internal_cieluv(xyz,w,result)
float *xyz,*w,*result;
{
  float L,u,v,X,Y,Z,Xn,Yn,Zn,un,vn,d,dn;

  X = xyz[0];
  Y = xyz[1];
  Z = xyz[2];

  Xn = w[0];
  Yn = w[1];
  Zn = w[2];

  L = Y/Yn;
  L =  (L > CIE_CUTOFF) ? 116.0*pow(L,0.33333333333) - 16.0 : L * 903.3 ;

  d = X + 15.0*Y + 3.0*Z;
  u = d ? 4.0*X / d : 0.0;
  v = d ? 9.0*Y / d : 0.0;
  dn = Xn + 15.0*Yn + 3.0*Zn;
  un = dn ? 4.0*Xn / dn : 0.0;
  vn = dn ? 9.0*Yn / dn : 0.0;

  result[0] = L;
  result[1] = 13.0 * L * (u - un);
  result[2] = 13.0 * L * (v - vn);
  
  return(0);
}

int internal_cieluv_rows(xyz,w,result,rows)
float *xyz,*w,*result;
int rows;
{
  register int i;

  for (i=0;i<rows;i++) {
    internal_cieluv(xyz,w,result);
    xyz += 3;
    result += 3;
  }
  return(0);
}



int internal_cielab_rows(xyz,w,result,rows)
float *xyz,*w,*result;
int rows;
{
  register int i;

  for (i=0;i<rows;i++) {
    internal_cielab(xyz,w,result);
    xyz += 3;
    result += 3;
  }
  return(0);
}

/* Wyszecki & Stiles, p. 828 */
int internal_cielab(xyz,w,result)
float *xyz,*w,*result;
{
  float L,a,b,Xn,Yn,Zn;

  Xn = xyz[0]/w[0];
  Yn = xyz[1]/w[1];
  Zn = xyz[2]/w[2];

  L = Yn;
  L = (L > CIE_CUTOFF) ? 116.0*pow(L,0.33333333333) - 16.0 : L * 903.3;

  Yn = (Yn > CIE_CUTOFF) ? pow(Yn,0.33333333) : 7.787 * Yn + 0.13793;
  Xn = (Xn > CIE_CUTOFF) ? pow(Xn,0.33333333) : 7.787 * Xn + 0.13793;
  Zn = (Zn > CIE_CUTOFF) ? pow(Zn,0.33333333) : 7.787 * Zn + 0.13793;

  result[0] = L;
  result[1] = 500.0 * (Xn - Yn);
  result[2] = 200.0 * (Yn - Zn);
  
  return(0);
}






