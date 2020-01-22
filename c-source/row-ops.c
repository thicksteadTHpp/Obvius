#include <stdio.h>
#include <math.h>
/*
ROUTINES: internal_row_add internal_row_sub internal_row_sum internal_row_swap
          internal_dot_product
	  internal_almost_equal internal_sc_almost_equal, internal_scalar_multiple,
	  internal_matrix_mul,internal_matrix_mul_transpose,
	  internal_matrix_transpose_mul,internal_matrix_transpose
AUTHOR:	Chichilnisky

PURPOSE: Speed up matrix calculations.
HISTORY:
Originally, matrix-mul and variants were not implemented 
the most efficient way (too many multiplications in indexing.)
because it was a pain in the ass to do it right.
These old versions are left commented out since the newer ones
are vecry ugly, though fast.

*/


float internal_dot_product(arr1,arr2,length)
float *arr1,*arr2;
int length;
{
  register int i;
  float tmp=0.0;

  for (i=length;i;i--) {
    tmp += (*arr1++) * (*arr2++);
  }

  return(tmp);
}

int internal_scalar_multiple(arr1,arr2,ratio,length,tolerance)
float *arr1,*arr2;
float ratio;
int length;
float tolerance;
{
  register int i;
  for (i=length;i;i--) {
    if ( fabs((*arr1++) - (ratio * (*arr2++))) > tolerance)
      return(1);
  }
  return(0);
}

int internal_almost_equal(arr1,arr2,length,tolerance)
float *arr1,*arr2;
int length;
float tolerance;
{
  register int i;

  for (i=length;i;i--) {
    if ( fabs((*arr1++) - (*arr2++)) > tolerance)
      return(1);
  }
  return(0);
}

int internal_sc_almost_equal(arr,val,length,tolerance)
float *arr;
float val;
int length;
float tolerance;
{
  register int i;
  
  for (i=length;i;i--) {
    if (fabs( (*arr++) - val) > tolerance)
      return(1);
  }
  return(0);
}


/* Make it easy (but slow) to index matrices */
/* If you are feeling gutsy, you can avoid lots of multiplications */
#define INDEX(array,i,j,col) (*(array + ((i)*(col)) + (j)))


int internal_row_swap(array,row1,row2,cols)
float *array;
int row1,row2,cols;
{
  register float *p,*q,tmp;
  register int col;

  p = array + row1*cols;
  q = array + row2*cols;

  for (col=cols;col;col--) {
    tmp = *p;
    *p++ = *q;
    *q++ = tmp;
  }
  return(0);
}

int internal_row_add(src_array,vector,dest_array,rows,cols)
float *src_array,*dest_array,*vector;
int rows,cols;
{
  register int row,col;
  register float *p;

  for (row=rows;row;row--) {
    for (col=cols,p=vector;col;col--) {
      *dest_array++ = *src_array++ + *p++;
    }
  }
  return(0);
}

int internal_row_sub(src_array,vector,dest_array,rows,cols)
float *src_array,*dest_array,*vector;
int rows,cols;
{
  register int row,col;
  register float *p;

  for (row=rows;row;row--) {
    for (col=cols,p=vector;col;col--) {
      *dest_array++ = *src_array++ - *p++;
    }
  }
  return(0);
}

int internal_row_sum(array,vector,rows,cols)
float *array,*vector;
int rows,cols;
{
  register int row,col;
  register float *p;

  for (col=cols,p=vector;col;col--) {
    *p++ = 0.0;
  }
  for (row=rows;row;row--) {
    for (col=cols,p=vector;col;col--) {
      *p++ += *array++;
    }
  }
  return(0);
}

int internal_col_sum(array,vector,rows,cols)
float *array,*vector;
int rows,cols;
{
  register int row,col;
  register float *p;

  for (row=rows,p=vector;row;row--) {
    *p++ = 0.0;
  }
  for (row=rows,p=vector;row;row--,p++) {
    for (col=cols;col;col--) {
      *p += *array++;
    }
  }
  return(0);
}


int internal_matrix_transpose(m1,r,c,m2)
float *m1,*m2;
int r,c;
{
  register int i,j;
  float tmp;

  if (m1==m2) {
    for(i = 0; i < r; i++) {
      for(j = 0; j < i; j++) {
	tmp = INDEX(m1,j,i,c);
	INDEX(m2,j,i,r) = INDEX(m1,i,j,c);
	INDEX(m2,i,j,r) = tmp;
      }
    }
  }
  else {
    for(i = 0; i < r; i++) {
      for(j = 0; j < c; j++) {
	INDEX(m2,j,i,r) = INDEX(m1,i,j,c);
      }
    }
  }
  return(0);
}



/* Matrix multiplication code speeded up a 11.11.91.
   The old code works fine but does lots of extra multiplication,
   since the new code is uglier, the old is kept below
   in case problems creep in. EJC */

int internal_matrix_mul(m1,r1,c1,m2,r2,c2,m3,r3,c3)
float *m1,*m2,*m3;
int r1,c1,r2,c2,r3,c3;
{
  register int i,j,k;
  register float *p1,*p2,*p3;
  register int off1;

  p3 = m3;
  for (i = r1,off1=0; i ;off1+=c1, i--){
    for (j = 0; j<c2 ; j++){
      p1 = m1 + off1;
      p2 = m2 + j;
      for (k = c1,*p3=0.0; k ; k--) {
	*p3 += *p1++ * *p2;
	p2 += c2;
      }
      *p3++;
    }
  }
  return(0);
}



int internal_matrix_mul_transpose(m1,r1,c1,m2,r2,c2,m3,r3,c3)
float *m1,*m2,*m3;
int r1,c1,r2,c2,r3,c3;
{
  register int i,j,k;
  register float *p1,*p2,*p3;
  register int off1,off2;

  p3 = m3;
  for (i = r1,off1=0; i ;off1+=c1, i--){
    for (j = r2,off2=0; j ; off2+=c2, j--){
      p1 = m1 + off1;
      p2 = m2 + off2;
      for (k = c1,*p3=0.0; k ; k--) {
	*p3 += *p1++ * *p2++;
      }
      *p3++;
    }
  }
  return(0);
}




int internal_matrix_transpose_mul(m1,r1,c1,m2,r2,c2,m3,r3,c3)
float *m1,*m2,*m3;
int r1,c1,r2,c2,r3,c3;
{
  register int i,j,k;
  register float *p1,*p2,*p3;

  p3 = m3;
  for (i = 0; i<c1 ; i++){
    for (j = 0; j<c2 ; j++){
      p1 = m1 + i;
      p2 = m2 + j;
      for (k = r1,*p3=0.0; k ; k--) {
	*p3 += *p1 * *p2;
	p1 += c1;
	p2 += c2;
      }
      *p3++;
    }
  }
  return(0);
}


/************* Old matrix multiplication code *************/
/*
int internal_matrix_mul(m1,r1,c1,m2,r2,c2,m3,r3,c3)
float *m1,*m2,*m3;
int r1,c1,r2,c2,r3,c3;
{
  register int i,j,k;
  float tmp;

  for (i = 0; i < r3; i++){
    for (j = 0; j < c3; j++){
      for (k = 0,tmp=0.0; k < c1; k++) {
	tmp += INDEX(m1,i,k,c1) * INDEX(m2,k,j,c2);
      }
      INDEX(m3,i,j,c3) = tmp;
    }
  }
  return(0);
}


int internal_matrix_transpose_mul(m1,r1,c1,m2,r2,c2,m3,r3,c3)
float *m1,*m2,*m3;
int r1,c1,r2,c2,r3,c3;
{
  register int i,j,k;
  float tmp;

  for (i = 0; i < r3; i++){
    for (j = 0; j < c3; j++){
      for(k = 0,tmp=0.0; k < r1; k++) { 
	tmp += INDEX(m1,k,i,c1) * INDEX(m2,k,j,c2);
      }
      INDEX(m3,i,j,c3) = tmp;
    }
  }
  return(0);
}

int internal_matrix_mul_transpose(m1,r1,c1,m2,r2,c2,m3,r3,c3)
float *m1,*m2,*m3;
int r1,c1,r2,c2,r3,c3;
{
  register int i,j,k;
  float tmp;

  for (i = 0; i < r3; i++){
    for (j = 0; j < c3; j++){
      for(k = 0,tmp=0.0; k < c1; k++) { 
	tmp += INDEX(m1,i,k,c1) * INDEX(m2,j,k,c2);
      }
      INDEX(m3,i,j,c3) = tmp;
    }
  }
  return(0);
}
*/
