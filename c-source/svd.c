#include <stdio.h>
#include <ctype.h>
#include <math.h>

/*
ROUTINE: internal_svd
AUTHOR:	Chichilnisky
PURPOSE:
  Compute the singular value decomposition A = U S Vt.
  U and V are unitary matrices.  
  S is a diagonal matrix with columns all positive 
  and decreasing in value.  
  The data are passed and returned in transposed form.
  Hence, pass in Xt and get back Ut, S, and Vt.
  Note that Xt gets destroyed by the routine.

  X = row by col
  U = row by row  (rotation)
  S = a vector of size = min(row,col)
  V = col by col  (rotation)

HISTORY:
  8.14.92   Put the temporary memory allocation into Lisp,
            since mallocs can cause a GC to happen and bust arrays.
  12.23.91  Found out that in fact the Linpack documentation says that
            dsvdc_ actually wants MIN(rows+1,cols) elements for the
	    singular values array.
  12.19.91  Bug found: dsvdc_ overwrites the end of the S array by one element.
            This was found by passing stuff from Lisp, but it happens here
	    in the C code.
  09.05.91  Linpack code taken from CAP 
  	    doublereal replaced with real globally
            d_sign() macro put in to replace FORTRAN function
	    LISP interface function internal_svd() written.

BUGS:

COPYRIGHT:
  The first routines are interfaces into the LINPACK fortran
  code, which was translated from FORTRAN.

*/
internal_svd(matrix_t, rows, cols, s_1, lsm_t, rsm_t, scratchCol, scratchRow)
float *matrix_t;		/* Destroyable copy of data matrix, transposed */
int rows,cols;			/* Dimensions of original data (not transposed) */
float *s_1;		        /* Singular values array with one extra location */
float *lsm_t;			/* Left singular matrix, rows by rows */
float *rsm_t;			/* Right singular matrix, cols by cols */
float *scratchCol;		/* Scratch array, size = rows */
float *scratchRow;		/* Scratch array, size = cols */
{
  int job = 11;
  int info;

  dsvdc_(matrix_t, &rows, &rows, &cols, s_1, scratchRow, lsm_t, &rows, rsm_t, &cols, 
	 scratchCol, &job, &info);
  
  return(info);
}


/* Avoid linking in the FORTRAN library by defining a macro that
   substitutes for a FORTRAN builtin. EJC 9.5.91 */
#define d_sign(px,py) ( *(py) < 0.0 ? -fabs(*(px)) : fabs(*(px)) )


/*****************************************************************/
/************************ Original code **************************/
/****************** Trivial mods marked by EJ ********************/
/*****************************************************************/

/* dsvdc.f -- translated by f2c (version of 3 February 1990  3:36:42).
   You must link the resulting object file with the libraries:
	-lF77 -lI77 -lm -lc   (in that order)
*/
#include "f2c.h" 


/* Table of constant values */

static integer c__1 = 1;
static real c_b44 = -1.;

/* Subroutine */ int dsvdc_(x, ldx, n, p, s, e, u, ldu, v, ldv, work, job, 
	info)
real *x;
integer *ldx, *n, *p;
real *s, *e, *u;
integer *ldu;
real *v;
integer *ldv;
real *work;
integer *job, *info;
{

    /* System generated locals */
    integer x_dim1, x_offset, u_dim1, u_offset, v_dim1, v_offset, i_1, i_2, 
	    i_3;
    real d_1, d_2, d_3, d_4, d_5, d_6, d_7;

    /* Builtin functions */
    /* This FORTRAN builtin is replaced by a macro. EJC 9.5.91 */
    /* double d_sign(); */
    double sqrt();

    /* Local variables */
    static integer kase;
    extern real ddot_();
    static integer jobu, iter;
    extern /* Subroutine */ int drot_();
    static real test;
    extern real dnrm2_();
    static integer nctp1;
    static real b, c;
    static integer nrtp1;
    static real f, g;
    static integer i, j, k, l, m;
    static real t, scale;
    extern /* Subroutine */ int dscal_();
    static real shift;
    extern /* Subroutine */ int dswap_(), drotg_();
    static integer maxit;
    extern /* Subroutine */ int daxpy_();
    static logical wantu, wantv;
    static real t1, ztest, el;
    static integer kk;
    static real cs;
    static integer ll, mm, ls;
    static real sl;
    static integer lu;
    static real sm, sn;
    static integer lm1, mm1, lp1, mp1, nct, ncu, lls, nrt;
    static real emm1, smm1;

    /* Parameter adjustments */
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;
    --s;
    --e;
    u_dim1 = *ldu;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    v_dim1 = *ldv;
    v_offset = v_dim1 + 1;
    v -= v_offset;
    --work;

    /* Function Body */
/*                                                                      
00000040*/
/*                                                                      
00000050*/
/*    dsvdc is a subroutine to reduce a double precision nxp matrix x   
00000060*/
/*    by orthogonal transformations u and v to diagonal form.  the      
00000070*/
/*    diagonal elements s(i) are the singular values of x.  the         
00000080*/
/*    columns of u are the corresponding left singular vectors,         
00000090*/
/*    and the columns of v the right singular vectors.                  
00000100*/
/*                                                                      
00000110*/
/*    on entry                                                          
00000120*/
/*                                                                      
00000130*/
/*        x         double precision(ldx,p), where ldx.ge.n.            
00000140*/
/*                  x contains the matrix whose singular value          
00000150*/
/*                  decomposition is to be computed.  x is              
00000160*/
/*                  destroyed by dsvdc.                                 
00000170*/
/*                                                                      
00000180*/
/*        ldx       integer.                                            
00000190*/
/*                  ldx is the leading dimension of the array x.        
00000200*/
/*                                                                      
00000210*/
/*        n         integer.                                            
00000220*/
/*                  n is the number of columns of the matrix x.         
00000230*/
/*                                                                      
00000240*/
/*        p         integer.                                            
00000250*/
/*                  p is the number of rows of the matrix x.            
00000260*/
/*                                                                      
00000270*/
/*        ldu       integer.                                            
00000280*/
/*                  ldu is the leading dimension of the array u.        
00000290*/
/*                  (see below).                                        
00000300*/
/*                                                                      
00000310*/
/*        ldv       integer.                                            
00000320*/
/*                  ldv is the leading dimension of the array v.        
00000330*/
/*                  (see below).                                        
00000340*/
/*                                                                      
00000350*/
/*        work      double precision(n).                                
00000360*/
/*                  work is a scratch array.                            
00000370*/
/*                                                                      
00000380*/
/*        job       integer.                                            
00000390*/
/*                  job controls the computation of the singular        
00000400*/
/*                  vectors.  it has the decimal expansion ab           
00000410*/
/*                  with the following meaning                          
00000420*/
/*                                                                      
00000430*/
/*                       a.eq.0    do not compute the left singular     
00000440*/
/*                                 vectors.                             
00000450*/
/*                       a.eq.1    return the n left singular vectors   
00000460*/
/*                                 in u.                                
00000470*/
/*                       a.ge.2    return the first min(n,p) singular   
00000480*/
/*                                 vectors in u.                        
00000490*/
/*                       b.eq.0    do not compute the right singular    
00000500*/
/*                                 vectors.                             
00000510*/
/*                       b.eq.1    return the right singular vectors    
00000520*/
/*                                 in v.                                
00000530*/
/*                                                                      
00000540*/
/*    on return                                                         
00000550*/
/*                                                                      
00000560*/
/*        s         double precision(mm), where mm=min(n+1,p).          
00000570*/
/*                  the first min(n,p) entries of s contain the         
00000580*/
/*                  singular values of x arranged in descending         
00000590*/
/*                  order of magnitude.                                 
00000600*/
/*                                                                      
00000610*/
/*        e         double precision(p).                                
00000620*/
/*                  e ordinarily contains zeros.  however see the       
00000630*/
/*                  discussion of info for exceptions.                  
00000640*/
/*                                                                      
00000650*/
/*        u         double precision(ldu,k), where ldu.ge.n.  if        
00000660*/
/*                                  joba.eq.1 then k.eq.n, if joba.ge.2 
00000670*/
/*                                  then k.eq.min(n,p).                 
00000680*/
/*                  u contains the matrix of right singular vectors.    
00000690*/
/*                  u is not referenced if joba.eq.0.  if n.le.p        
00000700*/
/*                  or if joba.eq.2, then u may be identified with x    
00000710*/
/*                  in the subroutine call.                             
00000720*/
/*                                                                      
00000730*/
/*        v         double precision(ldv,p), where ldv.ge.p.            
00000740*/
/*                  v contains the matrix of right singular vectors.    
00000750*/
/*                  v is not referenced if job.eq.0.  if p.le.n,        
00000760*/
/*                  then v may be identified with x in the              
00000770*/
/*                  subroutine call.                                    
00000780*/
/*                                                                      
00000790*/
/*        info      integer.                                            
00000800*/
/*                  the singular values (and their corresponding        
00000810*/
/*                  singular vectors) s(info+1),s(info+2),...,s(m)      
00000820*/
/*                  are correct (here m=min(n,p)).  thus if             
00000830*/
/*                  info.eq.0, all the singular values and their        
00000840*/
/*                  vectors are correct.  in any event, the matrix      
00000850*/
/*                  b = trans(u)*x*v is the bidiagonal matrix           
00000860*/
/*                  with the elements of s on its diagonal and the      
00000870*/
/*                  elements of e on its super-diagonal (trans(u)       
00000880*/
/*                  is the transpose of u).  thus the singular          
00000890*/
/*                  values of x and b are the same.                     
00000900*/
/*                                                                      
00000910*/
/*    linpack. this version dated 03/19/79 .                            
00000920*/
/*    g.w. stewart, university of maryland, argonne national lab.       
00000930*/
/*                                                                      
00000940*/
/*    dsvdc uses the following functions and subprograms.               
00000950*/
/*                                                                      
00000960*/
/*    external drot                                                     
00000970*/
/*    blas daxpy,ddot,dscal,dswap,dnrm2,drotg                           
00000980*/
/*    fortran dabs,dmax1,max0,min0,mod,dsqrt                            
00000990*/
/*                                                                      
00001000*/
/*    internal variables                                                
00001010*/
/*                                                                      
00001020*/
/*                                                                      
00001090*/
/*                                                                      
00001100*/
/*    set the maximum number of iterations.                             
00001110*/
/*                                                                      
00001120*/
    maxit = 30;
/*                                                                      
00001140*/
/*    determine what is to be computed.                                 
00001150*/
/*                                                                      
00001160*/
    wantu = FALSE_;
    wantv = FALSE_;
    jobu = *job % 100 / 10;
    ncu = *n;
    if (jobu > 1) {
	ncu = min(*n,*p);
    }
    if (jobu != 0) {
	wantu = TRUE_;
    }
    if (*job % 10 != 0) {
	wantv = TRUE_;
    }
/*                                                                      
00001240*/
/*    reduce x to bidiagonal form, storing the diagonal elements        
00001250*/
/*    in s and the super-diagonal elements in e.                        
00001260*/
/*                                                                      
00001270*/
    *info = 0;
/* Computing MAX */
    i_1 = *n - 1;
    nct = min(*p,i_1);
/* Computing MAX */
/* Computing MAX */
    i_3 = *p - 2;
    i_1 = 0, i_2 = min(*n,i_3);
    nrt = max(i_2,i_1);
    lu = max(nct,nrt);
    if (lu < 1) {
	goto L170;
    }
    i_1 = lu;
    for (l = 1; l <= i_1; ++l) {
	lp1 = l + 1;
	if (l > nct) {
	    goto L20;
	}
/*                                                                    
  00001360*/
/*          compute the transformation for the l-th column and        
  00001370*/
/*          place the l-th diagonal in s(l).                          
  00001380*/
/*                                                                    
  00001390*/
	i_2 = *n - l + 1;
	s[l] = dnrm2_(&i_2, &x[l + l * x_dim1], &c__1);
	if (s[l] == 0.) {
	    goto L10;
	}
	if (x[l + l * x_dim1] != 0.) {
	    s[l] = d_sign(&s[l], &x[l + l * x_dim1]);
	}
	i_2 = *n - l + 1;
	d_1 = 1. / s[l];
	dscal_(&i_2, &d_1, &x[l + l * x_dim1], &c__1);
	x[l + l * x_dim1] += 1.;
L10:
	s[l] = -s[l];
L20:
	if (*p < lp1) {
	    goto L50;
	}
	i_2 = *p;
	for (j = lp1; j <= i_2; ++j) {
	    if (l > nct) {
		goto L30;
	    }
	    if (s[l] == 0.) {
		goto L30;
	    }
/*                                                                
      00001520*/
/*             apply the transformation.                          
      00001530*/
/*                                                                
      00001540*/
	    i_3 = *n - l + 1;
	    t = -ddot_(&i_3, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
		    c__1) / x[l + l * x_dim1];
	    i_3 = *n - l + 1;
	    daxpy_(&i_3, &t, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
		    c__1);
L30:
/*                                                                
      00001580*/
/*          place the l-th row of x into  e for the               
      00001590*/
/*          subsequent calculation of the row transformation.     
      00001600*/
/*                                                                
      00001610*/
	    e[j] = x[l + j * x_dim1];
/* L40: */
	}
L50:
	if (! wantu || l > nct) {
	    goto L70;
	}
/*                                                                    
  00001660*/
/*          place the transformation in u for subsequent back         
  00001670*/
/*          multiplication.                                           
  00001680*/
/*                                                                    
  00001690*/
	i_2 = *n;
	for (i = l; i <= i_2; ++i) {
	    u[i + l * u_dim1] = x[i + l * x_dim1];
/* L60: */
	}
L70:
	if (l > nrt) {
	    goto L150;
	}
/*                                                                    
  00001750*/
/*          compute the l-th row transformation and place the         
  00001760*/
/*          l-th super-diagonal in e(l).                              
  00001770*/
/*                                                                    
  00001780*/
	i_2 = *p - l;
	e[l] = dnrm2_(&i_2, &e[lp1], &c__1);
	if (e[l] == 0.) {
	    goto L80;
	}
	if (e[lp1] != 0.) {
	    e[l] = d_sign(&e[l], &e[lp1]);
	}
	i_2 = *p - l;
	d_1 = 1. / e[l];
	dscal_(&i_2, &d_1, &e[lp1], &c__1);
	e[lp1] += 1.;
L80:
	e[l] = -e[l];
	if (lp1 > *n || e[l] == 0.) {
	    goto L120;
	}
/*                                                                    
  00001870*/
/*             apply the transformation.                              
  00001880*/
/*                                                                    
  00001890*/
	i_2 = *n;
	for (i = lp1; i <= i_2; ++i) {
	    work[i] = 0.;
/* L90: */
	}
	i_2 = *p;
	for (j = lp1; j <= i_2; ++j) {
	    i_3 = *n - l;
	    daxpy_(&i_3, &e[j], &x[lp1 + j * x_dim1], &c__1, &work[lp1], &
		    c__1);
/* L100: */
	}
	i_2 = *p;
	for (j = lp1; j <= i_2; ++j) {
	    i_3 = *n - l;
	    d_1 = -e[j] / e[lp1];
	    daxpy_(&i_3, &d_1, &work[lp1], &c__1, &x[lp1 + j * x_dim1], &c__1)
		    ;
/* L110: */
	}
L120:
	if (! wantv) {
	    goto L140;
	}
/*                                                                    
  00002010*/
/*             place the transformation in v for subsequent           
  00002020*/
/*             back multiplication.                                   
  00002030*/
/*                                                                    
  00002040*/
	i_2 = *p;
	for (i = lp1; i <= i_2; ++i) {
	    v[i + l * v_dim1] = e[i];
/* L130: */
	}
L140:
L150:
/* L160: */
    ;}
L170:
/*                                                                      
00002120*/
/*    set up the final bidiagonal matrix or order m.                    
00002130*/
/*                                                                      
00002140*/
/* Computing MAX */
    i_1 = *p, i_2 = *n + 1;
    m = min(i_2,i_1);
    nctp1 = nct + 1;
    nrtp1 = nrt + 1;
    if (nct < *p) {
	s[nctp1] = x[nctp1 + nctp1 * x_dim1];
    }
    if (*n < m) {
	s[m] = 0.;
    }
    if (nrtp1 < m) {
	e[nrtp1] = x[nrtp1 + m * x_dim1];
    }
    e[m] = 0.;
/*                                                                      
00002220*/
/*    if required, generate u.                                          
00002230*/
/*                                                                      
00002240*/
    if (! wantu) {
	goto L300;
    }
    if (ncu < nctp1) {
	goto L200;
    }
    i_1 = ncu;
    for (j = nctp1; j <= i_1; ++j) {
	i_2 = *n;
	for (i = 1; i <= i_2; ++i) {
	    u[i + j * u_dim1] = 0.;
/* L180: */
	}
	u[j + j * u_dim1] = 1.;
/* L190: */
    }
L200:
    if (nct < 1) {
	goto L290;
    }
    i_1 = nct;
    for (ll = 1; ll <= i_1; ++ll) {
	l = nct - ll + 1;
	if (s[l] == 0.) {
	    goto L250;
	}
	lp1 = l + 1;
	if (ncu < lp1) {
	    goto L220;
	}
	i_2 = ncu;
	for (j = lp1; j <= i_2; ++j) {
	    i_3 = *n - l + 1;
	    t = -ddot_(&i_3, &u[l + l * u_dim1], &c__1, &u[l + j * u_dim1], &
		    c__1) / u[l + l * u_dim1];
	    i_3 = *n - l + 1;
	    daxpy_(&i_3, &t, &u[l + l * u_dim1], &c__1, &u[l + j * u_dim1], &
		    c__1);
/* L210: */
	}
L220:
	i_2 = *n - l + 1;
	dscal_(&i_2, &c_b44, &u[l + l * u_dim1], &c__1);
	u[l + l * u_dim1] += 1.;
	lm1 = l - 1;
	if (lm1 < 1) {
	    goto L240;
	}
	i_2 = lm1;
	for (i = 1; i <= i_2; ++i) {
	    u[i + l * u_dim1] = 0.;
/* L230: */
	}
L240:
	goto L270;
L250:
	i_2 = *n;
	for (i = 1; i <= i_2; ++i) {
	    u[i + l * u_dim1] = 0.;
/* L260: */
	}
	u[l + l * u_dim1] = 1.;
L270:
/* L280: */
    ;}
L290:
L300:
/*                                                                      
00002630*/
/*    if it is required, generate v.                                    
00002640*/
/*                                                                      
00002650*/
    if (! wantv) {
	goto L350;
    }
    i_1 = *p;
    for (ll = 1; ll <= i_1; ++ll) {
	l = *p - ll + 1;
	lp1 = l + 1;
	if (l > nrt) {
	    goto L320;
	}
	if (e[l] == 0.) {
	    goto L320;
	}
	i_2 = *p;
	for (j = lp1; j <= i_2; ++j) {
	    i_3 = *p - l;
	    t = -ddot_(&i_3, &v[lp1 + l * v_dim1], &c__1, &v[lp1 + j * v_dim1]
		    , &c__1) / v[lp1 + l * v_dim1];
	    i_3 = *p - l;
	    daxpy_(&i_3, &t, &v[lp1 + l * v_dim1], &c__1, &v[lp1 + j * v_dim1]
		    , &c__1);
/* L310: */
	}
L320:
	i_2 = *p;
	for (i = 1; i <= i_2; ++i) {
	    v[i + l * v_dim1] = 0.;
/* L330: */
	}
	v[l + l * v_dim1] = 1.;
/* L340: */
    }
L350:
/*                                                                      
00002830*/
/*    main iteration loop for the singular values.                      
00002840*/
/*                                                                      
00002850*/
    mm = m;
    iter = 0;
L360:
/*                                                                      
00002890*/
/*       quit if all the singular values have been found.               
00002900*/
/*                                                                      
00002910*/
/*    ...exit                                                           
00002920*/
    if (m == 0) {
	goto L620;
    }
/*                                                                      
00002940*/
/*       if too many iterations have been performed, set                
00002950*/
/*       flag and return.                                               
00002960*/
/*                                                                      
00002970*/
    if (iter < maxit) {
	goto L370;
    }
    *info = m;
/*    ......exit                                                        
00003000*/
    goto L620;
L370:
/*                                                                      
00003030*/
/*       this section of the program inspects for                       
00003040*/
/*       negligible elements in the s and e arrays.  on                 
00003050*/
/*       completion the variables kase and l are set as follows.        
00003060*/
/*                                                                      
00003070*/
/*          kase = 1     if s(m) and e(l-1) are negligible and l.lt.m   
00003080*/
/*          kase = 2     if s(l) is negligible and l.lt.m               
00003090*/
/*          kase = 3     if e(l-1) is negligible, l.lt.m, and           
00003100*/
/*                       s(l), ..., s(m) are not negligible (qr step).  
00003110*/
/*          kase = 4     if e(m-1) is negligible (convergence).         
00003120*/
/*                                                                      
00003130*/
    i_1 = m;
    for (ll = 1; ll <= i_1; ++ll) {
	l = m - ll;
/*       ...exit                                                      
  00003160*/
	if (l == 0) {
	    goto L400;
	}
	test = (d_1 = s[l], abs(d_1)) + (d_2 = s[l + 1], abs(d_2));
	ztest = test + (d_1 = e[l], abs(d_1));
	if (ztest != test) {
	    goto L380;
	}
	e[l] = 0.;
/*       ......exit                                                   
  00003220*/
	goto L400;
L380:
/* L390: */
    ;}
L400:
    if (l != m - 1) {
	goto L410;
    }
    kase = 4;
    goto L480;
L410:
    lp1 = l + 1;
    mp1 = m + 1;
    i_1 = mp1;
    for (lls = lp1; lls <= i_1; ++lls) {
	ls = m - lls + lp1;
/*          ...exit                                                   
  00003350*/
	if (ls == l) {
	    goto L440;
	}
	test = 0.;
	if (ls != m) {
	    test += (d_1 = e[ls], abs(d_1));
	}
	if (ls != l + 1) {
	    test += (d_1 = e[ls - 1], abs(d_1));
	}
	ztest = test + (d_1 = s[ls], abs(d_1));
	if (ztest != test) {
	    goto L420;
	}
	s[ls] = 0.;
/*          ......exit                                                
  00003430*/
	goto L440;
L420:
/* L430: */
    ;}
L440:
    if (ls != l) {
	goto L450;
    }
    kase = 3;
    goto L470;
L450:
    if (ls != m) {
	goto L460;
    }
    kase = 1;
    goto L470;
L460:
    kase = 2;
    l = ls;
L470:
L480:
    ++l;
/*                                                                      
00003610*/
/*       perform the task indicated by kase.                            
00003620*/
/*                                                                      
00003630*/
    switch (kase) {
	case 1:  goto L490;
	case 2:  goto L520;
	case 3:  goto L540;
	case 4:  goto L570;
    }
/*                                                                      
00003650*/
/*       deflate negligible s(m).                                       
00003660*/
/*                                                                      
00003670*/
L490:
    mm1 = m - 1;
    f = e[m - 1];
    e[m - 1] = 0.;
    i_1 = mm1;
    for (kk = l; kk <= i_1; ++kk) {
	k = mm1 - kk + l;
	t1 = s[k];
	drotg_(&t1, &f, &cs, &sn);
	s[k] = t1;
	if (k == l) {
	    goto L500;
	}
	f = -sn * e[k - 1];
	e[k - 1] = cs * e[k - 1];
L500:
	if (wantv) {
	    drot_(p, &v[k * v_dim1 + 1], &c__1, &v[m * v_dim1 + 1], &c__1, &
		    cs, &sn);
	}
/* L510: */
    }
    goto L610;
/*                                                                      
00003840*/
/*       split at negligible s(l).                                      
00003850*/
/*                                                                      
00003860*/
L520:
    f = e[l - 1];
    e[l - 1] = 0.;
    i_1 = m;
    for (k = l; k <= i_1; ++k) {
	t1 = s[k];
	drotg_(&t1, &f, &cs, &sn);
	s[k] = t1;
	f = -sn * e[k];
	e[k] = cs * e[k];
	if (wantu) {
	    drot_(n, &u[k * u_dim1 + 1], &c__1, &u[(l - 1) * u_dim1 + 1], &
		    c__1, &cs, &sn);
	}
/* L530: */
    }
    goto L610;
/*                                                                      
00003990*/
/*       perform one qr step.                                           
00004000*/
/*                                                                      
00004010*/
L540:
/*                                                                      
00004030*/
/*          calculate the shift.                                        
00004040*/
/*                                                                      
00004050*/
/* Computing MAX */
    d_6 = (d_1 = s[m], abs(d_1)), d_7 = (d_2 = s[m - 1], abs(d_2)), d_6 = max(
	    d_7,d_6), d_7 = (d_3 = e[m - 1], abs(d_3)), d_6 = max(d_7,d_6), 
	    d_7 = (d_4 = s[l], abs(d_4)), d_6 = max(d_7,d_6), d_7 = (d_5 = e[
	    l], abs(d_5));
    scale = max(d_7,d_6);
    sm = s[m] / scale;
    smm1 = s[m - 1] / scale;
    emm1 = e[m - 1] / scale;
    sl = s[l] / scale;
    el = e[l] / scale;
/* Computing 2nd power */
    d_1 = emm1;
    b = ((smm1 + sm) * (smm1 - sm) + d_1 * d_1) / 2.;
/* Computing 2nd power */
    d_1 = sm * emm1;
    c = d_1 * d_1;
    shift = 0.;
    if (b == 0. && c == 0.) {
	goto L550;
    }
/* Computing 2nd power */
    d_1 = b;
    shift = sqrt(d_1 * d_1 + c);
    if (b < 0.) {
	shift = -shift;
    }
    shift = c / (b + shift);
L550:
    f = (sl + sm) * (sl - sm) - shift;
    g = sl * el;
/*                                                                      
00004230*/
/*          chase zeros.                                                
00004240*/
/*                                                                      
00004250*/
    mm1 = m - 1;
    i_1 = mm1;
    for (k = l; k <= i_1; ++k) {
	drotg_(&f, &g, &cs, &sn);
	if (k != l) {
	    e[k - 1] = f;
	}
	f = cs * s[k] + sn * e[k];
	e[k] = cs * e[k] - sn * s[k];
	g = sn * s[k + 1];
	s[k + 1] = cs * s[k + 1];
	if (wantv) {
	    drot_(p, &v[k * v_dim1 + 1], &c__1, &v[(k + 1) * v_dim1 + 1], &
		    c__1, &cs, &sn);
	}
	drotg_(&f, &g, &cs, &sn);
	s[k] = f;
	f = cs * e[k] + sn * s[k + 1];
	s[k + 1] = -sn * e[k] + cs * s[k + 1];
	g = sn * e[k + 1];
	e[k + 1] = cs * e[k + 1];
	if (wantu && k < *n) {
	    drot_(n, &u[k * u_dim1 + 1], &c__1, &u[(k + 1) * u_dim1 + 1], &
		    c__1, &cs, &sn);
	}
/* L560: */
    }
    e[m - 1] = f;
    ++iter;
    goto L610;
/*                                                                      
00004470*/
/*       convergence.                                                   
00004480*/
/*                                                                      
00004490*/
L570:
/*                                                                      
00004510*/
/*          make the singular value  positive.                          
00004520*/
/*                                                                      
00004530*/
    if (s[l] >= 0.) {
	goto L580;
    }
    s[l] = -s[l];
    if (wantv) {
	dscal_(p, &c_b44, &v[l * v_dim1 + 1], &c__1);
    }
L580:
/*                                                                      
00004580*/
/*          order the singular value.                                   
00004590*/
/*                                                                      
00004600*/
L590:
    if (l == mm) {
	goto L600;
    }
/*          ...exit                                                     
00004620*/
    if (s[l] >= s[l + 1]) {
	goto L600;
    }
    t = s[l];
    s[l] = s[l + 1];
    s[l + 1] = t;
    if (wantv && l < *p) {
	dswap_(p, &v[l * v_dim1 + 1], &c__1, &v[(l + 1) * v_dim1 + 1], &c__1);

    }
    if (wantu && l < *n) {
	dswap_(n, &u[l * u_dim1 + 1], &c__1, &u[(l + 1) * u_dim1 + 1], &c__1);

    }
    ++l;
    goto L590;
L600:
    iter = 0;
    --m;
L610:
    goto L360;
L620:
    return 0;
} /* dsvdc_ */

/* blas.f -- translated by f2c (version of 3 February 1990  3:36:42).
   You must link the resulting object file with the libraries:
	-lF77 -lI77 -lm -lc   (in that order)
*/

/*  Already included ...
#include "f2c.h"
*/

/* Table of constant values */

static real c_b52 = 1.;

real dasum_(n, dx, incx)
integer *n;
real *dx;
integer *incx;
{
    /* System generated locals */
    integer i_1, i_2;
    real ret_val, d_1, d_2, d_3, d_4, d_5, d_6;

    /* Local variables */
    static integer i, m;
    static real dtemp;
    static integer nincx, mp1;

    /* Parameter adjustments */
    --dx;

    /* Function Body */
/*                                                                      
00000020*/
/*    takes the sum of the absolute values.                             
00000030*/
/*    jack dongarra, linpack, 3/11/78.                                  
00000040*/
/*                                                                      
00000050*/
/*                                                                      
00000080*/
    ret_val = 0.;
    dtemp = 0.;
    if (*n <= 0) {
	return ret_val;
    }
    if (*incx == 1) {
	goto L20;
    }
/*                                                                      
00000130*/
/*       code for increment not equal to 1                              
00000140*/
/*                                                                      
00000150*/
    nincx = *n * *incx;
    i_1 = nincx;
    i_2 = *incx;
    for (i = 1; i_2 < 0 ? i >= i_1 : i <= i_1; i += i_2) {
	dtemp += (d_1 = dx[i], abs(d_1));
/* L10: */
    }
    ret_val = dtemp;
    return ret_val;
/*                                                                      
00000220*/
/*       code for increment equal to 1                                  
00000230*/
/*                                                                      
00000240*/
/*                                                                      
00000250*/
/*       clean-up loop                                                  
00000260*/
/*                                                                      
00000270*/
L20:
    m = *n % 6;
    if (m == 0) {
	goto L40;
    }
    i_2 = m;
    for (i = 1; i <= i_2; ++i) {
	dtemp += (d_1 = dx[i], abs(d_1));
/* L30: */
    }
    if (*n < 6) {
	goto L60;
    }
L40:
    mp1 = m + 1;
    i_2 = *n;
    for (i = mp1; i <= i_2; i += 6) {
	dtemp = dtemp + (d_1 = dx[i], abs(d_1)) + (d_2 = dx[i + 1], abs(d_2)) 
		+ (d_3 = dx[i + 2], abs(d_3)) + (d_4 = dx[i + 3], abs(d_4)) + 
		(d_5 = dx[i + 4], abs(d_5)) + (d_6 = dx[i + 5], abs(d_6));
/* L50: */
    }
L60:
    ret_val = dtemp;
    return ret_val;
} /* dasum_ */

/* Subroutine */ int daxpy_(n, da, dx, incx, dy, incy)
integer *n;
real *da, *dx;
integer *incx;
real *dy;
integer *incy;
{
    /* System generated locals */
    integer i_1;

    /* Local variables */
    static integer i, m, ix, iy, mp1;

    /* Parameter adjustments */
    --dx;
    --dy;

    /* Function Body */
/*                                                                      
00000020*/
/*    constant times a vector plus a vector.                            
00000030*/
/*    uses unrolled loops for increments equal to one.                  
00000040*/
/*    jack dongarra, linpack, 3/11/78.                                  
00000050*/
/*                                                                      
00000060*/
/*                                                                      
00000090*/
    if (*n <= 0) {
	return 0;
    }
    if (*da == 0.) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }
/*                                                                      
00000130*/
/*       code for unequal increments or equal increments                
00000140*/
/*         not equal to 1                                               
00000150*/
/*                                                                      
00000160*/
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i_1 = *n;
    for (i = 1; i <= i_1; ++i) {
	dy[iy] += *da * dx[ix];
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;
/*                                                                      
00000270*/
/*       code for both increments equal to 1                            
00000280*/
/*                                                                      
00000290*/
/*                                                                      
00000300*/
/*       clean-up loop                                                  
00000310*/
/*                                                                      
00000320*/
L20:
    m = *n % 4;
    if (m == 0) {
	goto L40;
    }
    i_1 = m;
    for (i = 1; i <= i_1; ++i) {
	dy[i] += *da * dx[i];
/* L30: */
    }
    if (*n < 4) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i_1 = *n;
    for (i = mp1; i <= i_1; i += 4) {
	dy[i] += *da * dx[i];
	dy[i + 1] += *da * dx[i + 1];
	dy[i + 2] += *da * dx[i + 2];
	dy[i + 3] += *da * dx[i + 3];
/* L50: */
    }
    return 0;
} /* daxpy_ */

/* Subroutine */ int dcopy_(n, dx, incx, dy, incy)
integer *n;
real *dx;
integer *incx;
real *dy;
integer *incy;
{
    /* System generated locals */
    integer i_1;

    /* Local variables */
    static integer i, m, ix, iy, mp1;

    /* Parameter adjustments */
    --dx;
    --dy;

    /* Function Body */
/*                                                                      
00000020*/
/*    copies a vector, x, to a vector, y.                               
00000030*/
/*    uses unrolled loops for increments equal to one.                  
00000040*/
/*    jack dongarra, linpack, 3/11/78.                                  
00000050*/
/*                                                                      
00000060*/
/*                                                                      
00000090*/
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }
/*                                                                      
00000120*/
/*       code for unequal increments or equal increments                
00000130*/
/*         not equal to 1                                               
00000140*/
/*                                                                      
00000150*/
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i_1 = *n;
    for (i = 1; i <= i_1; ++i) {
	dy[iy] = dx[ix];
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;
/*                                                                      
00000260*/
/*       code for both increments equal to 1                            
00000270*/
/*                                                                      
00000280*/
/*                                                                      
00000290*/
/*       clean-up loop                                                  
00000300*/
/*                                                                      
00000310*/
L20:
    m = *n % 7;
    if (m == 0) {
	goto L40;
    }
    i_1 = m;
    for (i = 1; i <= i_1; ++i) {
	dy[i] = dx[i];
/* L30: */
    }
    if (*n < 7) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i_1 = *n;
    for (i = mp1; i <= i_1; i += 7) {
	dy[i] = dx[i];
	dy[i + 1] = dx[i + 1];
	dy[i + 2] = dx[i + 2];
	dy[i + 3] = dx[i + 3];
	dy[i + 4] = dx[i + 4];
	dy[i + 5] = dx[i + 5];
	dy[i + 6] = dx[i + 6];
/* L50: */
    }
    return 0;
} /* dcopy_ */

real ddot_(n, dx, incx, dy, incy)
integer *n;
real *dx;
integer *incx;
real *dy;
integer *incy;
{
    /* System generated locals */
    integer i_1;
    real ret_val;

    /* Local variables */
    static integer i, m;
    static real dtemp;
    static integer ix, iy, mp1;

    /* Parameter adjustments */
    --dx;
    --dy;

    /* Function Body */
/*                                                                      
00000020*/
/*    forms the dot product of two vectors.                             
00000030*/
/*    uses unrolled loops for increments equal to one.                  
00000040*/
/*    jack dongarra, linpack, 3/11/78.                                  
00000050*/
/*                                                                      
00000060*/
/*                                                                      
00000090*/
    ret_val = 0.;
    dtemp = 0.;
    if (*n <= 0) {
	return ret_val;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }
/*                                                                      
00000140*/
/*       code for unequal increments or equal increments                
00000150*/
/*         not equal to 1                                               
00000160*/
/*                                                                      
00000170*/
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i_1 = *n;
    for (i = 1; i <= i_1; ++i) {
	dtemp += dx[ix] * dy[iy];
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    ret_val = dtemp;
    return ret_val;
/*                                                                      
00000290*/
/*       code for both increments equal to 1                            
00000300*/
/*                                                                      
00000310*/
/*                                                                      
00000320*/
/*       clean-up loop                                                  
00000330*/
/*                                                                      
00000340*/
L20:
    m = *n % 5;
    if (m == 0) {
	goto L40;
    }
    i_1 = m;
    for (i = 1; i <= i_1; ++i) {
	dtemp += dx[i] * dy[i];
/* L30: */
    }
    if (*n < 5) {
	goto L60;
    }
L40:
    mp1 = m + 1;
    i_1 = *n;
    for (i = mp1; i <= i_1; i += 5) {
	dtemp = dtemp + dx[i] * dy[i] + dx[i + 1] * dy[i + 1] + dx[i + 2] * 
		dy[i + 2] + dx[i + 3] * dy[i + 3] + dx[i + 4] * dy[i + 4];
/* L50: */
    }
L60:
    ret_val = dtemp;
    return ret_val;
} /* ddot_ */

real dmach_(job)
integer *job;
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    static real huge, tiny, s, eps;

/*                                                                      
00000030*/
/*    smach computes machine parameters of floating point               
00000040*/
/*    arithmetic for use in testing only.  not required by              
00000050*/
/*    linpack proper.                                                   
00000060*/
/*                                                                      
00000070*/
/*    if trouble with automatic computation of these quantities,        
00000080*/
/*    they can be set by direct assignment statements.                  
00000090*/
/*    assume the computer has                                           
00000100*/
/*                                                                      
00000110*/
/*       b = base of arithmetic                                         
00000120*/
/*       t = number of base  b  digits                                  
00000130*/
/*       l = smallest possible exponent                                 
00000140*/
/*       u = largest possible exponent                                  
00000150*/
/*                                                                      
00000160*/
/*    then                                                              
00000170*/
/*                                                                      
00000180*/
/*       eps = b**(1-t)                                                 
00000190*/
/*       tiny = 100.0*b**(-l+t)                                         
00000200*/
/*       huge = 0.01*b**(u-t)                                           
00000210*/
/*                                                                      
00000220*/
/*    dmach same as smach except t, l, u apply to                       
00000230*/
/*    double precision.                                                 
00000240*/
/*                                                                      
00000250*/
/*    cmach same as smach except if complex division                    
00000260*/
/*    is done by                                                        
00000270*/
/*                                                                      
00000280*/
/*       1/(x+i*y) = (x-i*y)/(x**2+y**2)                                
00000290*/
/*                                                                      
00000300*/
/*    then                                                              
00000310*/
/*                                                                      
00000320*/
/*       tiny = sqrt(tiny)                                              
00000330*/
/*       huge = sqrt(huge)                                              
00000340*/
/*                                                                      
00000350*/
/*                                                                      
00000360*/
/*    job is 1, 2 or 3 for epsilon, tiny and huge, respectively.        
00000370*/
/*                                                                      
00000380*/
/*                                                                      
00000400*/
    eps = 1.;
L10:
    eps /= 2.;
    s = eps + 1.;
    if (s > 1.) {
	goto L10;
    }
    eps *= 2.;
/*                                                                      
00000460*/
    s = 1.;
L20:
    tiny = s;
    s /= 16.;
    if (s * (float)1. != 0.) {
	goto L20;
    }
    tiny = tiny / eps * (float)100.;
    huge = 1. / tiny;
/*                                                                      
00000530*/
    if (*job == 1) {
	ret_val = eps;
    }
    if (*job == 2) {
	ret_val = tiny;
    }
    if (*job == 3) {
	ret_val = huge;
    }
    return ret_val;
} /* dmach_ */

real dnrm2_(n, dx, incx)
integer *n;
real *dx;
integer *incx;
{
    /* Initialized data */

    static real zero = 0.;
    static real one = 1.;
    static real cutlo = 8.232e-11;
    static real cuthi = 1.304e19;

    /* Format strings */
    static char fmt_30[] = "";
    static char fmt_50[] = "";
    static char fmt_70[] = "";
    static char fmt_110[] = "";

    /* System generated locals */
    integer i_1, i_2;
    real ret_val, d_1;

    /* Builtin functions */
    /* This FORTRAN builtin is replaced by a macro. EJC 9.5.91 */
    /* double d_sign(); */
    double sqrt();

    /* Local variables */
    static real xmax;
    static integer next, i, j, nn;
    static real hitest, sum;

    /* Assigned format variables */
    char *next_fmt;

    /* Parameter adjustments */
    --dx;

    /* Function Body */
/*                                                                      
00000050*/
/*    euclidean norm of the n-vector stored in dx() with storage        
00000060*/
/*    increment incx .                                                  
00000070*/
/*    if    n .le. 0 return with result = 0.                            
00000080*/
/*    if n .ge. 1 then incx must be .ge. 1                              
00000090*/
/*                                                                      
00000100*/
/*          c.l.lawson, 1978 jan 08                                     
00000110*/
/*                                                                      
00000120*/
/*    four phase method     using two built-in constants that are       
00000130*/
/*    hopefully applicable to all machines.                             
00000140*/
/*        cutlo = maximum of  dsqrt(u/eps)  over all known machines.    
00000150*/
/*        cuthi = minimum of  dsqrt(v)      over all known machines.    
00000160*/
/*    where                                                             
00000170*/
/*        eps = smallest no. such that eps + 1. .gt. 1.                 
00000180*/
/*        u   = smallest positive no.   (underflow limit)               
00000190*/
/*        v   = largest  no.            (overflow  limit)               
00000200*/
/*                                                                      
00000210*/
/*    brief outline of algorithm..                                      
00000220*/
/*                                                                      
00000230*/
/*    phase 1    scans zero components.                                 
00000240*/
/*    move to phase 2 when a component is nonzero and .le. cutlo        
00000250*/
/*    move to phase 3 when a component is .gt. cutlo                    
00000260*/
/*    move to phase 4 when a component is .ge. cuthi/m                  
00000270*/
/*    where m = n for x() real and m = 2*n for complex.                 
00000280*/
/*                                                                      
00000290*/
/*    values for cutlo and cuthi..                                      
00000300*/
/*    from the environmental parameters listed in the imsl converter    
00000310*/
/*    document the limiting values are as follows..                     
00000320*/
/*    cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds 
are00000330*/
/*                  univac and dec at 2**(-103)                         
00000340*/
/*                  thus cutlo = 2**(-51) = 4.44089e-16                 
00000350*/
/*    cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.          
00000360*/
/*                  thus cuthi = 2**(63.5) = 1.30438e19                 
00000370*/
/*    cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.             
00000380*/
/*                  thus cutlo = 2**(-33.5) = 8.23181d-11               
00000390*/
/*    cuthi, d.p.   same as s.p.  cuthi = 1.30438d19                    
00000400*/
/*    data cutlo, cuthi / 8.232d-11,  1.304d19 /                        
00000410*/
/*    data cutlo, cuthi / 4.441e-16,  1.304e19 /                        
00000420*/
/*                                                                      
00000440*/
    if (*n > 0) {
	goto L10;
    }
    ret_val = zero;
    goto L300;
/*                                                                      
00000480*/
L10:
    next = 0;
    next_fmt = fmt_30;
    sum = zero;
    nn = *n * *incx;
/*                                                begin main loop       
00000520*/
    i = 1;
L20:
    switch (next) {
	case 0: goto L30;
	case 1: goto L50;
	case 2: goto L70;
	case 3: goto L110;
    }
L30:
    if ((d_1 = dx[i], abs(d_1)) > cutlo) {
	goto L85;
    }
    next = 1;
    next_fmt = fmt_50;
    xmax = zero;
/*                                                                      
00000580*/
/*                       phase 1.  sum is zero                          
00000590*/
/*                                                                      
00000600*/
L50:
    if (dx[i] == zero) {
	goto L200;
    }
    if ((d_1 = dx[i], abs(d_1)) > cutlo) {
	goto L85;
    }
/*                                                                      
00000630*/
/*                               prepare for phase 2.                   
00000640*/
    next = 2;
    next_fmt = fmt_70;
    goto L105;
/*                                                                      
00000670*/
/*                               prepare for phase 4.                   
00000680*/
/*                                                                      
00000690*/
L100:
    i = j;
    next = 3;
    next_fmt = fmt_110;
    sum = sum / dx[i] / dx[i];
L105:
    xmax = (d_1 = dx[i], abs(d_1));
    goto L115;
/*                                                                      
00000750*/
/*                  phase 2.  sum is small.                             
00000760*/
/*                            scale to avoid destructive underflow.     
00000770*/
/*                                                                      
00000780*/
L70:
    if ((d_1 = dx[i], abs(d_1)) > cutlo) {
	goto L75;
    }
/*                                                                      
00000800*/
/*                    common code for phases 2 and 4.                   
00000810*/
/*                    in phase 4 sum is large.  scale to avoid 
overflow.00000820*/
/*                                                                      
00000830*/
L110:
    if ((d_1 = dx[i], abs(d_1)) <= xmax) {
	goto L115;
    }
/* Computing 2nd power */
    d_1 = xmax / dx[i];
    sum = one + sum * (d_1 * d_1);
    xmax = (d_1 = dx[i], abs(d_1));
    goto L200;
/*                                                                      
00000880*/
L115:
/* Computing 2nd power */
    d_1 = dx[i] / xmax;
    sum += d_1 * d_1;
    goto L200;
/*                                                                      
00000910*/
/*                                                                      
00000920*/
/*                 prepare for phase 3.                                 
00000930*/
/*                                                                      
00000940*/
L75:
    sum = sum * xmax * xmax;
/*                                                                      
00000960*/
/*                                                                      
00000970*/
/*    for real or d.p. set hitest = cuthi/n                             
00000980*/
/*    for complex      set hitest = cuthi/(2*n)                         
00000990*/
/*                                                                      
00001000*/
L85:
    hitest = cuthi / (real) (*n);
/*                                                                      
00001020*/
/*                  phase 3.  sum is mid-range.  no scaling.            
00001030*/
/*                                                                      
00001040*/
    i_1 = nn;
    i_2 = *incx;
    for (j = i; i_2 < 0 ? j >= i_1 : j <= i_1; j += i_2) {
	if ((d_1 = dx[j], abs(d_1)) >= hitest) {
	    goto L100;
	}
/* L95: */
/* Computing 2nd power */
	d_1 = dx[j];
	sum += d_1 * d_1;
    }
    ret_val = sqrt(sum);
    goto L300;
/*                                                                      
00001100*/
L200:
    i += *incx;
    if (i <= nn) {
	goto L20;
    }
/*                                                                      
00001140*/
/*             end of main loop.                                        
00001150*/
/*                                                                      
00001160*/
/*             compute square root and adjust for scaling.              
00001170*/
/*                                                                      
00001180*/
    ret_val = xmax * sqrt(sum);
L300:
    return ret_val;
} /* dnrm2_ */

/* Subroutine */ int drot_(n, dx, incx, dy, incy, c, s)
integer *n;
real *dx;
integer *incx;
real *dy;
integer *incy;
real *c, *s;
{
    /* System generated locals */
    integer i_1;

    /* Local variables */
    static integer i;
    static real dtemp;
    static integer ix, iy;

    /* Parameter adjustments */
    --dx;
    --dy;

    /* Function Body */
/*                                                                      
00000020*/
/*    applies a plane rotation.                                         
00000030*/
/*    jack dongarra, linpack, 3/11/78.                                  
00000040*/
/*                                                                      
00000050*/
/*                                                                      
00000080*/
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }
/*                                                                      
00000110*/
/*      code for unequal increments or equal increments not equal       
00000120*/
/*        to 1                                                          
00000130*/
/*                                                                      
00000140*/
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i_1 = *n;
    for (i = 1; i <= i_1; ++i) {
	dtemp = *c * dx[ix] + *s * dy[iy];
	dy[iy] = *c * dy[iy] - *s * dx[ix];
	dx[ix] = dtemp;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;
/*                                                                      
00000270*/
/*      code for both increments equal to 1                             
00000280*/
/*                                                                      
00000290*/
L20:
    i_1 = *n;
    for (i = 1; i <= i_1; ++i) {
	dtemp = *c * dx[i] + *s * dy[i];
	dy[i] = *c * dy[i] - *s * dx[i];
	dx[i] = dtemp;
/* L30: */
    }
    return 0;
} /* drot_ */

/* Subroutine */ int drotg_(da, db, c, s)
real *da, *db, *c, *s;
{
    /* System generated locals */
    real d_1, d_2;

    /* Builtin functions */
    /* This FORTRAN builtin is replaced by a macro. EJC 9.5.91 */
    /* double d_sign(); */
    double sqrt();

    /* Local variables */
    static real r, scale, z, roe;

/*                                                                      
00000020*/
/*    construct givens plane rotation.                                  
00000030*/
/*    jack dongarra, linpack, 3/11/78.                                  
00000040*/
/*                                                                      
00000050*/
/*                                                                      
00000070*/
    roe = *db;
    if (abs(*da) > abs(*db)) {
	roe = *da;
    }
    scale = abs(*da) + abs(*db);
    if (scale != 0.) {
	goto L10;
    }
    *c = 1.;
    *s = 0.;
    r = 0.;
    goto L20;
L10:
/* Computing 2nd power */
    d_1 = *da / scale;
/* Computing 2nd power */
    d_2 = *db / scale;
    r = scale * sqrt(d_1 * d_1 + d_2 * d_2);
    r = d_sign(&c_b52, &roe) * r;
    *c = *da / r;
    *s = *db / r;
L20:
    z = 1.;
    if (abs(*da) > abs(*db)) {
	z = *s;
    }
    if (abs(*db) >= abs(*da) && *c != 0.) {
	z = 1. / *c;
    }
    *da = r;
    *db = z;
    return 0;
} /* drotg_ */

/* Subroutine */ int dscal_(n, da, dx, incx)
integer *n;
real *da, *dx;
integer *incx;
{
    /* System generated locals */
    integer i_1, i_2;

    /* Local variables */
    static integer i, m, nincx, mp1;

    /* Parameter adjustments */
    --dx;

    /* Function Body */
/*                                                                      
00000020*/
/*    scales a vector by a constant.                                    
00000030*/
/*    uses unrolled loops for increment equal to one.                   
00000040*/
/*    jack dongarra, linpack, 3/11/78.                                  
00000050*/
/*                                                                      
00000060*/
/*                                                                      
00000090*/
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1) {
	goto L20;
    }
/*                                                                      
00000120*/
/*       code for increment not equal to 1                              
00000130*/
/*                                                                      
00000140*/
    nincx = *n * *incx;
    i_1 = nincx;
    i_2 = *incx;
    for (i = 1; i_2 < 0 ? i >= i_1 : i <= i_1; i += i_2) {
	dx[i] = *da * dx[i];
/* L10: */
    }
    return 0;
/*                                                                      
00000200*/
/*       code for increment equal to 1                                  
00000210*/
/*                                                                      
00000220*/
/*                                                                      
00000230*/
/*       clean-up loop                                                  
00000240*/
/*                                                                      
00000250*/
L20:
    m = *n % 5;
    if (m == 0) {
	goto L40;
    }
    i_2 = m;
    for (i = 1; i <= i_2; ++i) {
	dx[i] = *da * dx[i];
/* L30: */
    }
    if (*n < 5) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i_2 = *n;
    for (i = mp1; i <= i_2; i += 5) {
	dx[i] = *da * dx[i];
	dx[i + 1] = *da * dx[i + 1];
	dx[i + 2] = *da * dx[i + 2];
	dx[i + 3] = *da * dx[i + 3];
	dx[i + 4] = *da * dx[i + 4];
/* L50: */
    }
    return 0;
} /* dscal_ */

/* Subroutine */ int dswap_(n, dx, incx, dy, incy)
integer *n;
real *dx;
integer *incx;
real *dy;
integer *incy;
{
    /* System generated locals */
    integer i_1;

    /* Local variables */
    static integer i, m;
    static real dtemp;
    static integer ix, iy, mp1;

    /* Parameter adjustments */
    --dx;
    --dy;

    /* Function Body */
/*                                                                      
00000020*/
/*    interchanges two vectors.                                         
00000030*/
/*    uses unrolled loops for increments equal one.                     
00000040*/
/*    jack dongarra, linpack, 3/11/78.                                  
00000050*/
/*                                                                      
00000060*/
/*                                                                      
00000090*/
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }
/*                                                                      
00000120*/
/*      code for unequal increments or equal increments not equal       
00000130*/
/*        to 1                                                          
00000140*/
/*                                                                      
00000150*/
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i_1 = *n;
    for (i = 1; i <= i_1; ++i) {
	dtemp = dx[ix];
	dx[ix] = dy[iy];
	dy[iy] = dtemp;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;
/*                                                                      
00000280*/
/*      code for both increments equal to 1                             
00000290*/
/*                                                                      
00000300*/
/*                                                                      
00000310*/
/*      clean-up loop                                                   
00000320*/
/*                                                                      
00000330*/
L20:
    m = *n % 3;
    if (m == 0) {
	goto L40;
    }
    i_1 = m;
    for (i = 1; i <= i_1; ++i) {
	dtemp = dx[i];
	dx[i] = dy[i];
	dy[i] = dtemp;
/* L30: */
    }
    if (*n < 3) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i_1 = *n;
    for (i = mp1; i <= i_1; i += 3) {
	dtemp = dx[i];
	dx[i] = dy[i];
	dy[i] = dtemp;
	dtemp = dx[i + 1];
	dx[i + 1] = dy[i + 1];
	dy[i + 1] = dtemp;
	dtemp = dx[i + 2];
	dx[i + 2] = dy[i + 2];
	dy[i + 2] = dtemp;
/* L50: */
    }
    return 0;
} /* dswap_ */

integer idamax_(n, dx, incx)
integer *n;
real *dx;
integer *incx;
{
    /* System generated locals */
    integer ret_val, i_1;
    real d_1;

    /* Local variables */
    static real dmax_;
    static integer i, ix;

    /* Parameter adjustments */
    --dx;

    /* Function Body */
/*                                                                      
00000020*/
/*    finds the index of element having max. absolute value.            
00000030*/
/*    jack dongarra, linpack, 3/11/78.                                  
00000040*/
/*                                                                      
00000050*/
/*                                                                      
00000080*/
    ret_val = 0;
    if (*n < 1) {
	return ret_val;
    }
    ret_val = 1;
    if (*n == 1) {
	return ret_val;
    }
    if (*incx == 1) {
	goto L20;
    }
/*                                                                      
00000140*/
/*       code for increment not equal to 1                              
00000150*/
/*                                                                      
00000160*/
    ix = 1;
    dmax_ = abs(dx[1]);
    ix += *incx;
    i_1 = *n;
    for (i = 2; i <= i_1; ++i) {
	if ((d_1 = dx[ix], abs(d_1)) <= dmax_) {
	    goto L5;
	}
	ret_val = i;
	dmax_ = (d_1 = dx[ix], abs(d_1));
L5:
	ix += *incx;
/* L10: */
    }
    return ret_val;
/*                                                                      
00000270*/
/*       code for increment equal to 1                                  
00000280*/
/*                                                                      
00000290*/
L20:
    dmax_ = abs(dx[1]);
    i_1 = *n;
    for (i = 2; i <= i_1; ++i) {
	if ((d_1 = dx[i], abs(d_1)) <= dmax_) {
	    goto L30;
	}
	ret_val = i;
	dmax_ = (d_1 = dx[i], abs(d_1));
L30:
    ;}
    return ret_val;
} /* idamax_ */





