/* stepit.c
AUTHOR:
 J.P.Chandler
DATE:
 June, 1975
PURPOSE:
 Finds Local Minima Of A Smooth Function Of Several Parameters.
HISTORY:
 10.19.92 EJ Chichilnisky

 Changed CAP version of stepit substantially
 to make it re-entrant, not reliant on globals, 
 and simpler to access from other functions. 
 These are the changes:

 - Removed all routines other than: stept_(), stbeg_(),sterr_().
 The others were extraneous.
 - Changed cstep and stork structures to contain pointers, not arrays.
 - Passed pointers to cstep_1 and stork_1 structures
 in these functions, rather than access them as globals.
 Changed arlists and calls to these functions.
 - Changed all the corresponding cstep_1.xxx references to cstep_1->xxx
 since it's now a pointer everywhere. Same for stork_1.
 - Made each error function call set the FOBJ value explicitly.
 Old code relied on error function to know the location of FOBJ.
 - Removed MAX_STEPIT_NVARS stuff, passing array in of the desired size
 and referencing cstep_1->NV for sizes.
 - Removed nvmax stuff which checked that (NV< MAX_STEPIT_NVARS)
 - Created "extras" structure to pass in cleanly as substitute
 for the static arrays in stept_.
 - Removed all static variables except for format strings
 - Added s_wsfe() and other FORTRAN library substitute functions
 - Formatted things to be a little more readable, but did not touch guts of code.
 - Introduced stepit_fit() routines that does all the setup given a bunch
 of arrays and parameters that stepit will use. 
 Documentation is sparse; should search the text for original docs.
 - Made mosq a passable parameter in extras structure because
 arrays must have dimensions related to this value. 
 This is the comment in the original code that alerted me to this:
 "The dimensions of all vectors and matrices (as opposed to arrays) 
 are nv, except for ....
 err(nv,mosq),xosc(nv,mosq),fosc(mosq).
 If errors are to be calculated by subroutine sterr, however, then
 err must be dimensioned at least err(nv,max(nv,mosq))."
 - Removed sterr_() function entirely. Never used and just takes up space.

 All new routines are near the top of the file. 
 
BUGS:
 No known bugs other than the generic local minima problem.

COPYRIGHT:
 STEPIT 7.4 JUNE 1975 
 Copyright (C) 1965, 1975 J. P. Chandler 
 Computer Science Dept., Oklahoma State University, Stillwater, Oklahoma 74074) 
 
 -Stepit Is A Phlegmatic Method Of Solving A Problem.- 
 -- J. H. Burrill, Jr., 360 Stepit - A User-S Manual 
 
 This Source Deck And A Write-Up Are Available From The Quantum Chemistry Program Exchange 
 Dept. Of Chemistry, Indiana University, Bloomington, Indiana 47401 

DOCUMENTATION:

  The header of stepit74.f reads as follows:

  input quantities.....  funk,nv,ntrac,matrx,mask,x,xmax,xmin,         
                              deltx,delmn,nfmax,nflat,kw               
  output quantities....  x,fobj,err,kflag,norep,kerfl           
                                                                       
     funk       --  the name of the subroutine that computes fobj      
                    given x(1),x(2),...,x(nv) (each such          
                    subroutine must be named in an external       
                    statement in the calling program)             
     nv         --  the number of parameters, x                        
     ntrac      --  =0 for normal output, =+1 for trace output,        
                    =-1 for no output                             
     matrx      --  =0 for no error calculation, =100+m for error      
                    calculation using steps 10**m times larger    
                    than the last steps used in the minimization  
     fobj       --  the value of the function to be minimized          
     mask(j)    --  nonzero if x(j) is to be held fixed                
     x(j)       --  the j-th parameter                                 
     xmax(j)    --  the upper limit on x(j)                            
     xmin(j)    --  the lower limit on x(j)                            
     deltx(j)   --  the initial step size for x(j)                     
     delmn(j)   --  the lower limit (convergence tolerance) on the     
                    step size for x(j)                            
     err(j,k)   --  returns the error matrix if -matrx- is nonzero     
                    (err is also used for scratch storage)        
     nfmax      --  the maximum number of function computations        
     nflat      --  nonzero if the search is to terminate when all     
                    trial steps give identical function values    
     jvary      --  stepit sets jvary nonzero if x(jvary) is the only  
                    x(j) that has changed since the last call to  
                    funk (this can be used to speed up funk)      
     nxtra      --  used by subroutine simplex but not by stepit       
     kflag      --  returned .gt. zero for a normal exit,              
                    returned .lt. zero for an abnormal exit       
     norep      --  returned .gt. zero if the function was not         
                    reproducible                                  
     kerfl      --  returned .lt. zero if subroutine sterr             
                    terminated abnormally                         
     kw         --  the logical unit number of the printer             

*/

#include "math.h"
#include "f2c.h"
#include <stdio.h>

struct stork {
  real *dx, *xs, *dlx;
  integer nactv, nssw, nf;
};
struct cstep {
  float *X,*XMAX,*XMIN,*DELTX,*DELMN,*ERR,FOBJ;
  int NV, NTRAC, MATRX, *MASK, NFMAX,NFLAT, JVARY, NXTRA, KFLAG, NOREP, KERFL, KW;
};
struct extras {
  float *xosc,*salvo,*fstor,*vec,*fosc;
  int mosq;
  int *jflat;
};


/* Re-entrant stepit interface function to other C programs.
   Stepit will use the arrays that are passed in.
   Some must be sensibly set up for stepit to read,
   others are scratch space.
   Those that have meaning to stepit as input are:
   parameters,lower_bounds,upper_bounds,smallest_steps,initial_steps,masks.
   Other arrays are scratch space. 
   All arrays have the same dimension N as the parameters array
   except for the ones mentioned in the comments above.
   All arrays are float except those marked.
*/
   
float stepit_fit(funk,parameters,lower_bounds,upper_bounds,smallest_steps,
		 initial_steps,err,dx,xs,dlx,xosc,salvo,fstor,vec,fosc,
		 masks,jflat,num_parameters,max_function_calls, ntrac,mosq)
float (*funk) ();
float *parameters,*lower_bounds,*upper_bounds,
  *smallest_steps,*initial_steps,*err,
  *dx,*xs,*dlx,*xosc,*salvo,*fstor,*vec,*fosc;
int *masks, *jflat, num_parameters,max_function_calls,ntrac,mosq;
{
  struct cstep the_cstep;
  struct stork the_stork;
  struct extras the_extras;

  /* Set up the data structures used by the internal code */
  the_extras.xosc = xosc;
  the_extras.jflat = jflat;	/* integer */
  the_extras.salvo = salvo;
  the_extras.fstor = fstor;
  the_extras.vec = vec;
  the_extras.fosc = fosc;
  the_extras.mosq = mosq;	/* 5 standard */

  the_stork.dx = dx;
  the_stork.xs = xs;
  the_stork.dlx = dlx;

  the_cstep.X = parameters;
  the_cstep.XMIN = lower_bounds;
  the_cstep.XMAX = upper_bounds;
  the_cstep.DELMN = smallest_steps;
  the_cstep.DELTX = initial_steps;
  the_cstep.MASK = masks;	/* integer */
  the_cstep.ERR = err;

  the_cstep.FOBJ = 0.0;
  the_cstep.NFMAX = max_function_calls;
  the_cstep.MATRX = 0;	/* 0 standard */
  the_cstep.NTRAC = ntrac;	/* -1 standard */
  the_cstep.KW = 0; 
  the_cstep.NV = num_parameters; 

  /* Do the fit */
  stept_(funk,&the_cstep,&the_stork,&the_extras);

  /* Return the lowest error function value */
  return(the_cstep.FOBJ);
}

/* Dummy substitutes for FORTRAN calls.
   When stepit encounters an error, such as an error function that does not
   return the same result when handed the same arguments twice,
   it spits out a message to the screen. To do this, it uses Fortran library calls.
   One can avoid loading the Fortran libraries by defining dummy functions, 
   like the ones below, that will generate an uninformative error 
   when stepit calls them. This is not useful for debugging, 
   but it means the stepit code is portable.
   The Fortran libraries omitted are lF77 lI77.
*/

int s_wsfe(c)
cilist *c;
{
 fprintf(stderr,"Unknown error in stepit.\nFormat string is: %s\n",c->cifmt);
}
int e_wsfe()
{
 fprintf(stderr,"Unknown error in stepit.\n");
}
int do_fio()
{
 fprintf(stderr,"Unknown error in stepit.\n");
}

/**********************************************************************************/
/**********************************************************************************/
/**********************************************************************************/


static integer c__1 = 1;

int datsw_(nssw, jump)
integer *nssw;
integer *jump;
{
  /* DUMMYSW2*/
  /* DUMMY VERSION OF SUBROUTINE DATSW (ALL SWITCHES PERMANENTLY OFF). 
     DUMMYSW3*/
  /* DUMMYSW4*/
  *jump = 2;
  return 0;
}				/* datsw_ */



int stbeg_(funk,cstep_1,stork_1)
float (*funk) ();
struct cstep *cstep_1;
struct stork *stork_1;
{
  /* Format strings */
  static char fmt_1020[] = 
    "(/\002 TURN OFF SENSE SWITCH \002i2//\002 \002)";
  static char fmt_1280[] = "(\0021SUBROUTINE STEPIT. COPYRIGHT (C) 1965 J. P. CHANDLER\002,//\002 INITIAL VALUES....\002/\002 \002)";
  static char fmt_1290[] = "(/\002 MASK = \002i7,7i13/(4x8i13))";
  static char fmt_1300[] = "(/\002 X = \0028e13.5/(10x8e13.5))";
  static char fmt_1310[] = "(/\002 XMAX = \0028e13.5/(10x8e13.5))";
  static char fmt_1320[] = "(/\002 XMIN = \0028e13.5/(10x8e13.5))";
  static char fmt_1330[] = "(/\002 DELTX = \0028e13.5/(10x8e13.5))";
  static char fmt_1340[] = "(/\002 DELMN = \0028e13.5/(10x8e13.5))";
  static char fmt_1370[] = "(////\002 WARNING.... FOBJ IS NOT A REPRODUCIBLE\002,\002 FUNCTION OF X(J).\0027x,\002 NF = \002i5//5x3e23.15)";
  static char fmt_1400[] = "(//1xi3,\002 VARIABLES,\002i3,\002 ACTIVE.\0029x,\002 MATRX =\002,i4,9x,\002 NFMAX =\002,i8,9x,\002 NFLAT =\002,i2,9x,\002 RELAC =\002,e11.4///,\002 FOBJ =\002e18.10///\002 BEGIN MINIMIZATION....\002///,\002 \002)";

  /* System generated locals */
  integer i_1;
  real r_1;

  /* Builtin functions */
  integer s_wsfe(), do_fio(), e_wsfe();

  /* Local variables */
  real huge, rten;
  integer jump, j;
  real deldf, relac, fsave;
  extern int datsw_();
  integer ktype;
  real rzero, runit, unitr, xplus;

  /* Fortran I/O blocks */
  static cilist io__13 = { 0, 0, 0, fmt_1020, 0 };
  static cilist io__17 = { 0, 0, 0, fmt_1280, 0 };
  static cilist io__18 = { 0, 0, 0, fmt_1290, 0 };
  static cilist io__19 = { 0, 0, 0, fmt_1300, 0 };
  static cilist io__20 = { 0, 0, 0, fmt_1310, 0 };
  static cilist io__21 = { 0, 0, 0, fmt_1320, 0 };
  static cilist io__22 = { 0, 0, 0, fmt_1330, 0 };
  static cilist io__23 = { 0, 0, 0, fmt_1340, 0 };
  static cilist io__25 = { 0, 0, 0, fmt_1370, 0 };
  static cilist io__26 = { 0, 0, 0, fmt_1400, 0 };


  /* STBEG 2*/
  /* STBEG 1.2 A.N.S.I. STANDARD FORTRAN JUNE 1975 STBEG 3*/
  /* COPYRIGHT (C) 1965, 1975 J. P. CHANDLER  STBEG 4*/
  /* STBEG 5*/
  /* STBEG SETS DEFAULT VALUES AND PRINTS INITIAL OUTPUT FOR STEPIT. STBEG 6*/
  /* THE CALL TO STBEG IS THE FIRST EXECUTABLE STATEMENT IN STEPIT, TO STBEG 7*/
  /* FACILITATE OVERLAYING IF NECESSARY.  STBEG 8*/
  /* STBEG 9*/
  /** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * STBEG 10*/
  /* STBEG 11*/
  /* INPUT QUANTITIES..... FUNK,X,XMAX,XMIN,DELTX,DELMN,NV,NTRAC, STBEG 12*/
  /* MATRX,MASK,NFMAX,NFLAT,KW STBEG 13*/
  /* OUTPUT QUANTITIES.... NSSW,NACTV,NF,KFLAG,NOREP, STBEG 14*/
  /* AND SOMETIMES X,XMAX,XMIN,DELTX,DELMN STBEG 15*/
  /* STBEG 16*/
  /* DOUBLE PRECISION X,XMAX,XMIN,DELTX,DELMN,ERR,FOBJ,DX,XS,DLX STBEG 17*/
  /* DOUBLE PRECISION HUGE,DELDF,RZERO,RUNIT,RTEN,RELAC,XPLUS,FSAVE, STBEG 18*/
  /* * UNITR  STBEG 19*/
  /* STBEG 20*/
  /* USER COMMON.....  STBEG 21*/
  /* STBEG 25*/
  /* INTERNAL STEPIT COMMON.....  STBEG 26*/
  /* STBEG 28*/
  /* THE REAL FORMAT SPECIFICATIONS USED ARE E13.5, E23.15, E11.4, 
     E18.10.STBEG 29*/
  /* STBEG 30*/
  /** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * STBEG 31*/
  /* STBEG 32*/
  /* SET FIXED QUANTITIES ....  STBEG 33*/
  /* STBEG 34*/
  /* KTYPE ... CONSOLE TYPEWRITER UNIT NUMBER STBEG 35*/
  /* (IRRELEVANT IF A DUMMY DATSW IS USED) STBEG 36*/
  ktype = 1;
  /* NSSW ... TERMINATION SENSE SWITCH NUMBER STBEG 38*/
  /* (IRRELEVANT IF A DUMMY DATSW IS USED) STBEG 39*/
  stork_1->nssw = 6;
  /* HUGE ... A VERY LARGE REAL NUMBER STBEG 41*/
  /* (DEFAULT VALUE FOR XMAX AND -XMIN) STBEG 42*/
  huge = (float)1e35;

  /* DELDF ... DEFAULT VALUE FOR DELTX(J) STBEG 46*/
  deldf = (float).01;
  /* STBEG 48*/
  rzero = (float)0.;
  runit = (float)1.;
  unitr = (float)1.;
  rten = (float)10.;
  /* STBEG 53*/
  /** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * STBEG 54*/
  /* STBEG 55*/
  /* NO REAL CONSTANTS ARE USED BEYOND THIS POINT. STBEG 56*/
  /* CHECK SOME INPUT QUANTITIES, AND SET THEM TO DEFAULT VALUES IF STBEG 57*/
  /* DESIRED. STBEG 58*/
  /* STBEG 59*/
  cstep_1->KFLAG = 0;
  cstep_1->NOREP = 0;
  /* MAKE SURE THE SENSE SWITCH IS OFF. STBEG 62*/
  jump = 2;
  datsw_(&stork_1->nssw, &jump);
  if (jump - 1 <= 0) {
    goto L1010;
  } else {
    goto L1040;
  }
  /* STBEG 66*/
  /* ONLY USAGE OF THE CONSOLE TYPEWRITER.... STBEG 67*/
 L1010:
  io__13.ciunit = ktype;
  s_wsfe(&io__13);
  do_fio(&c__1, (char *)&stork_1->nssw, (ftnlen)sizeof(integer));
  e_wsfe();
 L1030:
  datsw_(&stork_1->nssw, &jump);
  if (jump - 1 <= 0) {
    goto L1030;
  } else {
    goto L1040;
  }
  /* STBEG 73*/
  /* COMPUTE RELAC, THE RELATIVE PRECISION OF THE MACHINE AND ARITHMETIC STBEG 74*/
  /* BEING USED. RELAC IS USED IN SETTING DELMN(J) TO A DEFAULT VALUE. STBEG 75*/
  /* STBEG 76*/
 L1040:
  relac = runit;
 L1050:
  relac /= rten;
  xplus = runit + relac;
  if (xplus - unitr <= (float)0.) {
    goto L1060;
  } else {
    goto L1050;
  }
  /* STBEG 81*/
  /* NACTV ... NUMBER OF ACTIVE X(J) STBEG 82*/
 L1060:
  stork_1->nactv = 0;
  cstep_1->FOBJ = rzero;
  if (cstep_1->NV <= 0) {
    goto L1250;
  } else {
    goto L1070;
  }
 L1070:
  /* Removed nvmax reference. EJC */
  goto L1080;
 L1080:
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    if (cstep_1->MASK[j - 1] != 0) {
      goto L1240;
    } else {
      goto L1090;
    }
    /* 
      STBEG 89*/
    /* CHECK THAT DELTX(J) IS NOT NEGLIGIBLE. 
       STBEG 90*/
  L1090:
    if (cstep_1->DELTX[j - 1] != (float)0.) {
      goto L1100;
    } else {
      goto L1120;
    }
  L1100:
    xplus = cstep_1->X[j - 1] + cstep_1->DELTX[j - 1];
    if (xplus - cstep_1->X[j - 1] != (float)0.) {
      goto L1110;
    } else {
      goto L1120;
    }
  L1110:
    xplus = cstep_1->X[j - 1] - cstep_1->DELTX[j - 1];
    if (xplus - cstep_1->X[j - 1] != (float)0.) {
      goto L1150;
    } else {
      goto L1120;
    }
  L1120:
    if (cstep_1->X[j - 1] != (float)0.) {
      goto L1130;
    } else {
      goto L1140;
    }
  L1130:
    cstep_1->DELTX[j - 1] = deldf * cstep_1->X[j - 1];
    goto L1150;
  L1140:
    cstep_1->DELTX[j - 1] = deldf;
  L1150:
    if ((r_1 = cstep_1->DELMN[j - 1]) < (float)0.) {
      goto L1170;
    } else if (r_1 == 0) {
      goto L1160;
    } else {
      goto L1180;
    }
  L1160:
    cstep_1->DELMN[j - 1] = cstep_1->DELTX[j - 1] * relac;
    if (cstep_1->DELMN[j - 1] >= (float)0.) {
      goto L1180;
    } else {
      goto L1170;
    }
  L1170:
    cstep_1->DELMN[j - 1] = -(doublereal)cstep_1->DELMN[j - 1];
  L1180:
    if (cstep_1->XMAX[j - 1] - cstep_1->XMIN[j - 1] <= (float)0.) {
      goto L1190;
    } else {
      goto L1200;
    }
  L1190:
    cstep_1->XMAX[j - 1] = huge;
    cstep_1->XMIN[j - 1] = -(doublereal)huge;
  L1200:
    ++stork_1->nactv;
    /* X(J)=AMAX1(XMIN(J),AMIN1(XMAX(J),X(J))) 
       STBEG108*/
    if (cstep_1->X[j - 1] - cstep_1->XMAX[j - 1] <= (float)0.) {
      goto L1220;
    } else {
      goto L1210;
    }
  L1210:
    cstep_1->X[j - 1] = cstep_1->XMAX[j - 1];
  L1220:
    if (cstep_1->X[j - 1] - cstep_1->XMIN[j - 1] >= (float)0.) {
      goto L1240;
    } else {
      goto L1230;
    }
  L1230:
    cstep_1->X[j - 1] = cstep_1->XMIN[j - 1];
  L1240:
    ;}
  /* STBEG114*/
  if (stork_1->nactv <= 0) {
    goto L1250;
  } else {
    goto L1260;
  }
 L1250:
  cstep_1->KFLAG = -1;
  goto L1390;
  /* STBEG118*/
  /** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * STBEG119*/
  /* STBEG120*/
 L1260:
  if (cstep_1->NTRAC >= 0) {
    goto L1270;
  } else {
    goto L1350;
  }
 L1270:
  io__17.ciunit = cstep_1->KW;
  s_wsfe(&io__17);
  e_wsfe();
  io__18.ciunit = cstep_1->KW;
  s_wsfe(&io__18);
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&cstep_1->MASK[j - 1], (ftnlen)sizeof(integer));
  }
  e_wsfe();
  io__19.ciunit = cstep_1->KW;
  s_wsfe(&io__19);
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&cstep_1->X[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
  io__20.ciunit = cstep_1->KW;
  s_wsfe(&io__20);
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&cstep_1->XMAX[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
  io__21.ciunit = cstep_1->KW;
  s_wsfe(&io__21);
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&cstep_1->XMIN[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
  io__22.ciunit = cstep_1->KW;
  s_wsfe(&io__22);
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&cstep_1->DELTX[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
  io__23.ciunit = cstep_1->KW;
  s_wsfe(&io__23);
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&cstep_1->DELMN[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
  /* STBEG138*/
 L1350:
  cstep_1->JVARY = 0;
  cstep_1->FOBJ = (*funk)();
  fsave = cstep_1->FOBJ;
  cstep_1->FOBJ = (*funk)();
  /* NF ... NUMBER OF CALLS TO FUNK STBEG143*/
  stork_1->nf = 2;
  if (cstep_1->FOBJ - fsave != (float)0.) {
    goto L1360;
  } else {
    goto L1380;
  }
 L1360:
  cstep_1->NOREP = 1;
  io__25.ciunit = cstep_1->KW;
  s_wsfe(&io__25);
  do_fio(&c__1, (char *)&stork_1->nf, (ftnlen)sizeof(integer));
  do_fio(&c__1, (char *)&fsave, (ftnlen)sizeof(real));
  do_fio(&c__1, (char *)&cstep_1->FOBJ, (ftnlen)sizeof(real));
  e_wsfe();
  /* STBEG150*/
 L1380:
  if (cstep_1->NTRAC >= 0) {
    goto L1390;
  } else {
    goto L1440;
  }
 L1390:
  io__26.ciunit = cstep_1->KW;
  s_wsfe(&io__26);
  do_fio(&c__1, (char *)&cstep_1->NV, (ftnlen)sizeof(integer));
  do_fio(&c__1, (char *)&stork_1->nactv, (ftnlen)sizeof(integer));
  do_fio(&c__1, (char *)&cstep_1->MATRX, (ftnlen)sizeof(integer));
  do_fio(&c__1, (char *)&cstep_1->NFMAX, (ftnlen)sizeof(integer));
  do_fio(&c__1, (char *)&cstep_1->NFLAT, (ftnlen)sizeof(integer));
  do_fio(&c__1, (char *)&relac, (ftnlen)sizeof(real));
  do_fio(&c__1, (char *)&cstep_1->FOBJ, (ftnlen)sizeof(real));
  e_wsfe();
  /* STBEG158*/
 L1440:
  return 0;
  /* END STBEG.  STBEG 160*/
}				/* stbeg_ */





int stept_(funk,cstep_1, stork_1,extras_1)
float (*funk) ();
struct cstep *cstep_1;
struct stork *stork_1;
struct extras *extras_1;
{
  /* Format strings */
  static char fmt_2550[] = "(/\002 STEP SIZE\002i3,\002 INCREASED TO \002e13.5)";
  static char fmt_2780[] = "(/\002 ********\0024x\002 GIGANTIC STEP WITH PERIOD\002,i2,\002 BEING ATTEMPTED. COXCM, COSIN = \0022e13.5)";
  static char fmt_2930[] = "(/\002 NO. OF STEPS = \0029e11.3/(16x9e11.3))";
  static char fmt_2940[] = "(/\002 FOBJ =\002e16.8,7x,\002 NF = \002,i7,15x,\002 X(J)....\002/,(1x7e16.8))";
  static char fmt_3080[] = "(//\002 FOBJ =\002,e16.8,\002 AFTER\002,i3,\002 GIANT STEP(S).\002,10x,\002 X(J)....\002//(1x7e16.8))";
  static char fmt_3140[] = "(//\002 FOBJ =\002,e16.8,\002 AFTER\002,e11.3,\002 GIANT STEPS.\0024x,\002 X(J)....\002//(1x7e16.8))";
  static char fmt_3260[] = "(\002 \002)";
  static char fmt_3400[] = "(///\002 TERMINATED WHEN THE STEP SIZES\002,\002 BECAME AS SMALL AS THE DELMN(J).\002)";
  static char fmt_3480[] = "(///\002 TERMINATED WHEN THE FUNCTION VALUES\002,\002 AT ALL TRIAL POINTS WERE IDENTICAL.\002)";
  static char fmt_3490[] = "(///\002 CURRENT STEP SIZES....\002//(1x9e13.5))";
  static char fmt_3520[] = "(//60(\002 *\002)//\002 STEP SIZES REDUCED TO....\002//(1x9e13.5))";
  static char fmt_3540[] = "(///\002 ABNORMAL TERMINATION.... MORE THAN NFMAX = \002i7,\002 CALLS TO THE FOBJ SUBROUTINE.\002)";
  static char fmt_3560[] = "(///\002 ABNORMAL TERMINATION.... TERMINATED BY\002,\002 OPERATOR VIA SENSE SWITCH \002i2)";
  static char fmt_3610[] = "(////\002 WARNING.... FOBJ IS NOT A REPRODUCIBLE\002,\002 FUNCTION OF X(J).\0027x,\002 NF = \002i5//5x3e23.15)";
  static char fmt_3640[] = "(/////1xi6,\002 FUNCTION COMPUTATIONS \002,///,\002 FINAL VALUE OF FOBJ = \002,e23.15,///,9x,\002 FINAL VALUES OF X(J)....\002//(1x5e23.15))";

  /* System generated locals */
  integer i_1, i_2, i_3;
  real r_1, r_2;

  /* Builtin functions */
  integer s_wsfe(), do_fio(), e_wsfe();
  double sqrt(), pow_ri();

  /* Local variables */
  integer nack;
  real avec;
  integer matd, jock;
  integer nosc;
  real rten;
  integer jump, kwit, nzip, nstp;
  real sumv;
  integer j, k, nflag, ngian;
  real facup;
  integer ncirc;
  real denom;
  extern int stbeg_();
  real fsave, fbest, coxcm, cosin;
  integer nfsav;
  real cindr;
  integer mnosc, ngate;
  real fprev;
  extern int datsw_();
  real xsave;
  integer jflmn, nretr;
  real steps, rzero, stcut;
  integer nonzr, mxstp;
  real xplus;
  integer kl, jx, nt;
  real fac, ack, del;
  integer nah;
  real adx, dfu, dfz, dxu, dxz;
  real *xosc = extras_1->xosc;
  int *jflat = extras_1->jflat;
  real *salvo = extras_1->salvo;
  real *fstor = extras_1->fstor;
  real *vec = extras_1->vec;
  real *fosc = extras_1->fosc;
  int mosq = extras_1->mosq;

  /* Fortran I/O blocks */
  static cilist io__61 = { 0, 0, 0, fmt_2550, 0 };
  static cilist io__73 = { 0, 0, 0, fmt_2780, 0 };
  static cilist io__78 = { 0, 0, 0, fmt_2930, 0 };
  static cilist io__79 = { 0, 0, 0, fmt_2940, 0 };
  static cilist io__80 = { 0, 0, 0, fmt_2940, 0 };
  static cilist io__82 = { 0, 0, 0, fmt_3080, 0 };
  static cilist io__84 = { 0, 0, 0, fmt_3140, 0 };
  static cilist io__85 = { 0, 0, 0, fmt_2930, 0 };
  static cilist io__86 = { 0, 0, 0, fmt_2940, 0 };
  static cilist io__87 = { 0, 0, 0, fmt_3260, 0 };
  static cilist io__88 = { 0, 0, 0, fmt_2930, 0 };
  static cilist io__89 = { 0, 0, 0, fmt_2940, 0 };
  static cilist io__91 = { 0, 0, 0, fmt_3400, 0 };
  static cilist io__93 = { 0, 0, 0, fmt_3480, 0 };
  static cilist io__94 = { 0, 0, 0, fmt_3490, 0 };
  static cilist io__95 = { 0, 0, 0, fmt_3520, 0 };
  static cilist io__96 = { 0, 0, 0, fmt_3540, 0 };
  static cilist io__97 = { 0, 0, 0, fmt_3560, 0 };
  static cilist io__98 = { 0, 0, 0, fmt_3490, 0 };
  static cilist io__99 = { 0, 0, 0, fmt_3610, 0 };
  static cilist io__100 = { 0, 0, 0, fmt_3640, 0 };


  /* STEPIT 2*/
  /* NOREP -- RETURNED .GT. ZERO IF THE FUNCTION WAS NOT STEPIT53*/
  /* REPRODUCIBLE  STEPIT54*/
  /* KERFL -- RETURNED .LT. ZERO IF SUBROUTINE STERR STEPIT55*/
  /* TERMINATED ABNORMALLY STEPIT56*/
  /* KW -- THE LOGICAL UNIT NUMBER OF THE PRINTER STEPIT57*/
  /* STEPIT58*/
  /** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * STEPIT59*/
  /* STEPIT60*/
  /* STEPIT62*/
  /* THE FOLLOWING STATEMENTS CONVERT STEPIT TO DOUBLE PRECISION. STEPIT63*/
  /* STEPIT CONTAINS NO MIXED MODE STATEMENTS, NO MATTER WHETHER THE STEPIT64*/
  /* VARIABLES BEGINNING WITH A-H AND O-Z ARE ALL TYPE REAL OR ARE STEPIT65*/
  /* ALL TYPE DOUBLE PRECISION.  STEPIT66*/
  /* STEPIT67*/
  /* DOUBLE PRECISION X,XMAX,XMIN,DELTX,DELMN,ERR,FOBJ, STEPIT68*/
  /* X VEC,DLX,XS,FSTOR,DX,SALVO,XOSC,FOSC,Q,STCUT,ACK,FACUP STEPIT69*/
  /* DOUBLE PRECISION RZERO,RTEN,DELX,XPLUS, STEPIT70*/
  /* X FSAVE,FBEST,XSAVE,ADX,FPREV,DENOM,DEL,DXZ,DXU,DFZ,DFU, STEPIT71*/
  /* X AVEC,SUMV,CINDR,COXCM,COSIN,STEPS,FAC,QSQRT,DSQRT STEPIT72*/
  /* STEPIT73*/
  /* THE DIMENSIONS OF ALL VECTORS AND MATRICES (AS OPPOSED TO ARRAYS) STEPIT74*/
  /* ARE NV, EXCEPT FOR ....  STEPIT75*/
  /* ERR(NV,MOSQ),XOSC(NV,MOSQ),FOSC(MOSQ). STEPIT76*/
  /* IF ERRORS ARE TO BE CALCULATED BY SUBROUTINE STERR, HOWEVER, THEN STEPIT77*/
  /* ERR MUST BE DIMENSIONED AT LEAST ERR(NV,MAX(NV,MOSQ)) . STEPIT78*/
  /* STEPIT79*/
  /* STEPIT82*/
  /* IF UNLABELLED COMMON AND SINGLE PRECISION ARE USED AND THE EXTERNAL STEPIT83*/
  /* FACILITY IS NOT USED, STEPIT IS THEN WRITTEN ENTIRELY IN STEPIT84*/
  /* A.N.S.I. STANDARD BASIC FORTRAN.  STEPIT85*/
  /* STEPIT86*/
  /* USER COMMON.....  STEPIT87*/
  /* STEPIT91*/
  /* INTERNAL STEPIT COMMON.....  STEPIT92*/
  /* STEPIT94*/
  /* SET THE LIBRARY FUNCTION FOR SINGLE PRECISION (SQRT) OR FOR STEPIT95*/
  /* DOUBLE PRECISION (DSQRT). NO OTHER FUNCTIONS ARE USED, EITHER STEPIT96*/
  /* EXTERNAL OR INTRINSIC, EXCEPT THE ROUTINE INVOKED BY REAL**INTEGER. STEPIT97*/
  /* THE ONLY SUBROUTINES CALLED ARE FUNK, STBEG, STERR, AND DATSW. STEPIT98*/
  /* STEPIT TERMINATES IF SENSE SWITCH NUMBER -NSSW- IS ON. STEPIT99*/
  /* THE STATEMENT CALL DATSW(NSSW,JUMP) RETURNS JUMP=1 IF STEPI100*/
  /* SENSE SWITCH NUMBER -NSSW- IS ON, AND JUMP=2 IF IT IS OFF. STEPI101*/
  /* IF NO SENSE SWITCH IS TO BE USED, SUPPLY A DUMMY ROUTINE FOR DATSW. STEPI102*/
  /* STEPI103*/
  /* QSQRT(Q)=DSQRT(Q)  STEPI105*/
  /* STEPI106*/
  /* THE REAL FORMAT SPECIFICATIONS USED ARE E13.5, E16.8, E11.3, E23.15. STEPI107*/
  /* STEPI108*/
  /** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * STEPI109*/
  /* STEPI110*/
  /* CALL STBEG TO SET DEFAULT VALUES AND PRINT INITIAL OUTPUT. STEPI111*/
  /* STEPI112*/
  stbeg_(funk,cstep_1,stork_1);
  if (cstep_1->KFLAG >= 0) {
    goto L2010;
  } else {
    goto L3710;
  }
  /* FSAVE ... USED TO CHECK REPRODUCIBILITY STEPI115*/
 L2010:
  fsave = cstep_1->FOBJ;
  /* STEPI117*/
  /* SET FIXED QUANTITIES ....  STEPI118*/
  /* STEPI119*/
  /* MXSTP ... LOG2(MAXIMUM NUMBER OF STEPS) STEPI120*/
  mxstp = 5;
  /* FACUP ... IF MORE THAN FACUP STEPS ARE STEPI122*/
  /* TAKEN, THE STEP SIZE IS INCREASED STEPI123*/
  facup = (float)4.;
  /* ACK ... RATIO OF STEP SIZE INCREASE STEPI125*/
  ack = (float)2.;
  /* STCUT ... RATIO OF STEP SIZE DECREASE STEPI127*/
  stcut = (float)10.;

  /* MOSQ ... MAXIMUM DEPTH OF OSCILLATION STEPI129*/
  /* SEARCH  STEPI130*/
  /* mosq = 5; */
  /* Passed to this routine now. EJC. 10.19.92 */

  /* MNOSC ... MINIMUM OSCILLATION PERIOD STEPI132*/
  mnosc = 2;
  /* STEPI134*/
  rzero = (float)0.;
  rten = (float)10.;
  /* STEPI137*/
  /* NO REAL CONSTANTS ARE USED BEYOND THIS POINT. STEPI138*/
  /* STEPI139*/
  cstep_1->KERFL = 0;
  /* JOCK ... SWITCH USED IN SETTING JVARY STEPI141*/
  jock = 1;
  /* JUMP ... FLAG SET BY SUBROUTINE DATSW STEPI143*/
  jump = 2;
  /* NOSC ... CURRENT DEPTH OF THE OSCILLATION STEPI145*/
  /* INFORMATION STEPI146*/
  nosc = 0;
  /* KWIT ... TERMINATION SWITCH STEPI148*/
  kwit = 0;
  /* FBEST ... BEST PREVIOUS VALUE OF FOBJ STEPI150*/
  fbest = cstep_1->FOBJ;
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    /* DX(J) ... CURRENT STEP SIZE FOR X(J) 
       STEPI153*/
    /* L2020: */
    stork_1->dx[j - 1] = cstep_1->DELTX[j - 1];
  }
  /* STEPI155*/
  /** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * STEPI156*/
  /* STEPI157*/
  /* VARY THE PARAMETERS ONE AT A TIME.  STEPI158*/
  /* THIS IS THE STARTING POINT USED EACH TIME THE STEP SIZE IS REDUCED STEPI159*/
  /* OR A SUCCESSFUL GIANT STEP IS COMPLETED.  STEPI160*/
  /* STEPI161*/
  /* NCIRC ... NUMBER OF CONSECUTIVE X(JX) STEPI162*/
  /* WITHOUT SIZABLE CHANGES STEPI163*/
 L2030:
  ncirc = 0;
  /* NZIP ... NUMBER OF CONSECUTIVE CYCLES STEPI165*/
  /* WITHOUT A GIANT STEP STEPI166*/
  nzip = 0;
  /* STEPI168*/
  /* MAIN DO LOOP FOR CYCLING THROUGH THE VARIABLES.... STEPI169*/
  /* THE FIRST TRIAL STEP WITH EACH VARIABLE IS SEPARATE. STEPI170*/
  /* STEPI171*/
  /* NACK ... NUMBER OF ACTIVE X(JX) CYCLED STEPI172*/
  /* THROUGH  STEPI173*/
 L2040:
  nack = 0;
  i_1 = cstep_1->NV;
  for (jx = 1; jx <= i_1; ++jx) {
    /* JFLAT(JX) ... NONZERO IF CHANGING X(JX) 
       STEPI176*/
    /* DID NOT CHANGE FOBJ 
       STEPI177*/
    jflat[jx - 1] = 0;
    /* VEC(J) ... CURRENT VECTOR OF NUMBER OF 
       STEPI179*/
    /* STEPS IN X(J) 
       STEPI180*/
    vec[jx - 1] = rzero;
    /* DLX(JX) ... CHANGE IN X(JX) 
       STEPI182*/
    stork_1->dlx[jx - 1] = rzero;
    if (cstep_1->MASK[jx - 1] != 0) {
      goto L2050;
    } else {
      goto L2060;
    }
  L2050:
    vec[jx - 1] = -(doublereal)rzero;
    jflat[jx - 1] = 1;
    goto L3190;
  L2060:
    ++nack;
    /* ADX=ABS(DX(JX)) 
       STEPI189*/
    adx = stork_1->dx[jx - 1];
    if (adx >= (float)0.) {
      goto L2080;
    } else {
      goto L2070;
    }
  L2070:
    adx = -(doublereal)adx;
    /* CHECK THAT DX(JX) IS NOT NEGLIGIBLE. 
       STEPI193*/
  L2080:
    xsave = cstep_1->X[jx - 1];
    xplus = xsave + stork_1->dx[jx - 1];
    if (xplus - xsave != (float)0.) {
      goto L2090;
    } else {
      goto L2100;
    }
  L2090:
    xplus = xsave - stork_1->dx[jx - 1];
    if (xplus - xsave != (float)0.) {
      goto L2110;
    } else {
      goto L2100;
    }
  L2100:
    jflat[jx - 1] = 2;
    goto L2300;
    /* STEP X(JX). 
       STEPI201*/
  L2110:
    cstep_1->X[jx - 1] = xsave + stork_1->dx[jx - 1];
    cstep_1->JVARY = 0;
    if (jock <= 0) {
      goto L2130;
    } else {
      goto L2120;
    }
  L2120:
    jock = 0;
    cstep_1->JVARY = jx;
    /* NFLAG ... COUNTER USED IN SETTING JFLAG(
       J)STEPI207*/
  L2130:
    nflag = 1;
    if (cstep_1->X[jx - 1] - cstep_1->XMIN[jx - 1] >= (float)0.) {
      goto L2140;
    } else {
      goto L2150;
    }
  L2140:
    if (cstep_1->X[jx - 1] - cstep_1->XMAX[jx - 1] <= (float)0.) {
      goto L2160;
    } else {
      goto L2150;
    }
  L2150:
    nflag += 3;
    goto L2180;
  L2160:
    cstep_1->FOBJ = (*funk)();
    ++stork_1->nf;
    cstep_1->JVARY = jx;
    fprev = cstep_1->FOBJ;
    if ((r_1 = cstep_1->FOBJ - fbest) < (float)0.) {
      goto L2340;
    } else if (r_1 == 0) {
      goto L2170;
    } else {
      goto L2180;
    }
  L2170:
    ++nflag;
    /* STEP X(JX) THE OTHER WAY. 
       STEPI219*/
  L2180:
    xplus = cstep_1->X[jx - 1];
    cstep_1->X[jx - 1] = xsave - stork_1->dx[jx - 1];
    if (cstep_1->X[jx - 1] - cstep_1->XMIN[jx - 1] >= (float)0.) {
      goto L2190;
    } else {
      goto L2310;
    }
  L2190:
    if (cstep_1->X[jx - 1] - cstep_1->XMAX[jx - 1] <= (float)0.) {
      goto L2200;
    } else {
      goto L2310;
    }
  L2200:
    cstep_1->FOBJ = (*funk)();
    ++stork_1->nf;
    cstep_1->JVARY = jx;
    if ((r_1 = cstep_1->FOBJ - fbest) < (float)0.) {
      goto L2330;
    } else if (r_1 == 0) {
      goto L2210;
    } else {
      goto L2220;
    }
  L2210:
    ++nflag;
  L2220:
    if ((i_2 = nflag - 3) < 0) {
      goto L2230;
    } else if (i_2 == 0) {
      goto L2290;
    } else {
      goto L2310;
    }
    /* 
      STEPI230*/
    /* PERFORM PARABOLIC INTERPOLATION. 
       STEPI231*/
    /* 
      STEPI232*/
  L2230:
    denom = cstep_1->FOBJ - fbest - (fbest - fprev);
    if (denom != (float)0.) {
      goto L2240;
    } else {
      goto L2310;
    }
  L2240:
    stork_1->dlx[jx - 1] = -(doublereal)stork_1->dx[jx - 1] * (cstep_1->FOBJ 
							       - fprev) / (denom + denom);
    vec[jx - 1] = stork_1->dlx[jx - 1] / adx;
    cstep_1->X[jx - 1] = xsave + stork_1->dlx[jx - 1];
    if (cstep_1->X[jx - 1] - xsave != (float)0.) {
      goto L2260;
    } else {
      goto L2250;
    }
  L2250:
    cstep_1->FOBJ = fbest;
    goto L2280;
  L2260:
    cstep_1->FOBJ = (*funk)();
    ++stork_1->nf;
    if (cstep_1->FOBJ - fbest >= (float)0.) {
      goto L2280;
    } else {
      goto L2270;
    }
  L2270:
    fbest = cstep_1->FOBJ;
    jock = 1;
    goto L2320;
  L2280:
    stork_1->dlx[jx - 1] = rzero;
    vec[jx - 1] = rzero;
    goto L2310;
  L2290:
    jflat[jx - 1] = 1;
  L2300:
    vec[jx - 1] = -(doublereal)rzero;
  L2310:
    cstep_1->X[jx - 1] = xsave;
  L2320:
    ++ncirc;
    if (ncirc - stork_1->nactv >= 0) {
      goto L3280;
    } else {
      goto L2450;
    }
    /* 
      STEPI255*/
    /* FLIP DX(JX) FOR MORE EFFICIENT 
       OPERATION. STEPI256*/
  L2330:
    stork_1->dx[jx - 1] = -(doublereal)stork_1->dx[jx - 1];
    /* 
      STEPI258*/
    /* A LOWER VALUE OF FOBJ HAS BEEN FOUND. STEP, INCREASE THE STEP 
       SIZE, STEPI259*/
    /* AND REPEAT AS LONG AS FOBJ DECREASES, UP TO MXSTP TIMES. 
       STEPI260*/
    /* 
      STEPI261*/
  L2340:
    ncirc = 0;
    nstp = 0;
    del = stork_1->dx[jx - 1];
  L2350:
    fprev = fbest;
    fbest = cstep_1->FOBJ;
    vec[jx - 1] += del / adx;
    stork_1->dlx[jx - 1] += del;
    ++nstp;
    if (nstp - mxstp >= 0) {
      goto L2430;
    } else {
      goto L2360;
    }
  L2360:
    del = ack * del;
    xplus = xsave;
    xsave = cstep_1->X[jx - 1];
    cstep_1->X[jx - 1] = xsave + del;
    if (cstep_1->X[jx - 1] - cstep_1->XMIN[jx - 1] >= (float)0.) {
      goto L2370;
    } else {
      goto L2440;
    }
  L2370:
    if (cstep_1->X[jx - 1] - cstep_1->XMAX[jx - 1] <= (float)0.) {
      goto L2380;
    } else {
      goto L2440;
    }
  L2380:
    cstep_1->FOBJ = (*funk)();
    ++stork_1->nf;
    if (cstep_1->FOBJ - fbest >= (float)0.) {
      goto L2390;
    } else {
      goto L2350;
    }
    /* 
      STEPI280*/
    /* PERFORM PARABOLIC INTERPOLATION. 
       STEPI281*/
  L2390:
    dxz = xsave - xplus;
    dxu = cstep_1->X[jx - 1] - xsave;
    dfz = fbest - fprev;
    dfu = cstep_1->FOBJ - fbest;
    denom = dfz * dxu - dfu * dxz;
    if (denom != (float)0.) {
      goto L2400;
    } else {
      goto L2440;
    }
  L2400:
    /* Computing 2nd power */
    r_1 = dxu;
    /* Computing 2nd power */
    r_2 = dxz;
    del = (dfz * (r_1 * r_1) + dfu * (r_2 * r_2)) / (denom + denom);
    cstep_1->X[jx - 1] = xsave + del;
    if (cstep_1->X[jx - 1] - xsave != (float)0.) {
      goto L2410;
    } else {
      goto L2450;
    }
  L2410:
    cstep_1->FOBJ = (*funk)();
    ++stork_1->nf;
    if (cstep_1->FOBJ - fbest >= (float)0.) {
      goto L2440;
    } else {
      goto L2420;
    }
  L2420:
    fbest = cstep_1->FOBJ;
    stork_1->dlx[jx - 1] += del;
    vec[jx - 1] += del / adx;
  L2430:
    jock = 1;
    goto L2450;
  L2440:
    cstep_1->X[jx - 1] = xsave;
    /* SEE IF THE STEP SIZE SHOULD BE 
       INCREASED. STEPI300*/
  L2450:
    if (nzip <= 0) {
      goto L2460;
    } else {
      goto L2470;
    }
  L2460:
    if (nack - 1 <= 0) {
      goto L3190;
    } else {
      goto L2470;
    }
  L2470:
    avec = vec[jx - 1];
    if (avec < (float)0.) {
      goto L2480;
    } else if (avec == 0) {
      goto L3190;
    } else {
      goto L2490;
    }
  L2480:
    avec = -(doublereal)avec;
  L2490:
    if (avec - facup >= (float)0.) {
      goto L2500;
    } else {
      goto L2560;
    }
    /* 
      STEPI307*/
    /* INCREASE THE STEP SIZE. 
       STEPI308*/
  L2500:
    stork_1->dx[jx - 1] *= ack;
    vec[jx - 1] /= ack;
    if (nosc <= 0) {
      goto L2530;
    } else {
      goto L2510;
    }
  L2510:
    i_2 = nosc;
    for (j = 1; j <= i_2; ++j) {
      /* L2520: */
      cstep_1->ERR[jx + j * cstep_1->NV - (cstep_1->NV + 1)] /= ack;
    }
  L2530:
    if (cstep_1->NTRAC <= 0) {
      goto L2560;
    } else {
      goto L2540;
    }
  L2540:
    io__61.ciunit = cstep_1->KW;
    s_wsfe(&io__61);
    do_fio(&c__1, (char *)&jx, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&stork_1->dx[jx - 1], (ftnlen)sizeof(real));
    e_wsfe();
    /* 
      STEPI318*/
    /** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
     * STEPI319*/
    /* 
      STEPI320*/
    /* STEP ALONG A RESULTANT DIRECTION, IF POSSIBLE. 
       STEPI321*/
    /* 
      STEPI322*/
  L2560:
    if (nzip <= 0) {
      goto L3190;
    } else {
      goto L2570;
    }
  L2570:
    nonzr = 0;
    sumv = rzero;
    i_2 = cstep_1->NV;
    for (j = 1; j <= i_2; ++j) {
      if (vec[j - 1] != (float)0.) {
	goto L2580;
      } else {
	goto L2590;
      }
    L2580:
      ++nonzr;
    L2590:
      /* Computing 2nd power */
      r_1 = vec[j - 1];
      sumv += r_1 * r_1;
    }
    if (nonzr - 2 >= 0) {
      goto L2600;
    } else {
      goto L3190;
    }
  L2600:
    if (sumv <= (float)0.) {
      goto L2810;
    } else {
      goto L2610;
    }
    /* 
      STEPI332*/
    /* GIANT STEPS WILL BE ATTEMPTED. CHECK FOR POSSIBLE GIGANTIC STEPS. 
       STEPI333*/
    /* 
      STEPI334*/
  L2610:
    if (mosq <= 0) {
      goto L2810;
    } else {
      goto L2620;
    }
  L2620:
    /* 
      STEPI337*/
    /*X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X 
      X STEPI338*/
    /* 
      STEPI339*/
    /* OSCILLATION SEARCH SECTION......  
       STEPI340*/
    /* 
      STEPI341*/
    /* KL ... POINTER FOR OSCILLATION CHECK 
       STEPI342*/
    kl = 1;
    /* STORE OSCILLATION INFORMATION. 
       STEPI344*/
    /* NOSC=MIN0(NOSC+1,MOSQ) 
       STEPI345*/
    ++nosc;
    if (nosc - mosq <= 0) {
      goto L2660;
    } else {
      goto L2630;
    }
  L2630:
    nosc = mosq;
    if ((i_2 = nosc - 1) < 0) {
      goto L2810;
    } else if (i_2 == 0) {
      goto L2660;
    } else {
      goto L2640;
    }
    /* 
      STEPI350*/
    /* THE QUEUE OF OSCILLATION INFORMATION IS 
       STEPI351*/
    /* FULL. PUSH IT DOWN, THROWING AWAY 
       STEPI352*/
    /* THE OLDEST ITEM. 
       STEPI353*/
  L2640:
    i_2 = nosc;
    for (k = 2; k <= i_2; ++k) {
      fosc[k - 2] = fosc[k - 1];
      i_3 = cstep_1->NV;
      for (j = 1; j <= i_3; ++j) {
	xosc[j + (k - 1) * cstep_1->NV - (cstep_1->NV + 1)] = xosc[j + k * cstep_1->NV - (cstep_1->NV + 1)];
	/* L2650: */
	cstep_1->ERR[j + (k - 1) * cstep_1->NV - (cstep_1->NV + 1)] = cstep_1->ERR[j + k * cstep_1->NV - 
										   (cstep_1->NV + 1)];
      }
    }
    /* 
      STEPI359*/
    /* ADD THE NEW ITEM TO THE QUEUE. 
       STEPI360*/
  L2660:
    sumv = sqrt(sumv);
    i_3 = cstep_1->NV;
    for (j = 1; j <= i_3; ++j) {
      xosc[j + nosc * cstep_1->NV - (cstep_1->NV + 1)] = cstep_1->X[j - 1];
      /* L2670: */
      cstep_1->ERR[j + nosc * cstep_1->NV - (cstep_1->NV + 1)] = vec[j - 1] / sumv;
    }
    fosc[nosc - 1] = fbest;
    if (nosc - 2 >= 0) {
      goto L2680;
    } else {
      goto L2810;
    }
    /* 
      STEPI367*/
    /* SEARCH FOR A PREVIOUS SUCCESSFUL GIANT STEP IN A DIRECTION MORE 
       STEPI368*/
    /* NEARLY PARALLEL TO THE DIRECTION OF THE PROPOSED STEP THAN WAS THE 
       STEPI369*/
    /* IMMEDIATELY PREVIOUS ONE. THIS MAY MEAN THAT THE DIRECTIONS OF 
       THE STEPI370*/
    /* GIANT STEPS OSCILLATE PERIODICALLY (ZIG-ZAG). TRY GIGANTIC 
       STEPI371*/
    /* (OSCILLATION) STEPS OF DECREASING PERIOD, THEN ORDINARY GIANT 
       STEPS. STEPI372*/
    /* SINCE THE DIRECTIONS ARE GIVEN AS NUMBERS OF STEPS, THIS 
       STEPI373*/
    /* PROCEDURE IS SCALE INDEPENDENT.  
       STEPI374*/
    /* 
      STEPI375*/
  L2680:
    coxcm = rzero;
    i_3 = cstep_1->NV;
    for (j = 1; j <= i_3; ++j) {
      /* L2690: */
      coxcm += cstep_1->ERR[j + nosc * cstep_1->NV - (cstep_1->NV + 1)] * cstep_1->ERR[j + (nosc 
											    - 1) * cstep_1->NV - (cstep_1->NV + 1)];
    }
    nah = nosc - mnosc;
  L2700:
    if (kl - nah <= 0) {
      goto L2710;
    } else {
      goto L2810;
    }
  L2710:
    i_3 = nah;
    for (k = kl; k <= i_3; ++k) {
      /* NRETR ... NUMBER OF OSCILLATION 
	 PERIODS STEPI382*/
      /* YET TO BE TESTED 
	 STEPI383*/
      nretr = nah - k;
      cosin = rzero;
      i_2 = cstep_1->NV;
      for (j = 1; j <= i_2; ++j) {
	/* L2720: */
	cosin += cstep_1->ERR[j + nosc * cstep_1->NV - (cstep_1->NV + 1)] * cstep_1->ERR[j + k *
											 cstep_1->NV - (cstep_1->NV + 1)];
      }
      if (k - (nosc - 1) >= 0) {
	goto L2760;
      } else {
	goto L2730;
      }
    L2730:
      if (cosin <= (float)0.) {
	goto L2750;
      } else {
	goto L2740;
      }
    L2740:
      if (cosin - coxcm <= (float)0.) {
	goto L2750;
      } else {
	goto L2760;
      }
    L2750:
      ;}
    goto L2810;
    /* ZIG-ZAGGING DETECTED. ATTEMPT TO TAKE 
       STEPI393*/
    /* GIGANTIC STEPS. 
       STEPI394*/
  L2760:
    kl = k + 1;
    if (cstep_1->NTRAC <= 0) {
      goto L2790;
    } else {
      goto L2770;
    }
  L2770:
    nt = nosc - k;
    io__73.ciunit = cstep_1->KW;
    s_wsfe(&io__73);
    do_fio(&c__1, (char *)&nt, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&coxcm, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&cosin, (ftnlen)sizeof(real));
    e_wsfe();
  L2790:
    i_3 = cstep_1->NV;
    for (j = 1; j <= i_3; ++j) {
      /* SALVO ... SAVES DLX DURING GIGANTIC 
	 STEPS STEPI402*/
      salvo[j - 1] = stork_1->dlx[j - 1];
      /* L2800: */
      stork_1->dlx[j - 1] = cstep_1->X[j - 1] - xosc[j + k * cstep_1->NV - (cstep_1->NV + 1)];
    }
    fprev = fosc[k - 1];
    goto L2820;
    /* 
      STEPI407*/
    /*X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X 
      X STEPI408*/
    /* 
      STEPI409*/
    /* SIMON SAYS, TAKE AS MANY GIANT STEPS AS POSSIBLE. 
       STEPI410*/
    /* 
      STEPI411*/
  L2810:
    fprev = fstor[jx - 1];
    /* NRETR=-1 IF A GIANT STEP IS BEING TRIED. 
       STEPI413*/
    nretr = -1;
    /* NGIAN... NUMBER OF GIANT OR GIGANTIC 
       STEPI415*/
    /* STEPS COMPLETED 
       STEPI416*/
  L2820:
    ngian = 0;
    nfsav = stork_1->nf;
  L2830:
    i_3 = cstep_1->NV;
    for (j = 1; j <= i_3; ++j) {
      stork_1->xs[j - 1] = cstep_1->X[j - 1];
      if (cstep_1->MASK[j - 1] != 0) {
	goto L2880;
      } else {
	goto L2840;
      }
    L2840:
      cstep_1->X[j - 1] += stork_1->dlx[j - 1];
      /* X(J)=AMAX1(AMIN1(X(J),XMAX(J)),XMIN(
	 J)) STEPI423*/
      if (cstep_1->X[j - 1] - cstep_1->XMAX[j - 1] <= (float)0.) {
	goto L2860;
      } else {
	goto L2850;
      }
    L2850:
      cstep_1->X[j - 1] = cstep_1->XMAX[j - 1];
    L2860:
      if (cstep_1->X[j - 1] - cstep_1->XMIN[j - 1] >= (float)0.) {
	goto L2880;
      } else {
	goto L2870;
      }
    L2870:
      cstep_1->X[j - 1] = cstep_1->XMIN[j - 1];
    L2880:
      ;}
    jock = 0;
    cstep_1->JVARY = 0;
    cstep_1->FOBJ = (*funk)();
    ++stork_1->nf;
    if (cstep_1->FOBJ - fbest >= (float)0.) {
      goto L2960;
    } else {
      goto L2890;
    }
  L2890:
    fprev = fbest;
    fbest = cstep_1->FOBJ;
    i_3 = cstep_1->NV;
    for (j = 1; j <= i_3; ++j) {
      /* L2900: */
      stork_1->dlx[j - 1] *= ack;
    }
    ++ngian;
    if (cstep_1->NTRAC <= 0) {
      goto L2830;
    } else {
      goto L2910;
    }
  L2910:
    if (ngian - 1 <= 0) {
      goto L2920;
    } else {
      goto L2950;
    }
  L2920:
    io__78.ciunit = cstep_1->KW;
    s_wsfe(&io__78);
    i_3 = jx;
    for (j = 1; j <= i_3; ++j) {
      do_fio(&c__1, (char *)&vec[j - 1], (ftnlen)sizeof(real));
    }
    e_wsfe();
    io__79.ciunit = cstep_1->KW;
    s_wsfe(&io__79);
    do_fio(&c__1, (char *)&fprev, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&nfsav, (ftnlen)sizeof(integer));
    i_3 = cstep_1->NV;
    for (j = 1; j <= i_3; ++j) {
      do_fio(&c__1, (char *)&stork_1->xs[j - 1], (ftnlen)sizeof(real));
    }
    e_wsfe();
  L2950:
    io__80.ciunit = cstep_1->KW;
    s_wsfe(&io__80);
    do_fio(&c__1, (char *)&cstep_1->FOBJ, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&stork_1->nf, (ftnlen)sizeof(integer));
    i_3 = cstep_1->NV;
    for (j = 1; j <= i_3; ++j) {
      do_fio(&c__1, (char *)&cstep_1->X[j - 1], (ftnlen)sizeof(real));
    }
    e_wsfe();
    goto L2830;
  L2960:
    if (ngian <= 0) {
      goto L3040;
    } else {
      goto L2970;
    }
    /* 
      STEPI449*/
    /* PERFORM PARABOLIC INTERPOLATION. 
       STEPI450*/
    /* 
      STEPI451*/
  L2970:
    denom = ack * (fprev - fbest) - (fbest - cstep_1->FOBJ);
    if (denom != (float)0.) {
      goto L2980;
    } else {
      goto L3040;
    }
  L2980:
    cindr = ((fprev - fbest) * ack + (fbest - cstep_1->FOBJ) / ack) / (
								       denom + denom);
    i_3 = cstep_1->NV;
    for (j = 1; j <= i_3; ++j) {
      if (cstep_1->MASK[j - 1] != 0) {
	goto L3030;
      } else {
	goto L2990;
      }
    L2990:
      cstep_1->X[j - 1] = stork_1->xs[j - 1] + cindr * stork_1->dlx[j - 1];

      /*  
	STEPI458*/
      /* X(J)=AMAX1(AMIN1(X(J),XMAX(J)),XMIN(
	 J)) STEPI459*/
      if (cstep_1->X[j - 1] - cstep_1->XMAX[j - 1] <= (float)0.) {
	goto L3010;
      } else {
	goto L3000;
      }
    L3000:
      cstep_1->X[j - 1] = cstep_1->XMAX[j - 1];
    L3010:
      if (cstep_1->X[j - 1] - cstep_1->XMIN[j - 1] >= (float)0.) {
	goto L3030;
      } else {
	goto L3020;
      }
    L3020:
      cstep_1->X[j - 1] = cstep_1->XMIN[j - 1];
    L3030:
      ;}
    jock = 0;
    cstep_1->JVARY = 0;
    cstep_1->FOBJ = (*funk)();
    ++stork_1->nf;
    if (cstep_1->FOBJ - fbest >= (float)0.) {
      goto L3040;
    } else {
      goto L3120;
    }
  L3040:
    i_3 = cstep_1->NV;
    for (j = 1; j <= i_3; ++j) {
      if (nretr >= 0) {
	goto L3050;
      } else {
	goto L3060;
      }
    L3050:
      stork_1->dlx[j - 1] = salvo[j - 1];
    L3060:
      cstep_1->X[j - 1] = stork_1->xs[j - 1];
    }
    if (cstep_1->NTRAC <= 0) {
      goto L3090;
    } else {
      goto L3070;
    }
  L3070:
    io__82.ciunit = cstep_1->KW;
    s_wsfe(&io__82);
    do_fio(&c__1, (char *)&fbest, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&ngian, (ftnlen)sizeof(integer));
    i_3 = cstep_1->NV;
    for (j = 1; j <= i_3; ++j) {
      do_fio(&c__1, (char *)&cstep_1->X[j - 1], (ftnlen)sizeof(real));
    }
    e_wsfe();
  L3090:
    if (ngian <= 0) {
      goto L3100;
    } else {
      goto L3150;
    }
  L3100:
    /* 
      STEPI480*/
    /*X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X 
      X STEPI481*/
    /* 
      STEPI482*/
    /* STATEMENT USED IN THE OSCILLATION SEARCH.... 
       STEPI483*/
    /* 
      STEPI484*/
    if (nretr < 0) {
      goto L3170;
    } else if (nretr == 0) {
      goto L3110;
    } else {
      goto L2700;
    }
    /* 
      STEPI486*/
    /*X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X 
      X STEPI487*/
    /* 
      STEPI488*/
    /* IF ALL GIGANTIC STEPS WERE UNSUCCESSFUL, 
       STEPI489*/
    /* TRY A GIANT STEP. 
       STEPI490*/
  L3110:
    if (nretr != 0) {
      goto L3170;
    } else {
      goto L2810;
    }
    /* 
      STEPI492*/
  L3120:
    fbest = cstep_1->FOBJ;
    jock = 1;
    if (cstep_1->NTRAC <= 0) {
      goto L3150;
    } else {
      goto L3130;
    }
  L3130:
    steps = (real) ngian;
    steps += cindr;
    io__84.ciunit = cstep_1->KW;
    s_wsfe(&io__84);
    do_fio(&c__1, (char *)&fbest, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&steps, (ftnlen)sizeof(real));
    i_3 = cstep_1->NV;
    for (j = 1; j <= i_3; ++j) {
      do_fio(&c__1, (char *)&cstep_1->X[j - 1], (ftnlen)sizeof(real));
    }
    e_wsfe();
  L3150:
    if (nretr >= 0) {
      goto L3160;
    } else {
      goto L2030;
    }
  L3160:
    /* A SUCCESSFUL GIGANTIC STEP HAS OCCURRED. 
       STEPI503*/
    /* INSERT ADDITIONAL LOGIC HERE IF DESIRED. 
       STEPI504*/
    goto L2030;
    /* AN UNSUCCESSFUL GIANT STEP HAS OCCURRED. 
       STEPI506*/
    /* DELETE ITS OSCILLATION INFORMATION. 
       STEPI507*/
    /* NOSC=MAX0(NOSC-1,0) 
       STEPI508*/
  L3170:
    --nosc;
    if (nosc >= 0) {
      goto L3190;
    } else {
      goto L3180;
    }
  L3180:
    nosc = 0;
    /* COMPLETE THE MAIN DO LOOP. 
       STEPI512*/
    /* FSTOR(JX) ... SAVES FBEST FOR 
       STEPI513*/
    /* INTERPOLATION IN GIANT STEPS 
       STEPI514*/
  L3190:
    fstor[jx - 1] = fbest;
    /* RETURN IF THE SENSE SWITCH IS ON. 
       STEPI517*/
    datsw_(&stork_1->nssw, &jump);
    if (jump - 1 <= 0) {
      goto L3550;
    } else {
      goto L3200;
    }
    /* 
      STEPI520*/
  L3200:
    if (stork_1->nf - cstep_1->NFMAX <= 0) {
      goto L3210;
    } else {
      goto L3530;
    }
  L3210:
    ;}
  /* END OF THE MAIN DO LOOP. STEPI523*/
  /* STEPI524*/
  /** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * STEPI525*/
  /* STEPI526*/
  /* ANOTHER CYCLE THROUGH THE VARIABLES HAS BEEN COMPLETED. STEPI527*/
  /* PRINT ANOTHER LINE OF TRACES.  STEPI528*/
  /* STEPI529*/
  if (cstep_1->NTRAC <= 0) {
    goto L3230;
  } else {
    goto L3220;
  }
 L3220:
  io__85.ciunit = cstep_1->KW;
  s_wsfe(&io__85);
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&vec[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
 L3230:
  if (nzip != 0) {
    goto L3270;
  } else {
    goto L3240;
  }
 L3240:
  if (cstep_1->NTRAC <= 0) {
    goto L3270;
  } else {
    goto L3250;
  }
 L3250:
  io__86.ciunit = cstep_1->KW;
  s_wsfe(&io__86);
  do_fio(&c__1, (char *)&fbest, (ftnlen)sizeof(real));
  do_fio(&c__1, (char *)&stork_1->nf, (ftnlen)sizeof(integer));
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&cstep_1->X[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
  io__87.ciunit = cstep_1->KW;
  s_wsfe(&io__87);
  e_wsfe();
 L3270:
  ++nzip;
  goto L2040;
  /* STEPI540*/
  /* A NEW BASE POINT HAS BEEN FOUND. PRINT THE REMAINING TRACES. STEPI541*/
  /* STEPI542*/
 L3280:
  fstor[jx - 1] = fbest;
  if (cstep_1->NTRAC <= 0) {
    goto L3300;
  } else {
    goto L3290;
  }
 L3290:
  io__88.ciunit = cstep_1->KW;
  s_wsfe(&io__88);
  i_1 = jx;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&vec[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
  io__89.ciunit = cstep_1->KW;
  s_wsfe(&io__89);
  do_fio(&c__1, (char *)&fbest, (ftnlen)sizeof(real));
  do_fio(&c__1, (char *)&stork_1->nf, (ftnlen)sizeof(integer));
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&cstep_1->X[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
  /* STEPI547*/
  /* DECREASE THE SIZE OF THE STEPS FOR ALL VARIABLES. STEPI548*/
  /* STEPI549*/
 L3300:
  /* RETURN IF THE SENSE SWITCH IS ON. STEPI551*/
  datsw_(&stork_1->nssw, &jump);
  if (jump - 1 <= 0) {
    goto L3550;
  } else {
    goto L3310;
  }
  /* STEPI554*/
 L3310:
  if (stork_1->nf - cstep_1->NFMAX <= 0) {
    goto L3320;
  } else {
    goto L3530;
  }
  /* STEPI556*/
  /* CHECK WHETHER ALL ABS(DX(J)) .LE. DELMN(J)
     .STEPI557*/
 L3320:
  ngate = 1;
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    if (cstep_1->MASK[j - 1] != 0) {
      goto L3370;
    } else {
      goto L3330;
    }
    /* 
      STEPI561*/
    /* IF(ABS(DX(J))-DELMN(J)) 
       STEPI562*/
  L3330:
    adx = stork_1->dx[j - 1];
    if (adx >= (float)0.) {
      goto L3350;
    } else {
      goto L3340;
    }
  L3340:
    adx = -(doublereal)adx;
  L3350:
    if (adx - cstep_1->DELMN[j - 1] <= (float)0.) {
      goto L3370;
    } else {
      goto L3360;
    }
  L3360:
    ngate = 0;
  L3370:
    stork_1->dx[j - 1] /= stcut;
  }
  if (ngate <= 0) {
    goto L3410;
  } else {
    goto L3380;
  }
 L3380:
  cstep_1->KFLAG = 1;
  if (cstep_1->NTRAC >= 0) {
    goto L3390;
  } else {
    goto L3580;
  }
 L3390:
  io__91.ciunit = cstep_1->KW;
  s_wsfe(&io__91);
  e_wsfe();
  goto L3580;
  /* CHECK THE JFLAT(J). STEPI576*/
 L3410:
  if (cstep_1->NFLAT <= 0) {
    goto L3500;
  } else {
    goto L3420;
  }
 L3420:
  jflmn = 5;
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    if (cstep_1->MASK[j - 1] != 0) {
      goto L3450;
    } else {
      goto L3430;
    }
  L3430:
    if (jflat[j - 1] - jflmn >= 0) {
      goto L3450;
    } else {
      goto L3440;
    }
  L3440:
    jflmn = jflat[j - 1];
  L3450:
    ;}
  if (jflmn - 1 >= 0) {
    goto L3460;
  } else {
    goto L3500;
  }
 L3460:
  cstep_1->KFLAG = 2;
  if (cstep_1->NTRAC >= 0) {
    goto L3470;
  } else {
    goto L3580;
  }
 L3470:
  io__93.ciunit = cstep_1->KW;
  s_wsfe(&io__93);
  e_wsfe();
  io__94.ciunit = cstep_1->KW;
  s_wsfe(&io__94);
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&stork_1->dx[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
  goto L3580;
  /* PRINT THE DX(J) AND SEARCH SOME MORE. STEPI593*/
 L3500:
  if (cstep_1->NTRAC <= 0) {
    goto L2030;
  } else {
    goto L3510;
  }
 L3510:
  io__95.ciunit = cstep_1->KW;
  s_wsfe(&io__95);
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&stork_1->dx[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
  goto L2030;
  /* STEPI599*/
 L3530:
  cstep_1->KFLAG = -2;
  io__96.ciunit = cstep_1->KW;
  s_wsfe(&io__96);
  do_fio(&c__1, (char *)&cstep_1->NFMAX, (ftnlen)sizeof(integer));
  e_wsfe();
  goto L3570;
  /* STEPI605*/
 L3550:
  cstep_1->KFLAG = -3;
  io__97.ciunit = cstep_1->KW;
  s_wsfe(&io__97);
  do_fio(&c__1, (char *)&stork_1->nssw, (ftnlen)sizeof(integer));
  e_wsfe();
  /* STEPI610*/
 L3570:
  io__98.ciunit = cstep_1->KW;
  s_wsfe(&io__98);
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&stork_1->dx[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
  /* STEPI613*/
  /* SET SWITCH FOR TERMINATION. STEPI614*/
  kwit = 1;
  /* CALL FUNK WITH THE BEST SET OF X(J). STEPI616*/
 L3580:
  cstep_1->JVARY = 0;
  cstep_1->FOBJ = (*funk)();
  if (fbest - fsave <= (float)0.) {
    goto L3590;
  } else {
    goto L3600;
  }
 L3590:
  if (cstep_1->FOBJ - fbest != (float)0.) {
    goto L3600;
  } else {
    goto L3620;
  }
 L3600:
  cstep_1->NOREP += 2;
  io__99.ciunit = cstep_1->KW;
  s_wsfe(&io__99);
  do_fio(&c__1, (char *)&stork_1->nf, (ftnlen)sizeof(integer));
  do_fio(&c__1, (char *)&fsave, (ftnlen)sizeof(real));
  do_fio(&c__1, (char *)&fbest, (ftnlen)sizeof(real));
  do_fio(&c__1, (char *)&cstep_1->FOBJ, (ftnlen)sizeof(real));
  e_wsfe();
 L3620:
  if (cstep_1->NTRAC >= 0) {
    goto L3630;
  } else {
    goto L3650;
  }
 L3630:
  io__100.ciunit = cstep_1->KW;
  s_wsfe(&io__100);
  do_fio(&c__1, (char *)&stork_1->nf, (ftnlen)sizeof(integer));
  do_fio(&c__1, (char *)&cstep_1->FOBJ, (ftnlen)sizeof(real));
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    do_fio(&c__1, (char *)&cstep_1->X[j - 1], (ftnlen)sizeof(real));
  }
  e_wsfe();
 L3650:
  if (kwit != 0) {
    goto L3710;
  } else {
    goto L3660;
  }
  /* IF(IABS(MATRX-100)-50) STEPI632*/
 L3660:
  matd = cstep_1->MATRX - 100;
  if (matd >= 0) {
    goto L3680;
  } else {
    goto L3670;
  }
 L3670:
  matd = -matd;
 L3680:
  if (matd - 50 <= 0) {
    goto L3690;
  } else {
    goto L3710;
  }
  /* STEPI637*/
  /* SET THE STEP SIZES FOR SUBROUTINE STERR. STEPI638*/
 L3690:

  /* EJ: To hell with sterr_()
  i_1 = cstep_1->MATRX - 100;
  fac = pow_ri(&rten, &i_1);
  i_1 = cstep_1->NV;
  for (j = 1; j <= i_1; ++j) {
    stork_1->dx[j - 1] = fac * stork_1->dx[j - 1];
  }
  sterr_(funk,cstep_1,stork_1);
  */

  /* THIS IS THE ONLY RETURN STATEMENT.... STEPI645*/
 L3710:
  return 0;
  /* END STEPIT.  STEPI647*/
}				/* stept_ */


