#include"obv.h"

void srand48(long x)
{
	srand( (int) x );
}

/* Can achieve the 1.0 value... */
double drand48() 
{
	return( (double) rand() / (double)32767.0 );
}
