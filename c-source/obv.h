/*
These definitions are to keep the C code consistent across platforms.
Not all C source files will need to include this header file.
*/
#ifdef macintosh

#include <String.h>
#include <StdLib.h>
#include <StdIO.h>
#include <FCntl.h> /* For lseek() */

#define M_PI 3.14159265

#endif
/*
#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <share.h>

#define M_PI 3.14159265

#endif
*/

#include <unistd.h> /* For lseek() */
#include <stdio.h>
#include <math.h>

#ifdef __APPLE__
#include <unistd.h> /* For lseek() */
#include <stdio.h>

#endif
