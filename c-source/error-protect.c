#include <X11/Xlib.h>
#include<signal.h>

void error_hook (display, err)
Display *display;
XErrorEvent *err;
  {
  char msg[80];
  XGetErrorText (display, err->error_code, msg,80);
  printf ("X WINDOW ERROR !!! \n %s\n", msg);
  }


float fp_error (sig, code, scp, addr)
int sig, code;
struct sigcontext *scp;
char *addr;
  {
  return (1.0);
  }



set_signal (sig, func)
int sig;
void (*func)();
  {
  signal (sig,func);
  }

