/* Simple Xlib application drawing a box in a window. */

#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
  Display *d;
  int s;
  Window w;
  XEvent e;

//  d = XOpenDisplay(NULL);
  d = XOpenDisplay("127.0.0.1:0.0");
//   d = XOpenDisplay("192.168.58.2:1");

  if(!d)
  {
    printf("Cannot open display\n");
    exit(1);
  }

  s = DefaultScreen(d);

  printf("default screen=%d rootwindow=%ld\n", s, RootWindow(d,s));

//exit(9);

  /* create window */
  w = XCreateSimpleWindow(d, RootWindow(d, s), 10, 10, 100, 100, 1,
                         WhitePixel(d, s), BlackPixel(d, s));

//   XNextEvent(d, &e);
//   exit(9);

  /* select kind of events we are interested in */
  XSelectInput(d, w, ExposureMask | KeyPressMask);

//   XNextEvent(d, &e);
//   exit(9);

  /* map (show) the window */
  XMapWindow(d, w);

//   XNextEvent(d, &e);
//   exit(9);

  /* event loop */
  for (;;)
  {
    XNextEvent(d, &e);

//     exit(9);

    /* draw or redraw the window */
    if (e.type==Expose)
    {
      XFillRectangle(d, w, DefaultGC(d, s), 20, 20, 10, 10);
      XDrawString(d, w, DefaultGC(d, s), 50, 50, "Hello, World!",strlen("Hello, World!"));
    }

    /* exit on key press */
    if(e.type==KeyPress)
       break;
  }

  /* close connection to server */
  XCloseDisplay(d);

  return 0;
}
