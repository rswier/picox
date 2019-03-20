/* X.h */

#define X_PROTOCOL		11		/* current protocol version */
#define X_PROTOCOL_REVISION	0		/* current minor version */

typedef unsigned long XID;
typedef XID Window;
typedef XID Font;
typedef XID Pixmap;
typedef XID Colormap;
typedef XID GContext;
typedef XID Drawable;

typedef unsigned long VisualID;


#define CopyFromParent       	0


#define ExposureMask		(1L<<15)
#define KeyPressMask		(1L<<0)

#define KeyPress     		2
#define Expose       		12

#define CWBackPixel		(1L<<1)
#define CWBorderPixel   	(1L<<3)
#define CWEventMask		(1L<<11)

#define GCFunction              (1L<<0)
#define GCPlaneMask             (1L<<1)
#define GCForeground            (1L<<2)
#define GCBackground            (1L<<3)
#define GCLineWidth             (1L<<4)
#define GCLineStyle             (1L<<5)
#define GCCapStyle              (1L<<6)
#define GCJoinStyle		(1L<<7)
#define GCFillStyle		(1L<<8)
#define GCFillRule		(1L<<9)
#define GCTile			(1L<<10)
#define GCStipple		(1L<<11)
#define GCTileStipXOrigin	(1L<<12)
#define GCTileStipYOrigin	(1L<<13)
#define GCFont 			(1L<<14)
#define GCSubwindowMode		(1L<<15)
#define GCGraphicsExposures     (1L<<16)
#define GCClipXOrigin		(1L<<17)
#define GCClipYOrigin		(1L<<18)
#define GCClipMask		(1L<<19)
#define GCDashOffset		(1L<<20)
#define GCDashList		(1L<<21)
#define GCArcMode		(1L<<22)

#define GXcopy			0x3		/* src */

#define CapButt			1
#define ArcPieSlice		1		/* join endpoints to center of arc */


/* Xlib.h */

#define Bool int

#define RootWindow(dpy, scr) 	(((dpy)->screens[(scr)]).root)
#define DefaultScreen(dpy) 	((dpy)->default_screen)
#define DefaultGC(dpy, scr) 	(((dpy)->screens[(scr)]).default_gc)
#define BlackPixel(dpy, scr) 	(((dpy)->screens[(scr)]).black_pixel)
#define WhitePixel(dpy, scr) 	(((dpy)->screens[(scr)]).white_pixel)
#define AllPlanes 		(~0)

typedef struct _XExtData
{
  int number;			/* number returned by XRegisterExtension */
  struct _XExtData *next;	/* next item on list of data for structure */
  int (*free_private)();	/* called to free private storage */
  char *private_data;		/* data private to this extension. */
} XExtData;

typedef struct
{
  int function;			/* logical operation */
  unsigned long plane_mask;	/* plane mask */
  unsigned long foreground;	/* foreground pixel */
  unsigned long background;	/* background pixel */
  int line_width;		/* line width */
  int line_style;	 	/* LineSolid, LineOnOffDash, LineDoubleDash */
  int cap_style;	  	/* CapNotLast, CapButt, CapRound, CapProjecting */
  int join_style;	 	/* JoinMiter, JoinRound, JoinBevel */
  int fill_style;	 	/* FillSolid, FillTiled, FillStippled, FillOpaeueStippled */
  int fill_rule;	  	/* EvenOddRule, WindingRule */
  int arc_mode;			/* ArcChord, ArcPieSlice */
  Pixmap tile;			/* tile pixmap for tiling operations */
  Pixmap stipple;		/* stipple 1 plane pixmap for stipping */
  int ts_x_origin;		/* offset for tile or stipple operations */
  int ts_y_origin;
  Font font;	        	/* default text font for text operations */
  int subwindow_mode;     	/* ClipByChildren, IncludeInferiors */
  Bool graphics_exposures;	/* boolean, should exposures be generated */
  int clip_x_origin;		/* origin for clipping */
  int clip_y_origin;
  Pixmap clip_mask;		/* bitmap clipping; other calls for rects */
  int dash_offset;		/* patterned/dashed line information */
  char dashes;
} XGCValues;

typedef struct _XGC
{
  XExtData *ext_data;		/* hook for extension to hang data */
  GContext gid;			/* protocol ID for graphics context */
  Bool rects;			/* boolean: TRUE if clipmask is list of rectangles */
  Bool dashes;			/* boolean: TRUE if dash-list is really a list */
  unsigned long dirty;		/* cache dirty bits */
  XGCValues values;		/* shadow structure of values */
} *GC;

typedef struct
{
  XExtData *ext_data;		/* hook for extension to hang data */
  VisualID visualid;		/* visual id of this visual */
  int class;			/* class of screen (monochrome, etc.) */
  unsigned long red_mask, green_mask, blue_mask;	/* mask values */
  int bits_per_rgb;		/* log base 2 of distinct color values */
  int map_entries;		/* color map entries */
} Visual;

typedef struct
{
  int depth;			/* this depth (Z) of the depth */
  int nvisuals;			/* number of Visual types at this depth */
  Visual *visuals;		/* list of visuals possible at this depth */
} Depth;

typedef struct
{
  struct _XDisplay *display;	/* back pointer to display structure */
  Window root;			/* Root window id. */
  int width, height;		/* width and height of screen */
  int mwidth, mheight;		/* width and height of  in millimeters */
  int ndepths;			/* number of depths possible */
  Depth *depths;		/* list of allowable depths on the screen */
  int root_depth;		/* bits per pixel */
  Visual *root_visual;		/* root visual */
  GC default_gc;		/* GC for the root root visual */
  Colormap cmap;		/* default color map */
  unsigned long white_pixel;
  unsigned long black_pixel;	/* White and Black pixel values */
  int max_maps, min_maps;	/* max and min color maps */
  int backing_store;		/* Never, WhenMapped, Always */
  Bool save_unders;
  long root_input_mask;		/* initial root input mask */

} Screen;

typedef struct
{
//  XExtData *ext_data;		/* hook for extension to hang data */
  void *ext_data;		/* hook for extension to hang data */
  int depth;			/* depth of this image format */
  int bits_per_pixel;		/* bits/pixel at this depth */
  int scanline_pad;		/* scanline must padded to this multiple */
} ScreenFormat;

typedef struct _XDisplay
{
  int fd;			/* Network socket. */

  int proto_major_version;	/* maj. version of server's X protocol */
  int proto_minor_version;	/* minor version of servers X protocol */
  char *vendor;			/* vendor of the server hardware */

  long resource_base;		/* resource ID base */
  long resource_mask;		/* resource ID mask bits */
  long resource_id;		/* allocator current ID */
  int resource_shift;		/* allocator shift to correct bits */

  int byte_order;		/* screen byte order, LSBFirst, MSBFirst */
  int bitmap_unit;		/* padding and data requirements */
  int bitmap_pad;		/* padding requirements on bitmaps */
  int bitmap_bit_order;		/* LeastSignificant or MostSignificant */
  int nformats;			/* number of pixmap formats in list */
  ScreenFormat *pixmap_format;	/* pixmap format list */

  int release;			/* release of the server */

  int request;			/* sequence number of last request. */
  char *buffer;			/* Output buffer starting address. */
  char *bufptr;			/* Output buffer index pointer. */
  char *bufmax;			/* Output buffer maximum+1 address. */
  unsigned max_request_size;	/* maximum number 32 bit words in request*/

  int default_screen;		/* default screen for operations */
  int nscreens;			/* number of screens on this server*/
  Screen *screens;		/* pointer to list of screens */

  int motion_buffer;		/* size of motion buffer */

  int min_keycode;		/* minimum defined keycode */
  int max_keycode;		/* maximum defined keycode */

//	int (*event_vec[128])();/* vector for wire to event */
//	int (*wire_vec[128])();	/* vector for event to wire */

} Display;

typedef struct _XEvent
{
  unsigned char type;		/* of event (KeyPressed, ExposeWindow, etc.) */
  unsigned char detail;
  unsigned short sequence_number;
  unsigned long p1;
  unsigned long p2;
  unsigned long p3;
  unsigned long p4;
  unsigned long p5;
  unsigned long p6;
  unsigned long p7;
} XEvent;

XID XAllocID(Display *dpy);
void XFlush(Display *d);
Display *XOpenDisplay(char *d);
GC XCreateGC(Display *dpy, Drawable d, unsigned long valuemask,	XGCValues *values);
void XChangeGC(Display *dpy, GC gc, unsigned long valuemask, XGCValues *values);
GContext XGContextFromGC(GC gc);
Window XCreateSimpleWindow(Display *d, Window p, int x, int y, int w, int h, int bw, unsigned long b, unsigned long bg);
void XSelectInput(Display *d, Window w, unsigned long mask);
void XMapWindow(Display *d, Window w);
void XNextEvent(Display *d, XEvent *e);
void XFillRectangle(Display *d, Window drw, GC gc, int x, int y, int w, int h);
void XDrawString(Display *dpy, Drawable d, GC gc, int x, int y, char *string, int length);
void XCloseDisplay(Display *d);

/* end Xlib.h */
