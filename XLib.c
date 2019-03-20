/* Xlib.c */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <X11/Xlib.h>

#ifdef WIN32
#include <WinSock2.h>
#else
#include <netdb.h>
#endif

/* Xproto.h */

#define X_TCP_PORT 6000     /* add display number */
typedef struct
{
  unsigned char byteOrder;
  unsigned char pad;
  unsigned short majorVersion, minorVersion;
  unsigned short nbytesAuthProto;	/* Authorization protocol */
  unsigned short nbytesAuthString;	/* Authorization string */
  unsigned short pad2;
} xConnClientPrefix;

typedef struct
{
  unsigned char success;
  unsigned char lengthReason; /*num bytes in string following if failure */
  unsigned short majorVersion, minorVersion;
  unsigned short length;  /* 1/4 additional bytes in setup info */
} xConnSetupPrefix;

typedef struct
{
  unsigned long release;
  unsigned long ridBase, ridMask;
  unsigned long motionBufferSize;
  unsigned short nbytesVendor;  /* number of bytes in vendor string */
  unsigned short maxRequestSize;
  unsigned char numRoots;          /* number of roots structs to follow */
  unsigned char numFormats;        /* number of pixmap formats */
  unsigned char imageByteOrder;        /* LSBFirst, MSBFirst */
  unsigned char bitmapBitOrder;        /* LeastSignificant, MostSign...*/
  unsigned char bitmapScanlineUnit;     /* 8, 16, 32 */
  unsigned char bitmapScanlinePad;     /* 8, 16, 32 */
  unsigned char minKeyCode, maxKeyCode;
  unsigned long pad2;
} xConnSetup;

typedef struct
{
  unsigned char depth;
  unsigned char bitsPerPixel;
  unsigned char scanLinePad;
  unsigned char pad1;
  unsigned long pad2;
} xPixmapFormat;

typedef struct
{
  unsigned char depth;
  unsigned char pad1;
  unsigned short nVisuals;  /* number of xVisualType structures following */
  unsigned long pad2;
} xDepth;

typedef struct
{
  unsigned long visualID;
  unsigned char class;
  unsigned char bitsPerRGB;
  unsigned short colormapEntries;
  unsigned long redMask, greenMask, blueMask;
  unsigned long pad;
} xVisualType;

typedef struct
{
  unsigned long windowId;
  unsigned long defaultColormap;
  unsigned long whitePixel, blackPixel;
  unsigned long currentInputMask;
  unsigned short pixWidth, pixHeight;
  unsigned short mmWidth, mmHeight;
  unsigned short minInstalledMaps, maxInstalledMaps;
  unsigned long rootVisualID;
  unsigned char backingStore;
  unsigned char saveUnders;
  unsigned char rootDepth;
  unsigned char nDepths;  /* number of xDepth structures following */
} xWindowRoot;

/* Requests */
typedef struct
{
  unsigned char reqType;
  unsigned char data;
  unsigned short length;
} xReq;

typedef struct
{
  unsigned char reqType;
  unsigned char pad;
  unsigned short length;
  unsigned long gc;
  unsigned long drawable;
  unsigned long mask;
} xCreateGCReq;

typedef struct {
  unsigned char reqType;
  unsigned char pad;
  unsigned short length;
  unsigned long gc;
  unsigned long mask;
} xChangeGCReq;

typedef struct
{
  unsigned char reqType;
  unsigned char depth;
  unsigned short length;
  unsigned long wid, parent;
  short x, y;
  unsigned short width, height, borderWidth;
  unsigned short class;
  unsigned long visual;
  unsigned long mask;
} xCreateWindowReq;

typedef struct
{
  unsigned char reqType;
  unsigned char pad;
  unsigned short length;
  unsigned long window;
  unsigned long valueMask;
} xChangeWindowAttributesReq;

typedef struct
{
  unsigned char reqType;
  unsigned char pad;
  unsigned short length;
  unsigned long id;  /* a Window, Drawable, Font, GContext, Pixmap, etc. */
} xResourceReq;

typedef struct _xSegment
{
  short x1, y1, x2, y2;
} xSegment;

typedef struct _xPoint
{
  short x,y;
} xPoint;

typedef struct _xRectangle
{
  short x, y;
  unsigned short width, height;
} xRectangle;

typedef struct
{
  unsigned char reqType;
  unsigned char pad;
  unsigned short length;
  unsigned long drawable;
  unsigned long gc;
} xPolySegmentReq;

typedef xPolySegmentReq xPolyArcReq;
typedef xPolySegmentReq xPolyRectangleReq;
typedef xPolySegmentReq xPolyFillRectangleReq;
typedef xPolySegmentReq xPolyFillArcReq;

typedef struct {
    unsigned char reqType;
    unsigned char pad;
    unsigned short length;
    unsigned long drawable;
    unsigned long gc;
    short x, y;		/* items (xTextElt) start after struct */
} xPolyTextReq;

typedef xPolyTextReq xPolyText8Req;
typedef xPolyTextReq xPolyText16Req;

typedef struct {           /* followed by string */
    unsigned char len;	/* number of *characters* in string, or FontChange (255)
		   for font change, or 0 if just delta given */
    char delta;
} xTextElt;


#define X_Error		0		/* Error */

/* Request codes */
#define X_CreateWindow                  1
#define X_ChangeWindowAttributes        2
#define X_MapWindow                     8
#define X_CreateGC                     55
#define X_ChangeGC                     56
#define X_PolyFillRectangle            70
#define X_PolyText8                    74


#define BUFSIZE 2048			/* X output buffer size. */


/* XLibInt.c */

XID XAllocID(Display *dpy)
{
   unsigned long id;
   id = dpy->resource_base + (dpy->resource_id++ << dpy->resource_shift);
   printf("%ld=XallocID(resource_base = %ld dpy_resource_id = %ld resource_shift = %d)\n",id,
      dpy->resource_base, dpy->resource_id-1,dpy->resource_shift);
   return id;
}

/* XLib.c */

void XFlush(Display *d)
{
  if (d->bufptr > d->buffer)
  {
    send(d->fd, d->buffer, d->bufptr - d->buffer, 0);
    d->bufptr = d->buffer;
  }
}

Display *XOpenDisplay(char *d)
{
  static Screen scn;
  Display *dpy;
  char host[100];
  int i = 0;
  int j,k;
  int display_num = 0;

  struct sockaddr_in inaddr;	/* INET socket address. */
//  struct sockaddr *addr;		/* address to connect to */
  struct hostent *host_ptr;
  int addrlen;			/* length of address */
  int fd;				/* Network socket */

  xConnClientPrefix client;	/* client information */
  xConnSetupPrefix prefix;	/* prefix information */
	int vendorlen;			/* length of vendor string */

  char *setup, *p;			/* memory allocated at startup */
  long setuplength;	/* number of bytes in setup message */
	int screen_num;			/* screen number */

#ifdef WIN32
  WSADATA info;
  int rc;
  if ((rc = WSAStartup(MAKEWORD(2,0),&info)) != 0)
  {
    printf("WSAStartup failed with error %d\n",rc);
    WSACleanup();
    return 0;
  }
#endif

  if (d)
  {
    while (*d && *d != ':') host[i++] = *d++;
    if (*d) display_num = atoi(d + 1);
  }
  host[i] = '\0';
  screen_num = 0;  //?????after dot!

  if (!*host) gethostname(host, sizeof(host));
  printf("host: %s\n", host);

  /* Get the statistics on the specified host. */
  memset(&inaddr, 0, sizeof(struct sockaddr_in));
  if ((inaddr.sin_addr.s_addr = inet_addr(host)) == -1)
  {
    if ((host_ptr = gethostbyname(host)) == NULL)
    {
      /* No such host! */
      printf("no such host\n");
      return 0;
    }

	/* Check the address type for an internet host. */
    if (host_ptr->h_addrtype != AF_INET)
    {
      /* Not an Internet host! */
      printf("not and internet host\n");
      return 0;
    }

    /* Set up the socket data. */
    inaddr.sin_family = host_ptr->h_addrtype;
    memcpy((char *)&inaddr.sin_addr, (char *)host_ptr->h_addr, sizeof(inaddr.sin_addr));

    printf("inaddr = %08x\n", *(unsigned int *)&inaddr.sin_addr);
  }
  else
  {
    inaddr.sin_family = AF_INET;
  }

  inaddr.sin_port = htons(display_num + X_TCP_PORT);

  /* Open the network connection. */
//  if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0)
//  if ((fd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
  if ((fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
  {
    printf("socket failed\n");
    return 0;
  }

  if (connect(fd, (struct sockaddr *) &inaddr, sizeof (struct sockaddr_in)) == -1)
  {
    printf("connect failed\n");
    close(fd);
    return 0;
  }

  dpy = (Display *) malloc(sizeof(Display));
  //if ????
  memset(dpy, 0, sizeof(Display));
  dpy->fd = fd;

  /* First byte is the byte order byte. Authentication key is normally sent right after the connection.
     This (in MIT's case) will be kerberos. */
  i = 1;
  client.byteOrder = *((char *)&i) ? 'l' : 'B';
  client.majorVersion = X_PROTOCOL;
  client.minorVersion = X_PROTOCOL_REVISION;
  client.nbytesAuthProto = 0;
  client.nbytesAuthString = 0;

  send(dpy->fd, (void *) &client, sizeof(xConnClientPrefix), 0);

  /* Now see if connection was accepted... */
  recv(dpy->fd, (void *)&prefix, sizeof(xConnSetupPrefix), 0);

  if (prefix.majorVersion < X_PROTOCOL)
  {
    printf("Warning: Client built for newer server (prefix.majorVersion=%d < X_PROTOCOL=%d)!\n", prefix.majorVersion, X_PROTOCOL);
  }
  if (prefix.minorVersion != X_PROTOCOL_REVISION)
  {
    printf("Warning: Protocol rev. of client does not match server (prefix.minorVersion=%d != X_PROTOCOL_REVISION=%d)!\n",
    prefix.minorVersion, X_PROTOCOL_REVISION);
  }

  setuplength = prefix.length << 2;
  printf("setuplength = %ld\n", setuplength);
  setup = malloc(setuplength);

  recv(dpy->fd, setup, setuplength, 0);

  /* If the connection was not accepted by the server due to problems, give error message to the user... */
  if (prefix.success != 1)
  {
    setup[prefix.lengthReason] = '\0';
    printf("%s\n", setup);
    return 0;
  }

  p = setup;
  /* We succeeded at authorization, so let us move the data into the display structure. */
//	dpy->next				= (Display *) NULL;
  dpy->proto_major_version	= prefix.majorVersion;
  dpy->proto_minor_version	= prefix.minorVersion;
  dpy->release 				= ((xConnSetup *)p)->release;
  dpy->resource_base		= ((xConnSetup *)p)->ridBase;
  dpy->resource_mask		= ((xConnSetup *)p)->ridMask;
  dpy->min_keycode			= ((xConnSetup *)p)->minKeyCode;
  dpy->max_keycode			= ((xConnSetup *)p)->maxKeyCode;


//	dpy->keysyms			= (KeySym *) NULL;
//???	dpy->modifiermap		= XNewModifiermap(0);
//	dpy->keysyms_per_keycode = 0;
//???	dpy->current		= None;
//	dpy->xdefaults		= (char *)NULL;
//	dpy->scratch_length	= 0L;
//	dpy->scratch_buffer	= NULL;
	dpy->motion_buffer	= ((xConnSetup *)p)->motionBufferSize;
	dpy->nformats		= ((xConnSetup *)p)->numFormats;
	dpy->nscreens		= ((xConnSetup *)p)->numRoots;
	dpy->byte_order		= ((xConnSetup *)p)->imageByteOrder;
	dpy->bitmap_unit	= ((xConnSetup *)p)->bitmapScanlineUnit;
	dpy->bitmap_pad		= ((xConnSetup *)p)->bitmapScanlinePad;
	dpy->bitmap_bit_order   = ((xConnSetup *)p)->bitmapBitOrder;
	dpy->max_request_size	= ((xConnSetup *)p)->maxRequestSize;
//	dpy->ext_procs		= (_XExtension *)NULL;
//	dpy->ext_data		= (XExtData *)NULL;
//	dpy->ext_number 	= 0;
#if 0
	dpy->event_vec[X_Error] = _XUnknownWireEvent;
	dpy->event_vec[X_Reply] = _XUnknownWireEvent;
	dpy->wire_vec[X_Error]  = _XUnknownNativeEvent;
	dpy->wire_vec[X_Reply]  = _XUnknownNativeEvent;
	for (i = KeyPress; i < LASTEvent; i++) {
	    dpy->event_vec[i] 	= _XWireToEvent;
	    dpy->wire_vec[i] 	= NULL;
	}
	for (i = LASTEvent; i < 128; i++) {
	    dpy->event_vec[i] 	= _XUnknownWireEvent;
	    dpy->wire_vec[i] 	= _XUnknownNativeEvent;
	}
#endif
//	dpy->resource_id	= 0;
//	dpy->resource_shift	= ffs(dpy->resource_mask) - 1;

  printf("release = %d resource_base = %ld min_keycode = %d max_keycode = %d nformats = %d nscreens = %d byte_order = %d\n",
  dpy->release,
  dpy->resource_base,
  dpy->min_keycode,
  dpy->max_keycode,
  dpy->nformats,
  dpy->nscreens,
  dpy->byte_order);


//    for (i= dpy->resource_mask; i; i = (i>>1)) dpy->resource_shift++;
  for (i = 1, dpy->resource_shift = 0; dpy->resource_mask & i == 0; i = i<<1) dpy->resource_shift++;

    printf("resource_shift = %d resource_mask = %ld\n", dpy->resource_shift, dpy->resource_mask);

//	dpy->db 		= (struct _XrmResourceDataBase *)NULL;

  /* Initialize pointers to NULL so that XFreeDisplayStructure will work if we run out of memory */

//	dpy->screens = NULL;
//	dpy->display_name = NULL;
//	dpy->buffer = NULL;

  /* now extract the vendor string...  String must be null terminated,padded to multiple of 4 bytes. */
  vendorlen = ((xConnSetup *)p)->nbytesVendor;
  dpy->vendor = malloc(vendorlen + 1);
  p += sizeof(xConnSetup);	/* can't touch information in XConnSetup anymore..*/
  memcpy(dpy->vendor, p, vendorlen);
  dpy->vendor[vendorlen] = '\0';
  p += (vendorlen + 3) & ~3;

  printf("cool vendorlen =%d vendor = %s\n", vendorlen, dpy->vendor);


  /* Now iterate down setup information..... */
  printf("nformats = %d\n", dpy->nformats);
  dpy->pixmap_format = (ScreenFormat *) malloc((unsigned) (dpy->nformats *sizeof(ScreenFormat)));

  /* First decode the Z axis Screen format information. */
  for (i = 0; i < dpy->nformats; i++)
  {
    ScreenFormat *fmt = &dpy->pixmap_format[i];
    fmt->depth = ((xPixmapFormat *) p)->depth;
    fmt->bits_per_pixel = ((xPixmapFormat *) p)->bitsPerPixel;
    fmt->scanline_pad = ((xPixmapFormat *) p)->scanLinePad;
    fmt->ext_data = NULL;

    printf("depth = %d bpp = %d pad = %d\n", fmt->depth, fmt->bits_per_pixel, fmt->scanline_pad);
    p += sizeof(xPixmapFormat);
  }

  /* next the Screen structures. */
  printf("screens = %d\n",dpy->nscreens);
  dpy->screens = (Screen *) malloc((unsigned) dpy->nscreens * sizeof(Screen));

  /* Now go deal with each screen structure. */
  for (i = 0; i < dpy->nscreens; i++)
  {
    Screen *s = &dpy->screens[i];
	VisualID root_visualID = ((xWindowRoot *) p)->rootVisualID;
	printf("root_visualID = %ld\n", root_visualID);

    s->display	    = dpy;
    s->root 	    = ((xWindowRoot *) p)->windowId;
    s->cmap 	    = ((xWindowRoot *) p)->defaultColormap;
    s->white_pixel 	= ((xWindowRoot *) p)->whitePixel;
    s->black_pixel  = ((xWindowRoot *) p)->blackPixel;
    s->root_input_mask = ((xWindowRoot *) p)->currentInputMask;
    s->width	    = ((xWindowRoot *) p)->pixWidth;
    s->height	    = ((xWindowRoot *) p)->pixHeight;
    s->mwidth	    = ((xWindowRoot *) p)->mmWidth;
    s->mheight	    = ((xWindowRoot *) p)->mmHeight;
    s->min_maps     = ((xWindowRoot *) p)->minInstalledMaps;
    s->max_maps     = ((xWindowRoot *) p)->maxInstalledMaps;
    s->root_visual  = NULL;  /* filled in later, when we alloc Visuals */
    s->backing_store= ((xWindowRoot *) p)->backingStore;
    s->save_unders  = ((xWindowRoot *) p)->saveUnders;
    s->root_depth   = ((xWindowRoot *) p)->rootDepth;
    s->ndepths	    = ((xWindowRoot *) p)->nDepths;
 //   s->ext_data   = NULL;
    p += sizeof(xWindowRoot);

	printf("screen %d root=%ld height=%d width=%d\n", i, s->root, s->height, s->width);

    /* lets set up the depth structures. */
    s->depths = (Depth *) malloc(s->ndepths * sizeof(Depth));

	/* for all depths on this screen. */
	for (j = 0; j < s->ndepths; j++)
	{
      Depth *dp = &s->depths[j];

	  dp->depth = ((xDepth *) p)->depth;
	  dp->nvisuals = ((xDepth *) p)->nVisuals;
	  p += sizeof(xDepth);

	  dp->visuals = (Visual *) malloc(dp->nvisuals * sizeof(Visual));
      for (k = 0; k < dp->nvisuals; k++)
      {
		Visual *vp = &dp->visuals[k];

		if ((vp->visualid = ((xVisualType *) p)->visualID) == root_visualID) s->root_visual = vp;
		vp->class		= ((xVisualType *) p)->class;
		vp->bits_per_rgb= ((xVisualType *) p)->bitsPerRGB;
		vp->map_entries	= ((xVisualType *) p)->colormapEntries;
		vp->red_mask	= ((xVisualType *) p)->redMask;
		vp->green_mask	= ((xVisualType *) p)->greenMask;
		vp->blue_mask	= ((xVisualType *) p)->blueMask;
		vp->ext_data	= NULL;
		p += sizeof(xVisualType);

		printf("screen %d depth %d visual %d:  visualid=%ld class=%d bits_per_rgb=%d red_mask=%ld green_mask=%ld blue_mask=%ld\n",
		  i,j,k, vp->visualid, vp->class, vp->bits_per_rgb, vp->red_mask, vp->green_mask, vp->blue_mask);
	  }
	}
  }

  /* Setup other information in this display structure. */
//	dpy->vnumber = X_PROTOCOL;
//???	dpy->resource_alloc = _XAllocID;
//	dpy->synchandler = NULL;
//	dpy->request = 0;
//	dpy->last_request_read = 0;
	dpy->default_screen = screen_num;  /* Value returned by ConnectDisplay */

#if 0
	/* Salt away the host:display string for later use */
	if ((dpy->display_name = Xmalloc(
		(unsigned) (strlen(displaybuf) + 1))) == NULL) {
	        OutOfMemory (dpy, setup);
		UnlockMutex(&lock);
		return(NULL);
	}
	(void) strcpy (dpy->display_name, displaybuf);

 #endif
  /* Set up the output buffers. */
  dpy->bufptr = dpy->buffer = malloc(BUFSIZE);
  dpy->bufmax = dpy->buffer + BUFSIZE;

	/* Set up the input event queue and input event queue parameters. */
//	dpy->head = dpy->tail = NULL;
//	dpy->qlen = 0;

  /* Now start talking to the server to setup all other information... */

  free(setup);	/* all finished with setup information */

  /* Set up other stuff clients are always going to use. */
  for (i = 0; i < dpy->nscreens; i++)
  {
	Screen *sp = &dpy->screens[i];
	XGCValues values;
	values.foreground = sp->white_pixel;
	values.background = sp->black_pixel;
	sp->default_gc = XCreateGC(dpy, sp->root, GCForeground | GCBackground, &values);
  }

  /* call into synchronization routine so that all programs can be forced synchronize */
//  XSynchronize(dpy, _Xdebug);

  /* chain this stucture onto global list. */
//  dpy->next = _XHeadOfDisplayList;
//  _XHeadOfDisplayList = dpy;

#if 0
  /* get the resource manager database off the root window. */
	{
	    Atom actual_type;
	    int actual_format;
	    unsigned long nitems;
	    long leftover;
	    if (XGetWindowProperty(dpy, RootWindow(dpy, 0),
		XA_RESOURCE_MANAGER, 0L, 100000000L, False, XA_STRING,
		&actual_type, &actual_format, &nitems, &leftover,
		&dpy->xdefaults) != Success) {
			dpy->xdefaults = (char *) NULL;
		}
	    else {
	    if ( (actual_type != XA_STRING) ||  (actual_format != 8) ) {
		if (dpy->xdefaults != NULL) Xfree ( dpy->xdefaults );
		}
	    }
	}
#endif
  return dpy;
}

/* XCrGC.c */

/*
 * GenerateGCList looks at the GC dirty bits, and appends all the required
 * long words to the request being generated.  Clears the dirty bits in the GC.
 */
void _XGenerateGCList(Display *dpy, GC gc, xReq *req)
{
  /* Warning!  This code assumes that "unsigned long" is 32-bits wide */

  unsigned long values[32];
  unsigned long *value = values;
  long nvalues;
  XGCValues *gv = &gc->values;
  unsigned long dirty = gc->dirty;

  /* Note: The order of these tests are critical; the order must be the same as the GC mask bits in the word. */
  if (dirty & GCFunction)          *value++ = gv->function;
  if (dirty & GCPlaneMask)         *value++ = gv->plane_mask;
  if (dirty & GCForeground)        *value++ = gv->foreground;
  if (dirty & GCBackground)        *value++ = gv->background;
  if (dirty & GCLineWidth)         *value++ = gv->line_width;
  if (dirty & GCLineStyle)         *value++ = gv->line_style;
  if (dirty & GCCapStyle)          *value++ = gv->cap_style;
  if (dirty & GCJoinStyle)         *value++ = gv->join_style;
  if (dirty & GCFillStyle)         *value++ = gv->fill_style;
  if (dirty & GCFillRule)          *value++ = gv->fill_rule;
  if (dirty & GCTile)              *value++ = gv->tile;
  if (dirty & GCStipple)           *value++ = gv->stipple;
  if (dirty & GCTileStipXOrigin)   *value++ = gv->ts_x_origin;
  if (dirty & GCTileStipYOrigin)   *value++ = gv->ts_y_origin;
  if (dirty & GCFont)              *value++ = gv->font;
  if (dirty & GCSubwindowMode)     *value++ = gv->subwindow_mode;
  if (dirty & GCGraphicsExposures) *value++ = gv->graphics_exposures;
  if (dirty & GCClipXOrigin)       *value++ = gv->clip_x_origin;
  if (dirty & GCClipYOrigin)       *value++ = gv->clip_y_origin;
  if (dirty & GCClipMask)          *value++ = gv->clip_mask;
  if (dirty & GCDashOffset)        *value++ = gv->dash_offset;
  if (dirty & GCDashList)          *value++ = gv->dashes;
  if (dirty & GCArcMode)           *value++ = gv->arc_mode;

  req->length += (nvalues = value - values);

  nvalues <<= 2;

  if (dpy->bufptr + nvalues > dpy->bufmax) XFlush(dpy);
  memcpy(dpy->bufptr, values, nvalues);
  dpy->bufptr += nvalues;

  gc->dirty = 0L;
}

void FlushGC(Display *dpy, GC gc)
{
  xChangeGCReq *r;

  if (gc->dirty)
  {
    printf("FlushGC()\n");
    if (dpy->bufptr + sizeof(xChangeGCReq) > dpy->bufmax) XFlush(dpy);
    r = (xChangeGCReq *) dpy->bufptr;
    r->reqType = X_ChangeGC;
    r->length = sizeof(xChangeGCReq) >> 2;
    dpy->bufptr += sizeof(xChangeGCReq);
    dpy->request++;

    r->gc = gc->gid;
	r->mask = gc->dirty;

    _XGenerateGCList (dpy, gc, (xReq *) r);
  }
}

void _XUpdateGCCache (GC gc, long mask, XGCValues *att)
{
  XGCValues *gv = &gc->values;

  if (mask & GCFunction   && gv->function   != att->function)   { gv->function   = att->function;   gc->dirty |= GCFunction; }
  if (mask & GCPlaneMask  && gv->plane_mask != att->plane_mask) { gv->plane_mask = att->plane_mask; gc->dirty |= GCPlaneMask; }
  if (mask & GCForeground && gv->foreground != att->foreground) { gv->foreground = att->foreground; gc->dirty |= GCForeground; }
  if (mask & GCBackground && gv->background != att->background) { gv->background = att->background; gc->dirty |= GCBackground; }
  if (mask & GCLineWidth  && gv->line_width != att->line_width) { gv->line_width = att->line_width; gc->dirty |= GCLineWidth;  }
  if (mask & GCLineStyle  && gv->line_style != att->line_style) { gv->line_style = att->line_style; gc->dirty |= GCLineStyle; }
  if (mask & GCCapStyle   && gv->cap_style  != att->cap_style)  { gv->cap_style  = att->cap_style;  gc->dirty |= GCCapStyle; }
  if (mask & GCJoinStyle  && gv->join_style != att->join_style) { gv->join_style = att->join_style; gc->dirty |= GCJoinStyle; }
  if (mask & GCFillStyle  && gv->fill_style != att->fill_style) { gv->fill_style = att->fill_style; gc->dirty |= GCFillStyle; }
  if (mask & GCFillRule   && gv->fill_rule  != att->fill_rule)  { gv->fill_rule  = att->fill_rule;  gc->dirty |= GCFillRule; }
  if (mask & GCArcMode    && gv->arc_mode   != att->arc_mode)   { gv->arc_mode   = att->arc_mode;   gc->dirty |= GCArcMode; }

  /* always write through resource ID changes */
  if (mask & GCTile)                                            { gv->tile       = att->tile;       gc->dirty |= GCTile; }

  /* always write through resource ID changes */
  if (mask & GCStipple)                                         { gv->stipple    = att->stipple;    gc->dirty |= GCStipple; }
  if (mask & GCTileStipXOrigin && gv->ts_x_origin != att->ts_x_origin)     { gv->ts_x_origin = att->ts_x_origin; gc->dirty |= GCTileStipXOrigin; }
  if (mask & GCTileStipYOrigin && gv->ts_y_origin != att->ts_y_origin)     { gv->ts_y_origin = att->ts_y_origin; gc->dirty |= GCTileStipYOrigin; }

  /* always write through resource ID changes */
  if (mask & GCFont)                                                                   { gv->font               = att->font;               gc->dirty |= GCFont; }
  if (mask & GCSubwindowMode && gv->subwindow_mode != att->subwindow_mode)             { gv->subwindow_mode     = att->subwindow_mode;     gc->dirty |= GCSubwindowMode; }
  if (mask & GCGraphicsExposures && gv->graphics_exposures != att->graphics_exposures) { gv->graphics_exposures = att->graphics_exposures; gc->dirty |= GCGraphicsExposures; }
  if (mask & GCClipXOrigin && gv->clip_x_origin != att->clip_x_origin)                 { gv->clip_x_origin      = att->clip_x_origin;      gc->dirty |= GCClipXOrigin; }
  if (mask & GCClipYOrigin && gv->clip_y_origin != att->clip_y_origin)                 { gv->clip_y_origin      = att->clip_y_origin;      gc->dirty |= GCClipYOrigin; }
  if (mask & GCClipMask && (gv->clip_mask != att->clip_mask || gc->rects == 1))        { gv->clip_mask          = att->clip_mask;          gc->dirty |= GCClipMask; gc->rects = 0; }
  if (mask & GCDashOffset && gv->dash_offset != att->dash_offset)                      { gv->dash_offset        = att->dash_offset;        gc->dirty |= GCDashOffset; }
  if (mask & GCDashList && (gv->dashes != att->dashes || gc->dashes == 1))             { gv->dashes             = att->dashes;             gc->dirty |= GCDashList; gc->dashes = 0; }

  return;
}

GC XCreateGC(	Display *dpy, Drawable d,	/* Window or Pixmap for which depth matches */
				unsigned long valuemask,	/* which ones to set initially */
				XGCValues *values)			/* the values themselves */
{
  GC gc;
  xCreateGCReq *r;

  gc = (GC) malloc(sizeof(struct _XGC));
  memset(gc, 0, sizeof(struct _XGC));

  gc->values.function   = GXcopy;
  gc->values.plane_mask = AllPlanes;
  gc->values.background = 1;
  gc->values.cap_style  = CapButt;
  gc->values.arc_mode   = ArcPieSlice;
  gc->values.tile       = ~0;
  gc->values.stipple    = ~0;
  gc->values.font       = ~0;
  gc->values.graphics_exposures = 1;
  gc->values.dashes     = 4;

  if (valuemask) _XUpdateGCCache (gc, valuemask, values);

  if (dpy->bufptr + sizeof(xCreateGCReq) > dpy->bufmax) XFlush(dpy);
  r = (xCreateGCReq *) dpy->bufptr;
  r->reqType = X_CreateGC;
  r->length = sizeof(xCreateGCReq) >> 2;
  dpy->bufptr += sizeof(xCreateGCReq);
  dpy->request++;

  r->drawable = d;
  r->gc = gc->gid = XAllocID(dpy);

  if ((r->mask = gc->dirty))
    _XGenerateGCList (dpy, gc, (xReq *) r);

  printf("XCreateGC(%ld)\n",gc->gid);

  return gc;
}

void XChangeGC(Display *dpy, GC gc, unsigned long valuemask, XGCValues *values)
{
  if (valuemask) _XUpdateGCCache (gc, valuemask, values);

  /* if any Resource ID changed, must flush */
  if (valuemask & (GCFont | GCTile | GCStipple)) FlushGC(dpy, gc);
}

GContext XGContextFromGC(GC gc)
{
  return (gc->gid);
}

Window XCreateSimpleWindow(Display *d, Window parent, int x, int y, int width, int height, int bwidth, unsigned long border, unsigned long background)
{
  Window w;
  xCreateWindowReq *r;

  if (d->bufptr + sizeof(xCreateWindowReq) + 8 > d->bufmax) XFlush(d);
  r = (xCreateWindowReq *) d->bufptr;
  r->reqType = X_CreateWindow;
  r->length = (sizeof(xCreateWindowReq) + 8) >> 2;
  d->bufptr += sizeof(xCreateWindowReq) + 8;
  d->request++;

  r->parent = parent;
  r->x = x;
  r->y = y;
  r->width = width;
  r->height = height;
  r->borderWidth = bwidth;
  r->depth = 0;
  r->class = CopyFromParent;
  r->visual = CopyFromParent;

  w = r->wid = XAllocID(d);

  r->mask = CWBackPixel | CWBorderPixel;
  ((unsigned long *) (r+1))[0] = background;
  ((unsigned long *) (r+1))[1] = border;

  printf("XCreateSimpleWindow(%ld)\n",w);


  return (w);
}

void XSelectInput(Display *d, Window w, unsigned long mask)
{
  xChangeWindowAttributesReq *r;
  printf("XSelectInput()\n");

  if (d->bufptr + sizeof(xChangeWindowAttributesReq) + 4 > d->bufmax) XFlush(d);
  r = (xChangeWindowAttributesReq *) d->bufptr;
  r->reqType = X_ChangeWindowAttributes;
  r->length = (sizeof(xChangeWindowAttributesReq) + 4) >> 2;
  d->bufptr += sizeof(xChangeWindowAttributesReq) + 4;
  d->request++;

  r->window = w;
  r->valueMask = CWEventMask;
  ((unsigned long *) (r+1))[0] = mask;
}

void XMapWindow(Display *d, Window w)
{
  xResourceReq *r;
  printf("XMapWindow()\n");

  if (d->bufptr + sizeof(xResourceReq) > d->bufmax) XFlush(d);
  r = (xResourceReq *) d->bufptr;
  r->reqType = X_MapWindow;
  r->length = sizeof(xResourceReq) >> 2;
  r->id = w;
  d->bufptr += sizeof(xResourceReq);
  d->request++;
}

void XNextEvent(Display *d, XEvent *e)
{
  XFlush(d);
  for (;;)
  {
    recv(d->fd, (char *) e, sizeof(XEvent), 0);
    printf("XNextEvent(ty=%d detail=%d seq=%d) p1=%ld p2=%ld\n", e->type, e->detail, e->sequence_number, e->p1, e->p2);
    if (e->type == X_Error)
    {
      printf("X_Error(resourceid = %ld, error_code = %d, minor_major_code = %ld)\n", e->p1, e->detail, e->p2);
      //continue;
      break;
    }
    else
      break;
  }
}

void XFillRectangle(Display *d, Window win, GC gc, int x, int y, int w, int h)
{
  xPolyFillRectangleReq *r;
  printf("XFillRectangle()\n");
  if (gc->dirty) FlushGC(d, gc);

  if (d->bufptr + sizeof(xPolyFillRectangleReq) + sizeof(xRectangle) > d->bufmax) XFlush(d);
  r = (xPolyFillRectangleReq *) d->bufptr;
  r->reqType = X_PolyFillRectangle;
  r->length = (sizeof(xPolyFillRectangleReq) + sizeof(xRectangle)) >> 2;
  d->bufptr += sizeof(xPolyFillRectangleReq) + sizeof(xRectangle);
  d->request++;

  r->drawable = win;
  r->gc = gc->gid;
  ((xRectangle *) (r+1))->x = x;
  ((xRectangle *) (r+1))->y = y;
  ((xRectangle *) (r+1))->width = w;
  ((xRectangle *) (r+1))->height = h;
}

void XDrawString(Display *dpy, Drawable d, GC gc, int x, int y, char *string, int length)
{
  int Datalength;
  xPolyText8Req *r;
  int nbytes;
  int PartialNChars;
  xTextElt *elt;
  char *CharacterOffset;

  printf("XDrawString()\n");

  if (length <= 0)
    return;

  if (dpy->bufptr + sizeof(xPolyText8Req) > dpy->bufmax) XFlush(dpy);
  r = (xPolyText8Req *) dpy->bufptr;
  r->reqType = X_PolyText8;
  r->length = sizeof(xPolyText8Req) >> 2;
  dpy->bufptr += sizeof(xPolyText8Req);
  dpy->request++;

  r->drawable = d;
  r->gc = gc->gid;
  r->x = x;
  r->y = y;

  Datalength = sizeof(xTextElt) * ((length + 253) / 254) + length;

  r->length += (Datalength + 3)>>2;  /* convert to number of 32-bit words */

    /*
     * If the entire request does not fit into the remaining space in the
     * buffer, flush the buffer first.   If the request does fit into the
     * empty buffer, then we won't have to flush it at the end to keep
     * the buffer 32-bit aligned.
     */

  if (dpy->bufptr + Datalength > dpy->bufmax)
  	XFlush (dpy);

  PartialNChars = length;
  CharacterOffset = string;

  while(PartialNChars > 254)
  {
    nbytes = 254 + sizeof(xTextElt);

//    BufAlloc (xTextElt *, elt, nbytes);
    if (dpy->bufptr + nbytes > dpy->bufmax) XFlush(dpy);
    elt = (xTextElt *) dpy->bufptr;
    dpy->bufptr += nbytes;

    elt->delta = 0;
    elt->len = 254;
    memcpy((char *) (elt + 1), CharacterOffset, 254);
    PartialNChars = PartialNChars - 254;
    CharacterOffset += 254;
  }

  if (PartialNChars)
  {
    nbytes = PartialNChars + sizeof(xTextElt);

//    BufAlloc (xTextElt *, elt, nbytes);
    if (dpy->bufptr + nbytes > dpy->bufmax) XFlush(dpy);
    elt = (xTextElt *) dpy->bufptr;
    dpy->bufptr += nbytes;

    elt->delta = 0;
    elt->len = PartialNChars;
    memcpy((char *) (elt + 1), CharacterOffset, PartialNChars);
  }

    /* Pad request out to a 32-bit boundary */

  if (Datalength &= 3)
  {
	char *pad;
	/*
	 * BufAlloc is a macro that uses its last argument more than
	 * once, otherwise I'd write "BufAlloc (char *, pad, 4-length)"
	 */
	length = 4 - Datalength;

//	BufAlloc (char *, pad, length);
    if (dpy->bufptr + length > dpy->bufmax) XFlush(dpy);
    pad = (char *) dpy->bufptr;
    dpy->bufptr += length;

	/*
	 * if there are 3 bytes of padding, the first byte MUST be 0
	 * so the pad bytes aren't mistaken for a final xTextElt
	 */
	*pad = 0;
  }

    /*
     * If the buffer pointer is not now pointing to a 32-bit boundary,
     * we must flush the buffer so that it does point to a 32-bit boundary
     * at the end of this routine.
     */

  if ((dpy->bufptr - dpy->buffer) & 3)
    XFlush (dpy);

}




void XCloseDisplay(Display *d)
{
#ifdef WIN32
  closesocket(d->fd);
  WSACleanup();
#else
  close(d->fd);
#endif
}

#if 0 //???????????????????

/*
 * GetReq - Get the next avilable X request packet in the buffer and
 * return it.
 *
 * "name" is the name of the request, e.g. CreatePixmap, OpenFont, etc.
 * "req" is the name of the request pointer.
 *
 */

#define GetReq(name, req) \
	if ((dpy->bufptr + sizeof(x/**/name/**/Req)) > dpy->bufmax)\
		_XFlush(dpy);\
	req = (x/**/name/**/Req *)(dpy->last_req = dpy->bufptr);\
	req->reqType = X_/**/name;\
	req->length = (sizeof(x/**/name/**/Req))>>2;\
	dpy->bufptr += sizeof(x/**/name/**/Req);\
	dpy->request++

/* GetReqExtra is the same as GetReq, but allocates "n" additional
   bytes after the request. "n" must be a multiple of 4!  */


#define GetReqExtra(name, n, req) \
	if ((dpy->bufptr + sizeof(x/**/name/**/Req) + n) > dpy->bufmax)\
		_XFlush(dpy);\
	req = (x/**/name/**/Req *)(dpy->last_req = dpy->bufptr);\
	req->reqType = X_/**/name;\
	req->length = (sizeof(x/**/name/**/Req) + n)>>2;\
	dpy->bufptr += sizeof(x/**/name/**/Req) + n;\
	dpy->request++

/*
 * GetResReq is for those requests that have a resource ID
 * (Window, Pixmap, GContext, etc.) as their single argument.
 * "rid" is the name of the resource.
 */

#define GetResReq(name, rid, req) \
	if ((dpy->bufptr + sizeof(xResourceReq)) > dpy->bufmax)\
	    _XFlush(dpy);\
	req = (xResourceReq *) (dpy->last_req = dpy->bufptr);\
	req->reqType = X_/**/name;\
	req->length = 2;\
	req->id = (rid);\
	dpy->bufptr += sizeof(xResourceReq);\
	dpy->request++

/*
 * GetEmptyReq is for those requests that have no arguments
 * at all.
 */

#define GetEmptyReq(name, req) \
	if ((dpy->bufptr + sizeof(xReq)) > dpy->bufmax)\
	    _XFlush(dpy);\
	req = (xReq *) (dpy->last_req = dpy->bufptr);\
	req->reqType = X_/**/name;\
	req->length = 1;\
	dpy->bufptr += sizeof(xReq);\
	dpy->request++

#define SyncHandle() \
	if (dpy->synchandler) (*dpy->synchandler)(dpy)

#define FlushGC(dpy, gc) \
	if ((gc)->dirty) _XFlushGCCache((dpy), (gc))
/*
 * Data - Place data in the buffer and pad the end to provide
 * 32 bit word alignment.  Transmit if the buffer fills.
 *
 * "dpy" is a pointer to a Display.
 * "data" is a pinter to a data buffer.
 * "len" is the length of the data buffer.
 * we can presume buffer less than 2^16 bytes, so bcopy can be used safely.
 */
#define Data(dpy, data, len) \
	if (dpy->bufptr + (len) <= dpy->bufmax) {\
		bcopy(data, dpy->bufptr, (int)len);\
		dpy->bufptr += ((len) + 3) & ~3;\
	} else\
		_XSend(dpy, data, len)


/* Allocate bytes from the buffer.  No padding is done, so if
 * the length is not a multiple of 4, the caller must be
 * careful to leave the buffer aligned after sending the
 * current request.
 *
 * "type" is the type of the pointer being assigned to.
 * "ptr" is the pointer being assigned to.
 * "n" is the number of bytes to allocate.
 *
 * Example:
 *    xTextElt *elt;
 *    BufAlloc (xTextElt *, elt, nbytes)
 */

#define BufAlloc(type, ptr, n) \
    if (dpy->bufptr + (n) > dpy->bufmax) \
        _XFlush (dpy); \
    ptr = (type) dpy->bufptr; \
    dpy->bufptr += (n);

#ifndef BIGSHORTS
#define PackData(dpy, data, len) Data(dpy, data, len)
#define PackShorts(f, t, n)  bcopy(f, t, n)
#endif

#endif


