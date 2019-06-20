#include "RSCommon.h"

#ifdef CONS
#undef CONS
#endif
#include "xlisp.h"
#include <X11/X.h>
#include <X11/Xlib.h>
#include "xlgraph.h"

#include "Converters.h"
#include "R_ext/eventloop.h"

extern void XLispInitialize(int argc, char *argv[], CONTEXT *);

CONTEXT context;

static Rboolean XLispIsInitialized = FALSE;



void
RXLisp_EventHandler(void *data)
{
   extern void StProcessEvent();
   Display *dpy = StX11Display();

   while(XEventsQueued(dpy, QueuedAfterReading) > 0) {
	   XEvent ev;
	   XNextEvent(dpy, &ev);
	   StProcessEvent(dpy, ev);
   }
}

USER_OBJECT_
RXLisp_init(USER_OBJECT_ args, USER_OBJECT_ registerEvent)
{
 char **argv;
 int n = GET_LENGTH(args), i;

 if(!XLispIsInitialized) {
      /* Initialize only once and then set the flag to say it has been done.  */
   argv = (char **) malloc(sizeof(char *) * n);
   if(!argv) {
     PROBLEM  "cannot allocate space for XLisp initialization command line arguments"
     ERROR;
   }

    /* Copy the command line arguments so that they persist after the call is made. */
   for(i = 0; i < n ; i++) {
     char *tmp;
     USER_OBJECT_ el;
     el = STRING_ELT(args, i);
     tmp = CHAR(el);
     argv[i] = strdup(tmp);
   }

   /* Now, initialize XLisp. */
   XLispInitialize(n, argv, &context);
   XLispIsInitialized = TRUE;

   if(LOGICAL_DATA(registerEvent)[0]) {
       /* Register with the R event loop to handle all X events. */
     addInputHandler(R_InputHandlers, ConnectionNumber(StX11Display()), RXLisp_EventHandler, 10);
   }
 }

 RXLisp_initConverters();

 return(NULL_USER_OBJECT);
}



USER_OBJECT_
RXLisp_call(USER_OBJECT_ funName, USER_OBJECT_ args, USER_OBJECT_ convert, USER_OBJECT_ isFun)
{
  FRAMEP newfp, oldsp;
  LVAL xans;
  USER_OBJECT_ ans = NULL_USER_OBJECT;
  int nargs, i;
  CONTEXT ctxt;

  LVAL sym;

  nargs = GET_LENGTH(args);
  xlbegin(&ctxt, CF_TOPLEVEL | CF_CLEANUP | CF_BRKLEVEL | CF_ERROR, (LVAL)1);
  if(XL_SETJMP(ctxt.c_jmpbuf)) {
     PROBLEM "Error in XLisp computation"
     ERROR;
  }

  sym = xlenter(CHAR_DEREF(STRING_ELT(funName, 0)));

  if(!boundp(sym)) {
   sym = xlgetfunction(sym);
  }

  if(symbolp(sym) && !boundp(sym)) {
   xlpop();
   PROBLEM "No XLisp function by name %s",
	   CHAR_DEREF(STRING_ELT(funName, 0))
   ERROR;
  }

  newfp = oldsp = xlsp;
  pusharg(NIL);
  pusharg(sym);
  pusharg(NIL); 


/*  pusharg(cvfixnum(nargs)); */
  for(i = 0; i < nargs; i++) {
     pusharg(convertToXLisp(VECTOR_ELT(args, i)));
  }
  oldsp[0] = cvfixnum((FIXTYPE) (newfp - xlfp));
  oldsp[2] = cvfixnum(nargs);

  xlfp = newfp;


  xlsave(xans);
  xans = xlapply(nargs);

  if(LOGICAL_DATA(convert)[0]) {
    ans = convertToR(xans);
  } else {
    ans = makeXLispReference(xans, NULL);
  }

  xlpop();

  xlend(&ctxt);

  return(ans);
}




