#include "RSCommon.h"
#ifdef CONS
#undef CONS
#endif

#include "xlshlib.h"

#include "Converters.h"



/**
 This is a basic routine that allows one to call R
 functions from XLisp with no named arguments.
 For example, we 
  (R:CALL "hist" (poisson-rand 30 3.2))
  (R:CALL "rnorm" 10)
  (R:CALL "rnorm" 10 20)
  (R:CALL "plot" (poisson-rand 30 3.2))
  (R:CALL "plot" (poisson-rand 30 3.2) (R:CALL "rnorm" 30 ))


 It is tempting to handle named arguments in an XLisp-style.
 We might handle any symbol value in the argument list
 as the name of the next argument. This would allow calls of the
 form
  (R:CALL "plot" z :xlab "My label")

 There are a couple of problems with this.
  a) what if we have an actual symbol as a regular argument

  b) XLisp makes all symbols upper-case, so we cannot
   determine what the actual name of the argument is
   e.g. if R has a parameter named myValue and this is
   given in XLisp as :myValue, we get MYVALUE and we
   cannot convert it correctly

  c) we have to scan the XLisp argument list before
    

 To do this, it has to scan the argument list and
 identify the symbols and regular arguments first.
 This is partly because we cannot resize a LANGSXP
 in R.
 See extendedCallR() below for an implementation.
*/
static LVAL 
callR()
{
  int nargs, i;
  USER_OBJECT_ e = NULL_USER_OBJECT, sval, stmp;
  LVAL ans, xtmp;
  int errorOccurred = 0;

  nargs = xlargc - 1;
  PROTECT(e = allocVector(LANGSXP, nargs + 1));

   /* Handle function references. */
  xtmp = xlgetarg();
  SETCAR(e, Rf_install(getstring(xtmp)));
 
  stmp = CDR(e);
  for(i = 0; i < nargs; i++) {
     xtmp = xlgetarg();
     SETCAR(stmp, convertToR(xtmp));
     stmp = CDR(stmp);    
  }
  xllastarg();
  
  sval = R_tryEval(e, R_GlobalEnv, &errorOccurred);
  if(errorOccurred) {
    UNPROTECT(1);
    xlerror("Problem in R function call", NIL);
  }
  PROTECT(sval);
  ans = convertToXLisp(sval);
  UNPROTECT(2);

  return ans;
}


#define MAX_ARGS 100

/**
 This provides a heuristic but not fool-proof mechanism for
 using the XLisp syntax for specifying named arguments.

 (R:EXTENDEDCALL "plot" (R:CALL "rnorm" 10) :xlab "My X Label" :main "Main Title")

 This will not handle parameter names in R that have mixed case as we (have to)
 convert the XLisp names to lower case.

 So this is only offered as a convenience function. Please use 
 R:NAMEDCALL from XLisp.
*/
static LVAL 
extendedCallR()
{
  int nargs = 0, i, n;
  USER_OBJECT_ e = NULL_USER_OBJECT, sval, stmp;
  LVAL ans, xtmp;
  int errorOccurred = 0;
  LVAL args[MAX_ARGS];

  /* Scan the argument list and store them locally and compute the number of regular arguments,
     i.e. not names for arguments */
  n = xlargc;
  for(i = 0; i < n ; i++) {
     args[i] = xlgetarg();
     if(i > 0 && !symbolp(args[i]))
       nargs++;
  }
  xllastarg();

  PROTECT(e = allocVector(LANGSXP, nargs + 1));

   /* Handle function references. */
  SETCAR(e, Rf_install(getstring(args[0])));
 
  stmp = CDR(e);
  /* Loop over the XLisp arguments and handle the argument names and the regular values. */
  for(i = 1; i <= nargs; i++) {
     char *s = NULL;
     xtmp = args[i];
     if(symbolp(xtmp)) {
        LVAL sval = getpname(xtmp);
        s = getstring(sval);
	xtmp = args[++i];
     }
     SETCAR(stmp, convertToR(xtmp));

     /*  Set the argument name, converting to lower case! */
     if(s) {
       char *p;
       s = p = strdup(s);
       while(*p) {
         p[0] = tolower(p[0]);
	 p++;
       }
       SET_TAG(stmp, Rf_install(s));
       free(s);
     }
     stmp = CDR(stmp);    
  }

  
  sval = R_tryEval(e, R_GlobalEnv, &errorOccurred);

  if(errorOccurred) {
    UNPROTECT(1);
    xlerror("Problem in R function call", NIL);
  }
  PROTECT(sval);
  ans = convertToXLisp(sval);
  UNPROTECT(2);

  return ans;
}


/**
  Allows the XLisp user to call R functions with named
  arguments by giving the name-value pairs in order.
  (R:NAMEDCALL "plot" "" z "ylab" "My Y Label" "cex" 3)
*/
static LVAL 
namedArgCall()
{
  int nargs, i;
  USER_OBJECT_ e = NULL_USER_OBJECT, sval, stmp;
  LVAL ans, xtmp;
  int errorOccurred = 0;

  nargs = (xlargc - 1)/2;
  PROTECT(e = allocVector(LANGSXP, nargs + 1));

   /* Handle function references. */
  xtmp = xlgetarg();
  SETCAR(e, Rf_install(getstring(xtmp)));
 
  stmp = CDR(e);
  for(i = 0; i < nargs; i++) {
     char *s;
     xtmp = xlgetarg();
     s = getstring(xtmp);
     xtmp = xlgetarg();
     SETCAR(stmp, convertToR(xtmp));
     if(s && s[0]) {
       SET_TAG(stmp, Rf_install(s));
     }
     stmp = CDR(stmp);    
  }
  xllastarg();
  
  sval = R_tryEval(e, R_GlobalEnv, &errorOccurred);
  if(errorOccurred) {
    UNPROTECT(1);
    xlerror("Problem in R function call", NIL);
  }
  PROTECT(sval);
  ans = convertToXLisp(sval);
  UNPROTECT(2);

  return ans;
}


int
getConsLength(LVAL arg)
{
  int n;
  for (n = 0; consp(arg);) {
      arg = cdr(arg);
      n++;
  }
  return(n);
}


static LVAL
initR()
{
 extern int Rf_initEmbeddedR(int argc, char **argv);

  int nargs = 0, i;
  LVAL xargs, el;
  char **sargs = NULL; 

  if(xlargc == 0) {
    nargs = 1;
    sargs = (char **) malloc(sizeof(char *));
    sargs[0] = strdup("RXLisp");
  } else {
    xargs = xlgetarg();

    if(ntype(xargs) == CONS) {
      nargs = getConsLength(xargs);
      el = xargs;
      sargs = (char **) malloc(sizeof(char *) * nargs);
      for(i = 0 ; i < nargs ; i++) {
       if(!stringp(car(el))) {
          xlerror("Non string value passed to R:INIT()", NIL);
       }
       sargs[i] = getstring(car(el));
       el = cdr(el);
      }
    } else if(ntype(xargs) == STRING) {
       nargs = 1;
       sargs = (char **) malloc(sizeof(char *) * nargs);
       sargs[0] = getstring(xargs);
    } else {
      xlerror("Argument to R:INIT() must be a single string or a list of strings", NIL);
    }
  }

  Rf_initEmbeddedR(nargs, sargs);

  return cvfixnum(1);
}

static FUNDEF myfuns[] = {
  { "R:CALL", SUBR, callR },
  { "R:NAMEDCALL", SUBR, namedArgCall },
  { "R:INIT", SUBR, initR },
  { "R:EXTENDEDCALL", SUBR, extendedCallR},

  { NULL, 0, NULL }
};

static ULONGCONSTDEF myulongconsts[] = {
  {NULL, 0}
};

static xlshlib_modinfo_t myinfo = {
  XLSHLIB_VERSION_INFO(0,1,0,1),
  myfuns,
  NULL, 
  NULL, 
  NULL, 
  myulongconsts
};

xlshlib_modinfo_t *Rmodule__init() { return &myinfo; }
