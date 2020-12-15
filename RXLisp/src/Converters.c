#include "RSCommon.h"

#ifdef CONS
#undef CONS
#endif
#ifdef list2
#undef list2
#endif
#ifdef list3
#undef list3
#endif

#include "xlisp.h"
#include "xlstat.h"

#include "Converters.h"


static USER_OBJECT_ XLispObjectReferenceClass = NULL;

static LVAL XLispReferenceManager = NULL;

LVAL getXLispObjectReference(USER_OBJECT_ val);

void
RXLisp_initConverters(void)
{
 XLispObjectReferenceClass = Rf_install("XLispObjectReference");
}

LVAL convertRList(USER_OBJECT_ robj);


Rboolean isXLispObjectReference(USER_OBJECT_ robj)
{
  USER_OBJECT_ tmp;
  if(TYPEOF(robj) != VECSXP || GET_LENGTH(robj) < 2 ||   TYPEOF((tmp = VECTOR_ELT(robj, 1))) != EXTPTRSXP)
    return(FALSE);

  return(R_ExternalPtrTag(tmp) == XLispObjectReferenceClass ? TRUE : FALSE);
}

LVAL
convertToXLisp(USER_OBJECT_ robj)
{
  int n = GET_LENGTH(robj), i;
  LVAL ans = NULL, el;

  if(TYPEOF(robj) == NILSXP)
    return(NIL);

  if(isXLispObjectReference(robj)) {
    ans = getXLispObjectReference(robj);
    return(ans);
  } else if(TYPEOF(robj) == VECSXP) {
    return(convertRList(robj));
  } else if(TYPEOF(robj) == CLOSXP) {
    Rf_warning("Cannot currently convert R functions to XLisp-Stat");
    return(NIL);
  }

  if(n > 1) {
    LVAL tmp = NIL;
    xlsave(el);
    xlsave(ans);

    ans = newvector(n);
    for(i = 0; i < n ; i++) {
       switch(TYPEOF(robj)) {
         case REALSXP:
 	  el = cvflonum(NUMERIC_DATA(robj)[i]);
         break;
         case INTSXP:
          el = cvfixnum(INTEGER_DATA(robj)[i]);
	 break;
         case LGLSXP:
          el = cvfixnum(LOGICAL_DATA(robj)[i]);
	 break;
         case STRSXP:
          el = cvstring(CHAR_DEREF(STRING_ELT(robj, i)));
	 break;
         case VECSXP:
          rplacd(tmp, convertToXLisp(VECTOR_ELT(robj, i)));
          tmp = cdr(tmp);
	 break;
         default:
           continue;
       }
/*
    ans = NIL; 
    ans = cons(ans, el);
*/
       setelement(ans, i, el);
    }
    xlpopn(2);
  } else {
   switch(TYPEOF(robj)) {
    case REALSXP:
      ans = cvflonum(NUMERIC_DATA(robj)[0]);
      break;
    case INTSXP:
      ans = cvfixnum(INTEGER_DATA(robj)[0]);
      break;
    case LGLSXP:
      ans = cvfixnum(LOGICAL_DATA(robj)[0]);
      break;
    case STRSXP:
    { 
      char *tmp = CHAR_DEREF(STRING_ELT(robj,0));
      if(tmp && tmp[0] == ':')
        ans = xlintern(tmp+1, xlkeypack);
      else
        ans = cvstring(tmp);
    }
      break;
    default:
      Rf_error("Converter to XLisp not implemented yet for %d", 
	       TYPEOF(robj));
    }
  }
  return(ans);
}

int
XL_getLength(LVAL xobj)
{
 extern LVAL xsfuncall1(LVAL, LVAL);
 LVAL ans;
 ans = xsfuncall1(xlenter("LENGTH"), xobj);
 return(getfixnum(ans));
}

USER_OBJECT_
convertToR(LVAL xobj)
{
   USER_OBJECT_ ans = NULL_USER_OBJECT;

   if(xobj == NIL)
     return(NULL_USER_OBJECT);

   switch(ntype(xobj)) {
     case FIXNUM:
	ans = NEW_INTEGER(1);
	INTEGER_DATA(ans)[0] = getfixnum(xobj);
	break;
     case FLONUM:
	ans = NEW_NUMERIC(1);
	NUMERIC_DATA(ans)[0] = getflonum(xobj);
	break;

#ifdef XL_CHAR
     case XL_CHAR:
       {
        char buf[2] = "a";
	PROTECT(ans = NEW_CHARACTER(1));
	buf[0] = getchcode(xobj);
	SET_STRING_ELT(ans, 0, Rf_mkChar(buf));
        UNPROTECT(1);
       }
	break;
#endif /* XL_CHAR */

     case STRING:
	PROTECT(ans = NEW_CHARACTER(1));
	SET_STRING_ELT(ans, 0, Rf_mkChar(getstring(xobj)));
        UNPROTECT(1);
	break;
     case CONS:
     case VECTOR:
       {
        int n = XL_getLength(xobj), i;
        int type = -1; 
	USER_OBJECT_ el;
	LVAL xel;
	Rboolean homogeneous = TRUE;
	PROTECT(ans = NEW_LIST(n));

	for(i = 0; i < n; i++) {
    	   if(consp(xobj)) {
   	     xel = car(xobj);
  	     xobj = cdr(xobj);
	   } else {
	     xel = getelement(xobj, i);
	   }
           SET_VECTOR_ELT(ans, i, el = convertToR(xel));
           if(GET_LENGTH(ATTRIB(el)))
  	      homogeneous = FALSE;
           if(homogeneous) {
  	     if(i == 0)
  	       type = TYPEOF(el);
             else
               homogeneous = (type == TYPEOF(el)); 
	   }
	}
         /* Now collapse to a vector if homogeneous types. */
        if(homogeneous) {
          switch(type) {
   	     case INTSXP:
  	       ans = AS_INTEGER(ans);
	     break;
   	     case STRSXP:
  	       ans = AS_CHARACTER(ans);
	     break;
   	     case REALSXP:
  	       ans = AS_NUMERIC(ans);
	     break;
	  }
	}
	UNPROTECT(1);
       }
        break;

     case OBJECT:
       ans = makeXLispReference(xobj, "XLispObject");
      break;
     case SYMBOL:
     {
        USER_OBJECT_ tmp;
        PROTECT(ans = convertToR(getpname(xobj)));
	PROTECT(tmp = NEW_CHARACTER(1));
        SET_STRING_ELT(tmp, 0, Rf_mkChar("XLispSymbol"));
	SET_CLASS(ans, tmp);
	UNPROTECT(2);
     }
       break;
     default:
       Rf_warning("Converter to R not implemented yet for %d", 
		  ntype(xobj));
   }

   return(ans);
}

LVAL
getXLispReferenceManager()
{
  if(!XLispReferenceManager) {
    XLispReferenceManager = xlgetvalue(xlenter("REFERENCEMANAGER"));
  }

  return(XLispReferenceManager);
}


LVAL
callXLispReferenceManager(const char * const method, LVAL arg)
{
  LVAL args[3], xans;

  int n = sizeof(args)/sizeof(args[0]), i;
  for(i = 0; i < n; i++)
    xlsave1(args[i]);

  args[0] = getXLispReferenceManager();
  args[1] = xlenter((char *) method);
  args[2] = arg;
 
  pushargvec(xlenter("SEND"), n, args);
  xans = xlapply(n);
  xlpopn(n);

  return(xans);
}

LVAL
getXLispObjectReference(USER_OBJECT_ val)
{
  char buf[10];
  LVAL arg, ans;
  sprintf(buf, "%d", INTEGER_DATA(VECTOR_ELT(val, 0))[0]);
  xlsave1(arg);
  arg = cvstring(strdup(buf));
  ans = callXLispReferenceManager(":GET", arg);
  xlpop();
  return(ans);
}

int
registerXLispReference(LVAL xval)
{
  LVAL v;
  int val;
  v = callXLispReferenceManager(":ASSIGN", xval);
  val = getfixnum(v);
  return(val);
}


/*
XXX  Need to ensure that xobj is protected from garbage
 collection from XLisp. Can put into a special table or list
 variable that is itself protected.

Set the class if ntype(xobj) == OBJECT.
 */
USER_OBJECT_
makeXLispReference(LVAL xobj, const char * const className)
{
  USER_OBJECT_ ans, tmp, names;
  int pos;

  pos = registerXLispReference(xobj);

  PROTECT(ans = NEW_LIST(2));
  SET_VECTOR_ELT(ans, 0, tmp = NEW_INTEGER(1));
  INTEGER_DATA(tmp)[0] = pos;
  SET_VECTOR_ELT(ans, 1, R_MakeExternalPtr((void *) xobj, XLispObjectReferenceClass, NULL_USER_OBJECT));

  PROTECT(names = NEW_CHARACTER(2));
  SET_STRING_ELT(names, 0, Rf_mkChar("id"));
  SET_STRING_ELT(names, 1, Rf_mkChar("address"));
  SET_NAMES(ans, names);

  PROTECT(names = NEW_CHARACTER(className ? 2 : 1));
  pos = 0;
  if(className)
    SET_STRING_ELT(names, pos++, Rf_mkChar(className));  
  SET_STRING_ELT(names, pos++, Rf_mkChar("XLispReference"));  
  SET_CLASS(ans, names);

  UNPROTECT(3);

  return(ans);
}

#undef cons

extern LVAL cons (LVAL, LVAL);
 /* Convert an R list() to an XLisp cons() list. */
LVAL
convertRList(USER_OBJECT_ robj)
{
    LVAL tmp, ans;
    int i ,n;
    n = GET_LENGTH(robj); 

    if(n == 0)
      return(consa(NIL));

    xlsave1(ans);
    ans = consa(convertToXLisp(VECTOR_ELT(robj, n-1)));
    if(n > 1) {
      tmp = cdr(ans);
      for(i = n-2; i >= 0 ; i--) {
        ans = cons(convertToXLisp(VECTOR_ELT(robj, i)), ans);
      }
    }
    xlpop();
    return(ans);
}
