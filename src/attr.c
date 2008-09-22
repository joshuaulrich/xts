#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "xts.h"

/*
#define  xts_IndexSymbol        install("index")
#define  xts_ClassSymbol        install(".CLASS")
#define  xts_IndexFormatSymbol  install(".indexFORMAT")
#define  xts_IndexClassSymbol   install(".indexCLASS")
#define  xts_ATTRIB(x)          coerceVector(do_xtsAttributes(x),LISTSXP)
*/
SEXP do_xtsAttributes(SEXP x)
{
  SEXP a, values, names;
  int nattr=0, i=0, P=0;

  a = ATTRIB(x);
  if(length(a) <= 0)
    return R_NilValue;
  PROTECT(a); P++; // all attributes
  PROTECT(values = allocVector(VECSXP, length(a))); P++;
  PROTECT(names  = allocVector(STRSXP, length(a))); P++;

  //PROTECT(a = getAttrib(ATTRIB(x),R_NamesSymbol));  // this gets all names
  //PROTECT(a = VECTOR_ELT(AS_LIST(ATTRIB(x)),1));    // this gets individual values
  
  // CAR gets the first element of the dotted pair list
  // CDR gets the rest of the dotted pair list
  // TAG gets the symbol/name of the first element of dotted pair list
  for( /* a=ATTRIB(a) */; a != R_NilValue; a = CDR(a) ) {
    if(TAG(a) != xts_IndexSymbol &&
       TAG(a) != xts_ClassSymbol &&
       TAG(a) != xts_IndexFormatSymbol &&
       TAG(a) != xts_IndexClassSymbol &&
       TAG(a) != R_ClassSymbol &&
       TAG(a) != R_DimSymbol &&
       TAG(a) != R_DimNamesSymbol &&
       TAG(a) != R_NamesSymbol)
    {
      SET_VECTOR_ELT(values, i, CAR(a));
      SET_STRING_ELT(names,  i, PRINTNAME(TAG(a)));
      i++;
    }
  }
  if(i == 0) {
    UNPROTECT(P);
    return R_NilValue;
  }

  SET_LENGTH(values, i); // truncate list back to i-size
  SET_LENGTH(names,  i);
  setAttrib(values, R_NamesSymbol, names);
  UNPROTECT(P);
  return values;
}

void copy_xtsAttributes(SEXP x, SEXP y)
{
  SEXP attr;
  int P=0;
  //attr = coerceVector(xts_ATTRIB(x), LISTSXP);
  attr = xts_ATTRIB(x);
  //if(length(attr) <= 0 || y == R_NilValue)
  //  return y;

  if(length(attr) > 0 || y != R_NilValue) {
    PROTECT(attr); P++;
    for( ; attr != R_NilValue; attr = CDR(attr) ) {
      setAttrib(y, TAG(attr), CAR(attr));
    }
    UNPROTECT(P);
  }
}

SEXP ca (SEXP x, SEXP y)
{
  /* an example of internal copying of user-defined xts attributes
     this will be used inside of do_xts_subset and do_xts_merge  
     This particular usage is bad, as y is modified without copying
     resulting in all y and reference to y within R envir being altered
     the 'y' should only be the new value that is to be returned  */
  copy_xtsAttributes(x,y);
  return R_NilValue;
}
