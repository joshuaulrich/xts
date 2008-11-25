#include <R.h>
#include <Rinternals.h>
#include "xts.h"

SEXP tryXts (SEXP x)
{
  if( !isXts(x) ) {
    SEXP s, t, result;
    PROTECT(s = t = allocList(2));
    SET_TYPEOF(s, LANGSXP);
    SETCAR(t, install("try.xts")); t = CDR(t);
    SETCAR(t, x); t=CDR(t);
    PROTECT(result = eval(s, R_GlobalEnv));
    if( !isXts(result) ) {
      UNPROTECT(2);
      error("rbind.xts requires xtsible data");
    }
    UNPROTECT(2);
    return result;
  }
  return x;
}

/*
SEXP try_xts (SEXP x)
{
  SEXP y;
  PROTECT(y = tryXts(x));
  UNPROTECT(1);
  return y;
}
*/
