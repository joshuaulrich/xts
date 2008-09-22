#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

int isXts(SEXP x) 
{
  int i;
  SEXP attr;

  PROTECT( attr = coerceVector(getAttrib(x, R_ClassSymbol),STRSXP) );
  if(length(attr) <= 1) {
    UNPROTECT(1);
    return 0;
  }

  for(i = 0; i < length(attr); i++) {
    if(STRING_ELT(attr, i) == mkChar("xts")) {
      UNPROTECT(1);
      return 1;
    }
  }
  UNPROTECT(1);
  return 0;

}

/* test function and example */
SEXP test_isXts(SEXP x)
{
  if(isXts(x)) {
    Rprintf("TRUE\n");
  } else {
    Rprintf("FALSE\n");
  }
  return R_NilValue;
}
