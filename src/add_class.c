#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP add_class (SEXP x) //, SEXP class)
{
  SEXP class;
  PROTECT(class = allocVector(STRSXP, 2));
  SET_STRING_ELT(class, 0, mkChar("xts")); //STRING_ELT(class, 0));
  SET_STRING_ELT(class, 1, mkChar("zoo")); //STRING_ELT(class, 1));

  setAttrib(x, install("class"), class);
  UNPROTECT(1);

  return x;
}
