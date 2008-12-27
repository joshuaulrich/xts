#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP add_class (SEXP x, SEXP class)
{
  if(NAMED(x) == 2)
    x = duplicate(x);

  setAttrib(x, R_ClassSymbol, class);
  return(x);
}
