#include <R.h>
#include <Rinternals.h>

SEXP coredata (SEXP x)
{
  SEXP result;
  int i, j, ncs, nrs;
  PROTECT(result = allocVector(TYPEOF(x), length(x)));
  switch(TYPEOF(x)) {
    case REALSXP:
      memcpy(REAL(result), REAL(x), length(result) * sizeof(double));
      break;
    case INTSXP:
      memcpy(INTEGER(result), INTEGER(x), length(result) * sizeof(int));
      break;
    case LGLSXP:
      memcpy(LOGICAL(result), LOGICAL(x), length(result) * sizeof(int));
      break;
    case CPLXSXP:
      memcpy(COMPLEX(result), COMPLEX(x), length(result) * sizeof(Rcomplex));
      break;
    case STRSXP:
      ncs = ncols(x); nrs = nrows(x);
      for(j=0; j< ncs; j++)
      for(i=0; i< nrs; i++)
        //Rprintf("i: %i, j: %i, i+j*nrs: %i\n", i, j, i+j*nrs);
        SET_STRING_ELT(result, i+j*nrs, STRING_ELT(x, i+j*nrs));
      break;
    default:
      error("currently unsupported data type");
      break;
  }
  SEXP dim;
  PROTECT(dim = allocVector(INTSXP, 2));
  INTEGER(dim)[0] = nrows(x);
  INTEGER(dim)[1] = ncols(x);
  setAttrib(result, R_DimSymbol, dim);
  UNPROTECT(2);
  return result;
}
