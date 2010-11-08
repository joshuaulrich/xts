#include <R.h>
#include <Rinternals.h>

/*

  provide fast memcpy extraction by column
  major orientation for matrix objects. Should
  be as fast as extracting list elements or
  data.frame columns in R.

  One key difference is that we are also reattaching
  the index attribute to allow for the object to
  remain a zoo/xts object

*/

SEXP extract_col (SEXP x, SEXP j, SEXP drop) {
  SEXP result;
  int nrs, ncs, i, jj;

  ncs = ncols(x);
  PROTECT(result = allocVector(TYPEOF(x), nrows(x) * length(j)));

  switch(TYPEOF(x)) {
    case REALSXP:
      for(i=0; i<length(j); i++) {
        memcpy(&(REAL(result)[i*nrows(x)]), 
               &(REAL(x)[(INTEGER(j)[i]-1)*nrows(x)]), 
               nrows(x)*sizeof(double));
      }
      break;
    case INTSXP:
      for(i=0; i<length(j); i++) {
        memcpy(&(INTEGER(result)[i*nrows(x)]), 
               &(INTEGER(x)[(INTEGER(j)[i]-1)*nrows(x)]), 
               nrows(x)*sizeof(int));
      }
      break;
    case LGLSXP:
      for(i=0; i<length(j); i++) {
        memcpy(&(LOGICAL(result)[i*nrows(x)]), 
               &(LOGICAL(x)[(INTEGER(j)[i]-1)*nrows(x)]), 
               nrows(x)*sizeof(int));
      }
      break;
    case CPLXSXP:
      for(i=0; i<length(j); i++) {
        memcpy(&(COMPLEX(result)[i*nrows(x)]), 
               &(COMPLEX(x)[(INTEGER(j)[i]-1)*nrows(x)]), 
               nrows(x)*sizeof(Rcomplex));
      }
      break;
    case RAWSXP:
      for(i=0; i<length(j); i++) {
        memcpy(&(RAW(result)[i*nrows(x)]), 
               &(RAW(x)[(INTEGER(j)[i]-1)*nrows(x)]), 
               nrows(x)*sizeof(Rbyte));
      }
      break;
    case STRSXP:
      nrs = nrows(x);
      for(jj=0; jj<length(j); jj++)
      for(i=0; i< nrs; i++)
        SET_STRING_ELT(result, i+jj*nrs, STRING_ELT(x, i+(INTEGER(j)[jj]-1)*nrs));
      break;
    default:
      error("unsupported type");
  }

  copyMostAttrib(x, result);

  if(!asLogical(drop)) { /* keep dimension and dimnames */
    SEXP dim;
    PROTECT(dim = allocVector(INTSXP, 2));
    INTEGER(dim)[0] = nrows(x);
    INTEGER(dim)[1] = length(j);
    setAttrib(result, R_DimSymbol, dim);
    UNPROTECT(1);

    SEXP dimnames, currentnames, newnames;
    PROTECT(dimnames = allocVector(VECSXP, 2));
    PROTECT(newnames = allocVector(STRSXP, length(j)));
    currentnames = getAttrib(x, R_DimNamesSymbol);

    if(!isNull(currentnames)) {
      SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(currentnames,0));
      if(!isNull(VECTOR_ELT(currentnames,1))) {
        /* if colnames isn't NULL set */
        for(i=0; i<length(j); i++) {
          SET_STRING_ELT(newnames, i, STRING_ELT(VECTOR_ELT(currentnames,1), INTEGER(j)[i]-1));
        }
        SET_VECTOR_ELT(dimnames, 1, newnames);
      } else {
        /* else set to NULL */
        SET_VECTOR_ELT(dimnames, 1, R_NilValue);
      }
      setAttrib(result, R_DimNamesSymbol, dimnames);
    }
    UNPROTECT(2);
  }

  UNPROTECT(1);
  return result;
}
