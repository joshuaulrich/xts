#include <R.h>
#include <Rinternals.h>
#include "xts.h"

/*

  provide fast memcpy extraction by column
  major orientation for matrix objects. Should
  be as fast as extracting list elements or
  data.frame columns in R. ( update: it is much faster actually -jar )

  One key difference is that we are also reattaching
  the index attribute to allow for the object to
  remain a zoo/xts object

*/

SEXP extract_col (SEXP x, SEXP j, SEXP drop, SEXP first_, SEXP last_) {
  SEXP result, index, new_index;
  int nrs, nrsx, i, jj, first, last;

  nrsx = nrows(x);

  first = asInteger(first_)-1;
  last = asInteger(last_)-1;

  /* nrs = offset_end - offset_start - 1; */
  nrs = last - first + 1;
  

  PROTECT(result = allocVector(TYPEOF(x), nrs * length(j)));

  switch(TYPEOF(x)) {
    case REALSXP:
      for(i=0; i<length(j); i++) {
/*
Rprintf("j + i*nrs + first=%i\n", (int)(INTEGER(j)[i]-1 + i*nrs + first));
Rprintf("i=%i, j=%i, nrs=%i, first=%i\n", i, INTEGER(j)[i]-1, nrs, first);
*/
        memcpy(&(REAL(result)[i*nrs]), 
               &(REAL(x)[(INTEGER(j)[i]-1)*nrsx + first]), 
               nrs*sizeof(double));
      }
      break;
    case INTSXP:
      for(i=0; i<length(j); i++) {
        memcpy(&(INTEGER(result)[i*nrs]), 
               &(INTEGER(x)[(INTEGER(j)[i]-1)*nrsx + first]), 
               nrs*sizeof(int));
      }
      break;
    case LGLSXP:
      for(i=0; i<length(j); i++) {
        memcpy(&(LOGICAL(result)[i*nrs]), 
               &(LOGICAL(x)[(INTEGER(j)[i]-1)*nrsx + first]), 
               nrs*sizeof(int));
      }
      break;
    case CPLXSXP:
      for(i=0; i<length(j); i++) {
        memcpy(&(COMPLEX(result)[i*nrs]), 
               &(COMPLEX(x)[(INTEGER(j)[i]-1)*nrsx + first]), 
               nrs*sizeof(Rcomplex));
      }
      break;
    case RAWSXP:
      for(i=0; i<length(j); i++) {
        memcpy(&(RAW(result)[i*nrs]), 
               &(RAW(x)[(INTEGER(j)[i]-1)*nrsx + first]), 
               nrs*sizeof(Rbyte));
      }
      break;
    case STRSXP:
      for(jj=0; jj<length(j); jj++)
      for(i=0; i< nrs; i++)
        SET_STRING_ELT(result, i+jj*nrs, STRING_ELT(x, i+(INTEGER(j)[jj]-1)*nrsx+first));
      break;
    default:
      error("unsupported type");
  }

  if(nrs != nrows(x)) {
    copyAttributes(x, result);
    /* subset index */
    index = getAttrib(x, install("index"));
    PROTECT(new_index = allocVector(TYPEOF(index), nrs)); 
    if(TYPEOF(index) == REALSXP) {
      memcpy(REAL(new_index), &(REAL(index)[first]), nrs*sizeof(double)); 
    } else { /* INTSXP */
      memcpy(INTEGER(new_index), &(INTEGER(index)[first]), nrs*sizeof(int)); 
    }
    copyMostAttrib(index, new_index);
    setAttrib(result, install("index"), new_index);
    UNPROTECT(1);
  } else {
    copyMostAttrib(x, result); /* need an xts/zoo equal that skips 'index' */
  }

  if(!asLogical(drop)) { /* keep dimension and dimnames */
    SEXP dim;
    PROTECT(dim = allocVector(INTSXP, 2));
    INTEGER(dim)[0] = nrs;
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
