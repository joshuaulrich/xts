#include <R.h>
#include <R.h>
#include "xts.h" /* for coredata_xts */

SEXP make_unique (SEXP index, SEXP eps_) {
  SEXP newindex;
  int P=0, len, i;
  double eps = asReal(eps_);
  len=length(index);
  
/*  int  *index_int, *newindex_int; */
  double  *index_real, *newindex_real;

  if( TYPEOF(index) == INTSXP) {
    PROTECT(index = coerceVector(index,REALSXP)); P++;
  }
  
  PROTECT(newindex = allocVector(REALSXP, length(index))); P++;
  copyAttributes(index, newindex);

/*
  switch( TYPEOF(index)) {
    case INTSXP:
      index_int = INTEGER(index);
      newindex_int = INTEGER(newindex);
      newindex_int[0] = index_int[0];
      for(i=1; i<len; i++) {
        if(index_int[i-1] == index_int[i])
          newindex_int[i] = newindex_int[i-1] + eps;
        else
          newindex_int[i] = index_int[i];
      }
      break;
    case REALSXP:
*/
      index_real = REAL(index);
      newindex_real = REAL(newindex);
      newindex_real[0] = index_real[0]; 
      for(i=1; i<len; i++) {
        if(index_real[i-1] == index_real[i])
          newindex_real[i] = newindex_real[i-1] + eps;
        else
          newindex_real[i] = index_real[i];
      }
/*
      break;
  }
*/
  UNPROTECT(P);
  return(newindex);

}

SEXP make_index_unique (SEXP x_, SEXP eps_) {
  SEXP result;
  PROTECT(result = coredata_xts(x_));
  copyAttributes(x_, result); /* copy all attrib except index, and dim-related */
  setAttrib(result, install("index"), make_unique(getAttrib(x_, install("index")), eps_));
  UNPROTECT(1);
  return(result);
}

SEXP make_time_unique ( SEXP x_, SEXP eps_) {
  SEXP result, index, newindex;
  int P=0, nrs, i;
  double eps = asReal(eps_);
  nrs=nrows(x_);
  
  int *result_int, *index_int, *newindex_int;
  double *result_real, *index_real, *newindex_real;
  
  PROTECT(result = coredata_xts(x_)); P++;

  copyAttributes(x_, result); /* copy all attrib except index, and dim-related */

  index = getAttrib(x_, install("index"));
  PROTECT(newindex = allocVector(TYPEOF(index), length(index))); P++;
  copyAttributes(index, newindex);

  switch( TYPEOF(index)) {
    case INTSXP:
      /* this is probably not needed - maybe we force coercion to double first */
      index_int = INTEGER(index);
      newindex_int = INTEGER(newindex);
      newindex_int[0] = index_int[0];
      for(i=1; i<nrs; i++) {
        if(index_int[i-1] == index_int[i])
          newindex_int[i] = newindex_int[i-1] + eps;
        else
          newindex_int[i] = index_int[i];
      }
      break;
    case REALSXP:
      index_real = REAL(index);
      newindex_real = REAL(newindex);
      newindex_real[0] = index_real[0]; 
      for(i=1; i<nrs; i++) {
        if(index_real[i-1] == index_real[i])
          newindex_real[i] = newindex_real[i-1] + eps;
        else
          newindex_real[i] = index_real[i];
      }
      break;
  }
  setAttrib(result, install("index"), newindex);
  UNPROTECT(2);
  return(result);
}
