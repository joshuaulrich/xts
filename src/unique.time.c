#include <R.h>
#include <R.h>
#include "xts.h" /* for coredata_xts */

SEXP make_unique (SEXP index, SEXP eps_) {
  SEXP newindex;
  int P=0, len, i;
  len=length(index);

  double eps = asReal(eps_);
  double *index_real,
         *newindex_real;

  if( TYPEOF(index) == INTSXP) {
    PROTECT(index = coerceVector(index,REALSXP)); P++;
  }
  
  PROTECT(newindex = allocVector(REALSXP, length(index))); P++;
  copyAttributes(index, newindex);
  index_real = REAL(index);
  newindex_real = REAL(newindex);

  newindex_real[0] = index_real[0]; 
  for(i=1; i<len; i++) {
    if(index_real[i-1] == index_real[i])
      newindex_real[i] = newindex_real[i-1] + eps;
    else
      newindex_real[i] = index_real[i];
  }

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
