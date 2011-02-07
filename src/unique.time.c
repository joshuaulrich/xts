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

SEXP non_duplicates (SEXP x_, SEXP fromLast_) {
  int fromLast = asLogical(fromLast_),
      i, d=0,
      len   = length(x_);
  
  int *x_int;
  double *x_real;

  SEXP duplicates;
  int *duplicates_int;
  PROTECT(duplicates = allocVector(INTSXP, len)); /* possibly resize this */
  duplicates_int = INTEGER(duplicates);

  if(!fromLast) { /* keep first observation */
    duplicates_int[0] = ++d;
    switch(TYPEOF(x_)) {
      case INTSXP:
        x_int = INTEGER(x_);
        for(i=1; i < len-1; i++) {
          if( x_int[i-1] != x_int[i]) {
Rprintf("i=%i:  x[i-1]=%i, x[i]=%i\n",i,x_int[i-1],x_int[i]);
            duplicates_int[d++] = i+1;
          }
        }      
        break;
      case REALSXP:
        x_real = REAL(x_);
        for(i=1; i < len; i++) {
          /*
          if( x_real[i-1] == x_real[i])
            duplicates_int[d++] = (int)(-1*(i+1));
          */
          if( x_real[i-1] != x_real[i])
            duplicates_int[d++] = i+1;
        }      
        break;
      default:
        error("only numeric types supported");
        break;
    }
  } else {    /* keep last observation  */
    switch(TYPEOF(x_)) {
      case INTSXP:
        x_int = INTEGER(x_);
        for(i=1; i < len; i++) {
          if( x_int[i-1] != x_int[i])
            duplicates_int[d++] = i;
        }      
        break;
      case REALSXP:
        x_real = REAL(x_);
        for(i=1; i < len; i++) {
          if( x_real[i-1] != x_real[i])
            duplicates_int[d++] = i;
        }      
        break;
      default:
        error("only numeric types supported");
        break;
    }
    duplicates_int[d++] = len;
  }
  UNPROTECT(1);
  return(lengthgets(duplicates, d));
}
