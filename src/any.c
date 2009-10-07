#include <R.h>
#include <Rinternals.h>

SEXP any_negative (SEXP i_)
{
  int i;
  int len = length(i_);
  SEXP any;

  int *int_i=NULL;
  double *real_i=NULL;

  if(TYPEOF(i_)==INTSXP) {
    int_i = INTEGER(i_);
    for(i=0; i<len; i++) {
      if(int_i[i] >= 0)   
        continue;
      return ScalarLogical(1);
    }
  } else
  if(TYPEOF(i_)==REALSXP) {
    real_i = REAL(i_);
    for(i=0; i<len; i++) {
      if(real_i[i] >= 0)   
        continue;
      return ScalarLogical(1);
    }
  }
  return ScalarLogical(0);
}
