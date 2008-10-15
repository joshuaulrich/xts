#include<R.h>
#include<Rdefines.h>
#include<Rinternals.h>

SEXP do_is_ordered (SEXP x, SEXP increasing, SEXP strictly)
{
  int i;
  int nx = LENGTH(x) - 1;
  SEXP res;
  double *real_x;
  int *int_x;

  PROTECT( res = allocVector(LGLSXP, 1) );
  LOGICAL(res)[ 0 ] = 1; // default to true


  if(TYPEOF(x) == REALSXP) {
  /*
  Check for increasing order, strict or non-strict
  */
  real_x = REAL(x);
  if(LOGICAL(increasing)[ 0 ] == 1) { // INCREASING
    if(LOGICAL(strictly)[ 0 ] == 1) { // STRICTLY INCREASING ( > 0 )
      for(i = 0; i < nx; i++) {
        //if( (REAL(x)[ i+1 ] - REAL(x)[ i ]) <= 0.0 ) {
        if( real_x[i+1] - real_x[i] <= 0.0 ) {
          LOGICAL(res)[ 0 ] = 0;
          break;
        } 
      }
    } else { // NOT-STRICTLY ( 0 || > 0 )
      for(i = 0; i < nx; i++) {
        //if( (REAL(x)[ i+1 ] - REAL(x)[ i ]) < 0.0 ) {
        if( real_x[i+1] - real_x[i] < 0.0 ) {
          LOGICAL(res)[ 0 ] = 0;
          break;
        } 
      }
    }
 
  /*
  Check for decreasing order, strict or non-strict
  */
  } else { // DECREASING
    if(LOGICAL(strictly)[ 0 ] == 1) { // STRICTLY DECREASING ( < 0 )
      for(i = 0; i < nx; i++) {
        //if( (REAL(x)[ i+1 ] - REAL(x)[ i ]) >= 0.0 ) {
        if( real_x[i+1] - real_x[i] >= 0.0 ) {
          LOGICAL(res)[ 0 ] = 0;
          break;
        } 
      }
    } else { // NOT-STRICTLY ( 0 || < 0 )
      for(i = 0; i < nx; i++) {
        //if( (REAL(x)[ i+1 ] - REAL(x)[ i ]) > 0.0 ) {
        if( real_x[i+1] - real_x[i] > 0.0 ) {
          LOGICAL(res)[ 0 ] = 0;
          break;
        } 
      }
    }
  }
  
  } else
  if(TYPEOF(x) == INTSXP) {
  /*
  Check for increasing order, strict or non-strict
  */
  int_x = INTEGER(x);
  if(LOGICAL(increasing)[ 0 ] == 1) { // INCREASING
    if(LOGICAL(strictly)[ 0 ] == 1) { // STRICTLY INCREASING ( > 0 )
      for(i = 0; i < nx; i++) {
        //if( (INTEGER(x)[ i+1 ] - INTEGER(x)[ i ]) <= 0.0 ) {
        if( int_x[i+1] - int_x[i] <= 0 ) {
          LOGICAL(res)[ 0 ] = 0;
          break;
        } 
      }
    } else { // NOT-STRICTLY ( 0 || > 0 )
      for(i = 0; i < nx; i++) {
        //if( (INTEGER(x)[ i+1 ] - INTEGER(x)[ i ]) < 0.0 ) {
        if( int_x[i+1] - int_x[i] < 0 ) {
          LOGICAL(res)[ 0 ] = 0;
          break;
        } 
      }
    }
 
  /*
  Check for decreasing order, strict or non-strict
  */
  } else { // DECREASING
    if(LOGICAL(strictly)[ 0 ] == 1) { // STRICTLY DECREASING ( < 0 )
      for(i = 0; i < nx; i++) {
        //if( (INTEGER(x)[ i+1 ] - INTEGER(x)[ i ]) >= 0.0 ) {
        if( int_x[i+1] - int_x[i] >= 0 ) {
          LOGICAL(res)[ 0 ] = 0;
          break;
        } 
      }
    } else { // NOT-STRICTLY ( 0 || < 0 )
      for(i = 0; i < nx; i++) {
        //if( (INTEGER(x)[ i+1 ] - INTEGER(x)[ i ]) > 0.0 ) {
        if( int_x[i+1] - int_x[i] > 0 ) {
          LOGICAL(res)[ 0 ] = 0;
          break;
        } 
      }
    }
  }

  } else {
    error("'x' must be of type double or integer");
  }
  UNPROTECT(1);
  return res;
}
