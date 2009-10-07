#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

SEXP binsearch (SEXP key, SEXP vec, SEXP start)
{ 
  int found = 0;
  int rec = -1;
  int mid;
  int lo=1, hi=length(vec);
  if(isNull(vec)) {
    if(isNull(start)) {
      return ScalarInteger(NA_INTEGER);
    } else {
      return ScalarInteger(LOGICAL(start)[0]);
    }
  }

  
  if(TYPEOF(vec) == INTSXP) {
  PROTECT(key = coerceVector(key, INTSXP));
  while(hi >= lo) {
    mid = (int)((lo+hi)/2);
    if(mid == 0)
      return ScalarInteger(NA_INTEGER);
    //if(mid != 0 && INTEGER(key)[0] < INTEGER(vec)[mid]) {
    if(mid != 0 && INTEGER(key)[0] < INTEGER(vec)[mid-1]) {
      hi = mid-1;
    } 
    //else if(mid != 0 && INTEGER(key)[0] > INTEGER(vec)[mid]) {
    else if(mid != 0 && INTEGER(key)[0] > INTEGER(vec)[mid-1]) {
      lo = mid+1;
    }
    else {
      found = 1;
      if(!isNull(start)) {
        if(!LOGICAL(start)[0]) {
          while(1) {
            //if(mid == length(vec) || INTEGER(vec)[mid+1] != INTEGER(key)[0])
            if(mid == length(vec) || INTEGER(vec)[mid] != INTEGER(key)[0])
              break;
            mid++;
          }
        } else {
          while(1) {
            //if(mid == 1 || INTEGER(vec)[mid-1] != INTEGER(key)[0])
            if(mid == 1 || INTEGER(vec)[mid-2] != INTEGER(key)[0])
              break;
            mid--;
          }
        }
      }
      rec = mid;
      break;
    }
  }
  if(isNull(start) && rec == -1) {
    UNPROTECT(1);
    return ScalarInteger(NA_INTEGER);
  }
  if(rec == -1) {
    if(LOGICAL(start)[0]) {
      UNPROTECT(1);
      return ScalarInteger(lo);
    } else {
      UNPROTECT(1);
      return ScalarInteger(hi);
    }
  } else { 
      UNPROTECT(1);
      return ScalarInteger(rec);
  }

  } else 
  if(TYPEOF(vec) == REALSXP) {
  PROTECT(key = coerceVector(key,REALSXP));
  while(hi >= lo) {
    mid = (int)((lo+hi)/2);
    if(mid == 0)
      return ScalarInteger(NA_INTEGER);
    //if(mid != 0 && REAL(key)[0] < REAL(vec)[mid]) {
    if(mid != 0 && REAL(key)[0] < REAL(vec)[mid-1]) {
      hi = mid-1;
    } 
    //else if(mid != 0 && REAL(key)[0] > REAL(vec)[mid]) {
    else if(mid != 0 && REAL(key)[0] > REAL(vec)[mid-1]) {
      lo = mid+1;
    }
    else {
      found = 1;
      if(!isNull(start)) {
        if(!LOGICAL(start)[0]) {
          while(1) {
            //if(mid == length(vec) || REAL(vec)[mid+1] != REAL(key)[0])
            if(mid == length(vec) || REAL(vec)[mid] != REAL(key)[0])
              break;
            mid++;
          }
        } else {
          while(1) {
            //if(mid == 1 || REAL(vec)[mid-1] != REAL(key)[0])
            if(mid == 1 || REAL(vec)[mid-2] != REAL(key)[0])
              break;
            mid--;
          }
        }
      }
      rec = mid;
      break;
    }
  }
  if(isNull(start) && rec == -1) {
    UNPROTECT(1);
    return ScalarInteger(NA_INTEGER);
  }
  if(rec == -1) {
    if(LOGICAL(start)[0]) {
      UNPROTECT(1);
      return ScalarInteger(lo);
    } else {
      UNPROTECT(1);
      return ScalarInteger(hi);
    }
  } else { 
   UNPROTECT(1); return ScalarInteger(rec);
  }
  }
}

