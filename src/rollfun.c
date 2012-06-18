/*
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <R.h>
#include <Rinternals.h>
#include "xts.h"

SEXP roll_sum (SEXP x, SEXP n)
{
  /* Author: Joshua Ulrich */
  int i, P=0, nrs;
  nrs = nrows(x);

  /* Get values from pointers */
  int int_n = asInteger(n);

  /* Initalize result R object */
  SEXP result;
  PROTECT(result = allocVector(TYPEOF(x), length(x))); P++;
  int *int_result=NULL, *int_x=NULL;
  int int_sum = 0;
  double *real_result=NULL, *real_x=NULL;
  double real_sum = 0.0;

  /* check for non-leading NAs and get first non-NA location */
  SEXP first;
  PROTECT(first = naCheck(x, ScalarLogical(TRUE))); P++;
  int int_first = asInteger(first);
  if(int_n + int_first > nrs)
    error("not enough non-NA values");

  switch(TYPEOF(x)) {
    case REALSXP:
      real_result = REAL(result);
      real_x = REAL(x);
      //int_result = int_x = NULL;
      /* set leading NAs, find initial sum value */
      for(i=0; i<int_n+int_first; i++) {
        real_result[i] = NA_REAL;
        if(i >= int_first)
          real_sum += real_x[i];
      }
      real_result[ int_n + int_first - 1 ] = real_sum;
      /* loop over all other values */
      for(i=int_n+int_first; i<nrs; i++) {
        real_result[i] = real_result[i-1] + real_x[i] - real_x[i-int_n];
      }
      break;
    case INTSXP: /* how can we check for overflow? */
      int_result = INTEGER(result);
      int_x = INTEGER(x);
      //real_result = real_x = NULL;
      /* set leading NAs, find initial sum value */
      for(i=0; i<int_n+int_first; i++) {
        int_result[i] = NA_INTEGER;
        if(i >= int_first)
          int_sum += int_x[i];
      }
      int_result[ int_n + int_first -1 ] = int_sum;
      /* loop over all other values */
      for(i=int_n+int_first; i<nrs; i++) {
        int_result[i] = int_result[i-1] + int_x[i] - int_x[i-int_n];
      }
      break;
    /*
    case STRSXP:  fail!
    case LGLSXP:  convert to int, like sum, cumsum, etc?
    case CPLXSXP:
    */
    default:
      error("unsupported data type");
  }

  copyMostAttrib(x, result);
  /* still need to set dims and dimnames */
  setAttrib(result, R_DimSymbol, getAttrib(x, R_DimSymbol));
  setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));

  UNPROTECT(P);
  return result;
}

SEXP roll_min (SEXP x, SEXP n)
{
  /* Author: Joshua Ulrich */
  int i, j, P=0;

  /* Get values from pointers */
  int int_n = asInteger(n);

  /* initialize pointers and values */
  int *int_result=NULL, *int_x=NULL;
  int int_min;
  double *real_result=NULL, *real_x=NULL;
  double real_min;
  int nrs = nrows(x);
  int loc;

  /* Initalize result R object */
  SEXP result;
  PROTECT(result = allocVector(TYPEOF(x), length(x))); P++;

  /* check for non-leading NAs and get first non-NA location */
  SEXP first;
  PROTECT(first = naCheck(x, ScalarLogical(TRUE))); P++;
  int int_first = asInteger(first);
  if(int_n + int_first > nrs)
    error("not enough non-NA values");

  /* The branch by type allows for fewer type checks/branching
   * within the algorithm, providing a _much_ faster mechanism
   */
  switch(TYPEOF(x)) {
    /* need to implement other types (checking)? */
    case REALSXP:
      real_result = REAL(result);
      real_x = REAL(x);
      real_min = real_x[0];
      loc = 0;

      for(i=0; i<nrs; i++) {
        /* set leading NAs and find initial min value */
        if(i < int_first + int_n - 1) {
          real_result[i] = NA_REAL;
          if(real_x[i] < real_min) {
            real_min = real_x[i];  /* set min value */
            loc = 0;               /* set min location in window */
          }
          loc++;
          continue;
        } else {
          /* if the min leaves the window */
          if(loc >= int_n-1) {
            /* find the min over the entire window */
            real_min = real_x[i];
            for(j=0; j<int_n; j++) {
              if(real_x[i-j] < real_min) {
                real_min = real_x[i-j];
                loc = j;
              }
            }
          } else {
            /* if the new value is the new min */
            if(real_x[i] < real_min) {
              real_min = real_x[i];
              loc = 0;
            }
          }
        }
        /* set result, increment location */
        real_result[i] = real_min;
        loc++;
      }
      break;
    case INTSXP:
      int_result = INTEGER(result);
      int_x = INTEGER(x);
      int_min = int_x[0];
      loc = 0;

      for(i=0; i<nrs; i++) {
        /* set leading NAs and find initial min value */
        if(i < int_first + int_n - 1) {
          int_result[i] = NA_INTEGER;
          if(int_x[i] < int_min) {
            int_min = int_x[i];    /* set min value */
            loc = 0;               /* set min location in window */
          }
          loc++;
          continue;
        } else {
          /* if the min leaves the window */
          if(loc >= int_n - 1) {
            /* find the min over the entire window */
            int_min = int_x[i];
            for(j=0; j<int_n; j++) {
              if(int_x[i-j] < int_min) {
                int_min = int_x[i-j];
                loc = j;
              }
            }
          } else {
            /* if the new value is the new min */
            if(int_x[i] < int_min) {
              int_min = int_x[i];
              loc = 0;
            }
          }
        }
        /* set result, increment location */
        int_result[i] = int_min;
        loc++;
      }
      break;
    /*
    case STRSXP:  fail!
    case LGLSXP:  convert to int??
    case CPLXSXP:
    */
    default:
      error("unsupported data type");
  }

  copyMostAttrib(x, result);
  /* still need to set dims and dimnames */
  setAttrib(result, R_DimSymbol, getAttrib(x, R_DimSymbol));
  setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));

  UNPROTECT(P);
  return result;
}

SEXP roll_max (SEXP x, SEXP n)
{
  /* Author: Joshua Ulrich */
  int i, j, P=0;

  /* Get values from pointers */
  int int_n = asInteger(n);

  /* initialize pointers and values */
  int *int_result=NULL, *int_x=NULL;
  int int_min;
  double *real_result=NULL, *real_x=NULL;
  double real_max;
  int nrs = nrows(x);
  int loc;

  /* Initalize result R object */
  SEXP result;
  PROTECT(result = allocVector(TYPEOF(x), length(x))); P++;

  /* check for non-leading NAs and get first non-NA location */
  SEXP first;
  PROTECT(first = naCheck(x, ScalarLogical(TRUE))); P++;
  int int_first = asInteger(first);
  if(int_n + int_first > nrs)
    error("not enough non-NA values");

  /* The branch by type allows for fewer type checks/branching
   * within the algorithm, providing a _much_ faster mechanism
   */
  switch(TYPEOF(x)) {
    /* need to implement other types (checking)? */
    case REALSXP:
      real_result = REAL(result);
      real_x = REAL(x);
      real_max = real_x[0];
      loc = 0;

      for(i=0; i<nrs; i++) {
        /* set leading NAs and find initial min value */
        if(i < int_first + int_n - 1) {
          real_result[i] = NA_REAL;
          if(real_x[i] > real_max) {
            real_max = real_x[i];  /* set min value */
            loc = 0;               /* set min location in window */
          }
          loc++;
          continue;
        } else {
          /* if the min leaves the window */
          if(loc >= int_n-1) {
            /* find the min over the entire window */
            real_max = real_x[i];
            for(j=0; j<int_n; j++) {
              if(real_x[i-j] > real_max) {
                real_max = real_x[i-j];
                loc = j;
              }
            }
          } else {
            /* if the new value is the new min */
            if(real_x[i] > real_max) {
              real_max = real_x[i];
              loc = 0;
            }
          }
        }
        /* set result, increment location */
        real_result[i] = real_max;
        loc++;
      }
      break;
    case INTSXP:
      int_result = INTEGER(result);
      int_x = INTEGER(x);
      int_min = int_x[0];
      loc = 0;

      for(i=0; i<nrs; i++) {
        /* set leading NAs and find initial min value */
        if(i < int_first + int_n - 1) {
          int_result[i] = NA_INTEGER;
          if(int_x[i] > int_min) {
            int_min = int_x[i];    /* set min value */
            loc = 0;               /* set min location in window */
          }
          loc++;
          continue;
        } else {
          /* if the min leaves the window */
          if(loc >= int_n - 1) {
            /* find the min over the entire window */
            int_min = int_x[i];
            for(j=0; j<int_n; j++) {
              if(int_x[i-j] > int_min) {
                int_min = int_x[i-j];
                loc = j;
              }
            }
          } else {
            /* if the new value is the new min */
            if(int_x[i] > int_min) {
              int_min = int_x[i];
              loc = 0;
            }
          }
        }
        /* set result, increment location */
        int_result[i] = int_min;
        loc++;
      }
      break;
    /*
    case STRSXP:  fail!
    case LGLSXP:  convert to int??
    case CPLXSXP:
    */
    default:
      error("unsupported data type");
  }

  copyMostAttrib(x, result);
  /* still need to set dims and dimnames */
  setAttrib(result, R_DimSymbol, getAttrib(x, R_DimSymbol));
  setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));

  UNPROTECT(P);
  return result;
}

SEXP roll_cov (SEXP x, SEXP y, SEXP n, SEXP samp)
{
  /* Author: Joshua Ulrich */
  int i, P=0;

  /* ensure x and y have same length in R functions, since it's 
   * easier to throw user-informative errors */
  int nrx = nrows(x);
  int nry = nrows(y);
  if(nrx != nry) error("nrx != nry, blame the R function writer");

  /* Coerce to REALSXP to ensure roll_sum returns REALSXP */
  PROTECT(x = coerceVector(x, REALSXP)); P++;
  PROTECT(y = coerceVector(y, REALSXP)); P++;

  /* Get values from function arguments */
  double *real_x = REAL(PROTECT(coerceVector(x, REALSXP))); P++;
  double *real_y = REAL(PROTECT(coerceVector(y, REALSXP))); P++;
  int int_n = asInteger(n);
  int int_samp = asLogical(samp);
  
  /* Initalize result R object */
  SEXP result;
  PROTECT(result = allocVector(REALSXP, nrx)); P++;
  double *real_result = REAL(result);

  /* rolling sums for mean calculation */
  SEXP sum_x, sum_y, xy, sum_xy;
  PROTECT(sum_x = roll_sum(x, n)); P++;
  PROTECT(sum_y = roll_sum(y, n)); P++;
  double *real_sum_x = REAL(sum_x);
  double *real_sum_y = REAL(sum_y);
  
  /* rolling sum of x * y */
  PROTECT(xy = allocVector(REALSXP, nrx)); P++;
  double *real_xy = REAL(xy);
  for(i=nrx; i--;) {
    real_xy[i] = real_x[i] * real_y[i];
  }
  PROTECT(sum_xy = roll_sum(xy, n)); P++;
  double *real_sum_xy = REAL(sum_xy);

  /* check for non-leading NAs and get first non-NA location */
  SEXP first;
  PROTECT(first = naCheck(sum_xy, ScalarLogical(TRUE))); P++;
  int int_first = asInteger(first);
  if(int_n + int_first > nrx)
    error("not enough non-NA values");

  /* set leading NAs */
  for(i=0; i<int_first; i++) {
    real_result[i] = NA_REAL;
  }
  /* calculate cov */
  double mult = int_samp ? (double)int_n/(int_n-1) : 1;
  double int_n2 = int_n*int_n;
  for(i=int_first; i<nrx; i++) {
    real_result[i] = ( real_sum_xy[i] / int_n -
      real_sum_x[i] * real_sum_y[i] / int_n2 ) * mult;
  }

  copyMostAttrib(x, result);
  /* still need to set dims and dimnames */
  setAttrib(result, R_DimSymbol, getAttrib(x, R_DimSymbol));
  setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));

  UNPROTECT(P);
  return result;
}

/*
SEXP roll_median (SEXP x, SEXP n, SEXP center)
{

}

SEXP roll_mad (SEXP x, SEXP n, SEXP center)
{

}
*/

