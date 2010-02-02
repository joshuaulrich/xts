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
  int i, P=0;

  /* ensure 'x' is double */
  if(TYPEOF(x) != REALSXP) {
    PROTECT(x = coerceVector(x, REALSXP)); P++;
  }
  double *real_x=REAL(x);

  /* ensure 'n' is integer */
  if(TYPEOF(n) != INTSXP) {
    PROTECT(n = coerceVector(n, INTSXP)); P++;
  }
  int int_n = INTEGER(n)[0];

  /* Initalize result R object */
  SEXP result;
  PROTECT(result = allocVector(TYPEOF(x), length(x))); P++;
  double *real_result = REAL(result);

  /* check for non-leading NAs and get first non-NA location */
  SEXP first;
  PROTECT(first = naCheck(x, ScalarLogical(TRUE))); P++;
  int int_first = INTEGER(first)[0];

  /* set leading NAs, find initial sum value */
  double real_sum = 0.0;
  for(i=0; i<int_n+int_first; i++) {
    real_result[i] = NA_REAL;
    if(i >= int_first) {
      real_sum += real_x[i];
    }
  }
  /* set initial sum */
  real_result[int_n+int_first-1] = real_sum;

  /* loop over all other values */
  int nrs = nrows(x);
  for(i=int_n+int_first; i<nrs; i++) {
    real_result[i] = real_result[i-1] + real_x[i] - real_x[i-int_n];
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

  /* ensure 'n' is integer */
  if(TYPEOF(n) != INTSXP) {
    PROTECT(n = coerceVector(n, INTSXP)); P++;
  }
  int int_n = INTEGER(n)[0];

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
  int int_first = INTEGER(first)[0];

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

  /* ensure 'n' is integer */
  if(TYPEOF(n) != INTSXP) {
    PROTECT(n = coerceVector(n, INTSXP)); P++;
  }
  int int_n = INTEGER(n)[0];

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
  int int_first = INTEGER(first)[0];

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

/*
SEXP do_runsum (SEXP x, SEXP n, SEXP result)
{

}

SEXP do_run (SEXP x, SEXP n, (*void)FUN)
{
  SEXP result;
  int P=0;
  int i, nrs;
  int *int_n=NULL;
  if(TYPEOF(n) != INTSXP) {
    // assure that 'n' is an integer
    PROTECT(n = coerceVector(n, INTSXP)); P++;
  }
  int_n = INTEGER(n); // get the first element (everything from R is a vector)

  int *int_result=NULL, *int_x=NULL;
  int int_sum = 0;
  double *real_result=NULL, *real_x=NULL;
  double real_sum = 0.0;

  PROTECT(result = allocVector(TYPEOF(x), length(x))); P++;

  int _firstNonNA = firstNonNA(x);
*/  /* The branch by type allows for fewer type checks/branching
   * within the algorithm, providing a _much_ faster mechanism
   */
/*  switch(TYPEOF(x)) { */
    /* need to implement other types?, and checking
     *
     * This part of the code could be a call to a function pointer passed
     * in to the top-level call... maybe???  This would make it easier to
     * extend the framework and, _more_ importantly, allow for user-level
     * C functions (via R) to be used ad hoc
     */
/*
}
*/

