/*
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
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


#include<R.h>
#include<Rdefines.h>
#include<Rinternals.h>

SEXP do_is_ordered (SEXP x, SEXP increasing, SEXP strictly)
{
  int i;
  int nx = LENGTH(x) - 1;
  double *real_x;
  int *int_x;

  /*
  If length is 0 then it is ordered
  */
  if (nx < 0)
    return ScalarLogical(1);

  if(TYPEOF(x) == REALSXP) {
  /*
  Check for increasing order, strict or non-strict
  */
  real_x = REAL(x);
  if(LOGICAL(increasing)[ 0 ] == 1) { /* INCREASING */
    if(LOGICAL(strictly)[ 0 ] == 1) { /* STRICTLY INCREASING ( > 0 ) */
      for(i = 0; i < nx; i++) {
        if( real_x[i+1] <= real_x[i] ) {
          return ScalarLogical(0);
        } 
      }
    } else { /* NOT-STRICTLY ( 0 || > 0 ) */
      for(i = 0; i < nx; i++) {
        if( real_x[i+1] < real_x[i] ) {
          return ScalarLogical(0);
        } 
      }
    }
  /*
  Check for decreasing order, strict or non-strict
  */
  } else { 
    if(LOGICAL(strictly)[ 0 ] == 1) { /* STRICTLY DECREASING ( < 0 ) */
      for(i = 0; i < nx; i++) {
        if( real_x[i+1] >= real_x[i] ) {
          return ScalarLogical(0);
        } 
      }
    } else { /* NOT-STRICTLY ( 0 || < 0 ) */
      for(i = 0; i < nx; i++) {
        if( real_x[i+1] > real_x[i] ) {
          return ScalarLogical(0);
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
  if(LOGICAL(increasing)[ 0 ] == 1) { /* INCREASING */
    /* Not increasing order if first element is NA.  We know x has at least 1 element. */
    if( int_x[0] == NA_INTEGER )
      return ScalarLogical(0);

    if(LOGICAL(strictly)[ 0 ] == 1) { /* STRICTLY INCREASING ( > 0 ) */
      for(i = 0; i < nx; i++) {
        if( int_x[i+1] <= int_x[i] ) {
          if (i == (nx-1) && int_x[i+1] == NA_INTEGER) {
            continue; /* OK if NA is last element */
          }
          return ScalarLogical(0);
        } 
      }
    } else { /* NOT-STRICTLY ( 0 || > 0 ) */
      for(i = 0; i < nx; i++) {
        if( int_x[i+1] < int_x[i] ) {
          if (i == (nx-1) && int_x[i+1] == NA_INTEGER) {
            continue; /* OK if NA is last element */
          }
          return ScalarLogical(0);
        } 
      }
    }
  /*
  Check for decreasing order, strict or non-strict
  */
  } else { /* DECREASING */
    /* Not decreasing order if last element is NA */
    if( int_x[nx] == NA_INTEGER )
      return ScalarLogical(0);

    if(LOGICAL(strictly)[ 0 ] == 1) { /* STRICTLY DECREASING ( < 0 ) */
      for(i = 0; i < nx; i++) {
        if( int_x[i+1] >= int_x[i] ) {
          if (i == 0 && int_x[i] == NA_INTEGER) {
            continue; /* OK if NA is first element */
          }
          return ScalarLogical(0);
        } 
      }
    } else { /* NOT-STRICTLY ( 0 || < 0 ) */
      for(i = 0; i < nx; i++) {
        if( int_x[i+1] > int_x[i] ) {
          if (i == 0 && int_x[i] == NA_INTEGER) {
            continue; /* OK if NA is first element */
          }
          return ScalarLogical(0);
        } 
      }
    }
  }

  } else {
    error("'x' must be of type double or integer");
  }

  return ScalarLogical(1);  /* default to true */
}
