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


#include<R.h>
#include<Rdefines.h>
#include<Rinternals.h>

SEXP do_is_ordered (SEXP x, SEXP increasing, SEXP strictly)
{
  int i, P=0;
  int nx = LENGTH(x) - 1;
  SEXP res;
  double *real_x;
  int *int_x;

  PROTECT( res = allocVector(LGLSXP, 1) ); P++;
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
  UNPROTECT(P);
  return res;
}
