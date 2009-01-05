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
#include <Rdefines.h>
#include <Rinternals.h>

int firstNonNA (SEXP x)
{
  /*
    check for leading NA values, throw error if found
  */

  int i, nr;
  int *int_x=NULL;
  double *real_x=NULL;

  nr = nrows(x);
  switch(TYPEOF(x)) {
    case INTSXP:
      int_x = INTEGER(x);
      for(i=0; i<nr; i++) {
        if(int_x[i] != NA_INTEGER) {
          break;
        }
      }
      break;
    case REALSXP:
      real_x = REAL(x);
      for(i=0; i<nr; i++) {
        if(real_x[i] != NA_REAL) {
          break;
        }
      }
      break;
    default:
      error("unsupported type");
      break;
  }
  return(i);
}

SEXP naCheck (SEXP x, SEXP check)
{
  SEXP first;
  int _first;
  _first = firstNonNA(x);
  PROTECT(first = allocVector(INTSXP, 1));
  INTEGER(first)[0] = _first;

  if(LOGICAL(check)[0]) {
  /* check for NAs in rest of data */
  int i, nr;
  int *int_x = NULL;
  double *real_x = NULL;

  nr = nrows(x);
  switch(TYPEOF(x)) {
    case INTSXP:
      int_x = INTEGER(x);
      for(i=_first; i<nr; i++) {
        if(int_x[i] == NA_INTEGER) {
          error("Series contains non-leading NAs");  
        }
      }
      break;
    case REALSXP:
      real_x = REAL(x);
      for(i=_first; i<nr; i++) {
        if(real_x[i] == NA_REAL) {
          error("Series contains non-leading NAs");  
        }
      }
      break;
    default:
      error("unsupported type");
      break;
  }
  }
 
  UNPROTECT(1);
  return(first);
}
