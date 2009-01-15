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
#include "xts.h"

int firstNonNA (SEXP x)
{
  /*
    Internal use only; called by naCheck below.
  */

  int i=0, nr;
  int *int_x=NULL;
  double *real_x=NULL;

  nr = nrows(x);

  switch(TYPEOF(x)) {
    case INTSXP:
      int_x = INTEGER(x);
      for(i=0; i<nr; i++) {
        if(int_x[i]!=NA_INTEGER) {
          break;
        }
      }
      break;
    case REALSXP:
      real_x = REAL(x);
      for(i=0; i<nr; i++) {
        if(!ISNA(real_x[i])) {
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
  /*
    Check for leading NA values, throw error if found
  */
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
        if(ISNA(real_x[i])) {
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

SEXP na_locf (SEXP x)
{
  /* only works on univariate data */
  SEXP result;

  int i, nr, _first, P=0;
  _first = firstNonNA(x);

  int *int_x=NULL, *int_result=NULL;
  double *real_x=NULL, *real_result=NULL;

  if(ncols(x) > 1)
    error("na.locf.xts only handle univariate, dimensioned data");

  nr = nrows(x);

  PROTECT(result = allocVector(TYPEOF(x), nrows(x))); P++;

  switch(TYPEOF(x)) {
    case INTSXP:
      int_x = INTEGER(x);
      int_result = INTEGER(result);
      /* copy leading NAs */
      for(i=0; i < (_first+1); i++) {
        int_result[i] = int_x[i];
      }
      /* result[_first] now has first value */
      for(i=_first+1; i<nr; i++) {
        int_result[i] = int_x[i];
        if(int_result[i] == NA_INTEGER)
          int_result[i] = int_result[i-1];
      }
      break;
    case REALSXP:
      real_x = REAL(x);
      real_result = REAL(result);
      for(i=0; i < (_first+1); i++) {
        real_result[i] = real_x[i];
      }
      for(i=_first+1; i<nr; i++) {
        real_result[i] = real_x[i];
        if(ISNA(real_result[i]))
          real_result[i] = real_result[i-1];
      }
      break;
    default:
      error("unsupported type");
      break;
  }
  setAttrib(result, R_DimSymbol, getAttrib(x, R_DimSymbol));
  setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
  setAttrib(result, xts_IndexSymbol, getAttrib(x, xts_IndexSymbol));
  copy_xtsCoreAttributes(x, result);
  copy_xtsAttributes(x, result);
  UNPROTECT(P);
  return(result);
}
