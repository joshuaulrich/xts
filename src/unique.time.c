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


#include <R.h>
#include "xts.h" /* for coredata_xts */

SEXP make_unique (SEXP index_, SEXP eps_) {
  int P = 0, i;
  int len = length(index_);
  double eps = asReal(eps_);

  if (TYPEOF(index_) == INTSXP) {
    PROTECT(index_ = coerceVector(index_, REALSXP)); P++;
  }
  
  SEXP newindex_ = PROTECT(allocVector(REALSXP, len)); P++;
  copyAttributes(index_, newindex_);
  double *newindex_real = REAL(newindex_);
  memcpy(REAL(newindex_), REAL(index_), len * sizeof(double));

  double last_index = newindex_real[0];
  int warn_once = 1;
  for(i=1; i<len; i++) {
    if(newindex_real[i] <= newindex_real[i-1]) {
      if(warn_once && last_index != newindex_real[i]) {
        warn_once = 0;
        warning("index value is unique but will be replaced; it is less than the cumulative epsilon for the preceding duplicate index values");
      }
      newindex_real[i] = newindex_real[i-1] + eps;
    }
  }

  UNPROTECT(P);
  return newindex_;
}

SEXP make_index_unique (SEXP x_, SEXP eps_) {
  SEXP result;
  PROTECT(result = coredata_xts(x_));
  copyAttributes(x_, result); /* copy all attrib except index, and dim-related */
  setAttrib(result, xts_IndexSymbol, make_unique(getAttrib(x_, xts_IndexSymbol), eps_));
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
  /* need to reprotect lengthgets() result before returning */
  PROTECT_INDEX idx;
  PROTECT_WITH_INDEX(duplicates = allocVector(INTSXP, len), &idx);
  duplicates_int = INTEGER(duplicates);

  if(!fromLast) { /* keep first observation */
    duplicates_int[0] = ++d;
    switch(TYPEOF(x_)) {
      case INTSXP:
        x_int = INTEGER(x_);
        for(i=1; i < len-1; i++) {
          if( x_int[i-1] != x_int[i]) {
#ifdef DEBUG
            Rprintf("i=%i:  x[i-1]=%i, x[i]=%i\n",i,x_int[i-1],x_int[i]);
#endif
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
  REPROTECT(duplicates = lengthgets(duplicates, d), idx);
  UNPROTECT(1);
  return(duplicates);
}
