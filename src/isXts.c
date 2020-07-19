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
#include <Rinternals.h>
#include <Rdefines.h>
#include "xts.h"

SEXP isXts(SEXP x)
{
  int i;
  SEXP attr, index;

  index = getAttrib(x, xts_IndexSymbol);
  PROTECT( attr = coerceVector(getAttrib(x, R_ClassSymbol),STRSXP) );
  if(length(attr) <= 1) {
    UNPROTECT(1);
    return Rf_ScalarInteger(0);
  }

  for(i = 0; i < length(attr); i++) {
    if(STRING_ELT(attr, i) == mkChar("xts")) {
      /* check for index attribute */
      if(TYPEOF(index)==REALSXP || TYPEOF(index)==INTSXP) {
        UNPROTECT(1);
        return Rf_ScalarInteger(1);
      } else {
        UNPROTECT(1);
        return Rf_ScalarInteger(0);
      }
    }
  }
  UNPROTECT(1);
  return Rf_ScalarInteger(FALSE);

}

/* test function and example */
SEXP test_isXts(SEXP x)
{
    if(Rf_asInteger(isXts(x))) {
    Rprintf("TRUE\n");
  } else {
    Rprintf("FALSE\n");
  }
  return R_NilValue;
}
