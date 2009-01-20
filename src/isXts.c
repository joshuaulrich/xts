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
#include <Rdefines.h>

int isXts(SEXP x) 
{
  int i;
  SEXP attr;

  PROTECT( attr = coerceVector(getAttrib(x, R_ClassSymbol),STRSXP) );
  if(length(attr) <= 1) {
    UNPROTECT(1);
    return 0;
  }

  for(i = 0; i < length(attr); i++) {
    if(STRING_ELT(attr, i) == mkChar("xts")) {
      UNPROTECT(1);
      return 1;
    }
  }
  UNPROTECT(1);
  return FALSE;

}

/* test function and example */
SEXP test_isXts(SEXP x)
{
  if(isXts(x)) {
    Rprintf("TRUE\n");
  } else {
    Rprintf("FALSE\n");
  }
  return R_NilValue;
}
