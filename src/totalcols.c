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

SEXP number_of_cols (SEXP args)
{
  SEXP tcols;
  int P=0;

  args = CDR(args); // calling function name

  PROTECT(tcols = allocVector(INTSXP, length(args))); P++;
  int i=0;
  for(;args != R_NilValue; i++, args=CDR(args)) {
    SEXP arg_i = CAR(args);
    // use dims if available
    if(isNull(getAttrib(arg_i, R_DimSymbol))) {
      // no dims use number of columns
      if(length(arg_i) > 0) {
        INTEGER(tcols)[i] = ncols(arg_i);
      } else {
        // when arg_i is zero-length, ncols == 1
        INTEGER(tcols)[i] = 0;
      }
    } else {
      // have dims
      INTEGER(tcols)[i] = INTEGER(getAttrib(arg_i, R_DimSymbol))[1];
    }
  }

  UNPROTECT(P);
  return tcols;
}
