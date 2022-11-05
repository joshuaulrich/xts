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

int xts_ncols (SEXP _x)
{
  int ncols_x = 0;

  // use dims if possible
  if (isNull(getAttrib(_x, R_DimSymbol))) {
    // no dims, so we want:
    // * ncols_x == 0 for zero-length vectors, and
    // * ncols_x == 1 for anything else that doesn't have dims
    ncols_x = LENGTH(_x) > 0;
  } else {
    // use dims
    ncols_x = INTEGER(getAttrib(_x, R_DimSymbol))[1];
  }

  return ncols_x;
}

SEXP number_of_cols (SEXP args)
{
  int i = 0;
  args = CDR(args); // calling function name

  SEXP tcols = PROTECT(allocVector(INTSXP, length(args)));

  for(;args != R_NilValue; i++, args = CDR(args)) {
    INTEGER(tcols)[i] = xts_ncols(CAR(args));
  }

  UNPROTECT(1);
  return tcols;
}
