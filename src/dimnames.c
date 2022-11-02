/*
#   xts: eXtensible time-series 
#
#   Copyright (C) 2010  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
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

SEXP dimnames_zoo (SEXP x) {
  return(getAttrib(x, R_DimNamesSymbol));
}

SEXP xts_set_dimnames (SEXP x, SEXP value) {
  if (R_NilValue == value) {
    setAttrib(x, R_DimNamesSymbol, R_NilValue);
  } else {
    if (TYPEOF(value) != VECSXP || length(value) != 2) {
      error("invalid 'dimnames' given for xts");
    }
    /* xts objects never have row names */
    SET_VECTOR_ELT(value, 0, R_NilValue);
    setAttrib(x, R_DimNamesSymbol, value);
  }
  return x;
}
