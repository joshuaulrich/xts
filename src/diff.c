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

SEXP diffXts(SEXP x, SEXP lag, SEXP diff, SEXP arith, SEXP nap, SEXP dots)
{
  return R_NilValue;
}


SEXP lag_xts (SEXP x, SEXP _k, SEXP _pad) {
  /* this will eventually revert to NOT changing R default behaviors 
     for now it uses the 'standard' convention adopted by xts        */

  int k = asInteger(_k);
  /* ensure args are correct types; error if conversion fails */
  if(k == NA_INTEGER)
    error("'k' must be integer");
  if(asLogical(_pad) == NA_LOGICAL)
    error("'na.pad' must be logical");

  k = k * -1; /* change zoo default negative handling */
  return zoo_lag (x, ScalarInteger(k), _pad);
}

SEXP lagts_xts (SEXP x, SEXP _k, SEXP _pad) {
  /* this will use positive values of lag for carrying forward observations
 
     i.e. y = lagts(x, 1) is y(t) = x(t-1)
  */

  int k = asInteger(_k)*-1; /* change zoo default negative handling */
  /* ensure args are correct types; error if conversion fails */
  if(k == NA_INTEGER)
    error("'k' must be integer");
  if(asLogical(_pad) == NA_LOGICAL)
    error("'na.pad' must be logical");

  return zoo_lag (x, ScalarInteger(k), _pad);
}
