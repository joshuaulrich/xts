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
#include "xts.h"

SEXP tryXts (SEXP x)
{
  if( !Rf_asInteger(isXts(x)) ) {
    int P = 0;
    SEXP expr, result, env, str_xts;

    PROTECT(str_xts = mkString("xts")); P++;
    PROTECT(env = R_FindNamespace(str_xts)); P++;

    PROTECT(expr = lang2(install("try.xts"), x)); P++;
    PROTECT(result = eval(expr, env)); P++;

    if( !Rf_asInteger(isXts(result)) ) {
      UNPROTECT(P);
      error("rbind.xts requires xtsible data");
    }
    UNPROTECT(P);
    return result;
  }
  return x;
}

/*
SEXP try_xts (SEXP x)
{
  SEXP y;
  PROTECT(y = tryXts(x));
  UNPROTECT(1);
  return y;
}
*/
