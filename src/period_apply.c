/*
#   xts: eXtensible time-series
#
#   Copyright (C) 2008  Jeffrey A. Ryan   (FORTRAN implementation)
#   Copyright (C) 2018  Joshua M. Ulrich  (C implementation)
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


#include<Rinternals.h>
#include "xts.h"

SEXP xts_period_apply(SEXP _data, SEXP _index, SEXP _function, SEXP _env)
{
  int i;
  R_xlen_t n = xlength(_index);
  SEXP _result = PROTECT(allocVector(VECSXP, n));
  SEXP _j = PROTECT(allocVector(INTSXP, ncols(_data)));
  SEXP _drop = PROTECT(ScalarLogical(0));

  for (i = 0; i < ncols(_data); i++)
    INTEGER(_j)[i] = i + 1;

  SEXP _idx0 = PROTECT(ScalarInteger(0));
  SEXP _idx1 = PROTECT(ScalarInteger(0));
  int *idx0 = INTEGER(_idx0);
  int *idx1 = INTEGER(_idx1);

  /* reprotect the subset object */
  SEXP _xsubset;
  PROTECT_INDEX px;
  PROTECT_WITH_INDEX(_xsubset = R_NilValue, &px);

  /* subset object name */
  SEXP _subsym = install("_.*crazy*._.*name*._");
  defineVar(_subsym, _xsubset, _env);

  /* function call on subset */
  SEXP _subcall = PROTECT(lang3(_function, _subsym, R_DotsSymbol));

  int N = n - 1;

  switch(TYPEOF(_index)) {
    case REALSXP:
      ;
      double *d_index = REAL(_index);
      for (i = 0; i < N; i++) {
        idx0[0] = d_index[i] + 1;
        idx1[0] = d_index[i + 1];
        REPROTECT(_xsubset = extract_col(_data, _j, _drop, _idx0, _idx1), px);
        defineVar(_subsym, _xsubset, _env);
        SET_VECTOR_ELT(_result, i, eval(_subcall, _env));
      }
      break;

    case INTSXP:
      ;
      int *i_index = INTEGER(_index);
      for (i = 0; i < N; i++) {
        idx0[0] = i_index[i] + 1;
        idx1[0] = i_index[i + 1];
        REPROTECT(_xsubset = extract_col(_data, _j, _drop, _idx0, _idx1), px);
        defineVar(_subsym, _xsubset, _env);
        SET_VECTOR_ELT(_result, i, eval(_subcall, _env));
      }
      break;
    default:
      error("unsupported index type");
  }

  UNPROTECT(7);
  return _result;
}
