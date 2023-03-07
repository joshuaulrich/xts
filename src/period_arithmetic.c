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

SEXP xts_period_prod(SEXP _data, SEXP _index)
{
  if (ncols(_data) > 1) {
    error("single column data only");
  }
  if (!isInteger(_index)) {
    error("index must be integer");
  }
  if (!isReal(_data)) {
    error("data must be double");
  }

  R_xlen_t i, j;
  R_xlen_t n = xlength(_index) - 1;
  SEXP _result = PROTECT(allocVector(REALSXP, n));
  double *result = REAL(_result);

  int *index = INTEGER(_index);
  double *data = REAL(_data);
  R_xlen_t k = 0;

  for (i = 0; i < n; i++) {
    int idx0 = index[i];
    int idx1 = index[i + 1];
    double prod = data[idx0];
    for (j = idx0+1; j < idx1; j++) {
      prod *= data[j];
    }
    result[k++] = prod;
  }

  UNPROTECT(1);
  return _result;
}

SEXP xts_period_sum(SEXP _data, SEXP _index)
{
  if (ncols(_data) > 1) {
    error("single column data only");
  }
  if (!isInteger(_index)) {
    error("index must be integer");
  }
  if (!isReal(_data)) {
    error("data must be double");
  }

  R_xlen_t i, j;
  R_xlen_t n = xlength(_index) - 1;
  SEXP _result = PROTECT(allocVector(REALSXP, n));
  double *result = REAL(_result);

  int *index = INTEGER(_index);
  double *data = REAL(_data);
  R_xlen_t k = 0;

  for (i = 0; i < n; i++) {
    int idx0 = index[i];
    int idx1 = index[i + 1];
    double sum = data[idx0];
    for (j = idx0+1; j < idx1; j++) {
      sum += data[j];
    }
    result[k++] = sum;
  }

  UNPROTECT(1);
  return _result;
}
