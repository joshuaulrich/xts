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
#include <Rmath.h>

/* Binary search range to find interval written by Corwin Joy, with
 * contributions by Joshua Ulrich
 */

struct keyvec {
  double *dvec;
  double dkey;
  int *ivec;
  int ikey;
};

/* Predicate function definition and functions to determine which of the
 * two groups contains the value being searched for. Note that they're all
 * 'static inline' to hopefully help with the compiler optimizations.
 */
typedef int (*bound_comparer)(const struct keyvec, const int);
static inline int
cmp_dbl_upper(const struct keyvec kv, const int i)
{
  const double cv = kv.dvec[i];
  const double ck = kv.dkey;
  return cv > ck;
}
static inline int
cmp_dbl_lower(const struct keyvec kv, const int i)
{
  const double cv = kv.dvec[i];
  const double ck = kv.dkey;
  return cv >= ck;
}
static inline int
cmp_int_upper(const struct keyvec kv, const int i)
{
  const int cv = kv.ivec[i];
  const int ck = kv.ikey;
  return cv > ck;
}
static inline int
cmp_int_lower(const struct keyvec kv, const int i)
{
  const int cv = kv.ivec[i];
  const int ck = kv.ikey;
  return cv >= ck;
}

/* Binary search function */
SEXP binsearch(SEXP key, SEXP vec, SEXP start)
{
  if (!isLogical(start)) {
    error("start must be specified as true or false");
  }

  if (length(vec) < 1 || length(key) < 1) {
    return ScalarInteger(NA_INTEGER);
  }

  int use_start = LOGICAL(start)[0];
  bound_comparer cmp_func = NULL;
  struct keyvec data;

  switch (TYPEOF(vec)) {
    case REALSXP:
      data.dkey = REAL(key)[0];
      data.dvec = REAL(vec);
      cmp_func = (use_start) ? cmp_dbl_lower : cmp_dbl_upper;
      if (!R_finite(data.dkey)) {
        return ScalarInteger(NA_INTEGER);
      }
      break;
    case INTSXP:
      data.ikey = INTEGER(key)[0];
      data.ivec = INTEGER(vec);
      cmp_func = (use_start) ? cmp_int_lower : cmp_int_upper;
      if (NA_INTEGER == data.ikey) {
        return ScalarInteger(NA_INTEGER);
      }
      break;
    default:
      error("unsupported type");
  }

  int mid;
  int lo = 0;
  int hi = length(vec) - 1;

  while (lo < hi) {
    mid = lo + (hi - lo) / 2;
    if (cmp_func(data, mid)) {
      hi = mid;
    }
    else {
      lo = mid + 1;
    }
  }

  /* 'lo' contains the smallest index where cmp_func() is true, but we need
   * to handle edge cases where 'lo' is at the max/min end of the vector.
   */
  if (use_start) {
    /* cmp_func() := vector[index] >= key when start == true, and we need
     * to return the smallest index subject to vector[index] >= key.
     */
    if (!cmp_func(data, length(vec)-1)) {
      /* entire vector < key */
      return ScalarInteger(NA_INTEGER);
    }
  } else {
    /* cmp_func() := vector[index] > key when start == false, and we need
     * to return the largest index subject to vector[index] <= key.
     */
    if (cmp_func(data, lo)) {
      /* previous index value must satisfy vector[index] <= key, unless
       * current index value is zero.
       */
      lo--;
      if (lo < 0) {
        /* entire vector > key */
        return ScalarInteger(NA_INTEGER);
      }
    }
  }

  /* Convert from 0-based index to 1-based index */
  lo++;

  return ScalarInteger(lo);
}

SEXP fill_window_dups_rev(SEXP _x, SEXP _index)
{
  /* Translate user index (_x) to xts index (_index). '_x' contains the
   * upper bound of the location of the user index in the xts index.
   * This is necessary to handle duplicate dates in the xts index.
   */
  int n_x = length(_x);
  int *x = INTEGER(_x);

  if (length(_index) < 1) {
    return allocVector(INTSXP, 0);
  }

  PROTECT_INDEX px;
  SEXP _out;
  PROTECT_WITH_INDEX(_out = allocVector(INTSXP, length(_index)), &px);
  int *out = INTEGER(_out);

  int i, xi, j, k = 0, n_out = length(_out);
  switch (TYPEOF(_index)) {
    case REALSXP:
      {
        double *index = REAL(_index);
        /* Loop over locations in _x in reverse order */
        for (i = n_x; i > 0; i--) {
          xi = x[i-1];
          j = xi;
          do {
            /* Check if we need to lengthen output due to duplicates */
            if (k == n_out) {
              REPROTECT(_out = xlengthgets(_out, k+2*(i+1)), px);
              out = INTEGER(_out);
              n_out = length(_out);
            }
            out[k++] = j--;
          } while (j > 0 && index[xi-1] == index[j-1]);
        }
      }
      break;
    case INTSXP:
      {
        int *index = INTEGER(_index);
        /* Loop over locations in _x in reverse order */
        for (i = n_x; i > 0; i--) {
          xi = x[i-1];
          j = xi;
          do {
            /* Check if we need to lengthen output due to duplicates */
            if (k == n_out) {
              REPROTECT(_out = xlengthgets(_out, k+2*(i+1)), px);
              out = INTEGER(_out);
              n_out = length(_out);
            }
            out[k++] = j--;
          } while (j > 0 && index[xi-1] == index[j-1]);
        }
      }
      break;
    default:
      error("unsupported index type");
  }

  /* truncate so length(_out) = k
   * NB: output is in reverse order!
   */
  REPROTECT(_out = xlengthgets(_out, k), px);
  UNPROTECT(1);

  return _out;
}
