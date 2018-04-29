#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

/* Binary search range to find interval written by Corwin Joy, with
 * contributions by Joshua Ulrich
 */

static struct keyvec {
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

  if (length(vec) < 1) {
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
      break;
    case INTSXP:
      data.ikey = INTEGER(key)[0];
      data.ivec = INTEGER(vec);
      cmp_func = (use_start) ? cmp_int_lower : cmp_int_upper;
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
