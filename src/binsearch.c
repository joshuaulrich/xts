#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

/* Binary search range to find interval contributed by Corwin Joy */

/* Find the smallest index for which A[index] >= key */
int lower_bound(double key, double *A, int len_A) {
  int mid;
  int lo = 0;
  int hi = len_A-1;
  while (lo < hi) {
    mid = lo + (hi - lo) / 2;
    if (A[mid] >= key) {
      hi = mid;
    }
    else {
      lo = mid + 1;
    }
  }

  return lo; // lo is the least x for which A[x] >= key is true
}

/* Find the smallest index for which A[index] > key */
int upper_bound(double key, double *A, int len_A) {
  int mid;
  int lo = 0;
  int hi = len_A - 1;
  while (lo < hi) {
    mid = lo + (hi - lo) / 2;
    if (A[mid] > key) {
      hi = mid;
    }
    else {
      lo = mid + 1;
    }
  }

  return lo; // lo is the least x for which A[x] > key is true
}

/* bound the key in the array of vals.
   if start == true, return lowest index s.t. vals[index] >= key
   if start == false, return highest index s.t. vals[index] <= key
*/
int bound(double key, double *vals, int len_vals, int start) {
  int item;
  if (start) {
    item = lower_bound(key, vals, len_vals);
  }
  else {
    item = upper_bound(key, vals, len_vals);
    /* if handles edge cases. item may be at the lo/hi end of array */
    if (item > 0 && vals[item] > key) --item;
  }
  return(item);
}

SEXP binsearch(SEXP key, SEXP vec, SEXP start)
{
  double *rvec;
  double *rkey;
  int item;
  int len_vec = length(vec);

  if(!(TYPEOF(vec) == REALSXP && TYPEOF(key) == REALSXP)) {
    error("both key and vec must be of type double");
  }

  if(isNull(start)) {
    error("start must be specified as true or false");
  }

  if(len_vec < 1) {
    return ScalarInteger(NA_INTEGER);
  }

  rvec = REAL(vec);
  rkey = REAL(key);
  item = bound(*rkey, rvec, len_vec, LOGICAL(start)[0]);

  if(LOGICAL(start)[0]) {
    if(rvec[item] < *rkey) {
      /* start = TRUE, but entire array < key */
      return ScalarInteger(NA_INTEGER);
    }
  } else {
    if(rvec[item] > *rkey) {
      /* start = FALSE, but entire array > key */
      return ScalarInteger(NA_INTEGER);
    }
  }
  return ScalarInteger(item+1);
}

