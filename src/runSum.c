/***********************************************************************/
/* xts, copyright (C) Jeffrey A. Ryan, 2008                            */
//
// experimental code to provide rollapply functionality to standard
// functions.  Essentially a .Call port of Josh's run*** functions
// from TTR.  Though it should be obvious these are a lot more complex.
//
//  runSum, runMin, runMax, runMedian, runMean, runSD
//
/***********************************************************************/
#include <R.h>
#include <Rinternals.h>

SEXP runSum (SEXP x, SEXP n)
{
  SEXP result;
  int P=0;
  int i, nrs;
  int *int_n;
  if(TYPEOF(n) != INTSXP) {
    // assure that 'n' is an integer
    PROTECT(n = coerceVector(n, INTSXP)); P++;
  }
  int_n = INTEGER(n); // get the first element (everything from R is a vector)

  int *int_result, *int_x;
  int int_sum = 0;
  double *real_result, *real_x;
  double real_sum = 0.0;

  PROTECT(result = allocVector(TYPEOF(x), length(x))); P++;

  switch(TYPEOF(x)) {
    /* still need to implement other types, and checking
    //  
    // The branch by type allows for fewer type checks/branching
    // within the algorithm, providing a _much_ faster mechanism
    // to calculate the sum
    */
    case REALSXP:
      real_result = REAL(result);
      real_x = REAL(x);
      int_result = int_x = NULL;
      for(i = 0; i < (*int_n); i++) {
        real_result[i] = NA_REAL;
        real_sum = real_sum + real_x[i];
      }
      real_result[ (*int_n)-1 ] = real_sum;
      nrs = nrows(x);
      for(i = (*int_n); i < nrs; i++)
        real_result[i] = real_result[i-1] + real_x[i] - real_x[i-(*int_n)];
      break;
    case INTSXP:
      int_result = INTEGER(result);
      int_x = INTEGER(x);
      real_result = real_x = NULL;
      for(i = 0; i < (*int_n); i++) {  // (*int_n) is faster that INTEGER(n)[1], a constant would be equal
        int_result[i] = NA_INTEGER;
        int_sum = int_sum + int_x[i];
      }
      int_result[ (*int_n)-1 ] = int_sum;
      nrs = nrows(x);
      for(i = (*int_n); i < nrs; i++)
        int_result[i] = int_result[i-1] + int_x[i] - int_x[i-(*int_n)];
      break;
    /*
    case STRSXP:  fail!
    case LGLSXP:
    case CPLXSXP:
    */
  }

  /* there are MACROS and functions in xts.h that will do this */
  setAttrib(result, R_DimSymbol, getAttrib(x, R_DimSymbol));
  setAttrib(result, install("class"), getAttrib(x, install("class")));
  setAttrib(result, install("index"), getAttrib(x, install("index")));
  setAttrib(result, install(".indexFORMAT"), getAttrib(x, install(".indexFORMAT")));
  setAttrib(result, install(".indexCLASS"), getAttrib(x, install(".indexCLASS")));

  UNPROTECT(P);
  return result;
}
