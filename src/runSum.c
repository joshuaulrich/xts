/*
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
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
  int *int_n=NULL;
  if(TYPEOF(n) != INTSXP) {
    // assure that 'n' is an integer
    PROTECT(n = coerceVector(n, INTSXP)); P++;
  }
  int_n = INTEGER(n); // get the first element (everything from R is a vector)

  int *int_result=NULL, *int_x=NULL;
  int int_sum = 0;
  double *real_result=NULL, *real_x=NULL;
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
