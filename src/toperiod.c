#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "xts.h"

#define MAX(a,b) (a > b ? a : b)
#define MIN(a,b) (a < b ? a : b)

SEXP toPeriod(SEXP x, SEXP endpoints, SEXP hasVolume, SEXP hasAdjusted, SEXP first, SEXP colnames)
{
  SEXP result, ohlc, xindex, newindex, dimnames;

  int i, j=0, jstart, ii, nrx, ncx, ncr, n, P=0;
  nrx = nrows(x);
  ncx = ncols(x);
  n = nrows(endpoints) - 1;
  ncr = 4; // OHLC
  int mode = TYPEOF(x);

  if(INTEGER(hasVolume)[0]) ncr++; // Volume 
  if(INTEGER(hasAdjusted)[0]) ncr++; // Adjusted (Yahoo)

  // handle index values in xts
  PROTECT(xindex = getAttrib(x, xts_IndexSymbol)); P++;
  PROTECT(newindex = allocVector(TYPEOF(xindex), n)); P++;
  
  PROTECT(result = allocVector(mode, n * ncr )); P++;
  PROTECT(ohlc = allocVector(mode, 6)); P++;

  int _FIRST = (INTEGER(first)[0]);

  for(i = 0; i < n; i++) {
    jstart = j = INTEGER(endpoints)[i];

    if(_FIRST)
      INTEGER(newindex)[i] = INTEGER(xindex)[j];
    // set the Open, and initialize High, Low and Volume
    switch(mode) {
      case INTSXP:
        INTEGER(ohlc)[0] = INTEGER(x)[j];
        INTEGER(ohlc)[1] = INTEGER(x)[j + 1*nrx];
        INTEGER(ohlc)[2] = INTEGER(x)[j + 2*nrx];
        if(INTEGER(hasVolume))
          INTEGER(ohlc)[4] = (int)0;
        break;
      case REALSXP:
        REAL(ohlc)[0] = REAL(x)[j];           //OP
        REAL(ohlc)[1] = REAL(x)[j + 1*nrx];   //HI
        REAL(ohlc)[2] = REAL(x)[j + 2*nrx];   //LO
        if(INTEGER(hasVolume))
          REAL(ohlc)[4] = (double)0;          //VO
        break;
    }

    // set the High, Low, and Volume
    for( ; j < INTEGER(endpoints)[i+1]; j++) {
      INTEGER(ohlc)[1] = MAX(INTEGER(ohlc)[1], INTEGER(x)[j + 1*nrx]);  //HI
      INTEGER(ohlc)[2] = MIN(INTEGER(ohlc)[2], INTEGER(x)[j + 2*nrx]);  //LO
      if(INTEGER(hasVolume))
        INTEGER(ohlc)[4] = INTEGER(ohlc)[4] + INTEGER(x)[j + 4*nrx];    //VO
    }

    // set the Close and Adjusted columns
    j--;
    INTEGER(ohlc)[3] = INTEGER(x)[j + 3*nrx];
    if(INTEGER(hasAdjusted))
      INTEGER(ohlc)[5] = INTEGER(x)[j + 5*nrx];
   
    if(!_FIRST)  // if index at last
      INTEGER(newindex)[i] = INTEGER(xindex)[j];

    INTEGER(result)[i]     = INTEGER(ohlc)[0];
    INTEGER(result)[i+1*n] = INTEGER(ohlc)[1];
    INTEGER(result)[i+2*n] = INTEGER(ohlc)[2];
    INTEGER(result)[i+3*n] = INTEGER(ohlc)[3];
    if(INTEGER(hasVolume))
      INTEGER(result)[i+4*n] = INTEGER(ohlc)[4];
    if(INTEGER(hasAdjusted))
      INTEGER(result)[i+5*n] = INTEGER(ohlc)[5];


  }
  SEXP dim;
  dim = allocVector(INTSXP, 2);
  INTEGER(dim)[0] = n;
  INTEGER(dim)[1] = ncr;
  setAttrib(result, R_DimSymbol, dim);

  PROTECT(dimnames = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(dimnames, 0, R_NilValue);  // no rownames ever!   
  if(!isNull(colnames)) {
    SET_VECTOR_ELT(dimnames, 1, colnames);
  } else {
    SEXP newcolnames;
    PROTECT(newcolnames = allocVector(STRSXP, ncr));
    SET_STRING_ELT(newcolnames, 0, mkChar("Open"));
    SET_STRING_ELT(newcolnames, 1, mkChar("High"));
    SET_STRING_ELT(newcolnames, 2, mkChar("Low"));
    SET_STRING_ELT(newcolnames, 3, mkChar("Close"));
    if(INTEGER(hasVolume))
      SET_STRING_ELT(newcolnames, 4, mkChar("Volume"));
    if(INTEGER(hasVolume))
      SET_STRING_ELT(newcolnames, 5, mkChar("Adjusted"));
    SET_VECTOR_ELT(dimnames, 1, newcolnames);
    UNPROTECT(1);
  }
  setAttrib(result, R_DimNamesSymbol, dimnames);
  UNPROTECT(1);

  setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));

  setAttrib(result, xts_ClassSymbol, getAttrib(x, xts_ClassSymbol));
  setAttrib(result, xts_IndexSymbol, newindex);
  setAttrib(result, xts_IndexClassSymbol, getAttrib(x, xts_IndexClassSymbol));
  setAttrib(result, xts_IndexFormatSymbol, getAttrib(x, xts_IndexFormatSymbol));

  copy_xtsAttributes(x, result);
  UNPROTECT(P);
  return result;
}
