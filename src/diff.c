#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "xts.h"

SEXP diffXts(SEXP x, SEXP lag, SEXP diff, SEXP arith, SEXP nap, SEXP dots)
{
   
}


SEXP lagXts(SEXP x, SEXP k, SEXP pad)
{
  SEXP result;
  int nrs, ncs;
  int i, j, ij, iijj, K, NApad;
  int P=0; //PROTECT counter
  
  nrs = nrows(x);
  ncs = ncols(x);

  K = INTEGER(k)[ 0 ];
  K = (K > nrs) ? nrs : K;

  NApad = INTEGER(pad)[ 0 ];

  if(NApad) {
    PROTECT(result = allocVector(TYPEOF(x), nrs*ncs)); P++;
  } else {
    if(K > 0) {
      PROTECT(result = allocVector(TYPEOF(x), (nrs-K)*ncs)); P++;
    } else {
      PROTECT(result = allocVector(TYPEOF(x), (nrs+K)*ncs)); P++;
    }
  }

  for(i = 0; i < nrs; i++) {
  for(j = 0; j < ncs; j++) {
    ij = i + j * nrs;
//Rprintf("i=%i\tK=%i\t(nrs+K)=%i\n",i,K,(nrs+K));
    if(i < K ||
       K < 0 && i > (nrs+K-1)) {
    /* Pad NA values at beginning */
      if(NApad) {
      switch (TYPEOF(x)) {
        case LGLSXP:
        case INTSXP:
             INTEGER(result)[ij] = NA_INTEGER;
             break;
        case REALSXP:
             REAL(result)[ij] = NA_REAL;
             break;
        case CPLXSXP:
             COMPLEX(result)[ij].r = NA_REAL;
             COMPLEX(result)[ij].i = NA_REAL;
             break;
        case STRSXP:
             SET_STRING_ELT(result, ij, NA_STRING);
             break;
        case VECSXP:
             SET_VECTOR_ELT(result, ij, R_NilValue);
             break;
        case RAWSXP:
             RAW(result)[ij] = (Rbyte) 0;
             break;
        default:
             error("matrix subscripting not handled for this type");
             break;
      } // NA insertion
      } // NApad
    } else {
      iijj = i - K + j * nrs; // move back K positions to get data
      if(!NApad && K > 0) ij = i - K + j * (nrs - K);   // if not padding, start at the correct spot
      if(!NApad && K < 0) ij = i + j * (nrs + K);   // if not padding, start at the correct spot
//Rprintf("i=%i;\tj=%i;\tij=%i;\tiijj=%i\n", i, j, ij, iijj);
      switch (TYPEOF(x)) {
        case LGLSXP:
             LOGICAL(result)[ij] = LOGICAL(x)[iijj];
             break;
        case INTSXP:
             INTEGER(result)[ij] = INTEGER(x)[iijj];
             break;
        case REALSXP:
             REAL(result)[ij] = REAL(x)[iijj];
             break;
        case CPLXSXP:
             COMPLEX(result)[ij] = COMPLEX(x)[iijj];
             break;
        case STRSXP:
             SET_STRING_ELT(result, ij, STRING_ELT(x, iijj));
             break;
        case VECSXP:
             SET_VECTOR_ELT(result, ij, VECTOR_ELT(x, iijj));
             break;
        case RAWSXP:
             RAW(result)[ij] = RAW(x)[iijj];
             break;
        default:
             error("matrix subscripting not handled for this type");
             break;
      }
    }
  } // j-loop
  } // i-loop

  setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
  if(!NApad) { // No NA padding
    SEXP oindex, nindex, dims;
    int nRows = (K > 0) ? nrs-K : nrs+K;
    PROTECT(oindex = getAttrib(x, xts_IndexSymbol));
    PROTECT(nindex = allocVector(TYPEOF(oindex), nRows));
    if(K > 0) {
    switch(TYPEOF(oindex)) {
      case REALSXP:
        for( i = 0; i < (nrs-K); i++)
          REAL(nindex)[ i ] = REAL(oindex)[ i+K ];
        break;
      case INTSXP:
        for( i = 0; i < (nrs-K); i++)
          INTEGER(nindex)[ i ] = INTEGER(oindex)[ i+K ];
        break;
      default:
        break;
    }
    } else {
    switch(TYPEOF(oindex)) {
      case REALSXP:
        for( i = 0; i < nrs+K; i++)
          REAL(nindex)[ i ] = REAL(oindex)[ i ];
        break;
      case INTSXP:
        for( i = 0; i < nrs+K; i++)
          INTEGER(nindex)[ i ] = INTEGER(oindex)[ i ];
        break;
      default:
        break;
    }

    }
    setAttrib(result, xts_IndexSymbol, nindex);
    PROTECT(dims = allocVector(INTSXP, 2));
    INTEGER(dims)[0] = nRows;
    INTEGER(dims)[1] = ncs;
    setAttrib(result, R_DimSymbol, dims);
    setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
    UNPROTECT(3);
  } else {
    // NA pad
    setAttrib(result, xts_IndexSymbol, getAttrib(x, xts_IndexSymbol));
    setAttrib(result, R_DimSymbol, getAttrib(x, R_DimSymbol));
    setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
  }
  setAttrib(result, xts_ClassSymbol, getAttrib(x, xts_ClassSymbol));
  setAttrib(result, xts_IndexFormatSymbol, getAttrib(x, xts_IndexFormatSymbol));
  setAttrib(result, xts_IndexClassSymbol, getAttrib(x, xts_IndexClassSymbol));

  UNPROTECT(P);
  return result;
}
