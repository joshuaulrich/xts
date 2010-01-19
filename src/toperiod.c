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


#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "xts.h"

#ifndef MAX
#define MAX(a,b) (a > b ? a : b)
#endif

#ifndef MIN
#define MIN(a,b) (a < b ? a : b)
#endif

SEXP toPeriod(SEXP x, SEXP endpoints, SEXP hasVolume, SEXP hasAdjusted, SEXP first, SEXP colnames)
{
  SEXP result, ohlc, xindex, newindex, dimnames;

  int i, j=0, jstart, nrx, ncx, ncr, n, P=0;
  nrx = nrows(x);
  ncx = ncols(x);
  n = nrows(endpoints) - 1;
  ncr = 4; /* OHLC */
  int mode = TYPEOF(x);

  if(INTEGER(hasVolume)[0]) ncr++; /* Volume */
  if(INTEGER(hasAdjusted)[0]) ncr++; /* Adjusted (Yahoo) */

  /* handle index values in xts */
  PROTECT(xindex = getAttrib(x, xts_IndexSymbol)); P++;
  int index_mode = TYPEOF(xindex);
  PROTECT(newindex = allocVector(index_mode, n)); P++;
  
  PROTECT(result = allocVector(mode, n * ncr )); P++;
  PROTECT(ohlc = allocVector(mode, 6)); P++;

  int _FIRST = (INTEGER(first)[0]);
  int *ohlc_int = NULL,
      *x_int    = NULL;
  double *ohlc_real = NULL,
         *x_real    = NULL;

  switch(mode) {
    case INTSXP:
      ohlc_int = INTEGER(ohlc);
      x_int    = INTEGER(x);
      break;
    case REALSXP:
      ohlc_real = REAL(ohlc);
      x_real    = REAL(x);
      break;
    default:
      error("unsupported type");
   }
   
  int *_endpoints  = INTEGER(endpoints);
  int _hasAdjusted = INTEGER(hasAdjusted)[0]; 
  int _hasVolume   = INTEGER(hasVolume)[0]; 

  for(i = 0; i < n; i++) {
    jstart = j = _endpoints[i];

    if(_FIRST) {
      switch(index_mode) {
        case INTSXP:
          INTEGER(newindex)[i] = INTEGER(xindex)[j];
          break;
        case REALSXP:
          REAL(newindex)[i] = REAL(xindex)[j];
          break;
      }
    }
    // set the Open, and initialize High, Low and Volume
    switch(mode) {
      case INTSXP:
        ohlc_int[0] = x_int[j];                     //OP
        ohlc_int[1] = x_int[j + 1*nrx];             //HI
        ohlc_int[2] = x_int[j + 2*nrx];             //LO
        if(_hasVolume)
          ohlc_int[4] = (int)0;                     //VO
        break;
      case REALSXP:
        ohlc_real[0] = x_real[j];                 //OP
        ohlc_real[1] = x_real[j + 1*nrx];         //HI
        ohlc_real[2] = x_real[j + 2*nrx];         //LO
        if(_hasVolume)
          ohlc_real[4] = (double)0;                //VO
        break;
    }

    // set the High, Low, and Volume
    switch(mode) {
      case INTSXP:
        for( ; j < _endpoints[i+1]; j++) {
          ohlc_int[1] = MAX(ohlc_int[1], x_int[j + 1*nrx]);     /* HI */
          ohlc_int[2] = MIN(ohlc_int[2], x_int[j + 2*nrx]);     /* LO */
          if(_hasVolume)
            ohlc_int[4] = ohlc_int[4] + x_int[j + 4*nrx];       /* VO */
        }
        break;
      case REALSXP:
        for( ; j < _endpoints[i+1]; j++) {
          ohlc_real[1] = MAX(ohlc_real[1], x_real[j + 1*nrx]);  /* HI */
          ohlc_real[2] = MIN(ohlc_real[2], x_real[j + 2*nrx]);  /* LO */
          if(_hasVolume)
            ohlc_real[4] = ohlc_real[4] + x_real[j + 4*nrx];    /* VO */
        }
        break;
    }

    /* set the Close and Adjusted columns */
    /* Rprintf("i,j: %i,%i\t",i,j); */
    j--;
    switch(mode) {
      case INTSXP:
        ohlc_int[3] = x_int[j + 3*nrx];
        if(_hasAdjusted)
          ohlc_int[5] = x_int[j + 5*nrx];
        break;
      case REALSXP:
        ohlc_real[3] = x_real[j + 3*nrx];
        if(_hasAdjusted)
          ohlc_real[5] = x_real[j + 5*nrx];
        break;
    }
   
    if(!_FIRST) {  /* index at last position */
      switch(index_mode) {
        case INTSXP:
          INTEGER(newindex)[i] = INTEGER(xindex)[j];
          break;
        case REALSXP:
          REAL(newindex)[i] = REAL(xindex)[j];
          break;
      }
    }
    switch(mode) {
      case INTSXP:
        INTEGER(result)[i]     = ohlc_int[0];
        INTEGER(result)[i+1*n] = ohlc_int[1];
        INTEGER(result)[i+2*n] = ohlc_int[2];
        INTEGER(result)[i+3*n] = ohlc_int[3];
        if(_hasVolume)
          INTEGER(result)[i+4*n] = ohlc_int[4];
        if(_hasAdjusted)
          INTEGER(result)[i+5*n] = ohlc_int[5];
        break;
      case REALSXP:
        REAL(result)[i]     = REAL(ohlc)[0];
        REAL(result)[i+1*n] = REAL(ohlc)[1];
        REAL(result)[i+2*n] = REAL(ohlc)[2];
        REAL(result)[i+3*n] = REAL(ohlc)[3];
        if(_hasVolume)
          REAL(result)[i+4*n] = REAL(ohlc)[4];
        if(_hasAdjusted)
          REAL(result)[i+5*n] = REAL(ohlc)[5];
        break;
    }
    /* Rprintf("i,j: %i,%i\n",i,j); */

  }

  SEXP dim;
  dim = PROTECT(allocVector(INTSXP, 2)); P++;
  INTEGER(dim)[0] = n;
  INTEGER(dim)[1] = ncr;
  setAttrib(result, R_DimSymbol, dim);

  PROTECT(dimnames = allocVector(VECSXP, 2)); P++;
  SET_VECTOR_ELT(dimnames, 0, R_NilValue);  /* no rownames ever! */
  if(!isNull(colnames)) {
    SET_VECTOR_ELT(dimnames, 1, colnames);
  } else {
    SEXP newcolnames;
    PROTECT(newcolnames = allocVector(STRSXP, ncr));P++;
    SET_STRING_ELT(newcolnames, 0, mkChar("Open"));
    SET_STRING_ELT(newcolnames, 1, mkChar("High"));
    SET_STRING_ELT(newcolnames, 2, mkChar("Low"));
    SET_STRING_ELT(newcolnames, 3, mkChar("Close"));
    if(INTEGER(hasVolume))
      SET_STRING_ELT(newcolnames, 4, mkChar("Volume"));
    if(INTEGER(hasVolume))
      SET_STRING_ELT(newcolnames, 5, mkChar("Adjusted"));
    SET_VECTOR_ELT(dimnames, 1, newcolnames);
  }
  setAttrib(result, R_DimNamesSymbol, dimnames);
  setAttrib(result, xts_IndexSymbol, newindex);

  copy_xtsAttributes(x, result);
  copy_xtsCoreAttributes(x, result);

  UNPROTECT(P);
  return result;
}
