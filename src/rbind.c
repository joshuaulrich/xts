#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "xts.h"

SEXP do_rbind_xts (SEXP x, SEXP y)
{
  int nrx, ncx, nry, ncy, len;
  int i, j, ij, xp=1, yp=1;
  int P=0; // PROTECT counter
  SEXP result, xindex, yindex, newindex;

  nrx = nrows(x);
  ncx = ncols(x);

  nry = nrows(y);
  ncy = ncols(y);

  len = nrx + nry;

  if(ncx != ncy)
    error("objects must have same number of columns to bind by row");

  PROTECT(xindex = getAttrib(x, xts_IndexSymbol)); P++;
  PROTECT(yindex = getAttrib(y, xts_IndexSymbol)); P++;

  if( TYPEOF(xindex) != TYPEOF(yindex) ) 
  {
    PROTECT(xindex = coerceVector(xindex, REALSXP)); P++;
    PROTECT(yindex = coerceVector(yindex, REALSXP)); P++;
  }

  PROTECT(newindex = allocVector(TYPEOF(xindex), len)); P++;
  PROTECT(result   = allocVector(TYPEOF(x), len)); P++;

  if( TYPEOF(xindex) == REALSXP ) { 
  while( (xp + yp) <= (len + 1) ) { 
    if( xp > nrx ) { 
      yp++;
    } else
    if( yp > nry ) {
      xp++;
    } else
    if(REAL(xindex)[ xp-1 ] == REAL(yindex)[ yp-1 ]) {
      yp++;
      xp++;
    } else
    if( REAL(xindex)[ xp-1 ]  < REAL(yindex)[ yp-1 ] ) {
      xp++;
    } else
    if( REAL(xindex)[ xp-1 ]  > REAL(yindex)[ yp-1 ] ) {
      yp++;
    }
  }
  } else
  if( TYPEOF(xindex) == INTSXP ) {
  //while( (xp + yp) <= (len + 1) ) {
  for(i = 0; i < len; i++) {
    if( xp > nrx ) {
      //Rprintf("yindex:%i\n",INTEGER(yindex)[yp-1]);
      INTEGER(newindex)[ i ] = INTEGER(yindex)[ yp-1 ];
      switch(TYPEOF(x)) {
        case INTSXP:
          INTEGER(result)[ i ] = INTEGER(y)[ yp-1 ];
          break;
        case REALSXP:
          REAL(result)[ i ] = REAL(y)[ yp-1 ];
          break;
        default:
          break;
      }
      yp++;
    } else
    if( yp > nry ) {
      //Rprintf("xindex:%i\n",INTEGER(xindex)[xp-1]);
      INTEGER(newindex)[ i ] = INTEGER(xindex)[ xp-1 ];
      switch(TYPEOF(x)) {
        case INTSXP:
          INTEGER(result)[ i ] = INTEGER(x)[ xp-1 ];
          break;
        case REALSXP:
          REAL(result)[ i ] = REAL(x)[ xp-1 ];
          break;
        default:
          break;
      }
      xp++;
    } else
    if(INTEGER(xindex)[ xp-1 ] == INTEGER(yindex)[ yp-1 ]) {
      //Rprintf("xindex:%i\n",INTEGER(xindex)[xp-1]);
      //Rprintf("yindex:%i\n",INTEGER(yindex)[yp-1]);
      INTEGER(newindex)[ i ]   = INTEGER(xindex)[ xp-1 ];
      INTEGER(newindex)[ i+1 ] = INTEGER(yindex)[ yp-1 ];
      switch(TYPEOF(x)) {
        case INTSXP:
          INTEGER(result)[ i ]     = INTEGER(x)[ xp-1 ];
          INTEGER(result)[ i+1 ]   = INTEGER(y)[ yp-1 ];
          break;
        case REALSXP:
          REAL(result)[ i ]     = REAL(x)[ xp-1 ];
          REAL(result)[ i+1 ]   = REAL(y)[ yp-1 ];
          break;
        default:
          break;
      }
      yp++;
      xp++;
      i++;  // need to increase i as we now have filled in 2 values
    } else
    if( INTEGER(xindex)[ xp-1 ]  < INTEGER(yindex)[ yp-1 ] ) {
      //Rprintf("xindex:%i\n",INTEGER(xindex)[xp-1]);
      INTEGER(newindex)[ i ] = INTEGER(xindex)[ xp-1 ];
      switch(TYPEOF(x)) {
        case INTSXP:
          INTEGER(result)[ i ] = INTEGER(x)[ xp-1 ];
          break;
        case REALSXP:
          REAL(result)[ i ] = REAL(x)[ xp-1 ];
          break;
        default:
          break;
      }
      xp++;
    } else
    if( INTEGER(xindex)[ xp-1 ]  > INTEGER(yindex)[ yp-1 ] ) {
      //Rprintf("yindex:%i\n",INTEGER(yindex)[yp-1]);
      INTEGER(newindex)[ i ] = INTEGER(yindex)[ yp-1 ];
      switch(TYPEOF(x)) {
        case INTSXP:
          INTEGER(result)[ i ] = INTEGER(y)[ yp-1 ];
          break;
        case REALSXP:
          REAL(result)[ i ] = REAL(y)[ yp-1 ];
          break;
        default:
          break;
      }
      yp++;
    }
  }
  }

  UNPROTECT(P);
  setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
  SEXP dim;
  PROTECT(dim = allocVector(INTSXP, 2));
  INTEGER(dim)[0] = len;
  INTEGER(dim)[1] = INTEGER(getAttrib(x, R_DimSymbol))[1];
  UNPROTECT(1);
  setAttrib(result, R_DimSymbol, dim);
  setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
  
  setAttrib(result, xts_IndexSymbol, newindex);
  setAttrib(result, xts_IndexClassSymbol, getAttrib(x, xts_IndexClassSymbol));
  setAttrib(result, xts_IndexFormatSymbol, getAttrib(x, xts_IndexFormatSymbol));
  setAttrib(result, xts_ClassSymbol, getAttrib(x, xts_ClassSymbol));
  copy_xtsAttributes(x, result);
  return result;

} 
