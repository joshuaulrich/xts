#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "xts.h"

SEXP do_rbind_xts (SEXP x, SEXP y)
{
  int nrx, ncx, nry, ncy, len;
  int i, j, ij, ij_x, ij_y, xp=1, yp=1;
  int P=0; // PROTECT counter
  SEXP result, xindex, yindex, newindex;

  nrx = nrows(x);
  ncx = ncols(x);

  nry = nrows(y);
  ncy = ncols(y);

  len = nrx + nry;

  if(ncx != ncy)
    error("data must have same number of columns to bind by row");

  PROTECT(xindex = getAttrib(x, xts_IndexSymbol)); P++;
  PROTECT(yindex = getAttrib(y, xts_IndexSymbol)); P++;

  if( TYPEOF(xindex) != TYPEOF(yindex) ) 
  {
    PROTECT(xindex = coerceVector(xindex, REALSXP)); P++;
    PROTECT(yindex = coerceVector(yindex, REALSXP)); P++;
  }

  PROTECT(newindex = allocVector(TYPEOF(xindex), len)); P++;
  PROTECT(result   = allocVector(TYPEOF(x), len * ncx)); P++;

  /* need to convert different types of x and y if needed */
  if( TYPEOF(x) != TYPEOF(y) ) {
    PROTECT(x = coerceVector(x, REALSXP)); P++;
    PROTECT(y = coerceVector(y, REALSXP)); P++;
  } 

  if( TYPEOF(xindex) == REALSXP ) {  //FIXME
  for( i = 0; i < len; i++ ) {
    if( xp > nrx ) { 
      REAL(newindex)[ i ] = REAL(yindex)[ yp-1 ];
      for(j = 0; j < ncx; j++) {
        ij = i + j * len;
        ij_y = (yp-1) + j * nry;
        switch(TYPEOF(x)) {
          case LGLSXP:
            LOGICAL(result)[ ij ] = LOGICAL(y)[ ij_y ];
            break;
          case INTSXP:
            INTEGER(result)[ ij ] = INTEGER(y)[ ij_y ];
            break;
          case REALSXP:
            REAL(result)[ ij ] = REAL(y)[ ij_y ];
            break;
          case CPLXSXP:
            COMPLEX(result)[ ij ] = COMPLEX(y)[ ij_y ];
            break;
          case STRSXP:
            SET_STRING_ELT(result, ij, STRING_ELT(y, ij_y));
            break;
          default:
            break;
        }
      }
      yp++;
    } else
    if( yp > nry ) {
      REAL(newindex)[ i ] = REAL(xindex)[ xp-1 ];
      for(j = 0; j < ncx; j++) {
        ij = i + j * len;
        ij_x = (xp-1) + j * nrx;
        switch(TYPEOF(x)) {
          case LGLSXP:
            LOGICAL(result)[ ij ] = LOGICAL(x)[ ij_x ];
            break;
          case INTSXP:
            INTEGER(result)[ ij ] = INTEGER(x)[ ij_x ];
            break;
          case REALSXP:
            REAL(result)[ ij ] = REAL(x)[ ij_x ];
            break;
          case CPLXSXP:
            COMPLEX(result)[ ij ] = COMPLEX(x)[ ij_x ];
            break;
          case STRSXP:
            SET_STRING_ELT(result, ij, STRING_ELT(x, ij_x));
            break;
          default:
            break;
        }
      }
      xp++;
    } else
    if(REAL(xindex)[ xp-1 ] == REAL(yindex)[ yp-1 ]) {
      REAL(newindex)[ i ]   = REAL(xindex)[ xp-1 ];
      REAL(newindex)[ i+1 ] = REAL(yindex)[ yp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_x = (xp-1) + j * nrx;
      ij_y = (yp-1) + j * nry;
      switch(TYPEOF(x)) {
        case LGLSXP:
          LOGICAL(result)[ ij ] = LOGICAL(x)[ ij_x ];
          LOGICAL(result)[ ij+1 ] = LOGICAL(y)[ ij_y ];
          break;
        case INTSXP:
          INTEGER(result)[ ij ]     = INTEGER(x)[ ij_x ];
          INTEGER(result)[ ij+1 ]   = INTEGER(y)[ ij_y ];
          break;
        case REALSXP:
          REAL(result)[ ij ]     = REAL(x)[ ij_x ];
          REAL(result)[ ij+1 ]   = REAL(y)[ ij_y ];
          break;
        case CPLXSXP:
          COMPLEX(result)[ ij ] = COMPLEX(x)[ ij_x ];
          COMPLEX(result)[ ij+1 ] = COMPLEX(y)[ ij_y ];
          break;
        case STRSXP:
          SET_STRING_ELT(result, ij, STRING_ELT(x, ij_x));
          SET_STRING_ELT(result, ij+1, STRING_ELT(y, ij_y));
          break;
        default:
          break;
      }
      }
      yp++;
      xp++;
      i++;  // need to increase i as we now have filled in 2 values
    } else
    if( REAL(xindex)[ xp-1 ]  < REAL(yindex)[ yp-1 ] ) {
      REAL(newindex)[ i ] = REAL(xindex)[ xp-1 ];
      for(j = 0; j < ncx; j++) {
        ij = i + j * len;
        ij_x = (xp-1) + j * nrx;
        switch(TYPEOF(x)) {
          case LGLSXP:
            LOGICAL(result)[ ij ] = LOGICAL(x)[ ij_x ];
            break;
          case INTSXP:
            INTEGER(result)[ ij ] = INTEGER(x)[ ij_x ];
            break;
          case REALSXP:
            REAL(result)[ ij ] = REAL(x)[ ij_x ];
            break;
          case CPLXSXP:
            COMPLEX(result)[ ij ] = COMPLEX(x)[ ij_x ];
            break;
          case STRSXP:
            SET_STRING_ELT(result, ij, STRING_ELT(x, ij_x));
            break;
          default:
            break;
        }
      }
      xp++;
    } else
    if( REAL(xindex)[ xp-1 ]  > REAL(yindex)[ yp-1 ] ) {
      REAL(newindex)[ i ] = REAL(yindex)[ yp-1 ];
      for(j = 0; j < ncx; j++) {
        ij = i + j * len;
        ij_y = (yp-1) + j * nry;
        switch(TYPEOF(x)) {
          case LGLSXP:
            LOGICAL(result)[ ij ] = LOGICAL(y)[ ij_y ];
            break;
          case INTSXP:
            INTEGER(result)[ ij ] = INTEGER(y)[ ij_y ];
            break;
          case REALSXP:
            REAL(result)[ ij ] = REAL(y)[ ij_y ];
            break;
          case CPLXSXP:
            COMPLEX(result)[ ij ] = COMPLEX(y)[ ij_y ];
            break;
          case STRSXP:
            SET_STRING_ELT(result, ij, STRING_ELT(y, ij_y));
            break;
          default:
            break;
        }
      }
      yp++;
    }
  }
  } else
  if( TYPEOF(xindex) == INTSXP ) {
  for(i = 0; i < len; i++) {
    if( xp > nrx ) { 
      INTEGER(newindex)[ i ] = INTEGER(yindex)[ yp-1 ];
      for(j = 0; j < ncx; j++) {
        ij = i + j * len;
        ij_y = (yp-1) + j * nry;
        switch(TYPEOF(x)) {
          case LGLSXP:
            LOGICAL(result)[ ij ] = LOGICAL(y)[ ij_y ];
            break;
          case INTSXP:
            INTEGER(result)[ ij ] = INTEGER(y)[ ij_y ];
            break;
          case REALSXP:
            REAL(result)[ ij ] = REAL(y)[ ij_y ];
            break;
          case CPLXSXP:
            COMPLEX(result)[ ij ] = COMPLEX(y)[ ij_y ];
            break;
          case STRSXP:
            SET_STRING_ELT(result, ij, STRING_ELT(y, ij_y));
            break;
          default:
            break;
        }
      }
      yp++;
      
    } else
    if( yp > nry ) {
      INTEGER(newindex)[ i ] = INTEGER(xindex)[ xp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_x = (xp-1) + j * nrx;
      switch(TYPEOF(x)) {
        case LGLSXP:
          LOGICAL(result)[ ij ] = LOGICAL(x)[ ij_x ];
          break;
        case INTSXP:
          INTEGER(result)[ ij ] = INTEGER(x)[ ij_x ];
          break;
        case REALSXP:
          REAL(result)[ ij ] = REAL(x)[ ij_x ];
          break;
        case CPLXSXP:
          COMPLEX(result)[ ij ] = COMPLEX(x)[ ij_x ];
          break;
        case STRSXP:
          SET_STRING_ELT(result, ij, STRING_ELT(x, ij_x));
          break;
        default:
          break;
      }
      }
      xp++;
    } else
    if(INTEGER(xindex)[ xp-1 ] == INTEGER(yindex)[ yp-1 ]) {
      INTEGER(newindex)[ i ]   = INTEGER(xindex)[ xp-1 ];
      INTEGER(newindex)[ i+1 ] = INTEGER(yindex)[ yp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_x = (xp-1) + j * nrx;
      ij_y = (yp-1) + j * nry;
      switch(TYPEOF(x)) {
        case LGLSXP:
          LOGICAL(result)[ ij ]     = LOGICAL(x)[ ij_x ];
          LOGICAL(result)[ ij+1 ]   = LOGICAL(y)[ ij_y ];
          break;
        case INTSXP:
          INTEGER(result)[ ij ]     = INTEGER(x)[ ij_x ];
          INTEGER(result)[ ij+1 ]   = INTEGER(y)[ ij_y ];
          break;
        case REALSXP:
          REAL(result)[ ij ]     = REAL(x)[ ij_x ];
          REAL(result)[ ij+1 ]   = REAL(y)[ ij_y ];
          break;
        case CPLXSXP:
          COMPLEX(result)[ ij ]     = COMPLEX(x)[ ij_x ];
          COMPLEX(result)[ ij+1 ]   = COMPLEX(y)[ ij_y ];
          break;
        case STRSXP:
          SET_STRING_ELT(result, ij, STRING_ELT(x, ij_x));
          SET_STRING_ELT(result, ij+1, STRING_ELT(y, ij_y));
          break;
        default:
          break;
      }
      }
      yp++;
      xp++;
      i++;  // need to increase i as we now have filled in 2 values
    } else
    if( INTEGER(xindex)[ xp-1 ]  < INTEGER(yindex)[ yp-1 ] ) {
      INTEGER(newindex)[ i ] = INTEGER(xindex)[ xp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_x = (xp-1) + j * nrx;
      switch(TYPEOF(x)) {
        case LGLSXP:
          LOGICAL(result)[ ij ] = LOGICAL(x)[ ij_x ];
          break;
        case INTSXP:
          INTEGER(result)[ ij ] = INTEGER(x)[ ij_x ];
          break;
        case REALSXP:
          REAL(result)[ ij ] = REAL(x)[ ij_x ];
          break;
        case CPLXSXP:
          COMPLEX(result)[ ij ] = COMPLEX(x)[ ij_x ];
          break;
        case STRSXP:
          SET_STRING_ELT(result, ij, STRING_ELT(x, ij_x));
          break;
        default:
          break;
      }
      }
      xp++;
    } else
    if( INTEGER(xindex)[ xp-1 ]  > INTEGER(yindex)[ yp-1 ] ) {
      INTEGER(newindex)[ i ] = INTEGER(yindex)[ yp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_y = (yp-1) + j * nry;
      switch(TYPEOF(x)) {
        case LGLSXP:
          LOGICAL(result)[ ij ] = LOGICAL(y)[ ij_y ];
          break;
        case INTSXP:
          LOGICAL(result)[ ij ] = LOGICAL(y)[ ij_y ];
          break;
        case REALSXP:
          REAL(result)[ ij ] = REAL(y)[ ij_y ];
          break;
        case CPLXSXP:
          COMPLEX(result)[ ij ] = COMPLEX(y)[ ij_y ];
          break;
        case STRSXP:
          SET_STRING_ELT(result, ij, STRING_ELT(y, ij_y));
          break;
        default:
          break;
      }
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
