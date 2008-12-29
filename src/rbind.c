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


//SEXP do_rbind_xts (SEXP x, SEXP y, SEXP env) {{{
SEXP do_rbind_xts (SEXP x, SEXP y, SEXP env)
{
  int nrx, ncx, nry, ncy, len;
  int i, j, ij, ij_x, ij_y, xp=1, yp=1;
  int P=0; // PROTECT counter
  int mode;
  SEXP result, xindex, yindex, newindex;

  int *int_result=NULL, *int_x=NULL, *int_y=NULL;
  int *int_newindex=NULL, *int_xindex=NULL, *int_yindex=NULL;
  double *real_result=NULL, *real_x=NULL, *real_y=NULL;
  double *real_newindex=NULL, *real_xindex=NULL, *real_yindex=NULL;

  nrx = nrows(x);
  ncx = ncols(x);

  nry = nrows(y);
  ncy = ncols(y);

  len = nrx + nry;

  if( isNull(x) || isNull(y) ) {
    /* Handle NULL values by returning non-null object */
    if(!isNull(x)) return x;
    return y;
  }

  if( !isXts(x) ) {
    PROTECT( x = tryXts(x) ); P++;
  }
  if( !isXts(y) ) {
    PROTECT( y = tryXts(y) ); P++;
  }

  /* need to convert different types of x and y if needed */
  if( TYPEOF(x) != TYPEOF(y) ) {
    warning("mismatched types: converting objects to numeric");  // FIXME  not working!!!????
    PROTECT(x = coerceVector(x, REALSXP)); P++;
    PROTECT(y = coerceVector(y, REALSXP)); P++;
  } 


  mode = TYPEOF(x);

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

  switch( TYPEOF(x) ) {
    case INTSXP:
        int_x = INTEGER(x);
        int_y = INTEGER(y);
        int_result = INTEGER(result);
        break;
    case REALSXP:
        real_x = REAL(x);
        real_y = REAL(y);
        real_result = REAL(result);
        break;
    default:
        break;
  }

  /*
  The main body of code to follow branches based on the type
  of index, removing the need to test at each position.
  */
  if( TYPEOF(xindex) == REALSXP ) {
  real_newindex = REAL(newindex);
  real_xindex = REAL(xindex);
  real_yindex = REAL(yindex);
  for( i = 0; i < len; i++ ) {
    if( xp > nrx ) { 
      real_newindex[ i ] = real_yindex[ yp-1 ];
      for(j = 0; j < ncx; j++) {
        ij = i + j * len;
        ij_y = (yp-1) + j * nry;
        switch( mode ) {
          case LGLSXP:
            LOGICAL(result)[ ij ] = LOGICAL(y)[ ij_y ];
            break;
          case INTSXP:
            int_result[ ij ] = int_y[ ij_y ];
            break;
          case REALSXP:
            real_result[ ij ] = real_y[ ij_y ];
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
      real_newindex[ i ] = real_xindex[ xp-1 ];
      for(j = 0; j < ncx; j++) {
        ij = i + j * len;
        ij_x = (xp-1) + j * nrx;
        switch( mode ) {
          case LGLSXP:
            LOGICAL(result)[ ij ] = LOGICAL(x)[ ij_x ];
            break;
          case INTSXP:
            int_result[ ij ] = int_x[ ij_x ];
            break;
          case REALSXP:
            real_result[ ij ] = real_x[ ij_x ];
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
    if( real_xindex[ xp-1 ] == real_yindex[ yp-1 ] ) {
      real_newindex[ i ] = real_xindex[ xp-1 ];
      real_newindex[ i+ 1 ] = real_yindex[ yp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_x = (xp-1) + j * nrx;
      ij_y = (yp-1) + j * nry;
      switch( mode ) {
        case LGLSXP:
          LOGICAL(result)[ ij ] = LOGICAL(x)[ ij_x ];
          LOGICAL(result)[ ij+1 ] = LOGICAL(y)[ ij_y ];
          break;
        case INTSXP:
          int_result[ ij ] = int_x[ ij_x ];
          int_result[ ij+1 ] = int_y[ ij_y ];
          break;
        case REALSXP:
          real_result[ ij ] = real_x[ ij_x ];
          real_result[ ij+1 ] = real_y[ ij_y ];
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
    if( real_xindex[ xp-1 ] < real_yindex[ yp-1 ] ) {
      real_newindex[ i ] = real_xindex[ xp-1 ];
      for(j = 0; j < ncx; j++) {
        ij = i + j * len;
        ij_x = (xp-1) + j * nrx;
        switch( mode ) {
          case LGLSXP:
            LOGICAL(result)[ ij ] = LOGICAL(x)[ ij_x ];
            break;
          case INTSXP:
            int_result[ ij ] = int_x[ ij_x ];
            break;
          case REALSXP:
            real_result[ ij ] = real_x[ ij_x ];
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
    if( real_xindex[ xp-1 ] > real_yindex[ yp-1 ] ) {
      real_newindex[ i ] = real_yindex[ yp-1 ];
      for(j = 0; j < ncx; j++) {
        ij = i + j * len;
        ij_y = (yp-1) + j * nry;
        switch( mode ) {
          case LGLSXP:
            LOGICAL(result)[ ij ] = LOGICAL(y)[ ij_y ];
            break;
          case INTSXP:
            int_result[ ij ] = int_y[ ij_y ];
            break;
          case REALSXP:
            real_result[ ij ] = real_y[ ij_y ];
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
  int_newindex = INTEGER(newindex);
  int_xindex = INTEGER(xindex);
  int_yindex = INTEGER(yindex);
  for(i = 0; i < len; i++) {
    if( xp > nrx ) { 
      //INTEGER(newindex)[ i ] = INTEGER(yindex)[ yp-1 ];
      int_newindex[ i ] = int_yindex[ yp-1 ];
      for(j = 0; j < ncx; j++) {
        ij = i + j * len;
        ij_y = (yp-1) + j * nry;
        switch( mode ) {
          case LGLSXP:
            LOGICAL(result)[ ij ] = LOGICAL(y)[ ij_y ];
            break;
          case INTSXP:
            //INTEGER(result)[ ij ] = INTEGER(y)[ ij_y ];
            int_result[ ij ] = int_y[ ij_y ];
            break;
          case REALSXP:
            //REAL(result)[ ij ] = REAL(y)[ ij_y ];
            real_result[ ij ] = real_y[ ij_y ];
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
      //INTEGER(newindex)[ i ] = INTEGER(xindex)[ xp-1 ];
      int_newindex[ i ] = int_xindex[ xp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_x = (xp-1) + j * nrx;
      switch( mode ) {
        case LGLSXP:
          LOGICAL(result)[ ij ] = LOGICAL(x)[ ij_x ];
          break;
        case INTSXP:
          //INTEGER(result)[ ij ] = INTEGER(x)[ ij_x ];
          int_result[ ij ] = int_x[ ij_x ];
          break;
        case REALSXP:
          //REAL(result)[ ij ] = REAL(x)[ ij_x ];
          real_result[ ij ] = real_x[ ij_x ];
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
    //if(INTEGER(xindex)[ xp-1 ] == INTEGER(yindex)[ yp-1 ]) {
    if( int_xindex[ xp-1 ] == int_yindex[ yp-1 ] ) {
      //INTEGER(newindex)[ i ]   = INTEGER(xindex)[ xp-1 ];
      //INTEGER(newindex)[ i+1 ] = INTEGER(yindex)[ yp-1 ];
      int_newindex[ i ] = int_xindex[ xp-1 ];
      int_newindex[ i+1 ] = int_yindex[ yp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_x = (xp-1) + j * nrx;
      ij_y = (yp-1) + j * nry;
      switch( mode ) {
        case LGLSXP:
          LOGICAL(result)[ ij ]     = LOGICAL(x)[ ij_x ];
          LOGICAL(result)[ ij+1 ]   = LOGICAL(y)[ ij_y ];
          break;
        case INTSXP:
          //INTEGER(result)[ ij ]     = INTEGER(x)[ ij_x ];
          //INTEGER(result)[ ij+1 ]   = INTEGER(y)[ ij_y ];
          int_result[ ij ] = int_x[ ij_x ];
          int_result[ ij+1 ] = int_y[ ij_y ];
          break;
        case REALSXP:
          //REAL(result)[ ij ]     = REAL(x)[ ij_x ];
          //REAL(result)[ ij+1 ]   = REAL(y)[ ij_y ];
          real_result[ ij ] = real_x[ ij_x ];
          real_result[ ij+1 ] = real_y[ ij_y ];
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
    //if( INTEGER(xindex)[ xp-1 ]  < INTEGER(yindex)[ yp-1 ] ) {
    if( int_xindex[ xp-1 ] < int_yindex[ yp-1 ] ) {
      //INTEGER(newindex)[ i ] = INTEGER(xindex)[ xp-1 ];
      int_newindex[ i ] = int_xindex[ xp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_x = (xp-1) + j * nrx;
      switch( mode ) {
        case LGLSXP:
          LOGICAL(result)[ ij ] = LOGICAL(x)[ ij_x ];
          break;
        case INTSXP:
          //INTEGER(result)[ ij ] = INTEGER(x)[ ij_x ];
          int_result[ ij ] = int_x[ ij_x ];
          break;
        case REALSXP:
          //REAL(result)[ ij ] = REAL(x)[ ij_x ];
          real_result[ ij ] = real_x[ ij_x ];
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
    //if( INTEGER(xindex)[ xp-1 ]  > INTEGER(yindex)[ yp-1 ] ) {
    if( int_xindex[ xp-1 ] > int_yindex[ yp-1 ] ) {
      //INTEGER(newindex)[ i ] = INTEGER(yindex)[ yp-1 ];
      int_newindex[ i ] = int_yindex[ yp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_y = (yp-1) + j * nry;
      switch( mode ) {
        case LGLSXP:
          LOGICAL(result)[ ij ] = LOGICAL(y)[ ij_y ];
          break;
        case INTSXP:
          //LOGICAL(result)[ ij ] = LOGICAL(y)[ ij_y ];
          int_result[ ij ] = int_y[ ij_y ];
          break;
        case REALSXP:
          //REAL(result)[ ij ] = REAL(y)[ ij_y ];
          real_result[ ij ] = real_y[ ij_y ];
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
} //}}}

// SEXP rbindXts ( .External("rbindXts", ...) ) {{{
SEXP rbindXts (SEXP args)
{
  SEXP _x, _y;
  SEXP env;
  int P=0;

  args = CDR(args); // 'rbindXts' call name
  PROTECT(env = CAR(args)); P++;  // env
  args = CDR(args);  

  PROTECT(_x = CAR(args)); P++;
  args = CDR(args);

  if(args == R_NilValue) {
    UNPROTECT(P);
    return(_x);
  }

  PROTECT(_y = CAR(args)); P++;
  args = CDR(args);

  PROTECT(_x = do_rbind_xts(_x, _y, env)); P++;
  while(args != R_NilValue) {
    PROTECT(_x = do_rbind_xts(_x, CAR(args), env)); P++;
    args = CDR(args);
  }

/*
  if( args == R_NilValue ) {
    PROTECT(result = do_rbind_xts(_x, _y, env)); P++;
  } else {
    result = R_NilValue;
  }
*/
  if(P > 0) UNPROTECT(P);
  return _x;
} //}}}
