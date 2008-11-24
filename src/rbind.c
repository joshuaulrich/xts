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
  SEXP s, t;
  SEXP result, xindex, yindex, newindex;

  int *int_result, *int_x, *int_y;
  int *int_newindex, *int_xindex, *int_yindex;
  double *real_result, *real_x, *real_y;
  double *real_newindex, *real_xindex, *real_yindex;

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
    PROTECT(s = t = allocList(2)); P++;
    SET_TYPEOF(s, LANGSXP);
    SETCAR(t, install("try.xts")); t = CDR(t);
    SETCAR(t, x); t = CDR(t);
    PROTECT(x = eval(s, env)); P++;
  }
  if( !isXts(y) ) {
    PROTECT(s = t = allocList(2)); P++;
    SET_TYPEOF(s, LANGSXP);
    SETCAR(t, install("try.xts")); t = CDR(t);
    SETCAR(t, y); t = CDR(t);
    PROTECT(y = eval(s, env)); P++;
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

  if( TYPEOF(xindex) == REALSXP ) {  //FIXME
  real_newindex = REAL(newindex);
  real_xindex = REAL(xindex);
  real_yindex = REAL(yindex);
  for( i = 0; i < len; i++ ) {
    if( xp > nrx ) { 
      //REAL(newindex)[ i ] = REAL(yindex)[ yp-1 ];
      real_newindex[ i ] = real_yindex[ yp-1 ];
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
      //REAL(newindex)[ i ] = REAL(xindex)[ xp-1 ];
      real_newindex[ i ] = real_xindex[ xp-1 ];
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
    //if(REAL(xindex)[ xp-1 ] == REAL(yindex)[ yp-1 ]) {
    if( real_xindex[ xp-1 ] == real_yindex[ yp-1 ] ) {
      //REAL(newindex)[ i ]   = REAL(xindex)[ xp-1 ];
      //REAL(newindex)[ i+1 ] = REAL(yindex)[ yp-1 ];
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
    //if( REAL(xindex)[ xp-1 ]  < REAL(yindex)[ yp-1 ] ) {
    if( real_xindex[ xp-1 ] < real_yindex[ yp-1 ] ) {
      //REAL(newindex)[ i ] = REAL(xindex)[ xp-1 ];
      real_newindex[ i ] = real_xindex[ xp-1 ];
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
    //if( REAL(xindex)[ xp-1 ]  > REAL(yindex)[ yp-1 ] ) {
    if( real_xindex[ xp-1 ] > real_yindex[ yp-1 ] ) {
      //REAL(newindex)[ i ] = REAL(yindex)[ yp-1 ];
      real_newindex[ i ] = real_yindex[ yp-1 ];
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
