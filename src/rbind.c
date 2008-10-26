#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "xts.h"

SEXP do_rbind_xts (SEXP x, SEXP y)
//SEXP do_rbind_xts (SEXP args)
{
  int nrx, ncx, nry, ncy, len;
  int i, j, ij, ij_x, ij_y, xp=1, yp=1;
  int P=0; // PROTECT counter
  int mode;
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

  /* need to convert different types of x and y if needed */
  if( TYPEOF(x) != TYPEOF(y) ) {
    PROTECT(x = coerceVector(x, REALSXP)); P++;
    PROTECT(y = coerceVector(y, REALSXP)); P++;
  } 

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
          real_result[ ij ] - real_x[ ij_x ];
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
} 

//SEXP mergeXts (SEXP all, SEXP fill, SEXP retclass, SEXP colnames, SEXP retside, SEXP args)
SEXP rbindXts (SEXP args)
{
  SEXP _x, _y, xtmp, result, _INDEX;
  SEXP all, _all, fill, retc, retclass, colnames, rets, retside;
  int nr, nc, ncs=0, nrs=0;
  int index_len;
  int j, i, n=0, P=0;

  SEXP argstart;
  // args should now correspond to the ... objects we are looking to merge 
  argstart = args; // use this to rewind list...

  ncs = ncols(CAR(args));
  n = 1; // n: number of args passed in
  while(args != R_NilValue) {
    if(ncs != ncols(CAR(args)))
      error("number of columns in c/rbind must match");
    args = CDR(args);
    n++;
  }

  /* build an index to be used in all subsequent calls */
  args = argstart;
  PROTECT(_x = CAR(args)); P++;
  args = CDR(args);
  PROTECT(_y = CAR(args)); P++;
  args = CDR(args);

  if(args != R_NilValue) {
    /* generalized n-case optimization
       currently if n>2 this is faster and more memory efficient
       than recursively building a merged object, object by object. */
    PROTECT(_INDEX = do_rbind_xts(_x, _y)); P++;
    while(args != R_NilValue) { // merge all objects into one zero-width common index
      PROTECT(_INDEX = do_merge_xts(_INDEX, CAR(args), all, fill, retc, R_NilValue, rets)); P++;
//Rprintf("length of _INDEX: %i\n", length(GET_xtsIndex(_INDEX)));
      args = CDR(args);
      //i++;
    }
    index_len = length(GET_xtsIndex(_INDEX));
  
    args = argstart; // reset args
    int ii, jj, iijj, jj_result;
    int *int_result, *int_xtmp;
    double *real_result, *real_xtmp;

    PROTECT(result = allocVector(TYPEOF(_INDEX), index_len * ncs)); P++;
    switch(TYPEOF(result)) {
      case INTSXP:
        int_result = INTEGER(result);
        break;
      case REALSXP:
        real_result = REAL(result);
        break;
    }

    ncs = 0;
//    LOGICAL(_all)[0] = 1;
//    LOGICAL(_all)[1] = LOGICAL(all)[0];
    for(i = 0, nc=0; args != R_NilValue; i = i+nc, args = CDR(args)) { // merge each object with index
      xtmp = do_merge_xts(_INDEX, CAR(args), all, fill, retclass, /*colnames*/R_NilValue, retside);
      nc = ncols(xtmp);
      ncs += nc;
      nr = nrows(xtmp);
//Rprintf("i: %i, index_len: %i, nc: %i, nr: %i, ncs: %i\n", i, index_len, nc, nr, ncs);

      switch(TYPEOF(xtmp)) { // by type, insert merged data into result object
        case INTSXP:
          int_xtmp = INTEGER(xtmp);
          for(jj=0; jj < nc; jj++) {
            for(ii=0; ii < nr; ii++) {
              iijj = ii + jj * nr;
              //jj_result = iijj + (i * nr);
              jj_result = ii + ( (i+jj) * nr);
//Rprintf("jj_result: %i, newjj: %i, jj: %i, ii: %i\n", jj_result, ii + ((i+jj) * nr), jj, ii );
              int_result[ jj_result ] = int_xtmp[ iijj ];
            }
          }
          break;
        case REALSXP:
          real_xtmp = REAL(xtmp);
          for(jj=0; jj < nc; jj++) {
            for(ii=0; ii < nr; ii++) {
              iijj = ii + jj * nr;
              jj_result = ii + ( (i+jj) * nr);
              real_result[ jj_result ] = real_xtmp[ iijj ];
            }
          }
          break;
      }
    }

    SEXP dim;
    PROTECT(dim = allocVector(INTSXP, 2)); P++;
    INTEGER(dim)[0] = index_len;
    INTEGER(dim)[1] = ncs;
    setAttrib(result, R_DimSymbol, dim);
    SEXP dimnames;
    PROTECT(dimnames = allocVector(VECSXP, 2)); P++;
    SET_VECTOR_ELT(dimnames, 0, R_NilValue); // rownames are always NULL in xts
    SET_VECTOR_ELT(dimnames, 1, colnames);
    setAttrib(result, R_DimNamesSymbol, dimnames);


    SET_xtsIndex(result, GET_xtsIndex(_INDEX));
    copy_xtsCoreAttributes(_INDEX, result);
    copy_xtsAttributes(_INDEX, result);

  } else { /* 2-case optimization --- simply call main routine */
    PROTECT(result = do_rbind_xts(_x, _y)); P++;
  }

  if(P > 0) UNPROTECT(P); 
  return(result);
}
