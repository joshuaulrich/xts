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

SEXP rbind_append(SEXP, SEXP);

//SEXP do_rbind_xts (SEXP x, SEXP y, SEXP env) {{{
SEXP do_rbind_xts (SEXP x, SEXP y, SEXP dup)
{
  int nrx, ncx, nry, ncy, truelen, len;
  int no_duplicate = LOGICAL(dup)[0];
  int i, j, ij, ij_x, ij_y, xp=1, yp=1, add_y=0;
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

  truelen = len = nrx + nry;

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

#ifdef RBIND_APPEND
if(TYPEOF(xindex)==REALSXP) {
  if(REAL(xindex)[length(xindex)-1] < REAL(yindex)[0]) {
    UNPROTECT(P);
    return rbind_append(x,y);
    }
} else
if(TYPEOF(xindex)==INTSXP) {
  if(INTEGER(xindex)[length(xindex)-1] < INTEGER(yindex)[0]) {
    UNPROTECT(P);
    return rbind_append(x,y);
    }
}
#endif

  PROTECT(newindex = allocVector(TYPEOF(xindex), len)); P++;
  PROTECT(result   = allocVector(TYPEOF(x), len * ncx)); P++;

  copyMostAttrib(xindex, newindex);

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
  if( TYPEOF(xindex) == REALSXP ) {
    if(REAL(xindex)[nrx-1] < REAL(yindex)[0]) {
      memcpy(REAL(newindex), REAL(xindex), sizeof(double) * nrx);
      memcpy(REAL(newindex)+nrx, REAL(yindex), sizeof(double) * nry);
      switch(TYPEOF(x)) {
        case INTSXP:
          memcpy(INTEGER(result), INTEGER(x), sizeof(int) * (nrx*ncx));
          memcpy(INTEGER(result)+(nrx*ncx), INTEGER(y), sizeof(int) * (nry*ncy));
          break;
        case REALSXP:
          memcpy(REAL(result), REAL(x), sizeof(double) * (nrx*ncx));
          memcpy(REAL(result)+(nrx*ncx), REAL(y), sizeof(double) * (nry*ncy));
          break;
        default:
          break;
      }
UNPROTECT(P);
return(result);
    }
  } else {

  }
*/
  /*
  The main body of code to follow branches based on the type
  of index, removing the need to test at each position.
  */
  if( TYPEOF(xindex) == REALSXP ) {
  real_newindex = REAL(newindex);
  real_xindex = REAL(xindex);
  real_yindex = REAL(yindex);
  for( i = 0; i < len; i++ ) {
    if( i >= truelen ) {
      break;
    } else 
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
      if( real_xindex[ xp-1 ] < real_xindex[ xp   ] )
        add_y = 1;  /* add y values only if next xindex is new */
      if(no_duplicate) {
        add_y = 0;
        truelen--;
      }
      real_newindex[ i ] = real_xindex[ xp-1 ];
      if(add_y) real_newindex[ i+ 1 ] = real_yindex[ yp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_x = (xp-1) + j * nrx;
      ij_y = (yp-1) + j * nry;


      switch( mode ) {
        case LGLSXP:
          LOGICAL(result)[ ij ] = LOGICAL(x)[ ij_x ];
          if(add_y) LOGICAL(result)[ ij+1 ] = LOGICAL(y)[ ij_y ];
          break;
        case INTSXP:
          int_result[ ij ] = int_x[ ij_x ];
          if(add_y) int_result[ ij+1 ] = int_y[ ij_y ];
          break;
        case REALSXP:
          real_result[ ij ] = real_x[ ij_x ];
          if(add_y) real_result[ ij+1 ] = real_y[ ij_y ];
          break;
        case CPLXSXP:
          COMPLEX(result)[ ij ] = COMPLEX(x)[ ij_x ];
          if(add_y) COMPLEX(result)[ ij+1 ] = COMPLEX(y)[ ij_y ];
          break;
        case STRSXP:
          SET_STRING_ELT(result, ij, STRING_ELT(x, ij_x));
          if(add_y) SET_STRING_ELT(result, ij+1, STRING_ELT(y, ij_y));
          break;
        default:
          break;
      }
      }
      xp++;
      if(no_duplicate || add_y) { 
        yp++;
        if(!no_duplicate) i++;  // need to increase i as we now have filled in 2 values
        add_y = 0;
      }
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
    /*Rprintf("xp:%i, yp:%i, i:%i\n",xp,yp,i);*/
    if( i >= truelen ) {
      break;
    } else 
    if( xp > nrx ) { 
      int_newindex[ i ] = int_yindex[ yp-1 ];
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
      int_newindex[ i ] = int_xindex[ xp-1 ];
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
    if( int_xindex[ xp-1 ] == int_yindex[ yp-1 ] ) {
      if( int_xindex[ xp-1 ] < int_xindex[ xp  ] )
        add_y = 1;
      if(no_duplicate) {
        add_y = 0;
        truelen--;
      }
      int_newindex[ i ] = int_xindex[ xp-1 ];
      if(add_y) int_newindex[ i+1 ] = int_yindex[ yp-1 ];
      for(j = 0; j < ncx; j++) {
      ij = i + j * len;
      ij_x = (xp-1) + j * nrx;
      ij_y = (yp-1) + j * nry;
      switch( mode ) {
        case LGLSXP:
          LOGICAL(result)[ ij ]     = LOGICAL(x)[ ij_x ];
          if(add_y) LOGICAL(result)[ ij+1 ]   = LOGICAL(y)[ ij_y ];
          break;
        case INTSXP:
          int_result[ ij ] = int_x[ ij_x ];
          if(add_y) int_result[ ij+1 ] = int_y[ ij_y ];
          break;
        case REALSXP:
          real_result[ ij ] = real_x[ ij_x ];
          if(add_y) real_result[ ij+1 ] = real_y[ ij_y ];
          break;
        case CPLXSXP:
          COMPLEX(result)[ ij ]     = COMPLEX(x)[ ij_x ];
          if(add_y) COMPLEX(result)[ ij+1 ]   = COMPLEX(y)[ ij_y ];
          break;
        case STRSXP:
          SET_STRING_ELT(result, ij, STRING_ELT(x, ij_x));
          if(add_y) SET_STRING_ELT(result, ij+1, STRING_ELT(y, ij_y));
          break;
        default:
          break;
      }
      }
      xp++;
      if(no_duplicate || add_y) {
        yp++;
        if(!no_duplicate) i++;  // need to increase i as we now have filled in 2 values
        add_y = 0;
      }
    } else
    if( int_xindex[ xp-1 ] < int_yindex[ yp-1 ] ) {
      int_newindex[ i ] = int_xindex[ xp-1 ];
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
    if( int_xindex[ xp-1 ] > int_yindex[ yp-1 ] ) {
      int_newindex[ i ] = int_yindex[ yp-1 ];
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
    }}
  }

  if(truelen != len) {
    PROTECT(result = lengthgets(result, truelen * ncx)); P++;  /* reset length */
  }
  setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
  SEXP dim;
  PROTECT(dim = allocVector(INTSXP, 2));
  INTEGER(dim)[0] = truelen;
  INTEGER(dim)[1] = INTEGER(getAttrib(x, R_DimSymbol))[1];
  UNPROTECT(1);
  setAttrib(result, R_DimSymbol, dim);
  setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
  
  if(truelen != len) {
    PROTECT(newindex = lengthgets(newindex, truelen)); P++;
  }
  setAttrib(result, xts_IndexSymbol, newindex);
  setAttrib(result, xts_IndexClassSymbol, getAttrib(x, xts_IndexClassSymbol));
  setAttrib(result, xts_IndexTZSymbol, getAttrib(x, xts_IndexTZSymbol));
  setAttrib(result, xts_IndexFormatSymbol, getAttrib(x, xts_IndexFormatSymbol));
  setAttrib(result, xts_ClassSymbol, getAttrib(x, xts_ClassSymbol));
  copy_xtsAttributes(x, result);
  UNPROTECT(P);
  return result;
} //}}}

// SEXP rbindXts ( .External("rbindXts", ...) ) {{{
SEXP rbindXts (SEXP args)
{
  SEXP _x, _y;
  SEXP dup;
  int P=0;

  args = CDR(args); // 'rbindXts' call name
  PROTECT(dup = CAR(args)); P++;
  args = CDR(args);

  PROTECT(_x = CAR(args)); P++;
  args = CDR(args);

  if(args == R_NilValue) {
    UNPROTECT(P);
    return(_x);
  }

  PROTECT(_y = CAR(args)); P++;
  args = CDR(args);

  PROTECT(_x = do_rbind_xts(_x, _y, dup)); P++;
  while(args != R_NilValue) {
    PROTECT(_x = do_rbind_xts(_x, CAR(args), dup)); P++;
    args = CDR(args);
  }

  if(P > 0) UNPROTECT(P);
  return _x;
} //}}}


SEXP rbind_append (SEXP x, SEXP y) {
/*

  Provide fast row binding of xts objects if the
  left-hand object (binding target) has a last
  index value less than the right-hand object
  (object to bind).  This is an optimization to allow
  for real-time updating of objects without having to
  do much more than a memcpy of the two in coordinated
  fashion

*/
  /*Rprintf("rbind_append called\n");*/
  SEXP result;
  int nrs_x, nrs_y, ncs_x, ncs_y, nr;
  int i;

  ncs_x = ncols(x); ncs_y = ncols(y); nrs_x = nrows(x); nrs_y = nrows(y);

  if(ncs_x != ncs_y)
    error("objects must have the same number of columns"); /* FIXME */

  PROTECT(result = allocVector(TYPEOF(x), (nrs_x + nrs_y) * ncs_x));
  nr = nrs_x + nrs_y;

  switch(TYPEOF(x)) {
    case REALSXP:
      for(i=0; i< ncs_x; i++) {
        memcpy(&(REAL(result)[i*nr]), 
               &(REAL(x)[i*nrs_x]), 
               nrs_x*sizeof(double));
        memcpy(&(REAL(result)[i*nr + nrs_x]), 
               &(REAL(y)[i*nrs_y]), 
               nrs_y*sizeof(double));
      }
      break;
    case INTSXP:
      for(i=0; i< ncs_x; i++) {
        memcpy(&(INTEGER(result)[i*nr]), 
               &(INTEGER(x)[i*nrs_x]), 
               nrs_x*sizeof(int));
        memcpy(&(INTEGER(result)[i*nr + nrs_x]), 
               &(INTEGER(y)[i*nrs_y]), 
               nrs_y*sizeof(int));
      }
      break;
    case LGLSXP:
      for(i=0; i< ncs_x; i++) {
        memcpy(&(LOGICAL(result)[i*nr]), 
               &(LOGICAL(x)[i*nrs_x]), 
               nrs_x*sizeof(int));
        memcpy(&(LOGICAL(result)[i*nr + nrs_x]), 
               &(LOGICAL(y)[i*nrs_y]), 
               nrs_y*sizeof(int));
      }
      break;
    case CPLXSXP:
      for(i=0; i< ncs_x; i++) {
        memcpy(&(COMPLEX(result)[i*nr]), 
               &(COMPLEX(x)[i*nrs_x]), 
               nrs_x*sizeof(Rcomplex));
        memcpy(&(COMPLEX(result)[i*nr + nrs_x]), 
               &(COMPLEX(y)[i*nrs_y]), 
               nrs_y*sizeof(Rcomplex));
      }
      break;
    case RAWSXP:
      for(i=0; i< ncs_x; i++) {
        memcpy(&(RAW(result)[i*nr]), 
               &(RAW(x)[i*nrs_x]), 
               nrs_x*sizeof(Rbyte));
        memcpy(&(RAW(result)[i*nr + nrs_x]), 
               &(RAW(y)[i*nrs_y]), 
               nrs_y*sizeof(Rbyte));
      }
      break;
    case STRSXP:
      /* this requires an explicit loop like rbind.c and
         needs to be left with rbind.c
      */
      break;
    default:
      error("unsupported type");
  }

  copyAttributes(x, result); 

  SEXP index, xindex, yindex;
  xindex = getAttrib(x,install("index"));
  yindex = getAttrib(y,install("index"));
  int INDEXTYPE = TYPEOF(xindex);
  if(INDEXTYPE != NILSXP) {
    PROTECT(index = allocVector(INDEXTYPE, nr));
    if(INDEXTYPE==REALSXP) {
      memcpy(REAL(index), REAL(xindex), nrs_x * sizeof(double));
      memcpy(&(REAL(index)[nrs_x]), REAL(yindex), nrs_y * sizeof(double));
    } else
    if(INDEXTYPE==INTSXP) {
      memcpy(INTEGER(index), INTEGER(xindex), nrs_x * sizeof(int));
      memcpy(&(INTEGER(index)[nrs_x]), INTEGER(yindex), nrs_y * sizeof(int));
    }
    copyMostAttrib(xindex, index);
    setAttrib(result, install("index"), index);
    UNPROTECT(1);
  }

    SEXP dim;
    PROTECT(dim = allocVector(INTSXP, 2));
    INTEGER(dim)[0] = nr;
    INTEGER(dim)[1] = ncs_x; /* should be the same */
    setAttrib(result, R_DimSymbol, dim);
    UNPROTECT(1);

    setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
/*
    SEXP dimnames, currentnames, newnames;
    PROTECT(dimnames = allocVector(VECSXP, 2));
    PROTECT(newnames = allocVector(STRSXP, length(j)));
    currentnames = getAttrib(x, R_DimNamesSymbol);

    if(!isNull(currentnames)) {
      SET_VECTOR_ELT(dimnames, 0, R_NilValue);
      for(i=0; i<ncs_x; i++) {
        SET_STRING_ELT(newnames, i, STRING_ELT(VECTOR_ELT(currentnames,1), i));
      }
      SET_VECTOR_ELT(dimnames, 1, newnames);
      setAttrib(result, R_DimNamesSymbol, dimnames);
    }
    UNPROTECT(2);
*/

  UNPROTECT(1);
  return result;
}
