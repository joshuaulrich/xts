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
#include <Rdefines.h>
#include <Rinternals.h>
#include "xts.h"

int firstNonNA (SEXP x)
{
  /*
    Internal use only; called by naCheck below.
  */

  int i=0, nr;
  int *int_x=NULL;
  double *real_x=NULL;

  nr = nrows(x);

  switch(TYPEOF(x)) {
    case INTSXP:
      int_x = INTEGER(x);
      for(i=0; i<nr; i++) {
        if(int_x[i]!=NA_INTEGER) {
          break;
        }
      }
      break;
    case REALSXP:
      real_x = REAL(x);
      for(i=0; i<nr; i++) {
        if(!ISNA(real_x[i])) {
          break;
        }
      }
      break;
    default:
      error("unsupported type");
      break;
  }
  return(i);
}

SEXP naCheck (SEXP x, SEXP check)
{
  /*
    Check for non-leading NA values, throw error if found
  */
  SEXP first;
  int _first;
  _first = firstNonNA(x);
  PROTECT(first = allocVector(INTSXP, 1));
  INTEGER(first)[0] = _first;


  if(LOGICAL(check)[0]) {
  /* check for NAs in rest of data */
  int i, nr;
  int *int_x = NULL;
  double *real_x = NULL;

  nr = nrows(x);
  switch(TYPEOF(x)) {
    case INTSXP:
      int_x = INTEGER(x);
      for(i=_first; i<nr; i++) {
        if(int_x[i] == NA_INTEGER) {
          error("Series contains non-leading NAs");  
        }
      }
      break;
    case REALSXP:
      real_x = REAL(x);
      for(i=_first; i<nr; i++) {
        if(ISNA(real_x[i])) {
          error("Series contains non-leading NAs");  
        }
      }
      break;
    default:
      error("unsupported type");
      break;
  }
  }
  UNPROTECT(1);
  return(first);
}

SEXP na_locf (SEXP x, SEXP fromLast)
{
  /* only works on univariate data */
  SEXP result;

  int i, nr, _first, P=0;
  _first = firstNonNA(x);

  if(_first == nrows(x))
    return(x);

  int *int_x=NULL, *int_result=NULL;
  double *real_x=NULL, *real_result=NULL;

  if(ncols(x) > 1)
    error("na.locf.xts only handles univariate, dimensioned data");

  nr = nrows(x);

  PROTECT(result = allocVector(TYPEOF(x), nrows(x))); P++;

  switch(TYPEOF(x)) {
    case INTSXP:
      int_x = INTEGER(x);
      int_result = INTEGER(result);
      if(!LOGICAL(fromLast)[0]) {
        /* copy leading NAs */
        for(i=0; i < (_first+1); i++) {
          int_result[i] = int_x[i];
        }
        /* result[_first] now has first value fromLast=FALSE */
        for(i=_first+1; i<nr; i++) {
          int_result[i] = int_x[i];
          if(int_result[i] == NA_INTEGER)
            int_result[i] = int_result[i-1];
        }
      } else {
        /* nr-2 is first position to fill fromLast=TRUE */
        int_result[nr-1] = int_x[nr-1];
        for(i=nr-2; i>=0; i--) {
          int_result[i] = int_x[i];
          if(int_result[i] == NA_INTEGER)
            int_result[i] = int_result[i+1];
        }
      }
      break;
    case REALSXP:
      real_x = REAL(x);
      real_result = REAL(result);
      if(!LOGICAL(fromLast)[0]) {   /* fromLast=FALSE */
        for(i=0; i < (_first+1); i++) {
          real_result[i] = real_x[i];
        }
        for(i=_first+1; i<nr; i++) {
          real_result[i] = real_x[i];
          if(ISNA(real_result[i]))
            real_result[i] = real_result[i-1];
        }
      } else {                      /* fromLast=TRUE */
        real_result[nr-1] = real_x[nr-1];
        for(i=nr-2; i>=0; i--) {
          real_result[i] = real_x[i];
          if(ISNA(real_result[i]))
            real_result[i] = real_result[i+1];
        }

      }
      break;
    default:
      error("unsupported type");
      break;
  }
  if(isXts(x)) {
    setAttrib(result, R_DimSymbol, getAttrib(x, R_DimSymbol));
    setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
    setAttrib(result, xts_IndexSymbol, getAttrib(x, xts_IndexSymbol));
    copy_xtsCoreAttributes(x, result);
    copy_xtsAttributes(x, result);
  }
  UNPROTECT(P);
  return(result);
}

SEXP na_omit_xts (SEXP x)
{
  SEXP na_index, not_na_index, col_index, result;

  int i, j, ij, nr, nc; 
  int not_NA, NA;

  nr = nrows(x);
  nc = ncols(x);
  not_NA = nr;
  
  int *int_x=NULL, *int_na_index=NULL, *int_not_na_index=NULL;
  double *real_x=NULL;

  switch(TYPEOF(x)) {
    case LGLSXP:
      for(i=0; i<nr; i++) {
        for(j=0; j<nc; j++) {
          ij = i + j*nr;
          if(LOGICAL(x)[ij] == NA_LOGICAL) {
            not_NA--;
            break;
          }   
        }   
      }
      break;
    case INTSXP:
      int_x = INTEGER(x);
      for(i=0; i<nr; i++) {
        for(j=0; j<nc; j++) {
          ij = i + j*nr;
          if(int_x[ij] == NA_INTEGER) {
            not_NA--;
            break;
          }   
        }   
      }
      break;
    case REALSXP:
      real_x = REAL(x);
      for(i=0; i<nr; i++) {
        for(j=0; j<nc; j++) {
          ij = i + j*nr;
          if(ISNA(real_x[ij])) {
            not_NA--;
            break;
          }   
        }   
      }
      break;
    default:
      error("unsupported type");
      break;
  }

  if(not_NA==0 || not_NA==nr)
    return(x);

  PROTECT(not_na_index = allocVector(INTSXP, not_NA));
  PROTECT(na_index = allocVector(INTSXP, nr-not_NA));

  /* pointers for efficiency as INTEGER in package code is a function call*/
  int_not_na_index = INTEGER(not_na_index);
  int_na_index = INTEGER(na_index);

  not_NA = NA = 0;
  switch(TYPEOF(x)) {
    case LGLSXP:
      for(i=0; i<nr; i++) {
        for(j=0; j<nc; j++) {
          ij = i + j*nr;
          if(LOGICAL(x)[ij] == NA_LOGICAL) {
            int_na_index[NA] = i+1;
            NA++;
            break;
          }
          if(j==(nc-1)) {
            /* make it to end of column, OK*/
            int_not_na_index[not_NA] = i+1;
            not_NA++;
          }   
        }   
      }
      break;
    case INTSXP:
      for(i=0; i<nr; i++) {
        for(j=0; j<nc; j++) {
          ij = i + j*nr;
          if(int_x[ij] == NA_INTEGER) {
            int_na_index[NA] = i+1;
            NA++;
            break;
          }
          if(j==(nc-1)) {
            /* make it to end of column, OK*/
            int_not_na_index[not_NA] = i+1;
            not_NA++;
          }   
        }   
      }
      break;
    case REALSXP:
      for(i=0; i<nr; i++) {
        for(j=0; j<nc; j++) {
          ij = i + j*nr;
          if(ISNA(real_x[ij])) {
            int_na_index[NA] = i+1;
            NA++;
            break;
          }
          if(j==(nc-1)) {
            /* make it to end of column, OK*/
            int_not_na_index[not_NA] = i+1;
            not_NA++;
          }   
        }   
      }
      break;
    default:
      error("unsupported type");
      break;
  }

  PROTECT(col_index = allocVector(INTSXP, nc));
  for(i=0; i<nc; i++)
    INTEGER(col_index)[i] = i+1;

  SEXP drop;
  drop = allocVector(LGLSXP, 1);
  LOGICAL(drop)[0] = 0;

  PROTECT(result = do_subset_xts(x, not_na_index, col_index, drop));

  SEXP class;
  PROTECT(class = allocVector(STRSXP, 1));
  SET_STRING_ELT(class, 0, mkChar("omit"));
  setAttrib(na_index, R_ClassSymbol, class);
  UNPROTECT(1);

  setAttrib(result, install("na.action"), na_index);
  UNPROTECT(4);

  return(result);
}

