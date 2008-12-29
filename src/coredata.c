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

SEXP coredata (SEXP x)
{
  SEXP result;
  int i, j, ncs, nrs;
  int P=0;
  PROTECT(result = allocVector(TYPEOF(x), length(x))); P++;
  switch(TYPEOF(x)) {
    case REALSXP:
      memcpy(REAL(result), REAL(x), length(result) * sizeof(double));
      break;
    case INTSXP:
      memcpy(INTEGER(result), INTEGER(x), length(result) * sizeof(int));
      break;
    case LGLSXP:
      memcpy(LOGICAL(result), LOGICAL(x), length(result) * sizeof(int));
      break;
    case CPLXSXP:
      memcpy(COMPLEX(result), COMPLEX(x), length(result) * sizeof(Rcomplex));
      break;
    case STRSXP:
      ncs = ncols(x); nrs = nrows(x);
      for(j=0; j< ncs; j++)
      for(i=0; i< nrs; i++)
        //Rprintf("i: %i, j: %i, i+j*nrs: %i\n", i, j, i+j*nrs);
        SET_STRING_ELT(result, i+j*nrs, STRING_ELT(x, i+j*nrs));
      break;
    default:
      error("currently unsupported data type");
      break;
  }
  if(!isNull(getAttrib(x, R_DimSymbol))) {
    SEXP dim, dimnames, newdimnames, colnames;
    PROTECT(dim = allocVector(INTSXP, 2)); P++;
    INTEGER(dim)[0] = nrows(x);
    INTEGER(dim)[1] = ncols(x);
    setAttrib(result, R_DimSymbol, dim);
  
    PROTECT(dimnames = getAttrib(x, R_DimNamesSymbol)); P++;
    if( !isNull(dimnames) ) {  // only column-names exist for xts objects
      PROTECT(newdimnames = allocVector(VECSXP, 2)); P++;
      PROTECT(colnames = allocVector(STRSXP, ncols(x))); P++;
      colnames = VECTOR_ELT(dimnames, 1);
      SET_VECTOR_ELT(dimnames, 0, R_NilValue); // row-names are forced back to NULL here
      SET_VECTOR_ELT(dimnames, 1, colnames); 
      setAttrib(result, R_DimNamesSymbol, dimnames);
    }
  } else {
    setAttrib(result, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
  }
  UNPROTECT(P);
  return result;
}
