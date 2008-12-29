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

SEXP number_of_cols (SEXP args)
{
  SEXP tcols;
  int P=0;

  args = CDR(args); // calling function name

  PROTECT(tcols = allocVector(INTSXP, length(args))); P++;
  int i=0;
  for(;args != R_NilValue; i++, args=CDR(args)) {
    if( TAG(args) == R_NilValue ) {
      if( length(CAR(args)) > 0) {
        INTEGER(tcols)[i] = ncols(CAR(args));
      } else INTEGER(tcols)[i] = (int)0;
    }
  }
  UNPROTECT(P);
  return tcols;
}

SEXP col_names (SEXP args)
{
  SEXP num_cols, sym_names, coln, dimnames, argstart;
  int c, i=0, P=0;

  argstart = args;
  PROTECT( num_cols = number_of_cols(args) ); P++;
  args = argstart;
  
  args = CDR(args); // call name
  PROTECT(sym_names = CAR(args)); P++; args = CDR(args);

  PROTECT(coln = allocVector(STRSXP, length(args))); P++;
  for(;args != R_NilValue; i++, args=CDR(args)) {
    if( length(CAR(args)) > 0) {
      PROTECT(dimnames = getAttrib(CAR(args), R_DimNamesSymbol)); P++;
      if( !isNull(dimnames) && !isNull(VECTOR_ELT(dimnames,2)) ) {
        for(c=0; c < INTEGER(num_cols)[i]; c++) {
          
        }
      } else {

      }
    }
  } 

  UNPROTECT(P);
  return coln;
}
