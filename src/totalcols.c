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
  int c, i=0, nc, P=0;

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
