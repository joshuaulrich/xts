#include <R.h>
#include <Rinternals.h>
#include "xts.h"

static SEXP ExtractSubset(SEXP x, SEXP result, SEXP indx) //, SEXP call)
{
    int i, ii, n, nx, mode;
    SEXP tmp, tmp2;
    mode = TYPEOF(x);
    n = LENGTH(indx); 
    nx = length(x);
    tmp = result;
    
    /*if (x == R_NilValue)*/
    if (isNull(x))
    return x;
    
    for (i = 0; i < n; i++) {
    ii = INTEGER(indx)[i];
    if (ii != NA_INTEGER)
        ii--;
    switch (mode) {
    case LGLSXP:
        if (0 <= ii && ii < nx && ii != NA_LOGICAL)
        LOGICAL(result)[i] = LOGICAL(x)[ii];
        else
        LOGICAL(result)[i] = NA_LOGICAL;
        break;
    case INTSXP:
        if (0 <= ii && ii < nx && ii != NA_INTEGER)
        INTEGER(result)[i] = INTEGER(x)[ii];
        else
        INTEGER(result)[i] = NA_INTEGER;
        break;
    case REALSXP:
        if (0 <= ii && ii < nx && ii != NA_INTEGER)
        REAL(result)[i] = REAL(x)[ii];
        else
        REAL(result)[i] = NA_REAL;
        break;
    case CPLXSXP:
        if (0 <= ii && ii < nx && ii != NA_INTEGER) {
        COMPLEX(result)[i] = COMPLEX(x)[ii];
        }
        else {
        COMPLEX(result)[i].r = NA_REAL;
        COMPLEX(result)[i].i = NA_REAL;
        }
        break;
    case STRSXP:
        if (0 <= ii && ii < nx && ii != NA_INTEGER)
        SET_STRING_ELT(result, i, STRING_ELT(x, ii));
        else
        SET_STRING_ELT(result, i, NA_STRING);
        break;
    case VECSXP:
    case EXPRSXP:
        if (0 <= ii && ii < nx && ii != NA_INTEGER)
        SET_VECTOR_ELT(result, i, VECTOR_ELT(x, ii));
        else
        SET_VECTOR_ELT(result, i, R_NilValue);
        break;
    case LISTSXP:
        /* cannot happen: pairlists are coerced to lists */
    case LANGSXP:
        if (0 <= ii && ii < nx && ii != NA_INTEGER) {
        tmp2 = nthcdr(x, ii);
        SETCAR(tmp, CAR(tmp2));
        SET_TAG(tmp, TAG(tmp2));
        }
        else
        SETCAR(tmp, R_NilValue);
        tmp = CDR(tmp);
        break;
    case RAWSXP:
        if (0 <= ii && ii < nx && ii != NA_INTEGER)
        RAW(result)[i] = RAW(x)[ii];
        else
        RAW(result)[i] = (Rbyte) 0;
        break;
    default:
        error("error in subset\n");
        break;
    }
    }
    return result;
}

SEXP _do_subset_xts (SEXP x, SEXP sr, SEXP sc, SEXP drop) {
  SEXP result;
  int i, j, nr, nc, nrs, ncs;
  int P=0;

  nrs = nrows(x);ncs = ncols(x);
  nr = length(sr); nc = length(sc);

  SEXP oindex, nindex;
  oindex = getAttrib(x, install("index"));
  PROTECT(nindex = allocVector(TYPEOF(oindex), nr)); P++;
  PROTECT(result = allocVector(TYPEOF(x), nr*nc)); P++;
  j = 0;

  double *real_nindex=NULL, *real_oindex, *real_result=NULL, *real_x=NULL; 
  int *int_nindex=NULL, *int_oindex, *int_result=NULL, *int_x=NULL; 
  int *int_sr=NULL, *int_sc=NULL;
  int_sr = INTEGER(sr);
  int_sc = INTEGER(sc);

  copyAttributes(x, result);

  if(TYPEOF(x)==LGLSXP) {
    int_x = LOGICAL(x); 
    int_result = LOGICAL(result); 
    if(TYPEOF(nindex)==INTSXP) {
      int_nindex = INTEGER(nindex);
      int_oindex = INTEGER(oindex);
      for(i=0; i<nr; i++) {
        if(int_sr[i] == NA_INTEGER)
          error("'i' contains NA");
        if(int_sr[i] > nrs || int_sc[j] > ncs)
          error("'i' or 'j' out of range");
        int_nindex[i] = int_oindex[int_sr[i]-1];
        int_result[i+j*nr] = int_x[int_sr[i]-1 + ((int_sc[j]-1) * nrs)];
      }
    } else
    if(TYPEOF(nindex)==REALSXP) {
      real_nindex = REAL(nindex);
      real_oindex = REAL(oindex);
      for(i=0; i<nr; i++) {
        real_nindex[i] = real_oindex[int_sr[i]-1];
        int_result[i+j*nr] = int_x[int_sr[i]-1 + ((int_sc[j]-1) * nrs)];
      }
    }
    copyAttributes(oindex, nindex);
    setAttrib(result, install("index"), nindex);
    for(j=1; j<nc; j++) {
      for(i=0; i<nr; i++) {
        int_result[i+j*nr] = int_x[int_sr[i]-1 + ((int_sc[j]-1) * nrs)];
      }
    }
  } else
  /* branch into INTSXP and REALSXP, as these are most
     common/important types for time series data */
  if(TYPEOF(x)==INTSXP) {
    int_x = INTEGER(x); 
    int_result = INTEGER(result); 
    if(TYPEOF(nindex)==INTSXP) {
      int_nindex = INTEGER(nindex);
      int_oindex = INTEGER(oindex);
      for(i=0; i<nr; i++) {
        if(int_sr[i] == NA_INTEGER)
          error("'i' contains NA");
        if(int_sr[i] > nrs || int_sc[j] > ncs)
          error("'i' or 'j' out of range");
        int_nindex[i] = int_oindex[int_sr[i]-1];
        int_result[i+j*nr] = int_x[int_sr[i]-1 + ((int_sc[j]-1) * nrs)];
      }
    } else
    if(TYPEOF(nindex)==REALSXP) {
      real_nindex = REAL(nindex);
      real_oindex = REAL(oindex);
      for(i=0; i<nr; i++) {
        real_nindex[i] = real_oindex[int_sr[i]-1];
        int_result[i+j*nr] = int_x[int_sr[i]-1 + ((int_sc[j]-1) * nrs)];
      }
    }
    copyAttributes(oindex, nindex);
    setAttrib(result, install("index"), nindex);
    for(j=1; j<nc; j++) {
      for(i=0; i<nr; i++) {
        int_result[i+j*nr] = int_x[int_sr[i]-1 + ((int_sc[j]-1) * nrs)];
      }
    }
  } else
  if(TYPEOF(x)==REALSXP) {
    real_x = REAL(x);
    real_result = REAL(result);
    if(TYPEOF(nindex)==INTSXP) {
      int_nindex = INTEGER(nindex);
      int_oindex = INTEGER(oindex);
      for(i=0; i<nr; i++) {
        if(int_sr[i] == NA_INTEGER)
          error("'i' contains NA");
        if(int_sr[i] > nrs || int_sc[j] > ncs)
          error("'i' or 'j' out of range");
        int_nindex[i] = int_oindex[int_sr[i]-1];
        real_result[i+j*nr] = real_x[int_sr[i]-1 + ((int_sc[j]-1) * nrs)];
      }
    } else
    if(TYPEOF(nindex)==REALSXP) {
      real_nindex = REAL(nindex);
      real_oindex = REAL(oindex);
      for(i=0; i<nr; i++) {
        real_nindex[i] = real_oindex[int_sr[i]-1];
        real_result[i+j*nr] = real_x[int_sr[i]-1 + ((int_sc[j]-1) * nrs)];
      }
    }
    copyAttributes(oindex, nindex);
    setAttrib(result, install("index"), nindex);
    for(j=1; j<nc; j++) {
      for(i=0; i<nr; i++) {
        real_result[i+j*nr] = real_x[int_sr[i]-1 + ((int_sc[j]-1) * nrs)];
      }
    }
  }


  if(nr >= 1 && nc >= 0) {
  SEXP dim;
  PROTECT(dim = allocVector(INTSXP,2));P++;
  INTEGER(dim)[0] = nr;
  INTEGER(dim)[1] = nc;
  setAttrib(result, R_DimSymbol, dim);

   if (nr >= 0 && nc >= 0) {
    SEXP dimnames, dimnamesnames, newdimnames;
    dimnames = getAttrib(x, R_DimNamesSymbol);
    dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
    if (!isNull(dimnames)) {
        PROTECT(newdimnames = allocVector(VECSXP, 2));
        if (TYPEOF(dimnames) == VECSXP) {
          SET_VECTOR_ELT(newdimnames, 0,
            ExtractSubset(VECTOR_ELT(dimnames, 0),
                  allocVector(STRSXP, nr), sr));
          SET_VECTOR_ELT(newdimnames, 1,
            ExtractSubset(VECTOR_ELT(dimnames, 1),
                  allocVector(STRSXP, nc), sc));
        }
        else {
          SET_VECTOR_ELT(newdimnames, 0,
            ExtractSubset(CAR(dimnames),
                  allocVector(STRSXP, nr), sr));
          SET_VECTOR_ELT(newdimnames, 1,
            ExtractSubset(CADR(dimnames),
                  allocVector(STRSXP, nc), sc));
        }
        setAttrib(newdimnames, R_NamesSymbol, dimnamesnames);
        setAttrib(result, R_DimNamesSymbol, newdimnames);
        UNPROTECT(1);
    }
    }

  }
  setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
  if(ncs == 1 && LOGICAL(drop)[0])
    setAttrib(result, R_DimSymbol, R_NilValue);


  UNPROTECT(P);
  return result;
}

