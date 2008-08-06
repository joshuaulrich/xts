/*
Code currently borrowed from main/src/subset.c to see how to create a function to subset
an xts object in it's entirety

All modification are by Jeffrey A. Ryan 2008
*/

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

// xtsExtractSubset {{{
static SEXP xtsExtractSubset(SEXP x, SEXP result, SEXP indx) //, SEXP call)
{
    int i, ii, n, nx, mode;
    SEXP tmp, tmp2;
    mode = TYPEOF(x);
    n = LENGTH(indx);
    nx = length(x);
    tmp = result;

    if (x == R_NilValue)
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
        LOGICAL(result)[i] = NA_INTEGER;
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
//      errorcall(call, R_MSG_ob_nonsub, type2char(mode));
        break;
    }
    }
    return result;
} //}}}

SEXP xtsSubset(SEXP x, SEXP sr, SEXP sc) //SEXP s, SEXP call, int drop)
{
    SEXP attr, result, dim;
    int nr, nc, nrs, ncs;
    int i, j, ii, jj, ij, iijj;

//Rprintf("Inside xtsSubset\n");

    nr = nrows(x);
    nc = ncols(x);

    dim = getAttrib(x, R_DimSymbol);

    nrs = LENGTH(sr);
    ncs = LENGTH(sc);
    result = allocVector(TYPEOF(x), nrs*ncs);
    PROTECT(result);

    /* code to handle index of xts object efficiently */
    SEXP index, newindex;
    int indx;

    index = getAttrib(x, install("index"));
    PROTECT(index);

    if(TYPEOF(index) == INTSXP) {
      newindex = allocVector(INTSXP, LENGTH(sr));
      PROTECT(newindex);
      for(indx = 0; indx < nrs; indx++) {
        INTEGER(newindex)[indx] = INTEGER(index)[INTEGER(sr)[indx]-1];
      }
      setAttrib(result, install("index"), newindex);
      UNPROTECT(1);
    }
    if(TYPEOF(index) == REALSXP) {
      newindex = allocVector(REALSXP, LENGTH(sr));
      PROTECT(newindex);
      for(indx = 0; indx < nrs; indx++) {
        REAL(newindex)[indx] = REAL(index)[INTEGER(sr)[indx]-1];
      }
      setAttrib(result, install("index"), newindex);
      UNPROTECT(1);
    }
/*
    if(TYPEOF(index) == REALSXP) {
      newindex = allocVector(REALSXP, LENGTH(sr));
      PROTECT(newindex);
    }
*/

    // Begin row loop, as .Call proceeds in row-major order?
    for (i = 0; i < nrs; i++) {
      ii = INTEGER(sr)[i];
      if (ii != NA_INTEGER) {
        if (ii < 1 || ii > nr)
          error("i is out of range\n");
        ii--;
      }
      //if(!isNull(index)) {
      //  REAL(newindex)[i] = REAL(index)[INTEGER(sr)[i]-1];
      //}
      // Begin column loop
      for (j = 0; j < ncs; j++) {
        jj = INTEGER(sc)[j];
        if (jj != NA_INTEGER) {
        if (jj < 1 || jj > nc)
          error("j is out of range\n");
        jj--;
        }
        ij = i + j * nrs;
        if (ii == NA_INTEGER || jj == NA_INTEGER) {
          switch (TYPEOF(x)) {
            case LGLSXP:
            case INTSXP:
                 INTEGER(result)[ij] = NA_INTEGER;
                 break;
            case REALSXP:
                 REAL(result)[ij] = NA_REAL;
                 break;
            case CPLXSXP:
                 COMPLEX(result)[ij].r = NA_REAL;
                 COMPLEX(result)[ij].i = NA_REAL;
                 break;
            case STRSXP:
                 SET_STRING_ELT(result, ij, NA_STRING);
                 break;
            case VECSXP:
                 SET_VECTOR_ELT(result, ij, R_NilValue);
                 break;
            case RAWSXP:
                 RAW(result)[ij] = (Rbyte) 0;
                 break;
            default:
                 error("matrix subscripting not handled for this type");
                 break;
          }
        }
        else {
          iijj = ii + jj * nr;
          switch (TYPEOF(x)) {
            case LGLSXP:
                 LOGICAL(result)[ij] = LOGICAL(x)[iijj];
                 break;
            case INTSXP:
                 INTEGER(result)[ij] = INTEGER(x)[iijj];
                 break;
            case REALSXP:
                 REAL(result)[ij] = REAL(x)[iijj];
                 break;
            case CPLXSXP:
                 COMPLEX(result)[ij] = COMPLEX(x)[iijj];
                 break;
            case STRSXP:
                 SET_STRING_ELT(result, ij, STRING_ELT(x, iijj));
                 break;
            case VECSXP:
                 SET_VECTOR_ELT(result, ij, VECTOR_ELT(x, iijj));
                 break;
            case RAWSXP:
                 RAW(result)[ij] = RAW(x)[iijj];
                 break;
            default:
                 error("matrix subscripting not handled for this type");
                 break;
          }
        }
      } // end of column loop
    } // end of row loop
    if(nrs >= 0 && ncs >= 0) {
    PROTECT(attr = allocVector(INTSXP, 2));
    INTEGER(attr)[0] = nrs;
    INTEGER(attr)[1] = ncs;
    setAttrib(result, R_DimSymbol, attr);
    UNPROTECT(1);
    }

    /* The matrix elements have been transferred.  Now we need to */
    /* transfer the attributes.  Most importantly, we need to subset */
    /* the dimnames of the returned value. */

    if (nrs >= 0 && ncs >= 0) {
    SEXP dimnames, dimnamesnames, newdimnames;
    dimnames = getAttrib(x, R_DimNamesSymbol);
    dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
    if (!isNull(dimnames)) {
        PROTECT(newdimnames = allocVector(VECSXP, 2));
        if (TYPEOF(dimnames) == VECSXP) {
          SET_VECTOR_ELT(newdimnames, 0,
            xtsExtractSubset(VECTOR_ELT(dimnames, 0),
                  allocVector(STRSXP, nrs), sr));
          SET_VECTOR_ELT(newdimnames, 1,
            xtsExtractSubset(VECTOR_ELT(dimnames, 1),
                  allocVector(STRSXP, ncs), sc));
        }
        else {
          SET_VECTOR_ELT(newdimnames, 0,
            xtsExtractSubset(CAR(dimnames),
                  allocVector(STRSXP, nrs), sr));
          SET_VECTOR_ELT(newdimnames, 1,
            xtsExtractSubset(CADR(dimnames),
                  allocVector(STRSXP, ncs), sc));
        }
        setAttrib(newdimnames, R_NamesSymbol, dimnamesnames);
        setAttrib(result, R_DimNamesSymbol, newdimnames);
        UNPROTECT(1);
    }
    }
    setAttrib(result, install("class"), getAttrib(x, install("class")));
    setAttrib(result, install(".indexCLASS"), getAttrib(x, install(".indexCLASS")));
    setAttrib(result, install(".indexFORMAT"), getAttrib(x, install(".indexFORMAT")));


    UNPROTECT(2);
    return result;
}
