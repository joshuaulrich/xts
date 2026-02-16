/*
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
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

static SEXP xts_get_attrs(SEXP x)
{
    // use attributes() because ATTRIB() is not part of the public API
    SEXP call = PROTECT(lang2(install("attributes"), x));
    SEXP attrs = PROTECT(eval(call, R_GlobalEnv));
    UNPROTECT(2);
    return(attrs);
}

static void xts_copy_attrs(const SEXP from, SEXP to, const SEXP exclude)
{
    int P = 0;
    SEXP attrs = PROTECT(xts_get_attrs(from)); P++;
    SEXP names = PROTECT(getAttrib(attrs, R_NamesSymbol)); P++;

    int n = LENGTH(attrs);
    int m = LENGTH(exclude);

    /* reprotect name_symbol  inside for loop */
    SEXP name_symbol = R_NilValue;
    PROTECT_INDEX RP;
    PROTECT_WITH_INDEX(name_symbol, &RP); P++;

    for (int i = 0; i < n; i++) {
        REPROTECT(name_symbol = install(CHAR(STRING_ELT(names, i))), RP);

        int include = 1;
        for (int j = 0; j < m; j++) {
            if (name_symbol == VECTOR_ELT(exclude, j)) {
                include = 0;
                break;
            }
        }
        if (include) {
            setAttrib(to, name_symbol, VECTOR_ELT(attrs, i));
        }

    }
    UNPROTECT(P);
}

void copyAttributes(SEXP x, SEXP y)
{
    // similar to copyMostAttr, with index attribute removed

    // list of symbol names to exclude from 'x' attributes
    SEXP exclude = PROTECT(allocVector(VECSXP, 4));
    SET_VECTOR_ELT(exclude, 0, xts_IndexSymbol);
    SET_VECTOR_ELT(exclude, 1, R_DimSymbol);
    SET_VECTOR_ELT(exclude, 2, R_DimNamesSymbol);
    SET_VECTOR_ELT(exclude, 3, R_NamesSymbol);

    xts_copy_attrs(x, y, exclude);
    UNPROTECT(1);
}


void copy_xtsAttributes(SEXP x, SEXP y)
{
    // list of symbol names to exclude from 'x' attributes
    SEXP exclude = PROTECT(allocVector(VECSXP, 6));
    SET_VECTOR_ELT(exclude, 0, xts_IndexSymbol);
    SET_VECTOR_ELT(exclude, 1, xts_ClassSymbol);
    SET_VECTOR_ELT(exclude, 2, R_ClassSymbol);
    SET_VECTOR_ELT(exclude, 3, R_DimSymbol);
    SET_VECTOR_ELT(exclude, 4, R_DimNamesSymbol);
    SET_VECTOR_ELT(exclude, 5, R_NamesSymbol);

    xts_copy_attrs(x, y, exclude);
    UNPROTECT(1);
}

void copy_xtsCoreAttributes(SEXP x, SEXP y)
{
    // only copy xts_ClassSymbol or R_ClassSymbol if present
    SEXP attrs = PROTECT(xts_get_attrs(x));
    SEXP names = PROTECT(getAttrib(attrs, R_NamesSymbol));

    /* reprotect name_symbol  inside for loop */
    SEXP name_symbol = R_NilValue;
    PROTECT_INDEX RP;
    PROTECT_WITH_INDEX(name_symbol, &RP);

    int n = LENGTH(attrs);
    for (int i = 0; i < n; i++) {
        REPROTECT(name_symbol = install(CHAR(STRING_ELT(names, i))), RP);
        if (name_symbol == xts_ClassSymbol || name_symbol == R_ClassSymbol) {
            setAttrib(y, name_symbol, VECTOR_ELT(attrs, i));
        }
    }
    UNPROTECT(3);
}


SEXP ca (SEXP x, SEXP y)
{
  /* an example of internal copying of user-defined xts attributes
     this will be used inside of do_xts_subset and do_xts_merge  
     This particular usage is bad, as y is modified without copying
     resulting in all y and reference to y within R envir being altered
     the 'y' should only be the new value that is to be returned  */
  copy_xtsAttributes(x,y);
  return R_NilValue;
}

SEXP add_xtsCoreAttributes(SEXP _x, SEXP _index, SEXP _tzone,
        SEXP _tclass, SEXP _class, SEXP _tformat)
{
  int P=0;
  if(MAYBE_SHARED(_index)) {
    PROTECT(_index = duplicate(_index)); P++;
  }
  /* add tzone and tclass to index */
  setAttrib(_index, xts_IndexTclassSymbol, _tclass);
  setAttrib(_index, xts_IndexTzoneSymbol, _tzone);
  setAttrib(_index, xts_IndexTformatSymbol, _tformat);

  if(MAYBE_SHARED(_x)) {
    PROTECT(_x = duplicate(_x)); P++;
    //_x = duplicate(_x);
  }
  setAttrib(_x, xts_IndexSymbol, _index);              /* index */
  setAttrib(_x, R_ClassSymbol, _class);                /* class */

  UNPROTECT(P);
  return(_x);
}

