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

/*
#define  xts_IndexSymbol        install("index")
#define  xts_ClassSymbol        install(".CLASS")
#define  xts_IndexFormatSymbol  install(".indexFORMAT")
#define  xts_IndexClassSymbol   install(".indexCLASS")
#define  xts_ATTRIB(x)          coerceVector(do_xtsAttributes(x),LISTSXP)
*/
SEXP do_xtsAttributes(SEXP x)
{
  SEXP a, values, names;
  int i=0, P=0;

  a = ATTRIB(x);
  if(length(a) <= 0)
    return R_NilValue;
  PROTECT(a); P++; /* all attributes */
  PROTECT(values = allocVector(VECSXP, length(a))); P++;
  PROTECT(names  = allocVector(STRSXP, length(a))); P++;

  /*PROTECT(a = getAttrib(ATTRIB(x),R_NamesSymbol));  *//* this gets all names */
  /*PROTECT(a = VECTOR_ELT(AS_LIST(ATTRIB(x)),1));    *//* this gets individual values */
  
  /*
   CAR gets the first element of the dotted pair list
   CDR gets the rest of the dotted pair list
   TAG gets the symbol/name of the first element of dotted pair list
  */
  for( /* a=ATTRIB(a) */; a != R_NilValue; a = CDR(a) ) {
    if(TAG(a) != xts_IndexSymbol &&
       TAG(a) != xts_ClassSymbol &&
       TAG(a) != xts_IndexFormatSymbol &&
       TAG(a) != xts_IndexClassSymbol &&
       TAG(a) != R_ClassSymbol &&
       TAG(a) != R_DimSymbol &&
       TAG(a) != R_DimNamesSymbol &&
       TAG(a) != R_NamesSymbol)
    {
      SET_VECTOR_ELT(values, i, CAR(a));
      SET_STRING_ELT(names,  i, PRINTNAME(TAG(a)));
      i++;
    }
  }
  if(i == 0) {
    UNPROTECT(P);
    return R_NilValue;
  }

  SET_LENGTH(values, i); /* truncate list back to i-size */
  SET_LENGTH(names,  i);
  setAttrib(values, R_NamesSymbol, names);
  UNPROTECT(P);
  return values;
}

SEXP do_xtsCoreAttributes(SEXP x)
{
  SEXP a, values, names;
  int i=0, P=0;

  a = ATTRIB(x);
  if(length(a) <= 0)
    return R_NilValue;
  PROTECT(a); P++; /* all attributes */
  PROTECT(values = allocVector(VECSXP, length(a))); P++;
  PROTECT(names  = allocVector(STRSXP, length(a))); P++;

  /*
  PROTECT(a = getAttrib(ATTRIB(x),R_NamesSymbol));
  PROTECT(a = VECTOR_ELT(AS_LIST(ATTRIB(x)),1)); 
  */

  /*
   CAR gets the first element of the dotted pair list
   CDR gets the rest of the dotted pair list
   TAG gets the symbol/name of the first element of dotted pair list
  */
  for( /* a=ATTRIB(a) */; a != R_NilValue; a = CDR(a) ) {
    if(TAG(a) == xts_ClassSymbol ||
       TAG(a) == xts_IndexFormatSymbol ||
       TAG(a) == xts_IndexClassSymbol ||
       TAG(a) == R_ClassSymbol)
    {
      SET_VECTOR_ELT(values, i, CAR(a));
      SET_STRING_ELT(names,  i, PRINTNAME(TAG(a)));
      i++;
    }
  }
  if(i == 0) {
    UNPROTECT(P);
    return R_NilValue;
  }

  SET_LENGTH(values, i); /* truncate list back to i-size */
  SET_LENGTH(names,  i);
  setAttrib(values, R_NamesSymbol, names);
  UNPROTECT(P);
  return values;
}

void copy_xtsAttributes(SEXP x, SEXP y)
{
  SEXP attr;
  int P=0;
  attr = xts_ATTRIB(x);

  if(length(attr) > 0 || y != R_NilValue) {
    PROTECT(attr); P++;
    for( ; attr != R_NilValue; attr = CDR(attr) ) {
      setAttrib(y, TAG(attr), CAR(attr));
    }
    UNPROTECT(P);
  }
}

void copy_xtsCoreAttributes(SEXP x, SEXP y)
{
  SEXP attr;
  int P=0;
  attr = xts_COREATTRIB(x);

  if(length(attr) > 0 || y != R_NilValue) {
    PROTECT(attr); P++;
    for( ; attr != R_NilValue; attr = CDR(attr) ) {
      setAttrib(y, TAG(attr), CAR(attr));
    }
    UNPROTECT(P);
  }
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
