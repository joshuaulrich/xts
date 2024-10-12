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

SEXP xts_merge_make_colnames (SEXP colnames, SEXP suffixes, SEXP check_names, SEXP env)
{
  int p = 0;
  SEXP newcolnames = colnames;

  // add suffixes
  if (R_NilValue != suffixes) {

    SEXP args = PROTECT(allocList(3)); p++;
    SEXP vals = args;
    SETCAR(vals, newcolnames);      vals = CDR(vals);
    SETCAR(vals, suffixes);         vals = CDR(vals);
    SETCAR(vals, mkString(""));
    SET_TAG(vals, install("sep"));

    SEXP expr = PROTECT(LCONS(install("paste"), args)); p++;
    PROTECT(newcolnames = eval(expr, env)); p++;
  }

  // check that names are 'valid R names'
  if (LOGICAL(check_names)[0]) {

    SEXP args = PROTECT(allocList(2)); p++;
    SEXP vals = args;
    SETCAR(vals, newcolnames);        vals = CDR(vals);
    SETCAR(vals, ScalarLogical(1));
    SET_TAG(vals, install("unique"));

    SEXP expr = PROTECT(LCONS(install("make.names"), args)); p++;
    PROTECT(newcolnames = eval(expr, env)); p++;
  }

  UNPROTECT(p);
  return(newcolnames);
}

SEXP xts_merge_make_dimnames (SEXP _x, SEXP _y, int ncol_x, int ncol_y,
    SEXP _orig_colnames, SEXP _suffixes, SEXP _check_names, SEXP _env)
{
  int p = 0;
  int ncols = ncol_x + ncol_y;
  SEXP colnames = PROTECT(allocVector(STRSXP, ncols)); p++;

  SEXP dimnames_x = PROTECT(getAttrib(_x, R_DimNamesSymbol)); p++;
  SEXP dimnames_y = PROTECT(getAttrib(_y, R_DimNamesSymbol)); p++;

  // do 'x' and/or 'y' have column names?
  SEXP colnames_x = R_NilValue;
  SEXP colnames_y = R_NilValue;
  if (!isNull(dimnames_x) && !isNull(VECTOR_ELT(dimnames_x, 1))) {
    colnames_x = VECTOR_ELT(dimnames_x, 1);
  }
  if (!isNull(dimnames_y) && !isNull(VECTOR_ELT(dimnames_y, 1))) {
    colnames_y = VECTOR_ELT(dimnames_y, 1);
  }

  // time to combine the two
  for (int i = 0; i < ncols; i++) {
    if (i < ncol_x) {
      // copy column names from 'x'
      if (R_NilValue != colnames_x) {
        SET_STRING_ELT(colnames, i, STRING_ELT(colnames_x, i));
      } else {
        SET_STRING_ELT(colnames, i, STRING_ELT(_orig_colnames, i));
      }
    } else {
      // copy column names from 'y'
      if (R_NilValue != colnames_y) {
        SET_STRING_ELT(colnames, i, STRING_ELT(colnames_y, i - ncol_x));
      } else {
        SET_STRING_ELT(colnames, i, STRING_ELT(_orig_colnames, i));
      }
    }
  }

  colnames = PROTECT(xts_merge_make_colnames(colnames, _suffixes, _check_names, _env)); p++;

  SEXP dimnames = PROTECT(allocVector(VECSXP, 2)); p++;
  SET_VECTOR_ELT(dimnames, 0, R_NilValue);
  SET_VECTOR_ELT(dimnames, 1, colnames);

  UNPROTECT(p);
  return(dimnames);
}

/*
 * These macros allow _XTS_DO_MERGE_ to handle character xts objects as well
 * as all the other storage types.
 */
#define _XTS_SET_EQ_(_DES_, _DES_I_, _SRC_, _SRC_I_) \
    _DES_[_DES_I_] = _SRC_[_SRC_I_]

#define _XTS_SET_FN_(_DES_, _DES_I_, _SRC_, _SRC_I_) \
    SET_STRING_ELT(_DES_, _DES_I_, STRING_ELT(_SRC_, _SRC_I_))

/*
 * This is the core merge algorithm. It is implemented as a macro so it can be
 * called inside a switch() statement for each coredata storage type. Moving
 * the switch() statement outside the main loop improves performance by ~10%.
 *
 * This macro does (and should) not use any variables that aren't arguments or
 * defined within its scope. This isn't enforceable by the compiler, so all the
 * local variables have an underscore prefix and suffix to avoid referencing a
 * value outside of the macro's scope.
 */
#define _XTS_DO_MERGE_(_RESULT_, _X_, _Y_, _FILL_, _R_JOIN_, _L_JOIN_,         \
    _INDEX_, _X_INDEX_, _Y_INDEX_, _NROWS_, _X_NR_, _Y_NR_, _X_NC_, _Y_NC_)    \
int _xp_ = 1;                                                                  \
int _yp_ = 1;                                                                  \
int _i_, _j_, _ij_in_, _ij_out_;                                               \
                                                                               \
for (_i_ = 0; _i_ < _NROWS_; _i_++) {                                          \
  /* If we are past the last row in x, assign NA to merged data                \
     and copy the y column values to the second side of result                 \
  */                                                                           \
  if (_xp_ > _X_NR_) {                                                         \
    /* past the last row of x */                                               \
    if (_R_JOIN_) {                                                            \
      _INDEX_[_i_] = _Y_INDEX_[_yp_-1];                                        \
      /* we are out of x-values, so fill merged result with NAs */             \
      for (_j_ = 0; _j_ < _X_NC_; _j_++) { /* x-values */                      \
        _ij_out_ = _i_ + _j_ * _NROWS_;                                        \
        _XTS_SET_ELT_(_RESULT_, _ij_out_, _FILL_, 0);                          \
      }                                                                        \
      for (_j_ = 0; _j_ < _Y_NC_; _j_++) { /* y-values */                      \
        _ij_out_ = _i_ + (_j_+_X_NC_) * _NROWS_;                               \
        _ij_in_ = (_yp_-1) + _j_ * _Y_NR_;                                     \
        _XTS_SET_ELT_(_RESULT_, _ij_out_, _Y_, _ij_in_);                       \
      }                                                                        \
    } else {                                                                   \
      _i_--;  /* if all=FALSE, we must decrement i for each non-match */       \
    }                                                                          \
    _yp_++;                                                                    \
  } else if (_yp_ > _Y_NR_) {                                                  \
    /* past the last row of y */                                               \
    if (_L_JOIN_) {                                                            \
      _INDEX_[_i_] = _X_INDEX_[_xp_-1];                                        \
                                                                               \
      for (_j_ = 0; _j_ < _X_NC_; _j_++) { /* x-values */                      \
        _ij_out_ = _i_ + _j_ * _NROWS_;                                        \
        _ij_in_ = (_xp_-1) + _j_ * _X_NR_;                                     \
        _XTS_SET_ELT_(_RESULT_, _ij_out_, _X_, _ij_in_);                       \
      }                                                                        \
                                                                               \
      /* we are out of y-values, so fill merged result with NAs */             \
      for (_j_ = 0; _j_ < _Y_NC_; _j_++) { /* y-values */                      \
        _ij_out_ = _i_ + (_j_+_X_NC_) * _NROWS_;                               \
        _XTS_SET_ELT_(_RESULT_, _ij_out_, _FILL_, 0);                          \
      }                                                                        \
    } else {                                                                   \
      _i_--;  /* if all=FALSE, we must decrement i for each non-match */       \
    }                                                                          \
    _xp_++;                                                                    \
  } else if (_X_INDEX_[_xp_-1] == _Y_INDEX_[_yp_-1]) {                         \
    /* matching index values copy all column values from x and y to results */ \
    _INDEX_[_i_] = _X_INDEX_[_xp_-1];                                          \
                                                                               \
    for (_j_ = 0; _j_ < _X_NC_; _j_++) { /* x-values */                        \
      _ij_out_ = _i_ + _j_ * _NROWS_;                                          \
      _ij_in_ = (_xp_-1) + _j_ * _X_NR_;                                       \
      _XTS_SET_ELT_(_RESULT_, _ij_out_, _X_, _ij_in_);                         \
    }                                                                          \
    for (_j_ = 0; _j_ < _Y_NC_; _j_++) { /* y-values */                        \
      _ij_out_ = _i_ + (_j_+_X_NC_) * _NROWS_;                                 \
      _ij_in_ = (_yp_-1) + _j_ * _Y_NR_;                                       \
      _XTS_SET_ELT_(_RESULT_, _ij_out_, _Y_, _ij_in_);                         \
    }                                                                          \
    _xp_++;                                                                    \
    _yp_++;                                                                    \
  } else if (_X_INDEX_[_xp_-1] < _Y_INDEX_[_yp_-1]) {                          \
    if (_L_JOIN_) {                                                            \
      _INDEX_[_i_] = _X_INDEX_[_xp_-1];                                        \
                                                                               \
      for (_j_ = 0; _j_ < _X_NC_; _j_++) { /* x-values */                      \
        _ij_out_ = _i_ + _j_ * _NROWS_;                                        \
        _ij_in_ = (_xp_-1) + _j_ * _X_NR_;                                     \
        _XTS_SET_ELT_(_RESULT_, _ij_out_, _X_, _ij_in_);                       \
      }                                                                        \
      for (_j_ = 0; _j_ < _Y_NC_; _j_++) { /* y-values */                      \
        _ij_out_ = _i_ + (_j_+_X_NC_) * _NROWS_;                               \
        _XTS_SET_ELT_(_RESULT_, _ij_out_, _FILL_, 0);                          \
      }                                                                        \
    } else {                                                                   \
      _i_--;                                                                   \
    }                                                                          \
    _xp_++;                                                                    \
  } else if (_X_INDEX_[_xp_-1] > _Y_INDEX_[_yp_-1]) {                          \
    if (_R_JOIN_) {                                                            \
      _INDEX_[_i_] = _Y_INDEX_[_yp_-1];                                        \
                                                                               \
      for (_j_ = 0; _j_ < _X_NC_; _j_++) { /* x-values */                      \
        _ij_out_ = _i_ + _j_ * _NROWS_;                                        \
        _XTS_SET_ELT_(_RESULT_, _ij_out_, _FILL_, 0);                          \
      }                                                                        \
      for (_j_ = 0; _j_ < _Y_NC_; _j_++) { /* y-values */                      \
        _ij_out_ = _i_ + (_j_+_X_NC_) * _NROWS_;                               \
        _ij_in_ = (_yp_-1) + _j_ * _Y_NR_;                                     \
        _XTS_SET_ELT_(_RESULT_, _ij_out_, _Y_, _ij_in_);                       \
      }                                                                        \
    } else {                                                                   \
      _i_--;                                                                   \
    }                                                                          \
    _yp_++;                                                                    \
  }                                                                            \
}                                                                              \

/* 

  This is a merge_join algorithm used to
  allow two xts objects to be merged as one
  along a common index efficiently and fast

  The code is branched for REAL and INTEGER indexed values
  which allows for efficient memory usage and minimal
  testing/coercion

  
  Copyright Jeffrey A. Ryan 2008

*/
/* do_merge_xts {{{ */
SEXP do_merge_xts (SEXP x, SEXP y,
                   SEXP all,
                   SEXP fill,
                   SEXP retclass,
                   SEXP colnames,
                   SEXP suffixes,
                   SEXP retside,
                   SEXP check_names,
                   SEXP env,
                   SEXP coerce)
{
  int nrx, ncx, nry, ncy, len;
  int left_join, right_join;
  int p = 0;
  SEXP xindex, yindex, index, result, attr;

  int *int_index=NULL, *int_xindex=NULL, *int_yindex=NULL;
  double *real_index=NULL, *real_xindex=NULL, *real_yindex=NULL;

  /* Check whether the objects are NULL and treat NULL objects as
   * zero-width with an index that matches the non-null object
   */
  if (isNull(x) || isNull(y)) {
    if (!isNull(x)) {
      return(x);
    } else {
      return(y);
    }
  }

  PROTECT(xindex = getAttrib(x, xts_IndexSymbol)); p++;

  /* convert to xts object if needed */
  if (!asInteger(isXts(y))) {

    SEXP args = PROTECT(allocList(3)); p++;
    SEXP vals = args;
    SETCAR(vals, y);  vals = CDR(vals);

    SET_TAG(vals, install("length.out"));
    SETCAR(vals, ScalarInteger(length(xindex)));  vals = CDR(vals);

    SET_TAG(vals, install("error"));
    SETCAR(vals, install(".merge.xts.scalar"));

    SEXP expr = PROTECT(LCONS(install("try.xts"), args)); p++;
    PROTECT(y = eval(expr, env)); p++;
  }

  if (asInteger(isXts(y))) {
    PROTECT(yindex = getAttrib(y, xts_IndexSymbol)); p++;
  } else {
    PROTECT(yindex = getAttrib(x, xts_IndexSymbol)); p++;
  }

  if (TYPEOF(retside) != LGLSXP) {
    error("retside must be a logical value of TRUE or FALSE");
  }

  /* determine number of rows and columns to use for the inputs */
  int return_x_data = LOGICAL(retside)[0];
  int is_xobs_zero = LENGTH(x) == 0;
  int is_xdim_null = isNull(getAttrib(x, R_DimSymbol));

  nrx = nrows(x);
  ncx = ncols(x);

  if (return_x_data) {
    if (is_xdim_null) {
      if (is_xobs_zero) {
        nrx = LENGTH(xindex);
        ncx = 0;
        PROTECT(x = coerceVector(x, TYPEOF(y))); p++;
      }
    } else {
      if (is_xobs_zero) {
        nrx = LENGTH(xindex);
        ncx = INTEGER(getAttrib(x, R_DimSymbol))[1];
        PROTECT(x = coerceVector(x, TYPEOF(y))); p++;
      }
    }
  } else {
    nrx = LENGTH(xindex);
    ncx = 0;
  }

  int return_y_data = LOGICAL(retside)[1];
  int is_yobs_zero = LENGTH(y) == 0;
  int is_ydim_null = isNull(getAttrib(y, R_DimSymbol));

  nry = nrows(y);
  ncy = ncols(y);

  if (return_y_data) {
    if (is_ydim_null) {
      if (is_yobs_zero) {
        nry = LENGTH(yindex);
        ncy = 0;
        PROTECT(y = coerceVector(y, TYPEOF(x))); p++;
      }
    } else {
      if (is_yobs_zero) {
        nry = LENGTH(yindex);
        ncy = INTEGER(getAttrib(y, R_DimSymbol))[1];
        PROTECT(y = coerceVector(y, TYPEOF(x))); p++;
      }
    }
  } else {
    nry = LENGTH(yindex);
    ncy = 0;
  }

  /* do the inputs have any data to merge? */
  len = nrx + nry;
  if (len < 1 && ncx < 1 && ncy < 1) {

    /* return empty xts object if there are no rows or columns */
    PROTECT(result = allocVector(TYPEOF(x), 0)); p++;
    PROTECT(index  = allocVector(TYPEOF(xindex), 0)); p++;
    setAttrib(index, xts_IndexTzoneSymbol, getAttrib(xindex, xts_IndexTzoneSymbol));
    setAttrib(index, xts_IndexTclassSymbol, getAttrib(xindex, xts_IndexTclassSymbol));
    setAttrib(index, xts_IndexTformatSymbol, getAttrib(xindex, xts_IndexTformatSymbol));
    setAttrib(result, xts_IndexSymbol, index);

    if (LOGICAL(retclass)[0]) {
      setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
    }
    setAttrib(result, xts_ClassSymbol, getAttrib(x, xts_ClassSymbol));

    UNPROTECT(p);
    return result;
  }

  /* Ensure both indexes are REAL if they are not the same type. */
  if (TYPEOF(xindex) != TYPEOF(yindex)) {
    PROTECT(xindex = coerceVector(xindex, REALSXP)); p++;
    PROTECT(yindex = coerceVector(yindex, REALSXP)); p++;
  }
  int index_type = TYPEOF(xindex);

  if (TYPEOF(all) != LGLSXP) {
    error("all must be a logical value of TRUE or FALSE");
  }

  left_join = INTEGER(all)[0];
  right_join = INTEGER(all)[1];

  /* determine num_rows of final merged xts object
     
     this seems to only cost 1/1000 of a sec per
     1e6 observations.  Acceptable 'waste' given
     that now we can properly allocate space
     for our results

     We also check the index type and use the appropriate macros
   */
  int i = 0, xp = 1, yp = 1; /* x and y positions in index */
  if (index_type == REALSXP) {
    real_xindex = REAL(xindex);
    real_yindex = REAL(yindex);

    /* Check for illegal values before looping. Due to ordered index,
     * -Inf must be first, while NA, Inf, and NaN must be last. */
    int bad_x_index = nrx > 0 && (!R_FINITE(real_xindex[0]) || !R_FINITE(real_xindex[nrx-1]));
    int bad_y_index = nry > 0 && (!R_FINITE(real_yindex[0]) || !R_FINITE(real_yindex[nry-1]));
    if (bad_x_index || bad_y_index) {
      error("'index' cannot contain 'NA', 'NaN', or '+/-Inf'");
    }

    while ((xp + yp) <= (len + 1)) {
      if (xp > nrx) {
        yp++;
        if (right_join) i++;
      } else if (yp > nry) {
        xp++;
        if (left_join) i++;
      } else {
        double xi = real_xindex[xp-1];
        double yi = real_yindex[yp-1];
        if (xi == yi) {
          /* INNER JOIN -- only result if all = FALSE */
          yp++;
          xp++;
          i++;
        } else if (xi < yi) {
          /* LEFT JOIN */
          xp++;
          if (left_join) i++;
        } else if (xi > yi) {
          /* RIGHT JOIN */
          yp++;
          if (right_join) i++;
        } else {
          error("Invalid index element comparison (should never happen)");
        }
      }
    }
  } else if (index_type == INTSXP) {
    int_xindex = INTEGER(xindex);
    int_yindex = INTEGER(yindex);

    /* Check for NA before looping; logical ops on NA may yield surprising
     * results. Note that the NA_integer_ will appear in the last value of
     * the index because of sorting at the R level, even though NA_INTEGER
     * equals INT_MIN at the C level. */
    int bad_x_index = nrx > 0 && int_xindex[nrx-1] == NA_INTEGER;
    int bad_y_index = nry > 0 && int_yindex[nry-1] == NA_INTEGER;
    if (bad_x_index || bad_y_index) {
      error("'index' cannot contain 'NA'");
    }

    while ((xp + yp) <= (len + 1)) {
      if (xp > nrx) {
        yp++;
        if (right_join) i++;
      } else if (yp > nry) {
        xp++;
        if (left_join) i++;
      } else {
        int xi = int_xindex[xp-1];
        int yi = int_yindex[yp-1];
        if (xi == yi) {
          /* INNER JOIN -- only result if all = FALSE */
          yp++;
          xp++;
          i++;
        } else if (xi < yi) {
          /* LEFT JOIN */
          xp++;
          if (left_join) i++;
        } else if (xi > yi) {
          /* RIGHT JOIN */
          yp++;
          if (right_join) i++;
        } else {
          error("Invalid index element comparison (should never happen)");
        }
      }
    }
  } else {
      error("'index' must be either double or integer");
  }

  if (i == 0) {
    /* return a zero-length xts object if no rows match, consistent w/zoo */
    PROTECT(result = allocMatrix(TYPEOF(x), 0, ncx + ncy)); p++;
    PROTECT(index  = allocVector(TYPEOF(xindex), 0)); p++;
    /* set tclass, tzone, and tformat from x-index */
    setAttrib(index, xts_IndexTzoneSymbol, getAttrib(xindex, xts_IndexTzoneSymbol));
    setAttrib(index, xts_IndexTclassSymbol, getAttrib(xindex, xts_IndexTclassSymbol));
    setAttrib(index, xts_IndexTformatSymbol, getAttrib(xindex, xts_IndexTformatSymbol));
    SET_xtsIndex(result, index);

    /* dimnames */
    if (!isNull(colnames)) {
      /* only set DimNamesSymbol if passed colnames is not NULL */
      SEXP dimnames = PROTECT(xts_merge_make_dimnames(x, y, ncx, ncy, colnames, suffixes, check_names, env)); p++;
      setAttrib(result, R_DimNamesSymbol, dimnames);
    }

    if (LOGICAL(retclass)[0]) {
      setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
    }

    UNPROTECT(p);
    return result;
  }

  int num_rows = i;
  xp = 1; yp = 1;

  PROTECT(index  = allocVector(TYPEOF(xindex), num_rows)); p++;
  /* coercion/matching of TYPE for x and y needs to be checked,
     either here or in the calling R code.  I suspect here is
     more useful if other function can call the C code as well. 
     If objects are not the same type, convert to REALSXP. */
  if (asInteger(coerce) || TYPEOF(x) != TYPEOF(y)) {
    PROTECT(x = coerceVector(x, REALSXP)); p++;
    PROTECT(y = coerceVector(y, REALSXP)); p++;
  }
  PROTECT(result = allocVector(TYPEOF(x), (ncx + ncy) * num_rows)); p++;

  /* Ensure fill is the correct length and type */
  if (length(fill) < 1) {
    PROTECT(fill = ScalarLogical(NA_LOGICAL)); p++;
  }
  if (TYPEOF(fill) != TYPEOF(x)) {
    PROTECT(fill = coerceVector(fill, TYPEOF(x))); p++;
  }

  /* There are two type of supported index types, each branched from here */
  if (index_type == REALSXP) {
      real_index = REAL(index);

      switch (TYPEOF(x)) {
        case LGLSXP: {
            int *lgl_r = LOGICAL(result);
            int *lgl_x = LOGICAL(x);
            int *lgl_y = LOGICAL(y);
            int *lgl_fill = LOGICAL(fill);
            #define _XTS_SET_ELT_ _XTS_SET_EQ_
            _XTS_DO_MERGE_(lgl_r, lgl_x, lgl_y, lgl_fill, right_join, left_join,
                real_index, real_xindex, real_yindex, num_rows, nrx, nry, ncx, ncy);
            #undef _XTS_SET_ELT_
          }
          break;
        case INTSXP: {
            int *int_r = INTEGER(result);
            int *int_x = INTEGER(x);
            int *int_y = INTEGER(y);
            int *int_fill = INTEGER(fill);
            #define _XTS_SET_ELT_ _XTS_SET_EQ_
            _XTS_DO_MERGE_(int_r, int_x, int_y, int_fill, right_join, left_join,
                real_index, real_xindex, real_yindex, num_rows, nrx, nry, ncx, ncy);
            #undef _XTS_SET_ELT_
          }
          break;
        case REALSXP: {
            double *real_r = REAL(result);
            double *real_x = REAL(x);
            double *real_y = REAL(y);
            double *real_fill = REAL(fill);
            #define _XTS_SET_ELT_ _XTS_SET_EQ_
            _XTS_DO_MERGE_(real_r, real_x, real_y, real_fill, right_join, left_join,
                real_index, real_xindex, real_yindex, num_rows, nrx, nry, ncx, ncy);
            #undef _XTS_SET_ELT_
          }
          break;
        case CPLXSXP: {
            Rcomplex *cplx_r = COMPLEX(result);
            Rcomplex *cplx_x = COMPLEX(x);
            Rcomplex *cplx_y = COMPLEX(y);
            Rcomplex *cplx_fill = COMPLEX(fill);
            #define _XTS_SET_ELT_ _XTS_SET_EQ_
            _XTS_DO_MERGE_(cplx_r, cplx_x, cplx_y, cplx_fill, right_join, left_join,
                real_index, real_xindex, real_yindex, num_rows, nrx, nry, ncx, ncy);
            #undef _XTS_SET_ELT_
          }
          break;
        case STRSXP: {
            #define _XTS_SET_ELT_ _XTS_SET_FN_
            _XTS_DO_MERGE_(result, x, y, fill, right_join, left_join,
                real_index, real_xindex, real_yindex, num_rows, nrx, nry, ncx, ncy);
            #undef _XTS_SET_ELT_
          }
          break;
        default:
            error("unsupported data type");
          break;
      }
  } else if (index_type == INTSXP) {
      int_index = INTEGER(index);

      switch (TYPEOF(x)) {
        case LGLSXP: {
            int *lgl_r = LOGICAL(result);
            int *lgl_x = LOGICAL(x);
            int *lgl_y = LOGICAL(y);
            int *lgl_fill = LOGICAL(fill);
            #define _XTS_SET_ELT_ _XTS_SET_EQ_
            _XTS_DO_MERGE_(lgl_r, lgl_x, lgl_y, lgl_fill, right_join, left_join,
                int_index, int_xindex, int_yindex, num_rows, nrx, nry, ncx, ncy);
            #undef _XTS_SET_ELT_
          }
          break;
        case INTSXP: {
            int *int_r = INTEGER(result);
            int *int_x = INTEGER(x);
            int *int_y = INTEGER(y);
            int *int_fill = INTEGER(fill);
            #define _XTS_SET_ELT_ _XTS_SET_EQ_
            _XTS_DO_MERGE_(int_r, int_x, int_y, int_fill, right_join, left_join,
                int_index, int_xindex, int_yindex, num_rows, nrx, nry, ncx, ncy);
            #undef _XTS_SET_ELT_
          }
          break;
        case REALSXP: {
            double *real_r = REAL(result);
            double *real_x = REAL(x);
            double *real_y = REAL(y);
            double *real_fill = REAL(fill);
            #define _XTS_SET_ELT_ _XTS_SET_EQ_
            _XTS_DO_MERGE_(real_r, real_x, real_y, real_fill, right_join, left_join,
                int_index, int_xindex, int_yindex, num_rows, nrx, nry, ncx, ncy);
            #undef _XTS_SET_ELT_
          }
          break;
        case CPLXSXP: {
            Rcomplex *cplx_r = COMPLEX(result);
            Rcomplex *cplx_x = COMPLEX(x);
            Rcomplex *cplx_y = COMPLEX(y);
            Rcomplex *cplx_fill = COMPLEX(fill);
            #define _XTS_SET_ELT_ _XTS_SET_EQ_
            _XTS_DO_MERGE_(cplx_r, cplx_x, cplx_y, cplx_fill, right_join, left_join,
                int_index, int_xindex, int_yindex, num_rows, nrx, nry, ncx, ncy);
            #undef _XTS_SET_ELT_
          }
          break;
        case STRSXP: {
            #define _XTS_SET_ELT_ _XTS_SET_FN_
            _XTS_DO_MERGE_(result, x, y, fill, right_join, left_join,
                int_index, int_xindex, int_yindex, num_rows, nrx, nry, ncx, ncy);
            #undef _XTS_SET_ELT_
          }
          break;
        default:
            error("unsupported data type");
          break;
      }
  } else {
      error("'index' must be either double or integer");
  }

  /* following logic to allow for dimensionless xts objects (unsupported)
     to be used in Ops.xts calls This maps to how zoo behaves */
  if (return_x_data && !return_y_data && is_xdim_null) {
     setAttrib(result, R_DimSymbol, R_NilValue);
  } else
  if (return_y_data && !return_x_data && is_ydim_null) {
     setAttrib(result, R_DimSymbol, R_NilValue);
  } else /* set Dim and DimNames if there is at least 1 column */
  if ((ncx + ncy) > 0) {
    /* DIM */
    PROTECT(attr = allocVector(INTSXP, 2));
    INTEGER(attr)[0] = num_rows;
    INTEGER(attr)[1] = ncx + ncy;
    setAttrib(result, R_DimSymbol, attr);
    UNPROTECT(1);
    /* DIMNAMES */
    if (!isNull(colnames)) {
      /* only set DimNamesSymbol if passed colnames is not NULL */
      SEXP dimnames = PROTECT(xts_merge_make_dimnames(x, y, ncx, ncy, colnames, suffixes, check_names, env)); p++;
      setAttrib(result, R_DimNamesSymbol, dimnames);
    }
  } else {
    /* only used for zero-width results! xts always has dimension */
    setAttrib(result, R_DimSymbol, R_NilValue);
  }

  setAttrib(result, xts_IndexSymbol, index);
  if (LOGICAL(retclass)[0]) {
    setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
  }
  setAttrib(result, xts_ClassSymbol, getAttrib(x, xts_ClassSymbol));
  copy_xtsAttributes(x, result);

  UNPROTECT(p);
  return result;
} //}}}

//SEXP mergeXts (SEXP all, SEXP fill, SEXP retclass, SEXP colnames, SEXP retside, SEXP env, SEXP args)
/* called via .External("mergeXts", ...) */
SEXP mergeXts (SEXP args) // mergeXts {{{
{
  SEXP _x, _y, xtmp, result, _INDEX;
  /* colnames should be renamed as suffixes, as colnames need to be added at the C level */
  SEXP all, fill, retc, retclass, symnames,
       suffixes, rets, retside, env, tzone, check_names;
  int nr, nc, ncs=0;
  int index_len;
  int i, n=0, P=0;

  SEXP argstart;

  args = CDR(args); all = CAR(args);
  args = CDR(args); fill = CAR(args);
  args = CDR(args); retclass = CAR(args);
  args = CDR(args); symnames = CAR(args);
  args = CDR(args); suffixes = CAR(args);
  args = CDR(args); retside = CAR(args);
  args = CDR(args); env = CAR(args);
  args = CDR(args); tzone = CAR(args);
  args = CDR(args); check_names = CAR(args);
  args = CDR(args);
  /* args should now correspond to the ... objects we are looking to merge */
  argstart = args;  // use this to rewind list...

  n = 0;
  int type_of;
  SEXP coerce = PROTECT(ScalarInteger(0)); P++;

  if (args != R_NilValue) {
    type_of = TYPEOF(CAR(args));
  }

  /* number of columns in the output */
  while (args != R_NilValue) {

    ncs += xts_ncols(CAR(args));

    if (length(CAR(args)) > 0) {
      /* need to convert all objects if one non-zero-width needs to be converted */
      if (TYPEOF(CAR(args)) != type_of) {
        INTEGER(coerce)[0] = 1;
      }
    }
    args = CDR(args);
    n++;
  }

  /* build an index to be used in all subsequent calls */
  args = argstart;

  _x = CAR(args);
  args = CDR(args);

  int leading_non_xts = 0;
  while (!asInteger(isXts(_x))) {
    if (args == R_NilValue) {
      error("no xts object to merge");
    }
    leading_non_xts = 1;
    /*warning("leading non-xts objects may have been dropped");*/
    _x = CAR(args);
    args = CDR(args);
  }
  /* test for NULLs that may be present from cbind dispatch */
  if (!leading_non_xts) {
    /* leading non-xts in 2 case scenario was igoring non-xts value */
    if (n < 3 && (args == R_NilValue || (isNull(CAR(args)) && length(args) == 1))) {  /* no y arg or y==NULL */
      UNPROTECT(P);
      return(_x);
    }
  }

  if (args != R_NilValue) {
    _y = CAR(args);
    args = CDR(args);
  } else {
    PROTECT(_y = duplicate(_x)); P++;
  }

  if (n > 2 || leading_non_xts) {
    /* generalized n-case optimization
       currently if n>2 this is faster and more memory efficient
       than recursively building a merged object, object by object. */

    PROTECT(retc = allocVector(LGLSXP, 1)); P++;
    LOGICAL(retc)[0] = 1; /* return class == TRUE */
    PROTECT(rets = allocVector(LGLSXP, 2)); P++;
    LOGICAL(rets)[0] = 0; /* don't return left */
    LOGICAL(rets)[1] = 0; /* don't return right */

    if (isNull(_y)) {
      PROTECT(_y = duplicate(_x)); P++;
    }

    /* REPROTECT _INDEX in while loop */
    PROTECT_INDEX idx;
    PROTECT_WITH_INDEX(_INDEX = do_merge_xts(_x,
                                             _y,
                                             all,
                                             fill,
                                             retc,
                                             R_NilValue,
                                             R_NilValue,
                                             rets,
                                             check_names,
                                             env,
                                             coerce), &idx); P++;

    /* merge all objects into one zero-width common index */
    while (args != R_NilValue) {
      if (!isNull(CAR(args))) {
        REPROTECT(_INDEX = do_merge_xts(_INDEX,
                                        CAR(args),
                                        all,
                                        fill,
                                        retc,
                                        R_NilValue,
                                        R_NilValue,
                                        rets,
                                        check_names,
                                        env,
                                        coerce), idx);
      }
      args = CDR(args);
    }

    index_len = length(GET_xtsIndex(_INDEX));

    args = argstart; // reset args
    int ii, jj, iijj, jj_result;

    PROTECT(result = allocVector(TYPEOF(_INDEX), index_len * ncs)); P++;

    SEXP ColNames, NewColNames;
    PROTECT(NewColNames = allocVector(STRSXP, ncs)); P++;
    ncs = 0;
    /* REPROTECT xtmp inside for loop */
    PROTECT_INDEX idxtmp, cnmtmp;
    PROTECT_WITH_INDEX(xtmp = NULL, &idxtmp); P++;
    PROTECT_WITH_INDEX(ColNames = NULL, &cnmtmp); P++;

    /* merge each object with index */
    for (i = 0, nc = 0; args != R_NilValue; i = i+nc, args = CDR(args)) {
      /* i is object current being merged/copied
       * nc is offset in current object
       */
      if (isNull(CAR(args))) {
        i = i-nc;
        continue;  // if NULL is passed, skip to the next object.
      }

      REPROTECT(xtmp = do_merge_xts(_INDEX,
                                    CAR(args),
                                    all,
                                    fill,
                                    retclass,
                                    R_NilValue,
                                    R_NilValue,
                                    retside,
                                    check_names,
                                    env,
                                    coerce), idxtmp);

      nr = nrows(xtmp);
      nc = xts_ncols(xtmp);
      ncs += nc;

      /* Use colnames from merged object, if it has them. Otherwise, use
       * use deparsed names */
      REPROTECT(ColNames = getAttrib(CAR(args),R_DimNamesSymbol), cnmtmp);
      SEXP colnames = R_NilValue;
      if (R_NilValue != ColNames) {
        colnames = VECTOR_ELT(ColNames, 1);
      }
      if (R_NilValue == colnames) {
        for (jj = 0; jj < nc; jj++) {
          SET_STRING_ELT(NewColNames, i+jj, STRING_ELT(symnames,i+jj));
        }
      } else {
        for (jj = 0; jj < nc; jj++) {
          SET_STRING_ELT(NewColNames, i+jj, STRING_ELT(colnames,  jj));
        }
      }

      /* by type, insert merged data into result object */
      switch(TYPEOF(xtmp)) {
        case LGLSXP:
          {
            int *xtmp_ = LOGICAL(xtmp);
            int *result_ = LOGICAL(result);
            for (jj = 0; jj < nc; jj++) {
              for (ii = 0; ii < nr; ii++) {
                iijj = ii + jj * nr;
                jj_result = ii + (i+jj) * nr;
                result_[jj_result] = xtmp_[iijj];
              }
            }
          }
          break;
        case INTSXP:
          {
            int *xtmp_ = INTEGER(xtmp);
            int *result_ = INTEGER(result);
            for (jj = 0; jj < nc; jj++) {
              for (ii = 0; ii < nr; ii++) {
                iijj = ii + jj * nr;
                jj_result = ii + (i+jj) * nr;
                result_[jj_result] = xtmp_[iijj];
              }
            }
          }
          break;
        case REALSXP:
          {
            double *xtmp_ = REAL(xtmp);
            double *result_ = REAL(result);
            for (jj = 0; jj < nc; jj++) {
              for (ii = 0; ii < nr; ii++) {
                iijj = ii + jj * nr;
                jj_result = ii + (i+jj) * nr;
                result_[jj_result] = xtmp_[iijj];
              }
            }
          }
          break;
        case CPLXSXP:
          {
            Rcomplex *xtmp_ = COMPLEX(xtmp);
            Rcomplex *result_ = COMPLEX(result);
            for (jj = 0; jj < nc; jj++) {
              for (ii = 0; ii < nr; ii++) {
                iijj = ii + jj * nr;
                jj_result = ii + (i+jj) * nr;
                result_[jj_result] = xtmp_[iijj];
              }
            }
          }
          break;
        case STRSXP:
          {
            for (jj = 0; jj < nc; jj++) {
              for (ii = 0; ii < nr; ii++) {
                iijj = ii + jj * nr;
                jj_result = ii + (i+jj) * nr;
                SET_STRING_ELT(result, jj_result, STRING_ELT(xtmp, iijj));
              }
            }
          }
          break;
        default:
          error("unsupported data type");
          break;
      }
    }

    if (ncs > 0) {
      SEXP dim;
      PROTECT(dim = allocVector(INTSXP, 2)); P++;
      INTEGER(dim)[0] = index_len;
      INTEGER(dim)[1] = ncs;
      setAttrib(result, R_DimSymbol, dim);

      PROTECT(NewColNames = xts_merge_make_colnames(NewColNames, suffixes, check_names, env)); P++;

      SEXP dimnames = PROTECT(allocVector(VECSXP, 2)); P++;
      SET_VECTOR_ELT(dimnames, 0, R_NilValue);
      SET_VECTOR_ELT(dimnames, 1, NewColNames);

      setAttrib(result, R_DimNamesSymbol, dimnames);
    }

    SET_xtsIndex(result, GET_xtsIndex(_INDEX));
    copy_xtsCoreAttributes(_INDEX, result);
    copy_xtsAttributes(_INDEX, result);

  } else { /* 2-case optimization --- simply call main routine */
    /* likely bug in handling of merge(1, xts) case */
    PROTECT(result = do_merge_xts(_x,
                                  _y,
                                 all,
                                fill,
                            retclass,
                            symnames,
                            suffixes,
                             retside,
                         check_names,
                                 env,
                              coerce)); P++;
  }

  SEXP index_tmp = getAttrib(result, xts_IndexSymbol);
  PROTECT(index_tmp); P++;
  if (isNull(tzone)) {
    setAttrib(index_tmp, xts_IndexTzoneSymbol,
              getAttrib(getAttrib(_x,xts_IndexSymbol), xts_IndexTzoneSymbol));
  } else {
    setAttrib(index_tmp, xts_IndexTzoneSymbol, tzone);
  }
  copyMostAttrib(getAttrib(_x,xts_IndexSymbol), index_tmp);
  setAttrib(result, xts_IndexSymbol, index_tmp);

  UNPROTECT(P);
  return(result);
} //}}} end of mergeXts
