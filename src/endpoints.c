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
#include <stdint.h>

SEXP endpoints (SEXP _x, SEXP _on, SEXP _k, SEXP _addlast /* TRUE */)
{
  /*
      efficient implementation of:

        c(0,which(diff(_x%/%on%/%k+1) != 0),NROW(_x))
  */
  int *int_index = NULL;
  double *real_index = NULL;
  int i=1,j=1, nr, P=0;
  int int_tmp[2];
  int64_t int64_tmp[2];

  /* shouldn't force to INTSXP, as this now excludes
     microsecond and millisecond calculations  FIXME */
  int on = INTEGER(coerceVector(_on,INTSXP))[0];
  int k  = INTEGER(coerceVector(_k,INTSXP))[0];
  nr = nrows(_x);

  /* ensure k > 0 (bug #4920) */
  if(k <= 0) error("'k' must be > 0");

  /* endpoints objects. max nr+2 ( c(0,ep,nr) ) */
  SEXP _ep = PROTECT(allocVector(INTSXP,nr+2)); P++;
  int *ep = INTEGER(_ep);

  
  /*switch(TYPEOF(getAttrib(_x, install("index")))) {*/
  switch(TYPEOF(_x)) {
    case INTSXP:
      /* start i at second elem */
      /*int_index = INTEGER(getAttrib(_x, install("index")));*/
      int_index = INTEGER(_x);
      ep[0] = 0;
      /* special handling if index values < 1970-01-01 00:00:00 UTC */
      if(int_index[0] < 0) {
          int_tmp[1] = (int_index[0] + 1) / on / k;
          for(i=1,j=1; i<nr; i++) {
            int idx_i0 = int_index[i];

            // if index equals epoch
            int epoch_adj = (idx_i0 == 0);
            // cast truncates toward zero
            // (i.e. *forward* in time if index < 0)
            if(idx_i0 < 0) idx_i0++;

            int_tmp[0] = idx_i0 / on / k + epoch_adj;
            if( int_tmp[0] != int_tmp[1] ) {
              ep[j] = i;
              j++;
            }
            int_tmp[1] = int_tmp[0] - epoch_adj;
          }
      } else {
          int_tmp[1] = int_index[0] / on / k;
          for(i=1,j=1; i<nr; i++) {
            int_tmp[0] = int_index[i] / on / k;
            if( int_tmp[0] != int_tmp[1] ) {
              ep[j] = i;
              j++;
            }
            int_tmp[1] = int_tmp[0];
          }
      }
      break;
    case REALSXP:
      /*real_index = REAL(getAttrib(_x, install("index")));*/
      real_index = REAL(_x);
      ep[0] = 0;
      /* special handling if index values < 1970-01-01 00:00:00 UTC */
      if(real_index[0] < 0) {
          int64_tmp[1] = (int64_t)(real_index[0] + 1) / on / k;
          for(i=1,j=1; i<nr; i++) {
            double idx_i0 = real_index[i];

            // if index equals epoch
            int epoch_adj = (idx_i0 == 0);
            // cast truncates toward zero
            // (i.e. *forward* in time if index < 0)
            if(idx_i0 < 0) idx_i0 += 1.0;

            int64_tmp[0] = (int64_t)idx_i0 / on / k + epoch_adj;
            if( int64_tmp[0] != int64_tmp[1] ) {
              ep[j] = i;
              j++;
            }
            int64_tmp[1] = int64_tmp[0] - epoch_adj;
          }
      } else {
          int64_tmp[1] = (int64_t)real_index[0] / on / k;
          for(i=1,j=1; i<nr; i++) {
            int64_tmp[0] = (int64_t)real_index[i] / on / k;
            if( int64_tmp[0] != int64_tmp[1] ) {
              ep[j] = i;
              j++;
            }
            int64_tmp[1] = int64_tmp[0];
          }
      }
      break;
    default:
      error("unsupported 'x' type");
      break;
  }
  if(ep[j-1] != nr && asLogical(_addlast)) { /* protect against endpoint at NR */
/* Rprintf("ep[%i-1] != %i\n", j, nr);  */
    ep[j] = nr;
    j++;
  }
  PROTECT(_ep = lengthgets(_ep, j)); P++;
  UNPROTECT(P);
  return(_ep);
}
