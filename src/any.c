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

SEXP any_negative (SEXP i_)
{
  int i;
  int len = length(i_);

  int *int_i=NULL;
  double *real_i=NULL;

  if(TYPEOF(i_)==INTSXP) {
    int_i = INTEGER(i_);
    for(i=0; i<len; i++) {
      if(int_i[i] >= 0)   
        continue;
      return ScalarLogical(1);
    }
  } else
  if(TYPEOF(i_)==REALSXP) {
    real_i = REAL(i_);
    for(i=0; i<len; i++) {
      if(real_i[i] >= 0)   
        continue;
      return ScalarLogical(1);
    }
  }
  return ScalarLogical(0);
}
