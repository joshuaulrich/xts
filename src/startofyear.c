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

SEXP do_startofyear (SEXP _from, SEXP _to, SEXP _origin)
{
// do_startofyear {{{
  int i, P = 0;

  int from = INTEGER(_from)[0];
  int to = INTEGER(_to)[0];
  int origin = INTEGER(_origin)[0];

  // _fromto is a vector of length (from:to)
  SEXP _fromto = PROTECT(allocVector(INTSXP, to-from+1)); P++;
  int *fromto = INTEGER(_fromto);

  int nyear[1] = { (to - from + 1) };
  int leap[nyear[0]];


  // generate sequence of dates to work with
  fromto[0] = from;
  for(i=1; i < nyear[0]; i++) {
    fromto[i] = fromto[i-1] + 1;
  } 

  for(i = 0; i < nyear[0]; i++) {
    leap[ i ] = ( (fromto[ i ] % 4 == 0 && fromto[ i ] % 100 != 0)
                 ||
                   fromto[ i ] % 400 == 0) ? 1 : 0;
  }

  for(i=0; i < nyear[0]; i++) {
    if(leap[i] == 1) { // a leapyear (366 days)
      fromto[i] = 366;
    } else {           // a non-leapyear (365 days)
      fromto[i] = 365;
    }
  }
  
  /*
    fromto now has proper number of days per year

    now calculate the cumulative sum back from origin (negative)
    and from origin (positive)
  */

  int days_before_origin = origin - from;
  //int days_after_origin  = nyear[0] - days_before_origin - 1; //why is this here?

  int tmp=0;

  for(i = days_before_origin; i < nyear[0]; i++) {
    tmp += fromto[i];
    fromto[i] = tmp;
  }

  tmp = 0;
  for(i = days_before_origin-1; i >= 0; i--) {
    tmp -= fromto[i];
    fromto[i] = tmp;
  }

  /* now insert a 0 at the origin, by going backwards */

  for(i = nyear[0] - 1; i > days_before_origin; i--) 
    fromto[ i ] = fromto[ i-1 ];

  fromto[ days_before_origin ] = 0;

  UNPROTECT(P);
  return _fromto;

} //}}}
