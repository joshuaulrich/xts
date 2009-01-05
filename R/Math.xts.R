#
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

#   This code adapted from Ops.zoo.R

cumsum.xts <- function(x)
{
  if( NCOL(x) == 1 ) {
    x[] <- cumsum(coredata(x))
  } else x[] <- apply(coredata(x), 2, function(y) cumsum(y))
  x
}

cumprod.xts <- function(x)
{
  if( NCOL(x) == 1 ) {
    x[] <- cumprod(coredata(x))
  } else x[] <- apply(coredata(x), 2, function(y) cumprod(y))
  x
}

cummin.xts <- function(x)
{
  if( NCOL(x) == 1 ) {
    x[] <- cummin(coredata(x))
  } else x[] <- apply(coredata(x), 2, function(y) cummin(y))
  x
}

cummax.xts <- function(x)
{
  if( NCOL(x) == 1 ) {
    x[] <- cummax(coredata(x))
  } else x[] <- apply(coredata(x), 2, function(y) cummax(y))
  x
}
