#
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


#' Check If A Vector Is Ordered
#' 
#' Check if a vector is strictly increasing, strictly decreasing, not
#' decreasing, or not increasing.
#' 
#' Designed for internal use with \pkg{xts}, this provides highly optimized
#' tests for ordering.
#' 
#' @param x A numeric vector.
#' @param increasing Test for increasing (`TRUE`) or decreasing (`FALSE`) values?
#' @param strictly When `TRUE`, vectors with duplicate values are *not*
#'   considered ordered.
#' 
#' @return A logical scalar indicating whether or not `x` is ordered.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`is.unsorted()`]
#' 
#' @keywords misc
#' @examples
#' 
#' # strictly increasing
#' isOrdered(1:10, increasing=TRUE)
#' isOrdered(1:10, increasing=FALSE)
#' isOrdered(c(1,1:10), increasing=TRUE)
#' isOrdered(c(1,1:10), increasing=TRUE, strictly=FALSE)
#' 
#' # decreasing
#' isOrdered(10:1, increasing=TRUE)
#' isOrdered(10:1, increasing=FALSE)
#' 
`isOrdered` <- function(x, increasing=TRUE, strictly=TRUE) {
  # x must be of type double or integer.  Checked in the C code.
  if(is.character(x))
    stop('character ordering unsupported')

  if(!is.numeric(x))
    x = as.numeric(x)

  .Call(C_do_is_ordered,
        x = x,
        increasing = as.logical(increasing),
        strictly   = as.logical(strictly))
}
