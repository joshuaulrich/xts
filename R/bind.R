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


#' @rdname rbind.xts
`c.xts` <-
function(...) {
  .External(C_rbindXts, dup=FALSE, ...)
}


#' Concatenate Two or More xts Objects by Row
#' 
#' Concatenate or bind by row two or more xts objects along a time-based index.
#' All objects must have the same number of columns and be xts objects or
#' coercible to xts.
#' 
#' Duplicate index values are supported. When one or more input has the same
#' index value, the duplicated index values in the result are in the same order
#' the objects are passed to `rbind()`. See examples.
#'
#' `c()` is an alias for `rbind()` for xts objects.
#' 
#' See [`merge.xts()`] for traditional merge operations.
#' 
#' @param \dots Objects to bind by row.
#' @param deparse.level Not implemented.
#' 
#' @return An xts object with one row per row for each object concatenated.
#' 
#' @note `rbind()` is a '.Primitive' function in \R, which means method dispatch
#'   occurs at the C-level, and may not be consistent with normal S3 method
#'   dispatch (see [`rbind()`] for details). Call `rbind.xts()` directly to
#'   avoid potential dispatch ambiguity.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`merge.xts()`] [`rbind()`]
#' 
#' @keywords utilities
#' @examples
#' 
#' x <- xts(1:10, Sys.Date()+1:10)
#' str(x)
#' 
#' merge(x,x)
#' rbind(x,x)
#' rbind(x[1:5],x[6:10])
#' 
#' c(x,x)
#' 
#' # this also works on non-unique index values
#' x <- xts(rep(1,5), Sys.Date()+c(1,2,2,2,3))
#' y <- xts(rep(2,3), Sys.Date()+c(1,2,3))
#' 
#' # overlapping indexes are appended
#' rbind(x,y)
#' rbind(y,x)
#' 
rbind.xts <- function(..., deparse.level=1)
{
  .External(C_rbindXts, dup=FALSE, ...)
}

`.rbind.xts` <-
function(..., deparse.level=1) {
  dots <- list(...)
  if(length(dots) < 2) return(dots[[1]])
  x <- dots[[1]]
  dots <- dots[-1]

  while( length(dots) > 0 ) {
    y <- dots[[1]]
    if( length(dots) > 0)
      dots <- dots[-1]
    if(!is.null(colnames(y)) && colnames(x) != colnames(y))
      warning('column names differ')
    x <- .Call(C_do_rbind_xts,x,y,FALSE)
  }
  return(x)
}
