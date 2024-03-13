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
#' 
#' Implemented in C, these functions bind `xts` objects by row, resulting
#' in another `xts` object
#' 
#' There may be non-unique index values in either the original series, or the
#' resultant series.
#' 
#' Identical indexed series are bound in the order or the arguments passed to
#' rbind. See examples.
#' 
#' All objects must have the same number of columns, as well as be `xts`
#' objects or coercible to such.
#' 
#' `rbind` and `c` are aliases.
#' 
#' For traditional merge operations, see `merge.xts` and `cbind.xts`.
#' 
#' @param \dots objects to bind
#' @param deparse.level not implemented
#' 
#' @return An `xts` object with one row per row for each object
#' concatenated.
#' 
#' @note This differs from rbind.zoo in that non-unique index values are
#' allowed, in addition to the completely different algorithms used internally.
#' 
#' All operations may not behave as expected on objects with non-unique
#' indices.  You have been warned.
#' 
#' `rbind` is a .Primitive function in \R. As such method dispatch occurs at
#' the C-level, and may not be consistent with expectations.  See the details
#' section of the base function, and if needed call rbind.xts directly to avoid
#' dispatch ambiguity.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [merge.xts()] [rbind()]
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
