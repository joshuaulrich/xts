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


# dimnames will return the actual dimnames of the xts object
# dimnames<-.xts will force the rownames to always be NULL


#' Dimnames of an xts Object
#' 
#' Get or set dimnames of an xts object.
#' 
#' For efficienty, xts objects do not have rownames (unlike zoo objects).
#' Attempts to set rownames on an xts object will silently set them to `NULL`.
#' This is done for internal compatibility reasons, as well as to provide
#' consistency in performance regardless of object use.
#' 
#' @param x An xts object.
#' @param value A two element list. See Details.
#' 
#' @return A list or character string containing coerced row names and/or
#' actual column names.
#' 
#' Attempts to set rownames on xts objects via rownames or dimnames will
#' silently fail.
#' 
#' @note Unlike zoo, all xts objects have dimensions. xts objects cannot be
#' plain vectors.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`xts()`]
#' 
#' @keywords misc
#' @examples
#' 
#' x <- xts(1:10, Sys.Date()+1:10)
#' dimnames(x)
#' rownames(x)
#' rownames(x) <- 1:10
#' rownames(x)
#' str(x)
#' 
`dimnames.xts` <-
function(x) {
  #list(NULL, colnames(unclass(x)))
  .Call(C_dimnames_zoo,x);
  #list(as.character(index(x)), colnames(unclass(x)))
}

#' @rdname dimnames.xts
`dimnames<-.xts` <-
function(x, value) {
  .Call(C_xts_set_dimnames, x, value)
}
