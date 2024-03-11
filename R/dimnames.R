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
#' The functions \code{dimnames.xts} and \code{dimnames<-.xts} are methods for
#' the base functions \code{dimnames} and \code{dimnames<-}.
#' 
#' \code{xts} objects by design are intended for lightweight management of
#' time-indexed data.
#' 
#' Rownames are redundant in this design, as well as quite burdensome with
#' respect to memory consumption and internal copying costs.
#' 
#' \code{rownames} and \code{colnames} in make use of \code{dimnames} method
#' dispatch internally, and thus require only modifications to dimnames to
#' enforce the \code{xts} no rownames requirement.
#' 
#' To prevent accidental setting of rownames, \code{dimnames<-} for \code{xts}
#' will simply set the rownames to \code{NULL} when invoked, regardless of
#' attempts to set otherwise.
#' 
#' This is done for internal compatibility reasons, as well as to provide
#' consistency in performance regardless of object use.
#' 
#' User level interaction with either dimnames or rownames will produce a
#' character vector of the index, formatted based on the current specification
#' of \code{indexFormat}. This occurs within the call by converting the results
#' of calling \code{index(x)} to a character string, which itself first creates
#' the object type specified internally from the underlying numeric time
#' representation.
#' 
#' @param x an xts object
#' @param value a list object of length two. See Details.
#' 
#' @return A list or character string containing coerced row names and/or
#' actual column names.
#' 
#' Attempts to set rownames on xts objects via rownames or dimnames will
#' silently fail.  This is your warning.
#' 
#' @note All \code{xts} objects have dimension.  There are no \code{xts}
#' objects representable as named or unnamed vectors.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{xts}}
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
