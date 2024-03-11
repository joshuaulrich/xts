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


#' Get or Replace the Format of an xts Object's Index
#' 
#' Generic functions to get or replace the format that determines how an xts
#' object's index is printed.
#' 
#' Valid values for the \code{value} argument are the same as specified in the
#' \emph{Details} section of \code{\link{strptime}}.
#' 
#' An xts object's \code{tformat} is \code{NULL} by default, so the index will
#' be formatted according to its \code{\link{tclass}} (e.g. \code{Date},
#' \code{POSIXct}, \code{timeDate}, \code{yearmon}, etc.).
#' 
#' \code{tformat} only changes how the index is \emph{printed} and how the row
#' names are formatted when xts objects are converted to other classes (e.g.
#' \code{matrix} or \code{data.frame}. It does not affect the internal index in
#' any way.
#' 
#' indexFormat<-
#' @param x an \code{xts} object
#' @param value new index format string (see Details for valid values)
#' @param \dots arguments passed to other methods
#' 
#' @return A vector containing the format for the object's index.
#' 
#' @note Both \code{indexFormat} and \code{indexFormat<-} are deprecated in
#' favor of \code{tformat} and \code{tformat<-}, respectively.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{index}} has more information on the xts index,
#' \code{\link{tclass}} details how \pkg{xts} handles the class of the index,
#' \code{\link{tzone}} has more information about the index timezone settings.
#' 
#' @keywords ts utilities
#' @examples
#' 
#'   x <- timeBasedSeq('2010-01-01/2010-01-02 12:00')
#'   x <- xts(seq_along(x), x)
#' 
#'   # set a custom index format
#'   head(x)
#'   tformat(x) <- "%Y-%b-%d %H:%M:%OS3"
#'   head(x)
#' 
`tformat` <-
function(x, ...) {
  UseMethod('tformat')
}

#' @rdname tformat
`tformat<-` <-
function(x, value) {
  UseMethod('tformat<-')
}

`tformat.default` <-
function(x, ...) {
  attr(x, 'tformat')
}

`tormat<-.default` <-
function(x, value) {
  attr(x, '.tformat') <- value
  x
}

`tformat.xts` <-
function(x, ...) {
  ix <- .index(x)
  attr(ix, 'tformat')
}

`tformat<-.xts` <-
function(x, value) {

  if(!is.character(value) && !is.null(value))
    stop('must provide valid POSIX formatting string')

  # Remove format attrs (object created before 0.10-3)
  attr(x, ".indexFORMAT") <- NULL

  attr(attr(x, 'index'), 'tformat') <- value
  x
}

#' @rdname tformat
`indexFormat` <-
function(x) {
  .Deprecated("tformat", "xts")
  tformat(x)
}

#' @rdname tformat
`indexFormat<-` <-
function(x, value) {
  .Deprecated("tformat<-", "xts")
  `tformat<-`(x, value)
}
