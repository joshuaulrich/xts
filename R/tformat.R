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


`tformat` <-
function(x, ...) {
  UseMethod('tformat')
}

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

`indexFormat` <-
function(x) {
  .Deprecated("tformat", "xts")
  tformat(x)
}

`indexFormat<-` <-
function(x, value) {
  .Deprecated("tformat<-", "xts")
  `tformat<-`(x, value)
}
