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


# dimnames will return the actual dimnames of the xts object
# dimnames<-.xts will force the rownames to always be NULL

`dimnames.xts` <-
function(x) {
  #list(NULL, colnames(unclass(x)))
  .Call("dimnames_zoo",x);
  #list(as.character(index(x)), colnames(unclass(x)))
}

`dimnames<-.xts` <-
function(x, value) {
  oclass <- class(x)
  x <- unclass(x)
  d <- dim(x)
  if(is.null(value)) {
    dimnames(x) <- NULL
  }
  else {
    if(!is.list(value) || length(value) != 2L)
      stop("invalid 'dimnames' given for xts")
    value[[1L]] <- list(NULL)
    value[[2L]] <- as.character(value[[2L]])
  
    rownames(x) <- NULL
    colnames(x) <- value[[2L]]
    class(x) <- oclass
  }
  x
}
