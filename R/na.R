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


`na.omit.xts` <- function(object, ...) {
  xx <- .Call('na_omit_xts', object, PACKAGE="xts")
  naa <- attr(xx,'na.action')
  if(length(naa) == 0)
    return(xx)

  naa.index <- .index(object)[naa]

  ROWNAMES <- attr(object,'.ROWNAMES')
  if(!is.null(ROWNAMES)) {
    naa.rownames <- ROWNAMES[naa]
  } else naa.rownames <- NULL

  attr(xx,'na.action') <- structure(naa,
                                    index=naa.index,
                                    .ROWNAMES=naa.rownames)
  return(xx) 
}

`na.replace` <- function(x) {
  if(is.null(xtsAttributes(x)$na.action))
    return(x)

  # Create 'NA' xts object
  tmp <- xts(matrix(rep(NA,NCOL(x)*NROW(x)), nc=NCOL(x)),
             attr(xtsAttributes(x)$na.action, 'index'))

  # Ensure xts 'NA' object has *all* the same attributes
  # as the object 'x'; this is necessary for rbind to
  # work correctly
  CLASS(tmp) <- CLASS(x)
  xtsAttributes(tmp) <- xtsAttributes(x)
  attr(x,'na.action') <- attr(tmp,'na.action') <- NULL
  colnames(tmp) <- colnames(x)

  rbind(x,tmp)
}

na.locf.xts <- function(x, na.rm=FALSE, ...) {
    x <- if(dim(x)[2] > 1) {
      .xts(apply(x, 2, function(x) .Call('na_locf', x, PACKAGE='xts')),
           .index(x))
    } else .Call("na_locf", x, PACKAGE="xts")
    if(na.rm) {
      return(structure(na.omit(x),na.action=NULL))
    } else x
}
