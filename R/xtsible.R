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


xtsible <- function(x)
{
  if(inherits(try(as.xts(x),silent=TRUE),'try-error')) {
    FALSE
  } else TRUE
}

use.xts <- try.xts <- function(x, ..., error=TRUE)
{
  if(is.xts(x)) {
    attr(x,'.RECLASS') <- FALSE
    return(x)
  }

  xx <- try(as.xts(x,...),silent=TRUE)
  if(inherits(xx,'try-error')) {
    if(is.character(error)) {
      stop(error)
    } else  
    if(is.function(error)) {
      return(error(x, ...))
    } else 
    if(error) {
      stop(gsub('\n','',xx))
    } else {
      return(x) 
    }
  } else {
    # made positive: now test if needs to be reclassed
    xx
  }
}

.merge.xts.scalar <- function(x, length.out, ...) {
  if( length(x) == 1 )
    return(matrix(rep(x, length.out=length.out)))
  if( NROW(x) == length.out )
    return(x)
  stop("improper length of one or more arguments to merge.xts")
}

use.reclass <- Reclass <- function(x) {
  xx <- match.call()
  xxObj <- eval.parent(parse(text=all.vars(xx)[1]), 1)
  inObj <- try.xts(xxObj, error=FALSE)
  xx <- eval(match.call()[[-1]])
  reclass(xx, inObj)
}
