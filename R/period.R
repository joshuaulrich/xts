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


# optimized periodic apply functions
#
`is.timeBased` <- `timeBased` <-
function(x) {
if (!any(sapply(c("Date", "POSIXt", "chron", "dates", "times", 
        "timeDate", "yearmon", "yearqtr", "xtime"), function(xx) inherits(x, 
        xx)))) {
        FALSE
    } else TRUE
}

make.timeBased <- function(x, class='POSIXct', ...)
{
  do.call(class, list(x,...))
}

`period.sum` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")
  if(min(INDEX) < 0 || max(INDEX) > NROW(x)) stop("INDEX must be >= 0 and <= nrow(x)")
  ep <- INDEX
  if(ep[1] != 0) ep <- c(0,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(as.matrix(x))
  q <- .Fortran('psumz',ep=as.integer(ep),lep=as.integer(length(ep)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(ep)-1)))
                ,PACKAGE='xts')
  if(timeBased(index(x))) {
    tz <- xts(q$ret,index(x)[ep[-1]])
  } else {
    tz <- zoo(q$ret,index(x)[ep[-1]])
  }
  tz
}
`period.prod` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")
  if(min(INDEX) < 0 || max(INDEX) > NROW(x)) stop("INDEX must be >= 0 and <= nrow(x)")
  ep <- INDEX
  if(ep[1] != 0) ep <- c(0,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(as.matrix(x))
  q <- .Fortran('pprodz',ep=as.integer(ep),lep=as.integer(length(ep)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(ep)-1)))
                ,PACKAGE='xts')
  if(timeBased(index(x))) {
    tz <- xts(q$ret,index(x)[ep[-1]])
  } else {
    tz <- zoo(q$ret,index(x)[ep[-1]])
  }
  tz
}
`period.max` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")
  if(min(INDEX) < 0 || max(INDEX) > NROW(x)) stop("INDEX must be >= 0 and <= nrow(x)")
  ep <- INDEX
  if(ep[1] != 0) ep <- c(0,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(as.matrix(x))
  q <- .Fortran('pmaxz',ep=as.integer(ep),lep=as.integer(length(ep)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(ep)-1)))
                ,PACKAGE='xts')
  if(timeBased(index(x))) {
    tz <- xts(q$ret,index(x)[ep[-1]])
  } else {
    tz <- zoo(q$ret,index(x)[ep[-1]])
  }
  tz
}
`period.min` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")
  if(min(INDEX) < 0 || max(INDEX) > NROW(x)) stop("INDEX must be >= 0 and <= nrow(x)")
  ep <- INDEX
  if(ep[1] != 0) ep <- c(0,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(as.matrix(x))
  q <- .Fortran('pminz',ep=as.integer(ep),lep=as.integer(length(ep)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(ep)-1)))
                ,PACKAGE='xts')
  if(timeBased(index(x))) {
    tz <- xts(q$ret,index(x)[ep[-1]])
  } else {
    tz <- zoo(q$ret,index(x)[ep[-1]])
  }
  tz
}
