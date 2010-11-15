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


# to.period functionality from quantmod
#
# to.period base function
# to.minutes
# to.hourly
# to.daily
# to.weekly
# to.monthly
# to.quarterly
# to.yearly

to.period <- to_period <- function(x, period='months', k=1, indexAt=NULL, name=NULL, OHLC=TRUE, ...) {
  if(missing(name)) name <- deparse(substitute(x))

  xo <- x
  x <- try.xts(x)

  if(!OHLC) {
    xx <- x[endpoints(x, period, k),]
  } else {
  if(!is.null(indexAt)) {
    index_at <- switch(indexAt,
                       "startof" = TRUE,  # start time of period
                       "endof"   = FALSE, # end time of period
                       FALSE
                      )
  } else index_at <- FALSE

  # make suitable name vector

  cnames <- c("Open", "High", "Low", "Close")
  if (has.Vo(x)) 
    cnames <- c(cnames, "Volume")
  if (has.Ad(x) && is.OHLC(x))
    cnames <- c(cnames, "Adjusted")
  cnames <- paste(name,cnames,sep=".") 

  if(is.null(name))
    cnames <- NULL

  xx <- .Call("toPeriod", 
              x, 
              endpoints(x, period, k), 
              has.Vo(x), has.Vo(x,which=TRUE),
              has.Ad(x) && is.OHLC(x),
              index_at, 
              cnames, PACKAGE='xts')
  }

  if(!is.null(indexAt)) {
    if(indexAt=="yearmon" || indexAt=="yearqtr")
      indexClass(xx) <- indexAt
    if(indexAt=="firstof") {
      ix <- as.POSIXlt(c(.index(xx)))
      if(period %in% c("years","months","quarters","days"))
        index(xx) <- firstof(ix$year + 1900, ix$mon + 1)
      else
        index(xx) <- firstof(ix$year + 1900, ix$mon + 1, ix$mday,
                             ix$hour, ix$min, ix$sec)
    }
    if(indexAt=="lastof") {
      ix <- as.POSIXlt(c(.index(xx)))
      if(period %in% c("years","months","quarters","days"))
        index(xx) <- as.Date(lastof(ix$year + 1900, ix$mon + 1))
      else
        index(xx) <- lastof(ix$year + 1900, ix$mon + 1, ix$mday,
                             ix$hour, ix$min, ix$sec)
    }
  }
  reclass(xx,xo)
}


`to.minutes` <-
function(x,k,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  if(missing(k)) k <- 1
  to.period(x,'minutes',k=k,name=name,...)
}
`to.minutes3` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=3,name=name,...)
}
`to.minutes5` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=5,name=name,...)
}
`to.minutes10` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=10,name=name,...)
}
`to.minutes15` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=15,name=name,...)
}
`to.minutes30` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=30,name=name,...)
}
`to.hourly` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'hours',name=name,...)
}
`to.daily` <-
function(x,drop.time=TRUE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'days',name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.weekly` <-
function(x,drop.time=TRUE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'weeks',name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.monthly` <-
function(x,indexAt='yearmon',drop.time=FALSE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'months',indexAt=indexAt,name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.quarterly` <-
function(x,indexAt='yearqtr',drop.time=FALSE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'quarters',indexAt=indexAt,name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.yearly` <-
function(x,drop.time=TRUE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'years',name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`.drop.time` <-
function(x) {
  # function to remove HHMMSS portion of time index
  if(!inherits(x,"its")) {
    x <- as.xts(x)
    current.indexClass <- indexClass(x)
    if(current.indexClass[1] == 'POSIXt')
      indexClass(x) <- "Date"
      #reclass(x)
    x
  } else x
}
