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

to_period <- function(x, period='months', k=1, indexAt=NULL, name=NULL, OHLC=TRUE, ...) {
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
  if (has.Ad(x))
    cnames <- c(cnames, "Adjusted")
  cnames <- paste(name,cnames,sep=".") 

  if(is.null(name))
    cnames <- NULL

  xx <- .Call("toPeriod", 
              x, 
              endpoints(x, period, k), 
              has.Vo(x), has.Vo(x,which=TRUE),
              has.Ad(x), 
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

`to.period` <-
function(x,period='months',k=1,indexAt=NULL,name=NULL,OHLC=TRUE,...)
{
  original.class <- class(x) # used for classes derived from 'xts'

  if(is.null(name)) name <- deparse(substitute(x))

  # test if is class 'xts', if not, convert to 'xts'
  if(!is.xts(x)) {
    x <- as.xts(x)
    RECLASS <- TRUE
  } else RECLASS <- FALSE

  original.indexClass <- indexClass(x)
  
  if(any(is.na(x))) {
    x <- na.omit(x)  # can't calculate aggregation with NA values  ??? any suggestions
    warning("missing values removed from data")
  }

  # is this an OHLC object already? Force correct ordering first
  # if so, treat differently then single column object
  if(is.OHLC(OHLC(x))) {
    ep <- endpoints(x,period,k)
  
    tz <- as.double(as.matrix(x))

    hasvol <- ifelse(has.Vo(x), 1, 0) # test for volume column
    hasadj <- ifelse(has.Ad(x), 1, 0) # test for adjusted column (from yahoo)

    # ohlcq is the F77 routine to aggregate OHLC data to another periodicity
    q <- .Fortran("ohlcq", ep = as.integer(ep), lep = as.integer(length(ep)), 
        ia = as.double(tz), lia = as.integer(length(tz)), nri = as.integer(NROW(x)), 
        hasvol = as.integer(hasvol), 
        hasadj = as.integer(hasadj), ret = as.double(rep(0, (length(ep) - 
            1) * (NCOL(x)))), PACKAGE = "xts")
    tz <- xts(matrix(q$ret, nc = (4 + hasvol + hasadj), byrow = TRUE), 
        index(x)[ep[-1]], .CLASS=CLASS(x))
    cnames <- c("Open", "High", "Low", "Close")
    if (hasvol == 1) 
        cnames <- c(cnames, "Volume")
    if (hasadj == 1) 
        cnames <- c(cnames, "Adjusted")
    colnames(tz) <- paste(name,cnames,sep=".") 

    # copy any old xts attributes to new object
    xtsAttributes(tz) <- xtsAttributes(x)
    # return whichever object was originally passed in
  } else
  if(NCOL(x)==1) {
    ep <- endpoints(x, period, k)
    tz <- as.double(as.matrix(x))
    
    # ohlcz is the F77 routine to aggregate single-column data to another periodicity
    q <- .Fortran("ohlcz", ep = as.integer(ep), lep = as.integer(length(ep)), 
        ia = as.double(tz), lia = as.integer(length(tz)), 
        ret = as.double(rep(0, (length(ep) - 1) * 4)), PACKAGE = "xts")
    tz <- xts(matrix(q$ret, nc = 4, byrow = TRUE), index(x)[ep[-1]],.CLASS=CLASS(x))
    colnames(tz) = paste(name,c("Open", "High", "Low", "Close"),sep=".")

    # copy any old xts attributes to new object
    xtsAttributes(tz) <- xtsAttributes(x)
    # return whichever object was originally passed in
  }
  else {
    stop("'x' must be a single column or an OHLC type object")
  }

  # allow for nice and user specifiable formatting of dates - per Brian Peterson
  if(!is.null(indexAt)) {
    if(indexAt %in% c('yearmon','yearqtr')) {

      if('timeDate' %in% indexClass(x)) {
        # timeDate needs to be converted to an object of class POSIXct
        # to convert to yearmon/yearqtr correctly.  This is only useful
        # for objects that _can_ have yearmon/yearqtr as an index - 
        # not for 'ts', 'its' or 'timeSeries' 
        indexClass(x) <- "Date"
      }

      if(RECLASS && !is.null(CLASS(x)) && CLASS(x) %in% c('ts','its','timeSeries')) {
        # timeSeries can't handle either of these time-based indexes,
        # so default to startof rather than ugly <NA>
        # ts causes malloc issues when passed a non-numeric index - BAD!
        indexAt <- 'firstof'
      } else {
        # convert to yearmon or yearqtr as requested
        # and set original to current - to prevent reconversion
        indexClass(tz) <- as.character(indexAt)
        original.indexClass <- indexClass(tz)  
      }
    }
    if(indexAt=='startof') {
      index(tz) <- index(x)[startof(x,by=period, k=k)]
    }
    if(indexAt=='endof') {
      index(tz) <- index(x)[endof(x,by=period, k=k)]
    }
    if(indexAt=='firstof') {
      if(period %in% c('months','quarters')) {
        if(period=='months') {
          indexClass(tz) <- 'yearmon'
        } else indexClass(tz) <- 'yearqtr'
      # convert year* to POSIXct
      #indexClass(tz) <- 'POSIXct'
      indexClass(tz) <- indexClass(x)
      }
    }
    if(indexAt=='lastof') {
      if(period %in% c('months','quarters')) {
      # use yearmon/yearqtr as shortcuts to 1rst of month :)
        if(period=='months') {
          indexClass(tz) <- 'yearmon'
        } else indexClass(tz) <- 'yearqtr'
        index(tz) <- as.POSIXct(format(as.Date(index(tz),1)))
      # add one period and subtract a day
#      padv <- ifelse(period=='months',1,3)
#      index(tz) <- (index(tz)+padv/12)
#      # convert year* to POSIXct
#      indexClass(tz) <- 'POSIXct'
#      index(tz) <- index(tz)-86400
      }
    }
  }

  # reset indexClass back to the class originally passed in
  indexClass(tz) <- original.indexClass

  if(RECLASS) return(reclass(tz))
  class(tz) <- original.class # add back original class name if derived from 'xts'
  tz

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
