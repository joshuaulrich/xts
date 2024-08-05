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


#' Convert time series data to an OHLC series
#' 
#' Convert an OHLC or univariate object to a specified periodicity lower than
#' the given data object. For example, convert a daily series to a monthly
#' series, or a monthly series to a yearly one, or a one minute series to an
#' hourly series.
#' 
#' The result will contain the open and close for the given period, as well as
#' the maximum and minimum over the new period, reflected in the new high and
#' low, respectively. Aggregate volume will also be calculated if applicable.
#' 
#' An easy and reliable way to convert one periodicity of data into any new
#' periodicity. It is important to note that all dates will be aligned to the
#' *end* of each period by default - with the exception of `to.monthly()` and
#' `to.quarterly()`, which use the \pkg{zoo} package's [yearmon][zoo::zoo] and
#' [yearqtr][zoo::zoo] classes, respectively.
#' 
#' Valid period character strings include: `"seconds"`, `"minutes"`, `"hours"`,
#' `"days"`, `"weeks"`, `"months"`, `"quarters"`, and `"years"`. These are
#' calculated internally via [`endpoints()`]. See that function's help page for
#' further details.
#' 
#' To adjust the final indexing style, it is possible to set `indexAt` to one
#' of the following: \sQuote{yearmon}, \sQuote{yearqtr}, \sQuote{firstof},
#' \sQuote{lastof}, \sQuote{startof}, or \sQuote{endof}. The final index will
#' then be `yearmon`, `yearqtr`, the first time of the period, the last time
#' of the period, the starting time in the data for that period, or the ending
#' time in the data for that period, respectively.
#' 
#' It is also possible to pass a single time series, such as a univariate
#' exchange rate, and return an OHLC object of lower frequency - e.g. the
#' weekly OHLC of the daily series.
#' 
#' Setting `drop.time = TRUE` (the default) will convert a series that includes
#' a time component into one with just a date index, since the time component
#' is often of little value in lower frequency series.
#' 
#' @param x A univariate or OHLC type time-series object.
#' @param period Period to convert to. See details.
#' @param indexAt Convert final index to new class or date. See details.
#' @param drop.time Remove time component of POSIX datestamp (if any)?
#' @param k Number of sub periods to aggregate on (only for minutes and
#'   seconds).
#' @param name Override column names?
#' @param OHLC Should an OHLC object be returned? (only `OHLC = TRUE`
#'   currently supported)
#' @param \dots Additional arguments.
#' 
#' @return An object of the original type, with new periodicity.
#' 
#' @note In order for this function to work properly on OHLC data, it is
#' necessary that the Open, High, Low and Close columns be names as such;
#' including the first letter capitalized and the full spelling found.
#' Internally a call is made to reorder the data into the correct column order,
#' and then a verification step to make sure that this ordering and naming has
#' succeeded. All other data formats must be aggregated with functions such as
#' `aggregate()` and `period.apply()`.
#' 
#' This method should work on almost all time-series-like objects. Including
#' \sQuote{timeSeries}, \sQuote{zoo}, \sQuote{ts}, and \sQuote{irts}. It is
#' even likely to work well for other data structures - including
#' \sQuote{data.frames} and \sQuote{matrix} objects.
#' 
#' Internally a call to `as.xts()` converts the original `x` into the
#' universal xts format, and then re-converts back to the original type.
#' 
#' A special note with respect to \sQuote{ts} objects. As these are strictly
#' regular they may include `NA` values. These are removed before aggregation,
#' though replaced before returning the result. This inevitably leads to many
#' additional `NA` values in the result. Consider using an xts object or
#' converting to xts using `as.xts()`.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @keywords utilities
#' @aliases to_period
#' @examples
#' 
#' data(sample_matrix)
#' 
#' samplexts <- as.xts(sample_matrix)
#' 
#' to.monthly(samplexts)
#' to.monthly(sample_matrix)
#' 
#' str(to.monthly(samplexts))
#' str(to.monthly(sample_matrix))
#' 
to.period <- to_period <- function(x, period='months', k=1, indexAt=NULL, name=NULL, OHLC=TRUE, ...) {
  if(missing(name)) name <- deparse(substitute(x))

  xo <- x
  x <- try.xts(x)

  if(NROW(x)==0 || NCOL(x)==0)
    stop(sQuote("x")," contains no data")

  if(any(is.na(x))) {
    x <- na.omit(x)
    warning("missing values removed from data")
  }

  if(is.character(period)) {
    ep <- endpoints(x, period, k)
  } else {
    if(!is.numeric(period)) {
      stop("'period' must be a character or a vector of endpoint locations")
    }
    if(!missing("k")) {
      warning("'k' is ignored when using custom 'period' locations")
    }
    if(!is.null(indexAt)) {
      warning("'indexAt' is ignored when using custom 'period' locations")
      indexAt <- NULL
    }
    ep <- as.integer(period)
    # ensure 'ep' starts with 0 and ends with nrow(x)
    if(ep[1] != 0) {
      ep <- c(0L, ep)
    }
    if (ep[length(ep)] != NROW(x)) {
      ep <- c(ep, NROW(x))
    }
  }

  if(!OHLC) {
    xx <- x[ep, ]
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

  xx <- .Call(C_toPeriod,
              x, 
              ep,
              has.Vo(x), has.Vo(x,which=TRUE),
              has.Ad(x) && is.OHLC(x),
              index_at, 
              cnames)
  }

  if(!is.null(indexAt)) {
    if(indexAt=="yearmon" || indexAt=="yearqtr")
      tclass(xx) <- indexAt
    if(indexAt=="firstof") {
      ix <- as.POSIXlt(c(.index(xx)), tz=tzone(xx))
      if(period %in% c("years","months","quarters","days"))
        index(xx) <- firstof(ix$year + 1900, ix$mon + 1)
      else
        index(xx) <- firstof(ix$year + 1900, ix$mon + 1, ix$mday,
                             ix$hour, ix$min, ix$sec)
    }
    if(indexAt=="lastof") {
      ix <- as.POSIXlt(c(.index(xx)), tz=tzone(xx))
      if(period %in% c("years","months","quarters","days"))
        index(xx) <- as.Date(lastof(ix$year + 1900, ix$mon + 1))
      else
        index(xx) <- lastof(ix$year + 1900, ix$mon + 1, ix$mday,
                             ix$hour, ix$min, ix$sec)
    }
  }
  reclass(xx,xo)
}

#' @rdname to.period
`to.minutes` <-
function(x,k,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  if(missing(k)) k <- 1
  to.period(x,'minutes',k=k,name=name,...)
}

#' @rdname to.period
`to.minutes3` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=3,name=name,...)
}

#' @rdname to.period
`to.minutes5` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=5,name=name,...)
}

#' @rdname to.period
`to.minutes10` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=10,name=name,...)
}

#' @rdname to.period
`to.minutes15` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=15,name=name,...)
}

#' @rdname to.period
`to.minutes30` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=30,name=name,...)
}

#' @rdname to.period
`to.hourly` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'hours',name=name,...)
}

#' @rdname to.period
`to.daily` <-
function(x,drop.time=TRUE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'days',name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}

#' @rdname to.period
`to.weekly` <-
function(x,drop.time=TRUE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'weeks',name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}

#' @rdname to.period
`to.monthly` <-
function(x,indexAt='yearmon',drop.time=TRUE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'months',indexAt=indexAt,name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}

#' @rdname to.period
`to.quarterly` <-
function(x,indexAt='yearqtr',drop.time=TRUE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'quarters',indexAt=indexAt,name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}

#' @rdname to.period
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
  xts.in <- is.xts(x)  # is the input xts?
  if(!xts.in)          # if not, try to convert to xts
    x <- try.xts(x, error=FALSE)
  if(is.xts(x)) {
    # if x is xts, drop HHMMSS from index
    if(any(tclass(x)=='POSIXt')) {
      # convert index to Date
      index(x) <- as.Date(as.POSIXlt(index(x)))
      tclass(x) <- "Date"  # set tclass to Date
    }
    if(isClassWithoutTZ(tclass(x))) {
      tzone(x) <- "UTC"  # set tzone to UTC
    }
    # force conversion, even if we didn't set tclass to Date
    # because indexAt yearmon/yearqtr won't drop time from index
    index(x) <- index(x)
    if(xts.in)  x    # if input already was xts
    else reclass(x)  # if input wasn't xts, but could be converted
  } else  x          # if input wasn't xts, and couldn't be converted
}

`by.period` <-
function(x, FUN, on=Cl, period="days", k=1, fill=na.locf, ...) {
  # aggregate 'x' to a higher periodicity, apply 'FUN' to the 'on' columns
  # of the aggregate, then merge the aggregate results with 'x' and fill NAs
  # with na.locf.  E.g. you can apply a 5-day SMA of volume to tick data.
  x <- try.xts(x, error = FALSE)
  FUN <- match.fun(FUN)
  on <- match.fun(on)  # Allow function or name
  agg <- to.period(x, period, k, ...)
  res <- FUN(on(agg), ...)
  full <- merge(.xts(NULL,index(x)),res)
  full <- fill(full)  # Allow function or value
  return(full)
}

`to.frequency` <-
function(x, by, k=1, name=NULL, OHLC=TRUE, ...) {
  # similar to to.period, but aggregates on something other than time.
  # E.g. aggregate by volume, where a "period" is 10% of the 5-day volume SMA.

  # Most code pulled from to.period
  if(missing(name)) name <- deparse(substitute(x))

  xo <- x
  x <- try.xts(x)

  if(any(is.na(x))) {
    x <- na.omit(x)
    warning("missing values removed from data")
  }

  # make suitable name vector

  cnames <- c("Open", "High", "Low", "Close")
  if (has.Vo(x)) 
    cnames <- c(cnames, "Volume")
  if (has.Ad(x) && is.OHLC(x))
    cnames <- c(cnames, "Adjusted")
  cnames <- paste(name,cnames,sep=".") 

  if(is.null(name))
    cnames <- NULL

  # start to.frequency-specific code
  if (missing(by)) by <- rep(1L, nrow(x))
  byVec <- cumsum(by)
  bins <- byVec %/% k
  # ep contents must have the same format as output generated by endpoints(): 
  # first element must be zero and last must be nrow(x)
  ep <- c(0L, which(diff(bins) != 0))
  if (ep[length(ep)] != nrow(bins)) ep <- c(ep, nrow(bins))
  # end to.frequency-specific code

  xx <- .Call(C_toPeriod,
              x, 
              ep,
              has.Vo(x), has.Vo(x,which=TRUE),
              has.Ad(x) && is.OHLC(x),
              FALSE,
              cnames)

  reclass(xx,xo)
}
