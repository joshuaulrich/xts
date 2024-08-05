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


#' Locate Endpoints by Time
#' 
#' Extract index locations for an xts object that correspond to the *last*
#' observation in each period specified by `on` and `k`.
#' 
#' `endpoints()` returns a numeric vector that always begins with zero and ends
#' with the number of observations in `x`.
#' 
#' Periods are always based on the distance from the UNIX epoch (midnight
#' 1970-01-01 UTC), *not the first observation in `x`*. See the examples.
#' 
#' Valid values for the `on` argument are: \dQuote{us} (microseconds),
#' \dQuote{microseconds}, \dQuote{ms} (milliseconds), \dQuote{milliseconds},
#' \dQuote{secs} (seconds), \dQuote{seconds}, \dQuote{mins} (minutes),
#' \dQuote{minutes}, \dQuote{hours}, \dQuote{days}, \dQuote{weeks},
#' \dQuote{months}, \dQuote{quarters}, and \dQuote{years}.
#' 
#' @param x An xts object.
#' @param on A character string specifying the period.
#' @param k The number of periods each endpoint should cover.
#' 
#' @return A numeric vector of beginning with 0 and ending with the number of
#' of observations in `x`.
#' 
#' @author Jeffrey A. Ryan
#' @keywords utilities
#' @examples
#' 
#' data(sample_matrix)
#' 
#' endpoints(sample_matrix)
#' endpoints(sample_matrix, "weeks")
#' 
#' ### example of how periods are based on the UNIX epoch,
#' ### *not* the first observation of the data series
#' x <- xts(1:38, yearmon(seq(2018 - 1/12, 2021, 1/12)))
#' # endpoints for the end of every other year
#' ep <- endpoints(x, "years", k = 2)
#' # Dec-2017 is the end of the *first* year in the data. But when you start from
#' # Jan-1970 and use every second year end as your endpoints, the endpoints are
#' # always December of every odd year.
#' x[ep, ]
#' 
endpoints <-
function(x,on='months',k=1) {

  if(k < 1) {
    stop("'k' must be > 0")
  }

  if(timeBased(x)) {
    NR <- length(x)
    x <- xts(, order.by=x)
  } else NR <- NROW(x)
  addlast <- TRUE  # remove automatic NR last value

  if(!is.xts(x)) 
    x <- try.xts(x, error='must be either xts-coercible or timeBased')

  # special-case "secs" and "mins" for back-compatibility
  if(on == "secs" || on == "mins")
    on <- substr(on, 1L, 3L)
  on <- match.arg(on, c("years", "quarters", "months", "weeks", "days", "hours",
    "minutes", "seconds", "milliseconds", "microseconds", "ms", "us"))

  # posixltindex is costly in memory (9x length of time)
  # make sure we really need it
  if(on %in% c('years','quarters','months','weeks','days'))
    posixltindex <- as.POSIXlt(.POSIXct(.index(x)),tz=tzone(x))

  include_last <- function(x, k) {
    len <- length(x)
    i <- seq(1L ,len, k)
    if(i[length(i)] != len) {
      i <- c(i, len)
    }
    ep[i]
  }

  switch(on,
    "years" = {
      as.integer(c(0, which(diff(posixltindex$year %/% k + 1) != 0), NR))
    },
    "quarters" = {
      ixyear <- posixltindex$year * 100L + 190000L
      ixqtr <- ixyear + posixltindex$mon %/% 3L + 1L
      ep <- c(0L, which(diff(ixqtr) != 0L), NR)
      if(k > 1) {
        ep <- include_last(ep, k)
      }
      ep
    },
    "months" = {
      ixmon <- posixltindex$year * 100L + 190000L + posixltindex$mon
      ep <- .Call(C_endpoints, ixmon, 1L, 1L, addlast)
      if(k > 1) {
        ep <- include_last(ep, k)
      }
      ep
    },
    "weeks" = {
      .Call(C_endpoints, .index(x)+3L*86400L, 604800L, k, addlast)
    },
    "days" = {
      ixyday <- posixltindex$year * 1000L + 1900000L + posixltindex$yday
      .Call(C_endpoints, ixyday, 1L, k, addlast)
    },
    # non-date slicing should be indifferent to TZ and DST, so use math instead
    "hours" = {
      .Call(C_endpoints, .index(x), 3600L, k, addlast)
    },
    "minutes" = {
      .Call(C_endpoints, .index(x), 60L, k, addlast)
    },
    "seconds" = {
      .Call(C_endpoints, .index(x), 1L, k, addlast)
    },
    "ms" = ,
    "milliseconds" = {
      sec2ms <- .index(x) * 1e3
      .Call(C_endpoints, sec2ms, 1L, k, addlast)
    },
    "us" = ,
    "microseconds" = {
      sec2us <- .index(x) * 1e6
      .Call(C_endpoints, sec2us, 1L, k, addlast)
    }
  )
}

`startof` <-
function(x,by='months', k=1) {
  ep <- endpoints(x,on=by, k=k)
  (ep+1)[-length(ep)]
}

`endof` <-
function(x,by='months', k=1) {
  endpoints(x,on=by, k=k)[-1]
}


#' Create a POSIXct Object
#' 
#' Easily create of time stamps corresponding to the first or last observation
#' in a specified time period.
#' 
#' This is a wrapper to [`ISOdatetime()`] with defaults corresponding to the
#' first or last possible time in a given period.
#' 
#' @param year,month,day Numeric values to specify a day.
#' @param hour,min,sec Numeric vaues to specify time within a day.
#' @param tz Timezone used for conversion.
#' 
#' @return An POSIXct object.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`ISOdatetime()`]
#'
#' @keywords utilities
#' @examples
#' 
#' firstof(2000)
#' firstof(2005,01,01)
#' 
#' lastof(2007)
#' lastof(2007,10)
#' 
`firstof` <-
function(year=1970,month=1,day=1,hour=0,min=0,sec=0,tz="") {
  ISOdatetime(year,month,day,hour,min,sec,tz)
}

#' @param subsec Number of sub-seconds.
#' @rdname firstof
lastof <-
function (year = 1970,
          month = 12,
          day = 31,
          hour = 23, 
          min = 59,
          sec = 59,
          subsec=.99999, tz = "") 
{
    if(!missing(sec) && sec %% 1 != 0)
      subsec <- 0
    sec <- ifelse(year < 1970, sec, sec+subsec) # <1970 asPOSIXct bug workaround
    #sec <- sec + subsec
    mon.lengths <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 
        30, 31)
    if (missing(day)) {
        day <- ifelse(month %in% 2, ifelse(((year%%4 %in% 0 & 
            !year%%100 %in% 0) | (year%%400 %in% 0)), 29, 28), 
            mon.lengths[month])
    }
    # strptime has an issue (bug?) which returns NA when passed
    # 1969-12-31-23-59-59; pass 58.9 secs instead.
    sysTZ <- Sys.getenv("TZ")
    if (length(c(year, month, day, hour, min, sec)) == 6 &&
        all(c(year, month, day, hour, min, sec) == c(1969, 12, 31, 23, 59, 59)) &&
        (sysTZ == "" || isUTC(sysTZ)))
        sec <- sec-1
    ISOdatetime(year, month, day, hour, min, sec, tz)
}
