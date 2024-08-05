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

time_frequency <- function(x) {
  x <- gsub(":|/|-| ", "", x)
  nc <- nchar(x)
  if(nc < 4) stop("unrecognizable time.scale")
  if(nc ==  4) res <- 2678400 * 12 #"yearly"
  if(nc  >  4) res <- 2678400      #"monthly"
  if(nc  >  6) res <- 86400        #"daily"
  if(nc  >  8) res <- 3600         #"hourly"
  if(nc  > 10) res <- 60           #"minute"
  if(nc  > 12) res <- 1            #"seconds"
  return(res)
}


#' Approximate Series Periodicity
#' 
#' Estimate the periodicity of a time-series-like object by calculating the
#' median time between observations in days.
#' 
#' A simple wrapper to quickly estimate the periodicity of a given data.
#' Returning an object of type `periodicity`.
#' 
#' This calculates the median time difference between observations as a
#' difftime object, the numerical difference, the units of measurement, and the
#' derived scale of the data as a string.
#' 
#' The time index currently must be of either a 'Date' or 'POSIXct' class, or
#' or coercible to one of them.
#' 
#' The 'scale' component of the result is an estimate of the periodicity of the
#' data in common terms - e.g. 7 day daily data is best described as 'weekly',
#' and would be returned as such.
#' 
#' @param x A time-series-like object.
#' @param \dots Unused.
#' 
#' @return A 'periodicity' object with the following elements:
#' * the `difftime` object,
#' * frequency: the median time difference between observations
#' * start: the first observation
#' * end: the last observation
#' * units: one of secs, mins, hours, or days
#' * scale: one of seconds, minute, hourly, daily, weekly, monthly, quarterly, or yearly
#' * label: one of second, minute, hour, day, week, month, quarter, year
#' 
#' Possible `scale` values are: \sQuote{minute}, \sQuote{hourly}, \sQuote{daily},
#' \sQuote{weekly}, \sQuote{monthly}, \sQuote{quarterly}, and \sQuote{yearly}.
#' 
#' @note This function only attempts to be a *good estimate* for the underlying
#' periodicity. If the series is too short, or has highly irregular periodicity,
#' the return values will not be accurate. That said, it is quite robust and
#' used internally within \pkg{xts}.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`difftime()`]
#' 
#' @keywords utilities
#' @examples
#' 
#' zoo.ts <- zoo(rnorm(231),as.Date(13514:13744,origin="1970-01-01"))
#' periodicity(zoo.ts)
#' 
periodicity <- function(x, ...) {
  if( timeBased(x) ) {
    if( anyNA(x) ) {
      warning("removing NA in 'x' to calculate periodicity")
      x <- x[!is.na(x)]
    }
    x <- try.xts(x, error = "cannot convert 'x' to xts")
  }
  if (!is.xts(x)) {
    x <- try.xts(x, error = "cannot convert 'x' to xts")
  }

  n <- length(.index(x))
  if( n < 2 ) {
    res <- list(difftime = structure(0, units='secs', class='difftime'),
                frequency = 0,
                start = NA,
                end = NA,
                units = 'secs',
                scale = 'seconds',
                label = 'second')
    res <- structure(res, class='periodicity')

    if( n == 0 ) {
      warning("can not calculate periodicity of empty object")
    } else {
      warning("can not calculate periodicity of 1 observation")
      res$start <- start(x)
      res$end <- end(x)
    }
    return(res)
  }

  p <- median(diff( .index(x) ))

  # Date and POSIXct
  if(p < 60) {
    units <- "secs"
    scale <- "seconds"
    label <- "second"
  } else
  if(p < 3600) {
    units <- "mins"
    scale <- "minute"
    label <- "minute"
    p <- p/60L
  } else
  if(p < 86400) {
    # < 1 day
    units <- "hours"
    scale <- "hourly"
    label <- "hour"
  } else
  if(p == 86400) {
    units <- "days"
    scale <- "daily"
    label <- "day"
  } else
  if(p <= 604800) {
    # 86400 * 7
    units <- "days"
    scale <- "weekly"
    label <- "week"
  } else
  if(p <= 2678400) {
    # 86400 * 31
    units <- "days"
    scale <- "monthly"
    label <- "month"
  } else
  if(p <= 7948800) {
    # 86400 * 92
    units <- "days"
    scale <- "quarterly"
    label <- "quarter"
  } else {
    # years
    units <- "days"
    scale <- "yearly"
    label <- "year"
  }

  structure(list(difftime = as.difftime(p, units = units),
                 frequency = p,
                 start = start(x),
                 end = end(x),
                 units = units,
                 scale = scale,
                 label = label),
            class = 'periodicity')
}

`print.periodicity` <-
function (x, ...) 
{
    x.freq <- ifelse(x$scale %in% c("minute", "seconds"), x$frequency, 
        "")
    if (x.freq == "") {
        cap.scale <- paste(toupper(substring(x$scale, 1, 1)), 
            substring(x$scale, 2), sep = "")
        cat(paste(cap.scale, "periodicity from", x$start, "to", 
            x$end, "\n", sep = " "))
    }
    else {
        cat(paste(x.freq, x$scale, "periodicity from", x$start, 
            "to", x$end, "\n", sep = " "))
    }
}
