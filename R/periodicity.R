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

time.frequency <- function(x) {
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

periodicity <- function(x, ...) {
  if( timeBased(x) || !is.xts(x) )
    x <- try.xts(x, error='\'x\' needs to be timeBased or xtsible')

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

  if(any(tclass(x) == "yearmon")) {
    # yearmon median diff is 31 days, which doesn't "feel" like the right
    # frequency because 365/12 is 30.4166. So set it to 30
    if (p > 86400 * 31) {
      # > 1 month -> 2 months
      # > 2 months is quarterly
      p <- 30 * 2
    } else {
      p <- 30
    }
    units <- "days"
    scale <- "monthly"
    label <- "month"
  } else if(any(tclass(x) == "yearqtr")) {
    #  91 days is median for 1-quarter index
    # 183 days is median for 2-quarter index)
    # 274 days is median for 3-quarter index)
    if (p > 86400 * 183) {
      # > 2-quarter index -> 3 quarters
      p <- 274
    } else
    if (p > 86400 * 91) {
      # > 1-quarter index -> 2 quarters
      p <- 183
    } else {
      p <- 91
    }
    units <- "days"
    scale <- "quarterly"
    label <- "quarter"
  } else
  # Date and POSIXct
  if( p < 60 ) {
    units <- 'secs'
    scale <- 'seconds'
    label <- 'second'
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
    p <- p/3600L
  } else
  if(p < 86400 * 7) {
    units <- "days"
    scale <- "daily"
    label <- "day"
    p <- p/86400L
  } else
  if(p < 86400 * 31) {
    # < 1 month (2592000)
    units <- "days"
    scale <- 'weekly'
    label <- "week"
    p <- p/86400L
  } else 
  if(p < 86400 * 91) {
    # < 1 quarter (7862400)
    units <- "days"
    scale <- 'monthly'
    label <- "month"
    p <- p/86400L
  } else
  if(p < 86400 * 365) {
    # < 1 year (31536000)
    units <- "days"
    scale <- "quarterly"
    label <- "quarter"
    p <- p/86400L
  } else {
    # years
    units <- "days"
    scale <- "yearly"
    label <- "year"
    p <- p/86400L
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

`periodicity.old` <-
function (x, ...) 
{
    if(!is.xts(x)) x <- as.xts(x)

    # convert if necessary to usable format
    if(!tclass(x)[[1]] %in% c('Date','POSIXt')) tclass(x) <- "POSIXct"

    # this takes a long time on big data - possibly use some sort of sampling instead???
    p <- median(diff(time(x)))

    if (is.na(p)) 
        stop("cannot calculate periodicity from one observation")

    p.numeric <- as.numeric(p)
    units <- attr(p, "units")

    if (units == "secs") {
        scale <- "seconds"
    }
    if (units == "mins") {
        scale <- "minute"
        if (p.numeric > 59) 
            scale <- "hourly"
    }
    if (units == "hours") {
        scale <- "hourly"
    }
    if (units == "days") {
        scale <- "daily"
        if (p.numeric > 1) 
            scale <- "weekly"
        if (p.numeric > 7) 
            scale <- "monthly"
        if (p.numeric > 31) 
            scale <- "quarterly"
        if (p.numeric > 91) 
            scale <- "yearly"
    }

    structure(list(difftime = p, frequency = p.numeric, start = index(first(x)), 
        end = index(last(x)), units = units, scale = scale),class="periodicity")
#    class(xx) <- "periodicity"
#    xx  # used when structure was assigned to xx, useless now, remain until testing is done though -jar
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
