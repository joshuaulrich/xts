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

time.frequency <- function(x) {
  x <- gsub(":|/|-| ", "", x)
  nc <- nchar(x)
  if(nc < 4) stop("unrecognizable time.scale")
  if(nc ==  4) res <- 2678400 * 12 #"years"
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

  p <- median(diff( .index(x) ))

  if( is.na(p) ) stop("can not calculate periodicity of 1 observation")

  units <- 'days' # the default if p > hourly
  scale <- 'years'# the default for p > quarterly
  label <- 'year'

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
    units <- "hours"
    scale <- "hourly"
    label <- "hour"
  } else
  if(p == 86400) {
    scale <- "daily"
    label <- "day"
  } else
  if( p <= 604800) {
    # 86400 * 7
    scale <- 'weekly'
    label <- "week"
  } else 
  if( p <= 2678400 ) {
    # 86400 * 31
    scale <- 'monthly'
    label <- "month"
  } else
  if( p <=  7948800 ) {
    # 86400 * 92
    scale <- 'quarterly'
    label <- "quarter"
  }

  structure(list(difftime = structure(p,units=units,class='difftime'),
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
    if(!indexClass(x)[[1]] %in% c('Date','POSIXt')) indexClass(x) <- "POSIXct"

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

`period.apply` <-
function(x, INDEX, FUN, ...)
{
    x <- try.xts(x, error = FALSE)
    FUN <- match.fun(FUN)
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
                   FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
                })  
    if(is.vector(xx))
      xx <- t(xx)
    xx <- t(xx)
    if(is.null(colnames(xx)))
      colnames(xx) <- colnames(x)
    reclass(xx, x[INDEX])  
}


`period.apply.original` <-
function (x, INDEX, FUN, ...) 
{
  x <- use.xts(x,error=FALSE)

  if(!is.xts(x)) {
    FUN <- match.fun(FUN)
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
          FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
    })
  } else {
    FUN <- match.fun(FUN)
    new.index <- index(x)[INDEX]
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
          FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
    })
    xx <- xts(xx,new.index)
    CLASS(xx) <- CLASS(x)
    xtsAttributes(xx) <- xtsAttributes(x)
    xx <- reclass(xx)
  }
  xx
}

`apply.daily` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'days')
  period.apply(x,ep,FUN, ...)
}
`apply.weekly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'weeks')
  period.apply(x,ep,FUN, ...)
}

`apply.monthly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'months')
  period.apply(x,ep,FUN, ...)
}

`apply.quarterly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'quarters')
  period.apply(x,ep,FUN, ...)
}

`apply.yearly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'years')
  period.apply(x,ep,FUN, ...)
}
