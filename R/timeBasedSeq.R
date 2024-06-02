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


#' Create a Sequence or Range of Times
#' 
#' A function to create a vector of time-based objects suitable for indexing an
#' xts object, given a string conforming to the ISO-8601 time and date standard
#' for range-based specification. The resulting series can be of any class
#' supported by xts, including POSIXct, Date, chron, timeDate, yearmon, and
#' yearqtr.
#' 
#' `timeBasedRange()` creates a one or two element numeric vector representing
#' the start and end number of seconds since epoch (1970-01-01). For internal
#' use.
#' 
#' `timeBasedSeq()` creates sequences of time-based observations using strings
#' formatted according to the ISO-8601 specification. The general format is
#' *from/to/by* or *from::to::by*, where *to* and *by* are optional when the
#' 'length.out' argument is specified.
#' 
#' The *from* and *to* elements of the string must be left-specified with
#' respect to the standard *CCYYMMDD HHMMSS* form. All dates/times specified
#' will be set to either the earliest point (from) or the latest (to), to the
#' given the level of specificity. For example, \sQuote{1999} in the *from*
#' field would set the start to the beginning of 1999. \sQuote{1999} in the
#' *to* field would set the end to the end of 1999.
#' 
#' The amount of resolution in the result is determined by the resolution of
#' the *from* and *to* component, unless the optional *by* component is
#' specified.
#' 
#' For example, `timeBasedSeq("1999/2008")` returns a vector of Dates for
#' January 1st of each year. `timeBasedSeq("199501/1996")` returns a yearmon
#' vector of 24 months in 1995 and 1996. And `timeBasedSeq("19950101/1996")`
#' creates a Date vector for all the days in those two years.
#' 
#' The optional *by* field (the third delimited element to the string), will
#' the resolution heuristic described above and will use the specified *by*
#' resolution. The possible values for *by* are: 'Y' (years), 'm' (months),
#' 'd' (days), 'H' (hours), 'M' (minutes), 'S' (seconds). Sub-second
#' resolutions are not supported.
#' 
#' @param x An ISO-8601 time-date range string.
#' @param retclass The return class desired.
#' @param length.out Passed to `seq()` internally.
#' @param \dots Unused.
#' 
#' @return `timeBasedSeq()` returns a vector of time-based observations.
#' `timeBasedRange()` returns a one or two element numeric vector representing
#' the start and end number of seconds since epoch (1970-01-01).
#' 
#' When `retclass = NULL`, the result of `timeBasedSeq()` is a named list
#' containing elements "from", "to", "by" and "length.out".
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`timeBased()`], [`xts()`]
#' 
#' @references International Organization for Standardization: ISO 8601
#' <https://www.iso.org>
#' 
#' @keywords utilities
#' @examples
#' 
#' timeBasedSeq('1999/2008')
#' timeBasedSeq('199901/2008')
#' timeBasedSeq('199901/2008/d')
#' timeBasedSeq('20080101 0830',length=100) # 100 minutes
#' timeBasedSeq('20080101 083000',length=100) # 100 seconds
#' 
`timeBasedSeq` <- 
function(x, retclass=NULL, length.out=NULL) {
  if(!is.character(x))
    # allows for unquoted numerical expressions to work
    x <- deparse(match.call()$x)

  x <- gsub('::','/',x, perl=TRUE)  # replace all '::' range ops with '/'
  x <- gsub('[-:]','',x, perl=TRUE) # strip all remaining '-' and ':' seps
  x <- gsub('[ ]','',x, perl=TRUE) # strip all remaining white space
  x <- unlist(strsplit(x,"/"))
  from <- x[1]
  to   <- x[2]
  BY   <- x[3]

  # need to test for user specified length.out, currently just overriding
  if(from == "")
    from <- NA

  if(!is.na(from)) {
    year  <- as.numeric(substr(from,1,4))
    month <- as.numeric(substr(from,5,6))
    day   <- as.numeric(substr(from,7,8))
    hour  <- as.numeric(substr(from,9,10))
    mins  <- as.numeric(substr(from,11,12))
    secs  <- as.numeric(substr(from,13,14))
  
    time.args.from <- as.list(unlist(sapply(c(year,month,day,hour,mins,secs),
                                 function(x) if(!is.na(x)) x)
                          ))
  
    from <- do.call('firstof',time.args.from) 
  } 
  else time.args.from <- list()
 
  # only calculate if to is specified
  if(!is.na(to)) { 
    year  <- as.numeric(substr(to,1,4))
    month <- as.numeric(substr(to,5,6))
    day   <- as.numeric(substr(to,7,8))
    hour  <- as.numeric(substr(to,9,10))
    mins  <- as.numeric(substr(to,11,12))
    secs  <- as.numeric(substr(to,13,14))
  
    time.args.to <- as.list(unlist(sapply(c(year,month,day,hour,mins,secs),
                                 function(x) if(!is.na(x)) x)
                          ))
  
    to <- do.call('lastof',time.args.to) 
  } 
  else time.args.to <- list()

  max.resolution <- max(length(time.args.from), length(time.args.to))

  # if neither is set
  if(max.resolution == 0) 
    max.resolution <- 1

  resolution <- c('year','month','DSTday','hour','mins','secs')[max.resolution]

  if(!is.na(BY)) resolution <- names(match.arg(BY, list(year  ='Y',
                                                        month ='m',
                                                        day   ='d',
                                                        hour  ='H',
                                                        mins  ='M',
                                                        secs  ='S')))

  convert.to <- 'Date'
  if(max.resolution == 2 || resolution == 'month' ) convert.to <- 'yearmon'
  if(max.resolution >  3 || resolution %in% c("hour","mins","secs")) convert.to <- 'POSIXct'

 
  if(is.na(to) && missing(length.out))
    length.out <- 1L

  if(((!missing(retclass) && is.null(retclass)) || any(is.na(to),is.na(from)))) {
    # return the calculated values only
    return(list(from=from,to=to,by=resolution,length.out=length.out))
  }

  if(is.null(length.out)) {
    SEQ <- seq(from,to,by=resolution)
  } else {
    SEQ <- seq(from, by=resolution, length.out=length.out)
  }

  if(!is.null(retclass)) convert.to <- retclass
  if(convert.to == 'POSIXct') {
    structure(SEQ, class=c('POSIXct','POSIXt'))  # need to force the TZ to be used
  } else
  do.call(paste('as',convert.to,sep='.'), list(SEQ))

}
