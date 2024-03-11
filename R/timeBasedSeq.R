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
#' \emph{xts} object, given a string conforming to the ISO 8601 time and date
#' standard for range-based specification. The resultant series can be of any
#' class supported by \emph{xts}, including POSIXct, Date, chron, timeDate,
#' yearmon, and yearqtr.
#' 
#' \code{timeBasedRange} creates a vector of length 1 or 2 as seconds since the
#' epoch (1970-01-01) for use internally.
#' 
#' Designed to provide uniform creation of valid time-based objects for use
#' within \emph{xts}, the interface conforms (mostly) to the ISO recommended
#' format for specifying ranges.
#' 
#' In general, the format is a string specifying a time and/or date
#' \emph{from}, \emph{to}, and optionally \emph{by} delineated by either
#' \sQuote{"/"} or \sQuote{"::"}.
#' 
#' The first argument need not be quoted, as it is converted internally if need
#' be.
#' 
#' The general form is \emph{from/to/by} or \emph{from::to::by}, where
#' \emph{to} and \emph{by} are optional if the length.out arg is specified.
#' 
#' The \code{from} and \code{to} elements of the string must be left-specified
#' with respect to the standard \emph{CCYYMMDD HHMMSS} form.  All dates-times
#' specified will be set to either the earliest point (from) or the latest
#' (to), given the level of specificity.
#' 
#' For example \sQuote{1999} in the \emph{from} field would set the start to
#' the beginning of 1999. The opposite occurs in the \emph{to} field.
#' 
#' The level of detail in the request is interpretted as the level of detail in
#' the result.  The maximum detail of either \emph{from} or \emph{to} is the
#' basis of the sequence, unless the optional \emph{by} element is specified,
#' which will be covered later.
#' 
#' To request a yearly series, it is only necessary to use
#' \sQuote{"1999/2008"}.  Alternately, one could request a monthly series
#' (returned by default as class \code{yearmon}) with \sQuote{"199901/2008"} or
#' \sQuote{"1999-01/2008"}, or even \sQuote{"1999/2008-01"}. As the level of
#' granularity increases, so does the resultant sequence granularity - as does
#' its length.
#' 
#' Using the optional third \emph{by} field (the third delimited element to the
#' string), will override the granularity intepretation and return the
#' requested periodicity.  The acceptable arguments include \code{Y} for years,
#' \code{m} for months, \code{d} for days, \code{H} for hours, \code{M} for
#' minutes and \code{S} for seconds.
#' 
#' @param x a string representing the time-date range desired
#' @param retclass the return class desired
#' @param length.out passed to \code{seq} internally
#' @param \dots unused
#' 
#' @return A sequence or range of time-based objects.
#' 
#' If \code{retclass} is \code{NULL}, the result is a named list of from, to,
#' by and length.out.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{timeBased}}, \code{\link{xts}}
#' 
#' @references International Organization for Standardization: ISO 8601
#' \url{https://www.iso.org}
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
