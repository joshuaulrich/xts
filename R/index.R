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


index.xts <- time.xts <-
function(x, ...) {
  value <- indexClass(x)
  if(is.null(value))
    return(.index(x))

  #x.index  <- structure(.index(x), class=c("POSIXct","POSIXt"))
  x.index  <- .POSIXct(.index(x))
  if(length(x.index) == 0)
    return(integer())

  if(!is.list(value)) 
    value <- as.list(value)
  if(!value[[1]] %in% c('multitime','dates','chron',
                        'POSIXt','POSIXlt','POSIXct',
                        'Date','timeDate',
                        'yearmon','yearqtr') ) {
       stop(paste('unsupported',sQuote('indexClass'),'indexing type:',as.character(value[[1]])))
  }
  if(value[[1]]=='timeDate') {
    stopifnot('package:timeDate' %in% search() | require('timeDate',quietly=TRUE))
    x.index <- do.call(paste('as',value[[1]],sep='.'),list(x.index))
  } 
  else if(value[[1]] %in% c('chron','dates','POSIXt','POSIXct','POSIXlt','yearmon','yearqtr')) {
    if('POSIXt' %in% value[[1]]) value[[1]] <- value[[2]] # get specific ct/lt value
    if(value[[1]] %in% c('dates','chron')) {
      stopifnot('package:chron' %in% search() | require('chron',quietly=TRUE))
      value[[1]] <- 'chron'
    } 
    x.index <- do.call(paste('as',value[[1]],sep='.'),list(x.index))
  } else x.index <- as.Date(as.character(x.index))
  if(class(x.index)[1] %in% c('chron'))
    attr(x.index, 'tzone') <- NULL
  x.index
}

`time<-.xts` <- `index<-.xts` <- function(x, value) {
  if(length(index(x)) != length(value)) stop('length of index vectors does not match')

  if( !timeBased(value) ) 
    stop(paste('unsupported',sQuote('index'),
               'index type of class',sQuote(class(value))))

  # set index to the numeric value of the desired index class
  attr(x, 'index') <- as.numeric(as.POSIXct(value))

  # set the .indexCLASS attribute to the end-user specified class
  attr(x, '.indexCLASS') <- class(value)
  return(x)
}

`.index` <- function(x, ...) {
  if(is.list(attr(x, "index"))) {
    attr(x, 'index')[[1]]
  } else attr(x, "index")
}

`.index<-` <- function(x, value) {
  if(timeBased(value)) {
    if(inherits(value, 'Date')) {
      attr(x, 'index') <- as.numeric(value)
    } else {
      attr(x, 'index') <- as.numeric(as.POSIXct(value))
    }
  } else 
  if(is.numeric(value)) {
    attr(x, 'index') <- value
  } else stop(".index is used for low level operations - data must be numeric or timeBased")
  return(x)
}

`.indexsec` <- function(x) {
  #as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$sec
  as.POSIXlt(.POSIXct(.index(x)))$sec
}
`.indexmin` <- function(x) {
  #as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$min
  as.POSIXlt(.POSIXct(.index(x)))$min
}
`.indexhour` <- function(x) {
  #as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$hour
  as.POSIXlt(.POSIXct(.index(x)))$hour
}
`.indexmday` <- function(x) {
  #as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$mday
  as.POSIXlt(.POSIXct(.index(x)))$mday
}
`.indexmon` <- function(x) {
  #as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$mon
  as.POSIXlt(.POSIXct(.index(x)))$mon
}
`.indexyear` <- function(x) {
  #as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$year
  as.POSIXlt(.POSIXct(.index(x)))$year
}
`.indexwday` <- function(x) {
  #as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$wday
  as.POSIXlt(.POSIXct(.index(x)))$wday
}
`.indexbday` <- function(x) {
  # is business day T/F
  #as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$wday %% 6 > 0
  as.POSIXlt(.POSIXct(.index(x)))$wday %% 6 > 0
}
`.indexyday` <- function(x) {
  #as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$yday
  as.POSIXlt(.POSIXct(.index(x)))$yday
}
`.indexisdst` <- function(x) {
  #as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$isdst
  as.POSIXlt(.POSIXct(.index(x)))$isdst
}
`.indexDate` <- `.indexday` <- function(x) {
  .index(x) %/% 86400L
}
`.indexweek` <- function(x) {
  (.index(x) + (3 * 86400)) %/% 86400 %/% 7
}
`.indexyweek` <- function(x) {
  ((.index(x) + (3 * 86400)) %/% 86400 %/% 7) -
    ((startOfYear() * 86400 + (3 * 86400)) %/% 86400 %/% 7)[.indexyear(x) + 1]
}
