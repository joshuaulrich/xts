`index.xts` <-
function(x, ...) {
  value <- indexClass(x)
  if(is.null(value))
    return(.index(x))

  sys.TZ <- Sys.getenv('TZ')
  Sys.setenv(TZ='GMT')
  x.index  <- as.POSIXct(.index(x))
  if(length(x.index) == 0)
    return(integer())

  if(!is.list(value)) 
    value <- as.list(value)
  if(!value[[1]] %in% c('multitime','dates','chron','POSIXt','POSIXlt','POSIXct','Date','timeDate','yearmon','yearqtr') )
       stop(paste('unsupported',sQuote('indexClass'),'indexing type:',as.character(value[[1]])))

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
  } else x.index <- do.call("as.Date",list(x.index))

  #
  #if('timeDate' == value[[1]]) {
    # this will fail now... jar
    # remove unnecessary 'control' attribute
    #x <- structure(x,index=structure(index(x),control=NULL))
  #}

  Sys.setenv(TZ=sys.TZ)
  if(class(x.index)[1] %in% c('chron'))
    attr(x, 'tzone') <- NULL
  x.index
}

`time<-.xts` <- `index<-.xts` <- function(x, value) {
  if(length(index(x)) != length(value)) stop('length of index vectors does not match')

  if( !timeBased(value) ) 
    stop(paste('unsupported',sQuote('index'),
               'index type of class',sQuote(class(value))))

#  tB <- as.POSIXct(value)
#  attributes(x) <- list(attributes(x), index=as.numeric(tB),
#                        dimnames=list(as.character(tB), colnames(x)),
#                        .indexCLASS=class(value))
#  return(x)
  # set index to the numeric value of the desired index class
  attr(x, 'index') <- as.numeric(as.POSIXct(value))

  #rownames(x) <- as.character(as.POSIXct(value))
  
  # set the .indexCLASS attribute to the end-user specified class
  attr(x, '.indexCLASS') <- class(value)
  return(x)
}

`.index` <- function(x, ...) {
  attr(x, 'index')
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
  as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$sec
}
`.indexmin` <- function(x) {
  as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$min
}
`.indexhour` <- function(x) {
  as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$hour
}
`.indexmday` <- function(x) {
  as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$mday
}
`.indexmon` <- function(x) {
  as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$mon
}
`.indexyear` <- function(x) {
  as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$year
}
`.indexwday` <- function(x) {
  as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$wday
}
`.indexbday` <- function(x) {
  # is business day T/F
  as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$wday %% 6 > 0
}
`.indexyday` <- function(x) {
  as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$yday
}
`.indexisdst` <- function(x) {
  as.POSIXlt( structure( .index(x), class=c('POSIXt','POSIXct')) )$isdst
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
