`time<-.xts` <- `index<-.xts` <- function(x, value) {
  if(length(index(x)) != length(value)) stop('length of index vectors does not match')

  if(!class(value)[1] %in% c('dates','chron','POSIXt','POSIXlt','POSIXct','Date','timeDate','yearmon','yearqtr') )
       stop(paste('unsupported',sQuote('index'),'index type of class',sQuote(class(value))))

  attr(x, 'index') <- value
  return(x)
}

`indexClass` <-
function(x,...) {
  if(!is.xts(x)) x <- as.xts(x)
  class(index(x))
}

`indexClass<-` <-
function(x,value) {
  UseMethod('indexClass<-')
}

`indexClass<-.xts` <-
function(x,value) {
  if(value[[1]][1] %in% indexClass(x)[1]) return(x)

  original.indexClass <- indexClass(x)

  sys.TZ <- Sys.getenv('TZ')
  Sys.setenv(TZ='GMT')
  index(x) <- as.POSIXct(index(x))

  if(!is.list(value)) value <- as.list(value)
  if(!value[[1]] %in% c('dates','chron','POSIXt','POSIXlt','POSIXct','Date','timeDate','yearmon','yearqtr') )
       stop(paste('unsupported',sQuote('indexClass'),'indexing type:',as.character(value[[1]])))

  if(value[[1]]=='timeDate') {
    stopifnot('package:fSeries' %in% search() | require('fSeries',quietly=TRUE))
    index(x) <- do.call(paste('as',value[[1]],sep='.'),list(index(x)))
  } 
  else if(value[[1]] %in% c('chron','dates','POSIXt','POSIXct','POSIXlt','yearmon','yearqtr')) {
    if('POSIXt' %in% value[[1]]) value[[1]] <- value[[2]] # get specific ct/lt value
    if(value[[1]] %in% c('dates','chron')) {
      stopifnot('package:chron' %in% search() | require('chron',quietly=TRUE))
      value[[1]] <- 'chron'
    } 
    index(x) <- do.call(paste('as',value[[1]],sep='.'),list(index(x)))
  } else index(x) <- do.call("as.Date",list(index(x)))

  if('timeDate' == original.indexClass[1]) {
    # remove unnecessary 'control' attribute
    x <- structure(x,index=structure(index(x),control=NULL))
  }

  Sys.setenv(TZ=sys.TZ)
  
  x
}

