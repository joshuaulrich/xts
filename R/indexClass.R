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

`convertIndex` <-
function(x,value) {
  indexClass(x) <- value
  x
}

`indexClass` <-
function(x) {
  attr(x, '.indexCLASS')
}

`indexClass<-` <-
function(x,value) {
  UseMethod('indexClass<-')
}

`indexClass<-.xts` <-
function(x, value) {
  attr(x, '.indexCLASS') <- value
  x
}

`index.xts` <-
function(x, ...) {
  # the new V2 index is numeric -- therefore we need to convert to the user-defined index type

  value <- indexClass(x)

  sys.TZ <- Sys.getenv('TZ')
  Sys.setenv(TZ='GMT')
  x.index  <- as.POSIXct(attr(x, 'index'))

  


  if(!is.list(value)) 
    value <- as.list(value)
  if(!value[[1]] %in% c('dates','chron','POSIXt','POSIXlt','POSIXct','Date','timeDate','yearmon','yearqtr') )
       stop(paste('unsupported',sQuote('indexClass'),'indexing type:',as.character(value[[1]])))
#
  if(value[[1]]=='timeDate') {
    stopifnot('package:fSeries' %in% search() | require('fSeries',quietly=TRUE))
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
  if('timeDate' == value) {
    # this will fail now... jar
    # remove unnecessary 'control' attribute
    x <- structure(x,index=structure(index(x),control=NULL))
  }

  Sys.setenv(TZ=sys.TZ)
  
  x.index
}

