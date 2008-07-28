`index.xts` <-
function(x, ...) {
  value <- indexClass(x)

  sys.TZ <- Sys.getenv('TZ')
  Sys.setenv(TZ='GMT')
  x.index  <- as.POSIXct(attr(x, 'index'))

  if(!is.list(value)) 
    value <- as.list(value)
  if(!value[[1]] %in% c('dates','chron','POSIXt','POSIXlt','POSIXct','Date','timeDate','yearmon','yearqtr') )
       stop(paste('unsupported',sQuote('indexClass'),'indexing type:',as.character(value[[1]])))

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
  #if('timeDate' == value[[1]]) {
    # this will fail now... jar
    # remove unnecessary 'control' attribute
    #x <- structure(x,index=structure(index(x),control=NULL))
  #}

  Sys.setenv(TZ=sys.TZ)
  
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

`rindex` <- function(x, ...) {
  attr(x, 'index')
}

`rindex<-` <- function(x, value) {
  if(timeBased(value)) {
    if(inherits(value, 'Date')) {
      attr(x, 'index') <- as.numeric(value)
    } else {
      attr(x, 'index') <- as.numeric(as.POSIXct(value))
    }
  } else 
  if(is.numeric(value)) {
    attr(x, 'index') <- value
  } else stop("rindex is used for low level operations - data must be numeric or timeBased")
  return(x)
}
