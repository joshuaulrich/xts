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
  if(!is.list(value)) value <- as.list(value)
  if(!value[[1]] %in% c('POSIXt','POSIXlt','POSIXct','Date','yearmon','yearqtr') )
       stop(paste('illegal',sQuote('indexClass'),'value:',as.character(value[[1]])))

  if(value[[1]]=='timeDate') {
    stopifnot('package:fSeries' %in% search() | require('fSeries',quietly=TRUE))
    index(x) <- do.call(paste('as',value[[1]],sep='.'),list(index(x)))
  } 
  else if(value[[1]] %in% c('POSIXt','POSIXct','POSIXlt','yearmon','yearqtr')) {
    if(value[[1]] == 'POSIXt') value[[1]] <- value[[2]] # get specific ct/lt value
    index(x) <- do.call(paste('as',value[[1]],sep='.'),list(index(x)))
  } else index(x) <- do.call("as.Date",list(index(x)))
  x
}

