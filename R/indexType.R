`indexType` <-
function(x,...) {
  if(!is.xts(x)) x <- as.xts(x)
  class(index(x))
}

`indexType<-` <-
function(x,value) {
  UseMethod('indexType<-')
}

`indexType<-.xts` <-
function(x,value) {
  if(!is.list(value)) value <- as.list(value)
  if(!value[[1]] %in% c('POSIXlt','POSIXct','Date','timeDate') )
       stop(paste('illegal',sQuote('indexType'),'value:',as.character(value[[1]])))

  if(value[[1]]=='timeDate') {
    stopifnot('package:fSeries' %in% search() | require('fSeries',quietly=TRUE))
    index(x) <- do.call(paste('as',value[[1]],sep='.'),list(index(x),value))
  } 
  else if(value[[1]] %in% c('POSIXct','POSIXlt')) {
    index(x) <- do.call(paste('as',value[[1]],sep='.'),list(index(x),value))
  } else index(x) <- do.call("as.Date",list(index(x),value))
  x
}

