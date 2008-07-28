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
  if(!is.character(value) && length(value) != 1)
    stop('improperly specified value for indexClass')

  if(!value[1] %in% c('dates','chron','POSIXt','POSIXlt','POSIXct','Date','timeDate','yearmon','yearqtr') )
       stop(paste('unsupported',sQuote('indexClass'),'indexing type:',as.character(value[[1]])))

  attr(x, '.indexCLASS') <- value
  x
}

