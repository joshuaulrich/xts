`tindex` <-
function(x,...) {
  # extract and reformat times of time-series object
  UseMethod('tindex')
}
`tindex.timeSeries` <-
function(x,type=c('POSIXlt','POSIXct','Date','timeDate'),...) {
  if(type[1]=='timeDate') 
    stopifnot('package:fSeries' %in% search() | require('fSeries',quietly=TRUE))

  if(!type[1] %in% c('POSIXlt','POSIXct','Date','timeDate')) stop('unsupported time type')
  do.call(paste('as',type[1],sep='.'),list(x@positions,...))
}
`tindex.its` <- function(x,type=c('POSIXlt','POSIXct','Date','timeDate'),...) {
  if(type[1]=='timeDate') 
    stopifnot('package:fSeries' %in% search() | require('fSeries',quietly=TRUE))

  ix <- x@dates
  if(!missing(type)) {
    if(!type[1] %in% c('POSIXlt','POSIXct','Date','timeDate')) stop('unsupported time type')
    ix <- do.call(paste('as',type[1],sep='.'),list(ix,...))
  }

  if(!inherits(ix,'POSIXt') & !inherits(ix,'timeDate') &
     !inherits(ix,'Date')) warning("not a time-based index: potential incompatabilities")

  ix
}
`tindex.ts` <- function(x,...) {
  as.Date(as.numeric(time(x)),...)
}
`tindex.default` <- function(x,type=c('POSIXlt','POSIXct','Date','timeDate'),...) {
  if(type[1]=='timeDate') 
    stopifnot('package:fSeries' %in% search() | require('fSeries',quietly=TRUE))

  ix <- index(x)
  if(!missing(type)) {
    if(!type[1] %in% c('POSIXlt','POSIXct','Date','timeDate')) stop('unsupported time type')
    ix <- do.call(paste('as',type[1],sep='.'),list(index(x),...))
  }

  if(!inherits(ix,'POSIXt') & !inherits(ix,'timeDate') &
     !inherits(ix,'Date')) warning("not a time-based index: potential incompatabilities")

  ix
}

