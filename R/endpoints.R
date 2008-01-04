`endpoints` <-
function(x,on='months') {
  x <- as.xts(x)
  on.opts <- list(hours='%H',days='%j',weeks='%W',months='%m',years='%y')
  c(0,which(diff(as.numeric(format(index(x),on.opts[[on]]))) != 0),NROW(x))
}

`startof` <-
function(x,by='months') {
  ep <- endpoints(x,on=by)
  (ep+1)[-length(ep)]
}

`endof` <-
function(x,by='months') {
  endpoints(x,on=by)[-1]
}

`tindex` <-
function(x,...) {
  UseMethod('tindex')
}
`tindex.timeSeries` <-
function(x,type=c('POSIXlt','POSIXct','Date','timeDate'),...) {
  if(!type[1] %in% c('POSIXlt','POSIXct','Date','timeDate')) stop('unsupported time type')
  do.call(paste('as',type[1],sep='.'),list(x@positions))
}
`tindex.its` <- function(x,type=c('POSIXlt','POSIXct','Date','timeDate'),...) {
  ix <- x@dates
  if(!missing(type)) {
    if(!type[1] %in% c('POSIXlt','POSIXct','Date','timeDate')) stop('unsupported time type')
    ix <- do.call(paste('as',type[1],sep='.'),list(ix))
  }

  if(!inherits(ix,'POSIXt') & !inherits(ix,'timeDate') &
     !inherits(ix,'Date')) warning("not a time-based index: potential incompatabilities")

  ix
}
`tindex.ts` <- function(x,...) {
  as.Date(as.numeric(time(x)))
}
`tindex.default` <- function(x,type=c('POSIXlt','POSIXct','Date','timeDate'),...) {
  ix <- index(x)
  if(!missing(type)) {
    if(!type[1] %in% c('POSIXlt','POSIXct','Date','timeDate')) stop('unsupported time type')
    ix <- do.call(paste('as',type[1],sep='.'),list(index(x)))
  }

  if(!inherits(ix,'POSIXt') & !inherits(ix,'timeDate') &
     !inherits(ix,'Date')) warning("not a time-based index: potential incompatabilities")

  ix
}
