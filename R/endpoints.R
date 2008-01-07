`endpoints` <-
function(x,on='months') {
  x <- as.xts(x)
  on.opts <- list(secs='%S',seconds='%S',mins='%M',minutes='%M',
                  hours='%H',days='%j',
                  weeks='%W',months='%m',years='%y')
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

`firstof` <-
function(year=1970,month=1,day=1,hour=0,min=0,sec=0,tz="") {
  ISOdatetime(year,month,day,hour,min,sec,tz)
}

`lastof` <-
function(year=1970,month=12,day=31,hour=23,min=59,sec=59,tz="") {
  
  mon.lengths <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(missing(day)) {
    if(month==2 & year%%4==0) {
      # is it a leap year?
      day <- ifelse(month==2 & year%%400==0,28,29)
    } else day <- mon.lengths[month]
  }
  ISOdatetime(year,month,day,hour,min,sec,tz)
}



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
