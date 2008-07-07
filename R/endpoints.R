`endpoints` <-
function(x,on='months',k=1) {
  if(timeBased(x)) x <- xts(, order.by=x)
  if(!is.xts(x)) x <- try.xts(x, error='must be either xts-coercible or timeBased')

  sys.TZ <- Sys.getenv('TZ')
  on.exit(Sys.setenv(TZ=sys.TZ))
  Sys.setenv(TZ='GMT')
  indexClass(x) <- 'POSIXct'

  if(on=='quarters') {
    xi <- (as.POSIXlt(index(x))$mon%/%3) + 1
    c(0,which(diff(xi) != 0),NROW(x))
  } else {
    on.opts <- list(secs='%S',seconds='%S',mins='%M',minutes='%M',
                    hours='%H',days='%j',
                    weeks='%W',months='%m',years='%y')
    c(0,which(diff(as.numeric(format(index(x),on.opts[[on]]))%/%k+1) != 0),NROW(x))
  }
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
    if(month %in% 2 && year%%4 %in% 0) {
      # is it a leap year?
      day <- ifelse(month %in% 2 && year%%400 %in% 0,28,29)
    } else day <- mon.lengths[month]
  }
  ISOdatetime(year,month,day,hour,min,sec,tz)
}

