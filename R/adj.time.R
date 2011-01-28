adj.time <-
function(tm, ...) {
  tr <- match.call(expand=FALSE)$...
  if(length(tr) < 1) return(tm)

  oClass <- class(tm)
  tm <- as.POSIXlt(tm)
  ntime <- as.environment(unclass(tm))
  lapply(tr, function(T) {
    assign(all.vars(T), with(tm, eval(T)), envir=ntime)
  })
  tm <- structure(list(
    sec=ntime$sec, min=ntime$min, hour=ntime$hour, 
    mday=ntime$mday, mon=ntime$mon, year=ntime$year,
    wday=ntime$wday, yday=ntime$yday,isdst=ntime$isdst), tzone=attr(tm,"tzone"),
    class=c("POSIXt","POSIXlt"))
  do.call(paste('as',oClass[1],sep='.'), list(tm))
}

