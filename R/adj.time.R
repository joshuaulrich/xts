adj.time <-
function(x, ...) {
  tr <- match.call(expand.dots=FALSE)$...
  if(length(tr) < 1) return(x)

  oClass <- class(x)
  x <- as.POSIXlt(x)
  ntime <- as.environment(unclass(x))
  lapply(tr, function(T) {
    assign(all.vars(T), with(x, eval(T)), envir=ntime)
  })
  x <- structure(list(
    sec=ntime$sec, min=ntime$min, hour=ntime$hour, 
    mday=ntime$mday, mon=ntime$mon, year=ntime$year,
    wday=ntime$wday, yday=ntime$yday,isdst=ntime$isdst), tzone=attr(x,"tzone"),
    class=c("POSIXt","POSIXlt"))
  do.call(paste('as',oClass[1],sep='.'), list(x))
}

