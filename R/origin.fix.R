# fixes for R new/broken as.Date, as.POSIXlt and as.POSIXct
# hopefully to be removed when remedied in R
# taken directly from 'base', with origin set to '1970-01-01' (1970-01-01)

`as.Date.numeric` <- function(x, origin='1970-01-01', ...) {
  as.Date(origin,...) + x
}

`as.POSIXct.numeric` <- function(x, tz="", origin='1970-01-01', ...) {
  as.POSIXct(origin,tz=tz,...) + x
}

`as.POSIXlt.numeric` <- function(x, tz="", origin='1970-01-01', ...) {
  as.POSIXlt(as.POSIXct(origin,tz="UTC",...) + x, tz=tz)
}
