align.time <- function(x, ...) {
  UseMethod("align.time")
}

align.time.xts <- function(x, n=60, ...) {
  if(n <= 0) stop("'n' must be positive")
  .xts(x, .index(x) + (n-.index(x) %% n), tzone=indexTZ(x), tclass=indexClass(x))
}

align.time.POSIXct <- function(x, n=60, ...) {
  if(n <= 0) stop("'n' must be positive")
  structure(unclass(x) + (n - unclass(x) %% n),class=c("POSIXt","POSIXct"))
}

align.time.POSIXlt <- function(x, n=60, ...) {
  if(n <= 0) stop("'n' must be positive")
  as.POSIXlt(align.time(as.POSIXct(x),n=n,...))
}
