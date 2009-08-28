align.time <- function(x, ...) {
  UseMethod("align.time")
}

align.time.xts <- function(x, n=60, ...) {
  .xts(x, .index(x) + (n-.index(x) %% n))
}

align.time.POSIXct <- function(x, n=60, ...) {
  structure(unclass(x) + (n - unclass(x) %% n),class=c("POSIXt","POSIXct"))
}

align.time.POSIXlt <- function(x, n=60, ...) {
  as.POSIXlt(align.time(as.POSIXct(x),n=n,...))
}
