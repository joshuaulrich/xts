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

shift.time <- function(x, n=60, ...) {
  UseMethod("shift.time")
}

shift.time.xts <- function(x, n=60, ...) {
  .xts(x, .index(x) + n, tzone=indexTZ(x), tclass=indexClass(x))
}

make.index.unique <- make.time.unique <- function(x, eps=0.00001, ...) {
  UseMethod("make.index.unique")
}

make.index.unique.xts <- function(x, eps=0.00001, ...) {
  .Call('make_index_unique', x, eps, PACKAGE="xts")
}

make.index.unique.numeric <- function(x, eps=0.00001, ...) {
  .Call('make_unique', x, eps, PACKAGE="xts")
}

make.index.unique.POSIXct <- function(x, eps=0.00001, ...) {
  .Call('make_unique', x, eps, PACKAGE="xts")
}
