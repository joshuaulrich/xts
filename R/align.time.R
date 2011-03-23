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

is.index.unique <- is.time.unique <- function(x) {
  UseMethod("is.time.unique")
}

is.time.unique.xts <- function(x) {
  isOrdered(.index(x), strictly=TRUE)
}

is.time.unique.zoo <- function(x) {
  isOrdered(index(x), strictly=TRUE)
}

make.index.unique <- make.time.unique <- function(x, eps=0.000001, drop=FALSE, fromLast=FALSE, ...) {
  UseMethod("make.index.unique")
}

make.index.unique.xts <- function(x, eps=0.000001, drop=FALSE, fromLast=FALSE, ...) {
  if( !drop) {
    .Call('make_index_unique', x, eps, PACKAGE="xts")
  } else {
    x[.Call('non_duplicates', .index(x), fromLast, PACKAGE="xts")]
  }
}

make.index.unique.numeric <- function(x, eps=0.000001, drop=FALSE, fromLast=FALSE, ...) {
  if( !drop) {
    .Call('make_unique', x, eps, PACKAGE="xts")
  } else {
    x[.Call('non_duplicates', x, fromLast, PACKAGE="xts")]
  }
}

make.index.unique.POSIXct <- function(x, eps=0.000001, drop=FALSE, fromLast=FALSE, ...) {
  if( !drop) {
    .Call('make_unique', x, eps, PACKAGE="xts")
  } else {
    x[.Call('non_duplicates', x, fromLast, PACKAGE="xts")]
  }
}
