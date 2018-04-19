#
#   xts: eXtensible time-series
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.


align.time <- function(x, ...) {
  UseMethod("align.time")
}

align.time.xts <- function(x, n=60, ...) {
  if(n <= 0) stop("'n' must be positive")
  .xts(x, .index(x) + (n-.index(x) %% n), tzone=tzone(x), tclass=indexClass(x))
}

align.time.POSIXct <- function(x, n=60, ...) {
  if(n <= 0) stop("'n' must be positive")
  structure(unclass(x) + (n - unclass(x) %% n),class=c("POSIXct","POSIXt"))
}

align.time.POSIXlt <- function(x, n=60, ...) {
  if(n <= 0) stop("'n' must be positive")
  as.POSIXlt(align.time(as.POSIXct(x),n=n,...))
}

shift.time <- function(x, n=60, ...) {
  UseMethod("shift.time")
}

shift.time.xts <- function(x, n=60, ...) {
  .xts(x, .index(x) + n, tzone=tzone(x), tclass=indexClass(x))
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
