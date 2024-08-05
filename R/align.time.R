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


#' Align seconds, minutes, and hours to beginning of next period.
#' 
#' Change timestamps to the start of the next period, specified in multiples of
#' seconds.
#' 
#' This function is an S3 generic. The result is to round up to the next period
#' determined by 'n modulo x'.
#' 
#' @param x Object containing timestamps to align.
#' @param n Number of seconds to adjust by.
#' @param \dots Additional arguments. See details.
#' 
#' @return A new object with the same class as `x`.
#' 
#' @author Jeffrey A. Ryan with input from Brian Peterson
#' 
#' @seealso [`to.period()`]
#' 
#' @keywords chron manip ts misc
#' @examples
#' 
#' x <- Sys.time() + 1:1000
#' 
#' # every 10 seconds
#' align.time(x, 10)
#' 
#' # align to next whole minute
#' align.time(x, 60)
#' 
#' # align to next whole 10 min interval
#' align.time(x, 10 * 60)
#' 
align.time <- function(x, ...) {
  UseMethod("align.time")
}

#' @rdname align.time
align.time.xts <- function(x, n=60, ...) {
  if(n <= 0) stop("'n' must be positive")
  .xts(x, .index(x) + (n-.index(x) %% n), tzone=tzone(x), tclass=tclass(x))
}

align.time.POSIXct <- function(x, n=60, ...) {
  if(n <= 0) stop("'n' must be positive")
  structure(unclass(x) + (n - unclass(x) %% n),class=c("POSIXct","POSIXt"))
}

align.time.POSIXlt <- function(x, n=60, ...) {
  if(n <= 0) stop("'n' must be positive")
  as.POSIXlt(align.time(as.POSIXct(x),n=n,...))
}

#' @rdname align.time
shift.time <- function(x, n=60, ...) {
  UseMethod("shift.time")
}

shift.time.xts <- function(x, n=60, ...) {
  .xts(x, .index(x) + n, tzone=tzone(x), tclass=tclass(x))
}

#' @rdname make.index.unique
is.index.unique <- function(x) {
  UseMethod("is.time.unique")
}
#' @rdname make.index.unique
is.time.unique <- is.index.unique

is.time.unique.xts <- function(x) {
  isOrdered(.index(x), strictly=TRUE)
}

is.time.unique.zoo <- function(x) {
  isOrdered(index(x), strictly=TRUE)
}

#' Force Time Values To Be Unique
#' 
#' A generic function to force sorted time vectors to be unique. Useful for
#' high-frequency time-series where original time-stamps may have identical
#' values. For the case of xts objects, the default `eps` is set to ten
#' microseconds. In practice this advances each subsequent identical time by
#' `eps` over the previous (possibly also advanced) value.
#' 
#' The returned time-series object will have new time-stamps so that
#' `isOrdered(.index(x))` evaluates to `TRUE`.
#' 
#' @param x An xts object, or POSIXct vector.
#' @param eps A value to add to force uniqueness.
#' @param drop Should duplicates be dropped instead of adjusted by `eps`?
#' @param fromLast When `drop = TRUE`, `fromLast` controls which duplicated
#'   times are dropped. When `fromLast = FALSE`, the earliest observation with
#'   an identical timestamp is kept and subsequent observations are dropped.
#' @param \dots Unused.
#' 
#' @return A modified version of `x` with unique timestamps.
#' 
#' @note Incoming values must be pre-sorted, and no check is done to make sure
#'   that this is the case.  \sQuote{integer} index value will be coerced to
#'   \sQuote{double} when `drop = FALSE`.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`align.time()`]
#' 
#' @rdname make.index.unique
#' @keywords ts
#' @examples
#' 
#' ds <- options(digits.secs=6) # so we can see the change
#' 
#' x <- xts(1:10, as.POSIXct("2011-01-21") + c(1,1,1,2:8)/1e3)
#' x
#' make.index.unique(x)
#' 
#' options(ds)
#' 
make.index.unique <- function(x, eps=0.000001, drop=FALSE, fromLast=FALSE, ...) {
  UseMethod("make.index.unique")
}

#' @rdname make.index.unique
make.time.unique <- make.index.unique

make.index.unique.xts <- function(x, eps=0.000001, drop=FALSE, fromLast=FALSE, ...) {
  if( !drop) {
    .Call(C_make_index_unique, x, eps)
  } else {
    x[.Call(C_non_duplicates, .index(x), fromLast)]
  }
}

make.index.unique.numeric <- function(x, eps=0.000001, drop=FALSE, fromLast=FALSE, ...) {
  if( !drop) {
    .Call(C_make_unique, x, eps)
  } else {
    x[.Call(C_non_duplicates, x, fromLast)]
  }
}

make.index.unique.POSIXct <- function(x, eps=0.000001, drop=FALSE, fromLast=FALSE, ...) {
  if( !drop) {
    .Call(C_make_unique, x, eps)
  } else {
    x[.Call(C_non_duplicates, x, fromLast)]
  }
}
