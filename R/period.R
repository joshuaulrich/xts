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


#' Check if Class is Time-Based
#'
#' Used to verify that the object is one of the known time-based classes in R.
#' Current time-based objects supported are `Date`, `POSIXct`, `chron`,
#' `yearmon`, `yearqtr`, and `timeDate`.
#'
#' @param x Object to test.
#'
#' @return A logical scalar.
#'
#' @author Jeffrey A. Ryan
#'
#' @rdname timeBased
#' @keywords utilities
#' @examples
#'
#' timeBased(Sys.time())
#' timeBased(Sys.Date())
#' 
#' timeBased(200701)
#'
`is.timeBased` <-
function(x) {
  time.classes <-
    c("Date", "POSIXt", "chron", "dates", "times", "timeDate",
      "yearmon", "yearqtr", "xtime")
  inherits(x, time.classes)
}

#' @rdname timeBased
`timeBased` <- `is.timeBased`


#' Optimized Calculations By Period
#' 
#' Calculate a sum, product, minimum, or maximum for each non-overlapping
#' period specified by `INDEX`.
#' 
#' These functions are similar to calling `period.apply()` with the same
#' endpoints and function. There may be slight differences in the results due
#' to numerical accuracy.
#' 
#' For xts-coercible objects, an appropriate `INDEX` can be created by a call
#' to `endpoints()`.
#' 
#' @param x A univariate data object.
#' @param INDEX A numeric vector of endpoints for each period.
#' 
#' @return An xts or zoo object containing the sum, product, minimum, or
#' maximum for each endpoint in `INDEX`.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`endpoints()`], [`period.apply()`]
#' 
#' @keywords utilities
#' @rdname period_math
#' @examples
#' 
#' x <- c(1, 1, 4, 2, 2, 6, 7, 8, -1, 20)
#' i <- c(0, 3, 5, 8, 10)
#' 
#' period.sum(x, i)
#' period.prod(x, i)
#' period.min(x, i)
#' period.max(x, i)
#' 
#' data(sample_matrix)
#' y <- sample_matrix[, 1]
#' ep <- endpoints(sample_matrix)
#'
#' period.sum(y, ep)
#' period.sum(as.xts(y), ep)
#' 
#' period.prod(y, ep)
#' period.prod(as.xts(y), ep)
#' 
#' period.min(y, ep)
#' period.min(as.xts(y), ep)
#' 
#' period.max(y, ep)
#' period.max(as.xts(y), ep)
#' 
`period.sum` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")
  if(min(INDEX) < 0 || max(INDEX) > NROW(x)) stop("INDEX must be >= 0 and <= nrow(x)")
  ep <- as.integer(INDEX)
  if(ep[1L] != 0L) ep <- c(0L,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(x)
  xa <- .Call(C_xts_period_sum, xx, ep)

  if(timeBased(index(x))) {
    tz <- xts(xa, index(x)[ep[-1]])
  } else {
    tz <- zoo(xa, index(x)[ep[-1]])
  }
  tz
}

#' @rdname period_math
`period.prod` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")
  if(min(INDEX) < 0 || max(INDEX) > NROW(x)) stop("INDEX must be >= 0 and <= nrow(x)")
  ep <- as.integer(INDEX)
  if(ep[1] != 0L) ep <- c(0L,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(x)
  xa <- .Call(C_xts_period_prod, xx, ep)

  if(timeBased(index(x))) {
    tz <- xts(xa, index(x)[ep[-1]])
  } else {
    tz <- zoo(xa, index(x)[ep[-1]])
  }
  tz
}

#' @rdname period_math
`period.max` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")
  if(min(INDEX) < 0 || max(INDEX) > NROW(x)) stop("INDEX must be >= 0 and <= nrow(x)")
  ep <- as.integer(INDEX)
  if(ep[1] != 0L) ep <- c(0L,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(x)
  xa <- .Call(C_xts_period_max, xx, ep)

  if(timeBased(index(x))) {
    tz <- xts(xa, index(x)[ep[-1]])
  } else {
    tz <- zoo(xa, index(x)[ep[-1]])
  }
  tz
}

#' @rdname period_math
`period.min` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")
  if(min(INDEX) < 0 || max(INDEX) > NROW(x)) stop("INDEX must be >= 0 and <= nrow(x)")
  ep <- as.integer(INDEX)
  if(ep[1] != 0L) ep <- c(0L,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(x)
  xa <- .Call(C_xts_period_min, xx, ep)

  if(timeBased(index(x))) {
    tz <- xts(xa, index(x)[ep[-1]])
  } else {
    tz <- zoo(xa, index(x)[ep[-1]])
  }
  tz
}
