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


# optimized periodic apply functions
#

#' Check if Class is Time-Based
#'
#' Used to verify that the object is one of the known time-based classes in R.
#' Current time-based objects supported are \code{Date}, \code{POSIXct},
#' \code{chron}, \code{yearmon}, \code{yearqtr}, and \code{timeDate}.
#'
#' @param x object to test
#'
#' @return a logical scalar
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

make.timeBased <- function(x, class='POSIXct', ...)
{
  do.call(class, list(x,...))
}

#' Calculate Sum By Period
#' 
#' Calculate a sum for each period of INDEX. Essentially a rolling application
#' of sum over a series of non-overlapping sections.
#' 
#' Used to calculate a sum per period given an arbitrary index of sections to
#' be calculated over. This is an optimized function for sum.  There are
#' additionally optimized versions for min, max, and prod.
#' 
#' For xts-coercible objects, an appropriate INDEX can be derived from a call
#' to \code{endpoints}.
#' 
#' @param x a univariate data object
#' @param INDEX a numeric vector of endpoints to calculate sum on
#' 
#' @return An \code{xts} or \code{zoo} object of sums, indexed by the period
#' endpoints.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{endpoints}}, \code{\link{period.max}},
#' \code{\link{period.min}}, \code{\link{period.prod}}
#' 
#' @keywords utilities
#' @examples
#' 
#' period.sum(c(1,1,4,2,2,6,7,8,-1,20),c(0,3,5,8,10))
#' 
#' data(sample_matrix)
#' period.sum(sample_matrix[,1],endpoints(sample_matrix))
#' period.sum(as.xts(sample_matrix)[,1],endpoints(sample_matrix))
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


#' Calculate Product By Period
#' 
#' Calculate a product for each period of INDEX. Essentially a rolling
#' application of prod over a series of non-overlapping sections.
#' 
#' Used to calculate a product per period given an arbitrary index of sections
#' to be calculated over. This is an optimized function for product.  There are
#' additionally optimized versions for min, max, and sum.
#' 
#' For xts-coercible objects, an appropriate INDEX can be derived from a call
#' to \code{endpoints}.
#' 
#' @param x a univariate data object
#' @param INDEX a vector of breakpoints to calculate product on
#' 
#' @return An \code{xts} or \code{zoo} object of products, indexed by the
#' period endpoints.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{endpoints}}, \code{\link{period.sum}},
#' \code{\link{period.min}}, \code{\link{period.max}}
#' 
#' @keywords utilities
#' @examples
#' 
#' period.prod(c(1,1,4,2,2,6,7,8,-1,20),c(0,3,5,8,10))
#' 
#' data(sample_matrix)
#' period.prod(sample_matrix[,1],endpoints(sample_matrix))
#' period.prod(as.xts(sample_matrix)[,1],endpoints(sample_matrix))
#' 
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


#' Calculate Max By Period
#' 
#' Calculate a maximum for each period of INDEX. Essentially a rolling
#' application of maximum over a series of non-overlapping sections.
#' 
#' Used to calculate a maximum per period given an arbitrary index of sections
#' to be calculated over. This is an optimized function for maximum.  There are
#' additional optimized versions for min, sum, and prod.
#' 
#' For xts-coercible objects, an appropriate INDEX can be derived from a call
#' to 'endpoints'.
#' 
#' @param x a univariate data object
#' @param INDEX a numeric vector of endpoints to calculate maximum on
#' 
#' @return An xts or zoo object of maximums, indexed by the period endpoints.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{endpoints}}, \code{\link{period.sum}},
#' \code{\link{period.min}}, \code{\link{period.prod}}
#' 
#' @keywords utilities
#' @examples
#' 
#' period.max(c(1,1,4,2,2,6,7,8,-1,20),c(0,3,5,8,10))
#' 
#' data(sample_matrix)
#' period.max(sample_matrix[,1],endpoints(sample_matrix))
#' period.max(as.xts(sample_matrix)[,1],endpoints(sample_matrix))
#' 
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


#' Calculate Min By Period
#' 
#' Calculate a minimum for each period of INDEX. Essentially a rolling
#' application of minimum over a series of non-overlapping sections.
#' 
#' Used to calculate a minimum per period given an arbitrary index of sections
#' to be calculated over. This is an optimized function for minimum. There are
#' additional optimized versions for max, sum, and prod.
#' 
#' For xts-coercible objects, an appropriate INDEX can be derived from a call
#' to \code{endpoints}.
#' 
#' @param x a univariate data object
#' @param INDEX a numeric vector of endpoints to calculate maximum on
#' 
#' @return An xts or zoo object of minimums, indexed by the period endpoints.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{endpoints}}, \code{\link{period.sum}},
#' \code{\link{period.max}}, \code{\link{period.prod}}
#' 
#' @keywords utilities
#' @examples
#' 
#' period.min(c(1,1,4,2,2,6,7,8,-1,20),c(0,3,5,8,10))
#' 
#' data(sample_matrix)
#' period.min(sample_matrix[,1],endpoints(sample_matrix))
#' period.min(as.xts(sample_matrix)[,1],endpoints(sample_matrix))
#' 
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
