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


#' @rdname diff.xts
lag.xts <- function(x, k=1, na.pad=TRUE, ...) {
  zooCompat <- getOption('xts.compat.zoo.lag')
  if(is.logical(zooCompat) && zooCompat) {
    k <- -k
    if(missing(na.pad)) na.pad <- FALSE
  }
  if(length(k) > 1) {
    if(is.null(names(k)))
      names(k) <- paste("lag",k,sep="")
    return(do.call("merge.xts", lapply(k, lag.xts, x=x, na.pad=na.pad,...)))
  }
  .Call(C_lag_xts, x, k, na.pad)
}

lagts.xts <- function(x, k=1, na.pad=TRUE, ...) {
  # NOTE: not exported
  if(length(k) > 1) {
    if(is.null(names(k)))
      names(k) <- paste("lag",k,sep="")
    return(do.call("merge.xts", lapply(k, lag.xts, x=x, na.pad=na.pad,...)))
  }
  .Call(C_lag_xts, x, k, na.pad)
}

#' Lags and Differences of xts Objects
#' 
#' Methods for computing lags and differences on xts objects. This provides
#' similar functionality as the \pkg{zoo} counterparts, but with some different
#' defaults.
#' 
#' The primary motivation for these methods was to take advantage of a faster
#' C-level implementation. Another motivation was to make `lag()` behave using
#' standard sign for `k`. Both [zoo's lag() method][zoo::lag.zoo] and [`lag.default()`] require a
#' *negative* value for `k` in order to shift a series backward. So `k = 1`,
#' shifts the series *forward* one observation. This is especially confusing
#' because `k = 1` is the default for those functions. When `x` is an xts
#' object, `lag(x, 1)` returns an object where the value at time 't' is the
#' value at time 't-1' in the original object.
#'
#' Another difference is that `na.pad = TRUE` by default, to better reflect the
#' transformation visually and for functions the require positional alignment
#' of data.
#' 
#' Set `options(xts.compat.zoo.lag = TRUE)` to use make `lag.xts()` consistent
#' with `lag.zoo()` by reversing the sign of `k` and setting `na.pad = FALSE`.
#' 
#' @param x An xts object.
#' @param k Number of periods to shift.
#' @param lag Period to difference over.
#' @param differences Order of differencing.
#' @param arithmetic Should arithmetic or geometric differencing be used?
#' @param log Should (geometric) log differences be returned?
#' @param na.pad Should `NA` be added so the result has the same number of
#'   observations as `x`?
#' @param \dots Additional arguments.
#' 
#' @return An xts object with the desired lag and/or differencing.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @references <https://en.wikipedia.org/wiki/Lag>
#' 
#' @keywords manip chron
#' @examples
#' 
#' x <- xts(1:10, Sys.Date()+1:10)
#' lag(x)    # currently using xts-style positive k 
#' 
#' lag(x, k=2)
#' 
#' lag(x, k=-1, na.pad=FALSE) # matches lag.zoo(x, k=1)
#' 
#' diff(x)
#' diff(x, lag=1)
#' diff(x, diff=2)
#' diff(diff(x))
#' 
diff.xts <- function(x, lag=1, differences=1, arithmetic=TRUE, log=FALSE, na.pad=TRUE, ...)
{
  if(!is.integer(lag) && any(is.na(as.integer(lag))))
    stop("'lag' must be integer")

  differences <- as.integer(differences[1L])
  if(is.na(differences))
    stop("'differences' must be integer")

  if(is.logical(x)) {
    x <- .xts(matrix(as.integer(x), ncol=NCOL(x)), .index(x),
              tclass(x), dimnames=dimnames(x))
  }

  if(lag < 1 || differences < 1)
    stop("'diff.xts' defined only for positive lag and differences arguments")

  zooCompat <- getOption('xts.compat.zoo.lag')
  if(is.logical(zooCompat) && zooCompat) {
    # this has to negated to satisfy the test in lag.xts... oh my
    lag <- -lag
    if(missing(na.pad)) na.pad <- FALSE
  }

  if(differences > 1) {
    if(arithmetic && !log) { #log is FALSE or missing
      x <- x - lag.xts(x, k=lag, na.pad=na.pad)
    } else {
      if(log) {
        x <- log(x/lag.xts(x, k=lag, na.pad=na.pad))
      } else x <- x/lag.xts(x, k=lag, na.pad=na.pad)
    }
    diff(x, lag, differences=differences-1, arithmetic=arithmetic, log=log, na.pad=na.pad, ...)
  } else {
    if(arithmetic && !log) {
      x - lag.xts(x, k=lag, na.pad=na.pad)
    } else {
      if(log) {
        log(x/lag.xts(x, k=lag, na.pad=na.pad))
      } else x/lag.xts(x, k=lag, na.pad=na.pad)
    }
  }
}
