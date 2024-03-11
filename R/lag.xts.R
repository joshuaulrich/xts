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

# TODO: remove
`Lag.xts` <- function(x, k=1, na.action=na.pass, ...) {
  x <- try.xts(x, error=FALSE)
  
  if(!is.xts(x)) x <- as.matrix(x)
  
  xx <-sapply(k, 
         function(k) {
           apply(x, 2, 
             function(x)  {
               if(k==0) return(as.matrix(x)) 
               as.matrix(c(rep(NA, k), x[-((length(x) - k + 1):length(x))]))
             }
           )}
       )
  xx <- matrix(as.numeric(xx),nrow=NROW(x))
  colnames(xx) <- c(paste(colnames(x)[(rep(1:NCOL(x),length(k)))],
                          'lag',
                          rep(k, each=NCOL(x)),
                          sep = "."))
  as.function(na.action)(reclass(xx,x))
}


# TODO: remove
`Next.xts` <- function(x, k=1, na.action=na.pass, ...) {
  x <- try.xts(x, error=FALSE)
  
  if(!is.xts(x)) x <- as.matrix(x)
  
  xx <-sapply(k, 
         function(k) {
           apply(x, 2, 
             function(x)  {
               if(k==0) return(as.matrix(x)) 
               as.matrix(c(x[-(1:k)],rep(NA, k)))
             }
           )}
       )
  xx <- matrix(as.numeric(xx),nrow=NROW(x))
  colnames(xx) <- c(paste(colnames(x)[(rep(1:NCOL(x),length(k)))],
                          'next',
                          rep(k, each=NCOL(x)),
                          sep = "."))
  as.function(na.action)(reclass(xx,x))

}

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

# TODO: remove
lagts.xts <- function(x, k=1, na.pad=TRUE, ...) {
  if(length(k) > 1) {
    if(is.null(names(k)))
      names(k) <- paste("lag",k,sep="")
    return(do.call("merge.xts", lapply(k, lag.xts, x=x, na.pad=na.pad,...)))
  }
  .Call(C_lag_xts, x, k, na.pad)
}

#' Lags and Differences of xts Objects
#' 
#' Methods for computing lags and differences on \code{xts} objects.  This
#' matches most of the functionality of \pkg{zoo} methods, with some default
#' argument changes.
#' 
#' The primary motivation for having methods specific to \code{xts} was to make
#' use of faster C-level code within xts.  Additionally, it was decided that
#' \code{lag}'s default behavior should match the common time-series
#' interpretation of that operator --- specifically that a value at time
#' \sQuote{t} should be the value at time \sQuote{t-1} for a positive lag. This
#' is different than \code{lag.zoo} as well as \code{lag.ts}.
#' 
#' Another notable difference is that \code{na.pad} is set to TRUE by default,
#' to better reflect the transformation visually and within functions requiring
#' positional matching of data.
#' 
#' Backwards compatability with zoo can be achieved by setting
#' \code{options(xts.compat.zoo.lag=TRUE)}. This will change the defaults of
#' lag.xts to k=-1 and na.pad=FALSE.
#' 
#' @param x an \code{xts} object
#' @param k period to lag over
#' @param lag period to difference over
#' @param differences order of differencing
#' @param arithmetic should arithmetic or geometric differencing be used
#' @param log should (geometric) log differences be returned
#' @param na.pad pad vector back to original size
#' @param \dots additional arguments
#' 
#' @return An \code{xts} object reflected the desired lag and/or differencing.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @references \url{https://en.wikipedia.org/wiki/Lag }
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
