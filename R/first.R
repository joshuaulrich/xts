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


#' Return First or Last n Elements of A Data Object
#' 
#' A generic function to return the first or last elements or rows of a vector
#' or two-dimensional data object.
#' 
#' A more advanced subsetting is available for zoo objects with indexes
#' inheriting from POSIXt or Date classes.
#' 
#' Provides the ability to identify the first or last \code{n} rows or
#' observations of a data set.  The generic method behaves much like
#' \code{head} and \code{tail} from \pkg{base}, except by default only the
#' \emph{first} or \emph{last} observation will be returned.
#' 
#' The more useful method for the xts class allows for time based subsetting,
#' given an xtsible object.
#' 
#' \code{n} may be either a numeric value, indicating the number of
#' observations to return - forward from \code{first}, or backwards from
#' \code{last}, or it may be a character string describing the number and type
#' of periods to return.
#' 
#' \code{n} may be positive or negative, in either numeric or character
#' contexts. When positive it will return the result expected - e.g.
#' \code{last(X,'1 month')} will return the last month's data. If negative, all
#' data will be returned \emph{except} for the last month. It is important to
#' note that this is not the same as calling \code{first(X,'1 month')} or
#' \code{first(X,'-1 month')}. All 4 variations return different subsets of
#' data and have distinct purposes.
#' 
#' If \code{n} is a character string, it must be of the form \sQuote{n
#' period.type} or \sQuote{period.type}, where \code{n} is a numeric value
#' (defaults to 1 if not provided) describing the number of \code{period.types}
#' to move forward (first) or back (last).
#' 
#' For example, to return the last 3 weeks of a time oriented zoo object, one
#' could call \code{last(X,'3 weeks')}. Valid period.types are: secs, seconds,
#' mins, minutes, hours, days, weeks, months, quarters, and years.
#' 
#' It is possible to use any frequency specification (secs, mins, days,
#' \ldots{}) for the period.type portion of the string, even if the original
#' data is in a higher frequency. This makes it possible to return the last
#' \sQuote{2 months} of data from an oject that has a daily periodicity.
#' 
#' It should be noted that it is only possible to extract data with methods
#' equal to or less than the frequency of the original data set. Attempting
#' otherwise will result in error.
#' 
#' Requesting more data than is in the original data object will produce a
#' warning advising as such, and the object returned will simply be the
#' original data.
#' 
#' @param x 1 or 2 dimensional data object
#' @param n number of periods to return
#' @param keep should removed values be kept?
#' @param \dots additional args - unused
#' 
#' @return A subset of elements/rows of the original data.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @keywords utilities
#' @examples
#' 
#' first(1:100)
#' last(1:100)
#' 
#' data(LakeHuron)
#' first(LakeHuron,10)
#' last(LakeHuron)
#' 
#' x <- xts(1:100, Sys.Date()+1:100)
#' first(x, 10)
#' first(x, '1 day')
#' first(x, '4 days')
#' first(x, 'month')
#' last(x, '2 months')
#' last(x, '6 weeks')
#' 
`first` <-
function(x,...)
{
  UseMethod("first")
}

#' @rdname first
`first.default` <-
function(x,n=1,keep=FALSE,...)
{
  if(length(x) == 0)
    return(x)
  if(is.character(n)) {
    xx <- try.xts(x, error=FALSE)
    if(is.xts(xx)) {
      xx <- first.xts(x, n=n, keep=keep, ...)
      return(reclass(xx))
    }
  }
  if(is.null(dim(x))) {
    if(n > 0) {
      sub <- seq_len(min(n, length(x)))
      xx <- x[sub]
      if(keep) xx <- structure(xx,keep=x[(-(-n)+1):NROW(x)])
      xx
    } else if(n < 0) {
      sub <- seq.int(to = length(x), length.out = max(length(x)-(-n), 0L))
      xx <- x[sub]
      if(keep) xx <- structure(xx,keep=x[1:(-n)])
      xx
    } else {
      xx <- x[0]
      if(keep) xx <- structure(xx,keep=x[0])
      xx
    }
  } else {
    if(n > 0) {
      sub <- seq_len(min(n, NROW(x)))
      xx <- x[sub,,drop=FALSE]
      if(keep) xx <- structure(xx,keep=x[(-(-n)+1):NROW(x),])
      xx
    } else if(n < 0) {
      sub <- seq.int(to = NROW(x), length.out = max(NROW(x)-(-n), 0L))
      xx <- x[sub,,drop=FALSE]
      if(keep) xx <- structure(xx,keep=x[1:(-n),])
      xx
    } else {
      xx <- x[0,,drop=FALSE]
      if(keep) xx <- structure(xx,keep=x[0,])
      xx
    }
  }
}

#' @rdname first
`first.xts` <-
function(x,n=1,keep=FALSE,...)
{
  if(length(x) == 0)
    return(x)
  if(is.character(n)) {
    # n period set
    np <- strsplit(n," ",fixed=TRUE)[[1]]
    if(length(np) > 2 || length(np) < 1)
      stop(paste("incorrectly specified",sQuote("n"),sep=" "))
    # series periodicity
    sp <- periodicity(x)
    # requested periodicity$units
    sp.units <- sp[["units"]]
    rpu <- np[length(np)]
    rpf <- ifelse(length(np) > 1, as.numeric(np[1]), 1)
    if(rpu == sp.units) {
      n <- rpf
    } else {
      # if singular - add an s to make it work
      if(substr(rpu,length(strsplit(rpu,'')[[1]]),length(strsplit(rpu,'')[[1]])) != 's')
        rpu <- paste(rpu,'s',sep='')
      u.list <- list(secs=4,seconds=4,mins=3,minutes=3,hours=2,days=1,
                     weeks=1,months=1,quarters=1,years=1)
      dt.options <- c('seconds','secs','minutes','mins','hours','days',
                      'weeks','months','quarters','years')
      if(!rpu %in% dt.options)
        stop(paste("n must be numeric or use",paste(dt.options,collapse=',')))
      dt <- dt.options[pmatch(rpu,dt.options)]
      if(u.list[[dt]] > u.list[[sp.units]]) {
        #  req is for higher freq data period e.g. 100 mins of daily data
        stop(paste("At present, without some sort of magic, it isn't possible",
             "to resolve",rpu,"from",sp$scale,"data"))
      }
      ep <- endpoints(x,dt)
      if(rpf > length(ep)-1) {
        rpf <- length(ep)-1
        warning("requested length is greater than original")
      }
      if(rpf > 0) {
        n <- ep[rpf+1]
        if(is.null(dim(x))) {
          xx <- x[1:n]
        } else {
          xx <- x[1:n,,drop=FALSE]
        }
        if(keep) xx <- structure(xx,keep=x[(ep[-(-rpf)+1]+1):NROW(x)])
        return(xx)
      } else if(rpf < 0) {
        n <- ep[-rpf+1]+1
        if(is.null(dim(x))) {
          xx <- x[n:NROW(x)]
        } else {
          xx <- x[n:NROW(x),,drop=FALSE]
        }
        if(keep) xx <- structure(xx,keep=x[1:(ep[-rpf+1])])
        return(xx)
      } else {
        if(is.null(dim(x))) {
          xx <- x[0]
        } else {
          xx <- x[0,,drop=FALSE]
        }
        if(keep) xx <- structure(xx,keep=x[0])
        return(xx)
      }
    }
  }
  if(length(n) != 1) stop("n must be of length 1")
  if(n > 0) {
    n <- min(n, NROW(x))
    if(is.null(dim(x))) {
      xx <- x[1:n]
    } else {
      xx <- x[1:n,,drop=FALSE]
    }
    if(keep) xx <- structure(xx,keep=x[(-(-n)+1):NROW(x)])
    xx
  } else if(n < 0) {
    if(abs(n) >= NROW(x))
      return(x[0])
    if(is.null(dim(x))) {
      xx <- x[(-n+1):NROW(x)]
    } else {
      xx <- x[(-n+1):NROW(x),,drop=FALSE]
    }
    if(keep) xx <- structure(xx,keep=x[1:(-n)])
    xx
  } else {
    if(is.null(dim(x))) {
      xx <- x[0]
    } else {
      xx <- x[0,,drop=FALSE]
    }
    if(keep) xx <- structure(xx,keep=x[0])
    xx
  }
}
