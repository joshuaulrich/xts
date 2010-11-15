#
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.


as.numeric.xts <- function(x, drop=TRUE, ...)
{
  if(drop)
    return(as.numeric(coredata(x)))
  .xts(matrix(as.numeric(coredata(x)),nc=NCOL(x)), .index(x))
}

as.xts.numeric <-
function(x,order.by,dateFormat="POSIXct",frequency=NULL,...) {
  # jmu
  if(missing(order.by)) {
    if(is.null(names(x)))
      stop("order.by must be either 'names()' or otherwise specified")
    else
      # added '...' args to allow for tz specification
      order.by <- do.call(paste('as',dateFormat,sep='.'),list(names(x)))
  }
  xx <- xts(x, order.by=order.by, frequency=frequency,
            .CLASS='numeric', ...)
  return(xx)
}

re.numeric <-
function(x,...) {
  # jmu
  y <- as.numeric(x,...)
  names(y) <- index(x)
  return(y)
}

as.integer.xts <- function(x, drop=TRUE, ...)
{
  if(drop)
    return(as.integer(coredata(x)))
  .xts(matrix(as.integer(coredata(x)),nc=NCOL(x)), .index(x))
}

as.xts.integer <-
function(x,order.by,dateFormat="POSIXct",frequency=NULL,...) {
  # jmu
  if(missing(order.by)) {
    if(is.null(names(x)))
      stop("order.by must be either 'names()' or otherwise specified")
    else
      # added '...' args to allow for tz specification
      order.by <- do.call(paste('as',dateFormat,sep='.'),list(names(x)))
  }
  xx <- xts(x, order.by=order.by, frequency=frequency,
            .CLASS='integer', ...)
  return(xx)
}

re.integer <-
function(x,...) {
  # jmu
  y <- as.integer(x,...)
  names(y) <- index(x)
  return(y)
}

as.double.xts <- function(x, drop=TRUE, ...)
{
  if(drop)
    return(as.double(coredata(x)))
  .xts(matrix(as.double(coredata(x)),nc=NCOL(x)), .index(x))
}

as.xts.double <-
function(x,order.by,dateFormat="POSIXct",frequency=NULL,...) {
  # jmu
  if(missing(order.by)) {
    if(is.null(names(x)))
      stop("order.by must be either 'names()' or otherwise specified")
    else
      # added '...' args to allow for tz specification
      order.by <- do.call(paste('as',dateFormat,sep='.'),list(names(x)))
  }
  xx <- xts(x, order.by=order.by, frequency=frequency,
            .CLASS='double', ...)
  return(xx)
}

re.double <-
function(x,...) {
  # jmu
  y <- as.double(x,...)
  names(y) <- index(x)
  return(y)
}

as.complex.xts <- function(x, drop=TRUE, ...)
{
  if(drop)
    return(as.complex(coredata(x)))
  .xts(matrix(as.complex(coredata(x)),nc=NCOL(x)), .index(x))
}

as.xts.complex <-
function(x,order.by,dateFormat="POSIXct",frequency=NULL,...) {
  # jmu
  if(missing(order.by)) {
    if(is.null(names(x)))
      stop("order.by must be either 'names()' or otherwise specified")
    else
      # added '...' args to allow for tz specification
      order.by <- do.call(paste('as',dateFormat,sep='.'),list(names(x)))
  }
  xx <- xts(x, order.by=order.by, frequency=frequency,
            .CLASS='complex', ...)
  return(xx)
}

re.complex <-
function(x,...) {
  # jmu
  y <- as.complex(x,...)
  names(y) <- index(x)
  return(y)
}

as.logical.xts <- function(x, drop=TRUE, ...)
{
  if(drop)
    return(as.logical(coredata(x)))
  .xts(matrix(as.logical(coredata(x)),nc=NCOL(x)), .index(x))
}

as.xts.logical <-
function(x,order.by,dateFormat="POSIXct",frequency=NULL,...) {
  # jmu
  if(missing(order.by)) {
    if(is.null(names(x)))
      stop("order.by must be either 'names()' or otherwise specified")
    else
      # added '...' args to allow for tz specification
      order.by <- do.call(paste('as',dateFormat,sep='.'),list(names(x)))
  }
  xx <- xts(x, order.by=order.by, frequency=frequency,
            .CLASS='logical', ...)
  return(xx)
}

re.logical <-
function(x,...) {
  # jmu
  y <- as.logical(x,...)
  names(y) <- index(x)
  return(y)
}

