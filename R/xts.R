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


# xts core functions
#   additional methods are in correspondingly named .R files
#   current conversions include:
#     timeSeries, its, irts, ts, matrix, data.frame, and zoo
#     MISSING: fts, tis, fame
#
#  this file includes the main xts constructor as well as the reclass
#  function.
#
#  xts methods (which match foreign conversion methods in other files)
#  are also defined below

`xts` <-
function(x=NULL,
         order.by=index(x),
         frequency=NULL,
         unique=TRUE,
         tzone=Sys.getenv("TZ"),
         ...)
{
  if(is.null(x) && missing(order.by))
    return(structure(.xts(,0),index=integer()))
 
  if(!timeBased(order.by))
    stop("order.by requires an appropriate time-based object")

  #if(NROW(x) != length(order.by))
  if(NROW(x) > 0 && NROW(x) != length(order.by))
    stop("NROW(x) must match length(order.by)")

  orderBy <- class(order.by)

  if(!is.null(x) && !isOrdered(order.by, strictly=!unique) ) {
    indx <- order(order.by)
    if(NCOL(x) > 1 || is.matrix(x)) {
      x <- x[indx,]
    } else x <- x[indx]
    order.by <- order.by[indx]
  }

  if(!is.null(x) || length(x) != 0 ) {
    x <- as.matrix(x)
  } else x <- numeric(0)

  if(!is.null(attr(order.by,"tzone")) && missing(tzone))
    tzone <- attr(order.by, "tzone")
  index <- as.numeric(as.POSIXct(order.by))
  x <- structure(.Data=x,
            index=structure(index,tzone=tzone,tclass=orderBy),
            class=c('xts','zoo'),
            .indexCLASS=orderBy,
            .indexTZ=tzone,
            ...)
  if(!is.null(attributes(x)$dimnames[[1]]))
    # this is very slow if user adds rownames, but maybe that is deserved :)
    dimnames(x) <- dimnames(x) # removes row.names
  x
}

`.xts` <-
function(x=NULL, index, .indexCLASS=c("POSIXt","POSIXct"), tzone=Sys.getenv("TZ"),
        check=TRUE, unique=FALSE, ...) {
  if(check) {
    if( !isOrdered(index, increasing=TRUE, strictly=unique) )
      stop('index is not in',ifelse(unique, 'strictly', ''),'increasing order')
  }
  if(!is.numeric(index) && timeBased(index))
    index <- as.numeric(as.POSIXct(index))
  if(!is.null(x) && NROW(x) != length(index))
    stop("index length must match number of observations")

  if(!is.null(x)) {
    if(!is.matrix(x))
      x <- as.matrix(x)
  } else
  if(length(x) == 0 && !is.null(x)) {
    x <- vector(storage.mode(x))
  } else x <- numeric(0)

  structure(.Data=x,
            index=structure(index,tzone=tzone,tclass=.indexCLASS),
            .indexCLASS=.indexCLASS,.indexTZ=tzone,
            class=c('xts','zoo'), ...)
}

`reclass` <-
function(x, match.to, error=FALSE, ...) {
  if(!missing(match.to) && is.xts(match.to)) {
    if(NROW(x) != length(.index(match.to)))
      if(error) {
        stop('incompatible match.to attibutes')
      } else return(x)

    if(!is.xts(x)) x <- .xts(coredata(x),.index(match.to), .indexCLASS=indexClass(match.to))
    CLASS(x) <- CLASS(match.to)
    xtsAttributes(x) <- xtsAttributes(match.to)
  }
  oldCLASS <- CLASS(x)
  # should this be is.null(oldCLASS)?
  if(length(oldCLASS) > 0 && !inherits(oldClass,'xts')) {  
    if(!is.null(dim(x))) {
      if(!is.null(attr(x,'.ROWNAMES'))) {
        rownames(x) <- attr(x,'.ROWNAMES')[1:NROW(x)]
      } #else rownames(x) <- NULL
    }
    attr(x,'.ROWNAMES') <- NULL
    #if(is.null(attr(x,'.RECLASS')) || attr(x,'.RECLASS')) {#should it be reclassed?
    if(!is.null(attr(x,'.RECLASS')) && attr(x,'.RECLASS')) {#should it be reclassed?
      #attr(x,'.RECLASS') <- NULL
      do.call(paste('re',oldCLASS,sep='.'),list(x))
    } else {
      #attr(x,'.RECLASS') <- NULL
      x
    }
  } else {
    #attr(x,'.RECLASS') <- NULL
    x
  }
}

#`reclass` <- reclass2

`CLASS` <-
function(x) {
  cl <- attr(x,'.CLASS')

  if(!is.null(cl))
    return(structure(cl,class='CLASS'))

  return(NULL)
}

`print.CLASS` <-
function(x,...) {
  cat(paste("previous class:",x),"\n")
}

`CLASS<-` <-
function(x,value) {
  UseMethod("CLASS<-")
}

`CLASS<-.xts` <-
function(x,value) {
  attr(x,".CLASS") <- value
  x
}

`is.xts` <-
function(x) {
  inherits(x,'xts') &&
  is.numeric(.index(x)) &&
  !is.null(indexClass(x))
}

`as.xts` <-
function(x,...) {
  UseMethod('as.xts')
}

#as.xts.default <- function(x, ...) x

`re.xts` <-
function(x,...) {
  # simply return the object
  return(x)
}

`as.xts.xts` <-
function(x,...) {
  # Cannot use 'zoo()' on objects of class 'zoo' or '.CLASS' (etc.?)
  # Is the equivalent of a 'coredata.xts' needed? - jmu
  #yy <- coredata(x)
  #attr(yy, ".CLASS") <- NULL
  # using new coredata.xts method - jar
  if(length(x) == 0 && (!is.null(index(x)) && length(index(x))==0))
    return(x)
  xx <- xts(coredata(x),
            order.by=index(x),
            .CLASS='xts',
            ...)
  xx
}

`xts.to.xts` <-
function(x,...) {
  return(x)
}
