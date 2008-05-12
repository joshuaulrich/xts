# xts core functions
#   additional methods are in correspondingly named .R files
#   current conversions include:
#     timeSeries, its, irts, ts, matrix, data.frame, and zoo
#
#  this file includes the main xts constructor as well as the reclass
#  function.
#
#  xts methods (which match foreign conversion methods in other files)
#  are also defined below

`xts` <-
function(x=NULL,order.by=index(x),frequency=NULL,...) {
  if(!any(sapply(c('Date','POSIXct','chron','dates','times','timeDate','yearmon','yearqtr'),
     function(xx) inherits(order.by,xx)))) {
    stop("order.by requires an appropriate time-based object")
  }
    orderBy <- class(order.by)
    if('timeDate' %in% orderBy) {
      z <- structure(zoo(x=coredata(x),
                     order.by=as.POSIXct(order.by),
                     frequency=frequency),
                     class=c('xts','zoo'),...)
      indexClass(z) <- 'timeDate'
    } else {
      z <- structure(zoo(x=x,
                     order.by=order.by,
                     frequency=frequency),
                     class=c('xts','zoo'),...)
    }
#  z <- zoo(x=x, order.by=order.by, frequency=frequency)
#  z <- structure(z,class=c('xts','zoo'),...)
  if(!is.null(dim(x))) {
    attr(z,'.ROWNAMES') <- dimnames(z)[[1]]
    rownames(z) <- as.character(index(z))
  }
  return(z)
}

`reclass` <-
function(x) {
  old.class <- CLASS(x)
  if(length(old.class) > 0) {
    if(!is.null(dim(x))) {
      if(!is.null(attr(x,'.ROWNAMES'))) {
        rownames(x) <- attr(x,'.ROWNAMES')[1:NROW(x)]
      } else rownames(x) <- NULL
    }
    attr(x,'.ROWNAMES') <- NULL
    do.call(paste('re',old.class,sep='.'),list(x))
  } else x
}

`reclass2` <-
function(x, match.to, ...) {
  if(!missing(match.to) && is.xts(match.to)) {
    if(NROW(x) != length(index(match.to)))
      stop('incompatible match.to attibutes')
    if(!is.xts(x)) x <- xts(x,index(match.to))
    CLASS(x) <- CLASS(match.to)
    xtsAttributes(x) <- xtsAttributes(match.to)
  }
  oldCLASS <- CLASS(x)
  if(length(oldCLASS) > 0) {
    if(!is.null(dim(x))) {
      if(!is.null(attr(x,'.ROWNAMES'))) {
        rownames(x) <- attr(x,'.ROWNAMES')[1:NROW(x)]
      } else rownames(x) <- NULL
    }
    attr(x,'.ROWNAMES') <- NULL
    do.call(paste('re',oldCLASS,sep='.'),list(x))
  } else x
}

`reclass` <- reclass2

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
  inherits(x,'xts')
}

`as.xts` <-
function(x,...) {
  UseMethod('as.xts')
}

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
