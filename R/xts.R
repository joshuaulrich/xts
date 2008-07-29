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
function(x=NULL,order.by=index(x),frequency=NULL, row.names=TRUE, ...) {
  if(!timeBased(order.by))
    stop("order.by requires an appropriate time-based object")

  orderBy <- class(order.by)

  z <- structure(zoo(x=x,
                 order.by=as.numeric(as.POSIXct(order.by)),
                 frequency=frequency),
                 class=c('xts','zoo'), .indexCLASS=orderBy, 
                 dimnames=list(NULL,colnames(x)), ...)
  if(!is.null(dim(x)) && row.names) {
    attr(z,'.ROWNAMES') <- dimnames(z)[[1]]
    rownames(z) <- as.character(index(z))
  }

  return(z)
}

`.xts` <-
function(x=NULL, index, indexCLASS,  row.names=FALSE, check=FALSE, ...) {
  if(check) {
    ind <- order(index)
    rindex <- rindex[ind]
    x <- x[ind, drop=FALSE]
  }
  if(!is.numeric(index))
    index <- as.numeric(index)

  xx <- structure(.Data=x,
            index=index,
            .indexCLASS=indexCLASS,
            class=c('xts','zoo'))
  if(row.names) {
    #???
  }
  xx
}

`reclass` <-
function(x, match.to, error=FALSE, ...) {
  if(!missing(match.to) && is.xts(match.to)) {
    if(NROW(x) != length(index(match.to)))
      if(error) {
        stop('incompatible match.to attibutes')
      } else return(x)

    if(!is.xts(x)) x <- xts(coredata(x),index(match.to))
    CLASS(x) <- CLASS(match.to)
    xtsAttributes(x) <- xtsAttributes(match.to)
  }
  oldCLASS <- CLASS(x)
  # should this be is.null(oldCLASS)?
  if(length(oldCLASS) > 0 && !inherits(oldClass,'xts')) {  
    if(!is.null(dim(x))) {
      if(!is.null(attr(x,'.ROWNAMES'))) {
        rownames(x) <- attr(x,'.ROWNAMES')[1:NROW(x)]
      } else rownames(x) <- NULL
    }
    attr(x,'.ROWNAMES') <- NULL
    if(is.null(attr(x,'.RECLASS')) || attr(x,'.RECLASS')) {#should it be reclassed?
      attr(x,'.RECLASS') <- NULL
      do.call(paste('re',oldCLASS,sep='.'),list(x))
    } else {
      attr(x,'.RECLASS') <- NULL
      x
    }
  } else {
    attr(x,'.RECLASS') <- NULL
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
