# xts core functions
#   additional methods are in correspondingly named .R files
#   current conversions include:
#     timeSeries, its, ts, matrix, data.frame, and zoo
#
#  this file includes the main xts constructor as well as the reclass
#  function.
#
#  xts methods (which match foreign conversion methods in other files)
#  are also defined below

`xts` <-
function(x,order.by=index(x),frequency=NULL,...) {
  if(!inherits(order.by,'Date') 
     & !inherits(order.by,'POSIXct'))
  {
    stop("order.by requires a time-based object of either class POSIXct or Date")
  }

  z <- zoo(x=x,order.by=order.by,frequency=frequency)
  z <- structure(z,class=c('xts','zoo'),...)

  return(z)
}

`reclass` <-
function(x) {
  old.class <- attr(x,'.CLASS')
  do.call(paste('re',old.class,sep='.'),list(x))
}

`CLASS` <-
function(x) {
  attr(x,'.CLASS')
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
