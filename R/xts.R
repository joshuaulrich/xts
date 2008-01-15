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
function(x,order.by=index(x),frequency=NULL,...) {
  if(!inherits(order.by,'Date') 
     & !inherits(order.by,'POSIXct')
     & !inherits(order.by,'timeDate')
     & !inherits(order.by,'yearmon') & !inherits(order.by,'yearqtr'))
  {
    #if(length(order.by)==1) { # a number indicating the column, or column name
      #order.by <- x[,order.by]
    #}
    #order.by <- do.call(paste('as',indexClass,sep='.'),list(order.by,...))
    stop("order.by requires a time-based object of class POSIXct or Date")
  }

  z <- zoo(x=x,order.by=order.by,frequency=frequency)
  rownames(z) <- as.character(as.POSIXct(order.by))
  z <- structure(z,class=c('xts','zoo'),...)

  return(z)
}

`reclass` <-
function(x) {
  old.class <- CLASS(x)
  if(!is.null(old.class)) {
    do.call(paste('re',old.class,sep='.'),list(x))
  } else x
}

`CLASS` <-
function(x) {
  cl <- attr(x,'.CLASS')
  return(structure(cl,class='CLASS'))
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
