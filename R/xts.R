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

`is.xts` <-
function(x) {
  inherits(x,'xts')
}

`as.xts` <-
function(x,...) {
  UseMethod('as.xts')
}

`as.xts.ts` <-
function(x,dateFormat='Date',...) {
  x.mat <- as.matrix(x)
  order.by <- do.call(paste('as',dateFormat,sep='.'),list(index(x.mat)))
  xx <- xts(as.matrix(x),
            order.by=order.by,
            .CLASS='ts',
            ...)
  xx
}

`as.xts.xts` <-
function(x,...) {
  xx <- xts(x,
            order.by=index(x),
            .CLASS='xts',
            ...)
  xx
}

`as.xts.zoo` <-
function(x,order.by=index(x),frequency=NULL,...) {
  xx <- xts(x,
            order.by=order.by,
            frequency=frequency,
            .CLASS='zoo',
            ...)
  xx
}

`as.xts.data.frame` <-
function(x,order.by,dateFormat="POSIXct",frequency=NULL,...) {
  if(missing(order.by)) 
    order.by <- do.call(paste('as',dateFormat,sep='.'),list(rownames(x)))
  xx <- xts(x,
            order.by=order.by,
            frequency=frequency,
            .CLASS='data.frame',
            ...)
  xx
}

`as.xts.its` <-
function(x,...) {
  xx <- xts(x@.Data,
            order.by=x@dates,
            .CLASS='its',
            ...)
  xx
}

`as.xts.matrix` <-
function(x,order.by,frequency=NULL,...) {
  if(missing(order.by)) stop("order.by must be specified")
  xx <- xts(x,
            order.by=order.by,
            frequency=frequency,
            .CLASS='matrix',
            ...)
  xx
}

`re.timeSeries` <-
function(x,...) {
  stopifnot("package:fCalendar" %in% search() || require("fCalendar", quietly=TRUE))
  x.attr <- attributes(x)
  xx <- structure(x,dimnames=x.attr$dimnames,index=x.attr$index)
  original.attr <- attributes(x)[!names(attributes(x)) %in% 
                                 c("dim","dimnames","index","class")]
  for(i in names(original.attr)) {
    attr(xx,i) <- NULL
  }

  timeSeries(xx,charvec=rownames(xx),format=x.attr$format,
             zone=x.attr$FinCenter,FinCenter=x.attr$FinCenter,
             recordIDs=x.attr$recordIDs,title=x.attr$title,
             documentation=x.attr$documentation,...)
}

`as.xts.timeSeries` <-
function(x,dateFormat="POSIXct",FinCenter,recordIDs,title,documentation,...) {

  if(missing(FinCenter))
    FinCenter <- x@FinCenter
  if(missing(recordIDs))
    recordIDs <- x@recordIDs
  if(missing(title)) 
    title <- x@title
  if(missing(documentation)) 
    documentation <- x@documentation

  order.by <- do.call(paste('as',dateFormat,sep='.'),list(x@positions))

  xts(as.matrix(x@Data),
      order.by=order.by,
      format=x@format,
      FinCenter=FinCenter,
      recordIDs=recordIDs,
      title=title,
      documentation=documentation,
      .CLASS='timeSeries',
      ...)
}

