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

`re.ts` <-
function(x,...) {
  ts(coredata(x),
     start=as.numeric(start(x)),
     end=as.numeric(end(x)),
     frequency=attr(x,'frequency'),
     names=colnames(x))
}

`as.xts.ts` <-
function(x,dateFormat,...) {
  x.mat <- structure(as.matrix(x),dimnames=dimnames(x))
  colnames(x.mat) <- colnames(x)

  # quick hueristic - if numeric index is larger than one
  # full day of seconds (60*60*24) than use POSIXct, otherwise
  # assume we are counting my days, not seconds, and use Date -jar
  if(missing(dateFormat)) {
    dateFormat <- ifelse(max(time(x)) > 86400,'POSIXct','Date')
  }

  # added '...' to call for handling of tz params -jar
  # now using time() to extract time from tsp
  order.by <- do.call(paste('as',dateFormat,sep='.'),list(as.numeric(time(x)),...))
  xx <- xts(x.mat,
            order.by=order.by,
            frequency=frequency(x),
            .CLASS='ts',
            ...)
  xx
}

`re.xts` <-
function(x,...) {
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

`re.zoo` <-
function(x,...) {
  zoo(coredata(x),
      order.by=index(x),
      ...)
}

`as.xts.zoo` <-
function(x,order.by=index(x),frequency=NULL,...) {
  xx <- xts(coredata(x),          # Cannot use 'zoo()' on objects of class 'zoo' - jmu
            order.by=order.by,
            frequency=frequency,
            .CLASS='zoo',
            ...)
  xx
}

`re.data.frame` <-
function(x,...) {
  data.frame(x,...)
}

`as.xts.data.frame` <-
function(x,order.by,dateFormat="POSIXct",frequency=NULL,...) {
  # Should allow 'order.by' to be a vector of dates or a scaler
  # representing the column number to use.
  if(missing(order.by)) # added '...' args to allow for tz specification
    order.by <- do.call(paste('as',dateFormat,sep='.'),list(rownames(x),...))
  xx <- xts(x,
            order.by=order.by,
            frequency=frequency,
            .CLASS='data.frame',
            ...)
  xx
}

`re.its` <-
function(x, ...) {
  stopifnot("package:its" %in% search() || require("its",quietly=TRUE))
  xx <- coredata(x)
  dates <- attr(x,'index')
  its(xx,dates=dates,...)
}

`as.xts.its` <-
function(x,...) {
  xx <- xts(x@.Data,
            order.by=x@dates,
            .CLASS='its',
            ...)
  xx
}

`re.matrix` <-
function(x,...) {
  as.matrix(x,...)
}

`as.xts.matrix` <-
function(x,order.by,dateFormat="POSIXct",frequency=NULL,...) {
  # Should allow 'order.by' to be a vector of dates or a scaler
  # representing the column number to use.
  if(missing(order.by)) {
    # The 'index' of zoo objects is set to 'rownames' when converted with 'as.matrix',
    # but it is of class 'Date', not 'POSIXct'... - jmu
    if(is.null(rownames(x)))
      stop("order.by must be either 'rownames()' or otherwise specified")
    else
      # added '...' args to allow for tz specification
      order.by <- do.call(paste('as',dateFormat,sep='.'),list(rownames(x),...))
  }
  
  xx <- xts(x,
            order.by=order.by,
            frequency=frequency,
            .CLASS='matrix',
            ...)
  xx
}

`re.timeSeries` <-
function(x,...) {
  stopifnot("package:fSeries" %in% search() || require("fSeries", quietly=TRUE))

  # strip all non-'core' attributes so they're not attached to the Data slot
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

