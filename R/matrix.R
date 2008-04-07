# functions for matrix <--> xts conversions

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
      order.by <- do.call(paste('as',dateFormat,sep='.'),list(rownames(x)))
  }
  
  xx <- xts(x,
            order.by=order.by,
            frequency=frequency,
            .CLASS='matrix',
            ...)
  xx
}
