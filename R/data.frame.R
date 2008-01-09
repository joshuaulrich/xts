# functions to handle data.frame <--> xts conversions

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

`as.data.frame.xts` <-
function(x,row.names=NULL,optional=NULL,...) {
  as.data.frame(coredata(x),row.names,optional,...)
}

