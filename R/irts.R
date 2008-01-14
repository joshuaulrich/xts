# methods for tseries::irts

`re.irts` <-
function(x,...) {
  stopifnot("package:tseries" %in% search() || require("tseries",quietly=TRUE))
  indexClass(x) <- "POSIXct"
  irts(index(x),coredata(x))
}

`as.xts.irts` <-
function(x,order.by,frequency=NULL,...) {
  xx <- xts(x=x$value,
            order.by=x$time,
            frequency=frequency,
            .CLASS='irts',
            ...)
  xx
}


