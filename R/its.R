# functions for its <--> xts conversions

`re.its` <-
function(x, ...) {
  stopifnot("package:its" %in% search() || require("its",quietly=TRUE))
  xx <- coredata(x)
  dates <- index(x)
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

`xts.as.its` <-
function(x,...) {
  if(!is.xts(x)) stop('not an "xts" object')
  if(!inherits(class(index(x)),'POSIXct')) indexClass(x) <- "POSIXct"
  re.its(x,...)
}

