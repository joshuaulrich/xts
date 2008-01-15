# functions to handle zoo <--> xts conversions

`re.zoo` <-
function(x,...) {
  xx <- coredata(x)
  rownames(xx) <- attr(x,'zoo.rownames')
  zoo(xx,
      order.by=index(x),
      ...)
}

`as.xts.zoo` <-
function(x,order.by=index(x),frequency=NULL,...) {
  xx <- xts(coredata(x),          # Cannot use 'zoo()' on objects of class 'zoo' - jmu
            order.by=order.by,
            frequency=frequency,
            .CLASS='zoo',
            zoo.rownames=rownames(x),
            ...)
  xx
}

`as.zoo.xts` <-
function(x,...) {
  zoo(coredata(x),
      order.by=index(x),
      ...)
}
