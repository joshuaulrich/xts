# functions to handle zoo <--> xts conversions

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

`as.zoo.xts` <-
function(x,...) {
  zoo(coredata(x),
      order.by=index(x),
      ...)
}
