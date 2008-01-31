# functions to handle zoo <--> xts conversions

`re.zoo` <-
function(x,...) {
  xx <- coredata(x)
  xx <- zoo(xx,
        order.by=index(x),
        ...)
  attr(xx,'names') <- attr(x,'names') # trying to capture names - jar
  xx
}

`as.xts.zoo` <-
function(x,order.by=index(x),frequency=NULL,...) {
  xx <- xts(coredata(x),          # Cannot use 'zoo()' on objects of class 'zoo' - jmu
            order.by=order.by,
            frequency=frequency,
            .CLASS='zoo',
            names=attr(x,'names'), # trying to capture names -jar
            ...)
  xx
}

`as.zoo.xts` <-
function(x,...) {
  zoo(coredata(x),
      order.by=index(x),
      ...)
}
