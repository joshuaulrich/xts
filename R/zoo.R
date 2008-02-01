# functions to handle zoo <--> xts conversions

`re.zoo` <-
function(x,...) {
  xx <- coredata(x)

  xx <- zoo(xx,
        order.by=index(x),
        ...)

  dotNames <- attr(x,'names')
  if(!is.null(dotNames)) {
    xx <- structure(xx, .Names=dotNames)
  }
# if(is.null(dimnames(x)[2]) | length(dimnames(x)[2])==1) {
#   dimnames(xx) <- NULL
#   attr(xx,'names') <- dimnames(x)[1]
# }
  xx
}

`as.xts.zoo` <-
function(x,order.by=index(x),frequency=NULL,...) {
  xx <- xts(coredata(x),          # Cannot use 'zoo()' on objects of class 'zoo' - jmu
            order.by=order.by,
            frequency=frequency,
            .CLASS='zoo',
            ...)
#  attr(xx,'names') <- NULL

# if(is.null(dimnames(x))) {
#   if(!is.null(attr(x,'names')))  # trying to capture names -jar
#     dimnames(xx)[1] <- attr(x,'names')
#     dimnames(xx)[2] <- NULL
# }
  xx
}

`as.zoo.xts` <-
function(x,...) {
  zoo(coredata(x),
      order.by=index(x),
      ...)
}
