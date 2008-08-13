# functions to handle zoo <--> xts conversions

`re.zoo` <-
function(x,...) {
  xx <- coredata(x)

  xx <- zoo(xx,
        order.by=index(x),
        ...)

  if(length(dimnames(x)[[2]]) < 2) {
    dimnames(xx) <- NULL
    dim(xx) <- NULL
    attr(xx,'names') <- as.character(index(x))
  }
  xx
}

`as.xts.zoo` <-
function(x,order.by=index(x),frequency=NULL,...) {
  xx <- xts(coredata(x),          # Cannot use 'zoo()' on objects of class 'zoo' - jmu
            order.by=order.by,
            frequency=frequency,
            .CLASS='zoo',
            ...)

#
#  if(!is.null(attr(x,'names'))) {
#    dim(xx) <- c(NROW(xx),NCOL(xx))
#    dn <- list(attr(x,'names'),colnames(x))
#    dimnames(xx) <- dn
#    attr(xx,'.ROWNAMES') <- attr(x,'names')
#  }
#
  xx
}

`as.zoo.xts` <-
function(x,...) {
  zoo(coredata(x),
      order.by=index(x),
      ...)
}
