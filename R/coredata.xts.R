`coredata.xts` <-
function(x,...) {
  x.attr <- attributes(x)

  xx <- structure(x,dimnames=x.attr$dimnames) #,index=x.attr$index)

  # attributes not to be kept
  original.attr <- x.attr[!names(x.attr) %in%
                          c('dim','dimnames')]

  for(i in names(original.attr)) {
    attr(xx,i) <- NULL
  }

  return(xx)
}
