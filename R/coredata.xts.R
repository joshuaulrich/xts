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

`xtsAttributes` <-
function(x) {
  # get all additional attributes not standard to xts object
  stopifnot(is.xts(x))
  x.attr <- attributes(x)
  xa <- x.attr[!names(x.attr) %in% c('dim','dimnames','index','class','.CLASS')]
  if(length(xa) == 0) return(NULL)
  xa
}

`xtsAttributes<-` <-
function(x,value) {
  UseMethod('xtsAttributes<-')
}

`xtsAttributes<-.xts` <-
function(x,value) {
  for(nv in names(value)) {
    if(!nv %in% c('dim','dimnames','index','class','.CLASS'))
      attr(x,nv) <- value[[nv]]
  }
  x
}
