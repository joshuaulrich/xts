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

`xcoredata.default` <-
function(x,...) {
  x.attr <- attributes(x)
  original.attr <- x.attr[!names(x.attr) %in%
                          c('dim','dimnames')]
  original.attr
}


`xcoredata` <-
function(x,...) {
  UseMethod('xcoredata')
}

`xcoredata<-` <- function(x,value) {
  UseMethod('xcoredata<-')
}

`xcoredata<-.default` <- function(x,value) {
  if(is.null(value)) {
    return(coredata(x))
  } else {
    for(att in names(value)) {
      if(!att %in% c('dim','dimnames'))
        attr(x,att) <- value[[att]]
    }
  return(x)
  }
}

`xtsAttributes` <-
function(x, user=NULL) {
  # get all additional attributes not standard to xts object
  #stopifnot(is.xts(x))
  rm.attr <- c('dim','dimnames','index','class','names')
  x.attr <- attributes(x)

  if(is.null(user)) {
  # Both xts and user attributes
    rm.attr <- c(rm.attr,'.CLASS','.CLASSnames','.ROWNAMES', '.indexCLASS', '.indexFORMAT')
    xa <- x.attr[!names(x.attr) %in% rm.attr]
  }
  else
  if(user) {
  # Only user attributes
    rm.attr <- c(rm.attr,'.CLASS','.CLASSnames','.ROWNAMES', '.indexCLASS', '.indexFORMAT', 
                 x.attr$.CLASSnames)
    xa <- x.attr[!names(x.attr) %in% rm.attr]
  } else {
  # Only xts attributes
    xa <- x.attr[names(x.attr) %in% x.attr$.CLASSnames]
  }

  if(length(xa) == 0) return(NULL)
  xa
}

`xtsAttributes<-` <-
function(x,value) {
  UseMethod('xtsAttributes<-')
}

`xtsAttributes<-.xts` <-
function(x,value) {
  if(is.null(value)) {
    for(nm in names(xtsAttributes(x))) {
      attr(x,nm) <- NULL
    }
  } else
  for(nv in names(value)) {
    if(!nv %in% c('dim','dimnames','index','class','.CLASS','.ROWNAMES','.CLASSnames'))
      attr(x,nv) <- value[[nv]]
  }
  x
}
