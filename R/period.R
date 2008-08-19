# optimized periodic apply functions
#
`is.timeBased` <- `timeBased` <-
function(x) {
if (!any(sapply(c("Date", "POSIXct", "chron", "dates", "times", 
        "timeDate", "yearmon", "yearqtr"), function(xx) inherits(x, 
        xx)))) {
        FALSE
    } else TRUE
}

`period.sum` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")

  ep <- INDEX
  if(ep[1] != 0) ep <- c(0,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(as.matrix(x))
  q <- .Fortran('psumz',ep=as.integer(ep),lep=as.integer(length(ep)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(ep)-1)))
                ,PACKAGE='xts')
  if(timeBased(index(x))) {
    tz <- xts(q$ret,index(x)[ep[-1]])
  } else {
    tz <- zoo(q$ret,index(x)[ep[-1]])
  }
  tz
}
`period.prod` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")
  ep <- INDEX
  if(ep[1] != 0) ep <- c(0,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(as.matrix(x))
  q <- .Fortran('pprodz',ep=as.integer(ep),lep=as.integer(length(ep)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(ep)-1)))
                ,PACKAGE='xts')
  if(timeBased(index(x))) {
    tz <- xts(q$ret,index(x)[ep[-1]])
  } else {
    tz <- zoo(q$ret,index(x)[ep[-1]])
  }
  tz
}
`period.max` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")
  ep <- INDEX
  if(ep[1] != 0) ep <- c(0,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(as.matrix(x))
  q <- .Fortran('pmaxz',ep=as.integer(ep),lep=as.integer(length(ep)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(ep)-1)))
                ,PACKAGE='xts')
  if(timeBased(index(x))) {
    tz <- xts(q$ret,index(x)[ep[-1]])
  } else {
    tz <- zoo(q$ret,index(x)[ep[-1]])
  }
  tz
}
`period.min` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single column data only")
  ep <- INDEX
  if(ep[1] != 0) ep <- c(0,ep)
  if(ep[length(ep)] != NROW(x)) ep <- c(ep,NROW(x))

  xx <- as.double(as.matrix(x))
  q <- .Fortran('pminz',ep=as.integer(ep),lep=as.integer(length(ep)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(ep)-1)))
                ,PACKAGE='xts')
  if(timeBased(index(x))) {
    tz <- xts(q$ret,index(x)[ep[-1]])
  } else {
    tz <- zoo(q$ret,index(x)[ep[-1]])
  }
  tz
}
