# optimized periodic apply functions
#

`period.sum` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single col data only")
  bp <- INDEX
  xx <- as.double(as.matrix(x))
  q <- .Fortran('psumz',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(bp)-1)))
                ,PACKAGE='xts')
  tz <- xts(matrix(q$ret,nc=1,byrow=TRUE),index(x)[bp[-1]])
  tz

}
`period.prod` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single col data only")
  bp <- INDEX
  xx <- as.double(as.matrix(x))
  q <- .Fortran('pprodz',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(bp)-1)))
                ,PACKAGE='xts')
  tz <- xts(matrix(q$ret,nc=1,byrow=TRUE),index(x)[bp[-1]])
  tz
}
`period.max` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single col data only")
  bp <- INDEX
  xx <- as.double(as.matrix(x))
  q <- .Fortran('pmaxz',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(bp)-1)))
                ,PACKAGE='xts')
  tz <- xts(matrix(q$ret,nc=1,byrow=TRUE),index(x)[bp[-1]])
  tz
}
`period.min` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single col data only")
  bp <- INDEX
  xx <- as.double(as.matrix(x))
  q <- .Fortran('pminz',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(bp)-1)))
                ,PACKAGE='xts')
  tz <- xts(matrix(q$ret,nc=1,byrow=TRUE),index(x)[bp[-1]])
  tz
}
