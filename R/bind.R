`cbind.xts` <-
function(..., deparse.level = 1) {
    dat <- list(...)
    x <- dat[[1]]; dat <- dat[-1]
    while( length(dat) > 0 ) {
      y <- dat[[1]]
      if( length(dat) > 0 )
        dat <- dat[-1]
      x <- merge.xts(x, y, all=TRUE, fill=NA, suffixes=NULL, retclass="xts")
    }
    x
}

`c.xts` <-
function(x, y, ...) {
  rbind.xts(x, y, ...)
}


`rbind.xts` <-
function(x, y, ..., deparse.level=1) {
  .Call('do_rbind_xts',x, y)
}
