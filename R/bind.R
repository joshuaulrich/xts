`cbind.xts` <-
function(..., all=TRUE, fill=NA, suffixes=NULL, deparse.level=1) {
   if( deparse.level != 1)
     .NotYetUsed("deparse.level != 1")

   sc <- sys.call(sys.parent())
   mc <- match.call(call=sc,expand=FALSE)
   dots <- mc$...
   if(is.null(suffixes))
     suffixes <- all.vars(match.call(call=sc), unique=FALSE)[1:length(dots)]
 
   if( length(suffixes) != length(dots) ) {
     warning("length of suffixes and does not match number of merged objects")
     suffixes <- rep(suffixes, length.out=length(dots))
   }

   merge.xts(..., all=all, fill=fill, suffixes=suffixes)
#
#   dat <- list(...)
#   x <- dat[[1]]; dat <- dat[-1]
#   while( length(dat) > 0 ) {
#     y <- dat[[1]]
#     if( length(dat) > 0 )
#       dat <- dat[-1]
#     x <- merge.xts(x, y, all=TRUE, fill=NA, suffixes=NULL, retclass="xts")
#   }
#   x
}

`c.xts` <-
function(x, y, ...) {
  rbind.xts(x, y, ...)
}


`rbind.xts` <-
function(x, y, ..., deparse.level=1) {
  .Call('do_rbind_xts',x, y)
}
