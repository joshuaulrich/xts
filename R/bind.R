`cbind.xts` <-
function(x, y, ..., all=TRUE, fill=NA, suffixes=NULL, join="outer") {
    merge.xts(x, y, ..., all=all, fill=fill, suffixes=suffixes, retclass="xts")
}

`c.xts` <-
function(x, y, ...) {
  cbind.xts(x, y, ...)
}


`rbind.xts` <-
function(..., deparse.level=1) {

 args <- list(...)
 # Allow, but remove, NULL objects
 args <- args[!sapply(args,is.null)]
 if(length(args)==1)
   return(args[[1]])

 # Store original class attributes
 xts.CLASS <- sapply(args, CLASS)
 xts.CLASSattr <- lapply(args, xtsAttributes, user=FALSE)
 xts.USERattr <- lapply(args, xtsAttributes, user=TRUE)
 has.ROWNAMES <- any( as.logical(lapply(args, function(x) any(names(attributes(x))=='.ROWNAMES'))) )

 # Ensure index attributes match
 # store as POSIXct, convert to class of first object index
 # iclass <- class( index(args[[1]]) )
 iclass <- indexClass(args[[1]])  # using xts functions -- jar
 #timeBasedClass <- sapply( iclass, function(x) xts:::timeBased(structure(1,class=x)) )
 #iclass <- iclass[timeBasedClass][1]
 #for(arg in args) {
 #  index(arg) <- as.POSIXct(index(arg))
 #}
 
 # Bind objects
 ret <- zoo:::rbind.zoo(...)
 #ret <- structure( ret, class=c('xts','zoo') )
 ret <- as.xts(ret)

 # Restore first object index class
 # index(ret) <- do.call( paste('as', iclass, sep='.'), list(index(ret)) ) 
 indexClass(ret) <- iclass # changed above to utilize internal classing of V2 xts -- jar

 # Drop CLASS & USER attributes if they are not the same for all objects
 all.CLASS <- all( sapply(xts.CLASS, function(x) identical(x,xts.CLASS[[1]])) )
 all.USER <- all( sapply(xts.USERattr, function(x) identical(x,xts.USERattr[[1]])) )
 
 if( all.CLASS ) {
   # Need a better way to deal with different xtsAttributes than
   # simply assigning them to the value of the first object.
   xts.CLASSattr <- xts.CLASSattr[[1]]
   CLASS(ret) <- xts.CLASS[[1]]
   attr(ret, '.CLASSnames') <- attr(args[[1]], '.CLASSnames')

 } else {
   CLASS(ret) <- NULL
   xts.CLASSattr <- NULL
 }
 if( has.ROWNAMES ) {
 # .ROWNAMES attribute may not be similar, so set .ROWNAMES to index for now,
 # if it exists in at least one object
   attr(ret, '.ROWNAMES') <- as.character(index(ret))
 } else {
   attr(ret, '.ROWNAMES') <- NULL
 }
 if( all.USER ) {
   xts.USERattr <- xts.USERattr[[1]]
 } else {
   xts.USERattr <- NULL
 }
 # Re-attach _xts_ attributes
 xtsAttributes(ret) <- c( xts.CLASSattr, xts.USERattr )

 return(ret)
}
