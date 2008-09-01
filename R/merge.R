`merge.xts0` <- function(x,y,...,all=TRUE, fill=NA, suffixes=NULL) {
  # merge is currently optimized for the 2 case
  # scenario, but will handle k-merges via the overflow
  # ... arg.
  
  # still need an implementation with do_merge_xts
  # that can handle:
  #  suffixes=
  #

  dots <- list(...)
  if(length(all) != length(dots)+1)
    all <- rep(all,length.out=length(dots)+1)

  if(length(dots) > 0) {
    x <- .Call('do_merge_xts', x, y, all[1], fill[1])
#    cnames <- c( paste(suffixes[1],colnames(x),sep="."), paste(suffixes[2],colnames(y),sep=".") )
    for(i in 1:length(dots)) {
      x <- .Call('do_merge_xts', x, dots[[i]], all[i+1], fill[1])
#      cnames <- c( cnames, paste(suffixes[i],colnames(dots[[i]]),sep=".") )
    }
#    colnames(x) <- cnames
    return(x)
  } else {
#    cnames <- c( paste(suffixes[1],colnames(x),sep="."), paste(suffixes[2],colnames(y),sep=".") )
    x <- .Call('do_merge_xts', x, y, all[[1]], fill[1])
#    colnames(x) <- cnames
    return(x)
  }
}

`merge.xts` <-
function(..., all=TRUE, fill=NA, suffixes=NULL, retclass='xts') {

 args <- list(...)
 # Allow, but remove, NULL objects
 args <- args[!sapply(args,is.null)]
 if(length(args)==1)
   return(args[[1]])
 #retclass <- match.arg(retclass, retclass)

 # Store original class attributes
 xts.CLASS <- sapply(args, CLASS)
 xts.CLASSattr <- lapply(args, xtsAttributes, user=FALSE)
 xts.USERattr <- lapply(args, xtsAttributes, user=TRUE)
 has.ROWNAMES <- any( as.logical(sapply(args, function(x) any(names(attributes(x))=='.ROWNAMES'))) )

 # Merge objects
 ret <- zoo:::merge.zoo(..., all=all, fill=fill, suffixes=suffixes, retclass='zoo')
 ret <- as.xts(ret) # slightly more clean now that more than a name change happens internally
 #ret <- structure( ret, class=c('xts','zoo') )

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
 # if it exists in at least one object.
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
 # Add rownames
 rownames(ret) <- as.character(index(ret))

 return(ret)
}

