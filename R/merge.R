`merge.xts0` <- function(x,y,...,all=TRUE, fill=NA, suffixes=NULL, join="outer", retclass='xts') {
  # merge is currently optimized for the 2 case
  # scenario, but will handle k-merges via the overflow
  # ... arg.
  
  # still need an implementation with do_merge_xts
  # that can handle:
  #  suffixes=
  #
  # to match the behavior of zoo
  xyNames <- list(x=deparse(substitute(x)),y=deparse(substitute(y)))
  if( missing(y) ) 
    return(x)

  if(is.logical(retclass) && !retclass) {
    setclass <- FALSE
  } else setclass <- TRUE

  if( !is.xts(y) ) {
    y <- try.xts(y, error=FALSE)
    if( !is.xts(y) && NROW(y) != NROW(x) ) {
      y <- structure(rep(y, length.out=NROW(x)), index=.index(x))
    } else stop("can not convert 'y' to suitable class for merge")
  }

  if( !missing(join) ) {
    # join logic applied to index:
    # inspired by: http://blogs.msdn.com/craigfr/archive/2006/08/03/687584.aspx
    #
    #  (full) outer - all cases, equivelant to all=c(TRUE,TRUE)
    #         left  - all x,    &&  y's that match x
    #         right - all  ,y   &&  x's that match x
    #         inner - only x and y where index(x)==index(y)
    all <- switch(pmatch(join,c("outer","left","right","inner")),
                    c(TRUE,  TRUE ), #  outer
                    c(TRUE,  FALSE), #  left
                    c(FALSE, TRUE ), #  right
                    c(FALSE, FALSE)  #  inner
                 )
  }

  if( length(all) == 1 )
    all <- rep(all, length.out=2)

  dots <- list(...)

  if(length(dots) > 0) {

    x <- .Call('do_merge_xts', x, y, all, fill[1], setclass, PACKAGE="xts")
    for(i in 1:length(dots)) {
      if( !is.xts(dots[[i]]) ) {
        dots[[i]] <- try.xts(dots[[i]], error=FALSE)
        if( !is.xts(dots[[i]]) && NROW(dots[[i]]) != NROW(x) ) {
          dots[[i]] <- structure(rep(dots[[i]], length.out=NROW(x)), index=.index(x))
        } else stop("can not convert 'y' to suitable class for merge")
      }
      x <- .Call('do_merge_xts', x, dots[[i]], all, fill[1], setclass, PACKAGE="xts")
    }
    return(x)
  } else {
    ncx <- 1:NCOL(x)
    x <- .Call('do_merge_xts', x, y, all, fill[1], setclass, PACKAGE="xts")
    if(is.null(retclass)) {
      # needed for original Ops.xts(e1,e2), now for zoo compat
      assign(xyNames$x, x[,ncx], parent.frame())
      assign(xyNames$y, x[,-ncx],parent.frame())
      invisible(return(NULL))
    } else
    return(x)
  }
}

`merge.xts` <- merge.xts0

`.merge.xts` <-
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

