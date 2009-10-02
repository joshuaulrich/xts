rollapply.xts <- function(data, width, FUN, ..., by=1, ascending=TRUE, by.column=TRUE,
  na.pad=FALSE, align=c("center","left","right"), cumulative=FALSE) {

  # Code taken/adapted from rollapply.zoo from the 'zoo' package

  embedi <- function(n, k, by = 1, ascending = FALSE) {
  # n = no of time points, k = number of columns
  # by = increment. normally = 1 but if = b calc every b-th point 
  # ascending If TRUE, points passed in ascending order else descending.
  # Note that embed(1:n, k) corresponds to embedi(n, k, by = 1, rev = TRUE)
  # e.g. embedi(10, 3)
    s <- seq(1, n-k+1, by)
    lens <- length(s)
    cols <- 1:k
    if(!ascending) cols <- rev(cols)
    matrix(s + rep(cols, rep(lens,k))-1, lens)
  }

  # xts doesn't currently have these functions
  #  if(by.column && by == 1 && ascending && length(list(...)) < 1)
  #    switch(deparse(substitute(FUN)),
  #    mean = return(rollmean(data, width, na.pad = na.pad, align = align)),
  #    max = return(rollmax(data, width, na.pad = na.pad, align = align)),
  #    median = return(rollmedian(data, width, na.pad = na.pad, align = align)))

  ## evaluate FUN only on coredata(data)
  cdata <- coredata(data)
  nr <- NROW(cdata)
  width <- as.integer(width)[1]
  stopifnot( width > 0, width <= nr )

  ## process alignment
  align <- match.arg(align)
  n1 <- switch(align,    
    "left" = { width - 1},
    "center" = { floor(width/2) },
    "right" = { 0 })    
  tt <- index(data)[seq((width-n1), (nr-n1), by)]

  FUN <- match.fun(FUN)
  e <- embedi(nr, width, by, ascending)
  
  if( is.null(dim(cdata)) ) {
    xx <- sapply(1:nrow(e), function(i) FUN(cdata[e[i,]], ...))
    if( !is.null(dim(xx)) ) xx <- t(xx)
    res <- xts(xx, tt, if (by == 1) attr(data, "frequency"))
  } else if( by.column ) {
    # e <- embedi(nr, width, by, ascending)
    res <- xts( sapply( 1:ncol(cdata), function(i)
	                apply( e, 1, function(st) FUN(cdata[st,i], ...) ) ),
		        tt, if (by == 1) attr(data, "frequency") )
  } else {
    rval <- apply(embedi(nr, width, by, ascending), 1, function(st) FUN(cdata[st,], ...))
    if(!is.null(dim(rval))) rval <- t(rval)
    res <- xts(rval, tt, if (by == 1) attr(data, "frequency"))
  }	   
  if( na.pad ) {
    res <- merge(res, xts(,index(data), attr(data, "frequency")))
  }
  if( by.column && !is.null(dim(cdata)) ) {
    colnames(res) <- colnames(cdata)
  }
  return(res)
} 

