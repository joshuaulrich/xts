rollapply.xts <- function(data, width, FUN, ..., by=1, ascending=TRUE,
  by.column=TRUE, na.pad=FALSE, align=c("center","left","right")) {

  if(!ascending) {
    warning("ignoring 'ascending=FALSE'")
    ascending <- TRUE
  }
  if(by != 1) {
    warning(paste("ignoring 'by=",by,"'",sep=""))
    by <- 1
  }
  if(missing(align)) align <- "right"
  
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

  nr <- NROW(data)
  nc <- NCOL(data)
  width <- as.integer(width)[1]
  stopifnot( width > 0, width <= nr )

  ## process alignment
  align <- match.arg(align)
  n1 <- switch(align,    
    "left" = { width - 1},
    "center" = { floor(width/2) },
    "right" = { 0 })
  idx <- index(data)
  #tt <- index(data)[seq((width-n1), (nr-n1), by)]
  tt <- idx[seq((width-n1), (nr-n1), 1)]

  ## evaluate FUN only on coredata(data)
  #data <- coredata(data)

  FUN <- match.fun(FUN)

  ind <- as.matrix(width:nr)
  #e <- embedi(nr, width, by, ascending)

  if( nc==1 ) {
    #xx <- apply(e, 1, function(i) FUN(data[i,],...))
    #xx <- sapply(1:NROW(e), function(i) FUN(data[e[i,],],...))
    ##xx <- sapply(ind, function(i) FUN(data[(i-width+1):i,],...))
    xx <- sapply(ind, function(i) FUN(.subset_xts(data,(i-width+1):i,,...)))
    res <- xts(xx, tt, if (by == 1) attr(data, "frequency"))
  } else if( by.column ) {
    res <- xts( sapply( 1:NCOL(data), function(j)
                #apply(e, 1, function(i) FUN(data[i,j],...)) ),
                #apply(ind, 1, function(i) FUN(data[(i-width+1):i,j],...)) ),
                apply(ind, 1, function(i) FUN(.subset_xts(data,(i-width+1):i,j,...))) ),
		        tt, if (by == 1) attr(data, "frequency") )
  } else {
    #xx <- apply(e, 1, function(i) FUN(data[i,],...))
    ##xx <- apply(ind, 1, function(i) FUN(data[(i-width+1):i,],...))
    xx <- apply(ind, 1, function(i) FUN(.subset_xts(data,(i-width+1):i,...)))
    if(!is.null(dim(xx))) xx <- t(xx)
    res <- xts(xx, tt, if (by == 1) attr(data, "frequency"))
  }
  if( na.pad ) {
    tmp <- merge(res, xts(,idx, attr(data, "frequency")))
    if(is.null(colnames(res))) {
      colnames(tmp) <- colnames(res)
    }
    res <- tmp
  }
  if( by.column && !is.null(dim(data)) ) {
    colnames(res) <- colnames(data)
  }
  return(res)
} 

