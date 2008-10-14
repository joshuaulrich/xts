`merge.xts` <- function(x,y,...,all=TRUE, fill=NA, suffixes=NULL, join="outer", retclass='xts') {
  # merge is currently optimized for the 2 case
  # scenario, but will handle k-merges via the overflow
  # ... arg.
  
  if( missing(y) ) 
    return(x)

  if(is.logical(retclass) && !retclass) {
    setclass <- FALSE
  } else setclass <- TRUE

  if( !is.xts(y) )
    y <- try.xts(y, error=FALSE)
    
  if( !is.xts(y) ) { # if unsuccessful
    if( NROW(y) == NROW(x) ) {
      # same length, but no index
      y <- structure(y, index=.index(x))
    } else
    if( NROW(y)==1 && NCOL(y)==1) {
      # scalar
      y <- structure(rep(y, length.out=NROW(x)), index=.index(x))
    } else stop(paste("cannot convert",deparse(substitute(y)),"to suitable class for merge"))
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

  xyNames <- as.character(c(list(x=deparse(substitute(x)),y=deparse(substitute(y))),
               as.character(as.list(match.call(expand.dots=FALSE)$...))))

  make.colnames <- function(x, xname, y, yname, suffix=NULL) {
    colnamesX <- colnames(x)
    if(is.null(colnamesX)) {
      colnamesX <- c(paste(xname,1:NCOL(x),sep='.'))
    }
    colnamesY <- colnames(y)
    if(is.null(colnamesY)) {
      colnamesY <- c(paste(yname,1:NCOL(y),sep='.'))
    }
    if(!is.null(suffix) && length(suffix)==2) {
      return(c(paste(colnamesX,suffix[1],sep='.'),paste(colnamesY,suffix[2],sep='.')))
    } else return(c(colnamesX, colnamesY))
  }

  if(length(dots) > 0) {

    x <- .Call('do_merge_xts', x, y, all, fill[1], setclass, 
               #make.unique(c(colnames(x),colnames(y))), PACKAGE='xts')
               make.colnames(x,deparse(substitute(x)),
                             y,deparse(substitute(y)),
                             suffix=suffixes), PACKAGE="xts")
    for(i in 1:length(dots)) {
      if( !is.xts(dots[[i]]) ) {
        dots[[i]] <- try.xts(dots[[i]], error=FALSE)
        if( !is.xts(dots[[i]]) && NROW(dots[[i]]) == NROW(x) ) {
          dots[[i]] <- structure(dots[[i]], index=.index(x))
        } else
        if( !is.xts(dots[[i]]) && NROW(dots[[i]])==1 && NCOL(dots[[i]])==1) {
          dots[[i]] <- structure(rep(dots[[i]], length.out=NROW(x)), index=.index(x)) 
        } else stop("can not convert 'y' to suitable class for merge")
      }
      x <- .Call('do_merge_xts', x, dots[[i]], all, fill[1], setclass, 
                 #c(colnames(x), colnames(dots[[i]])), PACKAGE="xts")
                 make.colnames(x, NULL,
                               dots[[i]], xyNames[i+2],
                               suffix=suffixes),PACKAGE="xts")
    }
    colnames(x) <- make.unique(colnames(x))
    return(x)
  } else {
    ncx <- 1:NCOL(x)
    ncy <- 1:NCOL(y)
    cnames <- c(colnames(x), colnames(y))
    cnames <- if(is.null(cnames)) { NULL } else { make.unique(cnames) }
    x <- .Call('do_merge_xts', x, y, all, fill[1], setclass, 
               cnames, PACKAGE="xts")
               #make.unique(c(colnames(x), colnames(y))), PACKAGE="xts")
               #make.unique(make.colnames(x,deparse(substitute(x)),
               #              y,deparse(substitute(y)),
               #              suffix=suffixes)), PACKAGE="xts")
    #colnames(x) <- make.unique(colnames(x))
    if(is.null(retclass)) {
      # needed for original Ops.xts(e1,e2), now for zoo compat
      assign(xyNames$x, x[,ncx], parent.frame())
      assign(xyNames$y, x[,-ncx],parent.frame())
      invisible(return(NULL))
    } else
    return(x)
  }
}

`merge.xts0` <- function(x,y,...,all=TRUE, fill=NA, suffixes=NULL, join="outer", retclass='xts') {
  # this is for use withing Ops.xts, as it handles colnames differently
  if( missing(y) ) 
    return(x)

  if(is.logical(retclass) && !retclass) {
    setclass <- FALSE
  } else setclass <- TRUE

  if( !is.xts(y) ) {
    y <- try.xts(y, error=FALSE)
    if( !is.xts(y) && NROW(y) == NROW(x) ) {
      y <- structure(y, index=.index(x))
    } else
    if( !is.xts(y) && NROW(y)==1 && NCOL(y)==1) {
      y <- structure(rep(y, length.out=NROW(x)), index=.index(x))
    } else stop("cannot convert 'y' to suitable class for merge")
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

  x <- .Call('do_merge_xts', x, y, all, fill[1], setclass, c(colnames(x), colnames(y)), PACKAGE="xts")

  if(is.null(retclass)) {
    ncx <- 1:NCOL(x)
    # needed for original Ops.xts(e1,e2), now for zoo compat
    assign(xyNames$x, x[,ncx], parent.frame())
    assign(xyNames$y, x[,-ncx],parent.frame())
    invisible(return(NULL))
  } else
  return(x)
}
