merge.xts <- function(..., 
                     all=TRUE,
                     fill=NA,
                     suffixes=NULL,
                     join="outer",
                     retside=TRUE,
                     retclass="xts") {
  if(is.logical(retclass) && !retclass) {
    setclass=FALSE
  } else setclass <- TRUE
  
  mc <- match.call(expand=FALSE)
  dots <- mc$...
  if(is.null(suffixes))
    suffixes <- all.vars(match.call(), unique=FALSE)[1:length(dots)]

  if( length(suffixes) != length(dots) ) {
    warning("length of suffixes and does not match number of merged objects")
    suffixes <- rep(suffixes, length.out=length(dots))
  }
  i <- 0
  names.or.colnames <- function(x) {
    i <<- i + 1
    cnames <- colnames(eval.parent(x))
    if(is.null(cnames))
      cnames <- rep("",ncol(eval.parent(x)))
    return(paste(cnames,suffixes[i],sep='.'))
  }
  cnames <- make.unique(unlist(lapply(dots, names.or.colnames)))


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

  if( length(all) != 2 ) 
    all <- rep(all[1], 2)
  if( length(dots) > 2 )
    retside <- TRUE
  if( length(retside) != 2 ) 
    retside <- rep(retside[1], 2)

  x <- .External('mergeXts',
            all=all[1:2],
            fill=fill,
            setclass=setclass,
            colnames=cnames,
            retside=retside,
            ...)
  if(!is.logical(retclass) && retclass != 'xts') {
    asFun <- paste("as", retclass, sep=".")
    if(!exists(asFun)) {
      warning(paste("could not locate",asFun,"returning 'xts' object instead"))
      return(x)
    }
    xx <- try(do.call(asFun, list(x)))
    if(!inherits(xx,'try-error')) {
      return(xx)
    }
  }
  return(x)
}
.merge.xts <- function(x,y,...,
                      all=TRUE,
                      fill=NA,
                      suffixes=NULL,
                      join="outer",
                      retside=TRUE,
                      retclass="xts") {

  if(missing(y))
    return(x)
  if(is.logical(retclass) && !retclass) {
    setclass <- FALSE
  } else setclass <- TRUE

  mc <- match.call(expand=FALSE)
  xName <- deparse(mc$x)
  yName <- deparse(mc$y)
  dots <- mc$...

  if(!missing(...) && length(all) > 2) {
    xx <- list(x,y,...)
    all <- rep(all, length.out=length(xx))
    if(!base::all(all==TRUE) && !base::all(all==FALSE) ) {
      xT <- xx[which(all)] 
      xF <- xx[which(!all)] 
      return((rmerge0(do.call('rmerge0',xT),
                      do.call('rmerge0',xF), join="left"))[,c(which(all),which(!all))])
    }
  }

  tryXts <- function(y) {
  if(!is.xts(y)) {
    y <- try.xts(y, error=FALSE)
    if(!is.xts(y)) {
      if (NROW(y) == NROW(x)) {
        y <- structure(y, index = .index(x))
      }
      else if (NROW(y) == 1 && NCOL(y) == 1) {
        y <- structure(rep(y, length.out = NROW(x)), index = .index(x))
      }
      else stop(paste("cannot convert", deparse(substitute(y)), 
        "to suitable class for merge"))
    }
  }
  return(y)
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

  makeUnique <- function(cnames, nc, suff, dots) {
    if(is.null(suff) || length(suff) != (length(dots)+2)) return(make.unique(cnames))
    paste(cnames, rep(suff, times=nc),sep=".")
  }

  if( length(all) == 1 ) 
    all <- rep(all, length.out=length(dots)+2)
  if( length(retside) == 1 ) 
    retside <- rep(retside, length.out=length(dots)+2)

  y <- tryXts(y)

  COLNAMES <- c(colnames(x),colnames(y))
  if(length(COLNAMES) != (NCOL(x)+NCOL(y)))
    COLNAMES <- c(rep(xName,NCOL(x)), rep(yName,NCOL(y)))

  xCOLNAMES <- colnames(x)
  if(is.null(xCOLNAMES))
    xCOLNAMES <- rep(xName,NCOL(x))
  yCOLNAMES <- colnames(y)
  if(is.null(yCOLNAMES))
    yCOLNAMES <- rep(yName,NCOL(y))
  COLNAMES <- c(xCOLNAMES,yCOLNAMES)

  nCOLS <- c(NCOL(x), NCOL(y), sapply(dots, function(x) NCOL(eval.parent(x))))
  CNAMES <- if(length(dots)==0) {
              makeUnique(COLNAMES, nCOLS, suffixes, dots)
            } else NULL
 
  x <- .Call("do_merge_xts",
              x, y, all, fill[1], setclass, CNAMES, retside, PACKAGE="xts")
  if(length(dots) > 0) {
    for(i in 1:length(dots)) {
      currentCOLNAMES <- colnames(eval.parent(dots[[i]]))
      if(is.null(currentCOLNAMES))
        currentCOLNAMES <- rep(deparse(dots[[i]]),NCOL(eval.parent(dots[[i]])))
      COLNAMES <- c(COLNAMES, currentCOLNAMES)

      if( i==length(dots) ) #last merge, set colnames now
        CNAMES <- makeUnique(COLNAMES, nCOLS, suffixes, dots)
      x <- .Call("do_merge_xts",
                  x, tryXts(eval.parent(dots[[i]])), all,
                  fill[1], setclass, CNAMES, retside, PACKAGE="xts")
  
    }
  }
if(!is.logical(retclass) && retclass != 'xts') {
  xx <- try(do.call(paste("as",retclass,sep="."), list(x)))
  if(!inherits(xx,'try-error')) {
    return(xx)
  }
}
return(x)
}

rmerge0 <- function(x,y,...,
                   all=TRUE,
                   fill=NA,
                   suffixes=NULL,
                   join="outer",
                   retside=TRUE,
                   retclass="xts") {

  if(missing(y) || is.null(y))
    return(x)
  if(is.logical(retclass) && !retclass) {
    setclass <- FALSE
  } else setclass <- TRUE

  mc <- match.call(expand=FALSE)
  xName <- deparse(mc$x)
  yName <- deparse(mc$y)
  dots <- mc$...

#  if(!missing(...) && length(all) > 2) {
#    x <- list(x,y,...)
#    all <- rep(all, length.out=length(x))
#    xT <- x[which(all)] 
#    xF <- x[which(!all)] 
#    return((rmerge0(do.call('rmerge0',xT), do.call('rmerge0',xF), join="left"))[,c(which(all),which(!all))])
#  }

  tryXts <- function(y) {
  if(!is.xts(y)) {
    y <- try.xts(y, error=FALSE)
    if(!is.xts(y)) {
      if (NROW(y) == NROW(x)) {
        y <- structure(y, index = .index(x))
      }
      else if (NROW(y) == 1 && NCOL(y) == 1) {
        y <- structure(rep(y, length.out = NROW(x)), index = .index(x))
      }
      else stop(paste("cannot convert", deparse(substitute(y)), 
        "to suitable class for merge"))
    }
  }
  return(y)
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

  makeUnique <- function(cnames, nc, suff, dots) {
    if(is.null(suff) || length(suff) != (length(dots)+2)) return(make.unique(cnames))
    paste(cnames, rep(suff, times=nc),sep=".")
  }

  if( length(all) == 1 ) 
    all <- rep(all, length.out=length(dots)+2)
  if( length(retside) == 1 ) 
    retside <- rep(retside, length.out=length(dots)+2)
  y <- tryXts(y)

  COLNAMES <- c(colnames(x),colnames(y))
  if(length(COLNAMES) != (NCOL(x)+NCOL(y)))
    COLNAMES <- c(rep(xName,NCOL(x)), rep(yName,NCOL(y)))

  xCOLNAMES <- colnames(x)
  if(is.null(xCOLNAMES))
    xCOLNAMES <- rep(xName,NCOL(x))
  yCOLNAMES <- colnames(y)
  if(is.null(yCOLNAMES))
    yCOLNAMES <- rep(yName,NCOL(y))
  COLNAMES <- c(xCOLNAMES,yCOLNAMES)

  nCOLS <- c(NCOL(x), NCOL(y), sapply(dots, function(x) NCOL(eval.parent(x))))
#  CNAMES <- if(length(dots)==0) {
#              makeUnique(COLNAMES, nCOLS, suffixes, dots)
#            } else NULL
  CNAMES <- NULL
 

  x <- .Call("do_merge_xts",
              x, y, all, fill[1], setclass, CNAMES, retside, PACKAGE="xts")
  if(length(dots) > 0) {
    for(i in 1:length(dots)) {
      currentCOLNAMES <- colnames(eval.parent(dots[[i]]))
      if(is.null(currentCOLNAMES))
        currentCOLNAMES <- rep(deparse(dots[[i]]),NCOL(eval.parent(dots[[i]])))
      COLNAMES <- c(COLNAMES, currentCOLNAMES)

#      if( i==length(dots) ) #last merge, set colnames now
#        CNAMES <- makeUnique(COLNAMES, nCOLS, suffixes, dots)
      x <- .Call("do_merge_xts",
                  x, tryXts(eval.parent(dots[[i]])), all,
                  fill[1], setclass, CNAMES, retside, PACKAGE="xts")
  
    }
  }
return(x)
}
#library(xts)
#x <- .xts(1:10, 1:10)
#rmerge(x,x,x)
#rmerge(x,x,1)
#z <- as.zoo(x)
#rmerge(x,z)
#rmerge(x,x,z)
#rmerge(x,1,z,z)
#X <- .xts(1:1e6, 1:1e6)
#system.time(rmerge(X,X,X,X,X,X,X))
