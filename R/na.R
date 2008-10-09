`na.omit.xts` <- function(object, ...) {
  xx <- stats:::na.omit.default(object,...)
  naa <- attr(xx,'na.action')
  if(length(naa) == 0)
    return(xx)

  naa.index <- .index(object)[naa]

  ROWNAMES <- attr(object,'.ROWNAMES')
  if(!is.null(ROWNAMES)) {
    naa.rownames <- ROWNAMES[naa]
  } else naa.rownames <- NULL

  attr(xx,'na.action') <- structure(naa,
                                    index=naa.index,
                                    .ROWNAMES=naa.rownames)
  return(xx) 
}

`na.replace` <- function(x) {
  if(is.null(xtsAttributes(x)$na.action))
    return(x)

  # Create 'NA' xts object
  tmp <- xts(matrix(rep(NA,NCOL(x)*NROW(x)), nc=NCOL(x)),
             attr(xtsAttributes(x)$na.action, 'index'))

  # Ensure xts 'NA' object has *all* the same attributes
  # as the object 'x'; this is necessary for rbind to
  # work correctly
  CLASS(tmp) <- CLASS(x)
  xtsAttributes(tmp) <- xtsAttributes(x)
  attr(x,'na.action') <- attr(tmp,'na.action') <- NULL
  colnames(tmp) <- colnames(x)

  rbind(x,tmp)
}

