# dimnames will return the actual dimnames of the xts object
# dimnames<-.xts will force the rownames to always be NULL

`dimnames.xts` <-
function(x) {
  list(NULL, colnames(unclass(x)))
  #list(as.character(index(x)), colnames(unclass(x)))
}

`dimnames<-.xts` <-
function(x, value) {
  oclass <- class(x)
  x <- unclass(x)
  d <- dim(x)
  if(is.null(value)) {
    dimnames(x) <- NULL
  }
  else {
    if(!is.list(value) || length(value) != 2L)
      stop("invalid 'dimnames' given for xts")
    value[[1L]] <- list(NULL)
    value[[2L]] <- as.character(value[[2L]])
  
    rownames(x) <- NULL
    colnames(x) <- value[[2L]]
    class(x) <- oclass
  }
  x
}
