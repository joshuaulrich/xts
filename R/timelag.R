timelag <- function (x, k = 1, na.pad = TRUE) {
  x <- try.xts(x, error=FALSE)
  
  na.pad <- as.logical(na.pad)
  if(is.na(na.pad))
    stop("'na.pad' must be logical")
  
  k <- as.numeric(k)
  if(is.na(k))
    stop("'k' must be numeric")
  
  if(k == 0)
    return(x)
  
  offset.xts <- .xts(, .index(x) - k)
  m <- merge(offset.xts, merge(offset.xts, x, fill = na.locf), join = "left")
  .index(m) <- .index(x)
  if(k < 0)
    m[.index(m) > .index(last(m)) + k,] <- NA
  
  if (is.null(dimnames(x)[[2L]]))
    dimnames(m) <- NULL
  
  if(!na.pad)
    na.omit(m)
  else
    m
}
