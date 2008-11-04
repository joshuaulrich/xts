as.numeric.xts <- function(x, drop=TRUE, ...)
{
  if(drop)
    return(as.numeric(unclass(x)))
  .xts(matrix(as.numeric(unclass(x)),nc=NCOL(x)), .index(x))
}

as.integer.xts <- function(x, drop=TRUE, ...)
{
  if(drop)
    return(as.integer(unclass(x)))
  .xts(matrix(as.integer(unclass(x)),nc=NCOL(x)), .index(x))
}

as.double.xts <- function(x, drop=TRUE, ...)
{
  if(drop)
    return(as.double(unclass(x)))
  .xts(matrix(as.double(unclass(x)),nc=NCOL(x)), .index(x))
}

as.complex.xts <- function(x, drop=TRUE, ...)
{
  if(drop)
    return(as.complex(unclass(x)))
  .xts(matrix(as.complex(unclass(x)),nc=NCOL(x)), .index(x))
}

as.logical.xts <- function(x, drop=TRUE, ...)
{
  if(drop)
    return(as.logical(unclass(x)))
  .xts(matrix(as.logical(unclass(x)),nc=NCOL(x)), .index(x))
}
