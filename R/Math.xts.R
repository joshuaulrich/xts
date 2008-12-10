cumsum.xts <- function(x)
{
  if( NCOL(x) == 1 )
    return(cumsum(coredata(x)))
  apply(x, 2, function(y) cumsum(coredata(x)))
}

cumprod.xts <- function(x)
{
  if( NCOL(x) == 1 )
    return(cumprod(coredata(x)))
  apply(x, 2, function(y) cumprod(coredata(x)))
}

cummin.xts <- function(x)
{
  if( NCOL(x) == 1 )
    return(cummin(coredata(x)))
  apply(x, 2, function(y) cummin(coredata(x)))
}

cummax.xts <- function(x)
{
  if( NCOL(x) == 1 )
    return(cummax(coredata(x)))
  apply(x, 2, function(y) cummax(coredata(x)))
}
