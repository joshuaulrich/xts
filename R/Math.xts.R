cumsum.xts <- function(x)
{
  if( NCOL(x) == 1 ) {
    x[] <- cumsum(coredata(x))
  } else x[] <- apply(coredata(x), 2, function(y) cumsum(y))
  x
}

cumprod.xts <- function(x)
{
  if( NCOL(x) == 1 ) {
    x[] <- cumprod(coredata(x))
  } else x[] <- apply(coredata(x), 2, function(y) cumprod(y))
  x
}

cummin.xts <- function(x)
{
  if( NCOL(x) == 1 ) {
    x[] <- cummin(coredata(x))
  } else x[] <- apply(coredata(x), 2, function(y) cummin(y))
  x
}

cummax.xts <- function(x)
{
  if( NCOL(x) == 1 ) {
    x[] <- cummax(coredata(x))
  } else x[] <- apply(coredata(x), 2, function(y) cummax(y))
  x
}
