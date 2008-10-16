`as.xts.fts` <-
function(x, ...)
{
  dates <- as.numeric(dates(x))
  attr(x,'dates') <- NULL
  .xts(unclass(x), dates, ...)
}

`as.fts.xts` <-
function(x)
{
  fts(coredata(x), structure(.index(x), class=c("POSIXt","POSIXct")))
}
