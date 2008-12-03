# functions to support package:tis

as.POSIXct.tis <- function(x, offset=1, tz="", ...)
  as.numeric(POSIXct(x, offset, tz, ...)

as.xts.tis <- function(x, offset=1, ...)
{
  .xts(unclass(x), as.numeric(as.POSIXct.tis(x,offset)), ...)
}

re.tis <- function() {}

