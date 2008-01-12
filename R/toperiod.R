# to.period functionality from quantmod
#
# to.period base function
# to.minutes
# to.hourly
# to.daily
# to.weekly
# to.monthly
# to.quarterly
# to.yearly

# endpoints needs to be able to handle 'k' arguments to minutes...

`to.period` <-
function(x,period='months',k=1,name=NULL,...)
{
  if(is.null(name)) name <- deparse(substitute(x))

  # test if is class 'xts', if not, convert to 'xts'
  if(!is.xts(x)) x <- as.xts(x)
  
  is.na(x)
  x <- na.omit(x)  # can't calculate aggregation with NA values  ??? any suggestions

  # is this an OHLC object already?
  # if so, treat differently then single column object
  if(is.OHLC(x)) {
    ep <- endpoints(x,period,k)
  
    tz <- as.double(as.matrix(x))

    hasvol <- ifelse(has.Vo(x), 1, 0) # test for volume column
    hasadj <- ifelse(has.Ad(x), 1, 0) # test for adjusted column (from yahoo)

    # ohlcq is the F77 routine to aggregate OHLC data to another periodicity
    q <- .Fortran("ohlcq", ep = as.integer(ep), lep = as.integer(length(ep)), 
        ia = as.double(tz), lia = as.integer(length(tz)), nri = as.integer(NROW(x)), 
        hasvol = as.integer(hasvol), 
        hasadj = as.integer(hasadj), ret = as.double(rep(0, (length(ep) - 
            1) * (NCOL(x)))), PACKAGE = "xts")
    tz <- xts(matrix(q$ret, nc = (4 + hasvol + hasadj), byrow = TRUE), 
        index(x)[ep[-1]], .CLASS=CLASS(x))
    cnames <- c("Open", "High", "Low", "Close")
    if (hasvol == 1) 
        cnames <- c(cnames, "Volume")
    if (hasadj == 1) 
        cnames <- c(cnames, "Adjusted")
    colnames(tz) <- paste(name,cnames,sep=".") 

    # copy any old xts attributes to new object
    xtsAttributes(tz) <- xtsAttributes(x)
    # return whichever object was originally passed in
    reclass(tz)
  } else
  if(NCOL(x)==1) {
    ep <- endpoints(x, period)
    tz <- as.double(as.matrix(x))
    
    # ohlcz is the F77 routine to aggregate single-column data to another periodicity
    q <- .Fortran("ohlcz", ep = as.integer(ep), lep = as.integer(length(ep)), 
        ia = as.double(tz), lia = as.integer(length(tz)), 
        ret = as.double(rep(0, (length(ep) - 1) * 4)), PACKAGE = "xts")
    tz <- xts(matrix(q$ret, nc = 4, byrow = TRUE), index(x)[ep[-1]],.CLASS=CLASS(x))
    colnames(tz) = paste(name,c("Open", "High", "Low", "Close"),sep=".")

    # copy any old xts attributes to new object
    xtsAttributes(tz) <- xtsAttributes(x)
    # return whichever object was originally passed in
    reclass(tz)
  }
  else {
    stop("'x' must be a single column or an OHLC type object")
  }
}

`to.minutes` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,'minute',name=name)
}
`to.minutes3` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,'minutes',k=3,name=name)
}
`to.minutes5` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,'minutes',k=5,name=name)
}
`to.minutes10` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,'minutes',k=10,name=name)
}
`to.minutes15` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,'minutes',k=15,name=name)
}
`to.minutes30` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,'minutes',k=30,name=name)
}
`to.hourly` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,'hours',name=name)
}
`to.daily` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,'days',name=name)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.weekly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,'weeks',name=name)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.monthly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,'months',name=name)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.quarterly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,'quarters',name=name)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.yearly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,'years',name=name)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`.drop.time` <-
function(x) {
  # function to remove HHMMSS portion of time index
  if(!inherits(x,"its")) {
    x <- as.xts(x)
    indexClass(x) <- "Date"
    reclass(x)
  } else x
}
