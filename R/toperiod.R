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
function(x,period='months',k=1,indexAt=NULL,name=NULL,...)
{
  if(is.null(name)) name <- deparse(substitute(x))

  # test if is class 'xts', if not, convert to 'xts'
  if(!is.xts(x)) {
    x <- as.xts(x)
    RECLASS <- TRUE
  } else RECLASS <- FALSE
  
  
  if(any(is.na(x))) {
    x <- na.omit(x)  # can't calculate aggregation with NA values  ??? any suggestions
    warning("missing values removed from data")
  }

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
  }
  else {
    stop("'x' must be a single column or an OHLC type object")
  }

  # allow for nice and user specifiable formatting of dates - per Brian Peterson
  if(!is.null(indexAt)) {
    if(indexAt %in% c('yearmon','yearqtr')) {
      if(CLASS(x) %in% c('ts','its','timeSeries')) {
        # timeSeries can't handle either of these time-based indexes,
        # so default to startof rather than ugly <NA>
        # ts causes malloc issues when passed a non-numeric index - BAD!
        indexAt <- 'firstof'
      } else indexClass(tz) <- as.character(indexAt)
    }
    if(indexAt=='startof') {
      index(tz) <- index(x)[startof(x,by=period)]
    }
    if(indexAt=='endof') {
      index(tz) <- index(x)[endof(x,by=period)]
    }
    if(indexAt=='firstof') {
      if(period %in% c('months','quarters')) {
        if(period=='months') {
          indexClass(tz) <- 'yearmon'
        } else indexClass(tz) <- 'yearqtr'
      # convert year* to POSIXct
      #indexClass(tz) <- 'POSIXct'
      indexClass(tz) <- indexClass(x)
      }
    }
    if(indexAt=='lastof') {
      if(period %in% c('months','quarters')) {
      # use yearmon/yearqtr as shortcuts to 1rst of month :)
        if(period=='months') {
          indexClass(tz) <- 'yearmon'
        } else indexClass(tz) <- 'yearqtr'
      # add one period and subtract a day
      padv <- ifelse(period=='months',1,3)
      index(tz) <- (index(tz)+padv/12)
      # convert year* to POSIXct
      indexClass(tz) <- 'POSIXct'
      index(tz) <- index(tz)-86400
      }
    }
  }

  if(RECLASS) return(reclass(tz))
  tz

}

`to.minutes` <-
function(x,k,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  if(missing(k)) k <- 1
  to.period(x,'minutes',k=k,name=name,...)
}
`to.minutes3` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=3,name=name,...)
}
`to.minutes5` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=5,name=name,...)
}
`to.minutes10` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=10,name=name,...)
}
`to.minutes15` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=15,name=name,...)
}
`to.minutes30` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'minutes',k=30,name=name,...)
}
`to.hourly` <-
function(x,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  to.period(x,'hours',name=name,...)
}
`to.daily` <-
function(x,drop.time=TRUE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'days',name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.weekly` <-
function(x,drop.time=TRUE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'weeks',name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.monthly` <-
function(x,indexAt='yearmon',drop.time=FALSE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'months',indexAt=indexAt,name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.quarterly` <-
function(x,indexAt='yearqtr',drop.time=FALSE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'quarters',indexAt=indexAt,name=name,...)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.yearly` <-
function(x,drop.time=TRUE,name,...)
{
  if(missing(name)) name <- deparse(substitute(x))
  x <- to.period(x,'years',name=name,...)
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
