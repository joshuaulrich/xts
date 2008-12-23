# methods for handling ts <--> xts

`re.ts2` <-
function(x,...) {
  # attempt to provide a more robust reclass 'ts' method
  na.replace <- function(x) {
    na.removed <- attr(x,'na.action')
    if(class(na.removed) != 'omit') return()
    nrows <- NROW(x)
    ncols <- NCOL(x)
    xx <- vector('numeric',length=(nrows+length(na.removed)))
    xx[ na.removed,] <- NA
    xx[-na.removed] <- x
    xx
  }


}

`re.ts` <-
function(x,...) {
  #if(periodicity(x)$units == 'days' & !inherits(indexClass(x),"Date"))
  #  indexClass(x) <- "Date"
  # major issue with quick reclass.  Basically fails on data < 1970...
  #tsp.attr <- attr(x,'.tsp')
  #freq.attr <- attr(x,'.frequency')

  #xtsAttributes(x) <- NULL

  #ts(coredata(x), start=tsp.attr[1],frequency=freq.attr)
  dim <- attr(x, 'dim')
  dn <- attr(x,'dimnames')
  if(is.null(dim) || dim[2]==1) {
    attr(x,'dim') <- attr(x, 'dimnames') <- NULL
  }
  as.ts(x)
  #tsp(x) <- attr(x, '.tsp')
  #zoo:::as.ts.zoo(x)
}

`as.xts.ts` <-
function(x,dateFormat,...) {
  x.mat <- structure(as.matrix(x),dimnames=dimnames(x))
  colnames(x.mat) <- colnames(x)

  # quick hueristic - if numeric index is larger than one
  # full day of seconds (60*60*24) than use POSIXct, otherwise
  # assume we are counting my days, not seconds, and use Date -jar
  # 
  # I am sure this can be improved upon, but for now it is effective
  # in most circumstances.  Will break if frequency or time is from 1
  # not _break_ but be less useful
  # a bigger question is _should_ it throw an error if it can't guess,
  # or should the user simply beware.

  if(missing(dateFormat)) {
    if(frequency(x) == 1) {
      # assume yearly series: Date
      yr <- tsp(x)[1] %/% 1
      mo <- tsp(x)[1] %%  1
      if(mo %% (1/12) != 0 || yr > 3000) {
        # something finer than year.month is specified - can't reliable convert
        dateFormat <- ifelse(max(time(x)) > 86400,'POSIXct','Date')
        order.by <- do.call(paste('as',dateFormat,sep='.'),
                            list(as.numeric(time(x)),origin='1970-01-01',...))
      } else {
        mo <- ifelse(length(mo) < 1, 1,floor(mo * 12)+1)
        order.by <- seq.Date(as.Date(firstof(yr,mo),origin='1970-01-01'),length.out=length(x),by='year')   
      }
    } else
      if(frequency(x) == 4) {
        # quarterly series: yearqtr
        order.by <- as.yearqtr(time(x))
      } else
        if(frequency(x) == 12) {
           # monthly series: yearmon
           order.by <- as.yearmon(time(x))
        } else stop('could not convert index to appropriate type')
  } else  {
    order.by <- do.call(paste('as',dateFormat,sep='.'),
                        list(as.numeric(time(x)),...))
  }


  xx <- xts(x.mat,
            order.by=order.by,
            frequency=frequency(x),
            .CLASS='ts',
            .CLASSnames=c('frequency'),
#            .tsp=tsp(x),
#            .frequency=frequency(x),
            ...)
  attr(xx, 'tsp') <- NULL
  xx
}


`as.ts.xts` <-
function(x,...) {
  #if(attr(x,'.CLASS')=='ts') return(re.ts(x,...))
  if(!is.null(dim(x)) && dim(x)[[2]]==1)
    dim(x) <- NULL
  ts(coredata(x), frequency=frequency(x), ...)
}
