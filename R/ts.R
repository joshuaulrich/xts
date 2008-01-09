# methods for handling ts <--> xts

`re.ts` <-
function(x,...) {
  ts(coredata(x),
     start=as.numeric(start(x)),
     end=as.numeric(end(x)),
     frequency=attr(x,'frequency'),
     names=colnames(x))
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

  if(missing(dateFormat)) {
    dateFormat <- ifelse(max(time(x)) > 86400,'POSIXct','Date')
  }

  # added '...' to call for handling of tz params -jar
  # now using time() to extract time from tsp
  order.by <- do.call(paste('as',dateFormat,sep='.'),list(as.numeric(time(x)),...))
  xx <- xts(x.mat,
            order.by=order.by,
            frequency=frequency(x),
            .CLASS='ts',
            ...)
  xx
}

`as.ts.xts` <-
function(x,...) {
  if(attr(x,'.CLASS')=='ts') return(re.ts(x,...))
  ts(coredata(x),...)
}
