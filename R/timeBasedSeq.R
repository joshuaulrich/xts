`timeBasedSeq` <- 
function(x, retclass=NULL, length.out=NULL) {
  x <- gsub('::','/',x, perl=TRUE)  # replace all '::' range ops with '/'
  x <- gsub('[-:]','',x, perl=TRUE) # strip all remaining '-' and ':' seps
  x <- gsub('[ ]','',x, perl=TRUE) # strip all remaining white space
  x <- unlist(strsplit(x,"/"))
  from <- x[1]
  to   <- x[2]
  BY   <- x[3]

  year  <- as.numeric(substr(from,1,4))
  month <- as.numeric(substr(from,5,6))
  day   <- as.numeric(substr(from,7,8))
  hour  <- as.numeric(substr(from,9,10))
  mins  <- as.numeric(substr(from,11,12))
  secs  <- as.numeric(substr(from,13,14))

  time.args.from <- as.list(unlist(sapply(c(year,month,day,hour,mins,secs),
                               function(x) if(!is.na(x)) x)
                        ))

  from <- do.call('firstof',time.args.from) 

  
  year  <- as.numeric(substr(to,1,4))
  month <- as.numeric(substr(to,5,6))
  day   <- as.numeric(substr(to,7,8))
  hour  <- as.numeric(substr(to,9,10))
  mins  <- as.numeric(substr(to,11,12))
  secs  <- as.numeric(substr(to,13,14))

  time.args.to <- as.list(unlist(sapply(c(year,month,day,hour,mins,secs),
                               function(x) if(!is.na(x)) x)
                        ))

  to <- do.call('lastof',time.args.to) 

  max.resolution <- max(length(time.args.from), length(time.args.to))
  resolution <- c('year','month','day','hour','mins','secs')[max.resolution]

  if(!is.na(BY)) resolution <- names(match.arg(BY, list(year  ='Y',
                                                        month ='m',
                                                        day   ='d',
                                                        hour  ='H',
                                                        mins  ='M',
                                                        secs  ='S')))

  convert.to <- 'Date'
  if(max.resolution == 2 || resolution == 'month' ) convert.to <- 'yearmon'
  if(max.resolution >  3 || resolution %in% c("H","M","S")) convert.to <- 'POSIXct'

  if(!missing(retclass) && is.null(retclass)) {
    # return the calculated values only
    return(list(from=from,to=to,by=resolution,length.out=length.out))
  }

  if(is.null(length.out)) {
    SEQ <- seq(from,to,by=resolution)
  } else {
    SEQ <- seq(from, by=resolution, length.out=length.out)
  }

  if(!is.null(retclass)) convert.to <- retclass
  do.call(paste('as',convert.to,sep='.'), list(SEQ))

}

