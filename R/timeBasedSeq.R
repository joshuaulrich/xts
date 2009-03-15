#
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.


`timeBasedSeq` <- 
function(x, retclass=NULL, length.out=NULL) {
  if(!is.character(x))
    # allows for unquoted numerical expressions to work
    x <- deparse(match.call()$x)

  x <- gsub('::','/',x, perl=TRUE)  # replace all '::' range ops with '/'
  x <- gsub('[-:]','',x, perl=TRUE) # strip all remaining '-' and ':' seps
  x <- gsub('[ ]','',x, perl=TRUE) # strip all remaining white space
  x <- unlist(strsplit(x,"/"))
  from <- x[1]
  to   <- x[2]
  BY   <- x[3]

  # need to test for user specified length.out, currently just overriding
  if(from == "")
    from <- NA

  if(!is.na(from)) {
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
  } 
  else time.args.from <- list()
 
  # only calculate if to is specified
  if(!is.na(to)) { 
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
  } 
  else time.args.to <- list()

  max.resolution <- max(length(time.args.from), length(time.args.to))

  # if neither is set
  if(max.resolution == 0) 
    max.resolution <- 1

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

 
  if(is.na(to) && missing(length.out))
    length.out <- 1L

  if(((!missing(retclass) && is.null(retclass)) || any(is.na(to),is.na(from)))) {
    # return the calculated values only
    return(list(from=from,to=to,by=resolution,length.out=length.out))
  }

  if(is.null(length.out)) {
    SEQ <- seq(from,to,by=resolution)
  } else {
    SEQ <- seq(from, by=resolution, length.out=length.out)
  }

  if(!is.null(retclass)) convert.to <- retclass
  if(convert.to == 'POSIXct') {
    structure(SEQ, class=c('POSIXt','POSIXct'))  # need to force the TZ to be used
  } else
  do.call(paste('as',convert.to,sep='.'), list(SEQ))

}

