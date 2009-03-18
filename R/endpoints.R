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


`endpoints` <-
function(x,on='months',k=1) {
  if(timeBased(x)) x <- xts(, order.by=x)
  if(!is.xts(x)) x <- try.xts(x, error='must be either xts-coercible or timeBased')

  #sys.TZ <- Sys.getenv('TZ')
  #on.exit(Sys.setenv(TZ=sys.TZ))
  #Sys.setenv(TZ='GMT')
  #indexClass(x) <- 'POSIXct'

  if(on == 'years') {
    as.integer(c(0, which(diff(as.POSIXlt(index(x))$year %/% k + 1) != 0), NROW(x)) )
  } else

  if(on == 'quarters') {
    xi <- (as.POSIXlt(index(x))$mon%/%3) + 1
    as.integer(c(0,which(diff(xi) != 0),NROW(x)))
  } else 

  if(on == 'months') {
    as.integer(c(0, which(diff(as.POSIXlt(index(x))$mon %/% k + 1) != 0), NROW(x)) )
  } else 
  if(on == 'weeks') {
    as.integer(c(0, which(diff( (.index(x) + (3L * 86400L)) %/% 604800L %/% k + 1) != 0), NROW(x)) )
    #c(0,which(diff(as.numeric(format(index(x),'%W')) %/% k + 1) != 0),NROW(x))
  } else
  if(on == 'days') {
    #c(0, which(diff(as.POSIXlt(index(x))$yday %/% k + 1) != 0), NROW(x)) 
    as.integer(c(0, which(diff(.index(x) %/% 86400L %/% k + 1) != 0), NROW(x)))
  } else
  if(on == 'hours') {
    #c(0, which(diff(as.POSIXlt(index(x))$hour %/% k + 1) != 0), NROW(x)) 
    as.integer(c(0, which(diff(.index(x) %/% 3600L %/% k + 1) != 0), NROW(x)))
  } else
  if(on == 'minutes' || on == 'mins') {
    #c(0, which(diff(as.POSIXlt(index(x))$min %/% k + 1) != 0), NROW(x)) 
    as.integer(c(0, which(diff(.index(x) %/% 60L %/% k + 1) != 0), NROW(x)))
  } else
  if(on == 'seconds' || on == 'secs') {
    #c(0, which(diff(as.POSIXlt(index(x))$sec %/% k + 1) != 0), NROW(x)) 
    as.integer(c(0, which(diff(.index(x) %/%  k + 1) != 0), NROW(x)))
  } else
  if(on == 'milliseconds' || on == 'ms') {
    as.integer(c(0, which(diff(.index(x)%/%.001%/%k + 1) != 0), NROW(x)))
  } else
  if(on == 'microseconds' || on == 'us') {
    as.integer(c(0, which(diff(.index(x)%/%.000001%/%k + 1) != 0), NROW(x)))
  } else {
    stop('unsupported "on" argument')
  }
}

`startof` <-
function(x,by='months', k=1) {
  ep <- endpoints(x,on=by, k=k)
  (ep+1)[-length(ep)]
}

`endof` <-
function(x,by='months', k=1) {
  endpoints(x,on=by, k=k)[-1]
}

`firstof` <-
function(year=1970,month=1,day=1,hour=0,min=0,sec=0,tz="") {
  ISOdatetime(year,month,day,hour,min,sec,tz)
}

#`lastof` <-
#function(year=1970,month=12,day=31,hour=23,min=59,sec=59,tz="") {
#  mon.lengths <- c(31,28,31,30,31,30,31,31,30,31,30,31)
#  if(missing(day)) {
#    if(month %in% 2 && year%%4 %in% 0) {
#      # is it a leap year?
#      day <- ifelse(month %in% 2 && year%%400 %in% 0,28,29)
#    } else day <- mon.lengths[month]
#  }
#  # horrible workaround of bug? in strptime. -- jar
#  if(c(year,month,day,hour,min,sec) == c(1969,12,31,23,59,59) &&
#     Sys.getenv("TZ") %in% c("","GMT","UTC")) sec <- 58.9 
#  ISOdatetime(year,month,day,hour,min,sec,tz)
#}

lastof <-
function (year = 1970, month = 12, day = 31, hour = 23, min = 59, sec = 59, tz = "") 
{
    mon.lengths <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 
        30, 31)
    if (missing(day)) {
        day <- ifelse(month %in% 2, ifelse(((year%%4 %in% 0 & 
            !year%%100 %in% 0) | (year%%400 %in% 0)), 29, 28), 
            mon.lengths[month])
    }
    if (length(c(year, month, day, hour, min, sec)) == 7 && c(year, 
        month, day, hour, min, sec) == c(1969, 12, 31, 23, 59, 
        59) && Sys.getenv("TZ") %in% c("", "GMT", "UTC")) 
        sec <- 58.9
    ISOdatetime(year, month, day, hour, min, sec, tz)
}
