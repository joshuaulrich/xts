#
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.


endpoints <-
function(x,on='months',k=1) {
  if(timeBased(x)) {
    NR <- length(x)
    x <- xts(, order.by=x)
  } else NR <- NROW(x)
  addlast <- TRUE  # remove automatic NR last value

  if(!is.xts(x)) 
    x <- try.xts(x, error='must be either xts-coercible or timeBased')

  # special-case "secs" and "mins" for back-compatibility
  if(on == "secs" || on == "mins")
    on <- substr(on, 1L, 3L)
  on <- match.arg(on, c("years", "quarters", "months", "weeks", "days", "hours",
    "minutes", "seconds", "milliseconds", "microseconds", "ms", "us"))

  # posixltindex is costly in memory (9x length of time)
  # make sure we really need it
  if(on %in% c('years','quarters','months','weeks','days'))
    posixltindex <- as.POSIXlt(.POSIXct(.index(x)),tz=tzone(x))

  switch(on,
    "years" = {
      as.integer(c(0, which(diff(posixltindex$year %/% k + 1) != 0), NR))
    },
    "quarters" = {
      ixqtr <- posixltindex$year * 100L + 190000L + posixltindex$mon %/% 3 + 1
      as.integer(c(0,which(diff(ixqtr) != 0),NR))
    },
    "months" = {
      ixmon <- posixltindex$year * 100L + 190000L + posixltindex$mon
      ep <- .Call("endpoints", ixmon, 1L, 1L, addlast, PACKAGE='xts')
      if(k > 1)
        ep[seq(1,length(ep),k)]
      else ep
    },
    "weeks" = {
      .Call("endpoints", .index(x)+3L*86400L, 604800L, k, addlast, PACKAGE='xts')
    },
    "days" = {
      ixyday <- posixltindex$year * 1000L + 1900000L + posixltindex$yday
      .Call("endpoints", ixyday, 1L, k, addlast, PACKAGE='xts')
    },
    # non-date slicing should be indifferent to TZ and DST, so use math instead
    "hours" = {
      .Call("endpoints", .index(x), 3600L, k, addlast, PACKAGE='xts')
    },
    "minutes" = {
      .Call("endpoints", .index(x), 60L, k, addlast, PACKAGE='xts')
    },
    "seconds" = {
      .Call("endpoints", .index(x), 1L, k, addlast, PACKAGE='xts')
    },
    "ms" = ,
    "milliseconds" = {
      sec2ms <- .index(x) * 1e3
      .Call("endpoints", sec2ms, 1L, k, addlast, PACKAGE='xts')
    },
    "us" = ,
    "microseconds" = {
      sec2us <- .index(x) * 1e6
      .Call("endpoints", sec2us, 1L, k, addlast, PACKAGE='xts')
    }
  )
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

lastof <-
function (year = 1970,
          month = 12,
          day = 31,
          hour = 23, 
          min = 59,
          sec = 59,
          subsec=.99999, tz = "") 
{
    if(!missing(sec) && sec %% 1 != 0)
      subsec <- 0
    sec <- ifelse(year < 1970, sec, sec+subsec) # <1970 asPOSIXct bug workaround
    #sec <- sec + subsec
    mon.lengths <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 
        30, 31)
    if (missing(day)) {
        day <- ifelse(month %in% 2, ifelse(((year%%4 %in% 0 & 
            !year%%100 %in% 0) | (year%%400 %in% 0)), 29, 28), 
            mon.lengths[month])
    }
    # strptime has an issue (bug?) which returns NA when passed
    # 1969-12-31-23-59-59; pass 58.9 secs instead.
    if (length(c(year, month, day, hour, min, sec)) == 6 &&
        all(c(year, month, day, hour, min, sec) == c(1969, 12, 31, 23, 59, 59)) &&
        Sys.getenv("TZ") %in% c("", "GMT", "UTC"))
        sec <- sec-1
    ISOdatetime(year, month, day, hour, min, sec, tz)
}
