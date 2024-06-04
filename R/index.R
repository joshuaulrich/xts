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


#' Get and Replace the Class of an xts Index
#' 
#' Functions to get and replace an xts object's index values and it's
#' components.
#' 
#' An xts object's index is stored internally as the number of seconds since
#' UNIX epoch in the UTC timezone. The `.index()` and `.index<-` functions get
#' and replace the internal numeric value of the index, respectively. These
#' functions are primarily for internal use, but are exported because they may
#' be useful for users.
#' 
#' The replacement method also updates the [`tclass()`] and [`tzone()`] of the
#' index to match the class and timezone of the new index, respectively. The
#' `index()` method converts the internal numeric index to the class specified
#' by the 'tclass' attribute and with the timezone specified by the 'tzone'
#' attribute before returning the index values to the user.
#' 
#' The `.indexXXX()` functions below extract time components from the internal
#' time index. They return values like the values of [POSIXlt] components.
#' 
#' \describe{
#'   \item{`.indexsec`}{0 - 61: seconds of the minute (local time)}
#'   \item{`.indexmin`}{0 - 59: minutes of the hour (local time)}
#'   \item{`.indexhour`}{0 - 23: hours of the day (local time)}
#'   \item{`.indexDate`}{date as seconds since the epoch (UTC *not local time*}
#'   \item{`.indexday`}{date as seconds since the epoch (UTC *not local time*}
#'   \item{`.indexwday`}{0 - 6: day of the week (Sunday - Saturday, local time)}
#'   \item{`.indexmday`}{1 - 31: day of the month (local time)}
#'   \item{`.indexweek`}{weeks since the epoch (UTC *not local time*}
#'   \item{`.indexmon`}{0 - 11: month of the year (local time)}
#'   \item{`.indexyear`}{years since 1900 (local time)}
#'   \item{`.indexyday`}{0 - 365: day of the year (local time, 365 only in leap years)}
#'   \item{`.indexisdst`}{1, 0, -1: Daylight Saving Time flag. Positive if
#'       Daylight Saving Time is in effect, zero if not, negative if unknown.}
#' }
#' 
#' Changes in timezone, index class, and index format internal structure, by
#' \pkg{xts} version:
#' 
#' \describe{
#'   \item{Version 0.12.0:}{The `.indexTZ`, `.indexCLASS` and `.indexFORMAT`
#'     attributes are no longer stored on xts objects, only on the index itself.
#'     \cr\cr
#'     The `indexTZ()`, `indexClass()`, and `indexFormat()` functions (and
#'     their respective replacement methods) are deprecated in favor of their
#'     respective `tzone()`, `tclass()`, and `tformat()` versions. The previous
#'     versions throw a warning that they're deprecated, but they will continue
#'     to work. They will never be removed or throw an error. Ever.
#'     \cr\cr
#'     The new versions are careful to look for the old attributes on the xts
#'     object, in case they're ever called on an xts object that was created prior
#'     to the attributes being added to the index itself.
#'     \cr\cr
#'     You can set `options(xts.warn.index.missing.tzone = TRUE)` and
#'     `options(xts.warn.index.missing.tclass = TRUE)` to identify xts objects
#'     that do not have a 'tzone' or 'tclass' attribute on the index, even if
#'     there is a 'tzone' or 'tclass' attribute on the xts object itself. The
#'     warnings will be thrown when the object is printed.
#'     Use `x <- as.xts(x)` to update these objects to the new structure.}
#' 
#'   \item{Version 0.9.8:}{The index timezone is now set to "UTC" for time classes
#'     that do not have any intra-day component (e.g. days, months, quarters).
#'     Previously the timezone was blank, which meant "local time" as determined by
#'     \R and the OS.}
#' 
#'   \item{Version 0.9.2:}{There are new get/set methods for the timezone, index
#'     class, and index format attributes: `tzone()` and, `tzone<-`, `tclass()`
#'     and `tclass<-`, and `tformat()` and `tformat<-`. These new functions are
#'     aliases to their `indexTZ()`, `indexClass()`, and `indexFormat()`
#'     counterparts.}
#' 
#'   \item{Version 0.7.5:}{The timezone, index class, and index format were added
#'     as attributes to the index itself, as 'tzone', 'tclass', and 'tformat',
#'     respectively. This is in order to remove those three attributes from the xts
#'     object, so they're only on the index itself.
#'     \cr\cr
#'     The `indexTZ()`, `indexClass()`, and `indexFormat()` functions (and their
#'     respective replacement methods) will continue to work as in prior \pkg{xts}
#'     versions. The attributes on the index take priority over their respective
#'     counterparts that may be on the xts object.}
#' 
#'   \item{Versions 0.6.4 and prior:}{Objects track their timezone and index class
#'     in their '.indexTZ' and '.indexCLASS' attributes, respectively.}
#' }
#' 
#' @param x An xts object.
#' @param value A new time index value.
#' @param \dots Arguments passed to other methods.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`tformat()`] describes how the index values are formatted when
#' printed, [`tclass()`] documents how \pkg{xts} handles the index class, and
#' [`tzone()`] has more information about index timezone settings.
#' 
#' @keywords ts utilities
#' @examples
#' 
#' x <- timeBasedSeq('2010-01-01/2010-01-01 12:00/H')
#' x <- xts(seq_along(x), x)
#' 
#' # the index values, converted to 'tclass' (POSIXct in this case)
#' index(x)
#' class(index(x))  # POSIXct
#' tclass(x)        # POSIXct
#' 
#' # the internal numeric index
#' .index(x)
#' # add 1 hour (3600 seconds) to the numeric index
#' .index(x) <- index(x) + 3600
#' index(x)
#' 
#' y <- timeBasedSeq('2010-01-01/2010-01-02 12:00')
#' y <- xts(seq_along(y), y)
#' 
#' # Select all observations in the first 6 and last 3 minutes of the
#' # 8th and 15th hours on each day
#' y[.indexhour(y) %in% c(8, 15) & .indexmin(y) %in% c(0:5, 57:59)]
#' 
#' i <- 0:60000
#' focal_date <- as.numeric(as.POSIXct("2018-02-01", tz = "UTC"))
#' y <- .xts(i, c(focal_date + i * 15), tz = "UTC", dimnames = list(NULL, "value"))
#' 
#' # Select all observations for the first minute of each hour
#' y[.indexmin(y) == 0]
#' 
#' # Select all observations on Monday
#' mon <- y[.indexwday(y) == 1]
#' head(mon)
#' tail(mon)
#' unique(weekdays(index(mon))) # check
#' 
#' # Disjoint time of day selections
#' 
#' # Select all observations between 08:30 and 08:59:59.9999  or between 12:00 and 12:14:59.99999:
#' y[.indexhour(y) == 8 & .indexmin(y) >= 30 | .indexhour(y) == 12 & .indexmin(x) %in% 0:14]
#' 
#' ### Compound selections
#' 
#' # Select all observations for Wednesdays or Fridays between 9am and 4pm (exclusive of 4pm):
#' y[.indexwday(y) %in% c(3, 5) & (.indexhour(y) %in%  c(9:15))]
#' 
#' # Select all observations on Monday between 8:59:45 and 09:04:30:
#' 
#' y[.indexwday(y) == 1 & (.indexhour(y) == 8 & .indexmin(y) == 59 & .indexsec(y) >= 45 |
#'                         .indexhour(y) == 9 &
#'                         (.indexmin(y) <  4 | .indexmin(y) ==  4 & .indexsec(y) <= 30))]
#' 
#' i <- 0:30000
#' u <- .xts(i, c(focal_date + i * 1800), tz = "UTC", dimnames = list(NULL, "value"))
#' 
#' # Select all observations for January or February:
#' u[.indexmon(u) %in% c(0, 1)]
#' 
#' # Select all data for the 28th to 31st of each month, excluding any Fridays:
#' u[.indexmday(u) %in% 28:31 & .indexwday(u) != 5]
#' 
#' # Subset by week since origin
#' unique(.indexweek(u))
#' origin <- xts(1, as.POSIXct("1970-01-01"))
#' unique(.indexweek(origin))
#' 
#' # Select all observations in weeks 2515 to 2517.
#' u2 <- u[.indexweek(u) %in% 2515:2517]
#' head(u2); tail(u2)
#' 
#' # Select all observations after 12pm for day 50 and 51 in each year
#' u[.indexyday(u) %in% 50:51 & .indexhour(u) >= 12]
#' 
index.xts <-
function(x, ...) {
  value <- tclass(x)
  if(is.null(value) || !nzchar(value[1L])) {
    warning("index does not have a ", sQuote("tclass"), " attribute\n",
            "    returning c(\"POSIXct\", \"POSIXt\")")
    ix <- .index(x)
    attr(ix, "tclass") <- attr(ix, "class") <- c("POSIXct", "POSIXt")
    return(ix)
  }
  #  if tclass is Date, POSIXct time is set to 00:00:00 GMT. Convert here
  #  to avoid ugly and hard to debug TZ conversion.  What will this break? 
  if(value[[1]] == "Date")
    #return( as.Date(.index(x)/86400) )
    return( structure(.index(x) %/% 86400, class="Date")) 
    

  #x.index  <- structure(.index(x), class=c("POSIXct","POSIXt"))
  x.index  <- .POSIXct(.index(x), tz=attr(.index(x), "tzone"))

  if(!is.list(value)) 
    value <- as.list(value)

  switch(value[[1]],
    multitime = as.Date(as.character(x.index)),
    POSIXt = {
      # get specific ct/lt value
      do.call(paste('as',value[[2]],sep='.'),list(x.index))
    },
    POSIXct = as.POSIXct(x.index),
    POSIXlt = as.POSIXlt(x.index),
    timeDate = {
      if(!requireNamespace("timeDate", quietly=TRUE))
          stop("package:",dQuote("timeDate"),"cannot be loaded.")
      timeDate::as.timeDate(x.index)
    },
    chron = ,
    dates = {
      if(!requireNamespace("chron", quietly=TRUE))
        stop("package:",dQuote("chron"),"cannot be loaded.")
      chron::as.chron(format(x.index))
    },
    #Date = as.Date(as.character(x.index)),  # handled above
    yearmon = as.yearmon(x.index),
    yearqtr = as.yearqtr(x.index),
    stop(paste('unsupported',sQuote('tclass'),'indexing type:',value[[1]]))
  )
}

#' @rdname index.xts
`index<-.xts` <- function(x, value) {
  if(length(index(x)) != length(value)) stop('length of index vectors does not match')

  if( !timeBased(value) ) 
    stop(paste('unsupported',sQuote('index'),
               'index type of class',sQuote(class(value))))

  # copy original index attributes
  ixattr <- attributes(attr(x, 'index'))

  # set index to the numeric value of the desired index class
  if(inherits(value,"Date"))
    attr(x, 'index') <- structure(unclass(value)*86400, tclass="Date", tzone="UTC")
  else attr(x, 'index') <- as.numeric(as.POSIXct(value))

  # ensure new index is sorted
  if(!isOrdered(.index(x), strictly=FALSE))
    stop("new index needs to be sorted")

  # set tclass attribute to the end-user specified class
  attr(attr(x, 'index'), 'tclass') <- class(value)

  # set tzone attribute
  if(isClassWithoutTZ(object = value)) {
    attr(attr(x, 'index'), 'tzone') <- 'UTC'
  } else {
    if (is.null(attr(value, 'tzone'))) {
      # ensure index has tzone attribute if value does not
      attr(attr(x, 'index'), 'tzone') <- ixattr[["tzone"]]
    } else {
      attr(attr(x, 'index'), 'tzone') <- attr(value, 'tzone')
    }
  }
  return(x)
}

#' @rdname index.xts
`time<-.xts` <- `index<-.xts`

#' @rdname index.xts
time.xts <- index.xts

#' @rdname index.xts
`.index` <- function(x, ...) {
  if(is.list(attr(x, "index"))) {
    attr(x, 'index')[[1]]
  } else attr(x, "index")
}

#' @rdname index.xts
`.index<-` <- function(x, value) {
  if(timeBased(value)) {
    if(inherits(value, 'Date')) {
      attr(x, 'index') <- as.numeric(value)
    } else {
      attr(x, 'index') <- as.numeric(as.POSIXct(value))
    }
  } else 
  if(is.numeric(value)) {
    attr(value, 'tclass') <- tclass(x)
    attr(value, 'tzone') <- tzone(x)
    attr(x, 'index') <- value
  } else stop(".index is used for low level operations - data must be numeric or timeBased")
  return(x)
}

#' @rdname index.xts
`.indexsec` <- function(x) {
  as.POSIXlt(.POSIXct(.index(x), tz=tzone(x)))$sec
}

#' @rdname index.xts
`.indexmin` <- function(x) {
  as.POSIXlt(.POSIXct(.index(x), tz=tzone(x)))$min
}

#' @rdname index.xts
`.indexhour` <- function(x) {
  as.POSIXlt(.POSIXct(.index(x), tz=tzone(x)))$hour
}

#' @rdname index.xts
`.indexmday` <- function(x) {
  as.POSIXlt(.POSIXct(.index(x), tz=tzone(x)))$mday
}

#' @rdname index.xts
`.indexmon` <- function(x) {
  as.POSIXlt(.POSIXct(.index(x), tz=tzone(x)))$mon
}

#' @rdname index.xts
`.indexyear` <- function(x) {
  as.POSIXlt(.POSIXct(.index(x), tz=tzone(x)))$year
}

#' @rdname index.xts
`.indexwday` <- function(x) {
  as.POSIXlt(.POSIXct(.index(x), tz=tzone(x)))$wday
}

#' @rdname index.xts
`.indexbday` <- function(x) {
  # is business day T/F
  as.POSIXlt(.POSIXct(.index(x), tz=tzone(x)))$wday %% 6 > 0
}

#' @rdname index.xts
`.indexyday` <- function(x) {
  as.POSIXlt(.POSIXct(.index(x), tz=tzone(x)))$yday
}

#' @rdname index.xts
`.indexisdst` <- function(x) {
  as.POSIXlt(.POSIXct(.index(x), tz=tzone(x)))$isdst }

#' @rdname index.xts
`.indexDate` <- function(x) {
  .index(x) %/% 86400L
}

#' @rdname index.xts
`.indexday` <- .indexDate

#' @rdname index.xts
`.indexweek` <- function(x) {
  (.index(x) + (3 * 86400)) %/% 86400 %/% 7
}

#' @rdname index.xts
`.indexyweek` <- function(x) {
  ((.index(x) + (3 * 86400)) %/% 86400 %/% 7) -
    ((startOfYear() * 86400 + (3 * 86400)) %/% 86400 %/% 7)[.indexyear(x) + 1]
}

#' @rdname index.xts
`convertIndex` <-
function(x,value) {
  tclass(x) <- value
  x
}

.update_index_attributes <- function(x) {
  suppressWarnings({
    tclass(x) <- tclass(x)
    tzone(x) <- tzone(x)
  })
  return(x)
}
