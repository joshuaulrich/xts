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

indexTZ <- function(x, ...)
{
  .Deprecated("tzone", "xts")
  tzone(x, ...)
}

tzone <- function(x, ...) {
  UseMethod("tzone")
}

`indexTZ<-` <- function(x, value) {
  .Deprecated("tzone<-", "xts")
  `tzone<-`(x, value)
}

`tzone<-` <- function(x, value) {
  UseMethod("tzone<-")
}

`tzone<-.xts` <-
function(x, value)
{
  if (is.null(value)) {
    value <- ""
  }

  tzone <- as.character(value)
  attr(attr(x, "index"), "tzone") <- tzone
  # Remove tz attrs (object created before 0.10-3)
  attr(x, ".indexTZ") <- NULL
  attr(x, "tzone") <- NULL
  x
}

tzone.default <-
function(x, ...)
{
  attr(x, "tzone")
}

`tzone<-.default` <-
function(x, value)
{
  if (!is.null(value)) {
    tzone <- as.character(value)
  }
  attr(x, "tzone") <- value
  x
}

tzone.xts <-
function(x, ...)
{
  tzone <- attr(attr(x, "index"), "tzone")

  # For xts objects created pre-0.10.3
  if (is.null(tzone)) {
    warning("index does not have a ", sQuote("tzone"), " attribute")

    tzone <- attr(x, "tzone")
    if (is.null(tzone)) {
      tzone <- attr(x, ".indexTZ")
    }
    if (is.null(tzone)) {
      warning("object does not have a ", sQuote("tzone"), " or ",
              sQuote(".indexTZ"), " attribute")
      tzone <- ""
    }
  }
  return(tzone)
}

.classesWithoutTZ <- c("chron","dates","times","Date","yearmon","yearqtr")

check.TZ <- function(x, ...)
{
  #if( !getOption("xts_check_TZ", FALSE))
  #  return()
  check <- getOption("xts_check_TZ")
  if( !is.null(check) && !check)
    return()
  STZ <- as.character(Sys.getenv("TZ"))
  if(any(tclass(x) %in% .classesWithoutTZ)) {
    # warn if tzone is not UTC or GMT (GMT is not technically correct, since
    # it *is* a timezone, but it should work for all practical purposes)
    if (!(tzone(x) %in% c("UTC","GMT")))
      warning(paste0("index class is ", paste(class(index(x)), collapse=", "),
        ", which does not support timezones.\nExpected 'UTC' timezone",
        ", but tzone is ", sQuote(tzone(x))), call.=FALSE)
    else
      return()
  }
  if(!is.null(tzone(x)) && tzone(x) != "" &&
     !identical(STZ, as.character(tzone(x))))
    warning(paste("timezone of object (",tzone(x),
                  ") is different than current timezone (",STZ,").",sep=""),
           call.=FALSE)
}
