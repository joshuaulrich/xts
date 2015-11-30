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
  UseMethod("indexTZ")
}

tzone <- function(x, ...) {
  UseMethod("tzone")
}

`indexTZ<-` <- function(x, value) {
  UseMethod("indexTZ<-")
}

`tzone<-` <- function(x, value) {
  UseMethod("tzone<-")
}

`tzone<-.xts` <- `indexTZ<-.xts` <- function(x, value) {
  if( is.null(value) ) value <- ""

  attr(x, ".indexTZ") <- attr(x, "tzone") <- structure(value,.Names="TZ")
  attr(attr(x,"index"),"tzone") <- structure(value,.Names="TZ")
  x
}


tzone.default <- indexTZ.default <- function(x, ...) {
  attr(x, ".indexTZ")
}

tzone.xts <- indexTZ.xts <- function(x, ...)
{
  tzone <- attr(attr(x, "index"), "tzone")
  if(is.null(tzone))
    tzone <- attr(x, ".indexTZ")
  if(is.null(tzone))
    return("")
  else
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
  if(any(indexClass(x) %in% .classesWithoutTZ)) {
    # warn if indexTZ is not UTC or GMT (GMT is not technically correct, since
    # it *is* a timezone, but it should work for all practical purposes)
    if (!(indexTZ(x) %in% c("UTC","GMT")))
      warning(paste0("index class is ", paste(class(index(x)), collapse=", "),
        ", which does not support timezones.\nExpected 'UTC' timezone",
        ", but indexTZ is ", ifelse(indexTZ(x)=="", "''", indexTZ(x))), call.=FALSE)
    else
      return()
  }
  if(!is.null(indexTZ(x)) && indexTZ(x) != "" &&
     !identical(STZ, as.character(indexTZ(x))))
    warning(paste("timezone of object (",indexTZ(x),
                  ") is different than current timezone (",STZ,").",sep=""),
           call.=FALSE)
}
