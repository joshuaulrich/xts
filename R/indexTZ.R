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

indexTZ <- function(x, ...)
{
  UseMethod("indexTZ")
}

`indexTZ<-` <- function(x, value) {
  UseMethod("indexTZ<-")
}

`indexTZ<-.xts` <- function(x, value) {
  if( is.null(value) ) value <- ""

  attr(x, ".indexTZ") <- structure(value,.Names="TZ")
  attr(attr(x,"index"),"tzone") <- structure(value,.Names="TZ")
  x
}

indexTZ.default <- function(x, ...) {
  attr(x, ".indexTZ")
}

indexTZ.xts <- function(x, ...)
{
  tzone <- attr(attr(x, "index"), "tzone")
  if(is.null(tzone))
    tzone <- attr(x, ".indexTZ")
  if(is.null(tzone))
    return("")
  else
    return(tzone)
}

check.TZ <- function(x, ...)
{
  check <- getOption("xts_check_TZ")
  if( !is.null(check) && !check)
    return()
  STZ <- as.character(Sys.getenv("TZ"))
  if(any(indexClass(x) %in% c("chron","dates","times","Date")))
    return()
  if(!is.null(indexTZ(x)) && indexTZ(x) != "" &&
     !identical(STZ, as.character(indexTZ(x))))
    warning(paste("timezone of object (",indexTZ(x),
                  ") is different than current timezone (",STZ,").",sep=""),
           call.=FALSE)
}
