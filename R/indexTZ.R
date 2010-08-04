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
  UseMethod("indexTZ")
}

`indexTZ<-.xts` <- function(x, value) {
  attr(x, ".indexTZ") <- value
  attr(attr(x,"index"),"tzone") <- value
  x
}

indexTZ.default <- function(x, ...) {
  attr(x, ".indexTZ")
}

indexTZ.xts <- function(x, ...)
{
  tzone <- attr(attr(x, "index"), "tzone")
  if(is.null(tzone))
    attr(x, ".indexTZ")
  else tzone
}

check.TZ <- function(x, ...)
{
  STZ <- as.character(Sys.getenv("TZ"))
  if(!is.null(indexTZ(x)) && indexTZ(x) != "" &&
     !identical(STZ, as.character(indexTZ(x))))
    warning(paste("timezone of object (",indexTZ(x),
                  ") is different than current timezone (",STZ,").",sep=""),
           call.=FALSE)
}
