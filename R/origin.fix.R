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


# fixes for R new/broken as.Date, as.POSIXlt and as.POSIXct
# hopefully to be removed when remedied in R
# taken directly from 'base', with origin set to '1970-01-01' (1970-01-01)

`as.Date.numeric` <- function(x, origin='1970-01-01', ...) {
  as.Date(origin,...) + x
}

`as.POSIXct.numeric` <- function(x, tz="", origin='1970-01-01', ...) {
  structure(x, class=c("POSIXt", "POSIXct"))
}

`as.POSIXlt.numeric` <- function(x, tz="", origin='1970-01-01', ...) {
  as.POSIXlt(as.POSIXct(origin,tz="UTC",...) + x, tz=tz)
}

as.POSIXct.Date <- function(x, ...)
{
  as.POSIXct(as.character(x))
}

as.Date.POSIXct <- function(x, ...)
{
  as.Date(strftime(x))
#  z <- floor(unclass((x - unclass(as.POSIXct('1970-01-01'))))/86400)
#  attr(z, 'tzone') <- NULL
#  structure(z, class="Date")
}

as.POSIXlt.Date <- function(x, ...)
{
  as.POSIXlt(xts:::as.POSIXct.Date(x))
}

as.POSIXct.yearmon <- function(x, ...)
{
  structure(as.POSIXct("1970-01-01") + unclass(as.Date(x))*86400,
            class=c("POSIXt","POSIXct"))
}

as.POSIXlt.yearmon <- function(x, ...)
{
  as.POSIXlt(xts:::as.POSIXct.yearmon(x))
}

as.POSIXct.dates <- function(x, ...)
{
  # need to implement our own method to correctly handle TZ
  #as.POSIXct(as.character(as.POSIXlt(x,tz="GMT")))
  structure(as.POSIXct(as.POSIXlt(x, tz="GMT"), tz="GMT"),class=c("POSIXt","POSIXct"))
}
as.chron.POSIXct <- function(x, ...)
{
  structure(as.chron(as.POSIXlt(as.character(x))))
}
