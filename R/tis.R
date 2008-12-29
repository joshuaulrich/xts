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


# functions to support package:tis

as.POSIXct.tis <- function(x, offset=1, tz="", ...)
  as.numeric(POSIXct(x, offset, tz, ...))

as.xts.tis <- function(x, offset=1, ...)
{
  .xts(unclass(x), as.numeric(as.POSIXct.tis(x,offset)), ...)
}

re.tis <- function() {}

