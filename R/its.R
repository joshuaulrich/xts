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


# functions for its <--> xts conversions

`re.its` <-
function(x, ...) {
  if(!require('its', quietly=TRUE))
    its <- function(...) message("package 'fts' is required")

  xx <- coredata(x)
  dates <- index(x)
  its(xx,dates=dates,...)
}

`as.xts.its` <-
function(x,..., .RECLASS=FALSE) {
  if(.RECLASS) {
  xx <- xts(x@.Data,
            order.by=x@dates,
            .CLASS='its',
            ...)
  } else {
  xx <- xts(x@.Data,
            order.by=x@dates,
            ...)
  }
  xx
}

`xts.as.its` <-
function(x,...) {
  if(!is.xts(x)) stop('not an "xts" object')
  if(!inherits(class(index(x)),'POSIXct')) indexClass(x) <- "POSIXct"
  re.its(x,...)
}

