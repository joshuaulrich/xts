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


#' @rdname align.time
adj.time <-
function(x, ...) {
  tr <- match.call(expand.dots=FALSE)$...
  if(length(tr) < 1) return(x)

  oClass <- class(x)
  x <- as.POSIXlt(x)
  ntime <- as.environment(unclass(x))
  lapply(tr, function(T) {
    assign(all.vars(T), with(x, eval(T)), envir=ntime)
  })
  x <- structure(list(
    sec=ntime$sec, min=ntime$min, hour=ntime$hour, 
    mday=ntime$mday, month=ntime$mon, year=ntime$year,
    wday=ntime$wday, yday=ntime$yday,isdst=ntime$isdst), tzone=attr(x,"tzone"),
    class=c("POSIXlt","POSIXt"))
  do.call(paste('as',oClass[1],sep='.'), list(x))
}

