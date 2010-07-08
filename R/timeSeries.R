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


# functions to handle timeSeries <--> xts conversions

`re.timeSeries` <-
function(x,...) {
  stopifnot("package:timeSeries" %in% search() || require("timeSeries", quietly=TRUE))

  # strip all non-'core' attributes so they're not attached to the Data slot
  x.attr <- attributes(x)
  xx <- structure(x,dimnames=x.attr$dimnames,index=x.attr$index)
  original.attr <- attributes(x)[!names(attributes(x)) %in% 
                                 c("dim","dimnames","index","class")]
  for(i in names(original.attr)) {
    attr(xx,i) <- NULL
  }

  timeSeries(coredata(xx),charvec=format(index(x)),format=x.attr$format,
             zone=x.attr$FinCenter,FinCenter=x.attr$FinCenter,
             recordIDs=x.attr$recordIDs,title=x.attr$title,
             documentation=x.attr$documentation,...)
}

`as.xts.timeSeries` <-
function(x,dateFormat="POSIXct",FinCenter,recordIDs,title,documentation,...) {

  if(missing(FinCenter))
    FinCenter <- x@FinCenter
  if(missing(recordIDs))
    recordIDs <- x@recordIDs
  if(missing(title)) 
    title <- x@title
  if(missing(documentation)) 
    documentation <- x@documentation

  indexBy <- structure(x@positions, class=c("POSIXt","POSIXct"), tzone=FinCenter)
  order.by <- do.call(paste('as',dateFormat,sep='.'),list(as.character(indexBy)))


  xts(as.matrix(x@.Data),  
      order.by=order.by,
      format=x@format,
      FinCenter=FinCenter,
      recordIDs=recordIDs,
      title=title,
      documentation=documentation,
      .CLASS='timeSeries',
      .CLASSnames=c('FinCenter','recordIDs','title','documentation','format'),
      ...)
}

as.timeSeries.xts <- function(x, ...) {
  timeSeries(data=coredata(x), charvec=as.character(index(x)), ...)
}

`xts.as.timeSeries` <- function(x,...) {}
