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


# functions for matrix <--> xts conversions
`as.matrix.xts` <-
function(x, ...) {
  cd <- coredata(x)
  dimnames(cd) <- list( as.character(index(x)),  colnames(x) )
  cd
#  structure(coredata(x), dimnames=list(as.character(index(x)), colnames(x)))
}

`re.matrix` <-
function(x,...) {
  as.matrix(x,...)
}

`as.xts.matrix` <-
function(x,order.by,dateFormat="POSIXct",frequency=NULL,...,.RECLASS=FALSE) {
  # Should allow 'order.by' to be a vector of dates or a scaler
  # representing the column number to use.
  if(missing(order.by)) {
    # The 'index' of zoo objects is set to 'rownames' when converted with 'as.matrix',
    # but it is of class 'Date', not 'POSIXct'... - jmu
    if(is.null(rownames(x)))
      stop("order.by must be either 'rownames()' or otherwise specified")
    else
      # added '...' args to allow for tz specification
      order.by <- do.call(paste('as',dateFormat,sep='.'),list(rownames(x)))
  }
  
  if(.RECLASS) {
  xx <- xts(x,
            order.by=order.by,
            frequency=frequency,
            .CLASS='matrix',
            ...)
  } else {
  xx <- xts(x,
            order.by=order.by,
            frequency=frequency,
            ...)
  }
  xx
}
