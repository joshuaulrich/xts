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


# functions to handle data.frame <--> xts conversions

`re.data.frame` <-
function(x,...) {
  data.frame(x,...)
}

#' @rdname as.xts
`as.xts.data.frame` <-
function(x,
         order.by,
         dateFormat = "POSIXct",
         frequency = NULL,
         ...,
         .RECLASS = FALSE)
{
  # Should allow 'order.by' to be a vector of dates or a scaler
  # representing the column number to use.
  if(missing(order.by)) {
    order_by_ <- try({
      coerce.rownames <- paste("as", dateFormat, sep = ".")
      do.call(coerce.rownames, list(rownames(x)))
    }, silent = TRUE)
    if(inherits(order_by_, "try-error")) {
      # parsing row names failed, so look for a time-based column
      time.based.col <- vapply(x, is.timeBased, logical(1))
      if(any(time.based.col)) {
        # use the first time-based column
        which.col <- which.max(time.based.col)
        order_by_ <- x[[which.col]]
        x <- x[, -which.col, drop = FALSE]
      } else {
        stop("could not convert row names to a date-time and could not find a time-based column")
      }
    }
  } else {
    order_by_ <- order.by
  }

  if(.RECLASS) {
  xx <- xts(x,
            order.by=order_by_,
            frequency=frequency,
            .CLASS='data.frame',
            ...)
  } else {
  xx <- xts(x,
            order.by=order_by_,
            frequency=frequency,
            ...)
  }
  xx
}

`as.data.frame.xts` <-
function(x,row.names=NULL,optional=FALSE,...) {
  if(missing(row.names))
    row.names <- as.character(index(x))
  as.data.frame(coredata(x),row.names,optional,...)
}

