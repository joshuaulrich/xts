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


`convertIndex` <-
function(x,value) {
  tclass(x) <- value
  x
}

tclass <-
function(x, ...) {
  UseMethod('tclass')
}

tclass.default <-
function(x, ...)
{
  attr(x, "tclass")
}

tclass.xts <-
function(x, ...)
{
  tclass <- attr(attr(x, "index"), "tclass")

  # For xts objects created pre-0.10.3
  if (is.null(tclass)) {
    # no tclass on the index
    sq_tclass <- sQuote("tclass")
    sq_both <- paste(sq_tclass, "or", sQuote(".indexCLASS"))

    warn_msg <-
      paste0("index does not have a ", sq_tclass, " attribute")

    tclass <- attr(x, "tclass")
    if (is.null(tclass)) {
      # no tclass on the xts object, look for .indexCLASS
      tclass <- attr(x, ".indexCLASS")
    }

    if (is.null(tclass)) {
      # no .indexCLASS on the xts object
      tc <- c("POSIXct", "POSIXt")
      warn_msg <- paste0(warn_msg, "\n  and xts object does not have a ",
                         sq_both, " attribute\n", "  returning ", dQuote(tc))
      warning(warn_msg)
      return(tc)
    }

    sym <- deparse(substitute(x))
    warning(warn_msg, "\n use ", sym,
            " <- xts:::.update_index_attributes(", sym, ") to update the object")
  }
  return(tclass)
}

`tclass<-` <-
function(x,value) {
  UseMethod('tclass<-')
}

`tclass<-.default` <-
function(x, value)
{
  if (!is.null(value)) {
    value <- as.character(value)
  }
  attr(x, "tclass") <- value
  x
}


indexClass <-
function(x) {
  .Deprecated("tclass", "xts")
  tclass(x)
}

`indexClass<-` <-
function(x, value) {
  .Deprecated("tclass<-", "xts")
  `tclass<-`(x, value)
}

`tclass<-.xts` <-
function(x, value) {
  if(!is.character(value) && length(value) != 1)
    stop('improperly specified value for tclass')

  # remove 'POSIXt' from value, to prevent tclass(x) <- 'POSIXt'
  value <- value[!value %in% "POSIXt"]
  if(length(value)==0L)
    stop(paste('unsupported',sQuote('tclass'),'indexing type: POSIXt'))

  if(!value[1] %in% c('dates','chron','POSIXlt','POSIXct','Date','timeDate','yearmon','yearqtr','xtime') )
       stop(paste('unsupported',sQuote('tclass'),'indexing type:',as.character(value[[1]])))

  # Add 'POSIXt' virtual class
  if(value %in% c('POSIXlt','POSIXct'))
    value <- c(value,'POSIXt')

  # all index related meta-data will be stored in the index
  # as attributes
  if(isClassWithoutTZ(value)) {
    attr(attr(x,'index'), 'tzone') <- 'UTC'
  }
  attr(attr(x,'index'), 'tclass') <- value

  x_has_tz <- !isClassWithoutTZ(x)
  if(x_has_tz && isClassWithoutTZ(value)) {
    # update index values to midnight UTC (this also changes the tzone)
    index(x) <- index(x)
  }

  # Remove class attrs (object created before 0.10-3)
  attr(x, ".indexCLASS") <- NULL
  attr(x, "tclass") <- NULL

  x
}
