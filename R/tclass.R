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


#' Get or Replace the Class of an xts Object's Index
#' 
#' Generic functions to get or replace the class of an xts object's index.
#' 
#' Internally, an xts object's index is a *numeric* value corresponding to
#' seconds since the epoch in the UTC timezone. The index class is stored as
#' the `tclass` attribute on the internal index. This is used to convert
#' the internal index values to the desired class when the `index`
#' function is called.
#' 
#' The `tclass` function retrieves the class of the internal index, and
#' the `tclass<-` function sets it. The specified value for
#' `tclass<-` must be one of the following character strings:
#' `"Date"`, `"POSIXct"`, `"chron"`, `"yearmon"`,
#' `"yearqtr"`, or `"timeDate"`.
#' 
#' @param x An xts object.
#' @param value The new index class (see Details for valid values).
#' @param \dots Arguments passed to other methods.
#' 
#' @return A vector containing the class of the object's index.
#' 
#' @note Both `indexClass` and `indexClass<-` are deprecated in favor
#' of `tclass` and `tclass<-`, respectively.
#' 
#' Replacing the `tclass` can *potentially change* the values of the internal
#' index. For example, changing the 'tclass' from POSIXct to Date will
#' truncate the POSIXct value and convert the timezone to UTC (since the Date
#' class doesn't have a timezone). See the examples.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`index()`][xts::index.xts] has more information on the xts index, [`tformat()`]
#' details how the index values are formatted when printed, and [`tzone()`]
#' has more information about the index timezone settings.
#' 
#' The following help pages describe the characteristics of the valid index
#' classes: [`POSIXct()`], [`Date()`], [chron()][chron::chron],
#' [`yearmon()`][zoo::zoo], [`yearqtr()`][zoo::zoo],
#' [`timeDate()`][timeDate::timeDate]
#' 
#' @keywords ts utilities
#' @examples
#' 
#' x <- timeBasedSeq('2010-01-01/2010-01-02 12:00')
#' x <- xts(seq_along(x), x)
#' 
#' y <- timeBasedSeq('2010-01-01/2010-01-03 12:00/H')
#' y <- xts(seq_along(y), y, tzone = "America/New_York")
#' 
#' # Changing the tclass *changes* the internal index values
#' head(y)          # the index has times
#' head(.index(y))
#' tclass(y) <- "Date"
#' head(y)          # the index prints as a Date
#' head(.index(y))  # the internal index is truncated
#' 
tclass <-
function(x, ...) {
  UseMethod('tclass')
}

#' @rdname tclass
tclass.default <-
function(x, ...)
{
  attr(x, "tclass")
}

#' @rdname tclass
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

#' @rdname tclass
`tclass<-` <-
function(x,value) {
  UseMethod('tclass<-')
}

#' @rdname tclass
`tclass<-.default` <-
function(x, value)
{
  if (!is.null(value)) {
    value <- as.character(value)
  }
  attr(x, "tclass") <- value
  x
}

#' @rdname tclass
indexClass <-
function(x) {
  .Deprecated("tclass", "xts")
  tclass(x)
}

#' @rdname tclass
`indexClass<-` <-
function(x, value) {
  .Deprecated("tclass<-", "xts")
  `tclass<-`(x, value)
}

#' @rdname tclass
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
