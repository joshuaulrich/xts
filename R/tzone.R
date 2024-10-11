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

#' @rdname tzone
indexTZ <- function(x, ...)
{
  .Deprecated("tzone", "xts")
  tzone(x, ...)
}


#' Get or Replace the Timezone of an xts Object's Index
#' 
#' Generic functions to get or replace the timezone of an xts object's index.
#' 
#' Internally, an xts object's index is a *numeric* value corresponding to
#' seconds since the epoch in the UTC timezone. When an xts object is created,
#' all time index values are converted internally to [`POSIXct()`]
#' (which is also in seconds since the UNIX epoch), using the underlying OS
#' conventions and the \env{TZ} environment variable. The `xts()` function
#' manages timezone information as transparently as possible.
#' 
#' The `tzone<-` function *does not* change the internal index values
#' (i.e. the index will remain the same time in the UTC timezone).
#' 
#' @param x An xts object.
#' @param value A valid timezone value (see [`OlsonNames()`]).
#' @param \dots Arguments passed to other methods.
#' 
#' @return A one element named vector containing the timezone of the object's
#' index.
#' 
#' @note Both `indexTZ()` and `indexTZ<-` are deprecated in favor of
#' `tzone()` and `tzone<-`, respectively.
#' 
#' Problems may arise when an object that had been created under one timezone
#' are used in a session using another timezone. This isn't usually a issue,
#' but when it is a warning is given upon printing or subsetting. This warning
#' may be suppressed by setting `options(xts_check_TZ = FALSE)`.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`index()`][xts::index.xts] has more information on the xts index, [`tformat()`]
#' describes how the index values are formatted when printed, and [`tclass()`]
#' provides details how \pkg{xts} handles the class of the index.
#' 
#' @keywords ts utilities
#' @examples
#' 
#' # Date indexes always have a "UTC" timezone
#' x <- xts(1, Sys.Date())
#' tzone(x)
#' str(x)
#' print(x)
#' 
#' # The default 'tzone' is blank -- your machine's local timezone,
#' # determined by the 'TZ' environment variable.
#' x <- xts(1, Sys.time())
#' tzone(x)
#' str(x)
#' 
#' # now set 'tzone' to different values
#' tzone(x) <- "UTC"
#' str(x)
#' 
#' tzone(x) <- "America/Chicago"
#' str(x)
#' 
#' y <- timeBasedSeq('2010-01-01/2010-01-03 12:00/H')
#' y <- xts(seq_along(y), y, tzone = "America/New_York")
#' 
#' # Changing the tzone does not change the internal index values, but it
#' # does change how the index is printed!
#' head(y)
#' head(.index(y))
#' tzone(y) <- "Europe/London"
#' head(y)          # the index prints with hours, but
#' head(.index(y))  # the internal index is not changed!
#' 
tzone <- function(x, ...) {
  UseMethod("tzone")
}

#' @rdname tzone
`indexTZ<-` <- function(x, value) {
  .Deprecated("tzone<-", "xts")
  `tzone<-`(x, value)
}

#' @rdname tzone
`tzone<-` <- function(x, value) {
  UseMethod("tzone<-")
}

`tzone<-.xts` <-
function(x, value)
{
  if (is.null(value)) {
    value <- ""
  }

  tzone <- as.character(value)
  attr(attr(x, "index"), "tzone") <- tzone
  # Remove tz attrs (object created before 0.10-3)
  attr(x, ".indexTZ") <- NULL
  attr(x, "tzone") <- NULL
  x
}

tzone.default <-
function(x, ...)
{
  attr(x, "tzone")
}

`tzone<-.default` <-
function(x, value)
{
  if (!is.null(value)) {
    value <- as.character(value)
  }
  attr(x, "tzone") <- value
  x
}

tzone.xts <-
function(x, ...)
{
  tzone <- attr(attr(x, "index"), "tzone")

  # For xts objects created pre-0.10.3
  if (is.null(tzone)) {
    # no tzone on the index
    sq_tzone <- sQuote("tzone")
    sq_both <- paste(sq_tzone, "or", sQuote(".indexTZ"))

    warn_msg <-
      paste0("index does not have a ", sq_tzone, " attribute")

    tzone <- attr(x, "tzone")
    if (is.null(tzone)) {
      # no tzone on the xts object, look for .indexTZ
      tzone <- attr(x, ".indexTZ")
    }

    if (is.null(tzone)) {
      # no .indexTZ on the xts object
      tzone <- ""
      warn_msg <- paste0(warn_msg, "\n  and xts object does not have a ",
                         sq_both, " attribute\n", "  returning ", dQuote(tzone))
      warning(warn_msg)
      return(tzone)
    }

    sym <- deparse(substitute(x))
    warning(warn_msg, "\n use ", sym,
            " <- xts:::.update_index_attributes(", sym, ") to update the object")
  }
  return(tzone)
}

isClassWithoutTZ <-
function(tclass, object = NULL)
{
  .classesWithoutTZ <- c("chron","dates","times","Date","yearmon","yearqtr")
  has_no_tz <- FALSE

  if (is.null(object)) {
    has_no_tz <- any(tclass %in% .classesWithoutTZ)
  } else {
    has_no_tz <- inherits(object, .classesWithoutTZ)
  }
  return(has_no_tz)
}

isUTC <- function(tz = NULL) {
  if (is.null(tz)) {
    tz <- Sys.timezone()
  }
  switch(tz,
         "UTC" = ,
         "GMT" = ,
         "Etc/UTC" = ,
         "Etc/GMT" = ,
         "GMT-0" = ,
         "GMT+0" = ,
         "GMT0" = TRUE,
         FALSE)
}

check.TZ <- function(x, ...)
{
  check <- getOption("xts_check_TZ")

  if (!is.null(check) && !check) {
    return()
  }

  x_tz <- tzone(x)
  x_tclass <- tclass(x)

  if (isClassWithoutTZ(x_tclass)) {
    # warn if tzone is not UTC or GMT (GMT is not technically correct, since
    # it *is* a timezone, but it should work for all practical purposes)
    if (!isUTC(x_tz)) {
      warning(paste0("object index class (", paste(x_tclass, collapse = ", "),
        ") does not support timezones.\nExpected 'UTC' timezone, but tzone is ",
        sQuote(x_tz)), call. = FALSE)
    } else {
      return()
    }
  }

  x_tz_str <- as.character(x_tz)
  sys_tz <- Sys.getenv("TZ")

  if (!is.null(x_tz) && x_tz_str != "" && !identical(sys_tz, x_tz_str)) {
    msg <- paste0("object timezone ('", x_tz, "') is different ",
                  "from system timezone ('", sys_tz, "')")

    if (is.null(check)) {
      # xts_check_TZ is NULL by default
      # set to TRUE after messaging user how to disable the warning
      msg <- paste0(msg, "\n  NOTE: set 'options(xts_check_TZ = FALSE)' ",
                    "to disable this warning\n",
                    "    This note is displayed once per session")
      options(xts_check_TZ = TRUE)
    }

    warning(msg, call. = FALSE)
  }
}
