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

indexTZ <- function(x, ...)
{
  .Deprecated("tzone", "xts")
  tzone(x, ...)
}

tzone <- function(x, ...) {
  UseMethod("tzone")
}

`indexTZ<-` <- function(x, value) {
  .Deprecated("tzone<-", "xts")
  `tzone<-`(x, value)
}

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
    warning("index does not have a ", sQuote("tzone"), " attribute")

    tzone <- attr(x, "tzone")
    if (is.null(tzone)) {
      tzone <- attr(x, ".indexTZ")
    }
    if (is.null(tzone)) {
      warning("object does not have a ", sQuote("tzone"), " or ",
              sQuote(".indexTZ"), " attribute")
      tzone <- ""
    }
  }
  return(tzone)
}

.classesWithoutTZ <- c("chron","dates","times","Date","yearmon","yearqtr")

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

  if (any(tclass(x) %in% .classesWithoutTZ)) {
    # warn if tzone is not UTC or GMT (GMT is not technically correct, since
    # it *is* a timezone, but it should work for all practical purposes)
    if (!isUTC(x_tz)) {
      warning(paste0("object index class (", paste(tclass(x), collapse = ", "),
        ") does not support timezones.\nExpected 'UTC' timezone, but tzone is ",
        sQuote(x_tz)), call. = FALSE)
    } else {
      return()
    }
  }

  x_tz_str <- as.character(x_tz)
  sys_tz <- Sys.getenv("TZ")

  if (!is.null(tzone(x)) && x_tz_str != "" && !identical(sys_tz, x_tz_str)) {
    msg <- paste0("object timezone (", x_tz, ") is different ",
                  "from system timezone (", sys_tz, ")")

    if (is.null(check)) {
      # xts_check_TZ is NULL by default
      # set to TRUE after messaging user how to disable the warning
      msg <- paste0(msg, "\n  NOTE: set 'options(xts_check_TZ = FALSE)'",
                    "to disable this warning\n",
                    "    This note is displayed once per session")
      options(xts_check_TZ = TRUE)
    }

    warning(msg, call. = FALSE)
  }
}
