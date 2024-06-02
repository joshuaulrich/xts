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


#' @rdname ndays
`nseconds` <-
function(x) {
  length(endpoints(x,on='seconds'))-1
}

#' @rdname ndays
`nminutes` <-
function(x) {
  length(endpoints(x,on='minutes'))-1
}

#' @rdname ndays
`nhours` <-
function(x) {
  length(endpoints(x,on='hours'))-1
}


#' Number of Periods in Data
#' 
#' Calculate the number of specified periods in a given time series like data
#' object.
#' 
#' Essentially a wrapper to `endpoints()` with the appropriate period
#' specified. The result is the number of endpoints found.
#' 
#' As a compromise between simplicity and accuracy, the results will always
#' round up to the nearest complete period. Subtract 1 from the result to
#' get the completed periods.
#' 
#' For finer grain detail one should call the higher frequency functions.
#' 
#' An alternative summary can be found with `periodicity(x)` and
#' `unclass(periodicity(x))`.
#' 
#' @param x A time-based object.
#' 
#' @return The number of respective periods in `x`.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`endpoints()`]
#' 
#' @keywords utilities
#' @examples
#' 
#' \dontrun{
#' getSymbols("QQQQ")
#' 
#' ndays(QQQQ)
#' nweeks(QQQQ)
#' }
#' 
`ndays` <-
function(x) {
  length(endpoints(x,on='days'))-1
}

#' @rdname ndays
`nweeks` <-
function(x) {
  length(endpoints(x,on='weeks'))-1
}

#' @rdname ndays
`nmonths` <-
function(x) {
  length(endpoints(x,on='months'))-1
}

#' @rdname ndays
`nquarters` <-
function(x) {
  length(endpoints(x,on='quarters'))-1
}

#' @rdname ndays
`nyears` <-
function(x) {
  length(endpoints(x,on='years'))-1
}
