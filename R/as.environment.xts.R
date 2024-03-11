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

#' Coerce an \sQuote{xts} Object to an Environment by Column
#' 
#' Method to automatically convert an \sQuote{xts} object to an environment
#' containing vectors representing each column of the original xts object. Each
#' objects will be named according to the column name it is exracted by.
#' 
#' An experimental tool to convert \code{xts} objects into environments for
#' simplifying use withing the standard R formula/data paradigm.
#' 
#' @param x an \code{xts} object
#' 
#' @return An \code{environment} containing \code{ncol(x)} vectors extracted by
#' column from \code{x}.  Note that environments do not preserve (or have
#' knowledge) of column position, a.k.a order.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @keywords manip
#' @examples
#' 
#' x <- xts(1:10, Sys.Date()+1:10)
#' colnames(x) <- "X"
#' y <- xts(1:10, Sys.Date()+1:10)
#' colnames(x) <- "Y"
#' xy <- cbind(x,y)
#' colnames(xy)
#' e <- as.environment(xy)    # currently using xts-style positive k 
#' ls(xy)
#' ls.str(xy)
#' 
as.environment.xts <- function(x) {
  e <- new.env()
  lapply(1:NCOL(x), function(.) assign(colnames(x)[.], x[,.],envir=e))
  e
}

