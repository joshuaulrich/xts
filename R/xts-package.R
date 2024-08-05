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


#' Sample Data Matrix For xts Example and Unit Testing
#' 
#' Simulated 180 observations on 4 variables.
#' 
#' @usage data(sample_matrix)
#' 
#' @format \preformatted{The format is:
#'   num [1:180, 1:4] 50.0 50.2 50.4 50.4 50.2 ...
#'   - attr(*, "dimnames")=List of 2
#'     ..$ : chr [1:180] "2007-01-02" "2007-01-03" "2007-01-04" "2007-01-05" ...
#'     ..$ : chr [1:4] "Open" "High" "Low" "Close" }
#' 
#' @rdname sample.data
#' @keywords datasets
#' @examples
#' 
#' data(sample_matrix)
#' 
"sample_matrix"


#' Internal Documentation
#' 
#' This help file is to help in development of xts, as well as provide some
#' clarity and insight into its purpose and implementation.
#' 
#' Last modified: 2008-08-06 by Jeffrey A. Ryan Version: 0.5-0 and above
#' 
#' The \pkg{xts} package xts designed as a drop-in replacement for the very
#' popular \pkg{zoo} package.  Most all functionality of zoo has been extended
#' or carries into the xts package.
#' 
#' Notable changes in direction include the use of time-based indexing, at
#' first explicitely, now implicitely.
#' 
#' An xts object consists of data in the form of a matrix, an index -
#' ordered and increasing, either numeric or integer, and additional attributes
#' for use internally, or for end-user purposes.
#' 
#' The current implementation enforces two major rules on the object.  One is
#' that the index must be coercible to numeric, by way of `as.POSIXct`.
#' There are defined types that meet this criteria. See `timeBased` for
#' details.
#' 
#' The second requirement is that the object cannot have rownames. The
#' motivation from this comes in part from the work Matthew Doyle has done in
#' his data.table class, in the package of the same name.  Rownames in must be
#' character vectors, and as such are inefficient in both storage and
#' conversion.  By eliminating the rownames, and providing a numeric index of
#' internal type `REAL` or `INTEGER`, it is possible to maintain a
#' connection to standard date and time classes via the POSIXct functions,
#' while at at the same time maximizing efficiencies in data handling.
#' 
#' User level functions `index`, as well as conversion to other classes
#' proceeds as if there were rownames.  The code for `index` automatically
#' converts time to numeric in both extraction and replacement functionality.
#' This provides a level of abstraction to facilitate internal, and external
#' package use and inter-operability.
#' 
#' There is also new work on providing a C-level API to some of the xts
#' functionality to facilitate external package developers to utilize the fast
#' utility routines such as subsetting and merges, without having to call only
#' from .  Obviously this places far more burden on the developer to not only
#' understand the internal xts implementation, but also to understand all of
#' what is documented for R-internals (and much that isn't). At present the
#' functions and macros available can be found in the \sQuote{xts.h} file in
#' the src directory.
#' 
#' There is no current documentation for this API.  The adventure starts here.
#' Future documentation is planned, not implemented.
#' 
#' @name xts-internals
#' @author Jeffrey A. Ryan
#' @keywords utilities
NULL


#' xts: extensible time-series
#' 
#' Extensible time series class and methods, extending and behaving like zoo.
#' 
#' Easily convert one of \R's many time-series (and non-time-series) classes to a
#' true time-based object which inherits all of zoo's methods, while allowing
#' for new time-based tools where appropriate.
#' 
#' Additionally, one may use \pkg{xts} to create new objects which can contain
#' arbitrary attributes named during creation as name=value pairs.
#' 
#' @name xts-package
#' @author Jeffrey A. Ryan and Joshua M. Ulrich
#' 
#' Maintainer: Joshua M. Ulrich <josh.m.ulrich@gmail.com>
#' @seealso [`xts()`], [`as.xts()`], [`reclass()`], [`zoo()`][zoo::zoo]
#' @keywords package
"_PACKAGE"


#' xts C API Documentation
#' 
#' This help file is to help in development of xts, as well as provide some
#' clarity and insight into its purpose and implementation.
#' 
#' By Jeffrey A. Ryan, Dirk Eddelbuettel, and Joshua M. Ulrich Last modified:
#' 2018-05-02 Version: 0.10-3 and above
#' 
#' At present the \pkg{xts} API has publicly available interfaces to the
#' following functions (as defined in `xtsAPI.h`):
#' 
#' \preformatted{Callable from other R packages:
#'   SEXP xtsIsOrdered(SEXP x, SEXP increasing, SEXP strictly)
#'   SEXP xtsNaCheck(SEXP x, SEXP check)
#'   SEXP xtsTry(SEXP x)
#'   SEXP xtsRbind(SEXP x, SEXP y, SEXP dup)
#'   SEXP xtsCoredata(SEXP x)
#'   SEXP xtsLag(SEXP x, SEXP k, SEXP pad)
#' 
#' Internal use functions:
#'   SEXP isXts(SEXP x)
#'   void copy_xtsAttributes(SEXP x, SEXP y)
#'   void copy_xtsCoreAttributes(SEXP x, SEXP y)
#' 
#' Internal use macros:
#'   xts_ATTRIB(x)
#'   xts_COREATTRIB(x)
#'   GET_xtsIndex(x)
#'   SET_xtsIndex(x,value)
#'   GET_xtsIndexFormat(x)
#'   SET_xtsIndexFormat(x,value)
#'   GET_xtsCLASS(x)
#'   SET_xtsCLASS(x,value)
#' 
#' Internal use SYMBOLS:
#'   xts_IndexSymbol
#'   xts_ClassSymbol
#'   xts_IndexFormatSymbol
#' 
#' Callable from R:
#'   SEXP mergeXts(SEXP args)
#'   SEXP rbindXts(SEXP args)
#'   SEXP tryXts(SEXP x)
#' }
#' 
#' @name xtsAPI
#' @author Jeffrey A. Ryan
#' @keywords utilities
#' @examples
#' 
#' \dontrun{
#' # some example code to look at
#' 
#' file.show(system.file('api_example/README', package="xts"))
#' file.show(system.file('api_example/src/checkOrder.c', package="xts"))
#' }
#' 
NULL
