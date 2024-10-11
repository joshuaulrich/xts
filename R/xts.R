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


# xts() index attribute precedence should be:
#   1. .index* value (e.g. .indexTZ)  # backward compatibility
#   2. t* value (e.g. tzone)          # current function to override index attribute
#   3. attribute on order.by          # overridden by either 2 above
#
# Do we always have to override the value of an existing tzone on the index
# because the default value is Sys.getenv("TZ")?
#
# .xts() index attribute precedence is similar. But we cannot override tclass
# because it's a formal argument with a specific default. Historically .xts()
# has always set the tclass to POSIXct by default, whether or not the 'index'
# argument already had a tclass attribute.


#' Create or Test For An xts Time-Series Object
#' 
#' Constructor function for creating an extensible time-series object.
#' 
#' `xts()` is used to create an xts object from raw data inputs. The xts class
#' inherits from and extends the zoo class, which means most zoo functions can
#' be used on xts objects.
#' 
#' The `xts()` constructor is the preferred way to create xts objects. It
#' performs several checks to ensure it returns a well-formed xts object. The
#' `.xts()` constructor is mainly for internal use. It is more efficient then
#' the regular `xts()` constructor because it doesn't perform as many validity
#' checks. Use it with caution.
#' 
#' Similar to zoo objects, xts objects must have an ordered index. While zoo
#' indexes cannot contain duplicate values, xts objects have optionally
#' supported duplicate index elements since version 0.5-0. The xts class has
#' one additional requirement: the index must be a time-based class. Currently
#' supported classes include: \sQuote{Date}, \sQuote{POSIXct}, \sQuote{timeDate},
#' as well as \sQuote{yearmon} and \sQuote{yearqtr} where the index values
#' remain unique.
#' 
#' The uniqueness requirement was relaxed in version 0.5-0, but is still
#' enforced by default. Setting `unique = FALSE` skips the uniqueness check and
#' only ensures that the index is ordered via the `isOrdered()` function.
#' 
#' As of version 0.10-0, xts no longer allows missing values in the index. This
#' is because many xts functions expect all index values to be finite. The most
#' important of these is `merge.xts()`, which is used ubiquitously. Missing
#' values in the index are usually the result of a date-time conversion error
#' (e.g. incorrect format, non-existent time due to daylight saving time, etc.).
#' Because of how non-finite numbers are represented, a missing timestamp will
#' always be at the end of the index (except if it is `-Inf`, which will be
#' first).
#' 
#' Another difference from \pkg{zoo} is that xts object may carry additional
#' attributes that may be desired in individual time-series handling. This
#' includes the ability to augment the objects data with meta-data otherwise
#' not cleanly attachable to a standard zoo object. These attributes may be
#' assigned and extracted via [`xtsAttributes()`] and [`xtsAttributes<-`],
#' respectively.
#' 
#' Examples of usage from finance may include the addition of data for keeping
#' track of sources, last-update times, financial instrument descriptions or
#' details, etc.
#' 
#' The idea behind \pkg{xts} is to offer the user the ability to utilize a
#' standard zoo object, while providing an mechanism to customize the object's
#' meta-data, as well as create custom methods to handle the object in a manner
#' required by the user.
#' 
#' Many xts-specific methods have been written to better handle the unique
#' aspects of xts. These include, subsetting (`[`), `merge()`, `cbind()`,
#' `rbind()`, `c()`, math and logical operations, `lag()`, `diff()`,
#' `coredata()`, `head()`, and `tail()`. There are also xts-specific methods
#' for converting to/from R's different time-series classes.
#' 
#' Subsetting via `[` methods offers the ability to specify dates by range, if
#' they are enclosed in quotes. The style borrows from python by creating
#' ranges separated by a double colon \dQuote{"::"} or \dQuote{"/"}. Each side
#' of the range may be left blank, which would then default to the start and
#' end of the data, respectively. To specify a subset of times, it is only
#' required that the time specified be in standard ISO format, with some form
#' of separation between the elements. The time must be *left-filled*, that is
#' to specify a full year one needs only to provide the year, a month requires
#' the full year and the integer of the month requested - e.g. '1999-01'. This
#' format would extend all the way down to seconds - e.g. '1999-01-01 08:35:23'.
#' Leading zeros are not necessary. See the examples for more detail.
#' 
#' Users may also extend the xts class to new classes to allow for method
#' overloading.
#' 
#' Additional benefits derive from the use of [`as.xts()`] and [`reclass()`],
#' which allow for lossless two-way conversion between common R time-series
#' classes and the xts object structure. See those functions for more detail.
#' 
#' @param x An object containing the underlying data.
#' @param order.by A corresponding vector of dates/times of a known time-based
#'   class. See Details.
#' @param index A corresponding *numeric* vector specified as seconds since
#'   the UNIX epoch (1970-01-01 00:00:00.000).
#' @param frequency Numeric value indicating the frequency of `order.by`. See
#'   details.
#' @param unique Can the index only include unique timestamps? Ignored when
#'   `check = FALSE`.
#' @param check Must the index be ordered? The index cannot contain duplicates
#'   when `check = TRUE` and `unique = TRUE`.
#' @param tclass Time class to use for the index. See [`tclass()`].
#' @param tzone Time zone of the index (ignored for indices without a time
#'   component, e.g. Date, yearmon, yearqtr). See [`tzone()`].
#' @param \dots Additional attributes to be added. See details.
#' 
#' @return An S3 object of class xts.
#' 
#' @author Jeffrey A. Ryan and Joshua M. Ulrich
#' 
#' @seealso [`as.xts()`], [`index()`][xts::index.xts], [`tclass()`], [`tformat()`], [`tzone()`],
#' [`xtsAttributes()`]
#' 
#' @references \pkg{zoo}
#' 
#' @keywords utilities
#' @examples
#' 
#' data(sample_matrix)
#' sample.xts <- as.xts(sample_matrix, descr='my new xts object')
#' 
#' class(sample.xts)
#' str(sample.xts)
#' 
#' head(sample.xts)  # attribute 'descr' hidden from view
#' attr(sample.xts,'descr')
#' 
#' sample.xts['2007']  # all of 2007
#' sample.xts['2007-03/']  # March 2007 to the end of the data set
#' sample.xts['2007-03/2007']  # March 2007 to the end of 2007
#' sample.xts['/'] # the whole data set
#' sample.xts['/2007'] # the beginning of the data through 2007
#' sample.xts['2007-01-03'] # just the 3rd of January 2007
#' 
`xts` <-
function(x=NULL,
         order.by=index(x),
         frequency=NULL,
         unique=TRUE,
         tzone=Sys.getenv("TZ"),
         ...)
{
  if(is.null(x) && missing(order.by))
    return(.xts(NULL, integer()))
 
  if(!timeBased(order.by))
    stop("order.by requires an appropriate time-based object")

  #if(NROW(x) != length(order.by))
  if(NROW(x) > 0 && NROW(x) != length(order.by))
    stop("NROW(x) must match length(order.by)")

  order.by_ <- order.by  # make local copy and don't change order.by

  if(inherits(order.by, 'Date')) { 
    # convert to GMT POSIXct if specified
    order.by_ <- .POSIXct(unclass(order.by) * 86400, tz = "UTC")
  }

  if(!isOrdered(order.by_, strictly = !unique)) {
    indx <- order(order.by_)
    if(!is.null(x)) {
      if(NCOL(x) > 1 || is.matrix(x) || is.data.frame(x)) {
        x <- x[indx,,drop=FALSE]
      } else x <- x[indx]
    }
    order.by_ <- order.by_[indx]
  }

  if(is.null(x)) {
    x <- numeric(0)
  } else if (is.list(x)) {
    # list or data.frame
    if (is.data.frame(x)) {
      x <- as.matrix(x)
    } else {
      stop("cannot convert lists to xts objects")
    }
  } else if (NROW(x) > 0) {
    x <- as.matrix(x)
  }
  # else 'x' is a zero-length vector. Do not *add* dims via as.matrix().
  # It's okay if 'x' already has dims.

  if(inherits(order.by, "dates")) {
    fmt <- "%m/%d/%y"
    if(inherits(order.by, "chron")) {
      fmt <- paste0("(", fmt, " %H:%M:%S)")
    }
    order.by_ <- strptime(as.character(order.by_), fmt)  # POSIXlt
  }
  index <- as.numeric(as.POSIXct(order.by_))

  if(any(!is.finite(index)))
    stop("'order.by' cannot contain 'NA', 'NaN', or 'Inf'")

  # process index attributes
  ctor.call <- match.call(expand.dots = TRUE)

  tformat. <- attr(order.by, "tformat")
  if(hasArg(".indexFORMAT")) {
    warning(sQuote(".indexFORMAT"), " is deprecated, use tformat instead.")
    tformat. <- eval.parent(ctor.call$.indexFORMAT)
  } else if(hasArg("tformat")) {
    tformat. <- eval.parent(ctor.call$tformat)
  }

  tclass. <- attr(order.by, "tclass")
  if(hasArg(".indexCLASS")) {
    warning(sQuote(".indexCLASS"), " is deprecated, use tclass instead.")
    tclass. <- eval.parent(ctor.call$.indexCLASS)
  } else if(hasArg("tclass")) {
    tclass. <- eval.parent(ctor.call$tclass)
  } else if(is.null(tclass.)) {
    tclass. <- class(order.by)
    if(inherits(order.by, "POSIXt")) {
      #tclass. <- tclass.[tclass. != "POSIXt"]
    }
  }

  tzone. <- tzone  # default Sys.getenv("TZ")
  if(hasArg(".indexTZ")) {
    warning(sQuote(".indexTZ"), " is deprecated, use tzone instead.")
    tzone. <- eval.parent(ctor.call$.indexTZ)
  } else if(hasArg("tzone")) {
    tzone. <- eval.parent(ctor.call$tzone)
  } else {
    # no tzone argument
    if(inherits(order.by, "timeDate")) {
      tzone. <- order.by@FinCenter
    } else if(!is.null(attr(order.by, "tzone"))) {
      tzone. <- attr(order.by, "tzone")
    }
  }
  if(isClassWithoutTZ(object = order.by)) {
      if((hasArg(".indexTZ") || hasArg("tzone")) && !isUTC(tzone.)) {
        warning(paste(sQuote('tzone'),"setting ignored for ",
                      paste(class(order.by), collapse=", "), " indexes"))
      }
      tzone. <- "UTC" # change anything in isUTC() to UTC
  }
  # xts' tzone must only contain one element (POSIXlt tzone has 3)
  tzone. <- tzone.[1L]

  x <- structure(.Data = x,
                 index = structure(index, tzone = tzone.,
                                   tclass = tclass., tformat = tformat.),
                 class=c('xts','zoo'),
                 ...)

  # remove any index attributes that came through '...'
  index.attr <- c(".indexFORMAT", "tformat",
                  ".indexCLASS", "tclass",
                  ".indexTZ", "tzone")
  for(iattr in index.attr) {
    attr(x, iattr) <- NULL
  }

  if(!is.null(attributes(x)$dimnames[[1]]))
    # this is very slow if user adds rownames, but maybe that is deserved :)
    dimnames(x) <- dimnames(x) # removes row.names
  x
}


#' @rdname xts
`.xts` <-
function(x=NULL, index, tclass=c("POSIXct","POSIXt"),
         tzone=Sys.getenv("TZ"),
         check=TRUE, unique=FALSE, ...) {
  if(check) {
    if( !isOrdered(index, increasing=TRUE, strictly=unique) )
      stop('index is not in ',ifelse(unique, 'strictly', ''),' increasing order')
  }

  index_out <- index

  if(!is.numeric(index) && timeBased(index))
    index_out <- as.numeric(as.POSIXct(index))
  if(!is.null(x) && NROW(x) != length(index))
    stop("index length must match number of observations")
  if(any(!is.finite(index_out)))
    stop("'index' cannot contain 'NA', 'NaN', or 'Inf'")

  if(!is.null(x)) {
    if(!is.matrix(x))
      x <- as.matrix(x)
  } else
  if(length(x) == 0 && !is.null(x)) {
    x <- vector(storage.mode(x))
  } else x <- numeric(0)

  # process index attributes
  ctor.call <- match.call(expand.dots = TRUE)

  tformat. <- attr(index, "tformat")
  if(hasArg(".indexFORMAT")) {
    warning(sQuote(".indexFORMAT"), " is deprecated, use tformat instead.")
    tformat. <- eval.parent(ctor.call$.indexFORMAT)
  } else if(hasArg("tformat")) {
    tformat. <- eval.parent(ctor.call$tformat)
  }

  tclass. <- tclass  # default POSIXct
  if(hasArg(".indexCLASS")) {
    warning(sQuote(".indexCLASS"), " is deprecated, use tclass instead.")
    tclass. <- eval.parent(ctor.call$.indexCLASS)
  } else if(hasArg("tclass")) {
    tclass. <- eval.parent(ctor.call$tclass)
  } else {
    # no tclass argument
    tclass. <- attr(index, "tclass")
    if(is.null(tclass.) && timeBased(index)) {
      tclass. <- class(index)
    } else {
      if(!identical(tclass., c("POSIXct", "POSIXt"))) {
        # index argument has 'tclass' attribute but it will be ignored
        # FIXME:
        # This warning causes errors in dependencies (e.g. portfolioBacktest,
        # when the warning is thrown from PerformanceAnalytics). Reinstate this
        # warning after fixing downstream packages.
        #      warning("the index tclass attribute is ", index.class,
        #              " but will be changed to (POSIXct, POSIXt)")
        tclass. <- tclass  # default POSIXct
      }
    }
  }

  tzone. <- tzone  # default Sys.getenv("TZ")
  if(hasArg(".indexTZ")) {
    warning(sQuote(".indexTZ"), " is deprecated, use tzone instead.")
    tzone. <- eval.parent(ctor.call$.indexTZ)
  } else if(hasArg("tzone")) {
    tzone. <- eval.parent(ctor.call$tzone)
  } else {
    # no tzone argument
    if(inherits(index, "timeDate")) {
      tzone. <- index@FinCenter
    } else if(!is.null(attr(index, "tzone"))) {
      tzone. <- attr(index, "tzone")
    }
  }
  if(isClassWithoutTZ(object = index)) {
      if((hasArg(".indexTZ") || hasArg("tzone")) && !isUTC(tzone.)) {
        warning(paste(sQuote('tzone'),"setting ignored for ",
                      paste(class(index), collapse=", "), " indexes"))
      }
      tzone. <- "UTC" # change anything in isUTC() to UTC
  }
  # xts' tzone must only contain one element (POSIXlt tzone has 3)
  tzone <- tzone[1L]

  xx <- .Call(C_add_xtsCoreAttributes, x, index_out, tzone., tclass.,
              c('xts','zoo'), tformat.)

  # remove any index attributes that came through '...'
  # and set any user attributes (and/or dim, dimnames, etc)
  dots.names <- eval(substitute(alist(...)))
  if(length(dots.names) > 0L) {
    dot.attrs <- list(...)
    drop.attr <- c(".indexFORMAT", "tformat", ".indexCLASS", ".indexTZ")
    dot.attrs[drop.attr] <- NULL
    attributes(xx) <- c(attributes(xx), dot.attrs)
  }

  # ensure there are no rownames (they may have come though dimnames)
  rn <- dimnames(xx)[[1]]
  if(!is.null(rn)) {
    attr(xx, '.ROWNAMES') <- rn
    dimnames(xx)[1] <- list(NULL)
  }

  xx
}

#' @rdname xts
`is.xts` <-
function(x) {
  inherits(x,'xts') &&
  is.numeric(.index(x)) &&
  !is.null(tclass(x))
}


#' Convert Objects To and From xts
#' 
#' Conversion S3 methods to coerce data objects of arbitrary classes to xts
#' and back, without losing any attributes of the original format.
#' 
#' A simple and reliable way to convert many different objects into a uniform
#' format for use within \R.
#' 
#' `as.xts()` can convert objects of the following classes into an xts object:
#' object: [timeSeries][timeSeries::timeSeries], [ts], [matrix], [data.frame],
#' and [zoo][zoo::zoo]. `xtsible()` safely checks whether an object can be converted to
#' an xts object.
#' 
#' Additional `name = value` pairs may be passed to the function to be added to
#' the new object. A special [`print.xts()`] method ensures the attributes are
#' hidden from view, but will be available via \R's standard `attr()` function,
#' as well as the [`xtsAttributes()`] function.
#' 
#' When `.RECLASS = TRUE`, the returned xts object internally preserves all
#' relevant attribute/slot data from the input `x`. This allows for temporary
#' conversion to xts in order to use zoo and xts compatible methods. See
#' [`reclass()`] for details.
#' 
#' @param x Data object to convert. See details for supported types.
#' @param dateFormat What class should the dates be converted to?
#' @param FinCenter,recordIDs,title,documentation See [timeSeries][timeSeries::timeSeries] help.
#' @param order.by,frequency See [zoo][zoo::zoo] help.
#' @param \dots Additional parameters or attributes.
#' @param .RECLASS Should the conversion be reversible via [`reclass()`]?
#' 
#' @return An S3 object of class xts.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`xts()`], [`reclass()`], [`zoo()`][zoo::zoo]
#' 
#' @keywords utilities
#' @examples
#' 
#' \dontrun{
#' # timeSeries
#' library(timeSeries)
#' x <- timeSeries(1:10, 1:10)
#'
#' str(as.xts(x))
#' str(reclass(as.xts(x)))
#' str(try.xts(x))
#' str(reclass(try.xts(x)))
#' }
#' 
`as.xts` <-
function(x,...) {
  UseMethod('as.xts')
}

#' @rdname as.xts
xtsible <- function(x)
{
  if(inherits(try(as.xts(x),silent=TRUE),'try-error')) {
    FALSE
  } else TRUE
}

`re.xts` <-
function(x,...) {
  # simply return the object
  return(x)
}

`as.xts.xts` <-
function(x,...,.RECLASS=FALSE) {
  # Cannot use 'zoo()' on objects of class 'zoo' or '.CLASS' (etc.?)
  # Is the equivalent of a 'coredata.xts' needed? - jmu
  #yy <- coredata(x)
  #attr(yy, ".CLASS") <- NULL
  # using new coredata.xts method - jar
  if(length(x) == 0 && (!is.null(index(x)) && length(index(x))==0))
    return(x)
  if(.RECLASS) {
  xx <- xts(coredata(x),
            order.by=index(x),
            .CLASS='xts',
            ...)
  } else {
  xx <- xts(coredata(x),
            order.by=index(x),
            ...)
  }
  xx
}
