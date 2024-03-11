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


#' Create Or Test For An xts Time-Series Object
#' 
#' Constructor function for creating an extensible time-series object.
#' 
#' \code{xts} is used to create an \code{xts} object from raw data inputs.
#' 
#' An \code{xts} object extends the S3 class \code{zoo} from the package of the
#' same name.
#' 
#' The \code{xts()} constructor is the preferred way to create xts objects. It
#' performs several checks to ensure it returns a well-formed xts object. The
#' \code{.xts()} constructor is mainly for internal use. It is more efficient
#' than the regular \code{xts()} constructor because it doesn't perform as many
#' validity checks. Use it with caution.
#' 
#' % TODO: add notes here about the differences between 'empty', 'zero-width',
#' and % 'zero-length' xts objects.
#' 
#' Similar to zoo objects, xts objects must have an ordered index.  While zoo
#' indexes cannot contain duplicate values, xts objects have optionally
#' supported duplicate index elements since version 0.5-0.  The \code{xts}
#' class has one additional requirement, the index must be a time-based class.
#' Currently supported classes include: \sQuote{Date}, \sQuote{POSIXct},
#' \sQuote{timeDate}, as well as \sQuote{yearmon} and \sQuote{yearqtr} where
#' the index values remain unique.
#' 
#' The uniqueness requirement was relaxed in version 0.5-0, but is still
#' enforced by default.  Setting \code{unique = FALSE} skips the uniqueness
#' check and only ensures that the index is ordered via the \code{isOrdered}
#' function.
#' 
#' As of version 0.10-0, xts no longer allows missing values in the index.
#' This is because many xts functions expect all index values to be finite.
#' The most important of these is \code{merge.xts}, which is used ubiquitously.
#' Missing values in the index are usually the result of a date-time conversion
#' error (e.g. incorrect format, non-existent time due to daylight saving time,
#' etc).  Because of how non-finite numbers are represented, a missing
#' timestamp will always be at the end of the index (except if it is
#' \code{-Inf}, which will be first).
#' 
#' Another difference from \pkg{zoo} is that xts object may carry additional
#' attributes that may be desired in individual time-series handling. This
#' includes the ability to augment the objects data with meta-data otherwise
#' not cleanly attachable to a standard zoo object.
#' 
#' Examples of usage from finance may include the addition of data for keeping
#' track of sources, last-update times, financial instrument descriptions or
#' details, etc.
#' 
#' The idea behind \code{xts} is to offer the user the ability to utilize a
#' standard zoo object, while providing an mechanism to customize the object's
#' meta-data, as well as create custom methods to handle the object in a manner
#' required by the user.
#' 
#' Many xts-specific methods have been written to better handle the unique
#' aspects of xts.  These include, \sQuote{"["}, merge, cbind, rbind, c, Ops,
#' lag, diff, coredata, head and tail.  Additionally there are xts specific
#' methods for converting to/from R's different time-series classes.
#' 
#' Subsetting via "[" methods offers the ability to specify dates by range, if
#' they are enclosed in quotes.  The style borrows from python by creating
#' ranges with a double colon \dQuote{"::"} or \dQuote{"/"} operator.  Each
#' side of the operator may be left blank, which would then default to the
#' beginning and end of the data, respectively.  To specify a subset of times,
#' it is only required that the time specified be in standard ISO format, with
#' some form of separation between the elements. The time must be
#' \sQuote{left-filled}, that is to specify a full year one needs only to
#' provide the year, a month would require the full year and the integer of the
#' month requested - e.g. '1999-01'. This format would extend all the way down
#' to seconds - e.g. '1999-01-01 08:35:23'. Leading zeros are not necessary.
#' See the examples for more detail.
#' 
#' Users may also extend the \code{xts} class to new classes to allow for
#' method overloading.
#' 
#' Additional benefits derive from the use of \code{\link{as.xts}} and
#' \code{\link{reclass}}, which allow for lossless two-way conversion between
#' common R time-series classes and the \code{xts} object structure. See those
#' functions for more detail.
#' 
#' @param x an object containing the time series data
#' @param order.by a corresponding vector of dates/times of a known time-based
#' class. See Details.
#' @param index a corresponding \emph{numeric} vector specified as seconds
#' since the UNIX epoch (1970-01-01 00:00:00.000)
#' @param frequency numeric indicating frequency of \code{order.by}. See
#' Details.
#' @param unique check the index for unique timestamps?
#' @param check check that the index is ordered?
#' @param tclass time class to use for the index. See \code{\link{tclass}}.
#' @param tzone time zone of the index (ignored indices without a time
#' component, e.g. Date, yearmon, yearqtr). See \code{\link{tzone}}.
#' @param \dots additional attributes to be added. See Details.
#' 
#' @return An S3 object of class \code{xts}. As it inherits and extends the zoo
#' class, all zoo methods remain valid.  Additional attributes may be assigned
#' and extracted via \code{xtsAttributes}.
#' 
#' @note Most users will benefit the most by using the \code{as.xts} and
#' \code{reclass} functions to automagically handle \emph{all} data objects as
#' one would handle a \code{zoo} object.
#' 
#' @author Jeffrey A. Ryan and Joshua M. Ulrich
#' 
#' @seealso \code{\link{as.xts}}, \code{\link{index}}, \code{\link{tclass}},
#' \code{\link{tformat}}, \code{\link{tzone}}, \code{\link{xtsAttributes}}
#' 
#' @references \pkg{zoo}:
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


#' Convert Object To And From Class xts
#' 
#' Conversion functions to coerce data objects of arbitrary classes to class
#' \code{xts} and back, without losing any attributes of the original format.
#' 
#' A simple and reliable way to convert many different objects into a uniform
#' format for use within .
#' 
#' It is possible with a call to \code{as.xts} to convert objects of class
#' \code{timeSeries}, \code{ts}, \code{irts}, \code{matrix}, \code{data.frame},
#' and \code{zoo}.
#' 
#' \code{xtsible} safely checks whether an object can be converted to an
#' \code{xts} object; returning TRUE on success and FALSE otherwise.
#' 
#' The help file \code{as.xts} lists all available xts methods and arguments
#' specific to each coercible type.
#' 
#' Additional name=value pairs may be passed to the function to be added to the
#' new object. A special print.xts method will assure that the attributes are
#' hidden from view, but will be available via 's standard \code{attr}
#' function, as well as the \code{xtsAttributes} function.
#' 
#' The returned object will preserve all relevant attribute/slot data within
#' itself, allowing for temporary conversion to use zoo and xts compatible
#' methods. A call to \code{reclass} returns the object to its original class,
#' with all original attributes intact - unless otherwise changed.
#' 
#' It should be obvious, but any attributes added via the \dots{} argument will
#' not be carried back to the original data object, as there would be no
#' available storage slot/attribute.
#' 
#' \code{Reclass} is designed for top-level use, where it is desirable to have
#' the object returned from an arbitrary function in the same class as the
#' object passed in.  Most functions within are not designed to return objects
#' matching the original object's class.  While this tool is highly
#' experimental at present, it attempts to handle conversion and reconversion
#' transparently.  The caveats are that the original object must be coercible
#' to \code{xts}, the returned object must be of the same row length as the
#' original object, and that the object to reconvert to is the first argument
#' to the function being wrapped.
#' 
#' \code{try.xts} and \code{reclass} are functions that enable external
#' developers access to the reclassing tools within \pkg{xts} to help speed
#' development of time-aware functions, as well as provide a more robust and
#' seemless end-user experience, regardless of the end-user's choice of
#' data-classes.
#' 
#' The \code{error} argument to try.xts accepts a logical value, indicating
#' where an error should be thrown, a character string allowing for custom
#' error messages to be displayed, or a function of the form \code{f(x, ...)},
#' to be called upon construction error.
#' 
#' See the accompanying vignette for more details on the above usage and the
#' package in general.
#' 
#' @param x data object to convert. See details for supported types
#' @param match.to \code{xts} object whose attributes will be passed to
#' \code{x}
#' @param error error handling option. See Details.
#' @param \dots additional parameters or attributes
#' 
#' @return An S3 object of class \code{xts}.
#' 
#' In the case of \code{Reclass} and \code{reclass}, the object returned will
#' be of the original class as identified by \code{CLASS}.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{xts}},\code{\link{as.xts}}
#' 
#' @keywords utilities
#' 
`reclass` <-
function(x, match.to, error=FALSE, ...) {
  if(!missing(match.to) && is.xts(match.to)) {
    if(NROW(x) != length(.index(match.to)))
      if(error) {
        stop('incompatible match.to attibutes')
      } else return(x)

    if(!is.xts(x)) {
      x <- .xts(coredata(x), .index(match.to),
                tclass = tclass(match.to),
                tzone = tzone(match.to),
                tformat = tformat(match.to))
    }
    attr(x, ".CLASS") <- CLASS(match.to)
    xtsAttributes(x) <- xtsAttributes(match.to)
    tclass(x) <- tclass(match.to)
    tformat(x) <- tformat(match.to)
    tzone(x) <- tzone(match.to)
  }
  oldCLASS <- CLASS(x)
  # should this be is.null(oldCLASS)?
  if(length(oldCLASS) > 0 && !inherits(oldClass,'xts')) {  
    if(!is.null(dim(x))) {
      if(!is.null(attr(x,'.ROWNAMES'))) {
        # rownames<- (i.e. dimnames<-.xts) will not set row names
        # force them directly
        attr(x, "dimnames")[[1]] <- attr(x,'.ROWNAMES')[1:NROW(x)]
      }
    }
    attr(x,'.ROWNAMES') <- NULL
    #if(is.null(attr(x,'.RECLASS')) || attr(x,'.RECLASS')) {#should it be reclassed?
    if(isTRUE(attr(x,'.RECLASS'))) {#should it be reclassed?
      #attr(x,'.RECLASS') <- NULL
      do.call(paste('re',oldCLASS,sep='.'),list(x))
    } else {
      #attr(x,'.RECLASS') <- NULL
      x
    }
  } else {
    #attr(x,'.RECLASS') <- NULL
    x
  }
}


#' Extract and Set .CLASS Attribute
#' 
#' Simple extraction and replacement function to access \code{xts} .CLASS
#' attribute.  The .CLASS attribute is used by \code{reclass} to transform an
#' \code{xts} object back to its original class.
#' 
#' It is not recommended that CLASS be called in daily use.  While it may be
#' possible to coerce objects to other classes than originally derived from,
#' there is little, if any, chance that the \code{reclass} function will
#' perform as expected.
#' 
#' It is best to use the traditional \code{as} methods.
#' 
#' @param x an xts object
#' @param value the new .CLASS value to assign
#' 
#' @return Called for its side-effect of changing the .CLASS attribute
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{as.xts}},\code{\link{reclass}}
#' 
#' @keywords utilities
`CLASS` <-
function(x) {
  cl <- attr(x,'.CLASS')

  if(!is.null(cl)) {
    attr(cl, 'class') <- 'CLASS'
    return(cl)
  }

  return(NULL)
}

`print.CLASS` <-
function(x,...) {
  cat(paste("previous class:",x),"\n")
}

#' @rdname CLASS
`CLASS<-` <-
function(x,value) {
  UseMethod("CLASS<-")
}

`CLASS<-.xts` <-
function(x,value) {
  attr(x,".CLASS") <- value
  x
}

#' @rdname xts
`is.xts` <-
function(x) {
  inherits(x,'xts') &&
  is.numeric(.index(x)) &&
  !is.null(tclass(x))
}


#' Convert Object To And From Class xts
#' 
#' Conversion S3 methods to coerce data objects of arbitrary classes to class
#' \code{xts} and back, without losing any attributes of the original format.
#' 
#' A simple and reliable way to convert many different objects into a uniform
#' format for use within .
#' 
#' It is possible with a call to \code{as.xts} to convert objects of class
#' \code{timeSeries}, \code{ts}, \code{matrix}, \code{data.frame}, and
#' \code{zoo}.
#' 
#' Additional name=value pairs may be passed to the function to be added to the
#' new object. A special print.xts method will assure that the attributes are
#' hidden from view, but will be available via 's standard \code{attr}
#' function.
#' 
#' If \code{.RECLASS=TRUE}, the returned object will preserve all relevant
#' attribute/slot data within itself, allowing for temporary conversion to use
#' zoo and xts compatible methods. A call to \code{reclass} returns the object
#' to its original class, with all original attributes intact - unless
#' otherwise changed.  This is the default behavior when \code{try.xts} is used
#' for conversion, and should not be altered by the user; i.e. don't touch it
#' unless you are aware of the consequences.
#' 
#' It should be obvious, but any attributes added via the \dots{} argument will
#' not be carried back to the original data object, as there would be no
#' available storage slot/attribute.
#' 
#' @param x data object to convert. See details for supported types
#' @param dateFormat what format should the dates be converted to
#' @param FinCenter see timeSeries help
#' @param recordIDs see timeSeries help
#' @param title see timeSeries help
#' @param documentation see timeSeries help
#' @param order.by see \link[zoo]{zoo} help
#' @param frequency see \link[zoo]{zoo} help
#' @param \dots additional parameters or attributes
#' @param .RECLASS should conversion be reversible?
#' 
#' @return An S3 object of class \code{xts}.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{xts}}, \code{\link[zoo]{zoo}}
#' 
#' @keywords utilities
#' @examples
#' 
#'   \dontrun{
#'   # timeSeries
#'   library(timeSeries)
#'   x <- timeSeries(1:10, 1:10)
#' 
#'   str( as.xts(x) )
#'   str( reclass(as.xts(x)) )
#'   str( try.xts(x) )
#'   str( reclass(try.xts(x)) )
#'   }
#' 
`as.xts` <-
function(x,...) {
  UseMethod('as.xts')
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
