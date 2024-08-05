#
#   xts: eXtensible time-series
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#   window.xts contributed by Corwin Joy
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

.subsetTimeOfDay <- function(x, fromTimeString, toTimeString) {
  validateTimestring <- function(time) {
    h    <- "(?:[01]?\\d|2[0-3])"
    hm   <- paste0(h, "(?::?[0-5]\\d)")
    hms  <- paste0(hm, "(?::?[0-5]\\d)")
    hmsS <- paste0(hms, "(?:\\.\\d{1,9})?")
    pattern <- paste(h, hm, hms, hmsS, sep = ")$|^(")
    pattern <- paste0("^(", pattern, "$)")

    if (!grepl(pattern, time)) {
      # FIXME: this isn't necessarily true...
      # colons aren't required, and neither are all of the components
      stop("Supply time-of-day subsetting in the format of T%H:%M:%OS/T%H:%M:%OS",
           call. = FALSE)
    }
  }

  validateTimestring(fromTimeString)
  validateTimestring(toTimeString)

  getTimeComponents <- function(time) {
    # split on decimal point
    time. <- strsplit(time, ".", fixed = TRUE)[[1]]
    hms <- time.[1L]

    # ensure hms string has even nchar
    nocolon <- gsub(":", "", hms, fixed = TRUE)
    if (nchar(nocolon) %% 2 > 0) {
      # odd nchar means leading zero is omitted from hours
      # all other components require zero padding
      hms <- paste0("0", hms)
    }
    # add colons
    hms <- gsub("(.{2}):?", ":\\1", hms, perl = TRUE)
    # remove first character (a colon)
    hms <- substr(hms, 2, nchar(hms))

    # extract components
    comp <- strsplit(hms, ":", fixed = TRUE)[[1]]
    complist <-
      list(hour = comp[1L],
           min = comp[2L],
           sec = comp[3L],
           subsec = time.[2L])
    # remove all missing components
    complist <- complist[!vapply(complist, is.na, logical(1))]
    # convert to numeric
    complist <- lapply(complist, as.numeric)

    # add timezone and return
    c(tz = "UTC", complist)
  }

  # first second in period (no subseconds)
  from <- do.call(firstof, getTimeComponents(fromTimeString)[-5L])
  secBegin <- as.numeric(from) %% 86400L

  # last second in period
  to <- do.call(lastof, getTimeComponents(toTimeString))
  secEnd <- as.numeric(to) %% 86400L

  # do subsetting
  tz <- tzone(x)
  secOfDay <- as.POSIXlt(index(x), tz = tz)
  secOfDay <- secOfDay$hour * 60 * 60 + secOfDay$min * 60 + secOfDay$sec

  if (secBegin <= secEnd) {
    i <- secOfDay >= secBegin & secOfDay <= secEnd
  } else {
    i <- secOfDay >= secBegin | secOfDay <= secEnd
  }
  which(i)
}

.subset_xts <- function(x, i, j, ...) {
  if(missing(i)) {
    i <- 1:NROW(x)
  }
  if(missing(j)) {
    j <- 1:NCOL(x)
  }
  .Call(C__do_subset_xts, x, i, j, FALSE)
}


#' Extract Subsets of xts Objects
#' 
#' Details on efficient subsetting of xts objects for maximum performance
#' and compatibility.
#' 
#' One of the primary motivations and key points of differentiation of xts is
#' the ability to subset rows by specifying ISO-8601 compatible range strings.
#' This allows for natural range-based time queries without requiring prior
#' knowledge of the underlying class used for the time index.
#' 
#' When `i` is a character string, it is processed as an ISO-8601 formatted
#' datetime or time range using [`.parseISO8601()`]. A single datetime is
#' parsed from left to to right, according to the following specification:
#' 
#' CCYYMMDD HH:MM:SS.ss+
#' 
#' A time range can be specified by two datetimes separated by a forward slash
#' or double-colon. For example:
#' 
#' CCYYMMDD HH:MM:SS.ss+/CCYYMMDD HH:MM:SS.ss
#' 
#' The ISO8601 time range subsetting uses a custom binary search algorithm to
#' efficiently find the beginning and end of the time range. `i` can also be a
#' vector of ISO8601 time ranges, which enables subsetting by multiple
#' non-contiguous time ranges in one subset call.
#' 
#' The above parsing, both for single datetimes and time ranges, will be done
#' on each element when `i` is a character *vector*. This is very inefficient,
#' especially for long vectors. In this case, it's recommened to use `I(i)` so
#' the xts subset function can process the vector more efficiently. Another
#' alternative is to convert `i` to POSIXct before passing it to the subset
#' function. See the examples for an illustration of using `I(i)`.
#' 
#' The xts index is stored as POSIXct internally, regardless of the value of
#' its `tclass` attribute. So the fastest time-based subsetting is always when
#' `i` is a POSIXct vector.
#' 
#' @param x An xts object.
#' @param i The rows to extract. Can be a numeric vector, time-based vector, or
#'   an ISO-8601 style range string (see details).
#' @param j The columns to extract, either a numeric vector of column locations
#'   or a character vector of column names.
#' @param drop Should dimension be dropped, if possible? See notes section.
#' @param which.i Logical value that determines whether a subset xts object is
#'   returned (the default), or the locations of the matching rows (when
#'   `which.i = TRUE`).
#' @param \dots Additional arguments (currently unused).
#' 
#' @return An xts object containing the subset of `x`. When `which.i = TRUE`,
#' the corresponding integer locations of the matching rows is returned.
#' 
#' @note By design, xts objects always have two dimensions. They cannot be
#' vectors like zoo objects. Therefore `drop = FALSE` by default in order to
#' preserve the xts object's dimensions. This is different from both matrix and
#' zoo, which use `drop = TRUE` by default. Explicitly setting `drop = TRUE`
#' may be needed when performing certain matrix operations.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`xts()`], [`.parseISO8601()`], [`.index()`]
#' 
#' @references ISO 8601: Date elements and interchange formats - Information
#' interchange - Representation of dates and time <https://www.iso.org>
#' 
#' @rdname subset.xts
#' 
#' @aliases [.xts subset.xts .subset.xts .subset_xts
#' @keywords utilities
#' @examples
#' 
#' x <- xts(1:3, Sys.Date()+1:3)
#' xx <- cbind(x,x)
#' 
#' # drop = FALSE for xts, differs from zoo and matrix
#' z <- as.zoo(xx)
#' z/z[,1]
#' 
#' m <- as.matrix(xx)
#' m/m[,1]
#' 
#' # this will fail with non-conformable arrays (both retain dim)
#' tryCatch(
#'   xx/x[,1], 
#'   error = function(e) print("need to set drop = TRUE")
#' )
#' 
#' # correct way
#' xx/xx[,1,drop = TRUE]
#' 
#' # or less efficiently
#' xx/drop(xx[,1])
#' # likewise
#' xx/coredata(xx)[,1]
#' 
#' 
#' x <- xts(1:1000, as.Date("2000-01-01")+1:1000)
#' y <- xts(1:1000, as.POSIXct(format(as.Date("2000-01-01")+1:1000)))
#' 
#' x.subset <- index(x)[1:20]
#' x[x.subset] # by original index type
#' system.time(x[x.subset]) 
#' x[as.character(x.subset)] # by character string. Beware!
#' system.time(x[as.character(x.subset)]) # slow!
#' system.time(x[I(as.character(x.subset))]) # wrapped with I(), faster!
#' 
#' x['200001'] # January 2000
#' x['1999/2000'] # All of 2000 (note there is no need to use the exact start)
#' x['1999/200001'] # January 2000 
#' 
#' x['2000/200005'] # 2000-01 to 2000-05
#' x['2000/2000-04-01'] # through April 01, 2000
#' y['2000/2000-04-01'] # through April 01, 2000 (using POSIXct series)
#' 
#' 
#' ### Time of day subsetting 
#' 
#' i <- 0:60000
#' focal_date <- as.numeric(as.POSIXct("2018-02-01", tz = "UTC"))
#' x <- .xts(i, c(focal_date + i * 15), tz = "UTC", dimnames = list(NULL, "value"))
#' 
#' # Select all observations between 9am and 15:59:59.99999:
#' w1 <- x["T09/T15"] # or x["T9/T15"]
#' head(w1)
#' 
#' # timestring is of the form THH:MM:SS.ss/THH:MM:SS.ss
#' 
#' # Select all observations between 13:00:00 and 13:59:59.9999 in two ways:
#' y1 <- x["T13/T13"]
#' head(y1)
#' 
#' x[.indexhour(x) == 13]
#' 
#' # Select all observations between 9:30am and 30 seconds, and 4.10pm:
#' x["T09:30:30/T16:10"]
#' 
#' # It is possible to subset time of day overnight.
#' # e.g. This is useful for subsetting FX time series which trade 24 hours on week days
#' 
#' # Select all observations between 23:50 and 00:15 the following day, in the xts time zone
#' z <- x["T23:50/T00:14"]
#' z["2018-02-10 12:00/"] # check the last day
#' 
#' 
#' # Select all observations between 7pm and 8.30am the following day:
#' z2 <- x["T19:00/T08:29:59"]
#' head(z2); tail(z2)
#' 
`[.xts` <-
function(x, i, j, drop = FALSE, which.i=FALSE,...) 
{
    USE_EXTRACT <- FALSE # initialize to FALSE

    dimx <- dim(x)
    if(is.null(dimx)) {
      nr <- length(x)
      if(nr==0 && !which.i) {
        idx <- index(x)
        if(length(idx) == 0) {
          # this is an empty xts object (zero-length index and no columns)
          # return it unchanged to match [.zoo
          return(x)
        } else {
          return(xts(rep(NA, length(idx)), idx)[i])
        }
      }
      nr <- length(.index(x))
      nc <- 1L
    } else {
      nr <- dimx[1L]
      nc <- dimx[2L]
    }
    
    if(!missing(i)) {
    # test for negative subscripting in i
    if (is.numeric(i)) {

      # warn and convert if 'i' is not integer-like
      i_int <- as.integer(i)
      i_eps <- abs(i) - abs(i_int)
      if (isTRUE(any(i_eps > sqrt(.Machine$double.eps)))) {
        warning("converting 'i' to integer because it appears to contain fractions")
        i <- i_int
      }
      #if(any(i < 0)) {
      if(.Call(C_any_negative, i)) {
        if(!all(i <= 0))
          stop('only zeros may be mixed with negative subscripts')
        i <- (1:nr)[i]
      }
      # check boundary; length check avoids Warning from max(), and
      # any_negative ensures no NA (as of r608)
      #if(max(i) > nr)
      if(length(i) > 0 && max(i) > nr)
        stop('subscript out of bounds')
      #i <- i[-which(i == 0)]
    } else
    if (timeBased(i) || (inherits(i, "AsIs") && is.character(i)) ) {
      # Fast binary search on set of dates
      i <- window_idx(x, index. = i)
    } else 
    if(is.logical(i)) {
      i <- which(i) #(1:NROW(x))[rep(i,length.out=NROW(x))]
    } else
    if (is.character(i)) {
      time.of.day.pattern <- "(^/T)|(^T.*?/T)|(^T.*/$)"
      if (length(i) == 1 && !identical(integer(), grep(time.of.day.pattern, i[1]))) {
        # time of day subsetting
        ii <- gsub("T", "", i, fixed = TRUE)
        ii <- strsplit(ii, "/", fixed = TRUE)[[1L]]

        if (length(ii) == 1) {
          # i is right open ended (T.*/)
          ii <- c(ii, "23:59:59.999999999")
        } else if (nchar(ii[1L]) == 0) {
          # i is left open ended (/T)
          ii[1L] <- "00:00:00.000000000"
        } # else i is bounded on both sides (T.*/T.*)
        i <- .subsetTimeOfDay(x, ii[1L], ii[2L])
      } else {
        # enables subsetting by date style strings
        # must be able to process - and then allow for operations???

        i.tmp <- NULL
        tz <- as.character(tzone(x))

        for(ii in i) {
          adjusted.times <- .parseISO8601(ii, .index(x)[1], .index(x)[nr], tz=tz)
          if(length(adjusted.times) > 1) {
            i.tmp <- c(i.tmp, index_bsearch(.index(x), adjusted.times$first.time, adjusted.times$last.time))
          }
        }
        i <- i.tmp
      }
      i_len <- length(i)

      if(i_len == 1L)  # IFF we are using ISO8601 subsetting
        USE_EXTRACT <- TRUE
    }
  
    if(!isOrdered(i,strictly=FALSE)) {
      i <- sort(i)
    }
    # subset is picky, 0's in the 'i' position cause failures
    zero.index <- binsearch(0L, i, FALSE)
    if(!is.na(zero.index)) {
      # at least one 0; binsearch returns location of last 0
      i <- i[-(1L:zero.index)]
    }

    if(length(i) <= 0 && USE_EXTRACT) 
      USE_EXTRACT <- FALSE

    if(which.i)
      return(i)

    } # if(!missing(i)) { end

    if (missing(j)) {
      if(missing(i))
        i <- seq_len(nr)

      if(length(x)==0) {
        cdata <- rep(NA, length(i))
        storage.mode(cdata) <- storage.mode(x)
        x.tmp <- .xts(cdata, .index(x)[i], tclass(x), tzone(x),
                      dimnames = list(NULL, colnames(x)))
        return(x.tmp)
      } else {
        if(USE_EXTRACT) {
          return(.Call(C_extract_col,
                       x, as.integer(1:nc),
                       drop,
                       as.integer(i[1]), as.integer(i[length(i)])))
        } else {
          return(.Call(C__do_subset_xts,
                       x, as.integer(i),
                       as.integer(1:nc), 
                       drop))
        }
      }
    } else
    # test for negative subscripting in j
    if (is.numeric(j)) {

      # warn and convert if 'j' is not integer-like
      j_int <- as.integer(j)
      j_eps <- abs(j) - abs(j_int)
      if (isTRUE(any(j_eps > sqrt(.Machine$double.eps)))) {
        warning("converting 'j' to integer because it appears to contain fractions")
        j <- j_int
      }

      if(min(j,na.rm=TRUE) < 0) {
        if(max(j,na.rm=TRUE) > 0)
          stop('only zeros may be mixed with negative subscripts')
        j <- (1:nc)[j]
      }
      if(max(j,na.rm=TRUE) > nc)
        stop('subscript out of bounds')
    } else
    if(is.logical(j)) {
      if(length(j) == 1) {
        j <- (1:nc)[rep(j, nc)]
      }
      else if (length(j) > nc) {
          stop("(subscript) logical subscript too long")
      } else j <- (1:nc)[j]
    } else
    if(is.character(j)) {
      j <- match(j, colnames(x), nomatch=0L)
      # ensure all j are in colnames(x)
      if(any(j==0))
        stop("subscript out of bounds")
    }

    j0 <- which(!as.logical(j))
    if(length(j0)) 
      j <- j[-j0]
    if(length(j) == 0 || (length(j)==1 && (is.na(j) || j==0))) {
      if(missing(i))
        i <- seq_len(nr)

      output <- .xts(coredata(x)[i,j,drop=FALSE], .index(x)[i],
                     tclass(x), tzone(x), class = class(x))
      xtsAttributes(output) <- xtsAttributes(x)
      return(output)
    } 
    if(missing(i))
      return(.Call(C_extract_col, x, as.integer(j), drop, 1, nr))
    if(USE_EXTRACT) {
          return(.Call(C_extract_col,
                       x, as.integer(j),
                       drop,
                       as.integer(i[1]), as.integer(i[length(i)])))
    } else
    return(.Call(C__do_subset_xts, x, as.integer(i), as.integer(j), drop))
}

`.subset.xts` <- `[.xts`

# Replacement method for xts objects
#
# Adapted from [.xts code, making use of NextGeneric as
# replacement function in R already preserves all attributes
# and index value is left untouched

`[<-.xts` <-
#`xtsreplacement` <-
function(x, i, j, value) 
{
    if (!missing(i)) {
      i <- x[i, which.i=TRUE]
    }
    .Class <- "matrix"
    NextMethod(.Generic)
}

# Convert a character or time type to POSIXct for use by subsetting and window
# We make this an explicit function so that subset and window will convert dates consistently.
.toPOSIXct <-
function(i, tz) {
  if(inherits(i, "POSIXct")) {
    dts <- i
  } else if(is.character(i)) {
    dts <- as.POSIXct(as.character(i),tz=tz)  # Need as.character because i could be AsIs from I(dates)
  } else if (timeBased(i)) {
    if(inherits(i, "Date")) {
      dts <- as.POSIXct(as.character(i),tz=tz)
    } else {
      # force all other time classes to be POSIXct
      dts <- as.POSIXct(i,tz=tz)
    }
  } else {
    stop("invalid time / time based class")
  }
  dts
}

# find the rows of index. where the date is in [start, end].
# use binary search.
# convention is that NA start or end returns empty
index_bsearch <- function(index., start, end)
{
  if(!is.null(start) && is.na(start)) return(NULL)
  if(!is.null(end) && is.na(end)) return(NULL)

  if(is.null(start)) {
    si <- 1
  } else {
    si <- binsearch(start, index., TRUE)
  }
  if(is.null(end)) {
    ei <- length(index.)
  } else {
    ei <- binsearch(end, index., FALSE)
  }
  if(is.na(si) || is.na(ei) || si > ei) return(NULL)
  firstlast <- seq.int(si, ei)
  firstlast
}

# window function for xts series
# return indexes in x matching dates
window_idx <- function(x, index. = NULL, start = NULL, end = NULL)
{
  if(is.null(index.)) {
    usr_idx <- FALSE
    index. <- .index(x)
  } else {
    # Translate the user index to the xts index
    usr_idx <- TRUE
    idx <- .index(x)

    index. <- .toPOSIXct(index., tzone(x))
    index. <- unclass(index.)
    index. <- index.[!is.na(index.)]
    if(is.unsorted(index.)) {
      # index. must be sorted for index_bsearch
      # N.B!! This forces the returned values to be in ascending time order, regardless of the ordering in index, as is done in subset.xts.
      index. <- sort(index.)
    }
    # Fast search on index., faster than binsearch if index. is sorted (see findInterval)
    base_idx <- findInterval(index., idx)
    base_idx <- pmax(base_idx, 1L)
    # Only include indexes where we have an exact match in the xts series
    match <- idx[base_idx] == index.
    base_idx <- base_idx[match]
    index. <- index.[match]
    index. <- .POSIXct(index., tz = tzone(x))
    if(length(base_idx) < 1) return(x[NULL,])
  }

  if(!is.null(start)) {
    start <- .toPOSIXct(start, tzone(x))
  }

  if(!is.null(end)) {
    end <- .toPOSIXct(end, tzone(x))
  }

  firstlast <- index_bsearch(index., start, end)

  if(usr_idx && !is.null(firstlast)) {
    # Translate from user .index to xts index
    # We get back upper bound of index as per findInterval
    tmp <- base_idx[firstlast]

    res <- .Call(C_fill_window_dups_rev, tmp, .index(x))
    firstlast <- rev(res)
  }

  firstlast
}

# window function for xts series, use binary search to be faster than base zoo function
# index. defaults to the xts time index.  If you use something else, it must conform to the standard for order.by in the xts constructor.
# that is, index. must be time based,


#' Extract Time Windows from xts Objects
#' 
#' Method for extracting time windows from xts objects.
#' 
#' The xts `window()` method provides an efficient way to subset an xts object
#' between a start and end date using a binary search algorithm. Specifically,
#' it converts `start` and `end` to POSIXct and then does a binary search of
#' the index to quickly return a subset of `x` between `start` and `end`.
#' 
#' Both `start` and `end` may be any class that is convertible to POSIXct, such
#' as a character string in the format \sQuote{yyyy-mm-dd}. When `start = NULL`
#' the returned subset will begin at the first value of `index.`. When
#' `end = NULL` the returned subset will end with the last value of `index.`.
#' Otherwise the subset will contain all timestamps where `index.` is between
#' `start` and `end`, inclusive.
#'
#' When `index.` is specified, [`findInterval()`] is used to quickly retrieve
#' large sets of sorted timestamps. For the best performance, `index.` must be
#' a *sorted* POSIXct vector or a numeric vector of seconds since the epoch.
#' `index.` is typically a subset of the timestamps in `x`.
#' 
#' @param x An xts object.
#' @param index. A user defined time index (default `.index(x)`).
#' @param start A start time coercible to POSIXct.
#' @param end An end time coercible to POSIXct.
#' @param \dots Unused.
#' 
#' @return The subset of `x` that matches the time window.
#' 
#' @author Corwin Joy
#' 
#' @seealso [`subset.xts()`], [`findInterval()`], [`xts()`]
#' 
#' @keywords ts
#' @examples
#' 
#' ## xts example
#' x.date <- as.Date(paste(2003, rep(1:4, 4:1), seq(1,19,2), sep = "-"))
#' x <- xts(matrix(rnorm(20), ncol = 2), x.date)
#' x
#' 
#' window(x, start = "2003-02-01", end = "2003-03-01")
#' window(x, start = as.Date("2003-02-01"), end = as.Date("2003-03-01"))
#' window(x, index. = x.date[1:6], start = as.Date("2003-02-01"))
#' window(x, index. = x.date[c(4, 8, 10)])
#' 
#' ## Assign to subset
#' window(x, index. = x.date[c(4, 8, 10)]) <- matrix(1:6, ncol = 2)
#' x
#' 
window.xts <- function(x, index. = NULL, start = NULL, end = NULL, ...)
{
  # scalar NA values are treated as NULL
  if (isTRUE(is.na(start))) start <- NULL
  if (isTRUE(is.na(end))) end <- NULL
  
  if(is.null(start) && is.null(end) && is.null(index.)) return(x)

  # dispatch to window.zoo() for yearmon and yearqtr
  if(any(tclass(x) %in% c("yearmon", "yearqtr"))) {
    return(NextMethod(.Generic))
  }

  firstlast <- window_idx(x, index., start, end) # firstlast may be NULL

  .Call(C__do_subset_xts,
     x, as.integer(firstlast),
     seq.int(1, ncol(x)),
     drop = FALSE)
}

# Declare binsearch to call the routine in binsearch.c
binsearch <- function(key, vec, start=TRUE) {
  # Convert to double if both are not integer
  if (storage.mode(key) != storage.mode(vec)) {
    storage.mode(key) <- storage.mode(vec) <- "double"
  }
  .Call(C_binsearch, key, vec, start)
}
