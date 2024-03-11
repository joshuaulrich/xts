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
#' Details on efficient subsetting of \code{xts} objects for maximum
#' performance and compatibility.
#' 
#' One of the primary motivations, and key points of differentiation of the
#' time series class xts, is the ability to subset rows by specifying ISO-8601
#' compatible range strings.  This allows for natural range-based time queries
#' without requiring prior knowledge of the underlying time object used in
#' construction.
#' 
#' When a raw character vector is used for the \code{i} subset argument, it is
#' processed as if it was ISO-8601 compliant.  This means that it is parsed
#' from left to right, according to the following specification:
#' 
#' CCYYMMDD HH:MM:SS.ss+
#' 
#' A full description will be expanded from a left-specified truncated one.
#' 
#' Additionally, one may specify range-based queries by simply supplying two
#' time descriptions seperated by a forward slash:
#' 
#' CCYYMMDD HH:MM:SS.ss+/CCYYMMDD HH:MM:SS.ss
#' 
#' The algorithm to parse the above is \code{.parseISO8601} from the \pkg{xts}
#' package.
#' 
#' ISO-style subsetting, given a range type query, makes use of a custom binary
#' search mechanism that allows for very fast subsetting as no linear search
#' though the index is required.  ISO-style character vectors may be longer
#' than length one, allowing for multiple non-contiguous ranges to be selected
#' in one subsetting call.
#' 
#' If a character \emph{vector} representing time is used in place of numeric
#' values, ISO-style queries, or timeBased objects, the above parsing will be
#' carried out on each element of the i-vector.  This overhead can be very
#' costly. If the character approach is used when no ISO range querying is
#' needed, it is recommended to wrap the \sQuote{i} character vector with the
#' \code{I()} function call, to allow for more efficient internal processing.
#' Alternately converting character vectors to POSIXct objects will provide the
#' most performance efficiency.
#' 
#' As \code{xts} uses POSIXct time representations of all user-level index
#' classes internally, the fastest timeBased subsetting will always be from
#' POSIXct objects, regardless of the \code{tclass} of the original object.
#' All non-POSIXct time classes are converted to character first to preserve
#' consistent TZ behavior.
#' 
#' @param x xts object
#' @param i the rows to extract. Numeric, timeBased or ISO-8601 style (see
#' details)
#' @param j the columns to extract, numeric or by name
#' @param drop should dimension be dropped, if possible. See NOTE.
#' @param which.i return the \sQuote{i} values used for subsetting. No subset
#' will be performed.
#' @param \dots additional arguments (unused)
#' 
#' @return An extraction of the original xts object.  If \code{which.i} is
#' TRUE, the corresponding integer \sQuote{i} values used to subset will be
#' returned.
#' 
#' @note By design, drop=FALSE in the default case.  This preserves the basic
#' underlying type of \code{matrix} and the \code{dim()} to be non-NULL. This
#' is different from both matrix and \code{zoo} behavior as uses
#' \code{drop=TRUE}.  Explicitly passing \code{drop=TRUE} may be required when
#' performing certain matrix operations.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{xts}}, \code{\link{.parseISO8601}},
#' \code{\link{.index}}
#' 
#' @references ISO 8601: Date elements and interchange formats - Information
#' interchange - Representation of dates and time \url{https://www.iso.org}
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
#' # drop=FALSE for xts, differs from zoo and matrix
#' z <- as.zoo(xx)
#' z/z[,1]
#' 
#' m <- as.matrix(xx)
#' m/m[,1]
#' 
#' # this will fail with non-conformable arrays (both retain dim)
#' tryCatch(
#'   xx/x[,1], 
#'   error=function(e) print("need to set drop=TRUE")
#' )
#' 
#' # correct way
#' xx/xx[,1,drop=TRUE]
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


#' Extract time windows from an \code{xts} series
#' 
#' Method for extracting time windows from \code{xts} objects.
#' 
#' The point of having \code{window} in addition to the regular subset function
#' is to have a fast way of extracting time ranges from an \code{xts} series.
#' In particular, this method will convert \code{start} and \code{end} to
#' \code{POSIXct} then do a binary lookup on the internal \code{xts} index to
#' quickly return a range of matching dates. With a user supplied
#' \code{index.}, a similarly fast invocation of \code{findInterval} is used so
#' that large sets of sorted dates can be retrieved quickly.
#' 
#' @param x an object.
#' @param index. a user defined time index. This defaults to the \code{xts}
#' index for the series via \code{.index(x)}. When supplied, this is typically
#' a subset of the dates in the full series.\cr The \code{index.} must be a set
#' of dates that are convertible to \code{POSIXct}. If you want fast lookups,
#' then \code{index.} should be sorted and of class \code{POSIXct}.\cr If an
#' unsorted \code{index.} is passed in, \code{window} will sort it.
#' @param start a start time. Extract \code{xts} rows where \code{index. >=
#' start}. \code{start} may be any class that is convertible to \code{POSIXct}
#' such as a character variable in the format \sQuote{YYYY-MM-DD}.\cr If
#' \code{start} is \code{NULL} then all \code{index.} dates are matched.
#' @param end an end time. Extract \code{xts} rows where \code{index. <= end}.
#' \code{end} must be convertible to \code{POSIXct}. If \code{end} is
#' \code{NULL} then all \code{index.} dates are matched.
#' @param \dots currently not used.
#' 
#' @return The matching time window is extracted.
#' 
#' @author Corwin Joy
#' 
#' @seealso \code{\link{subset.xts}}, \code{\link[base]{findInterval}},
#' \code{\link{xts}}
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
#' window(x, index = x.date[1:6], start = as.Date("2003-02-01"))
#' window(x, index = x.date[c(4, 8, 10)])
#' 
#' ## Assign to subset
#' window(x, index = x.date[c(4, 8, 10)]) <- matrix(1:6, ncol = 2)
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

# Unit tests for the above code may be found in runit.xts.methods.R
