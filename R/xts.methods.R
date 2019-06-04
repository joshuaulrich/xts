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
  timestringToSeconds <- function(timeString) {
    # "09:00:00" to seconds of day
    origin <- paste("1970-01-01", timeString)
    as.numeric(as.POSIXct(origin, "UTC")) %% 86400L
  }

  # handle timezone
  tz <- indexTZ(x)
  secOfDay <- as.POSIXlt(index(x), tz = tz)
  secOfDay <- secOfDay$hour*60*60 + secOfDay$min*60 + secOfDay$sec

  secBegin <- timestringToSeconds(fromTimeString)
  secEnd   <- timestringToSeconds(toTimeString)

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
  .Call('_do_subset_xts', x, i, j, FALSE, PACKAGE='xts')
}

`.subset.xts` <- `[.xts` <-
function(x, i, j, drop = FALSE, which.i=FALSE,...) 
{
    USE_EXTRACT <- FALSE # initialize to FALSE

    dimx <- dim(x)
    if(is.null(dimx)) {
      nr <- length(x)
      if(nr==0 && !which.i)
        return( xts(rep(NA,length(index(x))), index(x))[i] )
      nr <- length(.index(x))
      nc <- 1L
    } else {
      nr <- dimx[1L]
      nc <- dimx[2L]
    }
    
    if(!missing(i)) {
    # test for negative subscripting in i
    if (is.numeric(i)) {
      #if(any(i < 0)) {
      if(.Call("any_negative", i, PACKAGE="xts")) {
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
      if(length(i) == 1 && !identical(integer(),grep("^T.*?/T",i[1]))) {
        # is i of the format T/T?
        ii <- gsub("T", "", i, fixed = TRUE)
        ii <- strsplit(ii, "/", fixed = TRUE)[[1L]]
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
        x.tmp <- .xts(rep(NA,length(i)), .index(x)[i], dimnames=list(NULL, colnames(x)))
        return(x.tmp)
      } else {
        if(USE_EXTRACT) {
          return(.Call('extract_col', 
                       x, as.integer(1:nc),
                       drop,
                       as.integer(i[1]), as.integer(i[length(i)]), PACKAGE="xts"))
        } else {
          return(.Call('_do_subset_xts', 
                       x, as.integer(i),
                       as.integer(1:nc), 
                       drop, PACKAGE='xts'))
        }
      }
    } else
    # test for negative subscripting in j
    if (is.numeric(j)) {
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
      return(.xts(coredata(x)[i,j,drop=FALSE], index=.index(x)[i]))
    } 
    if(missing(i))
      return(.Call("extract_col", x, as.integer(j), drop, 1, nr, PACKAGE='xts'))
    if(USE_EXTRACT) {
          return(.Call('extract_col', 
                       x, as.integer(j),
                       drop,
                       as.integer(i[1]), as.integer(i[length(i)]), PACKAGE='xts'))
    } else
    return(.Call('_do_subset_xts', x, as.integer(i), as.integer(j), drop, PACKAGE='xts'))
}

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

    res <- .Call("fill_window_dups_rev", tmp, .index(x), PACKAGE = "xts")
    firstlast <- rev(res)
  }

  firstlast
}

# window function for xts series, use binary search to be faster than base zoo function
# index. defaults to the xts time index.  If you use something else, it must conform to the standard for order.by in the xts constructor.
# that is, index. must be time based,
window.xts <- function(x, index. = NULL, start = NULL, end = NULL, ...)
{
  if(is.null(start) && is.null(end) && is.null(index.)) return(x)

  firstlast <- window_idx(x, index., start, end) # firstlast may be NULL

  .Call('_do_subset_xts',
     x, as.integer(firstlast),
     seq.int(1, ncol(x)),
     drop = FALSE, PACKAGE='xts')
}

# Declare binsearch to call the routine in binsearch.c
binsearch <- function(key, vec, start=TRUE) {
  # Convert to double if both are not integer
  if (storage.mode(key) != storage.mode(vec)) {
    storage.mode(key) <- storage.mode(vec) <- "double"
  }
  .Call("binsearch", key, vec, start, PACKAGE='xts')
}

# Unit tests for the above code may be found in runit.xts.methods.R
