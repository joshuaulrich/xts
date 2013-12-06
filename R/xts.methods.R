#
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
    if(is.null(dim(x))) {
      nr <- length(x)
      if(nr==0 && !which.i)
        return( xts(rep(NA,length(index(x))), index(x))[i] )
      nr <- length(.index(x))
      nc <- 1L
    } else {
      nr <- nrow(x)
      nc <- ncol(x)
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
    if(inherits(i, "AsIs") && is.character(i)) {
      i <- MATCH(i, format(index(x)))
    } else
    if (timeBased(i)) { # || (inherits(i, "AsIs") && is.character(i))) {
      if(inherits(i, "POSIXct")) {
        i <- which(!is.na(match(.index(x), i)))
      } else if(inherits(i, "Date")) {
        i <- which(!is.na(match(.index(x), as.POSIXct(as.character(i),tz=indexTZ(x)))))
      } else {
        # force all other time classes to be POSIXct
        i <- which(!is.na(match(.index(x), as.POSIXct(i,tz=indexTZ(x)))))
      }
      i[is.na(i)] <- 0
    } else 
    if(is.logical(i)) {
      i <- which(i) #(1:NROW(x))[rep(i,length.out=NROW(x))]
    } else
    if (is.character(i)) {
      # is i of the format T/T?
      if(length(i) == 1 && !identical(integer(),grep("^T.*?/T",i[1]))) {
      #if(grepl("^T.*?/T",i[1]) && length(i) == 1) {
        i <- gsub("T|:","",i)
        i <- strsplit(i, "/")[[1]]
        i <- .makeISO8601TT(x, i[1],i[2])
      }
      # enables subsetting by date style strings
      # must be able to process - and then allow for operations???

      i.tmp <- NULL
      tz <- as.character(indexTZ(x)) # ideally this moves to attr(index,"tzone")
      i_len <- length(i)

      for(ii in i) {
        adjusted.times <- .parseISO8601(ii, .index(x)[1], .index(x)[nr], tz=tz)
        if(length(adjusted.times) > 1) {
          firstlast <- c(seq.int(binsearch(adjusted.times$first.time, .index(x),  TRUE),
                                 binsearch(adjusted.times$last.time,  .index(x), FALSE))
                     )
          if(isOrdered(firstlast, strictly=FALSE)) # fixed non-match within range bug
            i.tmp <- c(i.tmp, firstlast)
        }
      }
      i <- i.tmp

      if(i_len == 1L)  # IFF we are using ISO8601 subsetting
        USE_EXTRACT <- TRUE
    }
  
    if(!isOrdered(i,strictly=FALSE)) {
      i <- sort(i)
    }
    # subset is picky, 0's in the 'i' position cause failures
    zero.index <- binsearch(0, i, NULL)
    if(!is.na(zero.index))
      i <- i[ -1L:-zero.index ]  # at least one 0; binsearch returns location of last 0

    if(length(i) <= 0 && USE_EXTRACT) 
      USE_EXTRACT <- FALSE

    if(which.i)
      return(i)

    } # if(!missing(i)) { end

    if (missing(j)) {
      if(missing(i))
        i <- seq_len(nr)

      if(length(x)==0) {
        x.tmp <- .xts(rep(NA,length(i)), .index(x)[i])
        return((colnames(x.tmp) <- colnames(x)))
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
    }

    j0 <- which(!as.logical(j))
    if(length(j0)) 
      j <- j[-j0]
    if(length(j) == 0 || (length(j)==1 && j==0)) {
      if(missing(i))
        i <- seq_len(nr)
      return(.xts(coredata(x)[i,j,drop=FALSE], index=.index(x)[i],
                  .indexCLASS=indexClass(x), .indexTZ=indexTZ(x)))
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
