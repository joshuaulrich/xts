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
    #check.TZ(x)
    #original.cols <- NCOL(x)
    #original.attr <- xtsAttributes(x)
    if(is.null(dim(x))) {
      nr <- length(x)
      nc <- 1L
    } else {
      nr <- nrow(x)
      nc <- ncol(x)
    }
    
    if(missing(i)) {
      i <- 1:nr
    } else
    # test for negative subscripting in i
    if (is.numeric(i)) {
      #if(any(i < 0)) {
      if(.Call("any_negative", i, PACKAGE="xts")) {
        if(!all(i <= 0))
          stop('only zeros may be mixed with negative subscripts')
        i <- (1:nr)[i]
      }
      if(max(i) > nr)
        stop('subscript out of bounds')
    } else
    if(inherits(i, "AsIs") && is.character(i)) {
      i <- MATCH(i, format(index(x)))
    } else
    if (timeBased(i)) { # || (inherits(i, "AsIs") && is.character(i))) {
      if(inherits(i, "POSIXct")) {
        #i <- match(i, .index(x))
        i <- which(!is.na(match(.index(x), i)))
      } else {
        #i <- match(as.POSIXct(as.character(i)), .index(x))
        i <- which(!is.na(match(.index(x), as.POSIXct(as.character(i)))))
      }
      i[is.na(i)] <- 0
    } else 
    if(is.logical(i)) {
      i <- which(i) #(1:NROW(x))[rep(i,length.out=NROW(x))]
    } else
    if (is.character(i)) {
      # is i of the format T/T?
      if(length(i) == 1 && !identical(integer(),grep("^T.*?/T",i[1]))) {
        i <- gsub("T|:","",i)
        i <- strsplit(i, "/")[[1]]
        i <- .makeISO8601TT(x, i[1],i[2])
      }
      # enables subsetting by date style strings
      # must be able to process - and then allow for operations???

      i.tmp <- NULL
      tz <- as.character(indexTZ(x)) # ideally this moves to attr(index,"tzone")

      for(ii in i) {
        #adjusted.times <- .parseISO8601(ii, first(.index(x)), last(.index(x)))
        #`[.POSIXct` <- function(x, ...) { .Class="Matrix"; NextMethod("[") }
        adjusted.times <- .parseISO8601(ii, .index(x)[1], .index(x)[nr], tz=tz)
        if(length(adjusted.times) > 1) {
          firstlast <- c(seq.int(binsearch(adjusted.times$first.time, .index(x),  TRUE),
                                 binsearch(adjusted.times$last.time,  .index(x), FALSE))
                     )
          if(isOrdered(firstlast, strict=FALSE)) # fixed non-match within range bug
            i.tmp <- c(i.tmp, firstlast)
        }
      }
      i <- i.tmp
      #if(is.null(i)) i <- NA
    }


  
    if(!isOrdered(i,strictly=FALSE)) {
      i <- sort(i)
    }
    # subset is picky, 0's in the 'i' position cause failures
    zero.index <- binsearch(0, i, NULL)
    if(!is.na(zero.index))
      i <- i[ -zero.index ]

    if(which.i)
      return(i)

    if (missing(j)) {
      if(length(x)==0) {
        x.tmp <- .xts(rep(NA,length(i)), .index(x)[i])
        return((colnames(x.tmp) <- colnames(x)))
      } else {
        return(.Call('_do_subset_xts', 
                     x, as.integer(i),
                     as.integer(1:nc), 
                     drop, PACKAGE='xts'))
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
      } else j <- (1:nc)[j]
    } else
    if(is.character(j)) {
      j <- which(match(colnames(x), j, nomatch=0L) > 0L)
    }

    #j0 <- which(j==0)
    j0 <- which(!as.logical(j))
    if(length(j0)) 
      j <- j[-j0]
    if(length(j) == 0 || (length(j)==1 && j==0))
      return(.xts(coredata(x)[i,j,drop=FALSE], index=.index(x)[i],
                  .indexCLASS=indexClass(x), .indexTZ=indexTZ(x)))
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

    if (timeBased(i)) { # || (inherits(i, "AsIs") && is.character(i))) {
      if(inherits(i, "POSIXct")) {
        #i <- match(i, .index(x))
        i <- which(!is.na(match(.index(x), i)))
      } else {
        #i <- match(as.POSIXct(as.character(i)), .index(x))
        i <- which(!is.na(match(.index(x), as.POSIXct(as.character(i)))))
      }
      i[is.na(i)] <- 0
    } else 
    if(is.logical(i)) {
      i <- which(i) #(1:NROW(x))[rep(i,length.out=NROW(x))]
    } else
    if (is.character(i)) {
      # enables subsetting by date style strings
      # must be able to process - and then allow for operations???

      i.tmp <- NULL
      for(ii in i) {
        if(!identical(grep("(::)|/",ii),integer(0))) {
          tBR <- timeBasedRange(ii)
          
          # the first index value to be found
          if(is.na(tBR[1])) {
            first.time <- .index(x)[1]
          } else first.time <- tBR[1]

          # the last index value ot be found
          if(is.na(tBR[2])) {
            last.time  <- .index(x)[nr]
          } else last.time <- tBR[2]

        } else {
          # if single date is given - get start and end points if resolution of
          # series is greater than the time specified
          dates <- paste(ii,ii,sep='/')
          tBR <- timeBasedRange(dates)
          first.time <- tBR[1]
          last.time  <- tBR[2]
        }      
        
        i.tmp <- c(i.tmp,
                   seq.int(binsearch(first.time, .index(x),  TRUE),
                           binsearch(last.time,  .index(x), FALSE))
                  )
      }
      i <- i.tmp
      # .subset is picky, 0's in the 'i' position cause failures -- is this still nec? -jar
      zero.index <- binsearch(0, i, NULL)
      if(!is.na(zero.index))
        i <- i[ -zero.index ]
    }
    }
    .Class <- "matrix"
    NextMethod(.Generic)
}
