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
  .Call('do_subset_xts', x, i, j, FALSE, PACKAGE='xts')
}

`.subset.xts` <- `[.xts` <-
function(x, i, j, drop = FALSE, ...) 
{
    #check.TZ(x)
    #original.cols <- NCOL(x)
    #original.attr <- xtsAttributes(x)
    
    if(missing(i)) {
      i <- 1:NROW(x)
    } else
    # test for negative subscripting in i
    if (!missing(i) && is.numeric(i)) {
      #if(any(i < 0)) {
      if(.Call("any_negative", i, PACKAGE="xts")) {
        if(!all(i <= 0))
          stop('only zeros may be mixed with negative subscripts')
        i <- (1:NROW(x))[i]
      }
      if(max(i) > NROW(x))
        stop('subscript out of bounds')
    } else
    if(inherits(i, "AsIs") && is.character(i)) {
      i <- MATCH(i, format(index(x)))
    } else
    if (timeBased(i)) { # || (inherits(i, "AsIs") && is.character(i))) {
      if(inherits(i, "POSIXct")) {
        i <- match(i, .index(x))
      } else {
        i <- match(as.POSIXct(as.character(i)), .index(x))
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
        #adjusted.times <- .parseISO8601(ii, first(.index(x)), last(.index(x)))
        #`[.POSIXct` <- function(x, ...) { .Class="Matrix"; NextMethod("[") }
        adjusted.times <- .parseISO8601(ii, .index(x)[1], .index(x)[NROW(x)])
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

    # test for negative subscripting in j
    if (!missing(j) && is.numeric(j)) { # && any(j < 0)) {
      if(any(j < 0)) {
        if(!all(j < 0))
          stop('only zeros may be mixed with negative subscripts')
        j <- (1:NCOL(x))[j]
      }
      if(max(j) > NCOL(x))
        stop('subscript out of bounds')
    }

  
    if(!isOrdered(i,strictly=FALSE)) {
      i <- sort(i)
    }
    # subset is picky, 0's in the 'i' position cause failures
    zero.index <- binsearch(0, i, NULL)
    if(!is.na(zero.index))
      i <- i[ -zero.index ]

    if (missing(j)) {
      if(length(x)==0) {
        x.tmp <- .xts(rep(NA,length(i)), .index(x)[i])
        return((colnames(x.tmp) <- colnames(x)))
      } else {
        return(.Call('do_subset_xts', 
                     x, as.integer(i),
                     as.integer(1:NCOL(x)), 
                     drop, PACKAGE='xts'))
      }
    }
    else {
        if(is.logical(j)) {
          if(length(j) == 1) {
            j <- (1:NCOL(x))[rep(j, NCOL(x))]
          } else j <- (1:NCOL(x))[j]
        }
        j <- sapply(j, function(xx) {
                         if(is.character(xx)) {
                           which(xx==colnames(x))
                         } else xx
                       })
        
        return(.Call('do_subset_xts', x, as.integer(i), as.integer(j), drop, PACKAGE='xts'))
   } 
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
    #sys.TZ <- Sys.getenv('TZ') 
    #Sys.setenv(TZ='GMT')
    #on.exit(Sys.setenv(TZ=sys.TZ))
    if (!missing(i)) {

    if (timeBased(i)) 
      # this shouldn't happen either, though less important I suspect  FIXME
      i <- as.character(i) 

    if(is.logical(i))
      i <- which(i) #(1:NROW(x))[rep(i,length.out=NROW(x))]

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
            last.time  <- .index(x)[NROW(x)]
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
    }
  
    # .subset is picky, 0's in the 'i' position cause failures -- is this still nec? -jar
    zero.index <- binsearch(0, i, NULL)
    if(!is.na(zero.index))
      i <- i[ -zero.index ]
    }

    .Class <- "matrix"
    NextMethod(.Generic)
}
