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


`binsearch` <-
function(key,  vec, start=TRUE) {
  # vec is a numeric vector to search
  # key is the numeric key we are looking for
  # start is a logical indicating
  # TRUE return the _next_ observation (e.g. 20070801 if
  # missing would return 20070802 if available
  # FALSE would return the _previous_ obs.
  found <- FALSE
  lo <- 1; hi <- length(vec);
  rec <- NULL

  while(hi >= lo) {
    mid <- round((lo + hi) / 2)
    if(mid == 0)
      return(NA)
    if(mid != 0 && key < vec[mid]) {
      hi <- mid - 1
    } else 
    if(mid != 0 && key > vec[mid]) {
      lo <- mid + 1
    } else {
      found <- TRUE
      i <- 0
      if(!is.null(start)) {
      if(!start) {
        while(1) {
          if(mid==length(vec) || vec[mid+1] != key) break
          mid <- mid+1
        }
      } else {
        while(1) {
          if(mid==1 || vec[mid-1] != key) break
          mid <- mid-1
        }
      }
      }
      rec <- mid
      break
    }
  }

  # force only exact matches to return a value other than NA
  if(is.null(start) && is.null(rec))
    return(NA)

  # if not found return the appropriate bound
  if(is.null(rec)) {
    if(start) {
      lo
    } else {
      hi
    }
  # if found - return the exact match location
  } else rec
}

naCheck <- function(x, n=0) {
  if(is.null(dim(x)[2])) {
    NAs <- .Call("naCheck", x, TRUE)
  } else NAs <- .Call("naCheck", rowSums(x), TRUE)
  ret <- list()
  ret$NAs <- NAs
  ret$nonNA <- (1+NAs):NROW(x)
  ret$beg <- n+NAs
  invisible(ret)
}
