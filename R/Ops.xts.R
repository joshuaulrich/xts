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


`Ops.xts` <-
function(e1, e2)
{
  # determine and output class
  # use 'e1' first because e2 is missing for unary +/-/!
  if(inherits(e1, "xts")) {
    # e1 could be a derived class; use its class for output
    # NOTE: we want the output to be an xts object even if e2 is a derived
    # class, because Ops.xts() might not create an appropriate derived class
    # object
    out_class <- class(e1)
  } else {
    # if 'e1' isn't xts, then e2 must be xts or a derived class, otherwise
    # this method wouldn't have been called
    out_class <- class(e2)
  }

  e <- if (missing(e2)) {
      .Class <- "matrix"
      NextMethod(.Generic)
  }
  else if (any(nchar(.Method) == 0)) {
      .Class <- "matrix"
      NextMethod(.Generic)
  }
  else {
    if( NROW(e1)==NROW(e2) && identical(.index(e1),.index(e2)) ) {
    .Class <- "matrix"
    NextMethod(.Generic)
    } else {
      tmp.e1 <- merge.xts(e1, e2, all=FALSE, retclass=FALSE, retside=c(TRUE,FALSE), check.names=FALSE)
      e2 <- merge.xts(e2, e1, all=FALSE, retclass=FALSE, retside=c(TRUE,FALSE), check.names=FALSE)
      e1 <- tmp.e1
      .Class <- "matrix"
      NextMethod(.Generic)
    }
  }
  # These return an object the same class as input(s); others return a logical object
  if(.Generic %in% c("+","-","*","/","^","%%","%/%")) {
    e <- .Call(C_add_class, e, out_class)
  }
  if(length(e)==0) {
    if(is.xts(e1)) {
      idx <- .index(e1)
    } else {
      idx <- .index(e2)
    }
    idx[] <- idx[0]
    attr(e,'index') <- idx
  }
  dn <- dimnames(e)
  if(!is.null(dn[[1L]])) {
    if(is.null(dn[[2L]])) {
      attr(e, "dimnames") <- NULL
    } else {
      dimnames(e) <- list(NULL, dn[[2L]])
    }
  }
  if(is.null(attr(e,'index'))) {
    if(is.xts(e1)) {
      e <- .xts(e, .index(e1), tclass(e1), tzone(e1), tformat = tformat(e1))
    } else if(is.xts(e2)) {
      e <- .xts(e, .index(e2), tclass(e2), tzone(e2), tformat = tformat(e2))
    } else {
      # neither have class = ('xts', 'zoo'), because they were overwritten
      # by the result of merge(..., retclass = FALSE). But they still have
      # an 'index' attribute.
      ix <- .index(e1)
      if (is.null(ix)) {
        ix <- .index(e2)
      }
      e <- .xts(e, ix, tclass(ix), tzone(ix), tformat = tformat(ix))
    }
    if(is.null(dim(e1)) && is.null(dim(e2)))
      dim(e) <- NULL
  }
  attr(e, "names") <- NULL
  e
}
