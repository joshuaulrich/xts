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


#' Extract/Replace Core Data of an xts Object
#' 
#' Mechanism to extract and replace the core data of an \code{xts} object.
#' 
#' Extract coredata of an \code{xts} object - removing all attributes except
#' \code{dim} and \code{dimnames} and returning a matrix object with rownames
#' converted from the index of the \code{xts} object.
#' 
#' The \code{fmt} argument, if TRUE, allows the internal index formatting
#' specified by the user to be used. Alternatively, it may be a valid
#' formatting string to be passed to \code{format}. Setting to FALSE will
#' return the row names by simply coercing the index class to a character
#' string in the default manner.
#' 
#' \code{xcoredata} is the functional complement to \code{coredata}, returning
#' all of the attributes normally removed by \code{coredata}.  Its purpose,
#' along with the replacement function \code{xcoredata<-} is primarily for use
#' by developers using \pkg{xts} to allow for internal replacement of values
#' removed during use of non xts-aware functions.
#' 
#' @param x an \code{xts} object
#' @param fmt should the rownames be formated in a non-standard way
#' @param value non-core attributes to assign
#' @param \dots further arguments [unused]
#' 
#' @return Returns either a matrix object for coredata, or a list of named
#' attributes.
#' 
#' The replacement functions are called for their side-effects.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link[zoo:zoo]{coredata}}, \code{\link{xtsAttributes}}
#' 
#' @keywords utilities
#' @examples
#' 
#' data(sample_matrix)
#' x <- as.xts(sample_matrix, myattr=100)
#' coredata(x)
#' xcoredata(x)
#' 
coredata.xts <- function(x, fmt=FALSE, ...) {
  x.attr <- attributes(x)

  if(is.character(fmt)) {
    tformat(x) <- fmt
    fmt <- TRUE
  }
  
  if(length(x) > 0 && fmt) {
    if(!is.null(tformat(x))) {
      x.attr$dimnames <- list(format(index(x), format=tformat(x)),
                              dimnames(x)[[2]])
      tformat(x) <- NULL  # remove before printing
    } else {
      x.attr$dimnames <- list(format(index(x)),dimnames(x)[[2]])
    }
      #attributes not to be kept
    original.attr <- x.attr[!names(x.attr) %in%
                          c('dim','dimnames')]
    if(is.null(dim(x))) {
      xx <- structure(coredata(x), names=x.attr$dimnames[[1]])
    } else {
      xx <- structure(coredata(x), dim=dim(x), dimnames=x.attr$dimnames) 
    }
    for(i in names(original.attr)) {
      attr(xx,i) <- NULL
    }
    return(xx)
  }

  if(length(x) == 0) {
    xx <- NextMethod(x)
    attr(xx, ".indexCLASS") <- NULL
    attr(xx, "tclass") <- NULL
    # Remove tz attrs (object created before 0.10-3)
    attr(xx, ".indexTZ") <- NULL
    attr(xx, "tzone") <- NULL
    return(xx)
  } else 
  return(.Call(C_coredata_xts, x))

}

`xcoredata.default` <-
function(x,...) {
  x.attr <- attributes(x)
  original.attr <- x.attr[!names(x.attr) %in%
                          c('dim','dimnames')]
  original.attr
}

#' @rdname coredata.xts
`xcoredata` <-
function(x,...) {
  UseMethod('xcoredata')
}

#' @rdname coredata.xts
`xcoredata<-` <- function(x,value) {
  UseMethod('xcoredata<-')
}

`xcoredata<-.default` <- function(x,value) {
  if(is.null(value)) {
    return(coredata(x))
  } else {
    for(att in names(value)) {
      if(!att %in% c('dim','dimnames'))
        attr(x,att) <- value[[att]]
    }
  return(x)
  }
}


#' Extract and Replace xts Attributes
#' 
#' Extract and replace non-core \code{xts} attributes.
#' 
#' Since \code{xts} objects are S3 objects with special attributes, a method is
#' necessary to properly assign and view the user-added attributes.
#' 
#' A call to \code{attributes} from the \pkg{base} package will return all
#' attributes, including those specific to the \code{xts} class.
#' 
#' @param x an xts object
#' @param user logical; should user-defined attributes be returned?  The
#' default of \code{NULL} returns all \code{xts} attributes.
#' @param value a list of new name=value attributes
#' 
#' @return A named list of user settable attributes.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{attributes}}
#'
#' @keywords utilities
#' @examples
#' 
#' x <- xts(matrix(1:(9*6),nc=6),
#'          order.by=as.Date(13000,origin="1970-01-01")+1:9,
#'          a1='my attribute')
#' 
#' xtsAttributes(x)
#' xtsAttributes(x) <- list(a2=2020)
#' 
#' xtsAttributes(x)
#' xtsAttributes(x) <- list(a1=NULL)
#' xtsAttributes(x)
#' 
`xtsAttributes` <-
function(x, user=NULL) {
  # get all additional attributes not standard to xts object
  #stopifnot(is.xts(x))
  rm.attr <- c('dim','dimnames','index','class','names')
  x.attr <- attributes(x)

  if(is.null(user)) {
  # Both xts and user attributes
    rm.attr <- c(rm.attr,'.CLASS','.CLASSnames','.ROWNAMES', '.indexCLASS', '.indexFORMAT', '.indexTZ', 'tzone', 'tclass')
    xa <- x.attr[!names(x.attr) %in% rm.attr]
  }
  else
  if(user) {
  # Only user attributes
    rm.attr <- c(rm.attr,'.CLASS','.CLASSnames','.ROWNAMES', '.indexCLASS', '.indexFORMAT','.indexTZ','tzone','tclass',
                 x.attr$.CLASSnames)
    xa <- x.attr[!names(x.attr) %in% rm.attr]
  } else {
  # Only xts attributes
    xa <- x.attr[names(x.attr) %in% x.attr$.CLASSnames]
  }

  if(length(xa) == 0) return(NULL)
  xa
}

#' @rdname xtsAttributes
`xtsAttributes<-` <-
function(x,value) {
  UseMethod('xtsAttributes<-')
}

`xtsAttributes<-.xts` <-
function(x,value) {
  if(is.null(value)) {
    for(nm in names(xtsAttributes(x))) {
      attr(x,nm) <- NULL
    }
  } else
  for(nv in names(value)) {
    if(!nv %in% c('dim','dimnames','index','class','.CLASS','.ROWNAMES','.CLASSnames'))
      attr(x,nv) <- value[[nv]]
  }
  # Remove tz attrs (object created before 0.10-3)
  attr(x, ".indexTZ") <- NULL
  attr(x, "tzone") <- NULL
  # Remove index class attrs (object created before 0.10-3)
  attr(x, ".indexCLASS") <- NULL
  attr(x, "tclass") <- NULL
  # Remove index format attr (object created before 0.10-3)
  attr(x, ".indexFORMAT") <- NULL
  x
}
