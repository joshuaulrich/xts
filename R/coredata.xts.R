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


coredata.xts <- function(x, fmt=FALSE, ...) {
  x.attr <- attributes(x)

  if(is.character(fmt)) {
    indexFormat(x) <- fmt
    fmt <- TRUE
  }
  
  if(length(x) > 0 && fmt) {
    if(!is.null(indexFormat(x))) {
      x.attr$dimnames <- list(format(index(x), format=indexFormat(x)),
                              dimnames(x)[[2]])
      indexFormat(x) <- NULL  # remove before printing
    } else {
      x.attr$dimnames <- list(format(index(x)),dimnames(x)[[2]])
    }
      #attributes not to be kept
    original.attr <- x.attr[!names(x.attr) %in%
                          c('dim','dimnames')]
    if(is.null(dim(x))) {
      xx <- structure(unclass(x), names=x.attr$dimnames[[1]])
    } else {
      xx <- structure(unclass(x), dim=dim(x), dimnames=x.attr$dimnames) 
    }
    for(i in names(original.attr)) {
      attr(xx,i) <- NULL
    }
    return(xx)
  }

  if(length(x) == 0) {
    return(vector(storage.mode(x)))
  } else 
  return(.Call("coredata", x, PACKAGE="xts"))

}

`xcoredata.default` <-
function(x,...) {
  x.attr <- attributes(x)
  original.attr <- x.attr[!names(x.attr) %in%
                          c('dim','dimnames')]
  original.attr
}


`xcoredata` <-
function(x,...) {
  UseMethod('xcoredata')
}

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

`xtsAttributes` <-
function(x, user=NULL) {
  # get all additional attributes not standard to xts object
  #stopifnot(is.xts(x))
  rm.attr <- c('dim','dimnames','index','class','names')
  x.attr <- attributes(x)

  if(is.null(user)) {
  # Both xts and user attributes
    rm.attr <- c(rm.attr,'.CLASS','.CLASSnames','.ROWNAMES', '.indexCLASS', '.indexFORMAT', '.indexTZ')
    xa <- x.attr[!names(x.attr) %in% rm.attr]
  }
  else
  if(user) {
  # Only user attributes
    rm.attr <- c(rm.attr,'.CLASS','.CLASSnames','.ROWNAMES', '.indexCLASS', '.indexFORMAT','.indexTZ', 
                 x.attr$.CLASSnames)
    xa <- x.attr[!names(x.attr) %in% rm.attr]
  } else {
  # Only xts attributes
    xa <- x.attr[names(x.attr) %in% x.attr$.CLASSnames]
  }

  if(length(xa) == 0) return(NULL)
  xa
}

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
    if(!nv %in% c('dim','dimnames','index','class','.CLASS','.ROWNAMES','.CLASSnames',
                  '.indexCLASS','.indexFORMAT','.indexTZ'))
      attr(x,nv) <- value[[nv]]
  }
  x
}
