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


#' @inheritParams as.xts
#' @param error error handling option. See Details.
#' @rdname as.xts
#' @aliases use.xts
try.xts <- function(x, ..., error=TRUE)
{
  if(is.xts(x)) {
    #attr(x,'.RECLASS') <- FALSE
    return(x)
  }

  xx <- try(as.xts(x,..., .RECLASS=TRUE),silent=TRUE)
  if(inherits(xx,'try-error')) {
    if(is.character(error)) {
      stop(error)
    } else  
    if(is.function(error)) {
      return(error(x, ...))
    } else 
    if(error) {
      stop(gsub('\n','',xx))
    } else {
      return(x) 
    }
  } else {
    # made positive: now test if needs to be reclassed
    structure(xx, .RECLASS=TRUE)
  }
}
use.xts <- try.xts

#' Convert Object To And From Class xts
#' 
#' Conversion functions to coerce data objects of arbitrary classes to class
#' \code{xts} and back, without losing any attributes of the original format.
#' 
#' A simple and reliable way to convert many different objects into a uniform
#' format for use within .
#' 
#' It is possible with a call to \code{as.xts} to convert objects of class
#' \code{timeSeries}, \code{ts}, \code{irts}, \code{matrix}, \code{data.frame},
#' and \code{zoo}.
#' 
#' \code{xtsible} safely checks whether an object can be converted to an
#' \code{xts} object; returning TRUE on success and FALSE otherwise.
#' 
#' The help file \code{as.xts} lists all available xts methods and arguments
#' specific to each coercible type.
#' 
#' Additional name=value pairs may be passed to the function to be added to the
#' new object. A special print.xts method will assure that the attributes are
#' hidden from view, but will be available via 's standard \code{attr}
#' function, as well as the \code{xtsAttributes} function.
#' 
#' The returned object will preserve all relevant attribute/slot data within
#' itself, allowing for temporary conversion to use zoo and xts compatible
#' methods. A call to \code{reclass} returns the object to its original class,
#' with all original attributes intact - unless otherwise changed.
#' 
#' It should be obvious, but any attributes added via the \dots{} argument will
#' not be carried back to the original data object, as there would be no
#' available storage slot/attribute.
#' 
#' \code{Reclass} is designed for top-level use, where it is desirable to have
#' the object returned from an arbitrary function in the same class as the
#' object passed in.  Most functions within are not designed to return objects
#' matching the original object's class.  While this tool is highly
#' experimental at present, it attempts to handle conversion and reconversion
#' transparently.  The caveats are that the original object must be coercible
#' to \code{xts}, the returned object must be of the same row length as the
#' original object, and that the object to reconvert to is the first argument
#' to the function being wrapped.
#' 
#' \code{try.xts} and \code{reclass} are functions that enable external
#' developers access to the reclassing tools within \pkg{xts} to help speed
#' development of time-aware functions, as well as provide a more robust and
#' seemless end-user experience, regardless of the end-user's choice of
#' data-classes.
#' 
#' The \code{error} argument to try.xts accepts a logical value, indicating
#' where an error should be thrown, a character string allowing for custom
#' error messages to be displayed, or a function of the form \code{f(x, ...)},
#' to be called upon construction error.
#' 
#' See the accompanying vignette for more details on the above usage and the
#' package in general.
#' 
#' @param x data object to convert. See details for supported types
#' @param match.to \code{xts} object whose attributes will be passed to
#' \code{x}
#' @param error error handling option. See Details.
#' @param \dots additional parameters or attributes
#' 
#' @return An S3 object of class \code{xts}.
#' 
#' In the case of \code{Reclass} and \code{reclass}, the object returned will
#' be of the original class as identified by \code{CLASS}.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{xts}},\code{\link{as.xts}}
#' 
#' @keywords utilities
#' 
`reclass` <-
function(x, match.to, error=FALSE, ...) {
  if(!missing(match.to) && is.xts(match.to)) {
    if(NROW(x) != length(.index(match.to)))
      if(error) {
        stop('incompatible match.to attibutes')
      } else return(x)

    if(!is.xts(x)) {
      x <- .xts(coredata(x), .index(match.to),
                tclass = tclass(match.to),
                tzone = tzone(match.to),
                tformat = tformat(match.to))
    }
    attr(x, ".CLASS") <- CLASS(match.to)
    xtsAttributes(x) <- xtsAttributes(match.to)
    tclass(x) <- tclass(match.to)
    tformat(x) <- tformat(match.to)
    tzone(x) <- tzone(match.to)
  }
  oldCLASS <- CLASS(x)
  # should this be is.null(oldCLASS)?
  if(length(oldCLASS) > 0 && !inherits(oldClass,'xts')) {  
    if(!is.null(dim(x))) {
      if(!is.null(attr(x,'.ROWNAMES'))) {
        # rownames<- (i.e. dimnames<-.xts) will not set row names
        # force them directly
        attr(x, "dimnames")[[1]] <- attr(x,'.ROWNAMES')[1:NROW(x)]
      }
    }
    attr(x,'.ROWNAMES') <- NULL
    #if(is.null(attr(x,'.RECLASS')) || attr(x,'.RECLASS')) {#should it be reclassed?
    if(isTRUE(attr(x,'.RECLASS'))) {#should it be reclassed?
      #attr(x,'.RECLASS') <- NULL
      do.call(paste('re',oldCLASS,sep='.'),list(x))
    } else {
      #attr(x,'.RECLASS') <- NULL
      x
    }
  } else {
    #attr(x,'.RECLASS') <- NULL
    x
  }
}


#' @rdname as.xts
#' @aliases use.reclass
Reclass <- function(x) {
  xx <- match.call()
  xxObj <- eval.parent(parse(text=all.vars(xx)[1]), 1)
  inObj <- try.xts(xxObj, error=FALSE)
  xx <- eval(match.call()[[-1]])
  reclass(xx, inObj)
}
use.reclass <- Reclass


#' Extract and Set .CLASS Attribute
#' 
#' Simple extraction and replacement function to access \code{xts} .CLASS
#' attribute.  The .CLASS attribute is used by \code{reclass} to transform an
#' \code{xts} object back to its original class.
#' 
#' It is not recommended that CLASS be called in daily use.  While it may be
#' possible to coerce objects to other classes than originally derived from,
#' there is little, if any, chance that the \code{reclass} function will
#' perform as expected.
#' 
#' It is best to use the traditional \code{as} methods.
#' 
#' @param x an xts object
#' @param value the new .CLASS value to assign
#' 
#' @return Called for its side-effect of changing the .CLASS attribute
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso \code{\link{as.xts}},\code{\link{reclass}}
#' 
#' @keywords utilities
`CLASS` <-
function(x) {
  cl <- attr(x,'.CLASS')

  if(!is.null(cl)) {
    attr(cl, 'class') <- 'CLASS'
    return(cl)
  }

  return(NULL)
}

`print.CLASS` <-
function(x,...) {
  cat(paste("previous class:",x),"\n")
}

#' @rdname CLASS
`CLASS<-` <-
function(x,value) {
  UseMethod("CLASS<-")
}

`CLASS<-.xts` <-
function(x,value) {
  attr(x,".CLASS") <- value
  x
}
