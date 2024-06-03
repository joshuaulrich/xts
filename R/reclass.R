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


#' @rdname reclass
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


#' Convert Objects to xts and Back to Original Class
#' 
#' Functions to convert objects of arbitrary classes to xts and then back to
#' the original class, without losing any attributes of the original class.
#' 
#' A simple and reliable way to convert many different objects into a uniform
#' format for use within \R.
#' 
#' `try.xts()` and `reclass()` are functions that enable external developers
#' access to the reclassing tools within \pkg{xts} to help speed development of
#' time-aware functions, as well as provide a more robust and seemless end-user
#' experience, regardless of the end-user's choice of data-classes.
#' 
#' `try.xts()` calls `as.xts()` internally. See [`as.xts()`] for available xts
#' methods and arguments for each coercible class. Since it calls `as.xts()`,
#' you can add custom attributes as `name = value` pairs in the same way. But
#' these custom attributes will not be copied back to the original object when
#' `reclass()` is called.
#' 
#' The `error` argument can be a logical value indicating whether an error
#' should be thrown (or fail silently), a character string allowing for custom
#' error error messages, or a function of the form `f(x, ...)` that will be
#' called if the conversion fails.
#' 
#' `reclass()` converts an object created by `try.xts()` back to its original
#' class with all the original attributes intact (unless they were changed
#' after the object was converted to xts). The `match.to` argument allows you
#' copy the index attributes ([`tclass`], [`tformat`], and [`tzone`]) and
#' [`xtsAttributes()`] from another xts object to the result. `match.to` must
#' be an xts object with an index value for every observation in `x`.
#' 
#' `Reclass()` is designed for top-level use, where it is desirable to have
#' the object returned from an arbitrary function in the same class as the
#' object passed in. Most functions in \R are not designed to return objects
#' matching the original object's class. It attempts to handle conversion and
#' reconversion transparently but it requires the original object must be
#' coercible to xts, the result of the function must have the same number of
#' rows as the input, and the object to be converted/reclassed must be the
#' first argument to the function being wrapped. Note that this function
#' hasn't been tested for robustness.
#' 
#' See the accompanying vignette for more details on the above usage.
#' 
#' @param x Data object to convert. See details for supported types.
#' @param match.to An xts object whose attributes will be copied to the result.
#' @param error Error handling option. See Details.
#' @param \dots Additional parameters or attributes.
#' 
#' @return `try.xts()` returns an xts object when conversion is successful.
#' The `error` argument controls the function's behavior when conversion fails.
#' 
#' `Reclass()` and `reclass()` return the object as its original class, as
#' specified by the 'CLASS' attribute.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`as.xts()`]
#' 
#' @keywords utilities
#' @examples
#' 
#' a <- 1:10
#' 
#' # fails silently, the result is still an integer vector
#' try.xts(a, error = FALSE)
#' 
#' # control the result with a function
#' try.xts(a, error = function(x, ...) { "I'm afraid I can't do that." })
#' 
#' z <- zoo(1:10, timeBasedSeq("2020-01-01/2020-01-10"))
#' x <- try.xts(z)  # zoo to xts
#' str(x)
#' str(reclass(x))  # reclass back to zoo
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

#' @rdname reclass
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
#' Extraction and replacement functions to access the xts '.CLASS' attribute.
#' The '.CLASS' attribute is used by `reclass()` to transform an xts object
#' back to its original class.
#' 
#' This is meant for use in conjunction with `try.xts()` and `reclass()` and is
#' is not intended for daily use. While it's possible to interactively coerce
#' objects to other classes than originally derived from, it's likely to cause
#' unexpected behavior. It is best to use the usual `as.xts()` and other
#' classes' `as` methods.
#' 
#' @param x An xts object.
#' @param value The new value to assign to the '.CLASS' attribute.
#' 
#' @return Called for its side-effect of changing the '.CLASS' attribute.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @seealso [`as.xts()`], [`reclass()`]
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
