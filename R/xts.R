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


# xts core functions
#   additional methods are in correspondingly named .R files
#   current conversions include:
#     timeSeries, its, irts, ts, matrix, data.frame, and zoo
#     MISSING: tis, fame
#
#  this file includes the main xts constructor as well as the reclass
#  function.
#
#  xts methods (which match foreign conversion methods in other files)
#  are also defined below


# xts() index attribute precedence should be:
#   1. .index* value (e.g. .indexTZ)  # backward compatibility
#   2. t* value (e.g. tzone)          # current function to override index attribute
#   3. attribute on order.by          # overridden by either 2 above
#
# Do we always have to override the value of an existing tzone on the index
# because the default value is Sys.getenv("TZ")?
#
# .xts() index attribute precedence is similar. But we cannot override tclass
# because it's a formal argument with a specific default. Historically .xts()
# has always set the tclass to POSIXct by default, whether or not the 'index'
# argument already had a tclass attribute.

`xts` <-
function(x=NULL,
         order.by=index(x),
         frequency=NULL,
         unique=TRUE,
         tzone=Sys.getenv("TZ"),
         ...)
{
  if(is.null(x) && missing(order.by))
    return(.xts(NULL, integer()))
 
  if(!timeBased(order.by))
    stop("order.by requires an appropriate time-based object")

  #if(NROW(x) != length(order.by))
  if(NROW(x) > 0 && NROW(x) != length(order.by))
    stop("NROW(x) must match length(order.by)")

  order.by_ <- order.by  # make local copy and don't change order.by

  if(inherits(order.by, 'Date')) { 
    # convert to GMT POSIXct if specified
    order.by_ <- .POSIXct(unclass(order.by) * 86400, tz = "UTC")
  }

  if(!isOrdered(order.by_, strictly = !unique)) {
    indx <- order(order.by_)
    if(!is.null(x)) {
      if(NCOL(x) > 1 || is.matrix(x) || is.data.frame(x)) {
        x <- x[indx,,drop=FALSE]
      } else x <- x[indx]
    }
    order.by_ <- order.by_[indx]
  }

  if(is.null(x)) {
    x <- numeric(0)
  } else if (is.list(x)) {
    # list or data.frame
    if (is.data.frame(x)) {
      x <- as.matrix(x)
    } else {
      stop("cannot convert lists to xts objects")
    }
  } else if (NROW(x) > 0) {
    x <- as.matrix(x)
  }
  # else 'x' is a zero-length vector. Do not *add* dims via as.matrix().
  # It's okay if 'x' already has dims.

  if(inherits(order.by, "dates")) {
    fmt <- "%m/%d/%y"
    if(inherits(order.by, "chron")) {
      fmt <- paste0("(", fmt, " %H:%M:%S)")
    }
    order.by_ <- strptime(as.character(order.by_), fmt)  # POSIXlt
  }
  index <- as.numeric(as.POSIXct(order.by_))

  if(any(!is.finite(index)))
    stop("'order.by' cannot contain 'NA', 'NaN', or 'Inf'")

  # process index attributes
  ctor.call <- match.call(expand.dots = TRUE)

  tformat. <- attr(order.by, "tformat")
  if(hasArg(".indexFORMAT")) {
    warning(sQuote(".indexFORMAT"), " is deprecated, use tformat instead.")
    tformat. <- eval.parent(ctor.call$.indexFORMAT)
  } else if(hasArg("tformat")) {
    tformat. <- eval.parent(ctor.call$tformat)
  }

  tclass. <- attr(order.by, "tclass")
  if(hasArg(".indexCLASS")) {
    warning(sQuote(".indexCLASS"), " is deprecated, use tclass instead.")
    tclass. <- eval.parent(ctor.call$.indexCLASS)
  } else if(hasArg("tclass")) {
    tclass. <- eval.parent(ctor.call$tclass)
  } else if(is.null(tclass.)) {
    tclass. <- class(order.by)
    if(inherits(order.by, "POSIXt")) {
      #tclass. <- tclass.[tclass. != "POSIXt"]
    }
  }

  tzone. <- tzone  # default Sys.getenv("TZ")
  if(hasArg(".indexTZ")) {
    warning(sQuote(".indexTZ"), " is deprecated, use tzone instead.")
    tzone. <- eval.parent(ctor.call$.indexTZ)
  } else if(hasArg("tzone")) {
    tzone. <- eval.parent(ctor.call$tzone)
  } else {
    # no tzone argument
    if(inherits(order.by, "timeDate")) {
      tzone. <- order.by@FinCenter
    } else if(!is.null(attr(order.by, "tzone"))) {
      tzone. <- attr(order.by, "tzone")
    }
  }
  if(isClassWithoutTZ(object = order.by)) {
      if((hasArg(".indexTZ") || hasArg("tzone")) && !isUTC(tzone.)) {
        warning(paste(sQuote('tzone'),"setting ignored for ",
                      paste(class(order.by), collapse=", "), " indexes"))
      }
      tzone. <- "UTC" # change anything in isUTC() to UTC
  }
  # xts' tzone must only contain one element (POSIXlt tzone has 3)
  tzone. <- tzone.[1L]

  x <- structure(.Data = x,
                 index = structure(index, tzone = tzone.,
                                   tclass = tclass., tformat = tformat.),
                 class=c('xts','zoo'),
                 ...)

  # remove any index attributes that came through '...'
  index.attr <- c(".indexFORMAT", "tformat",
                  ".indexCLASS", "tclass",
                  ".indexTZ", "tzone")
  for(iattr in index.attr) {
    attr(x, iattr) <- NULL
  }

  if(!is.null(attributes(x)$dimnames[[1]]))
    # this is very slow if user adds rownames, but maybe that is deserved :)
    dimnames(x) <- dimnames(x) # removes row.names
  x
}

`.xts` <-
function(x=NULL, index, tclass=c("POSIXct","POSIXt"),
         tzone=Sys.getenv("TZ"),
         check=TRUE, unique=FALSE, ...) {
  if(check) {
    if( !isOrdered(index, increasing=TRUE, strictly=unique) )
      stop('index is not in ',ifelse(unique, 'strictly', ''),' increasing order')
  }

  index_out <- index

  if(!is.numeric(index) && timeBased(index))
    index_out <- as.numeric(as.POSIXct(index))
  if(!is.null(x) && NROW(x) != length(index))
    stop("index length must match number of observations")
  if(any(!is.finite(index_out)))
    stop("'index' cannot contain 'NA', 'NaN', or 'Inf'")

  if(!is.null(x)) {
    if(!is.matrix(x))
      x <- as.matrix(x)
  } else
  if(length(x) == 0 && !is.null(x)) {
    x <- vector(storage.mode(x))
  } else x <- numeric(0)

  # process index attributes
  ctor.call <- match.call(expand.dots = TRUE)

  tformat. <- attr(index, "tformat")
  if(hasArg(".indexFORMAT")) {
    warning(sQuote(".indexFORMAT"), " is deprecated, use tformat instead.")
    tformat. <- eval.parent(ctor.call$.indexFORMAT)
  } else if(hasArg("tformat")) {
    tformat. <- eval.parent(ctor.call$tformat)
  }

  tclass. <- tclass  # default POSIXct
  if(hasArg(".indexCLASS")) {
    warning(sQuote(".indexCLASS"), " is deprecated, use tclass instead.")
    tclass. <- eval.parent(ctor.call$.indexCLASS)
  } else if(hasArg("tclass")) {
    tclass. <- eval.parent(ctor.call$tclass)
  } else {
    # no tclass argument
    tclass. <- attr(index, "tclass")
    if(is.null(tclass.) && timeBased(index)) {
      tclass. <- class(index)
    } else {
      if(!identical(tclass., c("POSIXct", "POSIXt"))) {
        # index argument has 'tclass' attribute but it will be ignored
        # FIXME:
        # This warning causes errors in dependencies (e.g. portfolioBacktest,
        # when the warning is thrown from PerformanceAnalytics). Reinstate this
        # warning after fixing downstream packages.
        #      warning("the index tclass attribute is ", index.class,
        #              " but will be changed to (POSIXct, POSIXt)")
        tclass. <- tclass  # default POSIXct
      }
    }
  }

  tzone. <- tzone  # default Sys.getenv("TZ")
  if(hasArg(".indexTZ")) {
    warning(sQuote(".indexTZ"), " is deprecated, use tzone instead.")
    tzone. <- eval.parent(ctor.call$.indexTZ)
  } else if(hasArg("tzone")) {
    tzone. <- eval.parent(ctor.call$tzone)
  } else {
    # no tzone argument
    if(inherits(index, "timeDate")) {
      tzone. <- index@FinCenter
    } else if(!is.null(attr(index, "tzone"))) {
      tzone. <- attr(index, "tzone")
    }
  }
  if(isClassWithoutTZ(object = index)) {
      if((hasArg(".indexTZ") || hasArg("tzone")) && !isUTC(tzone.)) {
        warning(paste(sQuote('tzone'),"setting ignored for ",
                      paste(class(index), collapse=", "), " indexes"))
      }
      tzone. <- "UTC" # change anything in isUTC() to UTC
  }
  # xts' tzone must only contain one element (POSIXlt tzone has 3)
  tzone <- tzone[1L]

  xx <- .Call(C_add_xtsCoreAttributes, x, index_out, tzone., tclass.,
              c('xts','zoo'), tformat.)

  # remove any index attributes that came through '...'
  # and set any user attributes (and/or dim, dimnames, etc)
  dots.names <- eval(substitute(alist(...)))
  if(length(dots.names) > 0L) {
    dot.attrs <- list(...)
    drop.attr <- c(".indexFORMAT", "tformat", ".indexCLASS", ".indexTZ")
    dot.attrs[drop.attr] <- NULL
    attributes(xx) <- c(attributes(xx), dot.attrs)
  }

  # ensure there are no rownames (they may have come though dimnames)
  rn <- dimnames(xx)[[1]]
  if(!is.null(rn)) {
    attr(xx, '.ROWNAMES') <- rn
    dimnames(xx)[1] <- list(NULL)
  }

  xx
}

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

#`reclass` <- reclass2

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

`CLASS<-` <-
function(x,value) {
  UseMethod("CLASS<-")
}

`CLASS<-.xts` <-
function(x,value) {
  attr(x,".CLASS") <- value
  x
}

`is.xts` <-
function(x) {
  inherits(x,'xts') &&
  is.numeric(.index(x)) &&
  !is.null(tclass(x))
}

`as.xts` <-
function(x,...) {
  UseMethod('as.xts')
}

#as.xts.default <- function(x, ...) x

`re.xts` <-
function(x,...) {
  # simply return the object
  return(x)
}

`as.xts.xts` <-
function(x,...,.RECLASS=FALSE) {
  # Cannot use 'zoo()' on objects of class 'zoo' or '.CLASS' (etc.?)
  # Is the equivalent of a 'coredata.xts' needed? - jmu
  #yy <- coredata(x)
  #attr(yy, ".CLASS") <- NULL
  # using new coredata.xts method - jar
  if(length(x) == 0 && (!is.null(index(x)) && length(index(x))==0))
    return(x)
  if(.RECLASS) {
  xx <- xts(coredata(x),
            order.by=index(x),
            .CLASS='xts',
            ...)
  } else {
  xx <- xts(coredata(x),
            order.by=index(x),
            ...)
  }
  xx
}

`xts.to.xts` <-
function(x,...) {
  return(x)
}
