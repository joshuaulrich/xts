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


#' Merge xts Objects
#' 
#' Perform merge operations on xts objects by time index.
#' 
#' This xts method is compatible with [zoo's merge() method][zoo::merge.zoo] but implemented almost
#' entirely in C-level code for efficiency.
#' 
#' The function can perform all common database join operations along the time
#' index by setting 'join' to one of the values below. Note that 'left' and
#' 'right' are only implemented for two objects.
#' 
#' * outer: full outer (all rows in all objects)
#' * inner: only rows with common indexes in all objects
#' * left: all rows in the first object, and rows from the second object that
#'   have the same index as the first object
#' * right: all rows in the second object, and rows from the first object that
#'   have the same index as the second object
#'
#' The above join types can also be accomplished by setting 'all' to one of the
#' values below.
#' 
#' * outer: `all = TRUE` or `all = c(TRUE, TRUE)`
#' * inner: `all = FALSE` or `all = c(FALSE, FALSE)`
#' * left: `all = c(TRUE, FALSE)`
#' * right: `all = c(FALSE, TRUE)`
#'
#' The result will have the timezone of the leftmost argument if available. Use
#' the 'tzone' argument to override the default behavior.
#' 
#' When `retclass = NULL` the joined objects will be split and reassigned
#' silently back to the original environment they are called from. This is for
#' backward compatibility with zoo, but unused by xts. When `retclass = FALSE`
#' the object will be stripped of its class attribute. This is for internal use.
#' 
#' See the examples in order to join using an 'all' argument that is the same
#' arguments to join, like you can do with `merge.zoo()`.
#' 
#' @param \dots One or more xts objects, or objects coercible to class xts.
#' @param all A logical vector indicating merge type.
#' @param fill Values to be used for missing elements.
#' @param suffixes Suffix to be added to merged column names.
#' @param join Type of database join. One of 'outer', 'inner', 'left', or 'right'.
#' @param retside Which side of the merged object should be returned (2-case only)?
#' @param retclass Either a logical value indicating whether the result should
#'   have a 'class' attribute, or the name of the desired class for the result.
#' @param tzone Time zone to use for the merged result.
#' @param drop Not currently used.
#' @param check.names Use [`make.names()`] to ensure column names are vaild \R
#'   object names?
#' 
#' @return A new xts object containing the appropriate elements of the
#' objects passed in to be merged.
#' 
#' @note This is a highly optimized merge, specifically designed for ordered
#' data. The only supported merging is based on the underlying time index.
#' 
#' @author Jeffrey A. Ryan
#' 
#' @references Merge Join Discussion:
#' <https://learn.microsoft.com/en-us/archive/blogs/craigfr/merge-join>
#' 
#' @keywords manip utilities
#' @examples
#' 
#' (x <- xts(4:10, Sys.Date()+4:10))
#' (y <- xts(1:6, Sys.Date()+1:6))
#' 
#' merge(x,y)
#' merge(x,y, join='inner')
#' merge(x,y, join='left')
#' merge(x,y, join='right')
#' 
#' merge.zoo(zoo(x),zoo(y),zoo(x), all=c(TRUE, FALSE, TRUE))
#' merge(merge(x,x),y,join='left')[,c(1,3,2)]
#' 
#' # zero-width objects (only index values) can be used
#' xi <- xts( , index(x))
#' merge(y, xi)
#' 
merge.xts <- function(..., 
                     all=TRUE,
                     fill=NA,
                     suffixes=NULL,
                     join="outer",
                     retside=TRUE,
                     retclass="xts",
                     tzone=NULL,
                     drop=NULL,
                     check.names=NULL) {
  if(is.null(check.names)) {
    check.names <- TRUE
  }
  if(is.logical(retclass) && !retclass) {
    setclass=FALSE
  } else setclass <- TRUE

  fill.fun <- NULL
  if(is.function(fill)) {
    fill.fun <- fill 
    fill <- NA
  } 
  
  # as.list(substitute(list(...)))  # this is how zoo handles colnames - jar
  mc <- match.call(expand.dots=FALSE)
  dots <- mc$...
  if(is.null(suffixes)) {
    syms <- names(dots)
    if(is.null(syms)) {
      # Based on makeNames() in merge.zoo()
      syms <- substitute(alist(...))[-1L]
      nm <- names(syms)
      fixup <- if (is.null(nm)) seq_along(syms) else !nzchar(nm)
      dep <- sapply(syms[fixup], function(x) deparse(x, nlines = 1L))
      if(is.null(nm)) {
        nm <- dep
      } else if(any(fixup)) {
        nm[fixup] <- dep
      }
      syms <- nm
    } else {
      have.symnames <- nzchar(syms)
      if(any(!have.symnames)) {
        syms[!have.symnames] <- as.character(dots[!have.symnames])
      }
    }
  } else
  if(length(suffixes) != length(dots)) {
    warning("length of suffixes and does not match number of merged objects")
    syms <- as.character(dots)
    # should we ignore suffixes here?
    #suffixes <- NULL
  } else {
    syms <- as.character(suffixes)
  }

  .times <- .External(C_number_of_cols, ...)
  # moved call to make.names inside of mergeXts/do_merge_xts
  symnames <- rep(syms, .times)
  suffixes <- rep(suffixes, .times)

  if(length(dots) == 1) {
    # this is for compat with zoo; one object AND a name
    if(!is.null(names(dots))) {
      x <- list(...)[[1]]
      if(is.null(colnames(x))) 
        colnames(x) <- symnames
      return(x)
    }
  }

  if( !missing(join) ) { 
    # join logic applied to index:
    # inspired by: http://blogs.msdn.com/craigfr/archive/2006/08/03/687584.aspx
    #   
    #  (full) outer - all cases, equivelant to all=c(TRUE,TRUE)
    #         left  - all x,    &&  y's that match x
    #         right - all  ,y   &&  x's that match y
    #         inner - only x and y where index(x)==index(y)
    all <- switch(pmatch(join,c("outer","left","right","inner")),
                    c(TRUE,  TRUE ), #  outer
                    c(TRUE,  FALSE), #  left
                    c(FALSE, TRUE ), #  right
                    c(FALSE, FALSE)  #  inner
                 )   
    if( length(dots) > 2 ) {
      all <- all[1]
      warning("'join' only applicable to two object merges")
    }
  }

  if( length(all) != 2 ) {
    if( length(all) > 2 )
      warning("'all' must be of length two")
    all <- rep(all[1], 2)
  }
  if( length(dots) > 2 )
    retside <- TRUE
  if( length(retside) != 2 ) 
    retside <- rep(retside[1], 2)

  x <- .External(C_mergeXts,
            all=all[1:2],
            fill=fill,
            setclass=setclass,
            symnames=symnames,
            suffixes=suffixes,
            retside=retside,
            env=new.env(),
            tzone=tzone,
            check.names=check.names,
            ...)
  if(!is.logical(retclass) && retclass != 'xts') {
    asFun <- paste("as", retclass, sep=".")
    if(!exists(asFun)) {
      warning(paste("could not locate",asFun,"returning 'xts' object instead"))
      return(x)
    }
    xx <- try(do.call(asFun, list(x)))
    if(!inherits(xx,'try-error')) {
      return(xx)
    }
  }
  if(!is.null(fill.fun)) {
    fill.fun(x)
  } else
  return(x)
}


#' @rdname merge.xts
cbind.xts <- function(..., all=TRUE, fill=NA, suffixes=NULL) {
  merge.xts(..., all=all, fill=fill, suffixes=suffixes)
}

.merge.xts.scalar <- function(x, length.out, ...) {
  if( length.out == 0)
    return(vector(storage.mode(x), 0))
  if( length(x) == 1 )
    return(matrix(rep(x, length.out=length.out)))
  if( NROW(x) == length.out )
    return(x)
  stop("improper length of one or more arguments to merge.xts")
}
