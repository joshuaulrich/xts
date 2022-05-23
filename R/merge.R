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

  .times <- .External('number_of_cols', ..., PACKAGE="xts")
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

  x <- .External('mergeXts',
            all=all[1:2],
            fill=fill,
            setclass=setclass,
            symnames=symnames,
            suffixes=suffixes,
            retside=retside,
            env=new.env(),
            tzone=tzone,
            check.names=check.names,
            ..., PACKAGE="xts")
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
