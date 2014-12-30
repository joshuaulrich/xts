
modify.args <- function(formals, arglist, ..., dots=FALSE)
{
  # modify.args function from quantstrat
  
  # avoid evaluating '...' to make things faster
  dots.names <- eval(substitute(alist(...)))
  
  if(missing(arglist))
    arglist <- NULL
  arglist <- c(arglist, dots.names)
  
  # see 'S Programming' p. 67 for this matching
  
  # nothing to do if arglist is empty; return formals
  if(!length(arglist))
    return(formals)
  
  argnames <- names(arglist)
  if(!is.list(arglist) && !is.null(argnames) && !any(argnames == ""))
    stop("'arglist' must be a *named* list, with no names == \"\"")
  
  .formals  <- formals
  onames <- names(.formals)
  
  pm <- pmatch(argnames, onames, nomatch = 0L)
  #if(any(pm == 0L))
  #    message(paste("some arguments stored for", fun, "do not match"))
  names(arglist[pm > 0L]) <- onames[pm]
  .formals[pm] <- arglist[pm > 0L]
  
  # include all elements from arglist if function formals contain '...'
  if(dots && !is.null(.formals$...)) {
    dotnames <- names(arglist[pm == 0L])
    .formals[dotnames] <- arglist[dotnames]
    #.formals$... <- NULL  # should we assume we matched them all?
  }
  .formals
}

# This is how it is used in quantstrat in applyIndicators()
# # replace default function arguments with indicator$arguments
# .formals <- formals(indicator$name)
# .formals <- modify.args(.formals, indicator$arguments, dots=TRUE)
# # now add arguments from parameters
# .formals <- modify.args(.formals, parameters, dots=TRUE)
# # now add dots
# .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
# # remove ... to avoid matching multiple args
# .formals$`...` <- NULL
# 
# tmp_val <- do.call(indicator$name, .formals)


###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2014 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: utils.R 3302 2014-01-19 19:52:42Z braverock $
#
###############################################################################
