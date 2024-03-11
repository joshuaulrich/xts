#
#   xts: eXtensible time-series
#
#   Copyright (C) 2009-2015  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Ross Bennett and Joshua M. Ulrich
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

modify.args <- function(formals, arglist, ..., dots=FALSE)
{
  # modify.args function from quantstrat
  
  # avoid evaluating '...' to make things faster
  dots.names <- eval(substitute(alist(...)))
  
  if(missing(arglist))
    arglist <- NULL
  arglist <- c(arglist, dots.names)
  
  # see 'S Programming' p. 67 for this matching
  
  # nothing to do if arglist is empty; return formals as a list
  if(!length(arglist))
    return(as.list(formals))
  
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

  # return a list (not a pairlist)
  as.list(.formals)
}
