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


cbind.xts <- function(..., all=TRUE, fill=NA, suffixes=NULL) {
#  mc <- match.call(call=sys.call(sys.parent()))
#  mc[[1]] <- as.name("merge.xts")
#  eval(mc)
  merge.xts(..., all=all, fill=fill, suffixes=suffixes)
}  
#  # convert the call to a list to better manipulate it
#  mc <- as.list(match.call(call=sys.call(-1)))
#  
#  # remove deparse.level arg if called via cbind 'generic'
#  if(as.character(mc[[1]]) == "cbind")
#    mc <- mc[-2]
#
#  # check if any default args are missing from the call,
#  # and add them to the call with the cbind defaults
#  if(missing(all))      mc <- c(mc,all=all)
#  if(missing(fill))     mc <- c(mc,fill=fill)
#  if(missing(suffixes)) mc <- c(mc,suffixes=suffixes)
#  
#  # replace the call to cbind.xts with merge.xts
#  mc[[1]] <- as.name('merge.xts')
#
#  # convert the list into a call and evaluate it
#  mc <- as.call(mc)
#  eval(mc)
#} 
#   sc <- sys.call(sys.parent())
#   mc <- gsub('cbind|cbind.xts','merge.xts',deparse(match.call(call=sc)))
#   return(eval(parse(text=mc)))
#   dots <- mc$...
#   length.args <- sum(.External("number_of_cols",...,PACKAGE="xts"))
#   if(is.null(suffixes))
#     suffixes <- all.vars(match.call(call=sc), unique=FALSE)[1:length.args]
# 
#   if( length(suffixes) != length.args ) {
#     warning("length of suffixes and does not match number of merged objects")
#     suffixes <- rep(suffixes, length.out=length.args)
#   }
#
#   merge.xts(..., all=all, fill=fill, suffixes=suffixes)
#
#   dat <- list(...)
#   x <- dat[[1]]; dat <- dat[-1]
#   while( length(dat) > 0 ) {
#     y <- dat[[1]]
#     if( length(dat) > 0 )
#       dat <- dat[-1]
#     x <- merge.xts(x, y, all=TRUE, fill=NA, suffixes=NULL, retclass="xts")
#   }
#   x
#}

`c.xts` <-
function(...) {
  .External("rbindXts", dup=FALSE, ..., PACKAGE="xts")
}

rbind.xts <- function(..., deparse.level=1)
{
  .External("rbindXts", dup=FALSE, ..., PACKAGE="xts")
}

`.rbind.xts` <-
function(..., deparse.level=1) {
#  if(missing(y)) 
#    return(x)
#  if( missing(...) )
#    return(.Call('do_rbind_xts',x, y))
#  
  dots <- list(...)
  if(length(dots) < 2) return(dots[[1]])
  x <- dots[[1]]
  dots <- dots[-1]
#  y <- dots[[2]]
#  if(!is.null(colnames(y)) && colnames(x) != colnames(y))
#    warning('column names differ')
#  x <- .Call('do_rbind_xts', x, y)
  env <- new.env()
  while( length(dots) > 0 ) {
    y <- dots[[1]]
    if( length(dots) > 0)
      dots <- dots[-1]
    if(!is.null(colnames(y)) && colnames(x) != colnames(y))
      warning('column names differ')
    x <- .Call('do_rbind_xts',x,y,PACKAGE="xts")
  }
  return(x)
}
