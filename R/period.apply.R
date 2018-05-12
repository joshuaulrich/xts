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

`period.apply` <-
function(x, INDEX, FUN, ...)
{
    x <- try.xts(x, error = FALSE)
    FUN <- match.fun(FUN)

    if(!isOrdered(INDEX)) {
      # isOrdered returns FALSE if there are duplicates
      INDEX <- sort(unique(INDEX))
    }
    if(INDEX[1] != 0) {
      INDEX <- c(0, INDEX)
    }
    if(last(INDEX) != NROW(x)) {
      INDEX <- c(INDEX, NROW(x))
    }

    xx <- sapply(1:(length(INDEX) - 1), function(y) {
                   FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
                })
    if(is.vector(xx))
      xx <- t(xx)
    xx <- t(xx)
    if(is.null(colnames(xx)) && NCOL(x)==NCOL(xx))
      colnames(xx) <- colnames(x)
    reclass(xx, x[INDEX])
}


`period.apply.original` <-
function (x, INDEX, FUN, ...)
{
  x <- use.xts(x,error=FALSE)

  if(!is.xts(x)) {
    FUN <- match.fun(FUN)
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
          FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
    })
  } else {
    FUN <- match.fun(FUN)
    new.index <- index(x)[INDEX]
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
          FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
    })
    xx <- xts(xx,new.index)
    CLASS(xx) <- CLASS(x)
    xtsAttributes(xx) <- xtsAttributes(x)
    xx <- reclass(xx)
  }
  xx
}

`apply.daily` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'days')
  period.apply(x,ep,FUN, ...)
}
`apply.weekly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'weeks')
  period.apply(x,ep,FUN, ...)
}

`apply.monthly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'months')
  period.apply(x,ep,FUN, ...)
}

`apply.quarterly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'quarters')
  period.apply(x,ep,FUN, ...)
}

`apply.yearly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'years')
  period.apply(x,ep,FUN, ...)
}

period_apply <- function(x, INDEX, FUN, ...) {
  fun <- substitute(FUN)
  e <- new.env()
  pl <- .Call("xts_period_apply", x, INDEX, fun, e, PACKAGE = "xts")
  # need to copy other attributes to result?
  .xts(do.call(rbind, pl), .index(x)[INDEX])
}
