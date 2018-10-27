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

.lgl_ops <- c("&", "|", ">", ">=", "==", "!=", "<=", "<")

`Ops.xts` <-
function(e1, e2)
{
  if (missing(e2)) {
    .Class <- "matrix"
    e <- NextMethod(.Generic)
    if (.Generic == "!") {
      attributes(e) <- attributes(e1)
    }
    return(e)
  }

  .method <- nzchar(.Method)
  if (!all(.method)) {
    a <- if (.method[1L]) e1 else e2

    .Class <- "matrix"
    e <- NextMethod(.Generic)
    if (any(.Generic == .lgl_ops)) {
      attributes(e) <- attributes(a)
    }
  } else {
    if( NROW(e1)==NROW(e2) && identical(.index(e1),.index(e2)) ) {
      .Class <- "matrix"
      e <- NextMethod(.Generic)
      if (any(.Generic == .lgl_ops)) {
        attributes(e) <- attributes(e1)
      }
    } else {
      side <- c(TRUE, FALSE)
      e1 <- merge.xts(e1, e2, all=FALSE, retclass=TRUE,  retside=side)
      a1 <- e1
      e1 <- coredata(e1)
      e2 <- merge.xts(e2, a1, all=FALSE, retclass=FALSE, retside=side)

      .Class <- "matrix"
      e <- NextMethod(.Generic)

      if (length(attr(e, "index")) < 1 || any(.Generic == .lgl_ops)) {
        attributes(e) <- attributes(a1)
      }
    }
  }
  e
}
