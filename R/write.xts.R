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


`write.xts` <-
function(x) {
  NC <- NCOL(x)
  NR <- NROW(x)
  DAT <- c(NC,NR)
  x <- c(.index(x), as.numeric(x))
  offset <- 0
  for(i in 1:(NC+1)) {
    end   <- seq(i+offset*NR, length.out=NR)-offset
    DAT <- c(DAT, c(x[end[1]], diff(x[end])))
    offset <- offset + 1
  }
  DAT
}

`read.xts` <-
function(x) {
  NC <- x[1]
  NR <- x[2]
  x <- x[-c(1:2)]
  .xts(apply(matrix(x[-(1:NR)], nc=NC),2,cumsum), cumsum(x[1:NR]))
}

