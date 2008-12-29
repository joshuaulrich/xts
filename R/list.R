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


as.list.xts <- function(x, ...)
{
  if( NCOL(x) == 1 )
    return(list(x))

  cindex <- cnames <- colnames(x)
  if(is.null(cnames)) {
    cindex <- 1:NCOL(x)
    cnames <- paste("x",cindex,sep=".")
  }
  names(cindex) <- cnames 
  lapply(cindex,
            function(f) x[,f], ...)
}
