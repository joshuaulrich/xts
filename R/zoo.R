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


# functions to handle zoo <--> xts conversions

`re.zoo` <-
function(x,...) {
  xx <- coredata(x)

  xx <- zoo(xx,
        order.by=index(x),
        ...)

  if(length(dimnames(x)[[2]]) < 2) {
    dimnames(xx) <- NULL
    dim(xx) <- NULL
    attr(xx,'names') <- as.character(index(x))
  }
  xx
}

`as.xts.zoo` <-
function(x,order.by=index(x),frequency=NULL,...) {
  xx <- xts(coredata(x),          # Cannot use 'zoo()' on objects of class 'zoo' - jmu
            order.by=order.by,
            frequency=frequency,
            .CLASS='zoo',
            ...)

#
#  if(!is.null(attr(x,'names'))) {
#    dim(xx) <- c(NROW(xx),NCOL(xx))
#    dn <- list(attr(x,'names'),colnames(x))
#    dimnames(xx) <- dn
#    attr(xx,'.ROWNAMES') <- attr(x,'names')
#  }
#
  xx
}

#`as.zoo.xts` <-
#function(x,...) {
#  cd <- coredata(x);
#  if( length(cd)==0 )
#    cd <- NULL
#  zoo(cd,
#      order.by=index(x),
#      ...)
#}
