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


`convertIndex` <-
function(x,value) {
  indexClass(x) <- value
  x
}

`indexClass` <-
function(x) {
  class <- attr(attr(x, "index"), "tclass")
  if(is.null(class))
    attr(x, '.indexCLASS')
  else
    class
}

`indexClass<-` <-
function(x,value) {
  UseMethod('indexClass<-')
}

`indexClass<-.xts` <-
function(x, value) {
  if(!is.character(value) && length(value) != 1)
    stop('improperly specified value for indexClass')

  if(!value[1] %in% c('dates','chron','POSIXt','POSIXlt','POSIXct','Date','timeDate','yearmon','yearqtr','xtime') )
       stop(paste('unsupported',sQuote('indexClass'),'indexing type:',as.character(value[[1]])))

  attr(x, '.indexCLASS') <- value
  # all index related meta-data will be stored in the index
  # as attributes
  attr(attr(x,'index'), 'tclass') <- value
  x
}

