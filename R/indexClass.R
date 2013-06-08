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

tclass <- indexClass <-
function(x) {
  class <- attr(attr(x, "index"), "tclass")
  if(is.null(class))
    attr(x, '.indexCLASS')
  else
    class
}

`tclass<-` <-
function(x,value) {
  UseMethod('tclass<-')
}

`indexClass<-` <-
function(x,value) {
  UseMethod('indexClass<-')
}

`tclass<-.xts` <- `indexClass<-.xts` <-
function(x, value) {
  if(!is.character(value) && length(value) != 1)
    stop('improperly specified value for indexClass')

  # remove 'POSIXt' from value, to prevent indexClass(x) <- 'POSIXt'
  value <- value[!value %in% "POSIXt"]
  if(length(value)==0L)
    stop(paste('unsupported',sQuote('indexClass'),'indexing type: POSIXt'))

  if(!value[1] %in% c('dates','chron','POSIXlt','POSIXct','Date','timeDate','yearmon','yearqtr','xtime') )
       stop(paste('unsupported',sQuote('indexClass'),'indexing type:',as.character(value[[1]])))

  # Add 'POSIXt' virtual class
  if(value %in% c('POSIXlt','POSIXct'))
    value <- c(value,'POSIXt')

  if(value %in% 'Date') {
    
  }

  attr(x, '.indexCLASS') <- value
  # all index related meta-data will be stored in the index
  # as attributes
  attr(attr(x,'index'), 'tclass') <- value
  x
}

