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


`str.xts` <-
function(object,...) {
  if(length(object) == 0) {
    cat("An 'xts' object of zero-width\n")
  } else {
  cat(paste("An",sQuote('xts'),"object on",
      #index(first(object)),"to",index(last(object)),
      .makeISO8601(object),
      "containing:\n"))
  cat(paste("  Data:"))
  str(coredata(object))
  cat(paste("  Indexed by objects of class: "))
  cat(paste('[',paste(indexClass(object),collapse=','),'] ',sep=''))
  cat(paste("TZ: ", indexTZ(object), "\n", sep=""))
  if(!is.null(CLASS(object)))
    cat(paste("  Original class: '",CLASS(object),"' ",sep=""),"\n")
  cat(paste("  xts Attributes: "),"\n")
  str(xtsAttributes(object),...)
  }
}

