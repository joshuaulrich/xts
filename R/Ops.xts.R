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


`Ops.xts` <-
function(e1, e2)
{
  CLASS <- .Class
  e <- if (missing(e2)) {
      .Class <- "matrix"
      NextMethod(.Generic)
  }
  else if (any(nchar(.Method) == 0)) {
      .Class <- "matrix"
      NextMethod(.Generic)
  }
  else {
    if( NROW(e1)==NROW(e2) && identical(.index(e1),.index(e2)) ) {
    .Class <- "matrix"
    NextMethod(.Generic)
    } else {
      tmp.e1 <- merge.xts(e1, e2, all=FALSE, retclass=FALSE, retside=c(TRUE,FALSE))
      e2 <- merge.xts(e2, e1, all=FALSE, retclass=FALSE, retside=c(TRUE,FALSE))
      e1 <- tmp.e1
      .Class <- "matrix"
      NextMethod(.Generic)
    }
  }
  if(.Generic %in% c("+","-","*","/","^","%%","%/%")) {
    #.Call('add_xts_class', e)
    .Call('add_class', e, CLASS, PACKAGE="xts")
  }
  else 
  if(is.null(attr(e,'index'))) {
    if(is.xts(e1)) {
      .xts(e, .index(e1), 
              .indexCLASS=indexClass(e1), 
              .indexFORMAT=indexFormat(e1), 
              .indexTZ=indexTZ(e1))
    } else {
      .xts(e, .index(e2),
              .indexCLASS=indexClass(e2), 
              .indexFORMAT=indexFormat(e2), 
              .indexTZ=indexTZ(e2)
          )
    }
  } else {
  e
  }
}
