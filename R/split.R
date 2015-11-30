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

split.xts <-
function(x, f="months", drop=FALSE, k=1, ...) {
  if(is.character(f) && length(f) == 1L) {
    ep <- endpoints(x, on=f, k=k)
    sp <- (ep + 1)[-length(ep)]
    ep <- ep[-1]
    lapply(1:length(ep), function(X) x[sp[X]:ep[X]])
  } else
  NextMethod("split")
}

