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


`isOrdered` <- function(x, increasing=TRUE, strictly=TRUE) {
  # x must be of type double or integer.  Checked in the C code.
  if(is.character(x))
    stop('character ordering unsupported')

  if(!is.numeric(x))
    x = as.numeric(x)

  .Call('do_is_ordered', 
        x = x,
        increasing = as.logical(increasing),
        strictly   = as.logical(strictly), PACKAGE='xts')
}
