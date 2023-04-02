#
#   xts: eXtensible time-series
#
#   Copyright (C) 2019 Joshua M. Ulrich
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

all.equal.xts <-
function(target,
         current,
         ...,
         check.attributes = TRUE)
{
    if (isTRUE(check.attributes)) {
        # Remove potential index attributes on the objects
        attrNames <- c(".indexCLASS", ".indexTZ", "tclass", "tzone")
        for (aname in attrNames) {
            attr(target, aname) <- NULL
            attr(current, aname) <- NULL
        }
        # Order the object attributes
        a <- attributes(target)
        attributes(target) <- a[sort(names(a))]

        a <- attributes(current)
        attributes(current) <- a[sort(names(a))]

        # Order the index attributes
        a <- attributes(.index(target))
        attributes(.index(target)) <- a[sort(names(a))]

        a <- attributes(.index(current))
        attributes(.index(current)) <- a[sort(names(a))]
    }
    NextMethod("all.equal")
}
