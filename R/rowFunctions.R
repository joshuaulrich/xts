#
#   xts: eXtensible time-series
#
#   Copyright (C) 2022  Joshua M. Ulrich josh.m.ulrich @ gmail.com
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

rowSums <-
function(x,
         na.rm = FALSE,
         dims = 1L,
         ...)
{
    UseMethod("rowSums")
}

rowSums.default <-
function(x,
         na.rm = FALSE,
         dims = 1L,
         ...)
{
    base::rowSums(x, na.rm, dims)
}

rowSums.xts <-
    function(x,
             na.rm = FALSE,
             dims = 1L,
             ...,
             name = "sum")
{
    r <- matrix(rowSums(coredata(x), na.rm = na.rm, dims = dims))
    xcoredata(r) <- xcoredata(x)
    colnames(r) <- name
    return(r)
}

rowMeans <-
function(x,
         na.rm = FALSE,
         dims = 1L,
         ...)
{
    UseMethod("rowMeans")
}

rowMeans.default <-
function(x,
         na.rm = FALSE,
         dims = 1L,
         ...)
{
    base::rowMeans(x, na.rm, dims)
}

rowMeans.xts <-
    function(x,
             na.rm = FALSE,
             dims = 1L,
             ...,
             name = "mean")
{
    r <- matrix(rowMeans(coredata(x), na.rm = na.rm, dims = dims))
    xcoredata(r) <- xcoredata(x)
    colnames(r) <- name
    return(r)
}

