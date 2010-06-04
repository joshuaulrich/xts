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


# functions from quantmod to check for OHLC style/columns
# NOT TO BE EXPORTED
#
`OHLCV` <- 
function (x) 
{
    if (is.OHLCV(x)) 
        return(x[, has.OHLCV(x, 1)])
    NULL
}

`is.OHLCV` <-
function(x)
{
  all(has.Op(x),has.Hi(x),has.Lo(x),has.Cl(x),has.Vo(x))
}

`has.OHLCV` <-
function(x,which=FALSE)
{
  if(which) {
    c(has.Op(x,1),has.Hi(x,1),has.Lo(x,1),has.Cl(x,1),has.Vo(x,1))
  } else {
    c(has.Op(x),has.Hi(x),has.Lo(x),has.Cl(x),has.Vo(x))
  }
}

`OHLC` <- 
function (x) 
{
    if (is.OHLC(x)) 
        return(x[, has.OHLC(x, 1)])
    NULL
}

`is.OHLC` <-
function(x)
{
  all(has.Op(x),has.Hi(x),has.Lo(x),has.Cl(x))
}

`has.OHLC` <-
function(x,which=FALSE)
{
  if(which) {
    c(has.Op(x,1),has.Hi(x,1),has.Lo(x,1),has.Cl(x,1))
  } else {
    c(has.Op(x),has.Hi(x),has.Lo(x),has.Cl(x))
  }
}

`HLC` <- 
function (x) 
{
    if (is.HLC(x)) 
        return(x[, has.HLC(x, 1)])
    NULL
}

`is.HLC` <-
function(x)
{
  all(has.Hi(x),has.Lo(x),has.Cl(x))
}

`has.HLC` <-
function(x,which=FALSE)
{
  if(which) {
    c(has.Hi(x,1),has.Lo(x,1),has.Cl(x,1))
  } else {
    c(has.Hi(x),has.Lo(x),has.Cl(x))
  }
}

`Op` <-
function(x)
{
  if(has.Op(x))
    return(x[,grep('Open',colnames(x),ignore.case=TRUE)])
  NULL
}

`has.Op` <-
function(x,which=FALSE)
{
  loc <- grep('Open',colnames(x),ignore.case=TRUE)
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Hi` <-
function(x)
{
  if(has.Hi(x))
    return(x[,grep('High',colnames(x),ignore.case=TRUE)])
  NULL
}

`has.Hi` <-
function(x,which=FALSE)
{
  loc <- grep('High',colnames(x),ignore.case=TRUE)
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Lo` <-
function(x)
{
  if(has.Lo(x))
    return(x[,grep('Low',colnames(x),ignore.case=TRUE)])
  NULL
}

`has.Lo` <-
function(x,which=FALSE)
{
  loc <- grep('Low',colnames(x),ignore.case=TRUE)
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Cl` <-
function(x)
{
  if(has.Cl(x))
    return(x[,grep('Close',colnames(x),ignore.case=TRUE)])
  NULL
}
`has.Cl` <-
function(x,which=FALSE)
{
  loc <- grep('Close',colnames(x),ignore.case=TRUE)
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Vo` <-
function(x)
{
  #vo <- grep('Volume',colnames(x))
  #if(!identical(vo,integer(0)))
  if(has.Vo(x))
    return(x[,grep('Volume',colnames(x),ignore.case=TRUE)])
  NULL
}
`has.Vo` <-
function(x,which=FALSE)
{
  loc <- grep('Volume',colnames(x),ignore.case=TRUE)
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Ad` <-
function(x)
{
  if(has.Ad(x))
    return(x[,grep('Adjusted',colnames(x),ignore.case=TRUE)])
  NULL
}
`has.Ad` <-
function(x,which=FALSE)
{
  loc <- grep('Adjusted',colnames(x),ignore.case=TRUE)
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}
