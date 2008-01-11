# functions from quantmod to check for OHLC style/columns
# NOT TO BE EXPORTED
#

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

`Op` <-
function(x)
{
  if(has.Op(x))
    return(x[,grep('Open',colnames(x))])
  NULL
}

`has.Op` <-
function(x,which=FALSE)
{
  loc <- grep('Open',colnames(x))
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Hi` <-
function(x)
{
  if(has.Hi(x))
    return(x[,grep('High',colnames(x))])
  NULL
}

`has.Hi` <-
function(x,which=FALSE)
{
  loc <- grep('High',colnames(x))
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Lo` <-
function(x)
{
  if(has.Lo(x))
    return(x[,grep('Low',colnames(x))])
  NULL
}

`has.Lo` <-
function(x,which=FALSE)
{
  loc <- grep('Low',colnames(x))
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Cl` <-
function(x)
{
  if(has.Cl(x))
    return(x[,grep('Close',colnames(x))])
  NULL
}
`has.Cl` <-
function(x,which=FALSE)
{
  loc <- grep('Close',colnames(x))
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
    return(x[,grep('Volume',colnames(x))])
  NULL
}
`has.Vo` <-
function(x,which=FALSE)
{
  loc <- grep('Volume',colnames(x))
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Ad` <-
function(x)
{
  if(has.Ad(x))
    return(x[,grep('Adjusted',colnames(x))])
  NULL
}
`has.Ad` <-
function(x,which=FALSE)
{
  loc <- grep('Adjusted',colnames(x))
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}
