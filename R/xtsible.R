`xtsible` <-
function(x)
{
  if(inherits(try(as.xts(x),silent=TRUE),'try-error')) {
    FALSE
  } else TRUE
}

`use.xts` <-
function(x,warn=TRUE,fail=NULL,...)
{
  xx <- try(as.xts,...,silent=warn)
  if(inherits(xx,'try-error')) {
    
  } else {

  }
}
