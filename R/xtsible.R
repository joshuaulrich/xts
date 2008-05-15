`xtsible` <-
function(x)
{
  if(inherits(try(as.xts(x),silent=TRUE),'try-error')) {
    FALSE
  } else TRUE
}

`use.xts` <-
function(x, ..., error=TRUE)
{
  if(is.xts(x)) return(x)

  xx <- try(as.xts(x,...),silent=TRUE)
  if(inherits(xx,'try-error')) {
    if(is.character(error)) {
      message(error)
    } else  
    if(error) {
      message(gsub('\n','',xx))
    } else {
#      if(!exists(deparse(substitute(x))))
#        stop(paste('object',dQuote(deparse(substitute(x))),"not found"))
      return(x) 
    }
  } else {
    xx
  }
}

`using.xts` <- function(x) {
  xx <- match.call()
  xxObj <- eval(xx[[-1]][[2]])
  inObj <- use.xts(xxObj, error=FALSE)
  xx <- eval(match.call()[[-1]])
  reclass(xx, inObj)
}
