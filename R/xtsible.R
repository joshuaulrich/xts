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
  if(is.xts(x)) {
    return(x)
  }

  xx <- try(as.xts(x,...),silent=TRUE)
  if(inherits(xx,'try-error')) {
    if(is.character(error)) {
      message(error)
    } else  
    if(error) {
      message(gsub('\n','',xx))
    } else {
#     if(!exists(deparse(substitute(x))))
#        stop(paste('object',dQuote(deparse(substitute(x))),"not found"))
      return(x) 
    }
  } else {
    # made positive: now test if needs to be reclassed
    xx
  }
}

`try.xts` <- use.xts

`use.reclass` <- function(x) {
  xx <- match.call()
  xxObj <- eval.parent(xx[[-1]][[2]],1)
  inObj <- try.xts(xxObj, error=FALSE)
  xx <- eval(match.call()[[-1]])
  reclass(xx, inObj)
}
