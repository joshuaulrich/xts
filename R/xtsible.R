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
    attr(x,'.RECLASS') <- FALSE
    return(x)
  }

  xx <- try(as.xts(x,...),silent=TRUE)
  if(inherits(xx,'try-error')) {
    if(is.character(error)) {
      message(error)
    } else  
    if(is.function(error)) {
      return(error(x, ...))
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

.merge.xts.scalar <- function(x, length.out, ...) {
  if( length(x) == 1 )
    return(matrix(rep(x, length.out=length.out)))
  if( NROW(x) == length.out )
    return(x)
  stop("can't convert argument to be merged")  
}

`use.reclass` <- function(x) {
  xx <- match.call()
  xxObj <- eval.parent(xx[[-1]][[2]],1)
  inObj <- try.xts(xxObj, error=FALSE)
  xx <- eval(match.call()[[-1]])
  reclass(xx, inObj)
}

`Reclass` <- use.reclass

