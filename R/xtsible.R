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
  xx <- try(as.xts(x,...),silent=TRUE)
  if(inherits(xx,'try-error')) {
    if(is.character(error)) {
      message(error)
    } else  
    if(error) {
      message(gsub('\n','',xx))
    } else FALSE
  } else {
    xx
  }
}
