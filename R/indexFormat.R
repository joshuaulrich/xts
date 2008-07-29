`indexFormat` <-
function(x) {
  attr(x, '.indexFORMAT')
}

`indexFormat<-` <-
function(x, value) {
  UseMethod('indexFormat<-')
}

`indexFormat<-.xts` <-
function(x, value) {
  
  if(!is.character(value) && !is.null(value))
    stop('must provide valid POSIX formatting string')

  attr(x, '.indexFORMAT') <- value
  x
}
