`print.xts` <-
function(x,fmt,...) {
  if(missing(fmt)) 
    fmt <- indexFormat(x)
  
  xx <- as.matrix(x)
  if(!is.null(fmt)) {
    rownames(xx) <- format(index(x), fmt)
  }
  print(xx)
}

