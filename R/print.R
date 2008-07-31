`print.xts` <-
function(x,fmt,...) {
  if(missing(fmt)) 
    fmt <- indexFormat(x)
  if(is.null(fmt))
    fmt <- TRUE
  
  xx <- coredata(x, fmt)
  print(xx)
}

