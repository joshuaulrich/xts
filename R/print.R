`print.xts` <-
function(x,fmt,...) {
  if(missing(fmt)) 
    fmt <- indexFormat(x)
  if(is.null(fmt))
    fmt <- TRUE
  
  xx <- coredata(x, fmt)
  if(length(xx) == 0) {
    cat('Data:\n')
    print(numeric(0))
    cat('\n')
    cat('Index:\n')
    index <- index(x)
    if(length(index) == 0) {
      print(index)
    } else {
      str(index(x))
    }
  } else print(xx)
}

