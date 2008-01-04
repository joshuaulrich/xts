`timeformat` <-
function(x) {
  if(inherits(x,'zoo'))
    x <- as.character(index(x)[1])
  x <- strsplit(x,' ')[[1]]
  sep <- 
  reg <- c(CCYYMMDD='^\\d{4}.\\d{2}.\\d{2}$',
           CCYYMM='^\\d{4}.\\d{2}$',
           CCYY='^\\d{4}$')
  for(r in names(reg)) {
    if(regexpr(reg[[r]],x[1],perl=TRUE)==1) {
      tf <- r
      break
    }
  }

  Time = NULL

  if(length(x) > 1) {
    reg <- c(HHMMSS='^\\d{2}.\\d{2}.\\d{2}$',
             HHMM='^\\d{2}.\\d{2}$',
             HH='^\\d{2}$')
    for(r in names(reg)) {
      if(regexpr(reg[[r]],x[2],perl=TRUE)==1) {
        Time <- r
        break
      }
    }
  tf <- paste(tf,Time)  
  }

  return(structure(list(format=tf,detail=nchar(tf)),class='timeformat'))
}

`print.timeformat` <-
function(x,...) {
  cat(x$format,'\n')
}

`Ops.timeformat` <-
function(e1,e2) {
  if(nargs()==1) stop('unary ',.Generic,' not defined for xts objects')
  boolean <- switch(.Generic, "<" = , ">" = , "==" = , "!=" = , 
                    "<=" = , ">=" = TRUE, FALSE)
  if (!boolean) 
      stop(.Generic, " not defined for Date objects")
  e1 <- e1$detail
  e2 <- e2$detail
  NextMethod(.Generic)
}
