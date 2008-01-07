`timeformat` <-
function(x) {
  if(inherits(x,'zoo'))
    x <- as.character(index(x)[1])
  x <- strsplit(x,' ')[[1]]
  sep <- 
  reg <- list(CCYYMMDD=list('^\\d{4}.\\d{2}.\\d{2}$','%Y-%m-%d'),
              CCYYMM=list('^\\d{4}.\\d{2}$','%Y-%m'),
              CCYY=list('^\\d{4}$','%Y'))
  for(r in names(reg)) {
    if(regexpr(reg[[r]][[1]],x[1],perl=TRUE)==1) {
      tf <- r
      TF <- reg[[r]][[2]]
      break
    }
  }

  Time = NULL

  if(length(x) > 1) {
    reg <- list(HHMMSS=list('^\\d{2}.\\d{2}.\\d{2}$','%H:%M:%S'),
                HHMM=list('^\\d{2}.\\d{2}$','%H:%M'),
                HH=list('^\\d{2}$','%H'))
    for(r in names(reg)) {
      if(regexpr(reg[[r]][[1]],x[2],perl=TRUE)==1) {
        Time <- r
        TimeFormat <- reg[[r]][[2]]
        break
      }
    }
  tf <- paste(tf,Time)  
  TF <- paste(TF,TimeFormat)
  }

  return(structure(list(format=tf,FORMAT=TF,detail=nchar(tf)),class='timeformat'))
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
