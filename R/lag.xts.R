`Lag.xts` <- function(x, k=1, na.action=na.pass, ...) {
  x <- try.xts(x, error=FALSE)
  
  if(!is.xts(x)) x <- as.matrix(x)
  
  xx <-sapply(k, 
         function(k) {
           apply(x, 2, 
             function(x)  {
               if(k==0) return(as.matrix(x)) 
               as.matrix(c(rep(NA, k), x[-((length(x) - k + 1):length(x))]))
             }
           )}
       )
  xx <- matrix(as.numeric(xx),nr=NROW(x))
  colnames(xx) <- c(paste(colnames(x)[(rep(1:NCOL(x),length(k)))],
                          'lag',
                          rep(k, each=NCOL(x)),
                          sep = "."))
  as.function(na.action)(reclass(xx,x))
}


`Next.xts` <- function(x, k=1, na.action=na.pass, ...) {
  x <- try.xts(x, error=FALSE)
  
  if(!is.xts(x)) x <- as.matrix(x)
  
  xx <-sapply(k, 
         function(k) {
           apply(x, 2, 
             function(x)  {
               if(k==0) return(as.matrix(x)) 
               as.matrix(c(x[-(1:k)],rep(NA, k)))
             }
           )}
       )
  xx <- matrix(as.numeric(xx),nr=NROW(x))
  colnames(xx) <- c(paste(colnames(x)[(rep(1:NCOL(x),length(k)))],
                          'next',
                          rep(k, each=NCOL(x)),
                          sep = "."))
  as.function(na.action)(reclass(xx,x))

}


`lag.xts` <- function(x, k=1, na.pad=TRUE, ...) {
  .Call('lagXts', x, as.integer(k), as.logical(na.pad))
}

`diff.xts` <- function(x, lag=1, differences=1, arithmetic=TRUE, log=FALSE, na.pad=TRUE, ...)
{
  if(lag < 1 || differences < 1)
    stop("'diff.xts' defined only for positive lag and differences arguments")

  if(differences > 1) {
    if(arithmetic && !log) { #log is FALSE or missing
      x <- x - lag.xts(x, k=lag, na.pad=na.pad)
    } else {
      if(log) {
        x <- log(x/lag.xts(x, k=lag, na.pad=na.pad))
      } else x <- x/lag.xts(x, k=lag, na.pad=na.pad)
    }
    diff(x, lag, differences=differences-1, arithmetic=arithmetic, log=log, na.pad=na.pad, ...)
  } else {
    if(arithmetic && !log) {
      x - lag.xts(x, k=lag, na.pad=na.pad)
    } else {
      if(log) {
        log(x/lag.xts(x, k=lag, na.pad=na.pad))
      } else x/lag.xts(x, k=lag, na.pad=na.pad)
    }
  }
}
