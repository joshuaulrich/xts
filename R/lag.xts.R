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

`.diff.xts` <- function(x, lag=1, differences=1, arithmetic=TRUE,na.pad=TRUE,...) {
  ### TEMPORARY FIX UNTIL NEW xts METHOD IS WRITTEN
  x <- try.xts(x)
  xx <- diff(as.zoo(x),lag=lag,differences=differences, arithmetic=arithmetic,na.pad=na.pad,...)
  narm <- if(!na.pad) {
            seq_len(lag * differences)
          } else seq_len(NROW(x))
  reclass(xx, x[narm])
}


#`lag.xts` <- `Lag` <- function(x, k=1) {
#
#  x <- try.xts(x, error=FALSE)
#
#  if(NCOL(x) > 1) {
#    xx <- x[,1]
#  } else xx <- x
#
#  for(i in 1:NCOL(x)) {
#    xx <- cbind.zoo(xx,sapply(k, 
#                           function(k) {
#                             if(NCOL(x) > 1) x <- x[,i]
#                             if(k==0) return(x)
#                             if(k > NROW(x)) k <- NROW(x)
#                             c(rep(NA,k), x[-((NROW(x)-k+1):NROW(x))])
#                           }
#                         )
#               )
#  }
#  xx <- xx[,-1]
#  dim(xx) <- c(NROW(xx),NCOL(xx))
#  colnames(xx) <- c(paste('lag',
#                        rep(k,NCOL(x)),
#                        colnames(x)[sort(rep(1:NCOL(x),length(k)))],
#                        sep='.') )
#  reclass(coredata(xx),x)
#}
