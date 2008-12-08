as.list.xts <- function(x, ...)
{
  if( NCOL(x) == 1 )
    return(list(x))

  cindex <- cnames <- colnames(x)
  if(is.null(cnames)) {
    cindex <- 1:NCOL(x)
    cnames <- paste("x",cindex,sep=".")
  }
  names(cindex) <- cnames 
  lapply(cindex,
            function(f) x[,f], ...)
}
