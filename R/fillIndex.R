`fillIndex` <-
function(x) {
  p <- periodicity(x)
  xx <- xts(matrix(rep(NA,NCOL(x)),nr=1),
           seq(start(x),end(x),by=p$units))
  xx[index(xx) %in% index(x)] <- x
  colnames(xx) <- colnames(x)
  xx
}
