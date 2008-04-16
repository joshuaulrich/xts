`sort.xts` <-
function(x, decreasing=FALSE, MARGIN=1, ...)
{
  if(NCOL(x) > 1) {
    as.matrix(x)[order(x[,MARGIN],decreasing=decreasing,...),]
  } else as.matrix(x)[order(x,decreasing=decreasing,...),]
}
