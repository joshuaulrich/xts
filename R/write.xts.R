`write.xts` <-
function(x) {
  NC <- NCOL(x)
  NR <- NROW(x)
  DAT <- c(NC,NR)
  x <- c(.index(x), as.numeric(x))
  offset <- 0
  for(i in 1:(NC+1)) {
    end   <- seq(i+offset*NR, length.out=NR)-offset
    DAT <- c(DAT, c(x[end[1]], diff(x[end])))
    offset <- offset + 1
  }
  DAT
}

`read.xts` <-
function(x) {
  NC <- x[1]
  NR <- x[2]
  x <- x[-c(1:2)]
  .xts(apply(matrix(x[-(1:NR)], nc=NC),2,cumsum), cumsum(x[1:NR]))
}

