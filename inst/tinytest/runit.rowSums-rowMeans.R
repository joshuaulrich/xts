
test.rowSums_dispatch <- function() {
  x <- xts(cbind(1:3, 4:6), .Date(1:3))
  y <- xts(base::rowSums(x), index(x))
  checkEqualsNumeric(y, rowSums(x))
  checkEquals(index(y), index(x))

  d <- data.frame(x)
  v <- as.vector(y)
  checkEqualsNumeric(v, rowSums(d))
  checkEquals(rownames(d), as.character(index(x)))
}

test.rowMeans_dispatch <- function() {
  x <- xts(cbind(1:3, 4:6), .Date(1:3))
  y <- xts(base::rowMeans(x), index(x))
  checkEqualsNumeric(y, rowMeans(x))
  checkEquals(index(y), index(x))

  d <- data.frame(x)
  v <- as.vector(y)
  checkEqualsNumeric(v, rowMeans(d))
  checkEquals(rownames(d), as.character(index(x)))
}
