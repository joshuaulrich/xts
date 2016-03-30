data(sample_matrix)
sample.matrix <- sample_matrix
sample.xts <- as.xts(sample.matrix)

test.convert_matrix_to_xts <- function() {
  checkIdentical(sample.xts,as.xts(sample.matrix))
}
test.convert_matrix_to_xts_j1 <- function() {
  checkIdentical(sample.xts[,1],as.xts(sample.matrix)[,1])
}
test.convert_matrix_to_xts_i1 <- function() {
  checkIdentical(sample.xts[1,],as.xts(sample.matrix)[1,])
}
test.convert_matrix_to_xts_i1j1 <- function() {
  checkIdentical(sample.xts[1,1],as.xts(sample.matrix)[1,1])
}
test.matrix_reclass <- function() {
  checkIdentical(sample.matrix,reclass(try.xts(sample.matrix)))
}
test.matrix_reclass_subset_reclass_j1 <- function() {
  checkIdentical(sample.matrix[,1],reclass(try.xts(sample.matrix))[,1])
}
test.matrix_reclass_subset_as.xts_j1 <- function() {
  checkIdentical(sample.matrix[,1,drop=FALSE],reclass(try.xts(sample.matrix)[,1]))
  checkIdentical(sample.matrix[,1],reclass(try.xts(sample.matrix))[,1])
}
test.matrix_reclass_subset_matrix_j1 <- function() {
  checkIdentical(sample.matrix[,1,drop=FALSE],reclass(try.xts(sample.matrix[,1,drop=FALSE])))
}
