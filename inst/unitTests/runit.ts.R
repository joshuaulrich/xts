data(sample_matrix)
sample.ts1 <- ts(sample_matrix,start=as.numeric(as.Date(rownames(sample_matrix)[1])))
sample.xts.ts1 <- as.xts(sample.ts1)


test.convert_ts_to_xts <- function() {
  checkIdentical(sample.xts.ts1,as.xts(sample.ts1))
}
test.convert_ts_to_xts_j1 <- function() {
  checkIdentical(sample.xts.ts1[,1],as.xts(sample.ts1)[,1])
}
test.convert_ts_to_xts_i1 <- function() {
  checkIdentical(sample.xts.ts1[1,],as.xts(sample.ts1)[1,])
}
test.convert_ts_to_xts_i1j1 <- function() {
  checkIdentical(sample.xts.ts1[1,1],as.xts(sample.ts1)[1,1])
}


test.ts_reclass <- function() {
  checkIdentical(sample.ts1,reclass(as.xts(sample.ts1)))
}
test.ts_reclass_subset_reclass_j1 <- function() {
  checkIdentical(sample.ts1[,1],reclass(as.xts(sample.ts1))[,1])
}
test.ts_reclass_subset_as.xts_j1 <- function() {
  checkIdentical(sample.ts1[,1],reclass(as.xts(sample.ts1)[,1]))
}
test.ts_reclass_subset_ts_j1 <- function() {
  checkIdentical(sample.ts1[,1],reclass(as.xts(sample.ts1[,1])))
}
