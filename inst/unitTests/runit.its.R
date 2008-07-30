library(its)
data(sample_matrix)
sys.TZ <- Sys.getenv('TZ')
Sys.setenv(TZ='GMT')

sample.its <- its(sample_matrix,as.POSIXct(rownames(sample_matrix)))
sample.xts <- as.xts(sample.its)



test.convert_its_to_xts <- function() {
  checkIdentical(sample.xts,as.xts(sample.its))
}
test.convert_its_to_xts_j1 <- function() {
  checkIdentical(sample.xts[,1],as.xts(sample.its)[,1])
}
test.convert_its_to_xts_i1 <- function() {
  checkIdentical(sample.xts[1,],as.xts(sample.its)[1,])
}
test.convert_its_to_xts_i1j1 <- function() {
  checkIdentical(sample.xts[1,1],as.xts(sample.its)[1,1])
}
test.its_reclass <- function() {
#  DEACTIVATED()
  checkIdentical(sample.its,reclass(as.xts(sample.its)))
}
test.its_reclass_subset_reclass_j1 <- function() {
#  DEACTIVATED()
  checkIdentical(sample.its[,1],reclass(as.xts(sample.its))[,1])
}
test.its_reclass_subset_as.xts_j1 <- function() {
#  DEACTIVATED()
  checkIdentical(sample.its[,1],reclass(as.xts(sample.its)[,1]))
}
test.its_reclass_subset_its_j1 <- function() {
#  DEACTIVATED()
  checkIdentical(sample.its[,1],reclass(as.xts(sample.its[,1])))
}

Sys.setenv(TZ=sys.TZ)
