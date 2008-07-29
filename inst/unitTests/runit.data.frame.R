data(sample_matrix)

sysTZ <- Sys.getenv('TZ')
Sys.setenv(TZ='GMT')

sample.data.frame <- data.frame(sample_matrix)
sample.xts <- as.xts(sample.data.frame)

test.convert_data.frame_to_xts <- function() {
  checkIdentical(sample.xts,as.xts(sample.data.frame))
}
# the following are failing for no good reason - they work from the cli!
test.convert_data.frame_to_xts_j1 <- function() {
  checkIdentical(sample.xts[,1],as.xts(sample.data.frame)[,1])
}
test.convert_data.frame_to_xts_i1 <- function() {
  checkIdentical(sample.xts[1,],as.xts(sample.data.frame)[1,])
}
test.convert_data.frame_to_xts_i1j1 <- function() {
  checkIdentical(sample.xts[1,1],as.xts(sample.data.frame)[1,1])
}
test.data.frame_reclass <- function() {
  checkIdentical(sample.data.frame,reclass(as.xts(sample.data.frame)))
}
test.data.frame_reclass_subset_reclass_j1 <- function() {
  checkIdentical(sample.data.frame[,1],reclass(as.xts(sample.data.frame))[,1])
}

# subsetting to 1 col converts to simple numeric - can't successfully handle

test.data.frame_reclass_subset_as.xts_j1 <- function() {
  checkIdentical(sample.data.frame[,1],reclass(as.xts(sample.data.frame)[,1]))
}
test.data.frame_reclass_subset_data.frame_j1 <- function() {
  # subsetting results in a vector, so can't be converted to xts
  checkException(as.xts(sample.data.frame[,1]))
}

Sys.setenv(TZ=sysTZ)
