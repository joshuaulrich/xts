# In the other files, we are able to set up global data.
# For some reason, it does not work in this file.
# Use explicit setup and teardown
.setUp <- function() {
  data(sample_matrix)
  sysTZ <- Sys.getenv('TZ')
  assign("sysTZ", sysTZ, envir = .GlobalEnv)
  Sys.setenv(TZ='GMT')

  sample.data.frame <- data.frame(sample_matrix)
  sample.xts <- as.xts(sample.data.frame)
  assign("sample.data.frame", sample.data.frame, envir = .GlobalEnv)
  assign("sample.xts", sample.xts, envir = .GlobalEnv)
}

.tearDown <- function() {
  Sys.setenv(TZ=sysTZ)
  rm(sample.data.frame, envir = .GlobalEnv)
  rm(sample.xts, envir = .GlobalEnv)
}

test.convert_data.frame_to_xts <- function() {
  checkIdentical(sample.xts,as.xts(sample.data.frame))
}

test.convert_data.frame_to_xts_j1 <- function() {
  checkIdentical(sample.xts[,1],as.xts(sample.data.frame)[,1])
}
test.convert_data.frame_to_xts_i1 <- function() {
  checkIdentical(sample.xts[1,],as.xts(sample.data.frame)[1,])
}
test.convert_data.frame_to_xts_i1j1 <- function() {
  checkIdentical(sample.xts[1,1],as.xts(sample.data.frame)[1,1])
}

# The reclass tests fail. The attributes do not match.
no_test.data.frame_reclass <- function() {
  checkIdentical(sample.data.frame,reclass(as.xts(sample.data.frame)))
}
no_test.data.frame_reclass_subset_reclass_j1 <- function() {
  checkIdentical(sample.data.frame[,1],reclass(as.xts(sample.data.frame))[,1])
}

# subsetting to 1 col converts to simple numeric - can't successfully handle

no_test.data.frame_reclass_subset_as.xts_j1 <- function() {
  checkIdentical(sample.data.frame[,1,drop=FALSE],reclass(as.xts(sample.data.frame)[,1]))
}
no_test.data.frame_reclass_subset_data.frame_j1 <- function() {
  # subsetting results in a vector, so can't be converted to xts
  checkException(as.xts(sample.data.frame[,1]))
}
  


