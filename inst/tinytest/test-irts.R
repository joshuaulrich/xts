if (requireNamespace("tseries", quietly = TRUE)) {

  library(xts)
  data(sample_matrix)
  
  sample.irts <- tseries::irts(as.POSIXct(rownames(sample_matrix)),sample_matrix)
  sample.irts.xts <- as.xts(sample.irts)
  
  info_msg <- "test.convert_irts_to_xts <- function()"
  expect_identical(sample.irts.xts,as.xts(sample.irts), info = info_msg)

  info_msg <- "test.convert_irts_to_xts_j1"
  expect_identical(sample.irts.xts[,1],as.xts(sample.irts)[,1], info = info_msg)
 
  info_msg <- "test.convert_irts_to_xts_i1"
  expect_identical(sample.irts.xts[1,],as.xts(sample.irts)[1,], info = info_msg)
 
  info_msg <- "test.convert_irts_to_xts_i1j1"
  expect_identical(sample.irts.xts[1,1],as.xts(sample.irts)[1,1], info = info_msg)
}  # requireNamespace
