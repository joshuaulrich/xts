data(sample_matrix)
sample.zoo <- zoo(sample_matrix, as.Date(rownames(sample_matrix)))
sample.xts <- as.xts(sample.zoo)

info_msg <- "test.convert_zoo_to_xts"
expect_identical(sample.xts, as.xts(sample.zoo), info = info_msg)

info_msg <- "test.convert_zoo_to_xts_j1"
expect_identical(sample.xts[,1], as.xts(sample.zoo)[,1], info = info_msg)

info_msg <- "test.convert_zoo_to_xts_i1"
expect_identical(sample.xts[1,], as.xts(sample.zoo)[1,], info = info_msg)

info_msg <- "test.convert_zoo_to_xts_i1j1"
expect_identical(sample.xts[1,1], as.xts(sample.zoo)[1,1], info = info_msg)
# test.zoo_reclass <- function() {
#   DEACTIVATED("rownames are not kept yet in current xts-dev")
#   expect_identical(sample.zoo,reclass(try.xts(sample.zoo)), info = info_msg)
# }
# test.zoo_reclass_subset_reclass_j1 <- function() {
#   DEACTIVATED("rownames are not kept yet in current xts-dev")
#   expect_identical(sample.zoo[,1],reclass(try.xts(sample.zoo))[,1], info = info_msg)
# }

info_msg <- "test.zoo_reclass_subset_as.xts_j1"
expect_identical(sample.zoo[,1], reclass(try.xts(sample.zoo)[,1]), info = info_msg)

info_msg <- "test.zoo_reclass_subset_zoo_j1"
expect_identical(sample.zoo[,1], reclass(try.xts(sample.zoo[,1])), info = info_msg)
