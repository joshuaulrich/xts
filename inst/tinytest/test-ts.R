data(sample_matrix)
sample.ts1 <- ts(sample_matrix,start=as.numeric(as.Date(rownames(sample_matrix)[1])))
sample.xts.ts1 <- as.xts(sample.ts1)


info_msg <- "test.convert_ts_to_xts"
expect_identical(sample.xts.ts1, as.xts(sample.ts1), info = info_msg)
info_msg <- "test.convert_ts_to_xts_j1"
expect_identical(sample.xts.ts1[,1], as.xts(sample.ts1)[,1], info = info_msg)
info_msg <- "test.convert_ts_to_xts_i1"
expect_identical(sample.xts.ts1[1,], as.xts(sample.ts1)[1,], info = info_msg)
info_msg <- "test.convert_ts_to_xts_i1j1"
expect_identical(sample.xts.ts1[1,1], as.xts(sample.ts1)[1,1], info = info_msg)


info_msg <- "test.ts_reclass"
expect_identical(sample.ts1, reclass(try.xts(sample.ts1)), info = info_msg)
info_msg <- "test.ts_reclass_subset_reclass_j1"
expect_identical(sample.ts1[,1], reclass(try.xts(sample.ts1))[,1], info = info_msg)
info_msg <- "test.ts_reclass_subset_as.xts_j1"
expect_identical(sample.ts1[,1], reclass(try.xts(sample.ts1)[,1]), info = info_msg)
info_msg <- "test.ts_reclass_subset_ts_j1"
expect_identical(sample.ts1[,1], reclass(try.xts(sample.ts1[,1])), info = info_msg)

# quarterly series
sample.ts4 <- ts(sample_matrix,start=1960,frequency=4)
sample.xts.ts4 <- as.xts(sample.ts4)


info_msg <- "test.convert_ts4_to_xts"
expect_identical(sample.xts.ts4, as.xts(sample.ts4), info = info_msg)
info_msg <- "test.convert_ts4_to_xts_j1"
expect_identical(sample.xts.ts4[,1], as.xts(sample.ts4)[,1], info = info_msg)
info_msg <- "test.convert_ts4_to_xts_i1"
expect_identical(sample.xts.ts4[1,], as.xts(sample.ts4)[1,], info = info_msg)
info_msg <- "test.convert_ts4_to_xts_i1j1"
expect_identical(sample.xts.ts4[1,1], as.xts(sample.ts4)[1,1], info = info_msg)


info_msg <- "test.ts4_reclass"
expect_identical(sample.ts4, reclass(try.xts(sample.ts4)), info = info_msg)
info_msg <- "test.ts4_reclass_subset_reclass_j1"
expect_identical(sample.ts4[,1], reclass(try.xts(sample.ts4))[,1], info = info_msg)
info_msg <- "test.ts4_reclass_subset_as.xts_j1"
expect_identical(sample.ts4[,1], reclass(try.xts(sample.ts4)[,1]), info = info_msg)
info_msg <- "test.ts4_reclass_subset_ts_j1"
expect_identical(sample.ts4[,1], reclass(try.xts(sample.ts4[,1])), info = info_msg)

# monthly series
sample.ts12 <- ts(sample_matrix,start=1990,frequency=12)
sample.xts.ts12 <- as.xts(sample.ts12)


info_msg <- "test.convert_ts12_to_xts"
expect_identical(sample.xts.ts12, as.xts(sample.ts12), info = info_msg)
info_msg <- "test.convert_ts12_to_xts_j1"
expect_identical(sample.xts.ts12[,1], as.xts(sample.ts12)[,1], info = info_msg)
info_msg <- "test.convert_ts12_to_xts_i1"
expect_identical(sample.xts.ts12[1,], as.xts(sample.ts12)[1,], info = info_msg)
info_msg <- "test.convert_ts12_to_xts_i1j1"
expect_identical(sample.xts.ts12[1,1], as.xts(sample.ts12)[1,1], info = info_msg)


info_msg <- "test.ts12_reclass"
expect_identical(sample.ts12, reclass(try.xts(sample.ts12)), info = info_msg)
info_msg <- "test.ts12_reclass_subset_reclass_j1"
expect_identical(sample.ts12[,1], reclass(try.xts(sample.ts12))[,1], info = info_msg)
info_msg <- "test.ts12_reclass_subset_as.xts_j1"
expect_identical(sample.ts12[,1], reclass(try.xts(sample.ts12)[,1]), info = info_msg)
info_msg <- "test.ts12_reclass_subset_ts_j1"
expect_identical(sample.ts12[,1], reclass(try.xts(sample.ts12[,1])), info = info_msg)
