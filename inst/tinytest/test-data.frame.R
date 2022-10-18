library(xts)

data(sample_matrix)
sample.data.frame <- data.frame(sample_matrix)
sample.xts <- as.xts(sample.data.frame)

info_msg <- "test.convert_data.frame_to_xts"
expect_identical(sample.xts,
                 as.xts(sample.data.frame),
                 info_msg)


info_msg <- "test.convert_data.frame_to_xts_j1"
expect_identical(sample.xts[,1],
                 as.xts(sample.data.frame)[,1],
                 info_msg)

info_msg <- "test.convert_data.frame_to_xts_i1"
expect_identical(sample.xts[1,],
                 as.xts(sample.data.frame)[1,],
                 info_msg)

info_msg <- "test.convert_data.frame_to_xts_i1j1"
expect_identical(sample.xts[1,1],
                 as.xts(sample.data.frame)[1,1],
                 info_msg)

info_msg <- "test.data.frame_reclass"
expect_identical(sample.data.frame,
                 reclass(try.xts(sample.data.frame)),
                 info_msg)

info_msg <- "test.data.frame_reclass_subset_reclass_j1"
expect_identical(sample.data.frame[,1],
                 reclass(try.xts(sample.data.frame))[,1],
                 info_msg)

# subsetting to 1 col converts to simple numeric - can't successfully handle
info_msg <- "test.data.frame_reclass_subset_as.xts_j1"
expect_identical(sample.data.frame[,1,drop=FALSE],
                 reclass(try.xts(sample.data.frame)[,1]),
                 info_msg)

info_msg <- "test.data.frame_reclass_subset_data.frame_j1"
# subsetting results in a vector, so can't be converted to xts
expect_error(try.xts(sample.data.frame[,1]), info = info_msg)


# check for as.xts.data.frame when order.by is specified
info_msg <- "test.convert_data.frame_to_xts_order.by_POSIXlt"
orderby = as.POSIXlt(rownames(sample.data.frame))
x <- as.xts(sample.data.frame, order.by = orderby)
# tz = "" by default for as.POSIXlt.POSIXct
y <- xts(coredata(sample.xts), as.POSIXlt(index(sample.xts)))
expect_identical(y, x, info_msg)

info_msg <- "test.convert_data.frame_to_xts_order.by_POSIXct"
orderby = as.POSIXct(rownames(sample.data.frame))
x <- as.xts(sample.data.frame, order.by = orderby)
expect_identical(sample.xts, x, info_msg)

info_msg <- "test.convert_data.frame_to_xts_order.by_Date"
# tz = "UTC" by default for as.Date.POSIXct (y), but
# tz = "" by default for as.Date.character (orderby)
orderby = as.Date(rownames(sample.data.frame))
x <- as.xts(sample.data.frame, order.by = orderby)
y <- xts(coredata(sample.xts), as.Date(index(sample.xts), tz = ""))
expect_identical(y, x, info_msg)
