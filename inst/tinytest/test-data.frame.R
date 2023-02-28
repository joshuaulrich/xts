data(sample_matrix)
sample.data.frame <- data.frame(sample_matrix[1:15,])
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

### data.frame with Date/POSIXct column

df_date_col <- data.frame(Date = as.Date(rownames(sample.data.frame)),
                          sample.data.frame,
                          row.names = NULL)

info_msg <- "convert data.frame to xts from Date column"
x <- as.xts(df_date_col)
y <- xts(coredata(sample.xts), as.Date(index(sample.xts), tz = ""))
expect_equal(y, x, info = info_msg)

info_msg <- "convert data.frame to xts from POSIXct column"
dttm <- as.POSIXct(rownames(sample.data.frame), tz = "UTC") + runif(15)*10000
df_pxct_col <- data.frame(Timestamp = dttm,
                          sample.data.frame,
                          row.names = NULL)
x <- as.xts(df_pxct_col)
y <- xts(coredata(sample.xts), dttm)
expect_equal(y, x, info = info_msg)

info_msg <- "convert data.frame to xts errors when no rownames or column"
df_no_col <- data.frame(sample.data.frame, row.names = NULL)
expect_error(as.xts(df_no_col),
             pattern = "could not convert row names to a date-time and could not find a time-based column",
             info = info_msg)

info_msg <- "keep column name for data.frame with one non-time-based column"
x <- as.xts(df_date_col[, 1:2])
expect_identical(names(x), "Open", info = info_msg)
