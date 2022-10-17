data(sample_matrix)
sample.matrix <- sample_matrix
sample.xts <- as.xts(sample.matrix)

info_msg <- "test.convert_matrix_to_xts"
expect_identical(sample.xts,
                 as.xts(sample.matrix),
                 info = info_msg)

info_msg <- "test.convert_matrix_to_xts_j1"
expect_identical(sample.xts[, 1],
                 as.xts(sample.matrix)[, 1],
                 info = info_msg)

info_msg <- "test.convert_matrix_to_xts_i1"
expect_identical(sample.xts[1,],
                 as.xts(sample.matrix)[1,],
                 info = info_msg)

info_msg <- "test.convert_matrix_to_xts_i1j1"
expect_identical(sample.xts[1, 1],
                 as.xts(sample.matrix)[1, 1],
                 info = info_msg)

info_msg <- "test.matrix_reclass"
expect_identical(sample.matrix,
                 reclass(try.xts(sample.matrix)),
                 info = info_msg)

info_msg <- "test.matrix_reclass_subset_reclass_j1"
expect_identical(sample.matrix[, 1],
                 reclass(try.xts(sample.matrix))[, 1],
                 info = info_msg)

info_msg <- "test.matrix_reclass_subset_as.xts_j1"
expect_identical(sample.matrix[, 1, drop = FALSE],
                 reclass(try.xts(sample.matrix)[, 1]),
                 info = info_msg)
expect_identical(sample.matrix[, 1],
                 reclass(try.xts(sample.matrix))[, 1],
                 info = info_msg)

info_msg <- "test.matrix_reclass_subset_matrix_j1"
expect_identical(sample.matrix[, 1, drop = FALSE],
                 reclass(try.xts(sample.matrix[, 1, drop = FALSE])),
                 info = info_msg)

### zero-width to matrix
info_msg <- "test.zero_width_xts_to_matrix"
x <- .xts(,1)
xm <- as.matrix(x)
zm <- as.matrix(as.zoo(x))
expect_identical(xm, zm, info = info_msg)

### dim-less xts to matrix
info_msg <- "test.dimless_xts_to_matrix"
ix <- structure(1:3, tclass = c("POSIXct", "POSIXt"), tzone = "")
x <- structure(1:3, index = ix, class = c("xts", "zoo"))
m <- matrix(1:3, 3, 1, dimnames = list(format(.POSIXct(1:3)), "x"))
expect_identical(as.matrix(x), m, info = info_msg)
