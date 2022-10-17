# POSIXct index
info_msg <- "test.diff_integer_POSIXt"
x <- .xts(1:5, 1:5 + 0.0)
dx <- xts(rbind(NA_integer_, diff(coredata(x))), index(x))
expect_identical(diff(x), dx, info_msg)

info_msg <- "test.diff_numeric_POSIXt"
x <- .xts(1:5 + 1.0, 1:5 + 0.0)
dx <- xts(rbind(NA_real_, diff(coredata(x))), index(x))
expect_identical(diff(x), dx, info_msg)

info_msg <- "test.diff_logical_POSIXt"
x <- .xts(1:5 > 2, 1:5 + 0.0)
dx <- xts(rbind(NA, diff(coredata(x))), index(x))
expect_identical(diff(x), dx, info_msg)


# Date index
info_msg <- "test.diff_integer_Date"
x <- xts(1:5, as.Date("2016-01-01") - 5:1)
dx <- xts(rbind(NA_integer_, diff(coredata(x))), index(x))
expect_identical(diff(x), dx, info_msg)

info_msg <- "test.diff_numeric_Date"
x <- xts(1:5 + 1.0, as.Date("2016-01-01") - 5:1)
dx <- xts(rbind(NA_real_, diff(coredata(x))), index(x))
expect_identical(diff(x), dx, info_msg)

info_msg <- "test.diff_logical_Date"
x <- xts(1:5 > 2, as.Date("2016-01-01") - 5:1)
dx <- xts(rbind(NA, diff(coredata(x))), index(x))
expect_identical(diff(x), dx, info_msg)


# Type-check failure errors
info_msg <- "diff.xts() 'differences' argument must be integer"
x <- .xts(1:5, 1:5)
# (ignore NA introduced by coercion)
expect_error(suppressWarnings(diff(x, 1L, "a")), info = info_msg)

info_msg <- "diff.xts() 'lag' argument must be integer"
x <- .xts(1:5, 1:5)
# (ignore NA introduced by coercion)
expect_error(suppressWarnings(diff(x, "a", 1L)), info = info_msg)

info_msg <- "diff.xts() differences argument must be > 0"
expect_error(diff(.xts(1:5, 1:5), 1L, -1L), info = info_msg)

info_msg <- "diff.xts() lag argument must be > 0"
expect_error(diff(.xts(1:5, 1:5), -1L, 1L), info = info_msg)


info_msg <- "test.diff_logical_preserves_colnames"
cnames <- c("a", "b")
x <- .xts(matrix(rnorm(10) > 0, 5), 1:5, dimnames = list(NULL, cnames))
y <- diff(x)
expect_identical(colnames(y), cnames, info = info_msg)
