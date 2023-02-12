LAG <- function(x, k=1, na.pad=TRUE) {
  z <- lag(as.zoo(x), -k, na.pad)
  dimnames(z) <- NULL
  as.xts(z)
}

### POSIXct index
info_msg <- "test.lag_integer_POSIXt"
x <- .xts(1:5, 1:5 + 0.0)
expect_identical(lag(x), LAG(x), info = info_msg)

info_msg <- "test.lag_numeric_POSIXt"
x <- .xts(1:5 + 1.0, 1:5 + 0.0)
expect_identical(lag(x), LAG(x), info = info_msg)

info_msg <- "test.lag_logical_POSIXt"
x <- .xts(1:5 > 2, 1:5 + 0.0)
expect_identical(lag(x), LAG(x), info = info_msg)


### Date index
info_msg <- "test.lag_integer_Date"
x <- xts(1:5, as.Date("2016-01-01") - 5:1)
expect_identical(lag(x), LAG(x), info = info_msg)

info_msg <- "test.lag_numeric_Date"
x <- xts(1:5 + 1.0, as.Date("2016-01-01") - 5:1)
expect_identical(lag(x), LAG(x), info = info_msg)

info_msg <- "test.lag_logical_Date"
x <- xts(1:5 > 2, as.Date("2016-01-01") - 5:1)
expect_identical(lag(x), LAG(x), info = info_msg)


### Type-check failure errors
info_msg <- "test.lag_k_NA"
x <- .xts(1:5, 1:5)
expect_error(suppressWarnings(lag(x, "a")),  # NA introduced by coercion
             "'k' must be integer",
             info = info_msg)

info_msg <- "test.lag_k_zero_length"
x <- .xts(1:5, 1:5)
expect_error(suppressWarnings(lag(x, 1L, "a")),  # NA introduced by coercion
             "'na.pad' must be logical",
             info = info_msg)
