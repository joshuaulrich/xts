
# POSIXct index
test.diff_integer_POSIXt <- function() {
  x <- .xts(1:5, 1:5 + 0.0)
  dx <- xts(rbind(NA_integer_, diff(coredata(x))), index(x))
  checkIdentical(diff(x), dx)
}
test.diff_numeric_POSIXt <- function() {
  x <- .xts(1:5 + 1.0, 1:5 + 0.0)
  dx <- xts(rbind(NA_real_, diff(coredata(x))), index(x))
  checkIdentical(diff(x), dx)
}
test.diff_logical_POSIXt <- function() {
  x <- .xts(1:5 > 2, 1:5 + 0.0)
  dx <- xts(rbind(NA, diff(coredata(x))), index(x))
  checkIdentical(diff(x), dx)
}

# Date index
test.diff_integer_Date <- function() {
  x <- xts(1:5, as.Date("2016-01-01") - 5:1)
  dx <- xts(rbind(NA_integer_, diff(coredata(x))), index(x))
  checkIdentical(diff(x), dx)
}
test.diff_numeric_Date <- function() {
  x <- xts(1:5 + 1.0, as.Date("2016-01-01") - 5:1)
  dx <- xts(rbind(NA_real_, diff(coredata(x))), index(x))
  checkIdentical(diff(x), dx)
}
test.diff_logical_Date <- function() {
  x <- xts(1:5 > 2, as.Date("2016-01-01") - 5:1)
  dx <- xts(rbind(NA, diff(coredata(x))), index(x))
  checkIdentical(diff(x), dx)
}

