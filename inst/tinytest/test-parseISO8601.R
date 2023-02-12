# Constants
TZ <- "UTC"
START_N <- 1424390400
START_T <- .POSIXct(START_N, "UTC")
END_N <- 1425168000
END_T <- .POSIXct(1425168000, "UTC")

### Test basic functionality for dates (TODO: date-times)
info_msg <- "test.all_dates"
out <- list(first.time = START_T, last.time = END_T)
y <- .parseISO8601("/", START_N, END_N, "UTC")
expect_identical(y, out, info = info_msg)
y <- .parseISO8601("::", START_N, END_N, "UTC")
expect_identical(y, out, info = info_msg)

info_msg <- "test.start_to_right_open"
y <- .parseISO8601("2015-02-21/", START_N, END_N, "UTC")
start_t <- as.POSIXct("2015-02-21", tz = "UTC")
expect_identical(y, list(first.time = start_t, last.time = END_T), info = info_msg)

info_msg <- "test.left_open_to_end"
y <- .parseISO8601("/2015-02-21", START_N, END_N, "UTC")
end_t <- as.POSIXct("2015-02-22", tz = "UTC") - 1e-5
expect_identical(y, list(first.time = START_T, last.time = end_t), info = info_msg)

info_msg <- "test.left_open_to_end"
y <- .parseISO8601("/2015-02-21", START_N, END_N, "UTC")
end_t <- as.POSIXct("2015-02-22", tz = "UTC") - 1e-5
expect_identical(y, list(first.time = START_T, last.time = end_t), info = info_msg)

info_msg <- "test.single_date"
y <- .parseISO8601("2015-02-21", START_N, END_N, "UTC")
start_t <- as.POSIXct("2015-02-21", tz = "UTC")
end_t <- as.POSIXct("2015-02-22", tz = "UTC") - 1e-5
expect_identical(y, list(first.time = start_t, last.time = end_t), info = info_msg)

### Test expected failures
### These don't produce errors, but instead return values in UNKNOWN_TIME
UNKNOWN_TIME <- list(first.time = NA_real_, last.time = NA_real_)

info_msg <- "test.start_end_dates_do_not_exist"
x <- "2014-02-30/2015-02-30"
expect_warning(y <- .parseISO8601(x, START_N, END_N, "UTC"),
               pattern = "cannot determine first and last time")
y <- suppressWarnings(.parseISO8601(x, START_N, END_N, "UTC"))
expect_identical(y, UNKNOWN_TIME, info = info_msg)

### test.start_date_does_not_exist <- function() {
###   DEACTIVATED("FAILS: returns everything")
###   x <- "2015-02-30/2015-03-03"
###   y <- .parseISO8601(x, START_N, END_N, "UTC")
###   expect_identical(y, UNKNOWN_TIME, info = info_msg)
### }
###
### test.end_date_does_not_exist <- function() {
###   DEACTIVATED("FAILS: returns everything")
###   x <- "2015-02-25/2015-02-30"
###   y <- .parseISO8601(x, START_N, END_N, "UTC")
###   expect_identical(y, UNKNOWN_TIME, info = info_msg)
### }

### Fuzz tests
info_msg <- "test.start_end_dates_are_garbage"
x <- "0.21/8601.21"
expect_warning(y <- .parseISO8601(x, START_N, END_N, "UTC"),
               pattern = "cannot determine first and last time")
expect_identical(y, UNKNOWN_TIME, info = info_msg)

info_msg <- "test.start_date_is_garbage"
out <- list(first.time = START_T,
            last.time = as.POSIXct("2015-02-22", tz = "UTC") - 1e-5)

x <- "garbage/2015-02-21"
expect_warning(y <- .parseISO8601(x, START_N, END_N, "UTC"),
               pattern = "NAs introduced by coercion")
expect_identical(y, out, info = info_msg)

x <- "0.21/2015-02-21"
y <- .parseISO8601(x, START_N, END_N, "UTC")
expect_identical(y, out, info = info_msg)

info_msg <- "test.end_date_is_garbage"
out <- list(first.time = as.POSIXct("2015-02-25", tz = "UTC"),
            last.time = END_T)

###  # ERRORS (uninformative)
###  x <- "2015-02-25/garbage"
###  y <- .parseISO8601(x, START_N, END_N, "UTC")
###  expect_identical(y, UNKNOWN_TIME, info = info_msg)

x <- "2015-02-25/8601.21"
y <- .parseISO8601(x, START_N, END_N, "UTC")
expect_identical(y, out, info = info_msg)

info_msg <- "test.single_date_is_garbage"
###  # ERRORS (uninformative)
###  y <- .parseISO8601("garbage", START_N, END_N, "UTC")
###  expect_identical(y, UNKNOWN_TIME, info = info_msg)

expect_warning(y <- .parseISO8601("0.21", START_N, END_N, "UTC"),
               pattern = "cannot determine first and last time")
expect_identical(y, UNKNOWN_TIME, info = info_msg)
