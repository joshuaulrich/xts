# Constants
TZ <- "UTC"
START_N <- 1424390400
START_T <- .POSIXct(START_N, "UTC")
END_N <- 1425168000
END_T <- .POSIXct(1425168000, "UTC")

# Test basic functionality for dates (TODO: date-times)
test.all_dates <- function() {
  out <- list(first.time = START_T, last.time = END_T)

  y <- .parseISO8601("/", START_N, END_N, "UTC")
  checkIdentical(y, out)

  y <- .parseISO8601("::", START_N, END_N, "UTC")
  checkIdentical(y, out)
}

test.start_to_right_open <- function() {
  y <- .parseISO8601("2015-02-21/", START_N, END_N, "UTC")
  start_t <- as.POSIXct("2015-02-21", tz = "UTC")
  checkIdentical(y, list(first.time = start_t, last.time = END_T))
}

test.left_open_to_end <- function() {
  y <- .parseISO8601("/2015-02-21", START_N, END_N, "UTC")
  end_t <- as.POSIXct("2015-02-22", tz = "UTC") - 1e-5
  checkIdentical(y, list(first.time = START_T, last.time = end_t))
}

test.left_open_to_end <- function() {
  y <- .parseISO8601("/2015-02-21", START_N, END_N, "UTC")
  end_t <- as.POSIXct("2015-02-22", tz = "UTC") - 1e-5
  checkIdentical(y, list(first.time = START_T, last.time = end_t))
}

test.single_date <- function() {
  y <- .parseISO8601("2015-02-21", START_N, END_N, "UTC")
  start_t <- as.POSIXct("2015-02-21", tz = "UTC")
  end_t <- as.POSIXct("2015-02-22", tz = "UTC") - 1e-5
  checkIdentical(y, list(first.time = start_t, last.time = end_t))
}

# Test expected failures
# These don't produce errors, but instead return values in UNKNOWN_TIME
UNKNOWN_TIME <- list(first.time = NA_real_, last.time = NA_real_)

test.start_end_dates_do_not_exist <- function() {
  x <- "2014-02-30/2015-02-30"
  y <- .parseISO8601(x, START_N, END_N, "UTC")
  checkIdentical(y, UNKNOWN_TIME)
}

# test.start_date_does_not_exist <- function() {
#   DEACTIVATED("FAILS: returns everything")
#   x <- "2015-02-30/2015-03-03"
#   y <- .parseISO8601(x, START_N, END_N, "UTC")
#   checkIdentical(y, UNKNOWN_TIME)
# }
#
# test.end_date_does_not_exist <- function() {
#   DEACTIVATED("FAILS: returns everything")
#   x <- "2015-02-25/2015-02-30"
#   y <- .parseISO8601(x, START_N, END_N, "UTC")
#   checkIdentical(y, UNKNOWN_TIME)
# }

# Fuzz tests
test.start_end_dates_are_garbage <- function() {
  x <- "0.21/8601.21"
  y <- .parseISO8601(x, START_N, END_N, "UTC")
  checkIdentical(y, UNKNOWN_TIME)
}

test.start_date_is_garbage <- function() {
  out <- list(first.time = START_T,
              last.time = as.POSIXct("2015-02-22", tz = "UTC") - 1e-5)

  x <- "garbage/2015-02-21"
  y <- .parseISO8601(x, START_N, END_N, "UTC")
  checkIdentical(y, out)

  x <- "0.21/2015-02-21"
  y <- .parseISO8601(x, START_N, END_N, "UTC")
  checkIdentical(y, out)
}

test.end_date_is_garbage <- function() {
  out <- list(first.time = as.POSIXct("2015-02-25", tz = "UTC"),
              last.time = END_T)

#  # ERRORS (uninformative)
#  x <- "2015-02-25/garbage"
#  y <- .parseISO8601(x, START_N, END_N, "UTC")
#  checkIdentical(y, UNKNOWN_TIME)

  x <- "2015-02-25/8601.21"
  y <- .parseISO8601(x, START_N, END_N, "UTC")
  checkIdentical(y, out)
}

test.single_date_is_garbage <- function() {
#  # ERRORS (uninformative)
#  y <- .parseISO8601("garbage", START_N, END_N, "UTC")
#  checkIdentical(y, UNKNOWN_TIME)

  y <- .parseISO8601("0.21", START_N, END_N, "UTC")
  checkIdentical(y, UNKNOWN_TIME)
}

