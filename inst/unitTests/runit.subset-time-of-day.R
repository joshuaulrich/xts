# Time-of-day subset

test.time_of_day_start_equals_end <- function() {
  i <- 0:47
  x <- .xts(i, i * 3600, tz = "UTC")
  i1 <- .index(x[c(2L, 26L)])
  checkIdentical(.index(x["T01:00/T01:00"]), i1)
}

test.time_of_day_when_DST_starts <- function() {
  # 2017-03-12: no 0200
  tz <- "America/Chicago"
  tmseq <- seq(as.POSIXct("2017-03-11", tz),
               as.POSIXct("2017-03-14", tz), by = "1 hour")
  x <- xts(seq_along(tmseq), tmseq)
  i <- structure(c(1489215600, 1489219200, 1489222800, 1489302000,
                   1489305600, 1489384800, 1489388400, 1489392000),
       tzone = "America/Chicago", tclass = c("POSIXct", "POSIXt"))
  checkIdentical(.index(x["T01:00:00/T03:00:00"]), i)
}

test.time_of_day_when_DST_ends <- function() {
  # 2017-11-05: 0200 occurs twice
  tz <- "America/Chicago"
  tmseq <- seq(as.POSIXct("2017-11-04", tz),
               as.POSIXct("2017-11-07", tz), by = "1 hour")
  x <- xts(seq_along(tmseq), tmseq)
  i <- structure(c(1509775200, 1509778800, 1509782400, 1509861600, 1509865200,
       1509868800, 1509872400, 1509951600, 1509955200, 1509958800),
       tzone = "America/Chicago", tclass = c("POSIXct", "POSIXt"))
  checkIdentical(.index(x["T01:00:00/T03:00:00"]), i)
}


test.time_of_day_by_hour_start_equals_end <- function() {
  i <- 0:94
  x <- .xts(i, i * 1800, tz = "UTC")
  i1 <- .index(x[c(3, 4, 51, 52)])
  
  checkIdentical(.index(x["T01/T01"]), i1)
  checkIdentical(.index(x["T1/T1"]), i1)
}

test.time_of_day_by_minute <- function() {
  i <- 0:189
  x <- .xts(i, i * 900, tz = "UTC")
  i1 <- .index(x[c(5:8, 101:104)])

  checkIdentical(.index(x["T01:00/T01:45"]), i1)
  checkIdentical(.index(x["T01/T01:45"]), i1)
}

test.time_of_day_check_time_string <- function() {
  i <- 0:10
  x <- .xts(i, i * 1800, tz = "UTC")
  # Should work with and without colon separator
  checkIdentical(x["T0100/T0115"], x["T01:00/T01:15"])
}

test.time_of_day_by_second <- function() {
  i <- 0:500
  x <- .xts(c(i, i), c(i * 15, 86400 + i * 15), tz = "UTC")
  i1 <- .index(x[c(474L, 475L, 476L, 477L, 478L, 479L, 480L, 481L, 482L, 483L,
                   484L, 485L, 975L, 976L, 977L, 978L, 979L, 980L, 981L, 982L,
                   983L, 984L, 985L, 986L)])

  checkIdentical(.index(x["T01:58:05/T02:01:09"]), i1)
  # Can only omit 0 padding for hours. Only for convenience because it does
  # not conform to the ISO 8601 standard, which requires padding with zeros.
  checkIdentical(.index(x["T1:58:05/T2:01:09"]), i1)
  checkIdentical(.index(x["T1:58:05.000/T2:01:09.000"]), i1)
}

test.time_of_day_end_before_start <- function() {
  # Yes, this actually makes sense and is useful for financial markets
  # E.g. some futures markets open at 18:00 and close at 16:00 the next day
  i <- 0:47
  x <- .xts(i, i * 3600, tz = "UTC")
  i1 <- .index(x[-c(18L, 42L)])

  checkIdentical(.index(x["T18:00/T16:00"]), i1)
}

# TODO: Add tests for possible edge cases and/or errors
# end time before start time
# start time and/or end time missing "T" prefix

test.time_of_day_on_zero_width <- function() {
  # return relevant times and a column of NA; consistent with zoo
  i <- 0:47
  tz <- "America/Chicago"
  x <- .xts(, i * 3600, tzone = tz)
  y <- x["T18:00/T20:00"]
  checkIdentical(y, .xts(rep(NA, 6), c(0:2, 24:26)*3600, tzone = tz))
}

test.time_of_day_zero_padding <- function() {
  i <- 0:189
  x <- .xts(i, i * 900, tz = "UTC")
  i1 <- .index(x[c(5:8, 101:104)])

  checkIdentical(.index(x["T01:00/T01:45"]), i1)
  # we support un-padded hours, for convenience (it's not in the standard)
  checkIdentical(.index(x["T1/T1:45"]), i1)
  # minutes and seconds must be zero-padded
  checkException(x["T01:5:5/T01:45"])
  checkException(x["T01:05:5/T01:45"])
}
