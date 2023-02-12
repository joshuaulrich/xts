# index crosses the unix epoch
info_msg <- "test.double_index_cross_epoch"
x <- .xts(1:22, 1.0*(-10:11), tzone="UTC")
ep <- endpoints(x, "seconds", 2)
expect_identical(ep, 0:11*2L, info_msg)

info_msg <- "test.integer_index_cross_epoch"
x <- .xts(1:22, -10:11, tzone="UTC")
ep <- endpoints(x, "seconds", 2)
expect_identical(ep, 0:11*2L, info_msg)


#{{{daily data
data(sample_matrix)
xDailyDblIdx <- as.xts(sample_matrix, dateFormat="Date")
xDailyIntIdx <- xDailyDblIdx
storage.mode(.index(xDailyIntIdx)) <- "integer"

info_msg <- "test.days_double_index"
ep <- endpoints(xDailyDblIdx, "days", 7)
expect_identical(ep, c(0L, 1:26*7L-3L, nrow(xDailyDblIdx)), info_msg)

info_msg <- "test.days_integer_index"
ep <- endpoints(xDailyIntIdx, "days", 7)
expect_identical(ep, c(0L, 1:26*7L-3L, nrow(xDailyIntIdx)), info_msg)


info_msg <- "test.weeks_double_index"
ep <- endpoints(xDailyDblIdx, "weeks", 1)
expect_identical(ep, c(0L, 1:25*7L-1L, nrow(xDailyDblIdx)), info_msg)

info_msg <- "test.weeks_integer_index"
ep <- endpoints(xDailyIntIdx, "weeks", 1)
expect_identical(ep, c(0L, 1:25*7L-1L, nrow(xDailyIntIdx)), info_msg)


info_msg <- "test.months_double_index"
ep <- endpoints(xDailyDblIdx, "months", 1)
expect_identical(ep, c(0L, 30L, 58L, 89L, 119L, 150L, 180L), info_msg)

info_msg <- "test.months_integer_index"
ep <- endpoints(xDailyIntIdx, "months", 1)
expect_identical(ep, c(0L, 30L, 58L, 89L, 119L, 150L, 180L), info_msg)


info_msg <- "test.quarters_double_index"
ep <- endpoints(xDailyDblIdx, "quarters", 1)
expect_identical(ep, c(0L, 89L, 180L), info_msg)

info_msg <- "test.quarters_integer_index"
ep <- endpoints(xDailyIntIdx, "quarters", 1)
expect_identical(ep, c(0L, 89L, 180L), info_msg)


info_msg <- "test.years_double_index"
d <- seq(as.Date("1970-01-01"), by="1 day", length.out=365*5)
x <- xts(seq_along(d), d)
ep <- endpoints(x, "years", 1)
expect_identical(ep, c(0L, 365L, 730L, 1096L, 1461L, 1825L), info_msg)

info_msg <- "test.years_integer_index"
d <- seq(as.Date("1970-01-01"), by="1 day", length.out=365*5)
x <- xts(seq_along(d), d)
storage.mode(.index(x)) <- "integer"
ep <- endpoints(x, "years", 1)
expect_identical(ep, c(0L, 365L, 730L, 1096L, 1461L, 1825L), info_msg)

#}}}

#{{{second data
n <- 86400L %/% 30L * 365L * 2L
xSecIntIdx <- .xts(1L:n,
    seq(.POSIXct(0, tz="UTC"), by="30 sec", length.out=n), tzone="UTC")
xSecDblIdx <- xSecIntIdx 
storage.mode(.index(xSecDblIdx)) <- "double"

info_msg <- "test.seconds_double_index"
ep <- endpoints(xSecDblIdx, "seconds", 3600)
expect_identical(ep, seq(0L, nrow(xSecDblIdx), 120L), info_msg)

info_msg <- "test.seconds_integer_index"
ep <- endpoints(xSecIntIdx, "seconds", 3600)
expect_identical(ep, seq(0L, nrow(xSecIntIdx), 120L), info_msg)

info_msg <- "test.seconds_secs"
x <- .xts(1:10, 1:10/6)
ep1 <- endpoints(x, "seconds")
ep2 <- endpoints(x, "secs")
expect_identical(ep1, ep2, info_msg)


info_msg <- "test.minutes_double_index"
ep <- endpoints(xSecDblIdx, "minutes", 60)
expect_identical(ep, seq(0L, nrow(xSecDblIdx), 120L), info_msg)

info_msg <- "test.minutes_integer_index"
ep <- endpoints(xSecIntIdx, "minutes", 60)
expect_identical(ep, seq(0L, nrow(xSecIntIdx), 120L), info_msg)

info_msg <- "test.minutes_mins"
x <- .xts(1:10, 1:10*10)
ep1 <- endpoints(x, "minutes")
ep2 <- endpoints(x, "mins")
expect_identical(ep1, ep2, info_msg)


info_msg <- "test.hours_double_index"
ep <- endpoints(xSecDblIdx, "hours", 1)
expect_identical(ep, seq(0L, nrow(xSecDblIdx), 120L), info_msg)

info_msg <- "test.hours_integer_index"
ep <- endpoints(xSecIntIdx, "hours", 1)
expect_identical(ep, seq(0L, nrow(xSecIntIdx), 120L), info_msg)


info_msg <- "test.days_double_index"
ep <- endpoints(xSecDblIdx, "days", 1)
expect_identical(ep, seq(0L, by=2880L, length.out=length(ep)), info_msg)

info_msg <- "test.days_integer_index"
ep <- endpoints(xSecIntIdx, "days", 1)
expect_identical(ep, seq(0L, by=2880L, length.out=length(ep)), info_msg)


info_msg <- "test.weeks_double_index"
ep <- endpoints(xSecDblIdx, "weeks", 1)
ep2 <- c(0L, seq(11520L, nrow(xSecDblIdx)-1L, 20160L), nrow(xSecDblIdx))
expect_identical(ep, ep2, info_msg)

info_msg <- "test.weeks_integer_index"
ep <- endpoints(xSecIntIdx, "weeks", 1)
ep2 <- c(0L, seq(11520L, nrow(xSecIntIdx)-1L, 20160L), nrow(xSecIntIdx))
expect_identical(ep, ep2, info_msg)


info_msg <- "test.months_double_index"
ep <- endpoints(xSecDblIdx, "months", 1)
n <- 86400L * c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) / 30
ep2 <- as.integer(cumsum(c(0L, n, n)))
expect_identical(ep, ep2, info_msg)

info_msg <- "test.months_integer_index"
ep <- endpoints(xSecIntIdx, "months", 1)
n <- 86400L * c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) / 30
ep2 <- as.integer(cumsum(c(0L, n, n)))
expect_identical(ep, ep2, info_msg)


info_msg <- "test.quarters_double_index"
ep <- endpoints(xSecDblIdx, "quarters", 1)
n <- 86400L * c(90, 91, 92, 92) / 30
ep2 <- as.integer(cumsum(c(0L, n, n)))
expect_identical(ep, ep2, info_msg)

info_msg <- "test.quarters_integer_index"
ep <- endpoints(xSecIntIdx, "quarters", 1)
n <- 86400L * c(90, 91, 92, 92) / 30
ep2 <- as.integer(cumsum(c(0L, n, n)))
expect_identical(ep, ep2, info_msg)


info_msg <- "test.years_double_index"
ep <- endpoints(xSecDblIdx, "years", 1)
expect_identical(ep, c(0L, 1051200L, 2102400L), info_msg)

info_msg <- "test.years_integer_index"
ep <- endpoints(xSecIntIdx, "years", 1)
expect_identical(ep, c(0L, 1051200L, 2102400L), info_msg)

#}}}

# sparse endpoints could be a problem with POSIXlt elements (#169)
# TODO: sparse intraday endpoints
info_msg <- "test.sparse_years"
x <- xts(2:6, as.Date(sprintf("199%d-06-01", 2:6)))
ep <- endpoints(x, "years")
expect_identical(ep, 0:5, info_msg)

info_msg <- "test.sparse_quarters"
x <- xts(2:6, as.Date(sprintf("199%d-06-01", 2:6)))
ep <- endpoints(x, "quarters")
expect_identical(ep, 0:5, info_msg)

info_msg <- "test.sparse_months"
x <- xts(2:6, as.Date(sprintf("199%d-06-01", 2:6)))
ep <- endpoints(x, "months")
expect_identical(ep, 0:5, info_msg)

info_msg <- "test.sparse_weeks"
x <- xts(2:6, as.Date(sprintf("199%d-06-01", 2:6)))
ep <- endpoints(x, "weeks")
expect_identical(ep, 0:5, info_msg)

info_msg <- "test.sparse_days"
x <- xts(2:6, as.Date(sprintf("199%d-06-01", 2:6)))
ep <- endpoints(x, "days")
expect_identical(ep, 0:5, info_msg)


# sub-second resolution on Windows
info_msg <- "test.sub_second_resolution"
x <- .xts(1:6, .POSIXct(0:5 / 10 + 0.01))
ep <- endpoints(x, "ms", 250)
expect_identical(ep, c(0L, 3L, 5L, 6L), info_msg)


# precision issues
info_msg <- "test.sub_second_resolution_exact"
x <- .xts(1:6, .POSIXct(0:5 / 10))
ep <- endpoints(x, "ms", 250)
expect_identical(ep, c(0L, 3L, 5L, 6L), info_msg)

info_msg <- "test.sub_second_resolution_representation"
x <- .xts(1:10, .POSIXct(1.5e9 + 0:9 / 10))
ep <- endpoints(x, "ms", 200)
expect_identical(ep, seq(0L, 10L, 2L), info_msg)


# on = "quarters", k > 1
info_msg <- "test.multiple_quarters"
x <- xts(1:48, as.yearmon("2015-01-01") + 0:47 / 12)
expect_identical(endpoints(x, "quarters", 1), seq(0L, 48L, 3L), info_msg)
expect_identical(endpoints(x, "quarters", 2), seq(0L, 48L, 6L), info_msg)
expect_identical(endpoints(x, "quarters", 3), c(seq(0L, 48L, 9L), 48L), info_msg)
expect_identical(endpoints(x, "quarters", 4), seq(0L, 48L,12L), info_msg)
expect_identical(endpoints(x, "quarters", 5), c(seq(0L, 48L,15L), 48L), info_msg)
expect_identical(endpoints(x, "quarters", 6), c(seq(0L, 48L,18L), 48L), info_msg)


# end(x) always in endpoints(x) result
info_msg <- "test.last_obs_always_in_output"
N <- 341*12
xx <- xts(rnorm(N), seq(Sys.Date(), by = "day", length.out = N))

ep <- endpoints(xx, on = "quarters", k = 2) # OK
expect_identical(end(xx), end(xx[ep,]), paste(info_msg, "quarters, k=2"))

ep <- endpoints(xx, on = "quarters", k = 3) # NOPE
expect_identical(end(xx), end(xx[ep,]), paste(info_msg, "quarters, k=3"))

ep <- endpoints(xx, on = "quarters", k = 4) # NOPE
expect_identical(end(xx), end(xx[ep,]), paste(info_msg, "quarters, k=4"))

ep <- endpoints(xx, on = "quarters", k = 5) # NOPE
expect_identical(end(xx), end(xx[ep,]), paste(info_msg, "quarters, k=5"))

ep <- endpoints(xx, on = "months", k = 2) # NOPE
expect_identical(end(xx), end(xx[ep,]), paste(info_msg, "months, k=2"))

ep <- endpoints(xx, on = "months", k = 3) # OK
expect_identical(end(xx), end(xx[ep,]), paste(info_msg, "months, k=3"))

ep <- endpoints(xx, on = "months", k = 4) # NOPE
expect_identical(end(xx), end(xx[ep,]), paste(info_msg, "months, k=4"))

# For the "weeks" case works fine

ep <- endpoints(xx, on = "weeks", k = 2) # OK
expect_identical(end(xx), end(xx[ep,]), paste(info_msg, "weeks, k=2"))

ep <- endpoints(xx, on = "weeks", k = 3) # OK
expect_identical(end(xx), end(xx[ep,]), paste(info_msg, "weeks, k=3"))

ep <- endpoints(xx, on = "weeks", k = 4) # OK
expect_identical(end(xx), end(xx[ep,]), paste(info_msg, "weeks, k=4"))


info_msg <- "test.k_less_than_1_errors"
x <- xDailyIntIdx

expect_error(endpoints(x, on = "years", k =  0), info = info_msg)
expect_error(endpoints(x, on = "years", k = -1), info = info_msg)

expect_error(endpoints(x, on = "quarters", k =  0), info = info_msg)
expect_error(endpoints(x, on = "quarters", k = -1), info = info_msg)

expect_error(endpoints(x, on = "months", k =  0), info = info_msg)
expect_error(endpoints(x, on = "months", k = -1), info = info_msg)

expect_error(endpoints(x, on = "weeks", k =  0), info = info_msg)
expect_error(endpoints(x, on = "weeks", k = -1), info = info_msg)

expect_error(endpoints(x, on = "days", k =  0), info = info_msg)
expect_error(endpoints(x, on = "days", k = -1), info = info_msg)

x <- xSecIntIdx

expect_error(endpoints(x, on = "hours", k =  0), info = info_msg)
expect_error(endpoints(x, on = "hours", k = -1), info = info_msg)

expect_error(endpoints(x, on = "minutes", k =  0), info = info_msg)
expect_error(endpoints(x, on = "minutes", k = -1), info = info_msg)

expect_error(endpoints(x, on = "seconds", k =  0), info = info_msg)
expect_error(endpoints(x, on = "seconds", k = -1), info = info_msg)

x <- .xts(1:10, sort(1 + runif(10)))

expect_error(endpoints(x, on = "ms", k =  0), info = info_msg)
expect_error(endpoints(x, on = "ms", k = -1), info = info_msg)

expect_error(endpoints(x, on = "us", k =  0), info = info_msg)
expect_error(endpoints(x, on = "us", k = -1), info = info_msg)
