P <- structure(
  list(difftime = structure(0, units = "secs", class = "difftime"),
       frequency = 0,
       start = structure(.POSIXct(1, "UTC"), tclass = c("POSIXct", "POSIXt")),
       end = structure(.POSIXct(1, "UTC"), tclass = c("POSIXct", "POSIXt")),
       units = "secs",
       scale = "seconds",
       label = "second"),
  class = "periodicity")

test_date <- as.Date("2022-10-15")

info_msg <- "test.periodicity_on_one_observation_warns"
x <- xts(1, .POSIXct(1, "UTC"))
suppressWarnings(p <- periodicity(x))
expect_identical(p, P, info = info_msg)
expect_warning(p <- periodicity(x), info = info_msg)

info_msg <- "test.periodicity_on_zero_observations_warns"
x <- xts(, .POSIXct(numeric(0), "UTC"))
suppressWarnings(p <- periodicity(x))
P$start <- NA
P$end <- NA
expect_identical(p, P, info = info_msg)
expect_warning(p <- periodicity(x))

check_periodicity_result <-
function(p, units, scale, freq, msg)
{
  info_msg <- paste0(msg, " - units: ", p$units, ", expected: ", units)
  expect_equivalent(p$units, units, info = info_msg)

  info_msg <- paste0(msg, " - scale: ", p$scale, ", expected: ", scale)
  expect_equivalent(p$scale, scale, info = info_msg)

  info_msg <- paste0(msg, " - frequency: ", p$frequency, ", expected: ", freq)
  expect_equivalent(p$frequency, freq, info = info_msg)

  info_msg <- paste0(msg, " - difftime: ", p$difftime, ", expected: ", freq)
  expect_equivalent(as.numeric(p$difftime), freq, info = info_msg)

  invisible(NULL)
}

###############################################################################
info_msg <- "test.periodicity_on_sub_second_data"
set.seed(Sys.Date())
for (i in 1:100) {
  n <- sample(1000, 1) / 1000
  #if (n >= eps) n <- 1
  p <- periodicity(.xts(seq_len(100), n * seq_len(100)))
  check_periodicity_result(p, "secs", "seconds", n, info_msg)
}

# test periodicity between 0.95 and 1, which should round up to 1
#set.seed(Sys.Date())
#for (n in seq(0.9505, 0.99, 0.005)) {
#  p <- periodicity(.xts(seq_len(100), n * seq_len(100)))
#  check_periodicity_result(p, "secs", "seconds", n, info_msg)
#}

###############################################################################
info_msg <- "test.periodicity_on_second_data"
i <- seq_len(100)
for (n in 1:59) {
  p <- periodicity(.xts(i, i))
  check_periodicity_result(p, "secs", "seconds", 1, info_msg)
}

###############################################################################
info_msg <- "test.periodicity_on_minute_data"
i <- seq_len(100) * 60

for (n in 1:59) {
  p <- periodicity(.xts(i, n * i))
  check_periodicity_result(p, "mins", "minute", n, info_msg)
}

###############################################################################
info_msg <- "test.periodicity_on_hourly_data"
i <- seq_len(100) * 3600

for (n in 1:23) {
  p <- periodicity(.xts(i, n * i))
  # NOTE: frequency is in seconds for hourly data (see #54)
  check_periodicity_result(p, "hours", "hourly", n * 3600, info_msg)
}

###############################################################################
info_msg <- "test.periodicity_on_daily_data"
i <- seq_len(100) * 86400

# NOTE: frequency is in seconds for daily data (see #54)
n <- 1
p <- periodicity(.xts(i, n * i))
check_periodicity_result(p, "days", "daily", n * 86400, info_msg)

n <- 2
p <- periodicity(.xts(i, n * i))
check_periodicity_result(p, "days", "weekly", n * 86400, info_msg)

n <- 3
p <- periodicity(.xts(i, n * i))
check_periodicity_result(p, "days", "weekly", n * 86400, info_msg)

###############################################################################
info_msg <- "test.periodicity_on_weekly_data"
i <- 7 * seq_len(100) * 86400

# NOTE: frequency is in seconds for weekly data (see #54)
n <- 1
p <- periodicity(.xts(i, n * i))
check_periodicity_result(p, "days", "weekly", n * 86400 * 7, info_msg)

n <- 2
p <- periodicity(.xts(i, n * i))
check_periodicity_result(p, "days", "monthly", n * 86400 * 7, info_msg)

n <- 3
p <- periodicity(.xts(i, n * i))
check_periodicity_result(p, "days", "monthly", n * 86400 * 7, info_msg)

###############################################################################
info_msg <- "test.periodicity_on_month_data"

n <- 1
i <- seq(as.yearmon(test_date) - 12, by = n/12, length.out = 100)
x <- xts(i, i)
p <- periodicity(x)
check_periodicity_result(p, "days", "monthly", 86400 * 31, info_msg)
# monthly POSIXct
index(x) <- as.POSIXct(i)
p <- periodicity(x)
check_periodicity_result(p, "days", "monthly", 86400 * 31, info_msg)

n <- 2
i <- seq(as.yearmon(test_date) - 12, by = n/12, length.out = 100)
x <- xts(i, i)
p <- periodicity(x)
check_periodicity_result(p, "days", "quarterly", 86400 * 61, info_msg)
# monthly POSIXct
index(x) <- as.POSIXct(i)
p <- periodicity(x)
check_periodicity_result(p, "days", "quarterly", 86400 * 61, info_msg)

###############################################################################
info_msg <- "test.periodicity_on_quarter_data"

n <- 1
i <- seq(as.yearqtr(test_date) - 24, by = n/4, length.out = 100)
x <- xts(i, i)
p <- periodicity(x)
check_periodicity_result(p, "days", "quarterly", 86400 * 91, info_msg)
# quarterly POSIXct
index(x) <- as.POSIXct(i)
p <- periodicity(x)
check_periodicity_result(p, "days", "quarterly", 86400 * 91, info_msg)

n <- 2
i <- seq(as.yearqtr(test_date) - 48, by = n/4, length.out = 100)
p <- periodicity(xts(seq_len(100), i))
check_periodicity_result(p, "days", "yearly", 86400 * 183, info_msg)
# quarterly POSIXct
index(x) <- as.POSIXct(i)
p <- periodicity(x)
check_periodicity_result(p, "days", "yearly", 86400 * 183, info_msg)

n <- 3
i <- seq(as.yearqtr(test_date) - 50, by = n/4, length.out = 100)
p <- periodicity(xts(seq_len(100), i))
check_periodicity_result(p, "days", "yearly", 86400 * 274, info_msg)
# quarterly POSIXct
index(x) <- as.POSIXct(i)
p <- periodicity(x)
check_periodicity_result(p, "days", "yearly", 86400 * 274, info_msg)


###############################################################################
### These are the breakpoints in the code as-of 2022-10
### Woe to the soul who breaks backward compatibility!
info_msg <- "test.correct_units_for_edge_cases"
test01 <- list(p =          59, units = "secs",  scale = "seconds")
test02 <- list(p =          60, units = "mins",  scale = "minute")
test03 <- list(p =        3600, units = "hours", scale = "hourly")
test04 <- list(p =   86400 - 1, units = "hours", scale = "hourly")
test05 <- list(p =       86400, units = "days",  scale = "daily")
test06 <- list(p =  604800 - 1, units = "days",  scale = "weekly")
test07 <- list(p = 2678400 - 1, units = "days",  scale = "monthly")
test08 <- list(p = 7948800 - 1, units = "days",  scale = "quarterly")
test09 <- list(p =     7948800, units = "days",  scale = "quarterly")
test10 <- list(p = 1 + 7948800, units = "days",  scale = "yearly")

result01 <- periodicity(.xts(, test01$p * seq_len(100)))
result02 <- periodicity(.xts(, test02$p * seq_len(100)))
result03 <- periodicity(.xts(, test03$p * seq_len(100)))
result04 <- periodicity(.xts(, test04$p * seq_len(100)))
result05 <- periodicity(.xts(, test05$p * seq_len(100)))
result06 <- periodicity(.xts(, test06$p * seq_len(100)))
result07 <- periodicity(.xts(, test07$p * seq_len(100)))
result08 <- periodicity(.xts(, test08$p * seq_len(100)))
result09 <- periodicity(.xts(, test09$p * seq_len(100)))
result10 <- periodicity(.xts(, test10$p * seq_len(100)))

expect_identical(test01$units, result01$units, info = do.call(paste, c(list(info_msg), test01)))
expect_identical(test02$units, result02$units, info = do.call(paste, c(list(info_msg), test02)))
expect_identical(test03$units, result03$units, info = do.call(paste, c(list(info_msg), test03)))
expect_identical(test04$units, result04$units, info = do.call(paste, c(list(info_msg), test04)))
expect_identical(test05$units, result05$units, info = do.call(paste, c(list(info_msg), test05)))
expect_identical(test06$units, result06$units, info = do.call(paste, c(list(info_msg), test06)))
expect_identical(test07$units, result07$units, info = do.call(paste, c(list(info_msg), test07)))
expect_identical(test08$units, result08$units, info = do.call(paste, c(list(info_msg), test08)))
expect_identical(test09$units, result09$units, info = do.call(paste, c(list(info_msg), test09)))
expect_identical(test10$units, result10$units, info = do.call(paste, c(list(info_msg), test10)))

info_msg <- "periodicity warns when 'x' is time-based and contains NA"
x <- .Date(c(1:5, NA, 7:10))
expect_warning(periodicity(x), info = info_msg)
