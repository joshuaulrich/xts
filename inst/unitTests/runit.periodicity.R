P <- structure(
  list(difftime = structure(0, units = "secs", class = "difftime"),
       frequency = 0,
       start = structure(.POSIXct(1, "UTC"), tclass = c("POSIXct", "POSIXt")),
       end = structure(.POSIXct(1, "UTC"), tclass = c("POSIXct", "POSIXt")),
       units = "secs",
       scale = "seconds",
       label = "second"),
  class = "periodicity")

test.periodicity_on_one_observation_warns <- function() {
  x <- xts(1, .POSIXct(1, "UTC"))
  p <- periodicity(x)
  checkIdentical(p, P)

  opt <- options(warn = 2)
  on.exit(options(warn = opt$warn))

  checkException(p <- periodicity(x))
}
test.periodicity_on_zero_observations_warns <- function() {
  x <- xts(, .POSIXct(numeric(0), "UTC"))
  p <- periodicity(x)
  P$start <- NA
  P$end <- NA
  checkIdentical(p, P)

  opt <- options(warn = 2)
  on.exit(options(warn = opt$warn))

  checkException(p <- periodicity(x))
}

check_periodicity_result <-
function(p, units, scale, freq)
{
  checkTrue(p$units == units,
            paste("units is", p$units, "- expected", units))
  checkTrue(p$scale == scale,
            paste("scale is", p$scale, "- expected", scale))
  checkEqualsNumeric(p$frequency, freq,
                     paste("frequency is", p$frequency, "- expected", freq))
  checkEqualsNumeric(p$difftime, freq)
  invisible(NULL)
}

test.periodicity_on_sub_second_data <- function() {

  set.seed(Sys.Date())
  for (i in 1:100) {
    n <- sample(1000, 1) / 1000
    #if (n >= eps) n <- 1
    p <- periodicity(.xts(seq_len(100), n * seq_len(100)))
    check_periodicity_result(p, "secs", "seconds", n)
  }

  # test periodicity between 0.95 and 1, which should round up to 1
  #set.seed(Sys.Date())
  #for (n in seq(0.9505, 0.99, 0.005)) {
  #  p <- periodicity(.xts(seq_len(100), n * seq_len(100)))
  #  check_periodicity_result(p, "secs", "seconds", n)
  #}
}

test.periodicity_on_second_data <- function() {
  i <- seq_len(100)

  for (n in 1:59) {
    p <- periodicity(.xts(i, i))
    check_periodicity_result(p, "secs", "seconds", 1)
  }
}

test.periodicity_on_minute_data <- function() {
  i <- seq_len(100) * 60

  for (n in 1:59) {
    p <- periodicity(.xts(i, n * i))
    check_periodicity_result(p, "mins", "minute", n)
  }
}

test.periodicity_on_hourly_data <- function() {
  i <- seq_len(100) * 3600

  for (n in 1:23) {
    p <- periodicity(.xts(i, n * i))
    check_periodicity_result(p, "hours", "hourly", n)
  }
}

test.periodicity_on_daily_data <- function() {
  i <- seq_len(100) * 86400

  n <- 1
  p <- periodicity(.xts(i, n * i))
  check_periodicity_result(p, "days", "daily", n)

  n <- 2
  p <- periodicity(.xts(i, n * i))
  check_periodicity_result(p, "days", "daily", n)

  n <- 3
  p <- periodicity(.xts(i, n * i))
  check_periodicity_result(p, "days", "daily", n)
}

test.periodicity_on_weekly_data <- function() {
  i <- 7 * seq_len(100) * 86400

  n <- 1
  p <- periodicity(.xts(i, n * i))
  check_periodicity_result(p, "days", "weekly", 7*n)

  n <- 2
  p <- periodicity(.xts(i, n * i))
  check_periodicity_result(p, "days", "weekly", 7*n)

  n <- 3
  p <- periodicity(.xts(i, n * i))
  check_periodicity_result(p, "days", "weekly", 7*n)
}

test.periodicity_on_month_data <- function() {

  n <- 1
  i <- seq(as.yearmon(Sys.Date()) - 12, by = n/12, length.out = 100)
  x <- xts(i, i)
  p <- periodicity(x)
  check_periodicity_result(p, "days", "monthly", 30)
  # monthly POSIXct
  index(x) <- as.POSIXct(i)
  p <- periodicity(x)
  check_periodicity_result(p, "days", "monthly", 31)

  n <- 2
  i <- seq(as.yearmon(Sys.Date()) - 12, by = n/12, length.out = 100)
  x <- xts(i, i)
  p <- periodicity(x)
  check_periodicity_result(p, "days", "monthly", 60)
  # monthly POSIXct
  index(x) <- as.POSIXct(i)
  p <- periodicity(x)
  check_periodicity_result(p, "days", "monthly", 61)
}

test.periodicity_on_quarter_data <- function() {

  n <- 1
  i <- seq(as.yearqtr(Sys.Date()) - 24, by = n/4, length.out = 100)
  x <- xts(i, i)
  p <- periodicity(x)
  check_periodicity_result(p, "days", "quarterly", 91)
  # quarterly POSIXct
  index(x) <- as.POSIXct(i)
  p <- periodicity(x)
  check_periodicity_result(p, "days", "quarterly", 91)

  n <- 2
  i <- seq(as.yearqtr(Sys.Date()) - 48, by = n/4, length.out = 100)
  p <- periodicity(xts(seq_len(100), i))
  check_periodicity_result(p, "days", "quarterly", 183)
  # quarterly POSIXct
  index(x) <- as.POSIXct(i)
  p <- periodicity(x)
  check_periodicity_result(p, "days", "quarterly", 183)

  n <- 3
  i <- seq(as.yearqtr(Sys.Date()) - 50, by = n/4, length.out = 100)
  p <- periodicity(xts(seq_len(100), i))
  check_periodicity_result(p, "days", "quarterly", 274)
  # quarterly POSIXct
  index(x) <- as.POSIXct(i)
  p <- periodicity(x)
  check_periodicity_result(p, "days", "quarterly", 274)
}

test.correct_units_for_edge_cases <- function() {

  # These are the breakpoints in the code as-of 2022-10
  # Woe to the soul who breaks backward compatibility!
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

  checkIdentical(test01$units, result01$units, do.call(paste, test01))
  checkIdentical(test02$units, result02$units, do.call(paste, test02))
  checkIdentical(test03$units, result03$units, do.call(paste, test03))
  checkIdentical(test04$units, result04$units, do.call(paste, test04))
  checkIdentical(test05$units, result05$units, do.call(paste, test05))
  checkIdentical(test06$units, result06$units, do.call(paste, test06))
  checkIdentical(test07$units, result07$units, do.call(paste, test07))
  checkIdentical(test08$units, result08$units, do.call(paste, test08))
  checkIdentical(test09$units, result09$units, do.call(paste, test09))
  checkIdentical(test10$units, result10$units, do.call(paste, test10))
}
