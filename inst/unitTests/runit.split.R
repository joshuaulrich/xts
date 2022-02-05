# 'f' is character, but length(f) > 1
test.split_character_f_not_endpoints <- function() {
  x <- .xts(1:5, 1:5)
  f <- letters[1:nrow(x)]
  checkIdentical(split(x,f), split(as.zoo(x),f))
}

test.split_returns_named_list <- function() {
  qtr_2020 <- paste0("2020 Q", 1:4)
  qtr_2021 <- paste0("2021 Q", 1:4)

  msg <- "quarterly data split by year"
  x_q <- xts(1:8, as.yearqtr(c(qtr_2020, qtr_2021)))
  nm_q <- names(split(x_q, "years"))
  checkIdentical(c("2020", "2021"), nm_q, msg)

  # names formatted as yearqtr
  msg <- "monthly data split by quarter"
  x_mo <- xts(1:12, as.yearmon(2020 + 0:11/12))
  nm_mo <- names(split(x_mo, "quarters"))
  checkIdentical(qtr_2020, nm_mo, msg)

  # names formatted as yearmon
  msg <- "daily data split by month"
  x_day <- xts(1:10, .Date(-5:4))
  nm_day <- names(split(x_day, "months"))
  checkIdentical(c("Dec 1969", "Jan 1970"), nm_day, msg)

  # names formatted as Date
  msg <- "hourly data split by day"
  x_hr <- .xts(1:10, -5:4 * 3600, tzone = "UTC")
  nm_hr <- names(split(x_hr, "days"))
  checkIdentical(c("1969-12-31", "1970-01-01"), nm_hr, msg)

  nm_minutes <- c("1970-01-01 00:00:00", "1970-01-01 00:01:00")

  msg <- "second data split by minute"
  x_sec <- .xts(1:120, 1:120 - 1, tzone = "UTC")
  nm_sec <- names(split(x_sec, "minutes"))
  checkIdentical(nm_minutes, nm_sec, msg)

  t1 <- as.POSIXct(nm_minutes[1], tz = "UTC")

  msg <- "microsecond data split by milliseconds"
  us <- seq(1e-4, 2e-1, 1e-4)
  x_us <- xts(seq_along(us), t1 + us)
  nm_ms <- names(split(x_us, "milliseconds"))
  nm_target <- format(t1 + seq(0, 0.2, 0.001), "%Y-%m-%d %H:%M:%OS3")
  checkIdentical(nm_target, nm_ms, msg)
}
