# Tests for plotting functions
data(sample_matrix)
x <- as.xts(sample_matrix, dateFormat = "Date")

# axTicksByTime
info_msg <- "test.format_xts_yearqtr"
xq <- to.quarterly(x)
xtbt <- axTicksByTime(xq)
expect_identical(names(xtbt), c("2007-Q1", "2007-Q2"), info = info_msg)

info_msg <- "test.format_zoo_yearqtr"
xq <- to.quarterly(x)
xtbt <- axTicksByTime(as.zoo(xq))
expect_identical(names(xtbt), c("2007-Q1", "2007-Q2"), info = info_msg)

info_msg <- "test.axTicksByTime_ticks.on_quarter"
tick_marks <- setNames(c(1, 4, 7, 10, 13, 16, 19, 22, 25, 25),
  c("\nJan\n2016", "\nApr\n2016", "\nJul\n2016", "\nOct\n2016",
    "\nJan\n2017", "\nApr\n2017", "\nJul\n2017", "\nOct\n2017",
    "\nJan\n2018", "\nJan\n2018"))
if (.Platform$OS.type != "unix") {
  names(tick_marks) <- gsub("\n(.*)\n", "\\1 ", names(tick_marks))
}
ym <- as.yearmon("2018-01") - 24:0 / 12
x <- xts(seq_along(ym), ym)

xtbt <- axTicksByTime(x, ticks.on = "quarters")
expect_identical(xtbt, tick_marks, info = info_msg)

info_msg <- "test.xlim_set_before_rendering"
target <- c(86400.0, 864000.0)
p <- plot(xts(1:10, .Date(1:10)))
expect_equivalent(target, p$get_xlim(), info = info_msg)

info_msg <- "test.ylim_set_before_rendering"
x <- rnorm(10)
p <- plot(xts(x, .Date(1:10)))
expect_equivalent(range(x), p$get_ylim(), info = info_msg)

get_xcoords_respects_object_tzone <-
function()
{
  # random timezone
  tz <- sample(OlsonNames(), 1)
  dttm <- seq(as.POSIXct("2023-01-01 01:23:45"), , 1, 5)
  x <- xts(c(5, 1, 2, 4, 3), dttm, tzone = tz)

  print(p <- plot(x))
  expect_identical(tz, tzone(p$get_xcoords(x)),
                   info = paste("TZ =", tz))
}
get_xcoords_respects_object_tzone()
