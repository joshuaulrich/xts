# Tests for plotting functions
data(sample_matrix)
x <- as.xts(sample_matrix, dateFormat = "Date")

# axTicksByTime
test.format_xts_yearqtr <- function() {
  xq <- to.quarterly(x)
  xtbt <- axTicksByTime(xq)
  checkIdentical(names(xtbt), c("2007-Q1", "2007-Q2"))
}

test.format_zoo_yearqtr <- function() {
  xq <- to.quarterly(x)
  xtbt <- axTicksByTime(as.zoo(xq))
  checkIdentical(names(xtbt), c("2007-Q1", "2007-Q2"))
}

test.axTicksByTime_ticks.on_quarter <- function() {
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
  checkIdentical(xtbt, tick_marks)
}

test.xlim_set_before_rendering <- function() {
  target <- c(86400.0, 864000.0)
  p <- plot(xts(1:10, .Date(1:10)))
  checkEqualsNumeric(target, p$get_xlim())
}

test.ylim_set_before_rendering <- function() {
  x <- rnorm(10)
  p <- plot(xts(x, .Date(1:10)))
  checkEqualsNumeric(range(x), p$get_ylim()[[2]])
}

test.yaxis.ticks <- function() {

  x <- xts(rnorm(50), .Date(1:50))
  ylim <- c(0, 10) # default case
  p1 <- plot(x)
  checkIdentical(pretty(ylim, 5), p1$Env$y_grid_lines(ylim))

  p2 <- plot(x, yaxis.ticks = 10) # twice as many y-axis grid lines
  checkIdentical(pretty(ylim, 10), p2$Env$y_grid_lines(ylim))
}
