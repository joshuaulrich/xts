# ensure first group is included in output
test.to.frequency_includes_first_group <- function() {
  data(sample_matrix)
  x <- as.xts(sample_matrix)
  x$Volume <- 1
  
  tf <- xts:::to.frequency(x, x$Volume, 90, name=NULL)
  tp <- .Call(xts:::C_toPeriod, x, c(0L, 90L, 180L), TRUE, 5L, FALSE, FALSE,
              c("Open", "High", "Low", "Close", "Volume"))

  checkIdentical(tf, tp)
}

test.to.period_custom_endpoints <- function() {
  data(sample_matrix)
  x <- as.xts(sample_matrix)
 
  ep <- endpoints(x, "months", 1)
  y1 <- to.period(x, "months", 1)
  y2 <- to.period(x, ep)

  checkIdentical(y1, y2)

  # period must be character or numeric
  checkException(to.period(x, TRUE))

  # 'k' and 'indexAt' are ignored
  op <- options(warn = 2)
  on.exit(options(warn = op$warn))
  checkException(to.period(x, ep, k = 2))
  checkException(to.period(x, ep, indexAt = ""))
}

