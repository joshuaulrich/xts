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
