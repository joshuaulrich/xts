# Tests for xts constructors
#

### NA in order.by {{{
# .xts()
test..xts_order.by_NA_integer <- function() {
  checkException(.xts(1:3, c(1L, 2L, NA)))
  checkException(.xts(1:3, c(NA, 2L, 3L)))
  checkException(.xts(1:3, c(1L, NA, 3L)))
}
test..xts_order.by_NA_double <- function() {
  checkException(.xts(1:3, c(1, 2, NA)))
  checkException(.xts(1:3, c(NA, 2, 3)))
  checkException(.xts(1:3, c(1, NA, 3)))
}
test..xts_order.by_NaN_double <- function() {
  checkException(.xts(1:3, c(1, 2, NaN)))
  checkException(.xts(1:3, c(NaN, 2, 3)))
  checkException(.xts(1:3, c(1, NaN, 3)))
}
test..xts_order.by_Inf_double <- function() {
  checkException(.xts(1:3, c(1, 2,  Inf)))
  checkException(.xts(1:3, c(-Inf, 2, 3)))
}
# xts()
test.xts_order.by_NA_integer <- function() {
  checkException(xts(1:3, as.Date(c(1L, 2L, NA), origin = "1970-01-01")))
  checkException(xts(1:3, as.Date(c(NA, 2L, 3L), origin = "1970-01-01")))
  checkException(xts(1:3, as.Date(c(1L, NA, 3L), origin = "1970-01-01")))
}
test.xts_order.by_NA_double <- function() {
  checkException(xts(1:3, .POSIXct(c(1, 2, NA))))
  checkException(xts(1:3, .POSIXct(c(NA, 2, 3))))
  checkException(xts(1:3, .POSIXct(c(1, NA, 3))))
}
test.xts_order.by_NaN_double <- function() {
  checkException(xts(1:3, .POSIXct(c(1, 2, NaN))))
  checkException(xts(1:3, .POSIXct(c(NaN, 2, 3))))
  checkException(xts(1:3, .POSIXct(c(1, NaN, 3))))
}
test.xts_order.by_Inf_double <- function() {
  checkException(xts(1:3, .POSIXct(c(1, 2,  Inf))))
  checkException(xts(1:3, .POSIXct(c(-Inf, 2, 3))))
}
### }}}
