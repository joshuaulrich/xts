# These tests check that the time class of a time series on which
# a relational operator is applied is not changed.

ts1 <- xts(17, order.by = as.Date('2020-01-29'))

test.get_tclass_ts1  <- function() {
  checkIdentical(tclass(ts1), c("Date"))
}

test.tclass_after_rel_op <- function() {
  checkIdentical(tclass(ts1 < 0), c("Date"))
  checkIdentical(tclass(ts1 > 0), c("Date"))
  checkIdentical(tclass(ts1 <= 0), c("Date"))
  checkIdentical(tclass(ts1 >= 0), c("Date"))
  checkIdentical(tclass(ts1 == 0), c("Date"))
  checkIdentical(tclass(ts1 != 0), c("Date"))
}

