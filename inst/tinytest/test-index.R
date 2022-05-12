test.get_index_does_not_error_if_index_has_no_attributes  <- function() {
  x <- .xts(1:3, 1:3, tzone = "UTC")

  ix <- index(x)
  ix <- ix + 3

  attr(x, "index") <- 4:6

  # get index (test will fail if it errors)
  i <- index(x)

  # won't get here if index(x) errors
  return(TRUE)
}

test.set_.index_copies_index_attributes <- function() {
  x <- .xts(1:3, 1:3, tzone = "UTC")

  ix <- index(x)
  ix <- ix + 3

  .index(x) <- 4:6
  checkEquals(index(x), ix)
}

test.set_index_copies_index_attributes <- function() {
  x <- .xts(1:3, 1:3, tzone = "UTC")

  ix <- index(x)
  ix <- ix + 3

  index(x) <- .POSIXct(4:6, "UTC")
  checkEquals(index(x), ix)
}

test.set_index_restores_tzone_attribute <- function() {
  # x index must be numeric, because index<-.xts coerces RHS to numeric
  x <- .xts(1:3, 1:3+0, tzone = "")
  y <- x

  # Ops.POSIXt drops tzone attribute when tzone = ""
  index(y) <- index(y) + 0

  checkIdentical(x, y)
}

test.get_index_zero_length_returns_correct_index_type <- function() {
  xd <- xts(1, .Date(1))
  zd <- as.zoo(xd)
  xd_index <- index(xd[0,])

  checkTrue(length(xd_index) == 0)
  checkEquals(index(xd[0,]), index(zd[0,]))
  checkEquals(index(xd[0,]), .Date(numeric()))

  xp <- xts(1, .POSIXct(1), tzone = "UTC")
  zp <- as.zoo(xp)

  xp_index <- index(xp[0,])
  zp_index <- index(zp[0,])
  zl_index <- .POSIXct(numeric(), tz = "UTC")

  checkTrue(length(xp_index) == 0)
  checkEquals(tzone(xp_index), tzone(zp_index))
  checkTrue(inherits(xp_index, c("POSIXct", "POSIXt")))
}
