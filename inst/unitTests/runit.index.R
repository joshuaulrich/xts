test.get_index_does_not_error_if_index_has_no_attributes  <- function() {
  x <- .xts(1:3, 1:3, tzone = "UTC")

  ix <- index(x)
  ix <- ix + 3

  attr(x, "index") <- 4:6
  checkEquals(index(x), ix)
}
